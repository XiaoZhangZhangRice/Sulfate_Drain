# ==========================================================
# "Combine into one giant file" chunk — v7
#
# What changed from v7:
#   - 10M's valid CH4 range widened to 52662–59662 (was 54662–57662,
#     then 52662–57662). Verified against your real data with the
#     final range: 69 of 568 standard rows fail, leaving 499 valid.
#     Every batch now has all 4 standard types present — no batches
#     flagged as missing a standard type.
#
# What changed from v6:
#   - Added a per-standard validity check based on acceptable
#     raw peak-area ranges (is_standard_valid() below). Any
#     standard row whose measured CH4/N2O peak falls outside its
#     expected range for that standard type gets dropped — it's
#     treated as a bad injection/run, not a usable calibration
#     point.
#   - After that filter, prints which batches are left missing
#     one or more of the 4 standard types entirely (1N500, 10M,
#     10N, ambient).
#
# What changed from v5:
#   - batch_47_12 and batch_47_15 are exempted from the
#     "drop standards from batches with an excluded sample" rule
#     (KEEP_STD_BATCHES below). Their excluded sample rows are
#     still dropped, but their standards are kept. Add more
#     batch names to that vector if other exceptions come up.
#
# What changed from v4:
#   - Sample-row exclusion now also drops year == 2026 (not just
#     2024) — so only year == 2025 samples survive, still on top
#     of the montna site exclusion.
#   - Standards are NO LONGER unconditionally kept: any standard
#     row is now dropped if its batch (GC_Run) contains at least
#     one excluded sample row (2024/2026 or montna), UNLESS the
#     batch is in KEEP_STD_BATCHES (see above).
# Drop this in place of your existing combine chunk in
# GC_Data_Wrangling.Rmd. Assumes Belt_Data, Peak_Area,
# Temperature, Headspace already exist (Belt_Data must come
# from the FIXED "Read belt files" chunk — this one relies on
# Belt_Data already being restricted to qualifying RGF/2025
# batches, with standard rows intact).
#
# What changed from v2:
#   - Added Date, Site, Year columns to the final file.
#     Date sits right after GC_Run (matches your Input.xlsx
#     template's column order). Site/Year are new. All three
#     come straight from Belt_Data and will be NA for standard
#     rows, same as Date always was in the historical template
#     (standards aren't tied to a specific plot/date/site).
#
# What changed from v1:
#   - Added Std_CH4_Peak back (raw CH4 peak area for standard
#     rows) — it was dropped because your original target
#     column list didn't include it, but VolCalc.R needs both
#     Std_CH4_Peak and Std_CH4_PPM to fit the CH4 calibration
#     curve (lm(Std_CH4_PPM ~ Std_CH4_Peak)).
#   - Removed the filter(site != "montna") / filter(year == 2025)
#     that was re-applied to Belt_Data here. Belt_Data is already
#     filtered upstream now, and standard rows still carry NA for
#     site/year (that's expected — see the Read chunk) so
#     re-filtering on those columns here would silently delete
#     every standard row again.
# ==========================================================

# ==========================================================
# 0. Known standard concentrations (ppm), by standard_name
#    Update this table if standards change or new ones are added.
# ==========================================================
standard_lookup <- tibble::tribble(
  ~standard_name, ~standard_color, ~CH4_ppm_known, ~N2O_ppm_known,
  "1N500",        "brown",         503,            1,
  "10M",          "red",           10.18,          0,
  "10N",          "blue",          0,              9.95,
  "ambient",      "green",         1.768,          0.299
) %>%
  mutate(standard_name = str_trim(standard_name))

# ==========================================================
# 0b. Acceptable raw peak-area ranges per standard, used below to
#     drop standard rows whose GC run looks bad for that standard
#     (contamination, bad injection, etc.) rather than a real
#     calibration point. Update these ranges if they change.
# ==========================================================
is_standard_valid <- function(standard_name, CH4, N2O) {
  case_when(
    standard_name == "1N500"   ~ CH4 >= 2695999 & CH4 <= 2995999,
    standard_name == "10M"     ~ CH4 >= 52662   & CH4 <= 59662 & N2O < 1000,
    standard_name == "10N"     ~ CH4 < 1000,
    standard_name == "ambient" ~ CH4 >= 9000    & CH4 <= 11000,
    TRUE ~ FALSE  # unrecognized standard_name — already flagged via unmatched_std
  )
}

# ==========================================================
# 1. Prepare Peak_Area
# ==========================================================
Peak_Area2 <- Peak_Area %>%
  mutate(
    batch     = as.character(batch),
    gc_number = as.numeric(gc_number)
  )

# ==========================================================
# 2. Prepare Belt_Data
#    + drop 2024/2026 and non-RGF SAMPLE rows
#    + drop any standard whose batch (GC_Run) had a dropped sample
#    + attach known ppm for standard rows via standard_name
# ==========================================================
Belt_Data2 <- Belt_Data %>%
  mutate(
    batch           = as.character(batch),
    gc_number       = as.numeric(gc_number),
    `GC_Run `       = batch,
    Plot            = as.character(plot_number),
    date_sampled_AD = as.Date(date_sampled_AD),
    Date            = date_sampled_AD,
    Site            = site,
    Year            = year,
    `Time `         = as.numeric(time),
    standard_name   = str_trim(standard_name),
    # normalized helper for the site check below only
    site_norm       = tolower(trimws(gsub("[_ ]+", "_", site))),
    exclude_sample  = sample_type == "sample" &
      (year %in% c(2024, 2026) | site_norm == "montna")
  )

# Batches exempted from the "drop standards from batches with an
# excluded sample" rule below — their standards are kept regardless.
KEEP_STD_BATCHES <- c("batch_47_12", "batch_47_15")

# Batches that had at least one excluded sample row — their
# standards get dropped too (unless exempted above), even if the
# batch also has samples that would otherwise qualify.
excluded_batches <- Belt_Data2 %>%
  filter(exclude_sample) %>%
  distinct(batch) %>%
  pull(batch) %>%
  setdiff(KEEP_STD_BATCHES)

Belt_Data2 <- Belt_Data2 %>%
  filter(
    !exclude_sample,
    !(sample_type == "standard" & batch %in% excluded_batches)
  ) %>%
  select(-exclude_sample, -site_norm) %>%
  left_join(
    standard_lookup %>% select(standard_name, CH4_ppm_known, N2O_ppm_known),
    by = "standard_name"
  )

# Flag any standard rows whose standard_name isn't in the lookup table,
# so a typo doesn't silently turn into NA in the final file.
unmatched_std <- Belt_Data2 %>%
  filter(sample_type == "standard", is.na(CH4_ppm_known), is.na(N2O_ppm_known))

if (nrow(unmatched_std) > 0) {
  warning(
    "Standard rows with no match in standard_lookup (check standard_name spelling): ",
    paste(unique(unmatched_std$standard_name), collapse = ", ")
  )
}

# ==========================================================
# 3. Add Peak_Area
# Match: batch + gc_number
# ==========================================================
final_flux <- Belt_Data2 %>%
  left_join(
    Peak_Area2 %>%
      select(batch, gc_number, CH4, N2O),
    by = c("batch", "gc_number")
  ) %>%
  mutate(
    # Raw sample peaks come straight from the GC (Peak_Area)
    CH4_Sample_Peak = if_else(sample_type == "sample", CH4, NA_real_),
    N2O_Sample_Peak = if_else(sample_type == "sample", N2O, NA_real_),

    # Std_CH4_Peak / Std_N2O_Peak = raw measured peak area for
    # standard rows (paired with the known PPM below to build
    # each gas's calibration curve)
    Std_CH4_Peak    = if_else(sample_type == "standard", CH4, NA_real_),
    Std_N2O_Peak    = if_else(sample_type == "standard", N2O, NA_real_),

    # Std_CH4_PPM / Std_N2O_PPM = known concentration of the standard run,
    # from standard_lookup (NOT the raw peak area)
    Std_CH4_PPM     = if_else(sample_type == "standard", CH4_ppm_known, NA_real_),
    Std_N2O_PPM     = if_else(sample_type == "standard", N2O_ppm_known, NA_real_),

    Sample_Type = case_when(
      sample_type == "sample"   ~ "Sample",
      sample_type == "standard" ~ "Std",
      TRUE ~ NA_character_
    )
  )

# Drop standard rows whose raw peak area is outside the acceptable
# range for that standard type (bad run — not a usable calibration
# point). Sample rows are never touched by this check.
final_flux <- final_flux %>%
  filter(
    sample_type != "standard" |
      is_standard_valid(standard_name, CH4, N2O)
  )

# Sanity check: you should see non-zero, non-NA counts for both gases
message(
  "Standard rows with a CH4 peak: ", sum(!is.na(final_flux$Std_CH4_Peak)),
  " | with a N2O peak: ", sum(!is.na(final_flux$Std_N2O_Peak)),
  " | with known CH4 ppm: ", sum(!is.na(final_flux$Std_CH4_PPM)),
  " | with known N2O ppm: ", sum(!is.na(final_flux$Std_N2O_PPM))
)

# Report any batch that, after the validity filter above, no longer
# has all 4 standard types (1N500, 10M, 10N, ambient) represented.
REQUIRED_STANDARDS <- c("1N500", "10M", "10N", "ambient")

batches_missing_standards <- final_flux %>%
  filter(sample_type == "standard") %>%
  distinct(batch, standard_name) %>%
  group_by(batch) %>%
  summarise(missing = paste(setdiff(REQUIRED_STANDARDS, standard_name), collapse = ", "), .groups = "drop") %>%
  filter(missing != "")

if (nrow(batches_missing_standards) > 0) {
  message("Batches missing at least one standard type after validity filtering:")
  print(batches_missing_standards, n = Inf)
} else {
  message("Every batch with standards has all 4 standard types present.")
}

# ==========================================================
# 4. Prepare Temperature
# Match: Plot + date_sampled_AD + Time
# ==========================================================
Temperature2 <- Temperature %>%
  filter(tolower(trimws(site)) == "river_garden", year == 2025) %>%
  mutate(
    Plot            = as.character(plot_number),
    date_sampled_AD = as.Date(date_sampled_AD),
    `Time `         = as.numeric(time)
  ) %>%
  select(
    Plot,
    date_sampled_AD,
    `Time `,
    Chamber_Temp_C = chamber_temp_C
  )

final_flux <- final_flux %>%
  left_join(
    Temperature2,
    by = c("Plot", "date_sampled_AD", "Time ")
  )

# ==========================================================
# 5. Prepare Headspace
# Match: Plot + date_sampled_AD
# ==========================================================
Headspace2 <- Headspace %>%
  filter(tolower(trimws(site)) == "river_garden", year == 2025) %>%
  mutate(
    Plot            = as.character(plot_number),
    date_sampled_AD = as.Date(date_sampled_AD)
  ) %>%
  select(
    Plot,
    date_sampled_AD,
    Headspace1_cm  = headspace1_cm,
    Headspace2_cm  = headspace2_cm,
    Headspace3_cm  = headspace3_cm,
    Headspace4_cm  = headspace4_cm,
    Extension_ft,
    Chamber_Radius = collar_radius_cm
  )

final_flux <- final_flux %>%
  left_join(
    Headspace2,
    by = c("Plot", "date_sampled_AD")
  )

# ==========================================================
# 6. Select final columns (names/order must match exactly —
#    "GC_Run " and "Time " keep their trailing space on purpose,
#    matching your existing Input.xlsx / RGF_2025.xlsx templates.
#    Std_CH4_Peak placed first to mirror that same template's
#    column order.)
# ==========================================================
final_flux <- final_flux %>%
  select(
    Std_CH4_Peak,
    Std_N2O_Peak,
    Std_CH4_PPM,
    Std_N2O_PPM,
    CH4_Sample_Peak,
    N2O_Sample_Peak,
    Sample_Type,
    `GC_Run `,
    Date,
    Plot,
    `Time `,
    Chamber_Temp_C,
    Headspace1_cm,
    Headspace2_cm,
    Headspace3_cm,
    Headspace4_cm,
    Extension_ft,
    Chamber_Radius,
    Site,
    Year
  )

# ==========================================================
# 7. Export Excel
# ==========================================================
write.xlsx(
  final_flux,
  "../Data/Flux_Calc/ImportFiles/final_flux_input.xlsx",
  overwrite = TRUE
)
