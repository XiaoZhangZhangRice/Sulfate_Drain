# ==========================================================
# "Read belt files" chunk — fixed version (v2)
#
# Two separate bugs were compounding here:
#
# 1) list.files("Data/Flux_Calc/GC_Data/Belt_Data", ...) matched
#    ZERO files (wrong working-directory convention — see prior
#    fix). map_dfr() over an empty vector returns a 0-column
#    tibble, so .id = "File_ID" never gets created, causing the
#    "object 'File_ID' not found" error.
#
# 2) Standard rows have site/year/date left BLANK in every belt
#    file (only sample rows carry that metadata) — confirmed by
#    inspecting the actual files. filter(site != "montna") and
#    filter(year == 2025) both drop a row when the value is NA,
#    so every single standard row was being deleted before it
#    ever reached the Peak_Area join. That's the real cause of
#    "all my standard peaks are not there."
#
#    On top of that, some batch files mix BOTH sites in one
#    file (e.g. batch_47_12, batch_47_15), and site spelling is
#    inconsistent across files ("river_garden" vs "river garden").
#    So standards can't just inherit "the file's site" — a batch
#    can legitimately contain samples from two sites.
#
# Fix: keep a batch (and all its standards) if ANY sample row in
# that batch is RGF/2025. Within a kept batch, keep standard rows
# unconditionally (they calibrate the whole GC run) and keep only
# the RGF/2025 sample rows (drop stray non-RGF/non-2025 samples
# that happen to share the same batch file).
# ==========================================================

belt_files <- list.files(
  "GC_Data/Belt_Data",
  pattern = "\\.xlsx$",
  full.names = TRUE
)

if (length(belt_files) == 0) {
  stop(
    "No Belt_Data files found. Looked in: ",
    normalizePath("GC_Data/Belt_Data", mustWork = FALSE),
    "\nCurrent working directory (getwd()): ", getwd(),
    "\nSet your working directory to the Flux_Calc folder ",
    "(e.g. setwd(...) or open the .Rproj there) and re-run."
  )
}

Belt_Data_raw <- belt_files %>%
  map_dfr(
    ~ read_excel(.x) %>%
      mutate(
        plot_number = as.character(plot_number),
        Source_File = basename(.x)
      )
  ) %>%
  mutate(
    batch = gsub("_belt_data\\.xlsx", "", Source_File),
    # normalize spelling ("river garden" vs "river_garden", stray case/space)
    site_norm = tolower(trimws(gsub("[_ ]+", "_", site)))
  )

# Batches that have at least one RGF/2025 SAMPLE row
qualifying_batches <- Belt_Data_raw %>%
  filter(sample_type == "sample", site_norm == "river_garden", year == 2025) %>%
  distinct(batch) %>%
  pull(batch)

Belt_Data <- Belt_Data_raw %>%
  filter(
    batch %in% qualifying_batches,
    sample_type == "standard" | (site_norm == "river_garden" & year == 2025)
  ) %>%
  select(-site_norm)

message(
  nrow(Belt_Data), " rows kept from ", length(qualifying_batches), " qualifying batches (",
  sum(Belt_Data$sample_type == "standard"), " standard rows, ",
  sum(Belt_Data$sample_type == "sample"), " sample rows)."
)

str(Belt_Data)
