# ==========================================================
# "Read temperature" chunk — with gap-filling
#
# Added: for a given date + time (0/21/42/63), if a plot is
# missing chamber_temp_C, fill it with the mean chamber_temp_C
# of the other plots sampled at that same date+time. This covers
# the 49 rows we found with genuinely missing readings (2025-05-09,
# 06-27, 07-28, 08-08, 08-15) and the 2025-06-20/Plot 101 case
# where the reading exists but was tagged "montna" instead of
# "river_garden" in this file (that row's real value now gets used
# to help fill its own group's mean once the site typo is fixed).
#
# Also fixed: filter(site != "monta") — the real value is "montna"
# (note the second "n"), so this filter was previously a no-op
# and let every site through. Now actually excludes montna.
# Verified against your real file: no date+time group is 100%
# missing, so every gap has at least one other plot to average.
# ==========================================================

Temperature <- read_xlsx("../Data/Flux_Calc/Field_Data/temperature_data_raw.xlsx", sheet = 1)

Temperature <- Temperature %>%
  filter(site != "montna") %>%   # was "monta" (typo — matched nothing)
  filter(year == 2025) %>%
  group_by(date_sampled_AD, time) %>%
  mutate(
    n_available    = sum(!is.na(chamber_temp_C)),
    chamber_temp_C = if_else(
      is.na(chamber_temp_C),
      mean(chamber_temp_C, na.rm = TRUE),
      chamber_temp_C
    )
  ) %>%
  ungroup()

# Sanity check: flag any date+time group where EVERY plot was
# missing a reading (gap-fill can't help there — mean of nothing
# is NaN, and the code above would fail to fix that row).
still_missing <- Temperature %>% filter(n_available == 0)
if (nrow(still_missing) > 0) {
  warning(
    nrow(still_missing), " rows have no reading anywhere in their date+time group ",
    "(gap-fill couldn't help). Dates affected: ",
    paste(unique(format(still_missing$date_sampled_AD)), collapse = ", ")
  )
}

Temperature <- Temperature %>% select(-n_available)

str(Temperature)
