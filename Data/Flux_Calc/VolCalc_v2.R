# ==========================================================
# VolCalc_v2.R
#
# Rewrite of VolCalc.R for a single consolidated ImportFiles/*.xlsx
# (all batches/GC_Runs in one file), while fixing the issues found
# while debugging final_flux_input.xlsx:
#
#  1. CALIBRATION IS NOW FIT PER GC_Run, NOT PER FILE.
#     The original script fit one CH4 curve and one N2O curve using
#     every standard row in the whole file. With many batches in one
#     file, that pools standards run on different days into a single
#     curve. Per-batch slopes turned out to be close (CH4 ~5% spread,
#     N2O ~8% spread) so this wasn't wildly wrong, but there's no
#     reason to accept even that much bias when each batch's own
#     standards are right there. This version fits a fresh CH4 and
#     N2O model per GC_Run and only applies it to that GC_Run's own
#     sample rows.
#
#  2. THE FLUX LOOP USES EACH GROUP'S ACTUAL SAMPLED TIMES.
#     The original script assumed every plot was measured at exactly
#     time <- c(0,21,42,63) and just walked the file in blind blocks
#     of 4 rows. That breaks in two ways: (a) if any plot doesn't
#     have exactly 4 rows, every subsequent block in the file shifts
#     out of alignment silently, and (b) some of your plots (e.g.
#     batch_47_12's B1/B2/B3 on 2025-05-02) were actually sampled on
#     a 0/9/18/27 schedule, so fitting them against 0/21/42/63 gave a
#     wrong slope. This version groups explicitly by (GC_Run, Plot,
#     Date) and fits against whatever Time values that group actually
#     has.
#
#  3. NON-DETECT IS NOW A VISIBLE COLUMN, NOT A SILENT OVERRIDE.
#     The original results table had "N20Detection"/"CH4Detection"
#     columns that were hardcoded to "PASS" and never actually
#     updated — the real non-detect signal only showed up as a
#     silent Slope/Flux of 0, indistinguishable from a failed
#     linearity test. This version reports "NONDETECT" vs "PASS" per
#     gas per group so you can tell which of the two reasons zeroed
#     a given flux.
#
#  4. DEFENSIVE ABOUT MISSING DATA.
#     Any group with fewer than 2 usable concentration points, or a
#     batch with fewer than 2 usable standards, gets marked
#     "MISSING" instead of crashing the whole run (this is what
#     used to halt on "missing value where TRUE/FALSE needed").
#
#  5. Same core science/constants as the original: same detection
#     threshold (0.000183), same linearity R^2 cutoffs (0.845 full,
#     0.8545 fallback), same flux conversion constant (240*60), same
#     concentration formula. Change the constants below if you want
#     to revisit any of those — nothing else in the script needs to
#     change to pick up a new threshold.
# ==========================================================

library(readxl)
library(writexl)
library(openxlsx)
library(dplyr)

# ---- Constants (unchanged from VolCalc.R unless noted above) ----
DETECTION_THRESHOLD   <- 0.000183
LINEARITY_R2_FULL     <- 0.845
LINEARITY_R2_FALLBACK <- 0.8545
FLUX_TIME_CONVERSION  <- 240 * 60
MOLAR_VOLUME_STP      <- 22.4     # L/mol at STP
CH4_MW                <- 0.016043 # g/mol
N2O_MW                <- 0.044014 # g/mol
CALIBRATION_R2_WARN    <- 0.95    # warn if a batch's calibration R^2 is below this

# ==========================================================
# Per-group gas fit: detection test + linearity test (with the
# original's 4-point "drop one, keep best 3" fallback), generalized
# to use whichever Time values the group actually has.
# ==========================================================
fit_gas <- function(times, conc) {
  ok    <- !is.na(times) & !is.na(conc)
  times <- times[ok]
  conc  <- conc[ok]
  n     <- length(times)

  if (n < 2) {
    return(list(rsq = NA_real_, grad = NA_real_, lin = "MISSING", detect = "MISSING"))
  }

  o     <- order(times)
  times <- times[o]
  conc  <- conc[o]

  # PASS if ANY later point clears the threshold vs. the first
  # reading; NONDETECT only if every later point stays within it.
  # (Previously: any() single flat comparison zeroed the whole
  # group even when other points clearly showed a real change.)
  base   <- conc[1]
  detect <- if (any(abs(conc[-1] - base) >= DETECTION_THRESHOLD)) "PASS" else "NONDETECT"

  fit  <- tryCatch(lm(conc ~ times), error = function(e) NULL)
  if (is.null(fit)) {
    return(list(rsq = NA_real_, grad = NA_real_, lin = "MISSING", detect = detect))
  }
  rsq  <- summary(fit)$r.squared
  grad <- unname(coef(fit)[2])

  lin <- "MISS"
  if (!is.na(rsq) && rsq > LINEARITY_R2_FULL) {
    lin <- "LIN"
  } else if (n == 4) {
    # Same fallback as the original script: drop each of the 4
    # points in turn, keep whichever 3-point fit has the best R^2.
    best_rsq <- -Inf
    best_grad <- grad
    for (drop in 1:4) {
      t3 <- times[-drop]
      c3 <- conc[-drop]
      f3 <- tryCatch(lm(c3 ~ t3), error = function(e) NULL)
      if (is.null(f3)) next
      r3 <- summary(f3)$r.squared
      if (!is.na(r3) && r3 > best_rsq) {
        best_rsq  <- r3
        best_grad <- unname(coef(f3)[2])
      }
    }
    if (is.finite(best_rsq) && best_rsq > LINEARITY_R2_FALLBACK) {
      lin  <- "LIN"
      rsq  <- best_rsq
      grad <- best_grad
    }
  }

  if (lin == "MISS")        grad <- 0
  if (detect == "NONDETECT") grad <- 0

  list(rsq = rsq, grad = grad, lin = lin, detect = detect)
}

# ==========================================================
# Main loop over every file in ImportFiles/
# ==========================================================
files <- list.files(path = file.path(getwd(), "ImportFiles"))
files <- files[grepl("\\.xlsx$", files) & !startsWith(files, "~$")]
message(length(files), " file(s) to process: ", paste(files, collapse = ", "))

for (rowname in files) {

  filepath <- file.path(getwd(), "ImportFiles", rowname)
  message("\n=== Processing: ", filepath, " ===")

  df <- read_excel(filepath)

  # Normalize the trailing-space column names used by your export
  # templates so this script works on both the old single-batch
  # templates (Input.xlsx, RGF_2025.xlsx) and the new consolidated
  # final_flux_input.xlsx.
  if ("GC_Run " %in% names(df)) df <- df %>% rename(GC_Run = `GC_Run `)
  if ("Time "   %in% names(df)) df <- df %>% rename(Time   = `Time `)
  if (!"GC_Run" %in% names(df)) {
    # No batch column at all — treat the whole file as one batch,
    # matching the original script's behavior.
    df$GC_Run <- rowname
  }

  # ----------------------------------------------------------
  # Chamber volume / area — unchanged, per-row geometry, not
  # batch-dependent.
  # ----------------------------------------------------------
  headspace_mean <- rowMeans(
    df[, c("Headspace1_cm", "Headspace2_cm", "Headspace3_cm", "Headspace4_cm")],
    na.rm = TRUE
  )
  extension_cm   <- df$Extension_ft * 30.48
  chamber_Height <- extension_cm + 7.62 + headspace_mean
  baseArea       <- pi * df$Chamber_Radius ^ 2
  ChamberVolCm3  <- baseArea * chamber_Height
  ChamberVolL    <- ChamberVolCm3 / 1000
  BaseAreaM2     <- baseArea / 10000  # cm^2 -> m^2 (area, not volume -- was mislabeled "M3" in the original script)
  VolAreaRatio   <- ChamberVolL / BaseAreaM2

  df$HeadspaceMean_cm <- headspace_mean
  df$Extension_cm     <- extension_cm
  df$Chamber_Height   <- chamber_Height
  df$Total_Volume_cm3 <- ChamberVolCm3
  df$Total_Volume_L   <- ChamberVolL
  df$Base_Area        <- baseArea
  df$Base_Area_m2     <- BaseAreaM2
  df$VolumeRatio      <- VolAreaRatio

  # ----------------------------------------------------------
  # Per-GC_Run calibration
  # ----------------------------------------------------------
  df$CH4_Output <- NA_real_
  df$N2O_Output <- NA_real_

  batches <- unique(df$GC_Run)

  for (b in batches) {
    rows_b <- which(df$GC_Run == b)

    std_ch4 <- df[rows_b, ]
    std_ch4 <- std_ch4[!is.na(std_ch4$Std_CH4_Peak) & !is.na(std_ch4$Std_CH4_PPM), ]
    if (nrow(std_ch4) < 2) {
      warning("Batch ", b, ": fewer than 2 usable CH4 standards — CH4_Output will be NA for this batch's samples.")
    } else {
      CH4model <- lm(Std_CH4_PPM ~ Std_CH4_Peak, data = std_ch4)
      r2 <- summary(CH4model)$r.squared
      if (!is.na(r2) && r2 < CALIBRATION_R2_WARN) {
        warning("Batch ", b, ": CH4 calibration R^2 = ", round(r2, 4), " (below ", CALIBRATION_R2_WARN, ")")
      }
      CH4_m <- unname(coef(CH4model)[2])
      CH4_c <- unname(coef(CH4model)[1])
      idx <- rows_b[!is.na(df$CH4_Sample_Peak[rows_b])]
      df$CH4_Output[idx] <- CH4_m * df$CH4_Sample_Peak[idx] + CH4_c
    }

    std_n2o <- df[rows_b, ]
    std_n2o <- std_n2o[!is.na(std_n2o$Std_N2O_Peak) & !is.na(std_n2o$Std_N2O_PPM), ]
    if (nrow(std_n2o) < 2) {
      warning("Batch ", b, ": fewer than 2 usable N2O standards — N2O_Output will be NA for this batch's samples.")
    } else {
      N2Omodel <- lm(Std_N2O_PPM ~ Std_N2O_Peak, data = std_n2o)
      r2 <- summary(N2Omodel)$r.squared
      if (!is.na(r2) && r2 < CALIBRATION_R2_WARN) {
        warning("Batch ", b, ": N2O calibration R^2 = ", round(r2, 4), " (below ", CALIBRATION_R2_WARN, ")")
      }
      N2O_m <- unname(coef(N2Omodel)[2])
      N2O_c <- unname(coef(N2Omodel)[1])
      idx <- rows_b[!is.na(df$N2O_Sample_Peak[rows_b])]
      df$N2O_Output[idx] <- N2O_m * df$N2O_Sample_Peak[idx] + N2O_c
    }

    # Optional QC plots per batch — comment out if you don't want
    # ~2 plots x number-of-batches added to Rplots.pdf each run.
    if (nrow(std_ch4) >= 2) {
      plot(std_ch4$Std_CH4_Peak, std_ch4$Std_CH4_PPM, type = "o",
           xlab = "CH4 Peak", ylab = "CH4 PPM",
           main = paste0(rowname, " / ", b, " — CH4 Calibration"))
    }
    if (nrow(std_n2o) >= 2) {
      plot(std_n2o$Std_N2O_Peak, std_n2o$Std_N2O_PPM, type = "o",
           xlab = "N2O Peak", ylab = "N2O PPM",
           main = paste0(rowname, " / ", b, " — N2O Calibration"))
    }
  }

  # ----------------------------------------------------------
  # Concentration conversion — same formula as the original script
  # ----------------------------------------------------------
  N2O_Vol <- (760 * MOLAR_VOLUME_STP * (273 + df$Chamber_Temp_C)) / (760 * 273)
  df$N2O_Volume        <- N2O_Vol
  df$N2O_Concentration <- df$N2O_Output / N2O_Vol * N2O_MW

  CH4_Vol <- (760 * MOLAR_VOLUME_STP * (273 + df$Chamber_Temp_C)) / (760 * 273)
  df$CH4_Volume        <- CH4_Vol
  df$CH4_Concentration <- df$CH4_Output / CH4_Vol * CH4_MW

  # ----------------------------------------------------------
  # Flux, grouped by (GC_Run, Plot, Date) using each group's own
  # actual Time values.
  # ----------------------------------------------------------
  flux_data <- df %>%
    filter(!is.na(Plot)) %>%
    select(GC_Run, Plot, Date, Time, N2O_Concentration, CH4_Concentration, VolumeRatio)

  group_keys <- flux_data %>% distinct(GC_Run, Plot, Date)

  results <- vector("list", nrow(group_keys))

  for (i in seq_len(nrow(group_keys))) {
    key <- group_keys[i, ]

    rows_g <- flux_data %>%
      filter(
        GC_Run == key$GC_Run,
        Plot   == key$Plot,
        (is.na(Date) & is.na(key$Date)) | (!is.na(Date) & !is.na(key$Date) & Date == key$Date)
      )

    if (any(duplicated(rows_g$Time))) {
      warning("GC_Run ", key$GC_Run, ", Plot ", key$Plot, ", Date ", format(key$Date),
              ": duplicate Time values in this group — check the merge for a many-to-many join.")
    }

    volumeratio <- rows_g$VolumeRatio[1]

    n2o_fit <- fit_gas(rows_g$Time, rows_g$N2O_Concentration)
    ch4_fit <- fit_gas(rows_g$Time, rows_g$CH4_Concentration)

    n2o_flux <- if (is.na(n2o_fit$grad) || is.na(volumeratio)) NA_real_ else n2o_fit$grad * volumeratio * FLUX_TIME_CONVERSION
    ch4_flux <- if (is.na(ch4_fit$grad) || is.na(volumeratio)) NA_real_ else ch4_fit$grad * volumeratio * FLUX_TIME_CONVERSION

    results[[i]] <- tibble(
      GC_Run        = key$GC_Run,
      Plot          = key$Plot,
      Date          = format(key$Date),
      N_points      = nrow(rows_g),
      N20Detection  = n2o_fit$detect,
      CH4Detection  = ch4_fit$detect,
      N20_Rsq       = n2o_fit$rsq,
      CH4_Rsq       = ch4_fit$rsq,
      N20_Linearity = n2o_fit$lin,
      CH4_Linearity = ch4_fit$lin,
      N20_Slope     = n2o_fit$grad,
      CH4_Slope     = ch4_fit$grad,
      N20_Flux      = n2o_flux,
      CH4_Flux      = ch4_flux
    )
  }

  results <- bind_rows(results)

  message(
    "  Groups: ", nrow(results),
    " | CH4 non-detect: ", sum(results$CH4Detection == "NONDETECT", na.rm = TRUE),
    " | CH4 missing: ", sum(results$CH4Detection == "MISSING", na.rm = TRUE),
    " | N2O non-detect: ", sum(results$N20Detection == "NONDETECT", na.rm = TRUE),
    " | N2O missing: ", sum(results$N20Detection == "MISSING", na.rm = TRUE)
  )

  out_name <- paste0(tools::file_path_sans_ext(rowname), "_results.xlsx")
  write_xlsx(results, file.path(getwd(), "Results", out_name))
  message("  Wrote ", file.path("Results", out_name))
}

message("\nDone.")
