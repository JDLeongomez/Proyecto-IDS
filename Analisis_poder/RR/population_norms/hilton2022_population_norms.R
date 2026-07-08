# hilton2022_population_norms.R
#
# Compute between-speaker population SDs for f0 mean, f0 SD, and formant
# frequencies (VTL proxy) from Hilton & Moser et al. (2022) IDS speech data,
# and derive ±1 SD manipulation targets for the Praat stimulus generation script.
#
# Source: Hilton, C. B., Moser, C. J., et al. (2022). Acoustic regularities in
#   infant-directed speech and song across cultures. Nature Human Behaviour, 6(11).
#   https://doi.org/10.1038/s41562-022-01410-x
#   Audio corpus: https://doi.org/10.5281/zenodo.5525161
#   GitHub repo:  https://github.com/themusiclab/infant-speech-song/
#
# Using a single cross-cultural source (n = 21 societies) for all three parameters
# ensures methodological consistency. Cox et al. (2023) and Broesch & Bryant (2014)
# serve as cross-checks for f0 mean and f0 SD respectively (see console output).
#
# Manipulation magnitude: ±1 SD (not ±1.5 SD). Perceptual piloting on a Colombian
# Mestizo female IDS recording (MES02B) showed that ±1.5 SD produced severe PSOLA
# artefacts (octave jumps, mechanical quality); ±1 SD produced natural-sounding output.
#
# Derived manipulation targets (implemented in IDS_manipulation.praat):
#   f0 mean : ±51 Hz  (1 × 50.8 Hz between-speaker SD; female IDS speech)
#   f0 SD   : ±28 Hz  (1 × 28.4 Hz between-speaker SD; female IDS speech)
#   Ffreq   : ±4.6%   (1 × CV 0.046 of geometric mean F1-F4; female IDS speech)
#
# These SDs are used to calibrate the ±1 SD acoustic manipulations in:
#   Leongomez, Vasquez-Amezquita & Puts -- Babies' acoustic preferences in
#   infant-directed speech [Stage 1 Registered Report]
#
# Inputs:
#   1. acoustics-editedAudio.csv  -- Hilton et al. GitHub repo:
#      https://github.com/themusiclab/infant-speech-song/blob/main/data/acoustics-editedAudio.csv
#      Contains per-recording acoustic measures including praat_f0_mean and praat_f0_std.
#      1,615 rows (all four vocalization types: IDS speech, IDS song, ADS speech, ADS song).
#
#   2. stimuli-info.csv           -- Hilton et al. GitHub repo:
#      https://github.com/themusiclab/infant-speech-song/blob/main/data/stimuli-info.csv
#      Contains: id, File Duration, Culture, Location, Gender.
#      Used to filter to female IDS speech speakers (Gender == "F", ids == "Yes", song == "0").
#
#   3. formant_summary.csv        -- output of extract_formants_IDS.praat
#      One row per IDS speech file (vocalization type B from Zenodo corpus).
#      Contains: filename, f0_mean, f0_sd, f1_mean, f2_mean, f3_mean, f4_mean.
#      F3 and F4 not available in the Hilton et al. CSV (their script only extracted F1-F2);
#      re-extracted using an adapted version of their Praat script (extract_formants_IDS.praat).
#
# Outputs:
#   - Console summary of between-speaker SDs, cross-checks, and manipulation targets
#   - population_norms.csv -- one row per parameter with pop mean, pop SD, and target
#   - population_norms_distributions.png -- histograms of between-speaker distributions

library(tidyverse)

# =============================================================================
# 1. Load Hilton et al. acoustic CSV
# =============================================================================

acoustics_url <- paste0(
  "https://raw.githubusercontent.com/themusiclab/",
  "infant-speech-song/main/data/acoustics-editedAudio.csv"
)

dat <- read_csv(acoustics_url) |>
  mutate(
    across(id_person:song, as.factor),
    infantdir = factor(infantdir, levels = c(0, 1), labels = c("No", "Yes")),
    song      = factor(ids,       levels = c(0, 1), labels = c("No", "Yes"))
  )

# =============================================================================
# 2. Load stimuli metadata (includes Gender)
# =============================================================================

stimuli_info <- read_csv(paste0(
  "https://raw.githubusercontent.com/themusiclab/",
  "infant-speech-song/main/data/stimuli-info.csv"
)) |>
  select(id, Gender, Culture, Location)

# =============================================================================
# 3. Load Praat output (F1-F4 per file)
# =============================================================================

formants <- read_csv("population_norms/formant_summary.csv") |>
  rename(id = filename) |>
  # Zero values were undefined (unvoiced) -- set to NA
  mutate(across(f0_mean:f4_mean, ~ if_else(.x == 0, NA_real_, .x)))

# =============================================================================
# 4. Join everything, filter to IDS speech + female speakers
# =============================================================================

dat_full <- dat |>
  left_join(stimuli_info, by = "id") |>
  inner_join(formants, by = "id", suffix = c("_hilton", "_praat"))

dat_ids_speech_f <- dat_full |>
  filter(
    infantdir == "Yes",   # infant-directed
    song      == "No",     # speech not song (song is a factor: "0" = speech)
    Gender    == "F"      # female speakers only
  )

cat(sprintf(
  "\nAfter filtering: %d IDS speech recordings from %d female speakers\n",
  nrow(dat_ids_speech_f),
  n_distinct(dat_ids_speech_f$id_person)
))

# Cultural breakdown for transparency
cat("\nRecordings per culture:\n")
dat_ids_speech_f |>
  count(Culture, Location) |>
  print(n = Inf)

# =============================================================================
# 5. One row per speaker
# =============================================================================

by_speaker <- dat_ids_speech_f |>
  group_by(id_person, Culture, Location) |>
  summarise(
    n_recordings = n(),
    # f0 from the Hilton et al. CSV (validated, matches paper)
    f0_mean_hz   = mean(praat_f0_mean, na.rm = TRUE),
    f0_sd_hz     = mean(praat_f0_std,  na.rm = TRUE),
    # formants from new Praat extraction
    f1_mean_hz   = mean(f1_mean, na.rm = TRUE),
    f2_mean_hz   = mean(f2_mean, na.rm = TRUE),
    f3_mean_hz   = mean(f3_mean, na.rm = TRUE),
    f4_mean_hz   = mean(f4_mean, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # VTL proxy: geometric mean of F1-F4
  # (standard in the vocal tract length literature; see Puts et al. 2012)
  mutate(
    vtl_proxy_hz = (f1_mean_hz * f2_mean_hz * f3_mean_hz * f4_mean_hz)^(1/4)
  )

cat(sprintf("\nSpeakers in analysis: %d\n", nrow(by_speaker)))

# =============================================================================
# 6. Between-speaker SDs and manipulation targets (±1 SD)
# =============================================================================

norms <- by_speaker |>
  summarise(
    # --- f0 mean ---
    f0mean_pop_mean  = mean(f0_mean_hz, na.rm = TRUE),
    f0mean_pop_sd    = sd(f0_mean_hz,   na.rm = TRUE),
    f0mean_manip_hz  = 1 * sd(f0_mean_hz, na.rm = TRUE),
    
    # --- f0 SD ---
    f0sd_pop_mean    = mean(f0_sd_hz, na.rm = TRUE),
    f0sd_pop_sd      = sd(f0_sd_hz,   na.rm = TRUE),
    f0sd_manip_hz    = 1 * sd(f0_sd_hz, na.rm = TRUE),
    
    # --- VTL proxy (geometric mean F1-F4) ---
    vtl_pop_mean     = mean(vtl_proxy_hz, na.rm = TRUE),
    vtl_pop_sd       = sd(vtl_proxy_hz,   na.rm = TRUE),
    vtl_cv           = sd(vtl_proxy_hz, na.rm = TRUE) / mean(vtl_proxy_hz, na.rm = TRUE),
    vtl_manip_factor = 1 * sd(vtl_proxy_hz, na.rm = TRUE) / mean(vtl_proxy_hz, na.rm = TRUE)
  )

cat("\n=== Population norms: Hilton et al. (2022) IDS speech, female speakers ===\n\n")
cat(sprintf(
  "f0 mean:   pop mean = %.1f Hz | pop SD = %.1f Hz | manip target = ±%.0f Hz\n",
  norms$f0mean_pop_mean, norms$f0mean_pop_sd, norms$f0mean_manip_hz
))
cat("           cross-check: Cox et al. (2023) ~40 Hz SD -> ±60 Hz\n\n")
cat(sprintf(
  "f0 SD:     pop mean = %.1f Hz | pop SD = %.1f Hz | manip target = ±%.0f Hz\n",
  norms$f0sd_pop_mean, norms$f0sd_pop_sd, norms$f0sd_manip_hz
))
cat("           cross-check: Broesch & Bryant (2014) ~24 Hz SD -> ±36 Hz\n\n")
cat(sprintf(
  "VTL proxy: pop mean = %.1f Hz | pop SD = %.1f Hz | CV = %.3f | manip factor = ±%.3f (%.1f%%)\n",
  norms$vtl_pop_mean, norms$vtl_pop_sd,
  norms$vtl_cv, norms$vtl_manip_factor, norms$vtl_manip_factor * 100
))
cat("           current target value in Praat script: ±4.6%\n\n")

# =============================================================================
# 7. Distributions
# =============================================================================

by_speaker |>
  select(f0_mean_hz, f0_sd_hz, vtl_proxy_hz) |>
  pivot_longer(everything(), names_to = "parameter", values_to = "value") |>
  mutate(parameter = recode(parameter,
                            f0_mean_hz   = "f0 mean (Hz)",
                            f0_sd_hz     = "f0 SD (Hz)",
                            vtl_proxy_hz = "VTL proxy: geom. mean F1-F4 (Hz)"
  )) |>
  ggplot(aes(x = value)) +
  geom_histogram(bins = 20, fill = "steelblue", colour = "white") +
  facet_wrap(~ parameter, scales = "free") +
  labs(
    title = "Between-speaker distributions: IDS speech, female speakers",
    subtitle = "Hilton & Moser et al. (2022)",
    x = NULL, y = "n speakers"
  ) +
  theme_minimal()

ggsave("population_norms/population_norms_distributions.png", width = 9, height = 3.5, dpi = 150)

# =============================================================================
# 8. Save norms to CSV
# =============================================================================

norms_long <- tibble(
  parameter    = c("f0_mean", "f0_sd", "vtl_proxy"),
  source       = "Hilton et al. (2022), IDS speech, female speakers",
  n_speakers   = nrow(by_speaker),
  pop_mean_hz  = c(norms$f0mean_pop_mean, norms$f0sd_pop_mean,  norms$vtl_pop_mean),
  pop_sd_hz    = c(norms$f0mean_pop_sd,   norms$f0sd_pop_sd,    norms$vtl_pop_sd),
  manip_target = c(norms$f0mean_manip_hz, norms$f0sd_manip_hz,  NA),
  manip_factor = c(NA, NA, norms$vtl_manip_factor),
  notes = c(
    "Hz; cross-check: Cox et al. (2023) ~40 Hz SD -> ±60 Hz",
    "Hz; cross-check: Broesch & Bryant (2014) ~24 Hz SD -> ±36 Hz",
    "Geometric mean F1-F4 (Hz); factor used as proportional scaling in Praat script"
  )
)

write_csv(norms_long, "population_norms/population_norms.csv")
write_csv(by_speaker, "population_norms/by_speaker.csv")
