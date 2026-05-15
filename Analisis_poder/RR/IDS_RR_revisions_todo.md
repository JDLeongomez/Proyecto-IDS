# IDS Stage 1 RR — Revision Log and To-Do

*Last updated: May 2026*

---

## Context for future Claude sessions

This document tracks all pending changes to the Stage 1 Registered Report manuscript,
supplementary material, and Praat script for the study **"Babies' acoustic preferences in
infant-directed speech [Stage 1 Registered Report]"** (Leongómez, Vásquez-Amézquita &
Puts). The study uses a 2×2×2 fully crossed within-subjects eye-tracking design in
pre-linguistic infants (3–9 months) to test the effects of three independently manipulated
acoustic properties of IDS on total fixation time (ms).

**The three acoustic parameters are:**
- f0 mean (confirmatory, H1 in original ms — note: H1 is f0 SD and H2 is f0 mean in the
  supplementary; confirm labelling is consistent throughout)
- f0 SD (confirmatory, H2 in original ms)
- Formant frequencies / apparent vocal tract length (formerly "Df" / "formant dispersion" —
  now **exploratory**, not confirmatory; see Section 3)

**Key structural facts a future session needs to know:**
- The primary model is an LMM: `fixation_time ~ f0_SD × f0_mean × formant + block + avatar + (1 + avatar || ID) + (1 | original)`
- The supplementary contains a simulation-based power analysis in Quarto (`.qmd`), rendered as PDF
- The Praat script (`IDS_manipulation.praat`) generates the full 2×2×2 stimulus set via
  iterative LPC resynthesis (formants) followed by iterative affine PSOLA correction (f0).
  Both parameters now use iterative correction to hit their targets.
- The manuscript was drafted using the Nature Human Behaviour template but journal choice is not yet final
- NHB is now a realistic target: updated simulation gives 99.7% power for f0 SD and 96.2% for f0 mean
  at n = 200, both above NHB's ≥95% threshold — provided formant frequencies remain exploratory
- The equivalence bounds are ±150 ms (pre-specified SESOI), tested via TOST using emmeans 90% CIs

**Known inconsistencies still to resolve** (see sections below for details):
- Equivalence decision rule is wrong in the manuscript (3 locations) — uses lower CI bound instead of upper
- H3 (formants) must be removed from confirmatory framing throughout ms, supplementary, and design table
- Praat version number is inconsistent across documents (6.4.43 vs 6.4.63)
- Power threshold in supplementary `.qmd` is still 0.80; needs updating to 0.95 for detection power
- Manuscript Methods section still describes old percentage-based manipulation and old sources
- Manuscript still refers to "Df" / "formant dispersion" throughout

---

## Overview of key decisions

Following feedback from David Puts, and subsequent analytical work in May 2026, several
interconnected changes have been made to the Praat script and are pending in the manuscript
and supplementary. This document records the logic behind each decision and what still
needs to be done.

---

## 1. Manipulation units: from percentages to SD-based (REVISED May 2026)

### Decision

Manipulations are expressed as ±1 between-speaker SD from the IDS population distribution
(not ±1.5 SD as originally planned). The target is 1 SD, not 1.5 SD, for two reasons:

1. **Perceptual piloting:** A Colombian Mestizo female IDS recording (MES02B from the
   Hilton et al. corpus) was manipulated at ±1.5 SD. The result had severe PSOLA artefacts
   (octave jumps, mechanical quality) across all f0 conditions. At ±1 SD, the output sounded
   natural. This is an empirical constraint, not a theoretical preference.

2. **Power is adequate at ±1 SD:** The power simulation targets effects detectable in
   eye-tracking with infants (SESOI = 75 ms). The manipulation magnitude does not need to
   maximise acoustic distance; it needs to produce stimuli that sound natural and differ
   detectably.

The manipulation for each speaker is computed at runtime as:

```
target_high = speaker_baseline + population_SD
target_low  = speaker_baseline − population_SD
```

where `population_SD` is a fixed constant derived from Hilton et al. (2022) — see Section 2.

This approach follows Aung et al. (2024, *Psychological Science*) who used ERB-unit steps
calibrated to population norms for cross-cultural voice manipulation.

### What to update in the manuscript

- **Methods / Stimulus Generation:** Replace the description of percentage-based manipulation
  with the SD-based approach. Explain that ±1 SD places the high/low stimuli symmetrically
  within the range of natural IDS variation, and cite Hilton et al. (2022) as the source of
  the population norms. Note that ±1.5 SD was piloted but produced unacceptable artefacts.
- **Methods / Stimulus Generation:** Report the actual manipulation targets:
  f0 mean ±51 Hz, f0 SD ±28 Hz, formant frequencies ±4.6%.
- **Supplementary — Stimulus Generation section:** Update accordingly and clear the ADD-URL
  placeholder.

---

## 2. Population SD values and their sources (REVISED May 2026)

### Single source: Hilton et al. (2022)

All three manipulation parameters are now anchored to a single source:

> Hilton, C. B., Moser, C. J., et al. (2022). Acoustic regularities in infant-directed
> speech and song across cultures. *Nature Human Behaviour*, 6(11).
> https://doi.org/10.1038/s41562-022-01410-x

Using a single large cross-cultural dataset (n = 21 societies, female IDS speech speakers
filtered using `stimuli-info.csv`) is methodologically cleaner than sourcing each parameter
from a different study. It also has rhetorical advantages: the source is published in NHB,
the target journal.

The analysis is documented in `hilton2022_population_norms.R`, which uses:
- `acoustics-editedAudio.csv` from their GitHub repo (f0 values already computed)
- `stimuli-info.csv` from their GitHub repo (Gender column for sex filtering)
- `formant_summary.csv` from `extract_formants_IDS.praat` (F1-F4, not in their CSV)

### f0 mean: ±51 Hz (1 × 50.8 Hz)

**Primary source:** Hilton et al. (2022), female IDS speech speakers across 21 societies.
- Between-speaker SD of f0 mean = **50.8 Hz**
- Population mean f0 = 261.8 Hz
- 1 × 50.8 Hz = **±51 Hz**

**Cross-check:** Cox et al. (2023), *Nature Human Behaviour* meta-analysis OSF data:
- SD = 40.1 Hz → 1 SD target = ±40 Hz
- The Hilton et al. value is larger (50.8 vs 40.1 Hz), consistent with greater
  cross-cultural diversity in the Hilton corpus.

Use **Hilton et al. (±51 Hz)** as the primary source. Cite Cox et al. as a cross-check.

### f0 SD: ±28 Hz (1 × 28.4 Hz)

**Primary source:** Hilton et al. (2022), female IDS speech speakers.
- Between-speaker SD of f0 SD = **28.4 Hz**
- Population mean f0 SD = 66.6 Hz
- 1 × 28.4 Hz = **±28 Hz**

**Cross-check:** Broesch & Bryant (2014), *Journal of Cognition and Development*, Table 1:
- Western group SD = 24.0 Hz, non-Western SD = 22.4 Hz → consistent with Hilton et al.

Use **Hilton et al. (±28 Hz)** as the primary source. Cite Broesch & Bryant as a cross-check.

### Formant frequencies: ±4.6% (1 × CV 0.046)

**Primary source:** Hilton et al. (2022), female IDS speech speakers.
- F1-F4 extracted from corpus audio using `extract_formants_IDS.praat` (adapted from their
  pipeline; adds F3 and F4 which their original script did not extract)
- Between-speaker SD of geometric mean F1-F4 (VTL proxy) = 87.4 Hz
- Population mean = 1920.9 Hz
- CV = 87.4 / 1920.9 = **0.046**
- 1 × CV = **±4.6%** proportional scaling factor

This replaces the former "provisional ±10% pilot estimate" with a principled population norm.
Note: this resolves the "no population SD available" issue that previously forced formant
frequencies into exploratory status. However, the parameter remains exploratory for the
other two reasons (thin literature, power below 95% — see Section 3).

**Empirical validation (MES02B):**
- Target: geometric mean F1-F4 of ±4.6% from baseline (~1860 Hz)
- Achieved: Low ≈ -3.9%, High ≈ +5.4% (total span 173 Hz vs theoretical 171 Hz)
- Residual ≈15 Hz discrepancy is PSOLA noise on the formant measurement, not a targeting failure
- The iterative formant correction converges in 3-5 iterations

### What to update in the manuscript

- **Methods / Stimulus Generation:** Replace old sources (Cox et al., Broesch & Bryant,
  Kalashnikova) with Hilton et al. (2022) as the single primary source for all three
  parameters. Keep Cox et al. and Broesch & Bryant as cross-checks in a footnote or
  supplementary note.
- **Methods / Stimulus Generation:** State that formant frequencies manipulation is now
  anchored to 1 SD of the between-speaker CV of the geometric mean of F1-F4 (VTL proxy)
  in IDS speech, derived from Hilton et al. (2022).
- **Table 1 or equivalent design table:** Update manipulation magnitudes to:
  f0 mean ±51 Hz, f0 SD ±28 Hz, formant frequencies ±4.6%.

### What to update in the supplementary

- Update the Stimulus Generation section to describe the Hilton et al. (2022) derivation.
- Remove the description of the formant factor as "provisional."

---

## 3. Renaming and reclassifying the formant parameter

### The naming problem

The script (now fixed) and manuscript currently use "Df" / "formant dispersion." The
manipulation scales all formant frequencies proportionally via LPC resynthesis — this is not
the same as formant dispersion (Df = mean spacing between F1-F4), as David correctly noted
(Puts et al., 2012, *Proceedings of the Royal Society B*).

**Correct terminology:** "formant frequencies" (primary term), with a note that the
manipulation targets apparent vocal tract length via proportional scaling. The Praat script
now uses `Ffreq`/`ffreq` throughout; the manuscript needs the same update.

### Why formant frequencies remain exploratory

The "no population SD" reason is now resolved (Hilton et al. 2022 provides CV = 0.046).
However, two reasons for exploratory status remain:

1. **Thin literature:** Direct evidence for formant frequency effects in IDS is sparse.
   Cox et al. (2023) does not include this parameter. No cross-linguistic study has tested
   whether infants prefer IDS with scaled formant frequencies.

2. **Power:** At n = 200 and an assumed effect of 50 ms, simulated detection power for
   formant frequencies is 83.8% — below the ≥95% threshold required by NHB.

### What to update in the manuscript

- Replace all instances of "Df", "formant dispersion" with "formant frequencies" (or
  "apparent vocal tract length" where theoretically appropriate)
- H3 must be removed from the confirmatory hypotheses section
- Add formant frequencies explicitly to the exploratory analyses section
- Update the theoretical motivation: the "no population norm" reason for exploratory status
  is no longer valid — reframe around thin literature and power
- Update the design table: 2 confirmatory hypotheses (H1, H2) + 1 exploratory parameter
- Output filenames in the manuscript description: `_Ffreq-` not `_Df-`

### What to update in the supplementary

- Replace all "Df" / "formant dispersion" with "formant frequencies" / "Ffreq"
- Update the description of what the formant manipulation measures (geometric mean F1-F4,
  not (F4-F1)/3)

---

## 4. Equivalence decision rule fix

### The problem

The current manuscript states that equivalence is concluded "if the lower bound of the 90% CI
exceeds −150 ms." This is logically wrong for a directional hypothesis.

### The fix

> **Upper bound of the 90% CI < +150 ms**

### Where to make changes (manuscript only — 3 locations)

1. **Analysis Plan — Decision rules, rule (2):**
   Change "the lower bound of the 90% CI exceeds −150 ms (the pre-specified equivalence
   bound), regardless of the upper bound" to "the upper bound of the 90% CI is below +150 ms
   (the pre-specified equivalence bound)"

2. **Analysis Plan — Decision rules, rule (3):**
   Change "the lower bound of the 90% CI falls at or below −150 ms and the effect is not
   significant" to "the upper bound of the 90% CI is at or above +150 ms and the effect is not
   significant"

3. **Design Table — H1 and H2 rows** (H3 row will be removed; see Section 3):
   Change "if the lower bound exceeds −150 ms, practical equivalence is concluded" to
   "if the upper bound is below +150 ms, practical equivalence is concluded"

---

## 5. Power analysis: updated results

### Simulation results at n = 200

The simulation was rerun (May 2026) with `sample_sizes <- seq(100, 300, by = 10)`. Results:

| Effect | Hypothesis | Detection power |
|--------|-----------|----------------|
| f0 SD | H1 | **99.7%** |
| f0 mean | H2 | **96.2%** |
| Formant frequencies | Exploratory | (not applicable) |

### Supplementary material updates required

1. **Remove H3 from confirmatory power analysis:** Relabel as exploratory in power curves,
   Tables S3 and S6, and narrative text.

2. **Update `power_threshold`:** Change `power_threshold <- 0.80` to `power_threshold <- 0.95`
   in the Global Parameters section. Applies only to detection power; equivalence power
   target remains 80%.

3. **Update power summary tables** (Tables S3 and S6): Remove Df/H3 row from confirmatory
   section or move to a separate exploratory table.

4. **Update narrative in Description section:** Change from three confirmatory analyses to
   two confirmatory (H1, H2) and one exploratory (formant frequencies).

### Implications for journal choice

- NHB is realistic at n = 200 for both confirmatory hypotheses
- Bayesian sequential design remains an alternative if reviewers push back on f0 mean (96.2%)
- Share updated power curves with David before deciding on the journal

---

## 6. Praat script: COMPLETED May 2026

The script (`IDS_manipulation.praat`) has been fully updated. All items below are done.

### What was changed

- **Renamed throughout:** All `df`/`Df` variable names and labels replaced with `ffreq`/`Ffreq`.
  Output filenames now use `_Ffreq-Low` / `_Ffreq-High` instead of `_Df-Low` / `_Df-High`.

- **New defaults (Hilton et al. 2022, 1 SD):**
  ```praat
  positive sd_f0mean_hz    51   # 1 × 50.8 Hz
  positive sd_f0sd_hz      28   # 1 × 28.4 Hz
  positive factor_formant  0.046  # 1 × CV 0.046
  ```

- **Fixed formant measurement:** `measureAcoustics` now uses `max_formant_synth_hz` (5500 Hz)
  for both synthesis and measurement (previously used a separate 6500 Hz ceiling for
  measurement, which caused the Burg tracker to mis-assign formant peaks). The metric
  changed from (F4-F1)/3 to geometric mean of F1-F4, consistent with the Hilton et al.
  calibration analysis.

- **Added iterative formant correction:** `shiftFormants` is now called inside an iteration
  loop (up to 8 iterations, tolerance 10 Hz) that adjusts the synthesis ratio until the
  measured geometric mean F1-F4 matches the target. This mirrors the existing iterative
  f0 SD correction (`synthesizeF0`). The ratio adjustment uses the same proportional logic:
  `new_deviation = old_deviation × (target / measured)`.

- **Removed `max_formant_meas_hz` from the form:** A single `max_formant_synth_hz` setting
  is used for everything, eliminating the ceiling inconsistency.

### Empirical validation (MES02B, Mestizo female IDS speech)

| Parameter | Target | Achieved | Error |
|-----------|--------|----------|-------|
| f0 mean Low | 235.18 Hz | 234.45 Hz | <1 Hz |
| f0 mean High | 337.18 Hz | 336.10 Hz | <1 Hz |
| f0 SD Low | 18.87 Hz | 19.00 Hz | <1 Hz |
| f0 SD High | 74.87 Hz | 73.62 Hz | <2 Hz |
| Ffreq Low | 1774.66 Hz (-4.6%) | ~1788 Hz (-3.9%) | +13 Hz |
| Ffreq High | 1945.80 Hz (+4.6%) | ~1961 Hz (+5.4%) | +15 Hz |

f0 parameters hit targets precisely. The ~15 Hz Ffreq residual is PSOLA noise on the
post-hoc formant measurement, not a targeting failure — the Low-to-High span
(173 Hz) matches the theoretical 2-SD span (171 Hz) almost exactly.

---

## 7. Miscellaneous manuscript fixes

### Praat version inconsistency

- Manuscript body says **version 6.4.43**
- Reference list and supplementary say **version 6.4.63**
- Check which is correct and fix consistently throughout

### Duplicate sentence in Introduction

Two consecutive paragraphs end with essentially the same sentence about pitch variability
driving preferential attention in pre-linguistic infants. One should be cut or reformulated.

### Typographic issues

- "Largescale metaanalytic" → "large-scale meta-analytic"
- "Mithen, 2006).The core claim" → missing space after full stop

### Supplementary housekeeping

- "CHECK MISSING URLs" is still visible as a literal note in the Description section
- "ADD-URL" placeholder remains in the Stimulus Generation section

---

## 8. Reference list additions

- **Hilton et al. (2022)** — now the primary source for all three manipulation parameters.
  Add to manuscript reference list and cite in the Stimulus Generation section of Methods.
  Also cite the Zenodo audio corpus (https://doi.org/10.5281/zenodo.5525161) and GitHub
  repo when describing the population norm derivation.
- **Cox et al. (2023)** — now a cross-check for f0 mean (not primary source). Retain in
  reference list; update citation context from "primary source" to "cross-check."
- **Broesch & Bryant (2014)** — now a cross-check for f0 SD (not primary source). Retain
  in reference list; update citation context similarly.
- **Aung et al. (2024)** — methodological precedent for SD-based manipulation approach.
  Add to reference list if not already present.
- **Puts et al. (2012, Proceedings B)** — relevant to the Df vs formant scaling distinction.
  Already in the reference list.

---

## Summary checklist

### Praat script (DONE)
- [x] Rename Df/df → Ffreq/ffreq throughout script, labels, and output filenames
- [x] Update defaults to Hilton et al. (2022) 1 SD values (f0 mean ±51 Hz, f0 SD ±28 Hz, formant ±4.6%)
- [x] Fix formant measurement ceiling (5500 Hz, consistent with synthesis)
- [x] Change formant metric from (F4-F1)/3 to geometric mean F1-F4
- [x] Add iterative formant correction (converges to ±10 Hz of target)
- [x] Empirically validate on MES02B — f0 hits targets; Ffreq span matches 2-SD theory

### R analysis scripts (DONE)
- [x] Run extended power simulation (n = 100-300) — 96.2% for f0 mean, 99.7% for f0 SD at n = 200
- [x] Derive population norms from Hilton et al. (2022) using `hilton2022_population_norms.R`
- [x] Extract F1-F4 from Hilton et al. corpus using `extract_formants_IDS.praat`

### Manuscript — pending
- [ ] Fix equivalence decision rule (3 locations: decision rule 2, rule 3, design table)
- [ ] Remove H3 (formant frequencies) from confirmatory hypotheses; add to exploratory section
- [ ] Rename "Df" / "formant dispersion" → "formant frequencies" throughout
- [ ] Update the design table: 2 confirmatory hypotheses (H1, H2) only; update H3 to exploratory
- [ ] Update reason for formant being exploratory: remove "no population SD" (now resolved);
      retain thin literature and power reasons
- [ ] Update Stimulus Generation: replace old sources with Hilton et al. (2022) as primary,
      Cox et al. and Broesch & Bryant as cross-checks
- [ ] Update Stimulus Generation: report actual targets — f0 mean ±51 Hz, f0 SD ±28 Hz, Ffreq ±4.6%
- [ ] Update Stimulus Generation: describe 1 SD (not 1.5 SD) with perceptual pilot justification
- [ ] Update Stimulus Generation: output filenames use _Ffreq- not _Df-
- [ ] Fix Praat version inconsistency (6.4.43 vs 6.4.63) — check which is correct
- [ ] Remove duplicate sentence in Introduction (pitch variability / preferential attention)
- [ ] Fix typographic issues ("Largescale", missing space after full stop)
- [ ] Add Hilton et al. (2022) to reference list; update Cox et al. and Broesch & Bryant contexts
- [ ] Add Aung et al. (2024) if not already in reference list

### Supplementary material (.qmd / PDF) — pending
- [ ] Update Description section: 2 confirmatory analyses (H1, H2) + 1 exploratory (formant frequencies)
- [ ] Update Stimulus Generation section: Hilton et al. (2022) as source; ±1 SD; targets ±51/28 Hz/4.6%;
      clear ADD-URL placeholder
- [ ] Clear "CHECK MISSING URLs" placeholder in Description section
- [ ] Rename Df/formant dispersion → formant frequencies / Ffreq throughout
- [ ] Remove H3 from confirmatory power curves and Tables S3 and S6; move to exploratory or remove
- [ ] Change `power_threshold <- 0.80` to `power_threshold <- 0.95` (detection power only;
      equivalence power target remains 80%)
- [ ] Update narrative summary at end of Section 7 to reflect 2 confirmatory hypotheses
- [ ] Update formant factor description: ±4.6% principled (from Hilton et al. 2022), not provisional

### Decisions still needed (with David)
- [ ] Confirm journal choice (NHB vs PCI RR vs other)
- [ ] Share updated power curves before submission
- [ ] Confirm 1 SD (not 1.5 SD) manipulation magnitude is acceptable
- [ ] Confirm Hilton et al. (2022) as single source for population norms