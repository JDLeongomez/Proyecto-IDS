# Study 1 Materials

Acoustic manipulation script and supporting materials for:
Leongómez et al., *Acoustic drivers of infant attention in infant-directed speech* [Stage 1 RR]
Stage 1 component (manuscript + supplementary material): https://doi.org/10.17605/OSF.IO/D7V3E

## Contents

- `IDS_manipulation.praat` — main acoustic manipulation script (formant scaling + f0 affine transform)
- `population_norms/` — population norm derivation
    - `extract_formants_IDS.praat` — formant extraction (adapted from Hilton et al., 2022)
    - `by_speaker.csv` — aggregated speaker-level acoustic data (used by Supplementary_material.qmd)
    - `formant_summary.csv` — raw per-file formant output (intermediate, not loaded by code)
    - `population_norms.csv` — final population summary (Table S2)
    - `hilton2022_population_norms.R` — population norm analysis code
    - `population_norms_distributions.png` — exported version of Figure S1 (not used by code)

## Reproducibility

To re-render the Stage 1 supplementary material (`Supplementary_material.qmd`), this
`population_norms/` folder must be placed in the same working directory as that file.

## Licensing

`extract_formants_IDS.praat`: CC BY-NC-SA 4.0 (adapted from Hilton et al., 2022; see script header)
All other files: GPL-2.0-or-later (code) / CC-BY 4.0 (data)