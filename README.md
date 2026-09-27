# Saudi Arabia net-zero 2060 pathways: figure reproduction

Code and data to reproduce the result figures of six publications on Saudi Arabia's pathway to net-zero greenhouse gas emissions by 2060, based on scenarios from GCAM-KSA, the Saudi Arabia–focused version of the Global Change Analysis Model. All four scenarios (No Policy, Current Policy, NDCs Continued, NZ 2060) are archived in a single scenario database, and every figure is regenerated from it with one command. Each figure is written together with a CSV of the exact values plotted.

## Publications

| Folder | Publication |
|---|---|
| [`papers/esr2024_path-to-2060`](papers/esr2024_path-to-2060) | Kamboj, P., Hejazi, M., Qiu, Y., Kyle, P., Iyer, G. (2024). The path to 2060: Saudi Arabia's long-term pathway for GHG emission reduction. *Energy Strategy Reviews* 55, 101537. [doi:10.1016/j.esr.2024.101537](https://doi.org/10.1016/j.esr.2024.101537) |
| [`papers/ks2023-dp30_long-term-pathway`](papers/ks2023-dp30_long-term-pathway) | Kamboj, P., Hejazi, M., Qiu, Y., Kyle, P., Iyer, G. (2023). *Long-Term Decarbonization Pathway for Saudi Arabia To Reach Net Zero GHG Emissions by 2060.* KAPSARC Discussion Paper. [doi:10.30573/KS--2023-DP30](https://doi.org/10.30573/KS--2023-DP30) |
| [`papers/ks2023-dp31_electricity`](papers/ks2023-dp31_electricity) | Kamboj, P., Hejazi, M., Alhadhrami, K., Qiu, Y., Kyle, P., Iyer, G. (2023). *Saudi Arabia Net Zero GHG Emissions by 2060: Transformation of the Electricity Sector.* KAPSARC Discussion Paper. [doi:10.30573/KS--2023-DP31](https://doi.org/10.30573/KS--2023-DP31) |
| [`papers/ks2024-dp01_transport`](papers/ks2024-dp01_transport) | Kamboj, P., Hejazi, M., Qiu, Y., Kyle, P., Iyer, G. (2024). *Saudi Arabia Net-Zero GHG Emissions by 2060: Transformation of the Transport Sector.* KAPSARC Discussion Paper. [doi:10.30573/KS--2024-DP01](https://doi.org/10.30573/KS--2024-DP01) |
| [`papers/ks2024-dp07_buildings`](papers/ks2024-dp07_buildings) | Kamboj, P., Hejazi, M., Qiu, Y., Kyle, P., Iyer, G. (2024). *Saudi Arabia Net-Zero GHG Emissions by 2060: Transformation of the Building Sector.* KAPSARC Discussion Paper. [doi:10.30573/KS--2024-DP07](https://doi.org/10.30573/KS--2024-DP07) |
| [`papers/ks2024-dp18_industry`](papers/ks2024-dp18_industry) | Qiu, Y., Kamboj, P., Hejazi, M., Iyer, G., Kyle, P. (2024). *Reaching Net-Zero GHG Emissions in Saudi Arabia by 2060: Transformation of the Industrial Sector.* KAPSARC Discussion Paper. [doi:10.30573/KS--2024-DP18](https://doi.org/10.30573/KS--2024-DP18) |

## Quick start

Requires R ≥ 4.1.

```sh
Rscript install_packages.R   # dplyr, tidyr, ggplot2, patchwork, rgcam (GitHub: JGCRI/rgcam)
Rscript run_all.R            # all papers, about 30 s
```

A single paper can be run on its own from anywhere inside the repository:

```sh
Rscript papers/ks2023-dp31_electricity/make_figures.R
```

Output goes to `papers/<paper>/figures/` (PNG, 300 dpi) and `papers/<paper>/source_data/` (CSV). Figure files are numbered as in the publication, e.g. `fig04_electricity_generation_by_technology.png` is Figure 4. Multi-panel figures write one CSV per panel (`_a`, `_b`, …). `session_info.txt` records the R and package versions of the last full run.

## Repository layout

```
data/
  ksa_nz.dat              scenario database (rgcam project file; 4 scenarios, 83 queries, 2015–2060)
  external/               inputs not held in the scenario database (see below)
R/
  common.R                loads packages, data and helpers; sourced by every paper script
  constants.R             scenario labels, unit conversions, AR5 GWP100 values
  data.R                  access to the scenario database
  prep.R                  data preparation, one function per indicator
  plots.R                 plot builders for figure types shared across papers
  theme.R                 ggplot theme and colour palettes
  save.R                  writes each figure and its source data
papers/<paper>/make_figures.R
run_all.R
install_packages.R
```

Indicators that appear in several papers (economy-wide emissions, the generation mix, end-use electricity demand, final energy) are computed once in `R/prep.R`, so the same number is identical wherever it is plotted.

## Data provenance

**Scenario database.** `data/ksa_nz.dat` holds the GCAM-KSA output for Saudi Arabia used in all six publications. It is read with [`rgcam`](https://github.com/JGCRI/rgcam). Most figures are computed directly from it. Economy-wide GHG emissions cover CO₂ from fossil fuels and industry, land-use change CO₂, CH₄, N₂O and F-gases, weighted with IPCC AR5 100-year GWPs.

**External inputs** (`data/external/`). Some published figures rely on quantities that were post-processed outside the scenario database. These are shipped as the published figure data:

| File | Used in | Content |
|---|---|---|
| `ghg_reduction_decomposition_2060.csv` | DP30 Fig. 3 | Contribution of each mitigation measure to the 2060 gap between No Policy and NZ 2060, from single-measure sensitivity runs |
| `cooling_demand.csv` | ESR Fig. 6a; DP07 Fig. 5a | Space-cooling service demand (billion tons of cooling) |
| `cooling_electricity_consumption.csv` | ESR Fig. 6b; DP07 Fig. 5b | Electricity used for cooling, No Policy vs NZ 2060 |
| `power_capacity_investment.csv` | DP31 Fig. 5 | Installed capacity and new-capacity investment, derived from generation, capacity factors and capital costs |
| `industry/*.csv` | DP18 Figs. 7–12; ESR Fig. 7 | Industrial emissions, fuel use (with CCS-equipped use separated), cement and steel technology shares, refinery output, DAC energy use |

**Industrial sector results.** The industry figures use the published figure data in `data/external/industry/`, not the scenario database. For No Policy and Current Policy the two sources agree to within 0.01 Mt CO₂. The NDCs Continued and NZ 2060 industry results in the publications, however, come from a model run that differs slightly from the archived one. For example, 2060 industrial CO₂ under NDCs Continued is 221 Mt in the publications and 207 Mt in `ksa_nz.dat`. The published values are used so that the figures reproduce the papers. The DAC sequestration line in DP18 Fig. 12 is the only industry quantity read from the scenario database.

**Net-zero residual.** Economy-wide GHG emissions in NZ 2060 in 2060 are 1.3 Mt CO₂e in the scenario database (0.2% of 2015 emissions). The published figure data round this to zero. The figures here plot the database value.

## Citation

Please cite the publication whose figures you use (table above) and this repository.

## Licence

Code: MIT. Data in `data/` and the generated figures and source data: © King Abdullah Petroleum Studies and Research Center (KAPSARC), licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). Reuse is permitted with citation of the relevant publication. Full terms in `LICENSE`.
