# Replication findings

Audit prepared October 23, 2026 for Ormeno and Korolev, *Re-evaluating Augusto Pinochet's influence on Chile's economic growth*, Economics Letters 261 (2026), 112841.

## Scope and reference version

The journal article and the supplied website supplement are the publication references. The later manuscript in `May2025/EL_Revision2/`, dated December 16, 2025, contains the six appendix tables and eighteen appendix figures used here. The similarly named manuscript directly under `May2025/`, dated December 5, predates some of that material. Private manuscript files and publisher PDFs are not distributed in this archive.

Original code and data were preserved. Estimation was rerun from the supplied data, and saved figures and table workbooks were used as comparison targets rather than substitutes for computation. The supplied original R environment was unavailable. `renv.lock` therefore records a reconstructed environment that has been tested, not a recovered original session.

## Verification results

| Result | Evidence | Finding |
|---|---|---|
| Analysis data | Raw workbooks independently reshaped and compared with saved CSVs | All 4,895 World Bank and 9,425 Maddison observations agree; maximum log difference below 5e-14. |
| Figures 1–3 | All observed and estimated paths compared with original EPS files and directly with vector paths in the published PDF | All 72 paths agree within 0.001 log units. Maximum discrepancy against the journal PDF is below 0.000079 log units, consistent with finite plot precision. |
| Table A1 | Four complete simulation scripts, 250 draws per configuration | All 48 MSE entries reproduce at the published one-decimal precision. |
| Table A2 | Same simulation runs | All 32 mean/variance entries reproduce at the published two-decimal precision. |
| Table A3 | All nine placebo specifications, two datasets, three estimators | All 54 MSE entries reproduce at the published one-decimal precision. |
| Table A4 | Fresh GSCM fits using fect 2.0.5 | All 18 factor counts reproduce exactly. |
| Tables A5–A6 | Fresh SCM donor weights | One omitted reported weight, described below. The other 626 compared cells, including implied zero entries, agree at three-decimal precision. |
| Figures A16–A18: STSM curves | Fresh STSM fits compared with original EPS paths and directly with vector paths in the supplement PDF | Eight of 18 curves agree within 0.001 log units. Ten differ by more than this tolerance. Details below. |
| Figure A6 | Source search and inspection of saved assets | Six original illustration panels are available, but exact draw-selection/export commands were not found. These panels are reference-only. |

The simulations cover 16 configurations (four scripts × four sample sizes), with 250 draws each: 4,000 simulated panels. Each draw is evaluated by the three estimators. This was a full simulation rerun, not a reduced-size smoke test.

The saved workbooks also match all 134 published numerical cells in Tables A1–A3. This is an additional consistency check; the fresh reruns above establish computational reproduction.

The curve tolerance was set at 0.001 log units before comparison to allow for finite EPS/PDF coordinate precision. It is a numerical reproduction criterion, not a threshold for economic importance. The main workflow regenerates figures with updated layout and labels; agreement concerns their data and estimates, not pixel-identical rendering.

## Package versions and random numbers

Using fect 2.0.5 reproduces the main GSCM results, selected factor counts, placebo MSEs, and simulation tables. Using 2.4.5 with the same main estimation calls changes factor selection in 15 of 18 panels and can substantially change GSCM effect estimates. This is documented in [FECT_VERSION_COMPARISON.md](FECT_VERSION_COMPARISON.md). The default setup restores 2.0.5; other versions are permitted with a warning.

The tested environment uses R 4.6.1, CausalImpact 1.4.1, augsynth 0.2.0 at commit `7a90ea48877fae7925a72cb50bc03a315bc7c042`, and KFAS 1.6.0, with all dependencies recorded in the lockfile. Original table metadata identifies R 4.5.0 but does not establish all original package versions.

The simulation scripts retain their original `set.seed(123)`. The empirical wrapper sets the same seed for a reproducible run. CausalImpact 1.4.1 internally supplies `seed=1` to its bsts fit, and its empirical counterfactuals reproduce. The absence of a seed in the original empirical script is therefore not evidence of a BSTSM replication failure.

## Table A6: omitted Uruguay weight

In the Maddison, 1988, short-preperiod, largest-pool specification, the fitted SCM weight on Uruguay is **0.560157587**, which rounds to **0.560**. Table A6 prints a dash in this cell.

This is a reporting omission. The computation includes Uruguay, the complete weights sum to one, and the resulting SCM curve matches the published figure. Adding the missing table entry does not change the estimates or figures. `outputs/tables/Tables_A5_A6_weights.csv` contains the complete computed weights. The saved original reference table is preserved as originally supplied.

## Simulation description versus implemented DGP

The following distinctions matter if readers implement the simulation from the prose rather than run the supplied code. The archive retains the DGP that generated the reported tables.

| Quantity | Description in the supplement | Implemented in the code |
|---|---|---|
| Constant-variance factor loadings | Diagonal covariance entries of 0.5 | `rnorm(..., sd=0.5)`, hence variance 0.25. |
| Increasing-dispersion unit effects | Mean 0 and variance N/25 | Mean 7 and standard deviation N/25, hence variance (N/25)^2. |
| Increasing-dispersion factor loadings | Diagonal covariance entries N/50 | Standard deviation N/50, hence variance (N/50)^2. |
| Simulation sample size | Sometimes described as the number of donors | N is the total number of units, including the one treated unit; there are N−1 donors. |
| Simulation MSE | Displayed as squared actual-minus-counterfactual differences | Squared estimation error for the treatment effect: `(estimated effect - true effect)^2`, with true effect equal to one. |

These are differences in the description of the reported simulation, not failures to reproduce its numerical tables. Running a DGP that follows the prose literally would be a separate exercise and need not produce Tables A1–A2.

## Donor-pool labels

The usable donor counts excluding Chile are **16, 24, 42** for World Bank and **17, 25, 43** for Maddison. The largest samples therefore contain 43 and 44 units including Chile. The paper's 43/44 labels for those samples count Chile, unlike its smaller donor-pool labels. The historical code suffix `49` names a candidate list; it does not mean that 49 usable controls enter estimation. `verification/donor_pools.csv` records the actual country sets and exclusions.

## STSM robustness: remaining numerical differences

These are the additional structural time-series models in Figures A16–A18, implemented using the adapted Dreuw helper and KFAS. They are separate from the BSTSM estimates produced by CausalImpact in the main results and separate from the fect version issue.

The same original estimation expressions were run under the reconstructed environment. Maximum absolute differences from saved STSM paths are:

| Exercise | Pool | World Bank, log units | Maddison, log units |
|---|---|---:|---:|
| 1973 | Small | 0.014280 | 0.000023 |
| 1973 | Medium | 0.014260 | 0.000028 |
| 1973 | Large | 0.026543 | 0.026977 |
| 1988 | Small | 0.000042 | 0.000038 |
| 1988 | Medium | 0.000069 | 0.000038 |
| 1988 | Large | 0.000965 | 0.000060 |
| 1988 extended | Small | 0.008022 | 0.006589 |
| 1988 extended | Medium | 0.008022 | 0.006581 |
| 1988 extended | Large | 0.046877 | 0.006572 |

The ten discrepancies above tolerance range from 0.00657 to 0.04688 log units. Direct extraction of the supplement PDF confirms the same ten mismatches (maximum 0.046879 log units); all 72 observed/SCM/GSCM/BSTSM paths repeated in these appendix figures agree with the pinned rerun. Their cause has not been established. Differences in numerical optimization or the original R/package environment are possible explanations, not confirmed diagnoses. The portable workflow produces the same STSM results as the independent audit, so the packaging changes do not explain these discrepancies. The archive must not be described as reproducing all appendix STSM curves exactly.

`scripts/verify.R` compares new numerical outputs with the audited fresh-run baseline. Consequently, a passing STSM check means that the fresh run is repeatable under the tested environment; it does not mean that the published STSM curves have been reproduced. `verification/stm_curve_checks.csv` records the separate comparison with the original figures.

## Results-to-code guide

The specification names are `<intervention>_<pool>_<period>`, where the pools are `esc17`, `esc25`, and `49`, and `period` is `short` or `long`. The 1988 `long` specification extends the pre-intervention sample; its post-intervention sample still ends in 2014. Both datasets are estimated in each script.

| Publication item | Archive code | Fresh output |
|---|---|---|
| Figure 1 | `code/main/1973_*_short.R` | `outputs/figures/Figure_1.pdf`; `outputs/tables/main_estimates.csv` |
| Figure 2 | `code/main/1988_*_short.R` | `outputs/figures/Figure_2.pdf`; same estimates CSV |
| Figure 3 | `code/main/1988_*_long.R` | `outputs/figures/Figure_3.pdf`; same estimates CSV |
| Figure A1 | `scripts/appendix_figures.R`, frozen input CSVs | `outputs/figures/Figure_A1.pdf` |
| Figures A2–A5 | Main fitted factors; `scripts/appendix_figures.R` | Corresponding PDFs in `outputs/figures/` |
| Figure A6 | Exact plotting commands unavailable | Original six EPS panels in `reference/figures/` |
| Tables A1–A2 | `code/simulations/{20,40}_{const,incr}.R` | `outputs/tables/Table_A1_simulation_mse.csv`, `Table_A2_simulation_factors.csv` |
| Table A3; Figures A7–A15 | `code/placebo/` and matching main fits | `outputs/tables/Table_A3_placebo_mse.csv`; 54 EPS panels in `outputs/figures/` |
| Table A4 | GSCM fits in `code/main/` | `outputs/tables/Table_A4_factors.csv` |
| Tables A5–A6 | SCM fits in `code/main/` | `outputs/tables/Tables_A5_A6_weights.csv` |
| Figures A16–A18 | `code/stm/`, including `R_functions.R` | EPS panels and `estimates.csv` in each `outputs/stm/<spec>/` directory |

`verification/figure_map.csv` maps each of the 112 original figure assets to its publication label and original source-script name. It complements the portable script names above. Complete numerical audit results are in `verification/`; fresh numerical runs that provide the regression baseline are in `verification/rerun/`.

The portable main, placebo, simulation, and STSM stages were compared with those independent audit runs. All seven groups of numerical checks pass with zero difference in the tested environment; see `verification/portable_workflow_checks.csv`. A separate main-stage run with fect 2.4.5 also completed with the intended warning and exactly matched the current-version audit. A fresh-machine package installation has not been independently tested; source-package installation may require the platform's compiler tools.

## Changes made for portability

- Removed machine-specific working directories, destructive workspace clearing, and broad package-loading blocks. A single runner manages paths, package loading, logs, and output directories.
- Split main estimation, placebo estimation, simulations, and STSM robustness into stages. Explicitly combined Chile and placebo fit objects before running the original placebo plotting expressions, which previously depended on session state.
- Rebuilt the wide STSM input objects from the verified CSVs instead of distributing saved author workspaces.
- Renamed the simulation identifier column `unit` to `unit_id` consistently. This avoids an augsynth/tidy-evaluation name collision; unit values, observations, estimator calls, and DGP parameters are otherwise unchanged. The complete simulation results still reproduce.
- Removed unused `rio` and `sjPlot` imports from the STSM helper. Retained the estimation functions and existing attribution.
- Added data checks, numerical verification, a package lockfile, session information, and explicit descriptions of deviations. Main and factor-distribution figures are assembled by a portable plotting script; original EPS assets are retained for comparison.

No corrections were made to the original research files. The public archive contains only selected code, frozen data, references, and audit evidence. Data-source details and third-party code terms are recorded in [DATA_SOURCES.md](DATA_SOURCES.md) and [THIRD_PARTY_NOTICES.md](THIRD_PARTY_NOTICES.md).
