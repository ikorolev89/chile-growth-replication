# Replication archive: Chile's economic growth

Hector Ormeno and Ivan Korolev (2026), **Re-evaluating Augusto Pinochet's influence on Chile's economic growth**, *Economics Letters* 261, 112841. [Published article](https://doi.org/10.1016/j.econlet.2026.112841).

Public replication archive, version **1.0.0**, released October 25, 2026. The computational audit was completed on October 23, 2026. The main estimates, placebo MSEs, and all four complete simulation exercises reproduce. The appendix STSM robustness exercise has numerical differences documented in [REPLICATION_NOTES.md](REPLICATION_NOTES.md). That file also records a missing table entry and differences between the simulation description and the implemented DGP.

## Run

Use R 4.6.1 (the tested version), an internet connection for the initial package installation, and compiler tools if your platform lacks package binaries. From this directory:

```sh
Rscript --vanilla scripts/setup.R
Rscript --vanilla run.R all
Rscript --vanilla scripts/verify.R
```

`setup.R` restores the 92-package `renv.lock` into this project. For reproduction of the published results, use **fect 2.0.5**, as specified in this environment. The main estimation code also runs with **fect 2.4.5**, but GSCM results differ, sometimes substantially, including changes in the sign of estimated effects. Across the 18 main panels, the newer version changes 15 selected factor counts and reverses the average post-intervention GDP gap in two panels. SCM and BSTSM estimates are unchanged in this comparison. See [the version comparison](FECT_VERSION_COMPARISON.md).

The runner warns but permits other `fect` versions. `scripts/verify.R` compares results with the archived numerical baseline, so differences under another version will be reported as verification failures. For an optional modern-version comparison, use a separate copy of the archive, run setup, then run `Rscript --vanilla -e 'source("scripts/load_environment.R"); renv::install("fect@2.4.5", project=".")'` before estimation. This changes that copy's installed library without rewriting the supplied lockfile. Re-run setup to restore the pinned versions. Compatibility with every future release is not guaranteed.

`augsynth` is pinned to Git commit `7a90ea48877fae7925a72cb50bc03a315bc7c042`. The lockfile is a reconstructed, tested environment, not a claim that all packages are the authors' original 2025 versions. Original table metadata identifies R 4.5.0; the complete original environment was not retained. The `fect` pin resolves the main GSCM replication issue; it does not resolve the separate STSM differences in Figures A16–A18.

Stages can also be run separately:

| Command | Results |
|---|---|
| `Rscript --vanilla run.R main` | Figures 1–3; Figure A1; Figures A2–A5; Table A4; weights for Tables A5–A6 |
| `Rscript --vanilla run.R placebo` | Main stage plus Table A3 and the 54 panels in Figures A7–A15 |
| `Rscript --vanilla run.R simulations` | Tables A1–A2: all 250 draws in each of 16 DGP configurations |
| `Rscript --vanilla run.R stm` | Figures A16–A18, with the numerical differences noted below |

Results are written under `outputs/`; input files are never overwritten. Each run records logs, numerical results, and session information. The simulations took about 108–153 seconds per script in the audit; runtimes depend on the machine. Allow roughly 10 minutes for a complete serial run, excluding installation. No fresh data download is needed after setup.

`Rscript --vanilla scripts/check_data.R` independently reconstructs the analysis CSVs from the supplied workbooks and checks their contents.

## Contents

- `code/`: the estimation expressions actually used in verification, with path/session plumbing removed. The estimator specifications and DGP parameters are preserved.
- `data/`: frozen analysis CSVs and donor lists; `data/raw/`: the author-supplied workbook extracts. See [DATA_SOURCES.md](DATA_SOURCES.md).
- `reference/`: saved original figure assets and table workbooks. These are reference outputs, not evidence of having rerun an estimator.
- `verification/`: numerical comparisons and fresh-run outputs from the audit, including the one known weight-table omission.
- `scripts/`: setup, verification, data-preparation checks, and additional figure assembly.
- `renv.lock`: the reconstructed package environment.
- `code_provenance.csv`: original-to-archive code mapping and SHA-256 hashes; `archive_manifest.csv`: integrity hashes for the distributed files.

The six original Figure A6 illustration panels are preserved in `reference/figures/simulations_hetero_factor_*.eps`. Their exact draw-selection and export commands were not found in the supplied code, so **Figure A6 is reference-only**. No claim is made that the workflow regenerates those particular random illustrations.

The archive excludes the publisher PDF, private drafts, correspondence, R history, saved author sessions, unused Penn World Table data, and machine-specific R libraries.

## Citation and terms

See `CITATION.cff`, [DATA_SOURCES.md](DATA_SOURCES.md), and [THIRD_PARTY_NOTICES.md](THIRD_PARTY_NOTICES.md). No blanket software license is assigned to this archive. Data retain their source licenses; third-party code retains its original rights and attribution. See the third-party notice for the adapted STSM helper.
