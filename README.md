# Phosphate-modified gypsum drives sustainable restoration of saline-alkaline soils through calcium regulation

This repository contains the R scripts and experimental inputs used to prepare manuscript Figures 02–06 and available Supplementary Figures S01–S17 (except S07, whose script is not currently archived here).

## Reproduction instructions (start here)

1. Download or clone this repository, including the `data/` folder.
2. In RStudio, set the **working directory to the repository root**, the folder containing `README.md`, `R/` and `data/`. Do not set it to the folder containing an individual R script.
3. Install the R packages listed at the top of the figure script you plan to run. The map in S01 additionally uses `rnaturalearthhires`, which may require a download from GitHub and system geospatial dependencies.
4. Run the input inventory: `source("R/reproducibility_preflight.R")`, or from a terminal in the repository root run `Rscript R/reproducibility_preflight.R`.
5. Run a complete figure script with RStudio's **Source** command, for example `source("R/Figure_05.R")`; run other scripts from their paths under `R/` and `R/Supplementary_Figures/`.
6. Compare the generated figures and source-data spreadsheets against the manuscript. The preflight verifies filenames and spreadsheet sheets; it is **not** an end-to-end analytical or image-diff test.

Most script paths resolve their inputs under `data/` from the repository root (or locate a local copy in the working directory). The original XPS/EDS workbooks, the nine FTIR workbooks and the validation workbook are present under these exact names without upload suffixes such as `(2)`.

## Figure input guide

| Script(s) | Required numerical input | Status |
| --- | --- | --- |
| Figure 02 | `data/PEG.txt`, `data/XPS_GP.xls`, `data/XPS_PEG.xls`, `data/SEM_PEG.xlsx` | Present |
| Figure 03 | `data/Conventional_Treatments.xlsx` (`data` sheet) | Present; original plant-photo montage is not included |
| Figure 04 | `data/Conventional_Treatments.xlsx` (`data` sheet) | Present |
| Figure 05 | `data/Conventional_Treatments.xlsx` plus `data/EEM_parts/` | Present; all 15 EEM parts are combined automatically |
| Figure 06 | `data/Conventional_Treatments.xlsx` plus nine workbooks in `data/FTIR/` | Present; no local Windows path required |
| Supplementary S01 | Self-contained map and texture inputs; packages and map boundaries downloaded as required | No local data files required |
| Supplementary S02 | XRD `.txt` files in `data/` | Present; no local Windows path required |
| Supplementary S03 | `data/XPS_GP.xls`, `data/XPS_PEG.xls` | Present |
| Supplementary S04 | `data/SEM_PEG.xlsx` | Present |
| Supplementary S05 | `data/Phosphate_Primed_Gypsum.xlsx` | Present |
| Supplementary S06 | `data/Conventional_Treatments.xlsx`, `data/SO4_GP_PEG_Experiment.xlsx` | Present |
| Supplementary S07 | R script is not in this repository | **Script unavailable** |
| Supplementary S08–S10 | `data/Conventional_Treatments.xlsx` | Present |
| Supplementary S11 | `data/validation.xlsx` (`validation` sheet) | Present; photos are intentionally not published |
| Supplementary S12 | `data/Conventional_Treatments.xlsx` including `P+GP` | Present |
| Supplementary S13 | Treatment-level values embedded in the R script | Can regenerate the displayed figure; raw sequencing/ASV pipeline is not included |
| Supplementary S14–S17 | `data/Conventional_Treatments.xlsx` | Present |

### Visual assets intentionally not included

`Plant_Comparison_Tall.png` is not included for Figure 03; the figure script warns and draws empty photo placeholders. The five validation photographs (`Mongolia.jpg`, `Xian_01.jpg`–`Xian_04.jpg`) are likewise deliberately excluded: Supplementary Figure S11 warns and leaves its photo areas blank. These choices preserve the numerical panels but **do not reproduce the complete photographic compositions**. Authors with permission to use the photographs may provide them locally; they should not be substituted with generated images.

### Data provenance and scope

The three main soil/chemistry workbooks are reduced copies containing the worksheets needed by figure scripts; formula cells were replaced with their saved calculated values. The original source measurements, not practice or simulated values, are the intended basis for reproduction. Original binary XPS/EDS workbooks are also archived in `data/`; their CSV exports remain in `data/XPS_CSV/` and `data/SEM_PEG.csv` for convenience.

The original EEM dataset is stored as 15 ordered files in `data/EEM_parts/`. `Figure_05.R` reconstructs it in memory without changing measured values. Additional details and worksheet inventories are in [`data/README.md`](data/README.md).

### Limitations

The repository has been checked for file presence, workbook sheet names, and hard-coded local paths. A complete run of all plotting and statistical scripts, verification of every output against the manuscript, and full pinning of R package versions have **not** been completed here. Reproduction of S01 may require external map-package downloads; scientific reanalysis of S13 from raw sequencing reads would require raw sequencing/ASV inputs. The unavailable S07 script must be supplied if that supplementary figure is in scope.
