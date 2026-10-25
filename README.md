# Phosphate-modified gypsum drives sustainable restoration of saline-alkaline soils through calcium regulation

This repository contains the R scripts and experimental inputs used to prepare manuscript Figures 02–06 and available Supplementary Figures S01–S17 (except S07, whose script is not currently archived here).

## Reproduction instructions (start here)

1. Download or clone this repository, including the entire `data/` folder.
2. Open a terminal (or RStudio) **in the repository root**, the folder containing `README.md`, `R/`, and `data/`. Do not change the working directory to an individual script folder.
3. Install required R packages with `Rscript R/install_dependencies.R`. The map in S01 additionally requires high-resolution boundaries; see *Special requirements* below.
4. Run `Rscript R/reproducibility_preflight.R` (or `source("R/reproducibility_preflight.R")` in RStudio). It checks **40 input files**, required worksheet names, the 15 EEM parts, and whether **all 21 archived figure R scripts parse**.
5. Generate individual figures, for example `Rscript R/Figure_05.R` in the repository-root terminal, or `source("R/Figure_05.R")` in RStudio. For supplementary figures use paths under `R/Supplementary_Figures/`.
6. Find generated figures, tables and status files under the appropriate subfolder of **`outputs/`**. This folder is ignored by Git so generated files are not mistaken for experimental inputs.
7. Compare generated images and tables against the submitted manuscript; record `sessionInfo()` and package versions when reporting a full reproduction. Passing preflight is **not** proof that statistical fits, plot rendering and the original figure artwork are identical.

The automated GitHub Actions [reproducibility preflight](.github/workflows/reproducibility-preflight.yml) executes the same static/data checks on pushes and pull requests. It does **not** run the entire figure pipeline.

All archived scripts use repository-relative source paths under `data/`, assuming they are launched from the repository root. The original XPS/EDS workbooks, nine FTIR workbooks, and validation workbook use clean filenames without upload suffixes such as `(2)`.

### Special requirements

- **Supplementary S01:** `sf` needs the system geospatial libraries required by your operating system; the optional high-resolution `rnaturalearthhires` dataset can be installed with `INSTALL_MAP_HIRES=1 Rscript R/install_dependencies.R` (Linux/macOS) or `$env:INSTALL_MAP_HIRES="1"; Rscript R/install_dependencies.R` (PowerShell). Internet access is required during installation.
- **Fonts and image devices:** the scripts use Arial or sans-serif and export PDF, TIFF, and PNG. Rendering can vary across operating systems even if the numerical inputs and calculations match.
- **Supplementary S07:** the source R script is still missing; providing it is necessary to reproduce that supplementary figure.
- **Photographs:** Figure 03's plant montage and S11's Mongolia/Xinjiang photos are intentionally not published, so full photographic figures cannot be recreated from the repository alone. S11's numerical panels are still reproducible.
- **Supplementary S13:** reported numerical summary values are embedded in the script. Recomputing panels (a–c) from raw sequencing/ASV records is not possible without those source datasets.

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

The repository has been checked for file presence, expected workbook sheet names, and machine-specific file paths. The improved preflight also checks R source parsing and EEM-part row counts. A full execution of every analysis and figure, pixel-level comparison with the manuscript, and exact R package version locking have **not** been completed. GitHub Actions covers static checks, not full scientific reproducibility. Reproduction of S01 may require external map-package downloads; scientific reanalysis of S13 from raw sequencing reads would require raw sequencing/ASV inputs. The unavailable S07 script must be supplied if that supplementary figure is in scope.
