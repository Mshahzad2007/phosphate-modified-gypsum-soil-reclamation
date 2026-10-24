# Phosphate-modified gypsum drives sustainable restoration of saline-alkaline soils through calcium regulation

This repository contains the R scripts used to generate the manuscript figures and supplementary figures.

## Figure-generation input data

The `data/` folder contains reduced copies of the Excel workbooks, retaining only worksheets currently referenced by the figure-generation scripts:

- [`Conventional_Treatments.xlsx`](data/Conventional_Treatments.xlsx)
- [`Phosphate_Primed_Gypsum.xlsx`](data/Phosphate_Primed_Gypsum.xlsx)
- [`SO4_GP_PEG_Experiment.xlsx`](data/SO4_GP_PEG_Experiment.xlsx)

See [`data/README.md`](data/README.md) for the retained worksheet inventory and the figures that use each worksheet.

The relevant R scripts now resolve these workbook inputs from the repository's `data/` folder, while allowing a local copy in the working directory or parent directories. For the simplest setup, run the scripts with the repository root as the R working directory.

## Reproducing the figures

1. Download or clone this repository.
2. Open R/RStudio and set the working directory to the repository root.
3. Install the R packages required by the script you intend to run.
4. Run the relevant script under `R/` or `R/Supplementary_Figures/`.
5. Check the generated figure and statistics against the manuscript before use.

## Inputs still to be added

The repository is not yet a complete one-click reproduction package. Some scripts still require additional assets that have not been added to `data/`, including XPS workbooks and scans, `SEM_PEG.xlsx`, XRD text files, `EEM_plot_ready.csv`, FTIR workbooks, the Figure 03 plant montage, and validation workbooks/photographs. These requirements will be documented as the remaining assets are added.

## Data integrity

The reduced workbooks retain the measured values needed by the currently identified figure scripts. Formula cells in the reduced copies were replaced by their saved calculated values so that R can read them consistently. The original workbooks should be retained separately if the spreadsheet formulas themselves are needed. Do not substitute practice or simulated values for experimental observations.
