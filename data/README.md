# Figure-generation input data

These are reduced copies of the original workbooks. Only worksheets currently required by the figure scripts were retained. The original uploaded workbooks were not modified.

## Workbooks and retained worksheets

### `Conventional_Treatments.xlsx`
- `data` — primary long-format soil and plant variables; used by Figures 03–06 and Supplementary Figures 08, 10, 12, 14–17.
- `Ca_7_30` — Ca-pool observations at days 7 and 30; used by Supplementary Figure 09.
- `Ca_release` — Ca-release data; used by Supplementary Figure 06.
- `P_dissolve` — dissolved P time series; used by Supplementary Figure 06.
- `Phylum` — phylum-level microbial composition; used by Figure 05 and Supplementary Figure 17.
- `Genus_repli` — replicate-level genus composition; used by Figure 05 and Supplementary Figure 17.
- `P+GP` — additional P+GP treatment observations used when needed by Supplementary Figures 10 and 12.

### `Phosphate_Primed_Gypsum.xlsx`
- `data` — phosphate-dose data; used by Supplementary Figure 05.
- `pH_Hcl` — HCl-gradient pH and SAP data; used by Supplementary Figure 05.

### `SO4_GP_PEG_Experiment.xlsx`
- `SO4 Data` — sulfate-release measurements and supporting calculations.
- `Graph Data` — graph-ready sulfate series used by Supplementary Figure 06.

## Added characterization and spectroscopy inputs

### XRD text files
Raw XRD text data are stored in `data/`: `PEG.txt`, `control.txt`, `GP_Et.txt`, `PEG_CEt.txt`, `PEG_H.txt`, `PEG_HEt.txt`, `PEG_LEt.txt`, `PEG_MEt.txt`, and `PEG_VLEt.txt`.

### XPS spectra
The provided XPS source workbooks were exported sheet-by-sheet as CSV files under `data/XPS_CSV/`. The folder contains the O1s, Ca2p, C1s, P2p, survey, peak-table, and title sheets for both GP and PEG. The empty `Sheet1` worksheets were omitted. The original binary `.xls` workbooks are not stored in this repository; these CSV exports preserve the tabular data, not the original Excel formatting.

### EDS data
`SEM_PEG.csv` contains the tabular EDS data exported from the six spectra in the provided `SEM_PEG.xlsx` workbook. The original binary `.xlsx` workbook is not stored in this repository.

### EEM fluorescence data
The original `EEM_plot_ready.csv` was too large for a single connector upload, so it is stored as 15 individually readable CSV parts in `data/EEM_parts/`. Each part includes the column header. **`R/Figure_05.R` now loads and combines the 15 parts automatically when the single CSV is unavailable**, with checks for a complete 14,004-row, 78-column dataset. The original measurements are unchanged. For use outside Figure 05, the parts can be combined manually in numeric order:

```r
eem_files <- sort(list.files(
  file.path(data_dir, "EEM_parts"),
  pattern = "^EEM_plot_ready_part[0-9]+\\.csv$",
  full.names = TRUE
))
eem_data <- dplyr::bind_rows(lapply(
  eem_files,
  readr::read_csv,
  show_col_types = FALSE
))
```

## Notes for reproducibility

- The three reduced workbooks contain the measured values needed by the listed scripts. Formula cells in those reduced copies were replaced with saved calculated values so that R can read the results consistently without depending on Excel recalculation. Keep the original workbooks separately if spreadsheet formulas or formatting are needed.
- The figure scripts look for inputs in the repository's `data/` directory, while also allowing a local copy in the working directory or parent directories.
- Still required for full reproduction are the FTIR workbooks, the Figure 03 plant montage, and the validation workbook and photographs.
- Treat all inputs as experimental source data. Do not replace missing observations with simulated or practice values.
