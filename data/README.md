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

## Notes for reproducibility

- These reduced workbooks contain the original measured values needed by the listed scripts. Formula cells in the reduced copies were replaced with their saved calculated values so that R can read the results consistently without depending on Excel recalculation. Keep the original workbooks separately if the spreadsheet formulas themselves are needed.
- The figure scripts now look for inputs in the repository's `data/` directory, while also allowing a local copy in the working directory or parent directories.
- Other inputs are still required for full reproduction of all figures, including XPS/EDS workbooks, XRD text files, `EEM_plot_ready.csv`, FTIR workbooks, the Figure 03 plant montage, and validation data/photographs. Those assets have not yet been added to this folder.
- All inputs should be treated as experimental source data. Do not replace missing observations with simulated or practice values.
