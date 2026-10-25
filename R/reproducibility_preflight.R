# Reproducibility preflight check
# Run from the repository root: Rscript R/reproducibility_preflight.R
# Checks the filenames and workbook sheets used by figure scripts.
# It does NOT run models or verify that their plots match the manuscript.

if (!dir.exists("R") || !dir.exists("data") || !file.exists("README.md")) {
  stop("Run this script from the cloned repository root (folder containing R/, data/, README.md).")
}

ftirexpected <- file.path("data", "FTIR", paste0(
  c("CKWS", "GPWS", "PEGWS", "PCK", "PGP", "PPEG", "MCK", "MGP", "MPEG"),
  ".xlsx"
))

xrdfiles <- file.path("data", paste0(
  c("PEG", "control", "GP_Et", "PEG_CEt", "PEG_H", "PEG_HEt",
    "PEG_LEt", "PEG_MEt", "PEG_VLEt"),
  ".txt"
))

eemfiles <- file.path("data", "EEM_parts", sprintf(
  "EEM_plot_ready_part%02d.csv", 1:15
))

mandatory_data <- c(
  "data/Conventional_Treatments.xlsx",
  "data/Phosphate_Primed_Gypsum.xlsx",
  "data/SO4_GP_PEG_Experiment.xlsx",
  "data/validation.xlsx",
  "data/XPS_GP.xls",
  "data/XPS_PEG.xls",
  "data/SEM_PEG.xlsx",
  xrdfiles,
  ftirexpected,
  eemfiles
)

required_script_paths <- c(
  file.path("R", sprintf("Figure_%02d.R", 2:6)),
  file.path("R", "Supplementary_Figures",
            sprintf("Supplementary_Figure_S%02d.R", setdiff(1:17, 7)))
)

report_paths <- function(paths, label) {
  ok <- file.exists(paths)
  cat("\n", label, ": ", sum(ok), "/", length(ok), " present\n", sep = "")
  if (any(!ok)) cat("MISSING:\n", paste(" -", paths[!ok], collapse = "\n"), "\n")
  invisible(ok)
}

ok_data <- report_paths(mandatory_data, "Required numerical/spectroscopic inputs")
ok_scripts <- report_paths(required_script_paths, "Currently published figure scripts")

cat("\nWorkbook sheet check:\n")
expected_sheets <- list(
  "data/Conventional_Treatments.xlsx" = c(
    "data", "Ca_7_30", "Ca_release", "P_dissolve", "Phylum",
    "Genus_repli", "P+GP"
  ),
  "data/Phosphate_Primed_Gypsum.xlsx" = c("data", "pH_Hcl"),
  "data/SO4_GP_PEG_Experiment.xlsx" = c("SO4 Data", "Graph Data"),
  "data/validation.xlsx" = c("validation"),
  "data/SEM_PEG.xlsx" = paste0("Spectrum", 1:6),
  "data/FTIR/CKWS.xlsx" = c("CKWS1", "CKWS2", "CKWS3"),
  "data/FTIR/GPWS.xlsx" = c("GPWS1", "GPWS2", "GPWS3"),
  "data/FTIR/PEGWS.xlsx" = c("PEGWS1", "PEGWS2", "PEGWS3"),
  "data/FTIR/PCK.xlsx" = c("PCK1", "PCK2", "PCK3"),
  "data/FTIR/PGP.xlsx" = c("PGP1", "PGP2", "PGP3"),
  "data/FTIR/PPEG.xlsx" = c("PPEG1", "PPEG2", "PPEG3"),
  "data/FTIR/MCK.xlsx" = c("MCK1", "MCK2", "MCK3"),
  "data/FTIR/MGP.xlsx" = c("MGP1", "MGP2", "MGP3"),
  "data/FTIR/MPEG.xlsx" = c("MPEG1", "MPEG2", "MPEG3"),
  "data/XPS_GP.xls" = c("C1s Scan", "O1s Scan", "Ca2p Scan", "XPS Survey"),
  "data/XPS_PEG.xls" = c("C1s Scan", "O1s Scan", "Ca2p Scan", "P2p Scan", "XPS Survey")
)

sheet_fail <- FALSE
if (!requireNamespace("readxl", quietly = TRUE)) {
  warning("Install the 'readxl' R package to enable workbook sheet checks.")
} else {
  for (filename in names(expected_sheets)) {
    if (!file.exists(filename)) next
    observed <- tryCatch(
      readxl::excel_sheets(filename),
      error = function(e) {
        warning("Cannot read workbook: ", filename, " (", conditionMessage(e), ")")
        character(0)
      }
    )
    missing <- setdiff(expected_sheets[[filename]], observed)
    if (length(missing)) {
      sheet_fail <- TRUE
      cat("MISSING SHEETS in ", filename, ": ",
          paste(missing, collapse = ", "), "\n", sep = "")
    } else cat("OK: ", filename, "\n", sep = "")
  }
}

# Original photo files are intentionally not included in the publication
# repository. Figures 03 and S11 will display blank photo placeholders
# when the author does not provide these local files.
montage <- any(file.exists(c(
  "Plant_Comparison_Tall.png",
  "data/Plant_Comparison_Tall.png"
)))
photo_names <- c("Mongolia.jpg", sprintf("Xian_%02d.jpg", 1:4))
local_photos <- any(vapply(
  c("validation_photos", file.path("data", "validation_photos")),
  function(folder) all(file.exists(file.path(folder, photo_names))),
  logical(1)
))
cat("\nVisual supplements (optional local files):\n")
cat("Figure 03 photo montage: ", if (montage) "present" else "not shared; blank placeholder", "\n", sep = "")
cat("Figure S11 five photos: ", if (local_photos) "present" else "not shared; blank placeholders", "\n", sep = "")

cat("\nSupplementary Figure S07 script: ",
    if (file.exists("R/Supplementary_Figures/Supplementary_Figure_S07.R")) {
      "present"
    } else "not in repository; script not reproducible", "\n", sep = "")

if (!all(ok_data) || !all(ok_scripts) || sheet_fail) {
  stop("Preflight failed: see missing data, scripts or worksheets above.")
}
cat("\nPASS: All listed numerical inputs and available figure scripts exist.\n")
cat("Note: this is a path/sheet check, NOT proof that every R analysis executes.\n")