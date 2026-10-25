# Run archived figure scripts in isolated R processes, with a per-figure log.
#
# From the cloned repository root:
#   Rscript R/run_all_figures.R
#   Rscript R/run_all_figures.R main
#   Rscript R/run_all_figures.R supplementary
#
# Prerequisite: run R/install_dependencies.R first.
# Source files, statistical models, and display settings are not modified.
# S07 is intentionally excluded until its original R script is supplied.

if (!dir.exists("R") || !dir.exists("data") || !file.exists("README.md")) {
  stop("Run from the repository root (containing README.md, R/ and data/).")
}

selected <- commandArgs(trailingOnly=TRUE)
scope <- if (length(selected)) tolower(selected[[1L]]) else "all"
if (!scope %in% c("all", "main", "supplementary")) {
  stop("Choose all, main, or supplementary (or supply no argument).")
}

# Fail early if an expected source data file, worksheet or script is missing.
source("R/reproducibility_preflight.R", local=TRUE)

main_scripts <- file.path("R", sprintf("Figure_%02d.R", 2:6))
supp_scripts <- file.path(
  "R", "Supplementary_Figures",
  sprintf("Supplementary_Figure_S%02d.R", setdiff(1:17, 7))
)
scripts <- switch(
  scope,
  all=c(main_scripts, supp_scripts),
  main=main_scripts,
  supplementary=supp_scripts
)

rscript <- Sys.which("Rscript")
if (!nzchar(rscript)) {
  rscript <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )
}
if (!file.exists(rscript)) stop("Cannot locate the Rscript executable.")

logdir <- file.path("outputs", "_logs")
dir.create(logdir, showWarnings=FALSE, recursive=TRUE)
summary_path <- file.path("outputs", "reproduction_summary.csv")

result <- data.frame(
  script=basename(scripts),
  status=rep("NOT_RUN", length(scripts)),
  exit_code=rep(NA_integer_, length(scripts)),
  log_file=basename(sub("\\.R$", ".log", scripts)),
  stringsAsFactors=FALSE
)

writeLines(capture.output(sessionInfo()), file.path("outputs", "R_sessionInfo.txt"))
installed <- as.data.frame(installed.packages()[, c("Package", "Version")],
                           stringsAsFactors=FALSE)
write.csv(installed, file.path("outputs", "R_installed_package_versions.csv"),
          row.names=FALSE)

for (i in seq_along(scripts)) {
  script <- scripts[[i]]
  log_file <- file.path(logdir, result$log_file[[i]])
  message("\n[", i, "/", length(scripts), "] Running ", script)
  code <- tryCatch(
    suppressWarnings(system2(
      command=rscript,
      args=shQuote(script),
      stdout=log_file,
      stderr=log_file,
      wait=TRUE
    )),
    error=function(e) {
      writeLines(conditionMessage(e), log_file)
      999L
    }
  )
  code <- as.integer(code)
  result$exit_code[[i]] <- code
  result$status[[i]] <- if (identical(code, 0L)) "PASS" else "FAIL"
  write.csv(result, summary_path, row.names=FALSE)
  message(result$status[[i]], ": ", script, " (log: ", log_file, ")")
}

cat("\nReproduction run summary:\n")
print(result[, c("script", "status", "exit_code")], row.names=FALSE)
cat("\nDetailed logs: ", logdir, "\n", sep="")
cat("Summary CSV: ", summary_path, "\n", sep="")
if (any(result$status != "PASS")) {
  stop(sum(result$status != "PASS"),
       " script(s) failed. Review their logs. Do not claim full reproduction.")
}
message("All selected archived scripts exited successfully. Compare figures with the manuscript.")
