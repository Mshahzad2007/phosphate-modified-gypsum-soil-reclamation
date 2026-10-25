# =============================================================================
# SUPPLEMENTARY FIGURE 16
# Selected bivariate relationships linking Ca status, salinity-sodicity,
# soil structure, and plant performance across CK, GP, and PEG treatments.
#
# Panels:
# (a) RC vs Ex.Na
# (b) RC vs ESP
# (c) SO4 vs EC
# (d) HCO3 vs pH
# (e) ESP vs MWD
# (f) ESP vs total biomass
# (g) RC vs MCF
# (h) MCF vs MAOC
#
# Lines: ordinary least-squares fits
# Shaded bands: 95% confidence intervals
# Annotations: Pearson correlation coefficient (r) and P value
#
# Input workbook: Conventional_Treatments.xlsx
# Worksheet: data
# Expected source format: long data with treatment, replicate, variable, value
#
# IMPORTANT:
# - This script is prepared for local testing. It has NOT been uploaded to GitHub.
# - The statistics are calculated from the workbook; they are not hard-coded.
# - The input workbook must be in the current R working directory, or file_path
#   below must be changed to its full path.
# =============================================================================


# 0. RESET ---------------------------------------------------------------------

rm(list = ls())
graphics.off()


# 1. PACKAGES ------------------------------------------------------------------

required_packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "ggplot2",
  "patchwork",
  "openxlsx"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    repos = "https://cloud.r-project.org",
    dependencies = TRUE
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(openxlsx)
})


# 2. INPUT AND OUTPUT -----------------------------------------------------------

repo_input_path <- function(filename) {
  candidates <- c(
    filename,
    file.path("data", filename),
    file.path("..", "data", filename),
    file.path("..", "..", "data", filename)
  )
  matches <- candidates[file.exists(candidates)]
  if (length(matches)) return(matches[[1]])
  filename
}

file_path <- repo_input_path("Conventional_Treatments.xlsx")
sheet_name <- "data"

out_dir <- file.path("outputs", "Fig_S16_Bivariate_Correlations")

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}

time_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Figure aspect ratio is close to the supplied reference image.
figure_width_in  <- 8.60
figure_height_in <- 10.75
figure_dpi       <- 600
base_font        <- "Arial"

treatment_levels <- c("CK", "GP", "PEG")


# 3. HELPER FUNCTIONS -----------------------------------------------------------

clean_text <- function(x) {
  x <- as.character(x)

  # Standardise common Unicode spaces and subscript characters.
  x <- gsub("\u00A0", " ", x, fixed = TRUE)
  x <- gsub("\u2007", " ", x, fixed = TRUE)
  x <- gsub("\u202F", " ", x, fixed = TRUE)

  x <- gsub("\u2082", "2", x, fixed = TRUE)
  x <- gsub("\u2083", "3", x, fixed = TRUE)
  x <- gsub("\u2084", "4", x, fixed = TRUE)
  x <- gsub("\u2085", "5", x, fixed = TRUE)
  x <- gsub("\u2086", "6", x, fixed = TRUE)
  x <- gsub("\u2087", "7", x, fixed = TRUE)
  x <- gsub("\u2088", "8", x, fixed = TRUE)
  x <- gsub("\u2089", "9", x, fixed = TRUE)
  x <- gsub("\u207B", "minus", x, fixed = TRUE)
  x <- gsub("\u00B2", "2", x, fixed = TRUE)

  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}


norm_key <- function(x) {
  x <- clean_text(x)
  x <- tolower(gsub("[^[:alnum:]]", "", x))
  x
}


find_col <- function(column_names, candidates) {
  column_keys <- norm_key(column_names)
  candidate_keys <- norm_key(candidates)

  hit <- which(column_keys %in% candidate_keys)

  if (length(hit) == 0) {
    return(NA_character_)
  }

  column_names[hit[1]]
}


canonical_treatment <- function(x) {
  x <- toupper(gsub("[[:space:]]+", "", clean_text(x)))

  dplyr::case_when(
    x %in% c("CK", "CONTROL") ~ "CK",
    x %in% c("GP", "GYPSUM") ~ "GP",
    x %in% c("PEG", "PGP") ~ "PEG",
    TRUE ~ x
  )
}


canonical_variable <- function(x) {
  key <- norm_key(x)

  dplyr::case_when(
    # Reactive Ca
    key %in% c("rc", "reactiveca", "reactivecalcium") |
      grepl("^reactiveca", key) |
      grepl("^rc(cmol|kg|g)", key) ~ "RC",

    # Exchangeable Na; accepts labels such as Ex.Na, Ex_Na, ENa, and Na-Exch
    key %in% c(
      "exna", "ena", "exchangena", "exchangablena",
      "exchangeablesodium", "exchangesodium", "naexch", "naexchangeable"
    ) |
      grepl("^exna|^ena|^exchangena|^exchangablena|^exchangeablesodium|^naexch", key) ~
      "Ex_Na",

    # Exchangeable sodium percentage
    key %in% c("esp", "exchangesodiumpercentage", "exchangablesodiumpercentage") |
      grepl("^esp", key) ~ "ESP",

    # Sulfate
    grepl("^so4", key) |
      grepl("^sulfate|^sulphate", key) ~ "SO4",

    # Electrical conductivity
    key %in% c("ec", "electricalconductivity") |
      grepl("^ec|^electricalconductivity", key) ~ "EC",

    # Bicarbonate
    grepl("^hco3", key) |
      grepl("^bicarbonate", key) ~ "HCO3",

    # pH
    key %in% c("ph", "soilph") ~ "pH",

    # Mean weight diameter
    key %in% c("mwd", "meanweightdiameter") |
      grepl("^mwd", key) ~ "MWD",

    # Total plant biomass
    key %in% c(
      "biomass", "totalbiomass", "plantbiomass",
      "totalplantbiomass", "biomassgpot", "biomassgpot1"
    ) |
      grepl("^totalplantbiomass|^totalbiomass|^plantbiomass|^biomass", key) ~
      "Biomass",

    # MAOM-associated Ca F3, labelled MCF in the figure
    key %in% c(
      "mcf", "maomcaf3", "maomcf", "maomca3",
      "maomcafraction3", "mineralassociatedcaf3"
    ) |
      grepl("^mcf|^maomcaf3|^maomcf|^maomca3|^mineralassociatedcaf3", key) ~
      "MCF",

    # MAOM-associated organic C, labelled MAOC in the figure
    key %in% c(
      "maoc", "maomc", "maomcarbon", "maomorganiccarbon",
      "mineralassociatedorganiccarbon"
    ) |
      grepl("^maoc|^maomcarbon|^maomorganiccarbon|^mineralassociatedorganiccarbon", key) ~
      "MAOC",

    TRUE ~ NA_character_
  )
}


format_p <- function(p) {
  if (is.na(p)) {
    return("P = NA")
  }

  if (p < 0.001) {
    return("P < 0.001")
  }

  paste0("P = ", formatC(p, format = "f", digits = 3))
}


# 4. READ AND VALIDATE WORKBOOK -------------------------------------------------

if (!file.exists(file_path)) {
  stop(
    "Input workbook not found: ", file_path,
    "\nPlace Conventional_Treatments.xlsx in the current R working directory, ",
    "or change file_path near the top of this script.",
    call. = FALSE
  )
}

available_sheets <- readxl::excel_sheets(file_path)

if (!sheet_name %in% available_sheets) {
  stop(
    "Worksheet '", sheet_name, "' was not found in ", file_path, ".\n",
    "Available worksheets: ", paste(available_sheets, collapse = ", "),
    call. = FALSE
  )
}

raw <- readxl::read_excel(
  path = file_path,
  sheet = sheet_name,
  .name_repair = "minimal"
)

names(raw) <- clean_text(names(raw))

treatment_col <- find_col(
  names(raw),
  c("Treatments", "Treatment", "Trt")
)

replicate_col <- find_col(
  names(raw),
  c("Replicates", "Replicate", "Rep")
)

variable_col <- find_col(
  names(raw),
  c(
    "response.varibale",
    "response.variable",
    "response variable",
    "Variable",
    "Response"
  )
)

value_col <- find_col(
  names(raw),
  c("value", "Value", "values", "Values")
)

required_columns <- c(
  treatment_col,
  replicate_col,
  variable_col,
  value_col
)

if (anyNA(required_columns)) {
  stop(
    "Could not identify all required columns in worksheet '", sheet_name, "'.\n",
    "Expected columns equivalent to: Treatments, Replicates, ",
    "response.varibale, value.\n",
    "Columns found: ", paste(names(raw), collapse = " | "),
    call. = FALSE
  )
}


# 5. PREPARE SAMPLE-LEVEL DATA --------------------------------------------------

raw_long <- raw %>%
  dplyr::transmute(
    Treatment = canonical_treatment(.data[[treatment_col]]),
    Replicate = clean_text(.data[[replicate_col]]),
    Original_variable = clean_text(.data[[variable_col]]),
    Variable = canonical_variable(.data[[variable_col]]),
    Value = suppressWarnings(as.numeric(.data[[value_col]]))
  ) %>%
  dplyr::filter(
    Treatment %in% treatment_levels,
    !is.na(Replicate),
    Replicate != "",
    !is.na(Variable),
    is.finite(Value)
  )

if (nrow(raw_long) == 0) {
  stop(
    "No usable CK, GP, or PEG observations were found. Check the treatment ",
    "labels, variable labels, and value column.",
    call. = FALSE
  )
}

variables_needed <- c(
  "RC", "Ex_Na", "ESP", "SO4", "EC",
  "HCO3", "pH", "MWD", "Biomass", "MCF", "MAOC"
)

missing_variables <- setdiff(
  variables_needed,
  unique(raw_long$Variable)
)

if (length(missing_variables) > 0) {
  available_variables <- sort(unique(raw[[variable_col]]))

  stop(
    "The following variables could not be mapped: ",
    paste(missing_variables, collapse = ", "),
    "\nVariable labels in the workbook:\n",
    paste(available_variables, collapse = " | "),
    "\nIf a label uses a different name, add that alias to canonical_variable().",
    call. = FALSE
  )
}

duplicate_records <- raw_long %>%
  dplyr::count(Treatment, Replicate, Variable, name = "records") %>%
  dplyr::filter(records > 1)

if (nrow(duplicate_records) > 0) {
  warning(
    "Duplicate treatment × replicate × variable records were found. ",
    "They will be averaged before plotting. Check the exported data-used CSV."
  )
}

# If duplicates exist, average only within the same treatment, replicate, variable.
# No extra replicates are created.
wide_data <- raw_long %>%
  dplyr::group_by(Treatment, Replicate, Variable) %>%
  dplyr::summarise(
    Value = mean(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = Variable,
    values_from = Value
  ) %>%
  dplyr::mutate(
    Treatment = factor(Treatment, levels = treatment_levels)
  ) %>%
  dplyr::arrange(Treatment, Replicate)

if (nrow(wide_data) != 9) {
  warning(
    "The prepared data contain ", nrow(wide_data),
    " treatment × replicate rows; the caption expects n = 9 ",
    "(three treatments × three replicates). Check the workbook and filters."
  )
}

missing_sample_values <- wide_data %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(variables_needed),
      ~ sum(is.na(.x))
    )
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "Variable",
    values_to = "Missing_n"
  ) %>%
  dplyr::filter(Missing_n > 0)

if (nrow(missing_sample_values) > 0) {
  warning(
    "Missing sample-level values were detected for: ",
    paste(
      paste0(
        missing_sample_values$Variable,
        " (", missing_sample_values$Missing_n, ")"
      ),
      collapse = ", "
    ),
    ". Panel-specific n may be lower than 9."
  )
}


# 6. DEFINE THE EIGHT PANELS ----------------------------------------------------

panel_specs <- list(
  list(
    tag = "a",
    x = "RC",
    y = "Ex_Na",
    colour = "#35A6C8",
    xlab = expression(RC~(cmol[c]~kg^{-1})),
    ylab = expression(Ex.Na~(cmol[c]~kg^{-1}))
  ),
  list(
    tag = "b",
    x = "RC",
    y = "ESP",
    colour = "#009F83",
    xlab = expression(RC~(cmol[c]~kg^{-1})),
    ylab = expression(ESP~" (%)")
  ),
  list(
    tag = "c",
    x = "SO4",
    y = "EC",
    colour = "#F04E37",
    xlab = expression(SO[4]^{2*"-"}~(mmol~L^{-1})),
    ylab = expression(EC~(dS~m^{-1}))
  ),
  list(
    tag = "d",
    x = "HCO3",
    y = "pH",
    colour = "#E88B70",
    xlab = expression(HCO[3]^"-"~(meq~L^{-1})),
    ylab = expression(pH)
  ),
  list(
    tag = "e",
    x = "ESP",
    y = "MWD",
    colour = "#345B9D",
    xlab = expression(ESP~" (%)"),
    ylab = expression(MWD~(mm))
  ),
  list(
    tag = "f",
    x = "ESP",
    y = "Biomass",
    colour = "#7387BC",
    xlab = expression(ESP~" (%)"),
    ylab = expression(Biomass~(g~pot^{-1}))
  ),
  list(
    tag = "g",
    x = "RC",
    y = "MCF",
    colour = "#8B70C7",
    xlab = expression(RC~(cmol[c]~kg^{-1})),
    ylab = expression(MCF~(cmol[c]~kg^{-1}))
  ),
  list(
    tag = "h",
    x = "MCF",
    y = "MAOC",
    colour = "#72C6B5",
    xlab = expression(MCF~(cmol[c]~kg^{-1})),
    ylab = expression(MAOC~" (%)")
  )
)


# 7. FIT CORRELATIONS AND CREATE PANELS -----------------------------------------

make_panel <- function(spec, dat) {
  d <- dat %>%
    dplyr::filter(
      is.finite(.data[[spec$x]]),
      is.finite(.data[[spec$y]])
    )

  n <- nrow(d)

  if (n < 3) {
    stop(
      "Panel (", spec$tag, ") has fewer than three complete observations: ",
      spec$x, " vs ", spec$y, ".",
      call. = FALSE
    )
  }

  if (
    stats::sd(d[[spec$x]]) == 0 ||
    stats::sd(d[[spec$y]]) == 0
  ) {
    stop(
      "Panel (", spec$tag, ") contains a constant variable; Pearson r ",
      "cannot be calculated for ", spec$x, " vs ", spec$y, ".",
      call. = FALSE
    )
  }

  correlation_test <- stats::cor.test(
    d[[spec$x]],
    d[[spec$y]],
    method = "pearson"
  )

  fit <- stats::lm(
    stats::reformulate(spec$x, response = spec$y),
    data = d
  )

  r_value <- unname(correlation_test$estimate)
  p_value <- correlation_test$p.value
  coefficients <- stats::coef(fit)

  stats_row <- data.frame(
    Panel = paste0("(", spec$tag, ")"),
    X_variable = spec$x,
    Y_variable = spec$y,
    n = n,
    Pearson_r = r_value,
    P_value = p_value,
    Intercept = unname(coefficients[1]),
    Slope = unname(coefficients[2]),
    R_squared = summary(fit)$r.squared,
    stringsAsFactors = FALSE
  )

  stat_label <- paste0(
    "r = ", sprintf("%.2f", r_value), "\n",
    format_p(p_value)
  )

  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = .data[[spec$x]],
      y = .data[[spec$y]]
    )
  ) +
    ggplot2::geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = TRUE,
      level = 0.95,
      colour = spec$colour,
      fill = spec$colour,
      alpha = 0.42,
      linewidth = 1.05
    ) +
    ggplot2::annotate(
      "text",
      x = -Inf,
      y = Inf,
      label = stat_label,
      hjust = -0.03,
      vjust = 1.20,
      size = 3.15,
      lineheight = 0.98,
      family = base_font,
      colour = "black"
    ) +
    ggplot2::labs(
      x = spec$xlab,
      y = spec$ylab,
      tag = paste0("(", spec$tag, ")")
    ) +
    ggplot2::theme_classic(
      base_size = 10.5,
      base_family = base_font
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(
        family = base_font,
        colour = "black"
      ),
      axis.title = ggplot2::element_text(
        size = 10.0,
        colour = "black"
      ),
      axis.text = ggplot2::element_text(
        size = 9.0,
        colour = "black"
      ),
      axis.line = ggplot2::element_line(
        colour = "black",
        linewidth = 0.55
      ),
      axis.ticks = ggplot2::element_line(
        colour = "black",
        linewidth = 0.55
      ),
      axis.ticks.length = grid::unit(1.5, "mm"),
      plot.tag = ggplot2::element_text(
        size = 11.2,
        face = "bold",
        colour = "black"
      ),
      plot.tag.position = c(0, 1.02),
      plot.margin = ggplot2::margin(
        t = 12,
        r = 8,
        b = 6,
        l = 8
      ),
      legend.position = "none",
      panel.background = ggplot2::element_rect(
        fill = "white",
        colour = NA
      ),
      plot.background = ggplot2::element_rect(
        fill = "white",
        colour = NA
      )
    )

  list(
    plot = p,
    stats = stats_row
  )
}


panel_results <- lapply(
  panel_specs,
  make_panel,
  dat = wide_data
)

panel_plots <- lapply(
  panel_results,
  function(z) z[["plot"]]
)

names(panel_plots) <- letters[1:8]

stats_table <- dplyr::bind_rows(
  lapply(
    panel_results,
    function(z) z[["stats"]]
  )
)


# 8. ASSEMBLE THE FIGURE --------------------------------------------------------

# Each letter occupies one rectangular area.
# '#' is an empty cell in a patchwork design.
# This produces three panels, three panels, then two centred panels.
layout_design <- "
AABBCC
DDEEFF
#GGHH#
"

fig_s16 <- patchwork::wrap_plots(
  plotlist = panel_plots,
  design = layout_design
)

print(fig_s16)


# 9. EXPORT FIGURE, STATISTICS, AND DATA ----------------------------------------

file_prefix <- paste0(
  "Supplementary_Figure_S16_Bivariate_Correlations_",
  time_stamp
)

png_file <- file.path(
  out_dir,
  paste0(file_prefix, ".png")
)

tiff_file <- file.path(
  out_dir,
  paste0(file_prefix, ".tiff")
)

pdf_file <- file.path(
  out_dir,
  paste0(file_prefix, ".pdf")
)

xlsx_file <- file.path(
  out_dir,
  paste0("Supplementary_Figure_S16_Statistics_", time_stamp, ".xlsx")
)

csv_file <- file.path(
  out_dir,
  paste0("Supplementary_Figure_S16_Data_Used_", time_stamp, ".csv")
)


ggplot2::ggsave(
  filename = png_file,
  plot = fig_s16,
  width = figure_width_in,
  height = figure_height_in,
  units = "in",
  dpi = figure_dpi,
  bg = "white",
  limitsize = FALSE
)

ggplot2::ggsave(
  filename = tiff_file,
  plot = fig_s16,
  width = figure_width_in,
  height = figure_height_in,
  units = "in",
  dpi = figure_dpi,
  device = "tiff",
  compression = "lzw",
  bg = "white",
  limitsize = FALSE
)

if (capabilities("cairo")) {
  ggplot2::ggsave(
    filename = pdf_file,
    plot = fig_s16,
    width = figure_width_in,
    height = figure_height_in,
    units = "in",
    device = grDevices::cairo_pdf,
    bg = "white",
    limitsize = FALSE
  )
} else {
  ggplot2::ggsave(
    filename = pdf_file,
    plot = fig_s16,
    width = figure_width_in,
    height = figure_height_in,
    units = "in",
    device = grDevices::pdf,
    useDingbats = FALSE,
    bg = "white",
    limitsize = FALSE
  )
}


# Export the sample-level data actually used in the eight panels.
utils::write.csv(
  wide_data,
  file = csv_file,
  row.names = FALSE,
  na = ""
)

stats_workbook <- openxlsx::createWorkbook()

openxlsx::addWorksheet(stats_workbook, "Pearson_correlations")
openxlsx::writeData(
  stats_workbook,
  "Pearson_correlations",
  stats_table
)

openxlsx::addWorksheet(stats_workbook, "Data_used")
openxlsx::writeData(
  stats_workbook,
  "Data_used",
  wide_data
)

openxlsx::addWorksheet(stats_workbook, "Variable_mapping")

mapping_table <- raw_long %>%
  dplyr::distinct(Original_variable, Variable) %>%
  dplyr::arrange(Variable, Original_variable)

openxlsx::writeData(
  stats_workbook,
  "Variable_mapping",
  mapping_table
)

openxlsx::saveWorkbook(
  stats_workbook,
  file = xlsx_file,
  overwrite = TRUE
)


# 10. REPORT OUTPUTS ------------------------------------------------------------

message("\nSupplementary Figure 16 export completed.")
message("PNG:  ", normalizePath(png_file, winslash = "/", mustWork = FALSE))
message("TIFF: ", normalizePath(tiff_file, winslash = "/", mustWork = FALSE))
message("PDF:  ", normalizePath(pdf_file, winslash = "/", mustWork = FALSE))
message("Stats:", normalizePath(xlsx_file, winslash = "/", mustWork = FALSE))
message("Data: ", normalizePath(csv_file, winslash = "/", mustWork = FALSE))

message(
  "\nThe Pearson r and P values were calculated from the selected CK, GP, and PEG ",
  "observations in the input workbook. Review the exported Statistics workbook ",
  "and Data_Used CSV before finalising the manuscript caption."
)

# =============================================================================
# Suggested caption:
# Supplementary Fig. 16 | Selected bivariate relationships linking Ca status,
# salinity-sodicity, soil structure, and plant performance across CK, GP, and
# PEG treatments (n = 9). (a) RC vs Ex.Na; (b) RC vs ESP; (c) SO4^2- vs EC;
# (d) HCO3- vs pH; (e) ESP vs MWD; (f) ESP vs total biomass; (g) RC vs MCF;
# (h) MCF vs MAOC. Lines show ordinary least-squares fits and shaded bands
# denote 95% confidence intervals. Pearson correlation coefficients (r) and
# P values are shown within each panel.
# =============================================================================