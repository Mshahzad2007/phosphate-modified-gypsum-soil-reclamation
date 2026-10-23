# =============================================================================
# SUPPLEMENTARY FIGURE S16 | SELECTED BIVARIATE RELATIONSHIPS
# Eight OLS regressions with 95% confidence intervals
#
# Panels:
# (a) RC vs exchangeable Na
# (b) RC vs ESP
# (c) SO4 vs EC
# (d) HCO3 vs pH
# (e) ESP vs MWD
# (f) ESP vs total biomass
# (g) RC vs MCF
# (h) MCF vs MAOC
#
# Input workbook: Conventional_Treatments.xlsx
# Sheet: data
# Expected format: long data with treatment, replicate, variable, and value.
# The script accepts the historical spelling "response.varibale".
# Only CK, GP, and PEG are included (n should be 9 for complete data).
# =============================================================================

rm(list = ls())
graphics.off()
cat("\014")

# 1. PACKAGES AND PATHS --------------------------------------------------------

required_packages <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "patchwork", "openxlsx"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Please install the following packages before running this script: ",
    paste(missing_packages, collapse = ", ")
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

file_in <- "Conventional_Treatments.xlsx"
sheet_in <- "data"

out_dir <- "Fig_S16_Bivariate_Correlations"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Use the same order as the original figure.
treatment_levels <- c("CK", "GP", "PEG")

# Figure dimensions are close to the supplied 3 + 3 + 2 centered layout.
fig_width_in  <- 8.60
fig_height_in <- 9.35
fig_dpi       <- 600
base_family   <- "sans"

# 2. HELPERS -------------------------------------------------------------------

clean_text <- function(x) {
  z <- as.character(x)
  z <- gsub("\u00A0", " ", z, fixed = TRUE)
  z <- gsub("\u2007", " ", z, fixed = TRUE)
  z <- gsub("\u202F", " ", z, fixed = TRUE)
  z <- gsub("\u2084", "4", z, fixed = TRUE)
  z <- gsub("\u2083", "3", z, fixed = TRUE)
  z <- gsub("\u2082", "2", z, fixed = TRUE)
  z <- gsub("\u00B2", "2", z, fixed = TRUE)
  z <- gsub("\u207B", "minus", z, fixed = TRUE)
  z <- gsub("\\s+", " ", z)
  trimws(z)
}

norm_key <- function(x) {
  tolower(gsub("[^[:alnum:]]", "", clean_text(x)))
}

pick_col <- function(nms, candidates) {
  nm_key <- norm_key(nms)
  cand_key <- norm_key(candidates)
  hit <- which(nm_key %in% cand_key)
  if (length(hit) == 0) return(NA_character_)
  nms[hit[1]]
}

canonical_variable <- function(x) {
  k <- norm_key(x)

  dplyr::case_when(
    k %in% c(
      "rc", "reactiveca", "reactivecalcium"
    ) ~ "RC",

    k %in% c(
      "exna", "ena", "exchangena", "exchangablena",
      "exchangeablesodium", "exchangesodium"
    ) ~ "Ex_Na",

    k %in% c("esp", "exchangesodiumpercentage") ~ "ESP",

    grepl("^so4", k) |
      grepl("sulfate|sulphate", k) ~ "SO4",

    k %in% c("ec", "electricalconductivity") ~ "EC",

    grepl("^hco3", k) |
      grepl("bicarbonate", k) ~ "HCO3",

    k %in% c("ph", "soilph") ~ "pH",

    k %in% c("mwd", "meanweightdiameter") ~ "MWD",

    k %in% c(
      "biomass", "totalbiomass", "plantbiomass",
      "totalplantbiomass", "biomassgpot", "biomassgpot1"
    ) ~ "Biomass",

    k %in% c(
      "mcf", "maomcaf3", "maomcf", "maomca3",
      "mineralassociatedcaf3"
    ) ~ "MCF",

    k %in% c(
      "maoc", "maomc", "mineralassociatedorganiccarbon"
    ) ~ "MAOC",

    TRUE ~ NA_character_
  )
}

format_p <- function(p) {
  if (is.na(p)) return("P = NA")
  if (p < 0.001) return("P < 0.001")
  paste0("P = ", formatC(p, format = "f", digits = 3))
}

# 3. READ AND PREPARE DATA ------------------------------------------------------

if (!file.exists(file_in)) {
  stop(
    "Input workbook not found: ", file_in,
    "\nPlace Conventional_Treatments.xlsx in the current R working directory ",
    "or edit file_in near the top of this script."
  )
}

raw <- readxl::read_excel(file_in, sheet = sheet_in)

treatment_col <- pick_col(
  names(raw), c("Treatments", "Treatment", "trt")
)
replicate_col <- pick_col(
  names(raw), c("Replicates", "Replicate", "rep")
)
variable_col <- pick_col(
  names(raw),
  c(
    "response.varibale", "response.variable",
    "response variable", "variable", "response"
  )
)
value_col <- pick_col(
  names(raw), c("value", "values")
)

if (anyNA(c(treatment_col, replicate_col, variable_col, value_col))) {
  stop(
    "Could not identify all required columns in sheet '", sheet_in, "'.\n",
    "Required: treatment, replicate, variable, value.\n",
    "Columns found: ", paste(names(raw), collapse = ", ")
  )
}

raw_long <- raw %>%
  transmute(
    Treatment = toupper(clean_text(.data[[treatment_col]])),
    Replicate = clean_text(.data[[replicate_col]]),
    Original_variable = clean_text(.data[[variable_col]]),
    Variable = canonical_variable(.data[[variable_col]]),
    Value = suppressWarnings(as.numeric(.data[[value_col]]))
  ) %>%
  filter(
    Treatment %in% treatment_levels,
    !is.na(Replicate), Replicate != "",
    !is.na(Variable),
    is.finite(Value)
  )

if (nrow(raw_long) == 0) {
  stop(
    "No usable CK, GP, or PEG data were found. Check treatment labels and ",
    "variable names in the workbook."
  )
}

variables_needed <- c(
  "RC", "Ex_Na", "ESP", "SO4", "EC",
  "HCO3", "pH", "MWD", "Biomass", "MCF", "MAOC"
)

missing_variables <- setdiff(
  variables_needed, unique(raw_long$Variable)
)

if (length(missing_variables) > 0) {
  available_original <- sort(unique(raw[[variable_col]]))
  stop(
    "The following variables could not be mapped: ",
    paste(missing_variables, collapse = ", "),
    "\nVariable labels available in the workbook:\n",
    paste(available_original, collapse = " | "),
    "\nEdit canonical_variable() to add any missing alias."
  )
}

# Average duplicate records only within the same treatment × replicate × variable.
# This does not create additional replicates.
wide_data <- raw_long %>%
  group_by(Treatment, Replicate, Variable) %>%
  summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(Treatment = factor(Treatment, levels = treatment_levels)) %>%
  arrange(Treatment, Replicate)

if (nrow(wide_data) != 9) {
  warning(
    "The prepared data contain ", nrow(wide_data),
    " treatment × replicate rows rather than the expected 9. ",
    "Check the workbook before interpreting the results."
  )
}

# 4. PANEL DEFINITIONS ----------------------------------------------------------

panel_specs <- list(
  list(
    tag = "a", x = "RC", y = "Ex_Na", colour = "#35A6C8",
    xlab = expression(RC~(cmol[c]~kg^{-1})),
    ylab = expression(Ex.Na~(cmol[c]~kg^{-1}))
  ),
  list(
    tag = "b", x = "RC", y = "ESP", colour = "#009F83",
    xlab = expression(RC~(cmol[c]~kg^{-1})),
    ylab = expression(ESP~" (%)")
  ),
  list(
    tag = "c", x = "SO4", y = "EC", colour = "#F04E37",
    xlab = expression(SO[4]^{2-}~(mmol~L^{-1})),
    ylab = expression(EC~(dS~m^{-1}))
  ),
  list(
    tag = "d", x = "HCO3", y = "pH", colour = "#E88B70",
    xlab = expression(HCO[3]^{-}~(meq~L^{-1})),
    ylab = expression(pH)
  ),
  list(
    tag = "e", x = "ESP", y = "MWD", colour = "#345B9D",
    xlab = expression(ESP~" (%)"),
    ylab = expression(MWD~(mm))
  ),
  list(
    tag = "f", x = "ESP", y = "Biomass", colour = "#7387BC",
    xlab = expression(ESP~" (%)"),
    ylab = expression(Biomass~(g~pot^{-1}))
  ),
  list(
    tag = "g", x = "RC", y = "MCF", colour = "#8B70C7",
    xlab = expression(RC~(cmol[c]~kg^{-1})),
    ylab = expression(MCF~(cmol[c]~kg^{-1}))
  ),
  list(
    tag = "h", x = "MCF", y = "MAOC", colour = "#72C6B5",
    xlab = expression(MCF~(cmol[c]~kg^{-1})),
    ylab = expression(MAOC~" (%)")
  )
)

# 5. FIT CORRELATIONS AND BUILD PANELS ------------------------------------------

make_panel <- function(spec, dat) {
  d <- dat %>%
    filter(
      is.finite(.data[[spec$x]]),
      is.finite(.data[[spec$y]])
    )

  n <- nrow(d)
  if (n < 3) {
    stop(
      "Panel (", spec$tag, ") has fewer than 3 complete observations for ",
      spec$x, " vs ", spec$y, "."
    )
  }

  ct <- stats::cor.test(
    d[[spec$x]], d[[spec$y]],
    method = "pearson"
  )

  fit <- stats::lm(
    stats::reformulate(spec$x, response = spec$y),
    data = d
  )

  r_value <- unname(ct$estimate)
  p_value <- ct$p.value
  coefs <- stats::coef(fit)

  stats_row <- tibble::tibble(
    Panel = paste0("(", spec$tag, ")"),
    X_variable = spec$x,
    Y_variable = spec$y,
    n = n,
    Pearson_r = r_value,
    P_value = p_value,
    Intercept = unname(coefs[1]),
    Slope = unname(coefs[2]),
    R_squared = summary(fit)$r.squared
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
      x = -Inf, y = Inf,
      label = stat_label,
      hjust = -0.03,
      vjust = 1.22,
      size = 3.25,
      lineheight = 0.98,
      family = base_family,
      colour = "black"
    ) +
    ggplot2::labs(
      x = spec$xlab,
      y = spec$ylab,
      tag = paste0("(", spec$tag, ")")
    ) +
    ggplot2::theme_classic(
      base_size = 10.5,
      base_family = base_family
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(family = base_family, colour = "black"),
      axis.title = ggplot2::element_text(size = 10.2, colour = "black"),
      axis.text = ggplot2::element_text(size = 9.1, colour = "black"),
      axis.line = ggplot2::element_line(colour = "black", linewidth = 0.55),
      axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.55),
      axis.ticks.length = grid::unit(1.5, "mm"),
      plot.tag = ggplot2::element_text(
        size = 11.2, face = "bold", colour = "black"
      ),
      plot.tag.position = c(0, 1.035),
      plot.margin = ggplot2::margin(t = 12, r = 8, b = 5, l = 8),
      legend.position = "none",
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )

  list(plot = p, stats = stats_row)
}

panel_results <- lapply(panel_specs, make_panel, dat = wide_data)
panel_plots <- lapply(panel_results, function(z) z[["plot"]])
names(panel_plots) <- LETTERS[1:8]

stats_table <- dplyr::bind_rows(
  lapply(panel_results, function(z) z[["stats"]])
)

# Layout matches the supplied image: three panels, three panels, then two
# centered panels. Repeated letters make each panel occupy equal width.
layout_design <- "
AABBCC
DDEEFF
.GGHH.
"

fig_s16 <- patchwork::wrap_plots(
  plotlist = panel_plots,
  design = layout_design
)

print(fig_s16)

# 6. EXPORT FIGURE AND STATISTICS -----------------------------------------------

file_prefix <- paste0("Supplementary_Figure_S16_Bivariate_Correlations_", stamp)

png_file  <- file.path(out_dir, paste0(file_prefix, ".png"))
tiff_file <- file.path(out_dir, paste0(file_prefix, ".tiff"))
pdf_file  <- file.path(out_dir, paste0(file_prefix, ".pdf"))
xlsx_file <- file.path(
  out_dir,
  paste0("Supplementary_Figure_S16_Statistics_", stamp, ".xlsx")
)
csv_file <- file.path(
  out_dir,
  paste0("Supplementary_Figure_S16_Data_Used_", stamp, ".csv")
)

ggplot2::ggsave(
  filename = png_file,
  plot = fig_s16,
  width = fig_width_in,
  height = fig_height_in,
  units = "in",
  dpi = fig_dpi,
  bg = "white",
  limitsize = FALSE
)

ggplot2::ggsave(
  filename = tiff_file,
  plot = fig_s16,
  width = fig_width_in,
  height = fig_height_in,
  units = "in",
  dpi = fig_dpi,
  bg = "white",
  device = "tiff",
  compression = "lzw",
  limitsize = FALSE
)

if (capabilities("cairo")) {
  ggplot2::ggsave(
    filename = pdf_file,
    plot = fig_s16,
    width = fig_width_in,
    height = fig_height_in,
    units = "in",
    bg = "white",
    device = grDevices::cairo_pdf,
    limitsize = FALSE
  )
} else {
  ggplot2::ggsave(
    filename = pdf_file,
    plot = fig_s16,
    width = fig_width_in,
    height = fig_height_in,
    units = "in",
    bg = "white",
    device = grDevices::pdf,
    family = "Helvetica",
    useDingbats = FALSE,
    limitsize = FALSE
  )
}

# Preserve the exact sample-level observations used in the correlations.
utils::write.csv(wide_data, csv_file, row.names = FALSE, na = "")

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Pearson_correlations")
openxlsx::writeData(wb, "Pearson_correlations", stats_table)

openxlsx::addWorksheet(wb, "Data_used")
openxlsx::writeData(wb, "Data_used", wide_data)

openxlsx::addWorksheet(wb, "Variable_mapping")
mapping_table <- raw_long %>%
  distinct(Original_variable, Variable) %>%
  arrange(Variable, Original_variable)
openxlsx::writeData(wb, "Variable_mapping", mapping_table)

openxlsx::saveWorkbook(wb, xlsx_file, overwrite = TRUE)

message("\nSupplementary Fig. 16 export completed.")
message("PNG:  ", normalizePath(png_file, winslash = "/", mustWork = FALSE))
message("TIFF: ", normalizePath(tiff_file, winslash = "/", mustWork = FALSE))
message("PDF:  ", normalizePath(pdf_file, winslash = "/", mustWork = FALSE))
message("Stats:", normalizePath(xlsx_file, winslash = "/", mustWork = FALSE))
message("Data: ", normalizePath(csv_file, winslash = "/", mustWork = FALSE))
message(
  "\nImportant: this script calculates Pearson r and P values from the workbook. ",
  "It has not been executed here because the Excel workbook is not attached to this session."
)
