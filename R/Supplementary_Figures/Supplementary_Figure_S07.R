# ============================================================================
# SUPPLEMENTARY FIG. S7 — SOIL CHEMICAL PROPERTIES: FOUR TREATMENTS
# Nature Communications-inspired formatting, portrait A4 Word manuscript
#
# Display order: CK, GP, P+GP, PEG (same four-column format in both sheets)
# Panels: (a) EC; (b) ESP; (c) SAP; (d) CEC;
#         (e) Sol.Mg; (f) Sol.K; (g) Ex.Mg; (h) Ex.K
# Statistics: mean ± SE; replicate-level points; one-way ANOVA followed by
# Tukey HSD across ALL FOUR treatments; compact letter displays (CLD).
# Source aliases verified: Mg=>Sol_Mg, K=>Sol_K, EMg=>Ex_Mg, EK=>Ex_K.
# NOTE: CLD replaces the old 3-treatment brackets, which would not be valid
# after introduction of a fourth treatment. No data are simulated.
# ============================================================================

rm(list = ls())
graphics.off()

# 1. PACKAGES -----------------------------------------------------------------
pkgs <- c("readxl", "ggplot2", "dplyr", "tidyr", "patchwork",
          "openxlsx", "tibble", "scales", "grid", "multcompView")
missing_pkg <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkg)) install.packages(missing_pkg,
                                          repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(pkgs, library,
                                                character.only = TRUE)))

# 2. FILES, SHEETS, OUTPUTS ----------------------------------------------------
# Run from the repository root (containing README.md, R/ and data/).
file_in <- file.path("data", "Conventional_Treatments.xlsx")
sheet_main <- "data"
sheet_p_gp <- "P+GP"

out_dir <- file.path("outputs", "Supplementary_Fig07_Soil_Chemistry")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Keep the figure at native size when inserting into portrait Word.
WORD_W_CM <- 15.5
WORD_H_CM <- 20.0

# Four treatment colours: original CK, GP, PEG + distinct P+GP violet
trt_levels <- c("CK", "GP", "P+GP", "PEG")
trt_cols <- c(
  "CK"   = "#E64B35",
  "GP"   = "#4DBBD5",
  "PEG"  = "#3C5488",
  "P+GP" = "#8C6BB1"
)

FONT_FAMILY <- "Arial"
FONT_SIZE <- 8.5
BAR_WIDTH <- 0.52
JITTER_WIDTH <- 0.065
ALPHA_SIG <- 0.05

if (!file.exists(file_in)) {
  stop("Missing Excel workbook: ", file_in,
       "\nWorking directory: ", getwd(),
       "\nSet file_in to its full path or move the workbook here.", call. = FALSE)
}
available_sheets <- readxl::excel_sheets(file_in)
if (!all(c(sheet_main, sheet_p_gp) %in% available_sheets)) {
  stop("Required sheet names: 'data' and 'P+GP'. Available sheets: ",
       paste(available_sheets, collapse = ", "), call. = FALSE)
}

# 3. READ IDENTICAL LONG-FORMAT TABLES FROM BOTH SHEETS -----------------------
# Both sheets use the SAME row format (whether or not a header row exists):
#   column 1 = treatment     e.g. CK, GP, PEG or P+GP
#   column 2 = replicate     e.g. 1, 2, 3
#   column 3 = response      e.g. pH, EC, ESP, SAP, STP, CEC, Mg, EMg
#   column 4 = numeric value
#
# Using col_names=FALSE ensures that the first measurement row is never
# mistaken for the column headers, as happened previously (P+GP | 1 | pH).
# If an actual header exists it is skipped by the treatment / numeric checks.
#
# Important distinction: STP (soil total P) is NOT the same as SAP (soil
# available P); we do not silently substitute one for the other.

# Accepted variable labels, grouped by the exact eight panels in Fig. S7.
variable_candidates <- list(
  EC     = c("EC", "E.C", "electrical_conductivity"),
  ESP    = c("ESP", "exchangeable_sodium_percentage"),
  SAP    = c("SAP", "AP", "available_P", "available_phosphorus", "olsen_P"),
  CEC    = c("CEC", "cation_exchange_capacity"),
  # Verified from the uploaded 620-line long-format data table:
  # Mg and K are soluble ions; EMg and EK are exchangeable ions.
  Sol_Mg = c("Mg", "Sol.Mg", "Sol_Mg", "SolMg", "soluble_Mg"),
  Sol_K  = c("K", "Sol.K", "Sol_K", "SolK", "soluble_K"),
  Ex_Mg  = c("EMg", "E_Mg", "Ex.Mg", "Ex_Mg", "ExMg", "exchangeable_Mg"),
  Ex_K   = c("EK", "E_K", "Ex.K", "Ex_K", "ExK", "exchangeable_K")
)

variable_order <- names(variable_candidates)

normalize_var_name <- function(x) {
  x <- trimws(as.character(x))
  x <- tolower(gsub("[^A-Za-z0-9]+", "", x))
  x
}

alias_names <- unlist(lapply(variable_candidates, normalize_var_name),
                      use.names = FALSE)
alias_values <- rep(names(variable_candidates),
                    lengths(variable_candidates))
# "Sol.Mg" and "Sol_Mg" intentionally normalize to the same alias. Remove
# duplicate spellings when they refer to the SAME response variable.
alias_table <- tibble::tibble(alias = alias_names, variable = alias_values) %>%
  dplyr::distinct(alias, variable)
conflicting_aliases <- alias_table %>%
  dplyr::count(alias) %>%
  dplyr::filter(n > 1L)
if (nrow(conflicting_aliases)) {
  stop("Ambiguous variable aliases: ",
       paste(conflicting_aliases$alias, collapse = ", "))
}
variable_lookup <- stats::setNames(alias_table$variable, alias_table$alias)

read_same_format_sheet <- function(sheet_name, wanted_treatments) {
  raw <- readxl::read_excel(
    path = file_in,
    sheet = sheet_name,
    col_names = FALSE,
    col_types = "text"
  )
  if (ncol(raw) < 4L) {
    stop("Sheet '", sheet_name, "' must contain four columns: ",
         "treatment | replicate | variable | value.", call. = FALSE)
  }
  raw <- raw[, 1:4, drop = FALSE]
  names(raw) <- c("treatment", "replicate", "source_variable", "source_value")
  
  d <- raw %>%
    dplyr::mutate(
      source_sheet = sheet_name,
      treatment = toupper(gsub("[[:space:]]+", "", as.character(treatment))),
      replicate = trimws(as.character(replicate)),
      source_variable = trimws(as.character(source_variable)),
      value = suppressWarnings(as.numeric(trimws(as.character(source_value)))),
      variable_key = normalize_var_name(source_variable),
      variable = unname(variable_lookup[variable_key])
    )
  
  # Treatment codes are kept exactly as written. In particular, P+GP is
  # distinct from PEG, and PGP is not silently reinterpreted as PEG.
  
  d <- d %>% dplyr::filter(treatment %in% wanted_treatments)
  if (nrow(d) == 0L) {
    stop("No rows for ", paste(wanted_treatments, collapse = "/"),
         " were found in sheet '", sheet_name, "'.\n",
         "Check whether the first column contains treatment identifiers.",
         call. = FALSE)
  }
  
  invalid <- d %>%
    dplyr::filter(!is.na(variable) &
                    (is.na(replicate) | replicate == "" | !is.finite(value)))
  if (nrow(invalid) > 0) {
    print(invalid %>% dplyr::select(treatment, replicate,
                                    source_variable, source_value), n = Inf)
    stop("Eligible observations in sheet '", sheet_name,
         "' have missing replicate IDs or nonnumeric values.", call. = FALSE)
  }
  
  # Other variables such as pH, STP and Na are allowed in the source workbook,
  # but only the eight variable names above belong to THIS S7 figure.
  eligible <- d %>%
    dplyr::filter(!is.na(variable), is.finite(value)) %>%
    dplyr::select(source_sheet, treatment, replicate, variable, value,
                  source_variable)
  
  if (nrow(eligible) == 0L) {
    stop("Sheet '", sheet_name,
         "' has no recognized S7 response variables. Check column 3.",
         call. = FALSE)
  }
  message("Read ", nrow(eligible), " S7 measurement rows from sheet '",
          sheet_name, "' (", paste(unique(eligible$treatment), collapse = ", "), ").")
  eligible
}

# CK, GP, PEG and P+GP are all read by ONE parser; only sheet selection differs.
raw_main <- read_same_format_sheet(sheet_main, c("CK", "GP", "PEG"))
raw_pgp  <- read_same_format_sheet(sheet_p_gp, "P+GP")

long_source <- dplyr::bind_rows(raw_main, raw_pgp)
long_source$treatment <- factor(long_source$treatment, levels = trt_levels)

# A replicate–variable pair must occur once per treatment. Do not average
# duplicate entries or assign new replicate indices automatically.
duplicates <- long_source %>%
  dplyr::count(treatment, replicate, variable, name = "n_records") %>%
  dplyr::filter(n_records > 1)
if (nrow(duplicates) > 0) {
  print(duplicates, n = Inf)
  stop("Duplicate treatment–replicate–variable records detected. ",
       "Please check the Excel workbook.", call. = FALSE)
}

# Keep replicate labels attached to each observation in the audit table.
# Pivoting is for plotting convenience only; observations are never averaged.
all_data <- long_source %>%
  dplyr::select(treatment, replicate, variable, value) %>%
  tidyr::pivot_wider(
    id_cols = c(treatment, replicate),
    names_from = variable,
    values_from = value
  )

missing_columns <- setdiff(variable_order, names(all_data))
if (length(missing_columns)) {
  stop("Missing S7 variables across the workbook: ",
       paste(missing_columns, collapse = ", "),
       ". Note that STP is NOT SAP.", call. = FALSE)
}

obs_count <- long_source %>%
  dplyr::count(variable, treatment, name = "n") %>%
  tidyr::complete(
    variable = variable_order,
    treatment = factor(trt_levels, levels = trt_levels),
    fill = list(n = 0L)
  )

missing_data <- obs_count %>% dplyr::filter(n == 0)
if (nrow(missing_data)) {
  message("Missing S7 treatment-variable combinations:")
  print(missing_data, n = Inf)
  stop("Some required measurements are missing. ",
       "Check the variable spellings in column 3; STP must not replace SAP.",
       call. = FALSE)
}

# The uploaded table has n = 3 for every treatment-variable combination.
# Warn if the actual Excel workbook differs, without dropping valid values.
nonthree <- obs_count %>% dplyr::filter(n != 3L)
if (nrow(nonthree)) {
  message("NOTICE: these S7 treatment-variable groups do not have three rows:")
  print(nonthree, n = Inf)
  warning("Replicate counts differ from the uploaded example. ",
          "Check workbook rows before submission.", call. = FALSE)
}

message("Original replicate counts (expected 3 for each treatment/variable):")
print(obs_count, n = Inf)

# Audit the actual Excel labels used for the four cation panels.
# Expected entries in the uploaded example: Mg, K, EMg, EK.
source_label_audit <- long_source %>%
  dplyr::distinct(variable, source_variable, source_sheet) %>%
  dplyr::arrange(variable, source_sheet, source_variable)
message("Source-to-figure variable mappings:")
print(source_label_audit, n = Inf)

# Pass only original values to the ANOVA and Tukey calculations.
long_data <- long_source %>%
  dplyr::select(treatment, replicate, variable, value)

# 4. SUMMARY STATISTICS AND FOUR-GROUP TUKEY ----------------------------------
summary_table <- long_data %>%
  dplyr::group_by(variable, treatment, .drop = FALSE) %>%
  dplyr::summarise(
    n = dplyr::n(),
    mean = mean(value),
    sd = if (dplyr::n() > 1) stats::sd(value) else NA_real_,
    se = if (dplyr::n() > 1) stats::sd(value) / sqrt(dplyr::n()) else NA_real_,
    .groups = "drop"
  )

run_tukey <- function(variable_name) {
  x <- long_data %>%
    dplyr::filter(variable == variable_name) %>%
    dplyr::mutate(treatment = droplevels(treatment))
  
  counts <- x %>% dplyr::count(treatment)
  complete_four <- nrow(counts) == 4L && all(counts$n >= 2L)
  
  if (!complete_four) {
    warning("Tukey letters not calculated for ", variable_name,
            ": all four treatments need at least two replicates.")
    return(list(
      letters = tibble::tibble(variable = variable_name,
                               treatment = factor(trt_levels, levels = trt_levels),
                               letter = NA_character_),
      tukey = tibble::tibble(variable = variable_name, comparison = NA_character_,
                             p_adj = NA_real_, significant = NA),
      anova = tibble::tibble(variable = variable_name,
                             p_anova = NA_real_)
    ))
  }
  
  fit <- stats::aov(value ~ treatment, data = x)
  tk <- stats::TukeyHSD(fit, which = "treatment")
  all_pairs <- as.data.frame(tk$treatment)
  
  pairwise <- tibble::tibble(
    variable = variable_name,
    comparison = rownames(all_pairs),
    p_adj = all_pairs[["p adj"]],
    significant = all_pairs[["p adj"]] < ALPHA_SIG
  )
  
  letters_obj <- multcompView::multcompLetters4(fit, tk)
  letters_by_treatment <- letters_obj$treatment$Letters
  letters_df <- tibble::tibble(
    variable = variable_name,
    treatment = factor(names(letters_by_treatment), levels = trt_levels),
    letter = unname(letters_by_treatment)
  )
  
  aov_table <- summary(fit)[[1]]
  anova_df <- tibble::tibble(
    variable = variable_name,
    p_anova = aov_table[["Pr(>F)"]][1]
  )
  
  list(letters = letters_df, tukey = pairwise, anova = anova_df)
}

stat_results <- lapply(variable_order, run_tukey)
letters_table <- dplyr::bind_rows(lapply(stat_results, `[[`, "letters"))
tukey_table   <- dplyr::bind_rows(lapply(stat_results, `[[`, "tukey"))
anova_table   <- dplyr::bind_rows(lapply(stat_results, `[[`, "anova"))

plot_summary <- summary_table %>%
  dplyr::left_join(letters_table, by = c("variable", "treatment"))

# 5. PANEL LABELS --------------------------------------------------------------
variable_labels <- c(
  EC     = "EC (mS cm\u207B\u00B9)",
  ESP    = "ESP (%)",
  SAP    = "SAP (mg kg\u207B\u00B9)",
  CEC    = "CEC (cmolc kg\u207B\u00B9)",
  Sol_Mg = "Sol.Mg (mmol L\u207B\u00B9)",
  Sol_K  = "Sol.K (mmol L\u207B\u00B9)",
  Ex_Mg  = "Ex.Mg (cmolc kg\u207B\u00B9)",
  Ex_K   = "Ex.K (cmolc kg\u207B\u00B9)"
)

panel_tags <- stats::setNames(paste0("(", letters[1:8], ")"), variable_order)

# 6. NATURE-STYLE THEME --------------------------------------------------------
theme_s7 <- function() {
  ggplot2::theme_classic(base_size = FONT_SIZE, base_family = FONT_FAMILY) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black", linewidth = 0.43),
      axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.38),
      axis.ticks.length = grid::unit(1.4, "mm"),
      axis.text.x = ggplot2::element_text(size = 7.4, colour = "black",
                                          margin = ggplot2::margin(t = 2)),
      axis.text.y = ggplot2::element_text(size = 7.5, colour = "black"),
      axis.title.y = ggplot2::element_text(size = 8.1, colour = "black",
                                           margin = ggplot2::margin(r = 4)),
      axis.title.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_blank(),
      legend.position = "none",
      plot.margin = ggplot2::margin(t = 4, r = 3, b = 4, l = 4, unit = "pt")
    )
}

y_breaks <- function(limits) {
  xs <- pretty(c(0, max(limits, na.rm = TRUE)), n = 4)
  sort(unique(c(0, xs[xs >= 0 & xs <= max(limits, na.rm = TRUE)])))
}

make_panel <- function(var) {
  st <- plot_summary %>% dplyr::filter(variable == var) %>%
    dplyr::mutate(x_num = as.numeric(treatment))
  raw <- long_data %>% dplyr::filter(variable == var) %>%
    dplyr::mutate(x_num = as.numeric(treatment))
  
  y_peak <- max(c(st$mean + ifelse(is.na(st$se), 0, st$se), raw$value),
                na.rm = TRUE)
  if (!is.finite(y_peak) || y_peak <= 0) y_peak <- 1
  y_limit <- y_peak * 1.33
  
  st <- st %>% dplyr::mutate(
    label_y = mean + ifelse(is.na(se), 0, se) + 0.078 * y_peak
  )
  
  ggplot2::ggplot() +
    ggplot2::geom_col(
      data = st,
      ggplot2::aes(x = x_num, y = mean, fill = treatment),
      width = BAR_WIDTH, colour = "#242424", linewidth = 0.33
    ) +
    ggplot2::geom_errorbar(
      data = st %>% dplyr::filter(!is.na(se)),
      ggplot2::aes(x = x_num, ymin = pmax(0, mean - se), ymax = mean + se),
      width = 0.11, linewidth = 0.42, colour = "#242424"
    ) +
    ggplot2::geom_point(
      data = raw,
      ggplot2::aes(x = x_num, y = value, colour = treatment),
      position = ggplot2::position_jitter(width = JITTER_WIDTH, height = 0,
                                          seed = 123),
      shape = 21, fill = "white", size = 1.45,
      stroke = 0.42, show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = st %>% dplyr::filter(!is.na(letter)),
      ggplot2::aes(x = x_num, y = label_y, label = letter),
      size = 2.55, family = FONT_FAMILY, colour = "black", vjust = 0
    ) +
    ggplot2::annotate(
      "text", x = 0.61, y = y_limit * 0.985,
      label = panel_tags[[var]], family = FONT_FAMILY,
      fontface = "bold", size = 2.85, hjust = 0, vjust = 1
    ) +
    ggplot2::scale_fill_manual(values = trt_cols, drop = FALSE, guide = "none") +
    ggplot2::scale_colour_manual(values = trt_cols, drop = FALSE, guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = 1:4, labels = trt_levels,
      limits = c(0.55, 4.45), expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = y_breaks,
      limits = c(0, y_limit),
      expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::labs(y = variable_labels[[var]], x = NULL) +
    theme_s7()
}

# 7. ASSEMBLE: TWO COLUMNS × FOUR ROWS -----------------------------------------
plot_list <- lapply(variable_order, make_panel)
figS7 <- patchwork::wrap_plots(plot_list, ncol = 2, byrow = TRUE) +
  patchwork::plot_layout(heights = rep(1, 4)) &
  ggplot2::theme(plot.background = ggplot2::element_rect(
    fill = "white", colour = NA))

print(figS7)

# 8. EXPORT -------------------------------------------------------------------
png_out <- file.path(out_dir, "Fig_S7_Word_Portrait_600dpi.png")
tif_out <- file.path(out_dir, "Fig_S7_Word_Portrait_600dpi.tiff")
pdf_out <- file.path(out_dir, "Fig_S7_NC_Vector.pdf")

ggplot2::ggsave(png_out, figS7, width = WORD_W_CM, height = WORD_H_CM,
                units = "cm", dpi = 600, bg = "white")
ggplot2::ggsave(tif_out, figS7, width = WORD_W_CM, height = WORD_H_CM,
                units = "cm", dpi = 600, compression = "lzw", bg = "white")
ggplot2::ggsave(pdf_out, figS7, width = WORD_W_CM, height = WORD_H_CM,
                units = "cm", device = if (capabilities("cairo")) cairo_pdf else "pdf",
                bg = "white")

# 9. EXPORT AUDITABLE STATISTICAL TABLES ---------------------------------------
xlsx_out <- file.path(out_dir, "Fig_S7_4Treatments_Statistics.xlsx")
openxlsx::write.xlsx(
  list(
    "Raw_all_4_treatments" = all_data,
    "Source_rows_both_sheets" = long_source,
    "Replicate_counts" = obs_count,
    "Summary_mean_SD_SE" = summary_table,
    "Four_group_ANOVA" = anova_table,
    "Four_group_Tukey" = tukey_table,
    "Tukey_letters" = letters_table,
    "Figure_summary" = plot_summary,
    "Detected_variable_names" = source_label_audit,
    "Source_columns" = tibble::tibble(
      Variable = names(variable_candidates),
      Candidate_names = vapply(variable_candidates, paste, collapse = "; ",
                               FUN.VALUE = character(1))
    )
  ),
  file = xlsx_out,
  overwrite = TRUE
)

message("Supplementary Fig. S7 saved to: ", normalizePath(out_dir))
message("Word PNG: ", png_out)
message("Word TIFF: ", tif_out)
message("NC vector PDF: ", pdf_out)
message("Stats workbook: ", xlsx_out)

figS7