# =============================================================================
# SUPPLEMENTARY FIG. S10 | GERMINATION AND PLANT GROWTH
# Four treatments: CK, GP, P+GP, PEG (IN THIS ORDER)
# Portrait figure: 2 columns x 3 rows, 15.5 x 18.7 cm
# DISTINCT STYLE FROM S7: horizontal lollipop (germination) and
# horizontal point-range growth charts (mean +/- SE and raw replicates)
#
# Panels:
# (a) Germination (%)                 (b) Plant height (cm)
# (c) Root diameter (mm)             (d) Stem diameter (mm)
# (e) Stem dry weight (mg)           (f) Root dry weight (mg)
#
# Germination percentages: CK=18, GP=69, P+GP=76, PEG=88.
# The three percentages in the original S10 code were CK/GP/PEG;
# P+GP=76% was explicitly confirmed by the user.
# There are no replicate germination observations supplied, so panel (a)
# has no invented SE bars or significance letters.
#
# Data: Conventional_Treatments.xlsx, sheet "data" (preferred)
# Four columns: Treatments | Replicates | response.varibale | Value
# Accepts a header or a headerless first row.
# If P+GP growth rows are absent from "data", checks "P+GP" sheet.
# The original long-format growth data stores Stem_DW and Root_DW in g;
# the script multiplies by 1000 for the plot labels in mg.
#
# Growth tests: one-way ANOVA, Tukey HSD, compact letter display, alpha=.05.
# Means +/- SE, individual replicates; exact statistics exported to Excel.
# PNG/TIFF at 600 dpi and PDF (vector).
# =============================================================================

rm(list = ls())
graphics.off()

# 1. PACKAGES ----------------------------------------------------------------
pkgs <- c("readxl", "dplyr", "tidyr", "tibble", "ggplot2", "patchwork",
          "multcompView", "openxlsx", "scales", "grid")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages(invisible(lapply(pkgs, library,
                                                character.only = TRUE)))

# 2. SETTINGS ----------------------------------------------------------------
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

file_in <- repo_input_path("Conventional_Treatments.xlsx")
main_sheet <- "data"
optional_pgp_sheet <- "P+GP"   # used only if P+GP growth is absent from data
out_dir <- "Fig_S10_NC_Horizontal_PointRange"

# The order requested for all panels.
trt_levels <- c("CK", "GP", "P+GP", "PEG")
trt_cols <- c(CK = "#E64B35", GP = "#4DBBD5",
              "P+GP" = "#8C6BB1", PEG = "#3C5488")

# From original Fig. S10 plus user's confirmed P+GP value.
germ_pct <- c(CK = 18, GP = 69, "P+GP" = 76, PEG = 88)

# As displayed in the uploaded long-format measurements: values such as
# Stem_DW=1.4256 and Root_DW=0.3312 are grams; axes must show milligrams.
DRY_WEIGHT_INPUT_UNIT <- "g"   # set to "mg" ONLY if Excel source uses mg

FIG_W_CM <- 15.5
FIG_H_CM <- 18.7
FIG_DPI <- 600
BASE_FONT <- "Arial"
ALPHA <- 0.05

if (!file.exists(file_in)) {
  stop("File not found: ", file_in, "\nR working directory: ", getwd(),
       "\nPlace the Excel file here, or set file_in to its full path.",
       call. = FALSE)
}
if (!DRY_WEIGHT_INPUT_UNIT %in% c("g", "mg")) {
  stop("DRY_WEIGHT_INPUT_UNIT must be 'g' or 'mg'.", call. = FALSE)
}
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# 3. READ DATA IN THE EXACT LONG FORMAT --------------------------------------
clean_key <- function(x) {
  tolower(gsub("[^A-Za-z0-9]", "", trimws(as.character(x))))
}

read_four_column_data <- function(file, sheet) {
  first_try <- readxl::read_excel(file, sheet = sheet, col_names = TRUE,
                                  .name_repair = "unique")
  hdr <- clean_key(names(first_try))
  get_pos <- function(candidates) {
    i <- which(hdr %in% candidates)
    if (!length(i)) NA_integer_ else i[1]
  }
  p_trt <- get_pos(c("treatments", "treatment", "trt"))
  p_rep <- get_pos(c("replicates", "replicate", "rep"))
  p_var <- get_pos(c("responsevaribale", "responsevariable", "responsevarible",
                     "variable", "trait", "parameter"))
  p_val <- get_pos(c("value", "values", "measurement"))
  has_header <- all(!is.na(c(p_trt, p_rep, p_var, p_val)))
  
  if (has_header) {
    message("Reading sheet '", sheet, "' using its four-column header.")
    out <- tibble::tibble(
      treatment = first_try[[p_trt]],
      replicate = first_try[[p_rep]],
      source_var = first_try[[p_var]],
      value = first_try[[p_val]]
    )
  } else {
    message("Sheet '", sheet, "' has no recognized header; reading first 4 columns",
            " without skipping the first data row.")
    raw <- readxl::read_excel(file, sheet = sheet, col_names = FALSE,
                              col_types = "text")
    if (ncol(raw) < 4) {
      stop("Sheet '", sheet, "' has fewer than four columns.", call. = FALSE)
    }
    out <- tibble::tibble(
      treatment = raw[[1]], replicate = raw[[2]],
      source_var = raw[[3]], value = raw[[4]]
    )
  }
  out %>%
    dplyr::mutate(
      treatment = toupper(gsub("[[:space:]]+", "", as.character(treatment))),
      replicate = trimws(as.character(replicate)),
      source_var = trimws(as.character(source_var)),
      variable = clean_key(source_var),
      value = suppressWarnings(as.numeric(as.character(value))),
      source_sheet = sheet
    ) %>%
    dplyr::filter(treatment %in% trt_levels,
                  !is.na(replicate), replicate != "",
                  !is.na(value), is.finite(value)) %>%
    dplyr::select(source_sheet, treatment, replicate, source_var, variable, value)
}

available_sheets <- readxl::excel_sheets(file_in)
if (!main_sheet %in% available_sheets) {
  stop("Sheet '", main_sheet, "' was not found in ", file_in,
       "\nAvailable sheets: ", paste(available_sheets, collapse = ", "),
       call. = FALSE)
}
raw_data <- read_four_column_data(file_in, main_sheet)

# Excel names in the source data, not assumed column headers:
# Height, Root_dia, Stem_dia, Stem_DW, Root_DW.
# The key normalizer removes punctuation and capitalization differences.
name_map <- c(
  height = "Height", plantheight = "Height",
  rootdia = "RootDia", rootdiameter = "RootDia",
  stemdia = "StemDia", stemdiameter = "StemDia",
  stemdw = "StemDW", stemdryweight = "StemDW",
  rootdw = "RootDW", rootdryweight = "RootDW"
)

map_variables <- function(d) {
  d %>% dplyr::mutate(variable = unname(name_map[variable])) %>%
    dplyr::filter(!is.na(variable))
}

growth <- map_variables(raw_data)
needed <- c("Height", "RootDia", "StemDia", "StemDW", "RootDW")

# If the data sheet has no P+GP growth rows, also read its optional sheet.
pgp_missing <- setdiff(needed, unique(growth$variable[growth$treatment == "P+GP"]))
if (length(pgp_missing) && optional_pgp_sheet %in% available_sheets) {
  message("P+GP growth missing in 'data' for: ", paste(pgp_missing, collapse = ", "),
          ". Checking sheet '", optional_pgp_sheet, "'.")
  pgp_extra <- map_variables(read_four_column_data(file_in, optional_pgp_sheet)) %>%
    dplyr::filter(treatment == "P+GP", variable %in% pgp_missing)
  growth <- dplyr::bind_rows(growth, pgp_extra)
}

# 4. QUALITY CHECKS AND UNIT CONVERSION --------------------------------------
# Replicates must remain independent, never silently averaged or duplicated.
dup <- growth %>%
  dplyr::count(treatment, replicate, variable, name = "records") %>%
  dplyr::filter(records > 1L)
if (nrow(dup)) {
  print(dup, n = Inf)
  stop("Duplicate treatment/replicate/trait records were found. ",
       "Resolve duplicates in Excel before ANOVA.", call. = FALSE)
}

mass_conversion <- if (DRY_WEIGHT_INPUT_UNIT == "g") 1000 else 1
growth <- growth %>%
  dplyr::mutate(
    original_value = value,
    unit_used = dplyr::if_else(variable %in% c("StemDW", "RootDW"),
                               "mg", dplyr::case_when(
                                 variable == "Height" ~ "cm",
                                 TRUE ~ "mm")),
    value = dplyr::if_else(variable %in% c("StemDW", "RootDW"),
                           value * mass_conversion, value),
    treatment = factor(treatment, levels = trt_levels)
  )

counts <- growth %>%
  dplyr::count(variable, treatment, name = "n") %>%
  tidyr::complete(variable = needed,
                  treatment = factor(trt_levels, levels = trt_levels),
                  fill = list(n = 0L)) %>%
  dplyr::arrange(match(variable, needed), treatment)
message("Growth records per treatment and trait:")
print(counts, n = Inf)

if (any(counts$n < 2L)) {
  print(counts %>% dplyr::filter(n < 2), n = Inf)
  stop("Insufficient growth replicates for four-treatment ANOVA. ",
       "Each of the 5 traits needs at least 2 replicates in CK, GP, P+GP, PEG.",
       call. = FALSE)
}
if (any(counts$n != 3L)) {
  warning("A treatment/trait does not contain exactly three replicates. ",
          "Check the exported counts.")
}

# 5. ONE-WAY ANOVA + TUKEY HSD + GROUPING LETTERS ----------------------------
stats_for_trait <- function(var) {
  dat <- growth %>% dplyr::filter(variable == var) %>%
    dplyr::mutate(treatment = factor(treatment, levels = trt_levels))
  
  smry <- dat %>%
    dplyr::group_by(treatment, .drop = FALSE) %>%
    dplyr::summarise(n = dplyr::n(), Mean = mean(value),
                     SD = stats::sd(value), SE = SD / sqrt(n),
                     .groups = "drop") %>%
    dplyr::mutate(variable = var)
  
  fit <- stats::aov(value ~ treatment, data = dat)
  an <- as.data.frame(summary(fit)[[1]])
  an$term <- rownames(an)
  rownames(an) <- NULL
  an$variable <- var
  
  tk <- as.data.frame(stats::TukeyHSD(fit, "treatment")$treatment)
  tk$comparison <- rownames(tk)
  rownames(tk) <- NULL
  tk$variable <- var
  p_adj <- stats::setNames(tk$`p adj`, tk$comparison)
  if (any(!is.finite(p_adj))) {
    stop("Undefined Tukey P-values for ", var,
         "; verify variability and replicates.", call. = FALSE)
  }
  letter_vec <- multcompView::multcompLetters2(
    value ~ treatment, p_adj, data = dat)$Letters
  letters <- tibble::tibble(
    treatment = factor(names(letter_vec), levels = trt_levels),
    letter = unname(letter_vec))
  smry <- smry %>% dplyr::left_join(letters, by = "treatment")
  
  list(observations = dat, summary = smry,
       anova = an, tukey = tk, letters = letters)
}
res <- stats::setNames(lapply(needed, stats_for_trait), needed)

# 6. GERMINATION: OBSERVED PERCENTAGES, NO REPLICATE STATISTICS ---------------
germ <- tibble::tibble(
  treatment = factor(trt_levels, levels = trt_levels),
  percentage = unname(germ_pct[trt_levels]),
  source = c("Original S10", "Original S10", "User-confirmed: 76%", "Original S10")
)

# 7. DISTINCT S10 STYLE: HORIZONTAL LOLLIPOPS + POINT-RANGE ------------------
# Unlike Fig. S7 (vertical bars), S10 shows each treatment on its own row:
#   (a) Germination: grey 0-100% guide, coloured 0-to-observed-percent stem,
#       filled point, and the observed percentage at its end.
#   (b-f) Growth: faint baseline, measured replicate circles, a coloured
#       diamond at the mean, and horizontal mean +/- SE whiskers.
# Treatments read vertically from CK at the top to PEG at the bottom.
# Tukey letters use the same one-way ANOVA and post-hoc calculations above.

trt_y <- stats::setNames(rev(seq_along(trt_levels)), trt_levels)
ROW_GAP <- 0.34
PANEL_Y_LIMITS <- c(0.50, 4.66)

make_panel_theme <- function() {
  ggplot2::theme_classic(base_size = 9, base_family = BASE_FONT) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.position = "none",
      axis.line.x = ggplot2::element_line(linewidth = 0.40, colour = "#555555"),
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_line(linewidth = 0.36, colour = "#555555"),
      axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.length = grid::unit(2.0, "pt"),
      axis.text.x = ggplot2::element_text(size = 8.3, colour = "#333333"),
      axis.text.y = ggplot2::element_text(size = 8.5, colour = "#252525"),
      axis.title.x = ggplot2::element_text(
        size = 9.0, colour = "black", margin = ggplot2::margin(t = 6)),
      axis.title.y = ggplot2::element_blank(),
      plot.tag = ggplot2::element_text(size = 11, face = "bold",
                                       colour = "black"),
      plot.tag.position = c(0.018, 0.990),
      plot.margin = ggplot2::margin(t = 13, r = 9, b = 7, l = 8)
    )
}

make_germination_plot <- function() {
  dat <- germ %>%
    dplyr::mutate(row = unname(trt_y[as.character(treatment)]),
                  label = paste0(percentage, "%"))
  
  ggplot2::ggplot(dat, ggplot2::aes(y = row)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = 100, yend = row),
      linewidth = 2.5, colour = "#E8EAED", lineend = "round") +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = percentage, yend = row,
                   colour = treatment),
      linewidth = 2.6, lineend = "round") +
    ggplot2::geom_point(
      ggplot2::aes(x = percentage, colour = treatment),
      size = 3.5, shape = 16) +
    ggplot2::geom_text(
      ggplot2::aes(x = percentage + 4, label = label),
      hjust = 0, size = 3.15, colour = "#252525") +
    ggplot2::scale_colour_manual(values = trt_cols, drop = FALSE) +
    ggplot2::scale_y_continuous(
      limits = PANEL_Y_LIMITS, breaks = unname(trt_y),
      labels = names(trt_y), expand = ggplot2::expansion(mult = 0)) +
    ggplot2::scale_x_continuous(
      limits = c(0, 119), breaks = seq(0, 100, 25),
      expand = ggplot2::expansion(mult = 0)) +
    ggplot2::labs(x = "Germination (%)", tag = "(a)") +
    make_panel_theme()
}

make_growth_plot <- function(var, x_label, panel_tag) {
  st <- res[[var]]$summary %>%
    dplyr::mutate(row = unname(trt_y[as.character(treatment)]))
  d <- res[[var]]$observations %>%
    dplyr::mutate(row = unname(trt_y[as.character(treatment)])) %>%
    dplyr::group_by(treatment) %>%
    dplyr::arrange(replicate, .by_group = TRUE) %>%
    dplyr::mutate(
      # Small vertical offsets expose the three true replicate values.
      raw_y = row + seq(-0.16, 0.16, length.out = dplyr::n())) %>%
    dplyr::ungroup()
  
  peak <- max(c(d$value, st$Mean + st$SE), na.rm = TRUE)
  if (!is.finite(peak) || peak <= 0) {
    stop("Invalid growth values for trait: ", var, call. = FALSE)
  }
  # One consistent location for grouping letters makes them easy to read.
  st <- st %>% dplyr::mutate(letter_x = peak * 1.17)
  plot_end <- peak * 1.32
  
  ggplot2::ggplot() +
    # Thin baseline instead of bars. No values are stacked.
    ggplot2::geom_segment(
      data = st, ggplot2::aes(x = 0, xend = Mean,
                              y = row, yend = row),
      colour = "#D9DDE2", linewidth = 1.05) +
    # Raw observations as unfilled circles (three original replicates).
    ggplot2::geom_point(
      data = d, ggplot2::aes(x = value, y = raw_y,
                             colour = treatment),
      shape = 21, fill = "white", stroke = 0.48, size = 1.85,
      alpha = 0.96) +
    # Horizontal SE ranges; means are shown as coloured diamonds.
    ggplot2::geom_segment(
      data = st,
      ggplot2::aes(x = pmax(0, Mean - SE), xend = Mean + SE,
                   y = row, yend = row, colour = treatment),
      linewidth = 0.95) +
    ggplot2::geom_segment(
      data = st,
      ggplot2::aes(x = pmax(0, Mean - SE), xend = pmax(0, Mean - SE),
                   y = row - 0.11, yend = row + 0.11, colour = treatment),
      linewidth = 0.70) +
    ggplot2::geom_segment(
      data = st,
      ggplot2::aes(x = Mean + SE, xend = Mean + SE,
                   y = row - 0.11, yend = row + 0.11, colour = treatment),
      linewidth = 0.70) +
    ggplot2::geom_point(
      data = st,
      ggplot2::aes(x = Mean, y = row, fill = treatment),
      shape = 23, size = 3.25, colour = "#252525", stroke = 0.38) +
    ggplot2::geom_text(
      data = st,
      ggplot2::aes(x = letter_x, y = row, label = letter),
      size = 3.6, colour = "black", fontface = "plain") +
    ggplot2::scale_colour_manual(values = trt_cols, drop = FALSE) +
    ggplot2::scale_fill_manual(values = trt_cols, drop = FALSE) +
    ggplot2::scale_y_continuous(
      limits = PANEL_Y_LIMITS, breaks = unname(trt_y),
      labels = names(trt_y), expand = ggplot2::expansion(mult = 0)) +
    ggplot2::scale_x_continuous(
      limits = c(0, plot_end),
      breaks = scales::breaks_pretty(n = 4),
      expand = ggplot2::expansion(mult = 0)) +
    ggplot2::labs(x = x_label, tag = panel_tag) +
    make_panel_theme()
}

pA <- make_germination_plot()
pB <- make_growth_plot("Height",  "Plant height (cm)", "(b)")
pC <- make_growth_plot("RootDia", "Root diameter (mm)", "(c)")
pD <- make_growth_plot("StemDia", "Stem diameter (mm)", "(d)")
pE <- make_growth_plot("StemDW",  "Stem dry weight (mg)", "(e)")
pF <- make_growth_plot("RootDW",  "Root dry weight (mg)", "(f)")

# Word portrait; 2 columns x 3 rows. All panels read left-to-right, top-to-bottom.
figS10 <- ((pA | pB) / (pC | pD) / (pE | pF)) +
  patchwork::plot_layout(heights = c(1, 1, 1)) &
  ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white",
                                                         colour = NA))
print(figS10)

# 8. HIGH-RESOLUTION EXPORT -------------------------------------------------
base_path <- file.path(out_dir, "Fig_S10_CK_GP_PplusGP_PEG_Horizontal")

save_figure <- function(ext, device = NULL) {
  f <- paste0(base_path, ".", ext)
  opts <- list(filename = f, plot = figS10,
               width = FIG_W_CM, height = FIG_H_CM,
               units = "cm", bg = "white")
  if (ext %in% c("png", "tiff")) opts$dpi <- FIG_DPI
  if (ext == "tiff") opts$compression <- "lzw"
  if (!is.null(device)) opts$device <- device
  do.call(ggplot2::ggsave, opts)
  message("Saved: ", normalizePath(f, mustWork = FALSE))
}
save_figure("png")
save_figure("tiff")
if (capabilities("cairo")) {
  save_figure("pdf", device = grDevices::cairo_pdf)
} else {
  warning("Cairo not available. Using standard vector PDF; font embedding may vary.")
  save_figure("pdf", device = "pdf")
}

# 9. DATA AND STATISTICAL AUDIT ----------------------------------------------
stats_tabs <- list(
  Germination = germ,
  Growth_input_and_units = growth,
  Growth_replicate_counts = counts
)
for (v in needed) {
  stats_tabs[[paste0(v, "_summary")]] <- res[[v]]$summary
  stats_tabs[[paste0(v, "_ANOVA")]] <- res[[v]]$anova
  stats_tabs[[paste0(v, "_Tukey")]] <- res[[v]]$tukey
  stats_tabs[[paste0(v, "_letters")]] <- res[[v]]$letters
}

stats_file <- paste0(base_path, "_statistics.xlsx")
wb <- openxlsx::createWorkbook()
for (nm in names(stats_tabs)) {
  openxlsx::addWorksheet(wb, nm)
  openxlsx::writeData(wb, nm, stats_tabs[[nm]])
}
openxlsx::saveWorkbook(wb, stats_file, overwrite = TRUE)
message("Statistics saved: ", normalizePath(stats_file, mustWork = FALSE))
message("Finished distinct-style Fig. S10 — germination: CK 18%, GP 69%, P+GP 76%, PEG 88%.")