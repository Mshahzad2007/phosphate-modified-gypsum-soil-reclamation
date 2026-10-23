rm(list = ls())
graphics.off()
cat("\014")

# ============================================================================
# Supplementary Fig. S5 — Control experiments separating phosphate supply
# from acidification effects on soil available phosphorus (SAP)
# Nature Communications style, portrait Word-friendly layout
#
# Preserved from the user's workflow:
# - Same source workbook and sheet names
# - Same summary statistics (mean, SD, SE)
# - Same statistical lettering logic:
#     * pH panel: global Tukey letters across all HCl groups
#     * SAP-HCl panel: Ca0 vs CaHigh within each HCl level
#     * SAP-P panel:  Ca0 vs CaHigh within each P level
# - Same Ca0 / CaHigh colours
#
# Main formatting changes:
# - Arial font, clean white background
# - Internal bold lowercase panel tags: (a), (b), (c)
# - Shared legend at the top
# - Two panels across the top, panel (c) spans the full bottom row
# - Export sizes suitable for portrait Word placement and journal vector output
# ============================================================================

# 1) PACKAGES -----------------------------------------------------------------
pkgs <- c(
  "readxl", "dplyr", "stringr", "tidyr", "ggplot2",
  "multcompView", "patchwork", "scales", "grid", "openxlsx"
)

to_install <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

suppressPackageStartupMessages({
  invisible(lapply(pkgs, library, character.only = TRUE))
})

# 2) INPUT / OUTPUT ------------------------------------------------------------
file_path <- "Phosphate_Primed_Gypsum.xlsx"
sheet_hcl <- "pH_Hcl"
sheet_p   <- "data"

out_dir <- "FigS5_NC_Portrait_v2"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Word portrait figure (insert at its native 15.5 cm width)
word_width_cm  <- 15.5
word_height_cm <- 14.6
journal_width_cm  <- 18.0
journal_height_cm <- word_height_cm * journal_width_cm / word_width_cm

# 3) GLOBAL STYLE --------------------------------------------------------------
FONT_FAMILY <- "Arial"
BASE_SIZE   <- 8.8

col_ca <- c(
  "Ca0"    = "#E64B35",
  "CaHigh" = "#3C5488"
)

shape_ca <- c("Ca0" = 16, "CaHigh" = 15)  # circle, square
line_ca  <- c("Ca0" = "solid", "CaHigh" = "22")

# Small x-nudges used only for statistical letters (not for points)
HCL_LABEL_NUDGE <- c("Ca0" = -4, "CaHigh" = 4)
P_LABEL_NUDGE   <- c("Ca0" = -0.10, "CaHigh" = 0.10)

nc_theme <- function(base_size = BASE_SIZE) {
  theme_classic(base_size = base_size, base_family = FONT_FAMILY) +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid       = element_blank(),
      panel.border     = element_blank(),
      axis.line        = element_line(linewidth = 0.46, colour = "black"),
      axis.ticks       = element_line(linewidth = 0.39, colour = "black"),
      axis.ticks.length = unit(1.6, "mm"),
      axis.title.x     = element_text(size = 9.1, colour = "black",
                                      margin = margin(t = 4)),
      axis.title.y     = element_text(size = 9.1, colour = "black",
                                      margin = margin(r = 5)),
      axis.text.x      = element_text(size = 8.4, colour = "black",
                                      margin = margin(t = 2)),
      axis.text.y      = element_text(size = 8.3, colour = "black"),
      legend.title     = element_blank(),
      legend.text      = element_text(size = 8.2, colour = "black"),
      legend.key       = element_blank(),
      legend.key.size  = unit(3.5, "mm"),
      legend.spacing.x = unit(2.0, "mm"),
      legend.margin    = margin(0, 0, 0, 0),
      plot.tag         = element_text(size = 10.2, face = "bold",
                                      colour = "black"),
      plot.tag.position = c(0.01, 0.99),
      plot.margin      = margin(t = 4, r = 5, b = 4, l = 5, unit = "pt")
    )
}

# 4) HELPER FUNCTIONS ----------------------------------------------------------
add_label_y <- function(plot_df, pad = 0.045, stagger = 0.055) {
  y_max <- max(plot_df$Mean + plot_df$SE, na.rm = TRUE)
  y_min <- min(plot_df$Mean - plot_df$SE, na.rm = TRUE)
  y_rng <- y_max - y_min
  if (!is.finite(y_rng) || y_rng <= 0) y_rng <- 1
  
  plot_df %>%
    mutate(
      lab_y = Mean + SE + pad * y_rng +
        if_else(CaLevel == "CaHigh", stagger * y_rng, 0)
    )
}

get_axis_limits <- function(plot_df, hard_zero = FALSE,
                            lower_pad = 0.08, upper_pad = 0.08) {
  y_min <- min(plot_df$Mean - plot_df$SE, na.rm = TRUE)
  y_max <- max(plot_df$lab_y, plot_df$Mean + plot_df$SE, na.rm = TRUE)
  y_rng <- y_max - y_min
  if (!is.finite(y_rng) || y_rng <= 0) y_rng <- 1
  
  lower <- if (hard_zero) 0 else y_min - lower_pad * y_rng
  upper <- y_max + upper_pad * y_rng
  c(lower, upper)
}

within_level_letters <- function(dsub, level_col) {
  levs <- levels(dsub[[level_col]])
  
  out <- expand.grid(
    Level = levs,
    CaLevel = c("Ca0", "CaHigh"),
    stringsAsFactors = FALSE
  ) %>%
    rename(!!level_col := Level) %>%
    mutate(CLD = "a")
  
  for (lv in levs) {
    tmp <- dsub %>% filter(.data[[level_col]] == lv)
    if (dplyr::n_distinct(tmp$CaLevel) < 2) next
    if (nrow(tmp) < 3) next
    
    tt <- tryCatch(t.test(Value ~ CaLevel, data = tmp), error = function(e) NULL)
    if (is.null(tt)) next
    
    if (!is.na(tt$p.value) && tt$p.value < 0.05) {
      m <- tmp %>% group_by(CaLevel) %>% summarise(mu = mean(Value, na.rm = TRUE), .groups = "drop")
      hi <- as.character(m$CaLevel[which.max(m$mu)])
      lo <- as.character(m$CaLevel[which.min(m$mu)])
      
      out <- out %>%
        mutate(CLD = case_when(
          .data[[level_col]] == lv & CaLevel == hi ~ "a",
          .data[[level_col]] == lv & CaLevel == lo ~ "b",
          TRUE ~ CLD
        ))
    }
  }
  out
}

legend_guide <- guide_legend(
  nrow = 1, byrow = TRUE,
  override.aes = list(
    linewidth = 0.70,
    shape = unname(shape_ca[c("Ca0", "CaHigh")]),
    linetype = unname(line_ca[c("Ca0", "CaHigh")]),
    size = 2.6
  )
)

# 5) READ HCl DATA -------------------------------------------------------------
if (!file.exists(file_path)) {
  stop("File not found: ", file_path,
       "\nWorking directory: ", getwd(), call. = FALSE)
}

dat_hcl <- readxl::read_excel(file_path, sheet = sheet_hcl) %>%
  mutate(
    Variable  = stringr::str_trim(as.character(Variable)),
    Treatment = stringr::str_trim(as.character(Treatment)),
    CaLevel   = stringr::str_trim(as.character(CaLevel)),
    CaLevel   = factor(CaLevel, levels = c("Ca0", "CaHigh")),
    HCl_uL    = as.numeric(HCl_uL),
    HCl_f     = factor(HCl_uL, levels = sort(unique(HCl_uL))),
    Replicate = factor(Replicate),
    Value     = as.numeric(Value)
  ) %>%
  filter(
    !is.na(Variable), !is.na(CaLevel), !is.na(HCl_uL), !is.na(Value),
    Variable %in% c("pH", "SAP")
  )

summaries_hcl <- dat_hcl %>%
  group_by(Variable, CaLevel, HCl_f, HCl_uL) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD   = sd(Value, na.rm = TRUE),
    N    = dplyr::n(),
    SE   = SD / sqrt(N),
    .groups = "drop"
  ) %>%
  mutate(SE = ifelse(is.na(SE), 0, SE))

# 6) pH LETTERS: GLOBAL TUKEY --------------------------------------------------
dat_pH <- dat_hcl %>%
  filter(Variable == "pH") %>%
  mutate(
    Group = factor(
      paste0(CaLevel, "_HCl", HCl_f),
      levels = as.vector(t(outer(
        c("Ca0", "CaHigh"),
        paste0("HCl", levels(HCl_f)),
        paste, sep = "_"
      )))
    )
  )

fit_pH <- aov(Value ~ Group, data = dat_pH)
tk_pH  <- TukeyHSD(fit_pH)
cld_pH_list <- multcompView::multcompLetters4(fit_pH, tk_pH)

cld_pH <- data.frame(
  Group = names(cld_pH_list$Group$Letters),
  CLD   = cld_pH_list$Group$Letters,
  stringsAsFactors = FALSE
) %>%
  mutate(
    CaLevel = sub("_HCl.*", "", Group),
    HCl_f   = sub(".*_HCl", "", Group),
    CaLevel = factor(CaLevel, levels = c("Ca0", "CaHigh")),
    HCl_f   = factor(HCl_f, levels = levels(dat_hcl$HCl_f))
  )

plot_pH <- summaries_hcl %>%
  filter(Variable == "pH") %>%
  left_join(cld_pH %>% select(CaLevel, HCl_f, CLD), by = c("CaLevel", "HCl_f")) %>%
  mutate(
    HCl_f = factor(HCl_f, levels = levels(dat_hcl$HCl_f)),
    x_lab = HCl_uL + unname(HCL_LABEL_NUDGE[as.character(CaLevel)])
  ) %>%
  add_label_y(pad = 0.040, stagger = 0.055)

# 7) SAP ACROSS HCl ------------------------------------------------------------
dat_SAP_hcl <- dat_hcl %>% filter(Variable == "SAP")

sap_hcl_cld <- within_level_letters(dat_SAP_hcl, level_col = "HCl_f") %>%
  mutate(
    HCl_f = factor(HCl_f, levels = levels(dat_hcl$HCl_f)),
    CaLevel = factor(CaLevel, levels = c("Ca0", "CaHigh"))
  )

plot_SAP_hcl <- summaries_hcl %>%
  filter(Variable == "SAP") %>%
  left_join(sap_hcl_cld, by = c("HCl_f", "CaLevel")) %>%
  mutate(
    HCl_f = factor(HCl_f, levels = levels(dat_hcl$HCl_f)),
    x_lab = HCl_uL + unname(HCL_LABEL_NUDGE[as.character(CaLevel)])
  ) %>%
  add_label_y(pad = 0.050, stagger = 0.060)

# 8) READ P-DOSE DATA ----------------------------------------------------------
dat_p <- readxl::read_excel(file_path, sheet = sheet_p) %>%
  mutate(
    Treatment = stringr::str_trim(as.character(Treatment)),
    CaLevel   = stringr::str_trim(as.character(CaLevel)),
    CaLevel   = factor(CaLevel, levels = c("Ca0", "CaHigh")),
    PLevel    = stringr::str_trim(as.character(PLevel)),
    Replicate = factor(Replicate),
    Variable  = stringr::str_trim(as.character(Variable)),
    Value     = as.numeric(Value)
  ) %>%
  filter(
    !is.na(Value), !is.na(Variable), !is.na(CaLevel), !is.na(PLevel),
    Variable == "SAP"
  )

p_label_map <- c(
  "0"    = "P0",
  "Low"  = "PLow",
  "Med"  = "PMed",
  "High" = "PHigh",
  "Ctrl" = "pH control"
)

df_p_main <- dat_p %>%
  filter(PLevel %in% c("0", "Low", "Med", "High")) %>%
  mutate(
    P_show = p_label_map[PLevel],
    P_show = factor(P_show, levels = c("P0", "PLow", "PMed", "PHigh"))
  )

sum_p_main <- df_p_main %>%
  group_by(P_show, CaLevel) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD   = sd(Value, na.rm = TRUE),
    N    = dplyr::n(),
    SE   = SD / sqrt(N),
    .groups = "drop"
  ) %>%
  mutate(SE = ifelse(is.na(SE), 0, SE))

cld_p_main <- within_level_letters(df_p_main, level_col = "P_show") %>%
  mutate(
    P_show = factor(P_show, levels = c("P0", "PLow", "PMed", "PHigh")),
    CaLevel = factor(CaLevel, levels = c("Ca0", "CaHigh"))
  )

plot_P <- sum_p_main %>%
  left_join(cld_p_main, by = c("P_show", "CaLevel")) %>%
  mutate(
    x_num = as.numeric(P_show),
    x_lab = x_num + unname(P_LABEL_NUDGE[as.character(CaLevel)])
  ) %>%
  add_label_y(pad = 0.055, stagger = 0.060)

# 9) PLOT FUNCTIONS ------------------------------------------------------------
# IMPORTANT: the numeric HCl x-axis is preserved, and the P-axis is explicitly
# numeric so peak labels, error bars and points are always on the same scale.

add_inside_panel_tag <- function(p, tag, left_x, y_limits) {
  p +
    annotate(
      "text",
      x = left_x,
      y = y_limits[2] - 0.025 * diff(y_limits),
      label = tag,
      family = FONT_FAMILY,
      size = 3.8,
      fontface = "bold",
      hjust = 0, vjust = 1,
      colour = "black"
    )
}

make_hcl_panel <- function(df_var, ylab_txt, panel_tag,
                           show_x_title = TRUE, show_legend = FALSE) {
  
  lims <- get_axis_limits(
    df_var,
    hard_zero = FALSE,
    lower_pad = 0.10,
    upper_pad = 0.12
  )
  
  x_ticks <- sort(unique(df_var$HCl_uL))
  x_start <- min(x_ticks)
  x_end <- max(x_ticks)
  x_tag <- x_start + 0.025 * (x_end - x_start)
  
  p <- ggplot(
    df_var,
    aes(
      x = HCl_uL,
      y = Mean,
      colour = CaLevel,
      shape = CaLevel,
      linetype = CaLevel,
      group = CaLevel
    )
  ) +
    geom_line(linewidth = 0.60) +
    geom_errorbar(
      aes(ymin = Mean - SE, ymax = Mean + SE),
      width = 5, linewidth = 0.38,
      show.legend = FALSE
    ) +
    geom_point(size = 2.15, stroke = 0.25) +
    geom_text(
      aes(x = x_lab, y = lab_y, label = CLD),
      size = 2.65,
      vjust = 0,
      show.legend = FALSE
    ) +
    scale_colour_manual(
      values = col_ca,
      breaks = c("Ca0", "CaHigh"),
      drop = FALSE,
      name = NULL,
      guide = if (show_legend) "legend" else "none"
    ) +
    scale_shape_manual(
      values = shape_ca,
      breaks = c("Ca0", "CaHigh"),
      drop = FALSE,
      name = NULL,
      guide = if (show_legend) "legend" else "none"
    ) +
    scale_linetype_manual(
      values = line_ca,
      breaks = c("Ca0", "CaHigh"),
      drop = FALSE,
      name = NULL,
      guide = if (show_legend) "legend" else "none"
    ) +
    scale_x_continuous(
      breaks = x_ticks,
      limits = c(x_start - 14, x_end + 14),
      expand = expansion(mult = 0)
    ) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 5),
      expand = expansion(mult = 0)
    ) +
    coord_cartesian(ylim = lims, expand = FALSE, clip = "on") +
    labs(
      x = if (show_x_title) "0.1 M HCl added (µL)" else NULL,
      y = ylab_txt
    ) +
    nc_theme() +
    theme(
      legend.position = if (show_legend) c(0.86, 0.98) else "none",
      legend.justification = if (show_legend) c(1, 1) else "center",
      legend.direction = "horizontal",
      legend.box.margin = margin(0, 0, 0, 0),
      axis.title.x = if (show_x_title) element_text(size = 9.1) else element_blank(),
      plot.tag = element_blank()
    )
  
  add_inside_panel_tag(p, panel_tag, x_tag, lims)
}

make_p_panel <- function(plot_df, panel_tag) {
  
  lims <- get_axis_limits(
    plot_df,
    hard_zero = FALSE,
    lower_pad = 0.11,
    upper_pad = 0.12
  )
  
  # Every colour group gets the same explicit horizontal offset in all
  # geometries; this avoids mismatches caused by mixing discrete and numeric x.
  p_points <- plot_df %>%
    mutate(
      x_num = as.numeric(P_show),
      x_dodged = x_num + if_else(CaLevel == "Ca0", -0.11, 0.11),
      x_label = x_dodged
    )
  
  p <- ggplot(
    p_points,
    aes(x = x_dodged, y = Mean, colour = CaLevel, shape = CaLevel)
  ) +
    geom_errorbar(
      aes(ymin = Mean - SE, ymax = Mean + SE),
      width = 0.065, linewidth = 0.43,
      show.legend = FALSE
    ) +
    geom_point(size = 2.4, stroke = 0.25, show.legend = FALSE) +
    geom_text(
      aes(x = x_label, y = lab_y, label = CLD),
      size = 2.9, vjust = 0,
      show.legend = FALSE
    ) +
    scale_colour_manual(
      values = col_ca,
      breaks = c("Ca0", "CaHigh"),
      name = NULL, guide = "none"
    ) +
    scale_shape_manual(
      values = shape_ca,
      breaks = c("Ca0", "CaHigh"),
      name = NULL, guide = "none"
    ) +
    scale_x_continuous(
      breaks = 1:4,
      labels = c("P0", "PLow", "PMed", "PHigh"),
      limits = c(0.60, 4.40),
      expand = expansion(mult = 0)
    ) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 5),
      expand = expansion(mult = 0)
    ) +
    coord_cartesian(ylim = lims, expand = FALSE, clip = "on") +
    labs(
      x = "P level",
      y = expression(SAP~(mg~kg^{-1}))
    ) +
    nc_theme() +
    theme(
      legend.position = "none",
      plot.tag = element_blank()
    )
  
  add_inside_panel_tag(p, panel_tag, 0.68, lims)
}

# 10) BUILD PANELS -------------------------------------------------------------
# Exactly the agreed layout:
# (a) Soil pH / HCl                  (b) SAP / HCl
# (c) SAP across P levels — FULL-WIDTH BOTTOM PANEL

p_pH <- make_hcl_panel(
  df_var      = plot_pH,
  ylab_txt    = "pH (1:5)",
  panel_tag   = "(a)",
  show_x_title = TRUE,
  show_legend = TRUE
)

p_SAP_hcl <- make_hcl_panel(
  df_var      = plot_SAP_hcl,
  ylab_txt    = expression(SAP~(mg~kg^{-1})),
  panel_tag   = "(b)",
  show_x_title = TRUE,
  show_legend = FALSE
)

p_P <- make_p_panel(
  plot_df     = plot_P,
  panel_tag   = "(c)"
)

# 11) CORRECT PORTRAIT LAYOUT --------------------------------------------------
# AB
# CC
# Two top panels each occupy half the width. Panel (c) occupies the full bottom.
# Do NOT use "AAC/BBC", which was the previous incorrect arrangement.

final_fig <- (p_pH | p_SAP_hcl) / p_P +
  patchwork::plot_layout(
    heights = c(1.08, 1.00),
    guides = "keep"
  ) &
  theme(
    plot.background = element_rect(fill = "white", colour = NA)
  )

print(final_fig)

# 12) EXPORT -------------------------------------------------------------------
word_png  <- file.path(out_dir, "Fig_S5_Word_Portrait_600dpi.png")
word_tif  <- file.path(out_dir, "Fig_S5_Word_Portrait_600dpi.tiff")
vector_pdf <- file.path(out_dir, "Fig_S5_NC_Vector.pdf")

ggsave(
  filename = word_png, plot = final_fig,
  width = word_width_cm, height = word_height_cm,
  units = "cm", dpi = 600, bg = "white", limitsize = FALSE
)

ggsave(
  filename = word_tif, plot = final_fig,
  width = word_width_cm, height = word_height_cm,
  units = "cm", dpi = 600,
  compression = "lzw", bg = "white", limitsize = FALSE
)

ggsave(
  filename = vector_pdf, plot = final_fig,
  width = journal_width_cm, height = journal_height_cm,
  units = "cm",
  device = if (capabilities("cairo")) cairo_pdf else "pdf",
  bg = "white", limitsize = FALSE
)

# 13) SAVE TABLES --------------------------------------------------------------
stats_out <- file.path(out_dir, paste0("FigS5_tables_", stamp, ".xlsx"))
openxlsx::write.xlsx(
  list(
    "Raw_HCl_clean"      = dat_hcl,
    "Summary_HCl"        = summaries_hcl,
    "pH_plot_table"      = plot_pH,
    "SAP_HCl_plot_table" = plot_SAP_hcl,
    "Raw_P_clean"        = dat_p,
    "SAP_P_plot_table"   = plot_P,
    "pH_ANOVA"           = as.data.frame(summary(fit_pH)[[1]]),
    "pH_Tukey"           = as.data.frame(tk_pH$Group)
  ),
  file = stats_out,
  overwrite = TRUE
)

cat("\nFig. S5 exported to:\n", normalizePath(out_dir), "\n")
cat(" - ", basename(word_png), "\n")
cat(" - ", basename(word_tif), "\n")
cat(" - ", basename(vector_pdf), "\n")
cat(" - ", basename(stats_out), "\n")