rm(list = ls())
graphics.off()
cat("\014")

# ============================================================================
# Supplementary Fig. S6 — Release kinetics of Ca2+, SO4^2- and dissolved P
# Nature Communications style, portrait Word-friendly layout
#
# Panel (a): cumulative Ca release from GP and PEG
# Panel (b): GP-SO4, PEG-SO4 and PEG-P in the same panel
#            with ONE common time axis (0–48 h)
#            left y-axis = SO4 concentration
#            right y-axis = dissolved P concentration
#
# Notes based on supervisor instruction:
# - Keep all three series in panel (b)
# - Use one common time axis for SO4 and dissolved P
# - Show the full 0–48 h timeline on the x-axis
# - Preserve the existing series identity:
#     GP-SO4, PEG-SO4, PEG-P
# ============================================================================

# 1) PACKAGES -----------------------------------------------------------------
pkgs <- c("readxl", "ggplot2", "dplyr", "tidyr", "patchwork", "openxlsx", "grid")

to_install <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

suppressPackageStartupMessages({
  invisible(lapply(pkgs, library, character.only = TRUE))
})

# 2) INPUT / OUTPUT ------------------------------------------------------------
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

ca_file  <- repo_input_path("Conventional_Treatments.xlsx")
so4_file <- repo_input_path("SO4_GP_PEG_Experiment.xlsx")
p_file   <- repo_input_path("Conventional_Treatments.xlsx")

for (f in c(ca_file, so4_file, p_file)) {
  if (!file.exists(f)) {
    stop(paste0("Cannot find file: ", f,
                "\nPlace it in the current R working directory."), call. = FALSE)
  }
}

out_dir <- file.path("outputs", "Fig_S6_NC_Portrait")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Word portrait insertion size
word_width_cm  <- 15.5
word_height_cm <- 17.0
journal_width_cm  <- 18.0
journal_height_cm <- word_height_cm * journal_width_cm / word_width_cm

# 3) STYLE --------------------------------------------------------------------
FONT_FAMILY <- "Arial"
BASE_SIZE   <- 8.8

col_map <- c(
  "GP-Ca"   = "#1F4E79",
  "PEG-Ca"  = "#C55A11",
  "GP-SO4"  = "#1F4E79",
  "PEG-SO4" = "#C55A11",
  "PEG-P"   = "black"
)

shape_map <- c(
  "GP-Ca"   = 21,
  "PEG-Ca"  = 23,
  "GP-SO4"  = 21,
  "PEG-SO4" = 23,
  "PEG-P"   = 24
)

linetype_map <- c(
  "GP-Ca"   = "solid",
  "PEG-Ca"  = "22",
  "GP-SO4"  = "solid",
  "PEG-SO4" = "22",
  "PEG-P"   = "42"
)

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
      axis.text.x      = element_text(size = 8.3, colour = "black",
                                      margin = margin(t = 2)),
      axis.text.y      = element_text(size = 8.2, colour = "black"),
      axis.title.y.right = element_text(size = 9.1, colour = "black",
                                        margin = margin(l = 5)),
      axis.text.y.right  = element_text(size = 8.2, colour = "black"),
      axis.line.y.right  = element_line(linewidth = 0.46, colour = "black"),
      axis.ticks.y.right = element_line(linewidth = 0.39, colour = "black"),
      legend.title     = element_blank(),
      legend.text      = element_text(size = 8.2, colour = "black"),
      legend.key       = element_blank(),
      legend.key.size  = unit(3.8, "mm"),
      legend.spacing.x = unit(1.8, "mm"),
      legend.margin    = margin(0, 0, 0, 0),
      plot.margin      = margin(t = 5, r = 8, b = 5, l = 6, unit = "pt")
    )
}

add_inside_tag <- function(p, label, x, y) {
  p + annotate(
    "text", x = x, y = y, label = label,
    family = FONT_FAMILY, fontface = "bold", size = 4.0,
    hjust = 0, vjust = 1, colour = "black"
  )
}

# 4) PANEL A DATA: cumulative Ca release --------------------------------------
ca_raw <- read_excel(path = ca_file, sheet = "Ca_release")

ca_df <- ca_raw %>%
  rename(Sample_Id = `Sample Id`) %>%
  filter(!is.na(Sample_Id)) %>%
  mutate(
    Treatment = sub("-[0-9.]+$", "", Sample_Id),
    Time_h    = as.numeric(sub("^[^-]+-", "", Sample_Id)),
    Ca        = as.numeric(Ca),
    Ca        = ifelse(Ca < 0, 0, Ca)
  ) %>%
  filter(!is.na(Time_h), !is.na(Ca), Treatment %in% c("GP", "PEG")) %>%
  mutate(
    Series = factor(paste0(Treatment, "-Ca"), levels = c("GP-Ca", "PEG-Ca"))
  )

# 5) PANEL B DATA: SO4 and dissolved P ----------------------------------------
# SO4: use explicit common 0–48 h scale (supervisor request)
so4_breaks <- c(0, 4, 8, 12, 16, 20, 24, 28, 31, 34, 37, 40, 43, 46, 48)

so4_raw <- read_excel(path = so4_file, sheet = "Graph Data", skip = 1)
so4_raw <- so4_raw[, 1:3]
colnames(so4_raw) <- c("Time_h", "GP", "PEG")

so4_df <- so4_raw %>%
  mutate(
    Time_h = as.numeric(Time_h),
    GP     = as.numeric(GP),
    PEG    = as.numeric(PEG)
  ) %>%
  filter(!is.na(Time_h)) %>%
  pivot_longer(cols = c(GP, PEG), names_to = "Treatment", values_to = "SO4") %>%
  filter(!is.na(SO4)) %>%
  mutate(
    Series = factor(paste0(Treatment, "-SO4"), levels = c("GP-SO4", "PEG-SO4"))
  )

p_raw <- read_excel(path = p_file, sheet = "P_dissolve")

p_df <- p_raw %>%
  rename(
    Time_h = `Time (h)`,
    P      = `Dissolved P (mg/L)`
  ) %>%
  mutate(
    Time_h = as.numeric(Time_h),
    P      = as.numeric(P)
  ) %>%
  filter(!is.na(Time_h), !is.na(P)) %>%
  mutate(Series = factor("PEG-P", levels = "PEG-P"))

# Scaling for secondary y-axis in panel (b)
# Preserve the original visual ranges but use the SAME x-axis for all series.
so4_ymin <- 5
so4_ymax <- 45
p_ymin   <- 0
p_ymax   <- 35

y_scale_P <- (so4_ymax - so4_ymin) / (p_ymax - p_ymin)

# transform P to left-axis coordinates: y_left = so4_ymin + P * y_scale_P
p_df <- p_df %>%
  mutate(P_on_left = so4_ymin + P * y_scale_P)

# 6) PANEL A PLOT --------------------------------------------------------------
p_a <- ggplot(
  ca_df,
  aes(x = Time_h, y = Ca, colour = Series, shape = Series, linetype = Series)
) +
  geom_line(linewidth = 0.72) +
  geom_point(size = 2.65, fill = "white", stroke = 0.95) +
  scale_colour_manual(
    values = col_map[c("GP-Ca", "PEG-Ca")],
    breaks = c("GP-Ca", "PEG-Ca"),
    labels = c("GP-Ca", "PEG-Ca")
  ) +
  scale_shape_manual(
    values = shape_map[c("GP-Ca", "PEG-Ca")],
    breaks = c("GP-Ca", "PEG-Ca"),
    labels = c("GP-Ca", "PEG-Ca")
  ) +
  scale_linetype_manual(
    values = linetype_map[c("GP-Ca", "PEG-Ca")],
    breaks = c("GP-Ca", "PEG-Ca"),
    labels = c("GP-Ca", "PEG-Ca")
  ) +
  scale_x_continuous(
    breaks = c(0, 2, 4, 16, 32, 48),
    limits = c(-1.5, 49.5),
    expand = expansion(mult = 0)
  ) +
  scale_y_continuous(
    limits = c(0, 25.5),
    breaks = seq(0, 25, by = 5),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Time (h)",
    y = "Total Ca cumulative mass released (mg)"
  ) +
  guides(
    colour = guide_legend(nrow = 1, byrow = TRUE),
    shape = guide_legend(nrow = 1, byrow = TRUE),
    linetype = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  nc_theme() +
  theme(
    legend.position = c(0.53, 0.99),
    legend.justification = c(0.5, 1),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    axis.title.x = element_text(size = 9.1),
    plot.tag = element_blank()
  )

p_a <- add_inside_tag(p_a, "(a)", x = -1.1, y = 25.4)

# 7) PANEL B PLOT --------------------------------------------------------------
p_b <- ggplot() +
  # SO4 curves (left y-axis)
  geom_line(
    data = so4_df,
    aes(x = Time_h, y = SO4, colour = Series, shape = Series, linetype = Series),
    linewidth = 0.72
  ) +
  geom_point(
    data = so4_df,
    aes(x = Time_h, y = SO4, colour = Series, shape = Series),
    size = 2.65, fill = "white", stroke = 0.95
  ) +
  # Dissolved P curve scaled to left axis, same x-axis
  geom_line(
    data = p_df,
    aes(x = Time_h, y = P_on_left, colour = Series, shape = Series, linetype = Series),
    linewidth = 0.72
  ) +
  geom_point(
    data = p_df,
    aes(x = Time_h, y = P_on_left, colour = Series, shape = Series),
    size = 2.75, fill = "black", stroke = 0.95
  ) +
  scale_colour_manual(
    values = col_map[c("GP-SO4", "PEG-SO4", "PEG-P")],
    breaks = c("GP-SO4", "PEG-SO4", "PEG-P"),
    labels = expression("GP-"*"SO"[4]^{"2-"}, "PEG-"*"SO"[4]^{"2-"}, "PEG-P")
  ) +
  scale_shape_manual(
    values = shape_map[c("GP-SO4", "PEG-SO4", "PEG-P")],
    breaks = c("GP-SO4", "PEG-SO4", "PEG-P"),
    labels = expression("GP-"*"SO"[4]^{"2-"}, "PEG-"*"SO"[4]^{"2-"}, "PEG-P")
  ) +
  scale_linetype_manual(
    values = linetype_map[c("GP-SO4", "PEG-SO4", "PEG-P")],
    breaks = c("GP-SO4", "PEG-SO4", "PEG-P"),
    labels = expression("GP-"*"SO"[4]^{"2-"}, "PEG-"*"SO"[4]^{"2-"}, "PEG-P")
  ) +
  scale_x_continuous(
    name   = "Time (h)",
    breaks = so4_breaks,
    limits = c(-1, 49),
    expand = expansion(mult = 0)
  ) +
  scale_y_continuous(
    name   = expression("SO"[4]^"2-"~"concentration (meq L"^{-1}*")"),
    limits = c(so4_ymin, so4_ymax),
    breaks = seq(5, 45, by = 5),
    expand = expansion(mult = c(0, 0)),
    sec.axis = sec_axis(
      trans = ~ (. - so4_ymin) / y_scale_P,
      name = expression("Dissolved P (mg L"^{-1}*")"),
      breaks = seq(0, 35, by = 5)
    )
  ) +
  labs() +
  guides(
    colour = guide_legend(
      nrow = 1, byrow = TRUE,
      override.aes = list(
        shape = c(21, 23, 24),
        linetype = c("solid", "22", "42"),
        fill = c("white", "white", "black")
      )
    ),
    shape = "none",
    linetype = "none"
  ) +
  nc_theme() +
  theme(
    legend.position = c(0.56, 0.985),
    legend.justification = c(0.5, 1),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    axis.title.x = element_text(size = 9.1),
    plot.tag = element_blank()
  )

p_b <- add_inside_tag(p_b, "(b)", x = -0.3, y = 44.8)

# 8) COMBINE FIGURE ------------------------------------------------------------
final_fig <- p_a / p_b +
  patchwork::plot_layout(heights = c(0.92, 1.08)) &
  theme(plot.background = element_rect(fill = "white", colour = NA))

print(final_fig)

# 9) EXPORT -------------------------------------------------------------------
out_png <- file.path(out_dir, "Fig_S6_Word_Portrait_600dpi.png")
out_tif <- file.path(out_dir, "Fig_S6_Word_Portrait_600dpi.tiff")
out_pdf <- file.path(out_dir, "Fig_S6_NC_Vector.pdf")

# Word PNG
ggsave(
  filename = out_png,
  plot = final_fig,
  width = word_width_cm,
  height = word_height_cm,
  units = "cm",
  dpi = 600,
  bg = "white",
  limitsize = FALSE
)

# TIFF
ggsave(
  filename = out_tif,
  plot = final_fig,
  width = word_width_cm,
  height = word_height_cm,
  units = "cm",
  dpi = 600,
  compression = "lzw",
  bg = "white",
  limitsize = FALSE
)

# Vector PDF
ggsave(
  filename = out_pdf,
  plot = final_fig,
  width = journal_width_cm,
  height = journal_height_cm,
  units = "cm",
  device = if (capabilities("cairo")) cairo_pdf else "pdf",
  bg = "white",
  limitsize = FALSE
)

# 10) SAVE CLEAN DATA / SUMMARY TABLES -----------------------------------------
ca_statistics <- ca_df %>%
  group_by(Series) %>%
  summarise(
    n = n(),
    mean_Ca = mean(Ca, na.rm = TRUE),
    sd_Ca = sd(Ca, na.rm = TRUE),
    maximum_Ca = max(Ca, na.rm = TRUE),
    final_Ca = Ca[which.max(Time_h)],
    .groups = "drop"
  )

so4_statistics <- so4_df %>%
  group_by(Series) %>%
  summarise(
    n = n(),
    mean_SO4 = mean(SO4, na.rm = TRUE),
    sd_SO4 = sd(SO4, na.rm = TRUE),
    maximum_SO4 = max(SO4, na.rm = TRUE),
    final_SO4 = SO4[which.max(Time_h)],
    .groups = "drop"
  )

p_statistics <- p_df %>%
  summarise(
    n = n(),
    mean_P = mean(P, na.rm = TRUE),
    sd_P = sd(P, na.rm = TRUE),
    max_P = max(P, na.rm = TRUE),
    final_P = P[which.max(Time_h)]
  )

stats_out <- file.path(out_dir, paste0("Fig_S6_tables_", stamp, ".xlsx"))
openxlsx::write.xlsx(
  list(
    Ca_raw_data = ca_df,
    SO4_raw_data = so4_df,
    P_dissolution_data = p_df,
    Ca_statistics = ca_statistics,
    SO4_statistics = so4_statistics,
    P_statistics = p_statistics
  ),
  file = stats_out,
  overwrite = TRUE
)

cat("\nFig. S6 exported to:\n", normalizePath(out_dir), "\n")
cat(" - ", basename(out_png), "\n")
cat(" - ", basename(out_tif), "\n")
cat(" - ", basename(out_pdf), "\n")
cat(" - ", basename(stats_out), "\n")