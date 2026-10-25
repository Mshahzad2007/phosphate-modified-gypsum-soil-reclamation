rm(list = ls())
graphics.off()
cat("\014")

################################################################################
# SUPPLEMENTARY FIG. S3 — XPS, WORD PORTRAIT / NATURE COMMUNICATIONS
# Preserved: ALL original peak-centre inputs, all label_x, label_y_frac,
# hjust, fit widths/constraints, fitting algorithm and arrow geometry.
# Only the display typography, output size/paths, and export formats change.
#
# SUPPLEMENTARY XPS FIGURE — COMPACT FIVE-PANEL VERSION
#
# (a) = GP C1s
# (b) = PEG C1s
# (c) = GP O1s
# (d) = PEG O1s
# (e) = GP Ca2p
#
# Layout:
#
# (a)     (b)
# (c)     (d)
# (e)     (e)
#
# AXES:
# - Y-axis titles shown in every panel
# - Y-axis numerical values removed
# - X-axis binding-energy values shown in every panel
# - "Binding Energy (eV)" shown ONLY below Panel (e)
#
# Panel tags: bold lowercase (a), (b), ... (e) — Nature style
################################################################################


# =============================================================================
# PACKAGES
# =============================================================================

pkgs <- c(
  "readxl", "dplyr", "ggplot2", "purrr", "stringr",
  "janitor", "patchwork", "minpack.lm", "tibble", "tidyr",
  "grid", "openxlsx"
)

missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, dependencies = TRUE)
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(purrr)
  library(stringr)
  library(janitor)
  library(patchwork)
  library(minpack.lm)
  library(tibble)
  library(tidyr)
  library(grid)
  library(openxlsx)
})


# =============================================================================
# FILES
# =============================================================================

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
file_gp  <- repo_input_path("XPS_GP.xls")
file_peg <- repo_input_path("XPS_PEG.xls")

out_dir <- "XPS_Supplementary_Fig_S3_NC_Portrait"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

scan_sheets <- c("C1s Scan", "O1s Scan", "Ca2p Scan")


# =============================================================================
# TEXT SETTINGS
# =============================================================================

PEAK_LABEL_SIZE <- 3.10
PEAK_LINEHEIGHT <- 0.78

# NC-friendly sans-serif figure typeface. No annotation coordinates change.
FIGURE_FONT <- "Arial"


# =============================================================================
# FILE CHECK
# =============================================================================

if (!file.exists(file_gp)) {
  stop(
    "\nFile not found: ", file_gp,
    "\nCurrent working directory:\n", getwd(),
    "\n\nProvide the original XPS_GP.xls under data/ (the CSV exports alone cannot be read by this script)."
  )
}

if (!file.exists(file_peg)) {
  stop(
    "\nFile not found: ", file_peg,
    "\nCurrent working directory:\n", getwd(),
    "\n\nProvide the original XPS_PEG.xls under data/ (the CSV exports alone cannot be read by this script)."
  )
}


# =============================================================================
# READ DATA
# =============================================================================

read_scan_sheet <- function(file, sheet_name, sample_name) {
  
  if (!sheet_name %in% readxl::excel_sheets(file)) {
    stop("\nSheet not found: ", sheet_name, " in file: ", file)
  }
  
  x <- readxl::read_excel(
    file,
    sheet = sheet_name,
    .name_repair = "minimal"
  ) %>%
    janitor::clean_names()
  
  x <- x %>% select(where(~ !all(is.na(.))))
  
  required_cols <- c("binding_energy", "counts")
  missing_cols <- setdiff(required_cols, names(x))
  
  if (length(missing_cols) > 0) {
    stop(
      "\nRequired columns missing in ", file, " | sheet: ", sheet_name,
      "\nMissing: ", paste(missing_cols, collapse = ", "),
      "\n\nRequired columns:\nbinding_energy\ncounts"
    )
  }
  
  if (!"backgnd_counts" %in% names(x)) x$backgnd_counts <- NA_real_
  
  x %>%
    mutate(
      binding_energy = suppressWarnings(as.numeric(binding_energy)),
      counts         = suppressWarnings(as.numeric(counts)),
      backgnd_counts = suppressWarnings(as.numeric(backgnd_counts)),
      sample = sample_name,
      sheet  = sheet_name
    ) %>%
    filter(is.finite(binding_energy), is.finite(counts))
}

files <- c(GP = file_gp, PEG = file_peg)

scan_data <- imap_dfr(files, function(file, sample_name) {
  map_dfr(scan_sheets, function(sh) read_scan_sheet(file, sh, sample_name))
})

scan_data <- scan_data %>%
  mutate(
    sheet  = factor(sheet,  levels = scan_sheets),
    sample = factor(sample, levels = c("GP", "PEG"))
  )

message("\nDetected scan data:")
print(scan_data %>% count(sheet, sample))


# =============================================================================
# STATISTICS STORAGE
# =============================================================================

fit_components_store <- tibble()
fit_quality_store    <- tibble()


# =============================================================================
# LABEL FUNCTIONS
# =============================================================================

get_region_label <- function(sheet_name) {
  if (sheet_name == "C1s Scan")  return("C 1s")
  if (sheet_name == "O1s Scan")  return("O 1s")
  if (sheet_name == "Ca2p Scan") return("Ca 2p")
  return("")
}

make_y_label <- function(sample_name, sheet_name) {
  region <- get_region_label(sheet_name)
  paste0(sample_name, " (", region, ")")
}


# =============================================================================
# PUBLICATION THEME
# =============================================================================

pub_theme <- function(base_size = 9) {
  theme_classic(base_size = base_size, base_family = FIGURE_FONT) +
    theme(
      axis.line         = element_line(colour = "black", linewidth = 0.45),
      axis.ticks        = element_line(colour = "black", linewidth = 0.40),
      axis.ticks.length = unit(2.0, "pt"),
      axis.text         = element_text(colour = "black", size = base_size - 1, face = "plain"),
      axis.title        = element_text(colour = "black", size = base_size, face = "plain"),
      axis.title.y      = element_text(margin = margin(r = 2)),
      plot.title        = element_blank(),
      plot.margin       = margin(t = 3, r = 3, b = 2, l = 5),
      panel.grid        = element_blank(),
      panel.background  = element_rect(fill = "white", colour = NA),
      plot.background   = element_rect(fill = "white", colour = NA),
      legend.position   = "none"
    )
}


# =============================================================================
# PANEL LABEL — bold lowercase Nature style
# =============================================================================

add_panel_label <- function(p, label) {
  # Kept as a pass-through helper. Panel tags are now drawn INSIDE each panel
  # within make_decon_panel() to match the style used in Fig. S1.
  p + theme(plot.margin = margin(t = 4, r = 3, b = 2, l = 6))
}


# =============================================================================
# SHIRLEY BACKGROUND
# =============================================================================

shirley_bg <- function(counts, tol = 1e-6, max_iter = 200) {
  
  n  <- length(counts)
  bg <- numeric(n)
  
  for (i in seq_len(max_iter)) {
    
    old   <- bg
    area  <- cumsum(counts - bg)
    total <- area[n]
    
    if (!is.finite(total) || abs(total) < 1e-10) break
    
    bg <- counts[1] + (counts[n] - counts[1]) * area / total
    
    if (max(abs(bg - old), na.rm = TRUE) < tol) break
  }
  
  bg
}


# =============================================================================
# ROBUST GAUSSIAN FITTING
# =============================================================================

fit_gaussians_robust <- function(be, net, peaks_init) {
  
  n       <- nrow(peaks_init)
  net_max <- max(net, na.rm = TRUE)
  
  if (!is.finite(net_max) || net_max <= 0) return(NULL)
  
  peaks_init <- peaks_init %>%
    rowwise() %>%
    mutate(
      amplitude = max(net[which.min(abs(be - center))] * 0.85, 50)
    ) %>%
    ungroup()
  
  terms <- paste0(
    sprintf("A%d*exp(-((x-mu%d)^2)/(2*sig%d^2))", 1:n, 1:n, 1:n),
    collapse = " + "
  )
  
  fml <- as.formula(paste("net ~", terms))
  
  nms <- c(
    paste0("A",   seq_len(n)),
    paste0("mu",  seq_len(n)),
    paste0("sig", seq_len(n))
  )
  
  lower <- setNames(
    c(rep(0, n), peaks_init$center - 2, rep(0.15, n)),
    nms
  )
  
  upper <- setNames(
    c(rep(net_max * 4, n), peaks_init$center + 2, rep(5, n)),
    nms
  )
  
  ctrl <- nls.lm.control(maxiter = 2000, ftol = 1e-10, ptol = 1e-10)
  
  for (sc in c(1, 0.5, 2, 0.25, 3)) {
    
    start <- setNames(
      c(peaks_init$amplitude * sc, peaks_init$center, peaks_init$sigma),
      nms
    )
    
    fit <- tryCatch(
      nlsLM(
        fml,
        data    = data.frame(x = be, net = net),
        start   = start,
        lower   = lower,
        upper   = upper,
        control = ctrl
      ),
      error = function(e) NULL
    )
    
    if (!is.null(fit)) return(fit)
  }
  
  NULL
}


# =============================================================================
# GAUSSIAN CURVES
# =============================================================================

gauss_curves <- function(be, fit, n) {
  
  if (is.null(fit)) return(NULL)
  
  co <- coef(fit)
  
  map_dfr(seq_len(n), function(i) {
    tibble(
      binding_energy = be,
      intensity = co[[paste0("A", i)]] *
        exp(-((be - co[[paste0("mu", i)]])^2) /
              (2 * co[[paste0("sig", i)]]^2)),
      peak_id = i
    )
  })
}


# =============================================================================
# SAVE FIT STATISTICS
# =============================================================================

save_fit_statistics <- function(
    fit, be, net, peaks_init, sheet_name, sample_name, panel_label
) {
  
  region <- get_region_label(sheet_name)
  
  if (is.null(fit)) {
    fit_quality_store <<- bind_rows(
      fit_quality_store,
      tibble(
        panel = panel_label, sample = sample_name, sheet = sheet_name,
        region = region, fit_status = "failed",
        n_points = length(be),
        r_squared = NA_real_, rmse = NA_real_, rss = NA_real_
      )
    )
    return(invisible(NULL))
  }
  
  co   <- coef(fit)
  n_pk <- nrow(peaks_init)
  
  predicted_net <- tryCatch(
    as.numeric(predict(fit, newdata = data.frame(x = be))),
    error = function(e) rep(NA_real_, length(be))
  )
  
  rss <- sum((net - predicted_net)^2, na.rm = TRUE)
  tss <- sum((net - mean(net, na.rm = TRUE))^2, na.rm = TRUE)
  
  r2   <- ifelse(is.finite(tss) && tss > 0, 1 - rss / tss, NA_real_)
  rmse <- sqrt(mean((net - predicted_net)^2, na.rm = TRUE))
  
  fit_quality_store <<- bind_rows(
    fit_quality_store,
    tibble(
      panel = panel_label, sample = sample_name, sheet = sheet_name,
      region = region, fit_status = "success",
      n_points = length(be),
      r_squared = r2, rmse = rmse, rss = rss
    )
  )
  
  comp_stats <- map_dfr(seq_len(n_pk), function(i) {
    
    amp  <- unname(co[[paste0("A", i)]])
    mu   <- unname(co[[paste0("mu", i)]])
    sig  <- unname(co[[paste0("sig", i)]])
    area <- amp * sig * sqrt(2 * pi)
    
    tibble(
      panel = panel_label,
      sample = sample_name,
      sheet  = sheet_name,
      region = region,
      component_id = i,
      peak_label = peaks_init$label[i],
      initial_center_ev = peaks_init$center[i],
      fitted_center_ev  = mu,
      shift_from_initial_ev = mu - peaks_init$center[i],
      sigma_ev  = sig,
      fwhm_ev   = 2.35482 * sig,
      amplitude = amp,
      area      = area
    )
  }) %>%
    mutate(area_percent = 100 * area / sum(area, na.rm = TRUE))
  
  fit_components_store <<- bind_rows(fit_components_store, comp_stats)
  
  invisible(NULL)
}


# =============================================================================
# DECONVOLUTION PANEL
# =============================================================================

make_decon_panel <- function(
    sheet_name,
    sample_name,
    peaks_init,
    label,
    show_x_title = FALSE
) {
  
  dat <- scan_data %>%
    filter(sheet == sheet_name, sample == sample_name) %>%
    arrange(desc(binding_energy))
  
  if (nrow(dat) == 0) {
    return(
      ggplot() +
        annotate(
          "text", x = 0.5, y = 0.5,
          label = paste("No data\n", sheet_name, sample_name),
          size = 3.2
        ) +
        theme_void()
    )
  }
  
  be     <- dat$binding_energy
  counts <- dat$counts
  
  bg  <- shirley_bg(counts)
  net <- pmax(counts - bg, 0)
  
  n_pk   <- nrow(peaks_init)
  fit    <- fit_gaussians_robust(be, net, peaks_init)
  curves <- gauss_curves(be, fit, n_pk)
  
  save_fit_statistics(
    fit = fit, be = be, net = net,
    peaks_init = peaks_init,
    sheet_name = sheet_name,
    sample_name = sample_name,
    panel_label = label
  )
  
  y_min <- min(counts, na.rm = TRUE)
  y_max <- max(counts, na.rm = TRUE)
  y_rng <- y_max - y_min
  if (!is.finite(y_rng) || y_rng <= 0) y_rng <- 1
  
  # -------- tight Y range --------
  y_lower   <- y_min - 0.025 * y_rng
  label_top <- max(peaks_init$label_y_frac, na.rm = TRUE)
  y_upper   <- y_min + max(1.10, label_top + 0.025) * y_rng
  
  bg_df <- tibble(binding_energy = be, bg = bg)
  
  p <- ggplot() +
    geom_point(
      data = dat,
      aes(binding_energy, counts),
      shape = 16, size = 0.22, colour = "black"
    ) +
    geom_line(
      data = bg_df,
      aes(binding_energy, bg),
      colour = "royalblue", linewidth = 0.38, linetype = "dashed"
    ) +
    scale_x_reverse(
      expand = expansion(mult = c(0.015, 0.015))
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0))
    ) +
    coord_cartesian(
      ylim = c(y_lower, y_upper),
      clip = "off"
    ) +
    labs(
      x = if (show_x_title) "Binding Energy (eV)" else NULL,
      y = make_y_label(sample_name, sheet_name),
      title = NULL
    ) +
    pub_theme(base_size = 9) +
    theme(
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank(),
      
      axis.text.x  = element_text(
        colour = "black", size = 8, face = "plain",
        margin = margin(t = 2)
      ),
      axis.ticks.x = element_line(colour = "black", linewidth = 0.40),
      
      axis.title.x = if (show_x_title) {
        element_text(
          colour = "black", size = 9, face = "plain",
          margin = margin(t = 3)
        )
      } else {
        element_blank()
      },
      
      axis.title.y = element_text(
        colour = "black", size = 9, face = "plain",
        margin = margin(r = 2)
      )
    )
  
  # Panel tag inside upper-left corner for consistency with Fig. S1
  be_min <- min(be, na.rm = TRUE)
  be_max <- max(be, na.rm = TRUE)
  x_tag  <- be_max - 0.02 * (be_max - be_min)
  y_tag  <- y_upper - 0.015 * (y_upper - y_lower)
  
  p <- p + annotate(
    "text",
    x = x_tag, y = y_tag,
    label = label,
    family = FIGURE_FONT,
    fontface = "bold",
    size = 4.0,
    hjust = 0, vjust = 1,
    colour = "black"
  )
  
  if (!is.null(curves)) {
    
    envelope <- curves %>%
      group_by(binding_energy) %>%
      summarise(fit_net = sum(intensity), .groups = "drop") %>%
      left_join(bg_df, by = "binding_energy") %>%
      mutate(fit_total = bg + fit_net)
    
    for (i in seq_len(n_pk)) {
      
      ci <- curves %>%
        filter(peak_id == i) %>%
        left_join(bg_df, by = "binding_energy") %>%
        mutate(y_total = intensity + bg)
      
      idx <- which.max(ci$y_total)
      
      pbe <- ci$binding_energy[idx]
      py  <- ci$y_total[idx]
      
      tx <- peaks_init$label_x[i]
      ty <- y_min + peaks_init$label_y_frac[i] * y_rng
      hj <- peaks_init$hjust[i]
      
      p <- p +
        geom_ribbon(
          data = ci,
          aes(binding_energy, ymin = bg, ymax = y_total),
          fill = peaks_init$color[i], alpha = 0.40, colour = NA
        ) +
        geom_line(
          data = ci,
          aes(binding_energy, y_total),
          colour = peaks_init$color[i], linewidth = 0.43
        ) +
        annotate(
          "segment",
          x = tx, y = ty - 0.018 * y_rng,
          xend = pbe, yend = py + 0.008 * y_rng,
          arrow = arrow(length = unit(0.050, "cm"), type = "closed"),
          linewidth = 0.21, colour = "black"
        ) +
        annotate(
          "text",
          x = tx, y = ty,
          label = peaks_init$label[i],
          size = PEAK_LABEL_SIZE,
          family = FIGURE_FONT,
          fontface = "plain",
          lineheight = PEAK_LINEHEIGHT,
          hjust = hj, vjust = 0.5,
          colour = "black"
        )
    }
    
    p <- p +
      geom_line(
        data = envelope,
        aes(binding_energy, fit_total),
        colour = "red", linewidth = 0.76
      )
  }
  
  add_panel_label(p, label)
}


# =============================================================================
# PEAK TABLES — COMPACT LABEL POSITIONS
# Standard scientific notation: Unicode subscripts/superscripts,
# proper sulfate charge and en-dash chemical bonds. Save script as UTF-8.
# =============================================================================

# ---- (a) GP C1s ----
c1s_gp <- tibble(
  center = c(284.8, 286.2, 288.5),
  sigma  = c(0.80, 0.90, 1.00),
  label  = c("C–C/C–H\n284.8", "C–O\n286.2", "O–C=O\n288.5"),
  color  = c("magenta", "orange", "cyan4"),
  label_x = c(283.9, 286.6, 289.2),
  label_y_frac = c(1.03, 0.86, 1.02),
  hjust = c(0, 0.5, 1)
)

# ---- (b) PEG C1s ----
c1s_peg <- tibble(
  center = c(284.8, 285.9, 286.6, 288.5),
  sigma  = c(0.75, 0.80, 0.85, 1.00),
  label  = c("C–C/C–H\n284.8", "C–O\n285.9", "C–O–C\n286.6", "O–C=O\n288.5"),
  color  = c("magenta", "orange", "green3", "cyan4"),
  label_x = c(283.1, 284.2, 287.7, 290.8),
  label_y_frac = c(1.03, 0.84, 0.93, 1.03),
  hjust = c(0, 0, 1, 1)
)

# ---- (c) GP O1s ----
o1s_gp <- tibble(
  center = c(531.5, 532.5, 533.3),
  sigma  = c(1.00, 1.00, 1.00),
  label  = c("SO₄²⁻\n531.5", "O–H\n532.5", "H₂O\n533.3"),
  color  = c("blue", "green3", "magenta"),
  label_x = c(529.8, 533.0, 535.0),
  label_y_frac = c(0.91, 1.10, 0.77),
  hjust = c(0, 1, 1)
)

# ---- (d) PEG O1s ----
o1s_peg <- tibble(
  center = c(530.8, 531.7, 532.5, 533.2),
  sigma  = c(0.90, 1.00, 1.00, 1.00),
  label  = c("P–O\n530.8", "SO₄²⁻\n531.7", "C–O–C\n532.5", "H₂O\n533.2"),
  color  = c("green3", "blue", "orange", "magenta"),
  label_x = c(535.8, 529.2, 533.0, 536.0),
  label_y_frac = c(0.65, 0.93, 1.10, 0.90),
  hjust = c(1, 0, 1, 1)
)

# ---- (e) GP Ca2p ----
ca2p_gp <- tibble(
  center = c(347.9, 351.4),
  sigma  = c(0.90, 0.90),
  label  = c("Ca 2p₃/₂\n347.9", "Ca 2p₁/₂\n351.4"),
  color  = c("red", "magenta"),
  label_x = c(347.2, 352.0),
  label_y_frac = c(1.03, 0.87),
  hjust = c(0, 1)
)


# =============================================================================
# BUILD PANELS — bold lowercase tags
# =============================================================================

pa <- make_decon_panel(
  sheet_name = "C1s Scan", sample_name = "GP",
  peaks_init = c1s_gp, label = "(a)", show_x_title = FALSE
)

pb <- make_decon_panel(
  sheet_name = "C1s Scan", sample_name = "PEG",
  peaks_init = c1s_peg, label = "(b)", show_x_title = FALSE
)

pc <- make_decon_panel(
  sheet_name = "O1s Scan", sample_name = "GP",
  peaks_init = o1s_gp, label = "(c)", show_x_title = FALSE
)

pd <- make_decon_panel(
  sheet_name = "O1s Scan", sample_name = "PEG",
  peaks_init = o1s_peg, label = "(d)", show_x_title = FALSE
)

pe <- make_decon_panel(
  sheet_name = "Ca2p Scan", sample_name = "GP",
  peaks_init = ca2p_gp, label = "(e)", show_x_title = TRUE
)


# =============================================================================
# COMPACT FINAL LAYOUT
# =============================================================================

layout_design <- "
ab
cd
ee
"

fig_pub <- wrap_plots(
  a = pa,
  b = pb,
  c = pc,
  d = pd,
  e = pe,
  design  = layout_design,
  heights = c(1, 1, 0.72)
) &
  theme(
    plot.background = element_rect(fill = "white", colour = NA)
  )


# =============================================================================
# SCAN SUMMARY
# =============================================================================

scan_summary <- scan_data %>%
  filter(
    (sample == "GP"  & sheet == "C1s Scan") |
      (sample == "PEG" & sheet == "C1s Scan") |
      (sample == "GP"  & sheet == "O1s Scan") |
      (sample == "PEG" & sheet == "O1s Scan") |
      (sample == "GP"  & sheet == "Ca2p Scan")
  ) %>%
  group_by(sample, sheet) %>%
  summarise(
    n_points = n(),
    binding_energy_min = min(binding_energy, na.rm = TRUE),
    binding_energy_max = max(binding_energy, na.rm = TRUE),
    counts_min = min(counts, na.rm = TRUE),
    counts_max = max(counts, na.rm = TRUE),
    counts_mean = mean(counts, na.rm = TRUE),
    counts_sd   = sd(counts, na.rm = TRUE),
    background_available = any(is.finite(backgnd_counts)),
    .groups = "drop"
  )


# =============================================================================
# SAVE FIGURE — NATURE COMMUNICATIONS / PORTRAIT WORD
# =============================================================================
# Insert the Word version at 15.5 cm width (do not resize after insertion).
# Both export sizes retain the same aspect ratio and unchanged annotation
# coordinates, peak positions, arrow geometry, and panel layout.
# Line figures: vector PDF recommended for final journal artwork.
# TIFF and PNG are provided for Word / supplementary-document assembly.

WORD_W_CM <- 15.5
WORD_H_CM <- 18.0
NC_W_CM <- 18.0
NC_H_CM <- WORD_H_CM * NC_W_CM / WORD_W_CM

out_png <- file.path(out_dir, "Fig_S3_XPS_Word_Portrait_600dpi.png")
out_tif <- file.path(out_dir, "Fig_S3_XPS_Word_Portrait_600dpi.tiff")
out_pdf <- file.path(out_dir, "Fig_S3_XPS_NatureCommunications_Vector.pdf")

ggsave(
  filename = out_png,
  plot     = fig_pub,
  width    = WORD_W_CM,
  height   = WORD_H_CM,
  units    = "cm",
  dpi      = 600,
  bg       = "white",
  limitsize = FALSE
)

ggsave(
  filename = out_tif,
  plot     = fig_pub,
  width    = WORD_W_CM,
  height   = WORD_H_CM,
  units    = "cm",
  dpi      = 600,
  compression = "lzw",
  bg       = "white",
  limitsize = FALSE
)

ggsave(
  filename = out_pdf,
  plot     = fig_pub,
  width    = NC_W_CM,
  height   = NC_H_CM,
  units    = "cm",
  device   = if (capabilities("cairo")) cairo_pdf else "pdf",
  bg       = "white",
  limitsize = FALSE
)


# =============================================================================
# SAVE STATISTICS
# =============================================================================

stats_file <- file.path(out_dir, "Fig_S3_XPS_Fit_Statistics.xlsx")

wb <- createWorkbook()

add_sheet <- function(wb, sheet_name, data) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, data)
  freezePane(wb, sheet_name, firstRow = TRUE)
  setColWidths(wb, sheet_name, cols = 1:max(1, ncol(data)), widths = "auto")
}

add_sheet(wb, "Scan_summary",   scan_summary)
add_sheet(wb, "Fit_quality",    fit_quality_store)
add_sheet(wb, "Fit_components", fit_components_store)

saveWorkbook(wb, stats_file, overwrite = TRUE)


# =============================================================================
# DONE
# =============================================================================

message("\n======================================================")
message("DONE")
message("======================================================")

message("\nPanels:")
message("(a) = GP C1s")
message("(b) = PEG C1s")
message("(c) = GP O1s")
message("(d) = PEG O1s")
message("(e) = GP Ca2p")

message("\nAxis settings:")
message("Y-axis titles shown")
message("Y-axis numerical values removed")
message("X-axis binding-energy values shown in (a)-(e)")
message("Binding Energy (eV) title shown only in (e)")

message("\nWord PNG saved: ", out_png)
message("Word TIFF saved: ", out_tif)
message("NC vector PDF saved: ", out_pdf)
message("Fit statistics saved: ", stats_file)
message("\nWord portrait figure: ", WORD_W_CM, " x ", WORD_H_CM, " cm")
message("NC vector size: ", round(NC_W_CM, 2), " x ", round(NC_H_CM, 2), " cm")
message("Raster resolution: 600 dpi")
message("All peak input centres, label positions, and arrow geometry preserved.")