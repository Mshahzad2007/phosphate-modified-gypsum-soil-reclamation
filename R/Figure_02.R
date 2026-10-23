### 21/10/26
rm(list = ls())
graphics.off()
cat("\014")

# ---------------------------------------------------------------------------
# EXECUTION AND OUTPUT CHECK — use Source to run this WHOLE file in RStudio.
# Run getwd() in the R Console to see where input/output files are located.
# ---------------------------------------------------------------------------
FIG2_SCRIPT_VERSION <- "2026-10-21 teacher five-panel design; 2 P components and 3 labeled Ca components (complete paired model)"
message("\n============================================================")
message("FIGURE 2 SCRIPT: ", FIG2_SCRIPT_VERSION)
message("Current R working directory: ", normalizePath(getwd(), winslash = "/"))
message("IMPORTANT: Run the full R file with Source; not only selected lines.")
message("============================================================\n")

# ============================================================================
# FIGURE 2 — FINAL FIVE-PANEL VERSION
# (a) XRD                     (b) XPS survey
# (c) P 2p high-resolution XPS (d) Ca 2p high-resolution XPS
# (e) EDS elemental composition (GP and PEG; full width)
#
# Layout: 180 mm wide x 155 mm high; three rows (2 / 2 / 1).
# Original measurements and the original five-panel design are preserved.
# XPS peak fitting is revised to linked physical spin-orbit doublets.
# Instrument backgrounds are used when available; otherwise a Shirley estimate.
# Model-derived energies are used in figure annotations; no hardcoded peak eV.
# XPS charge referencing must be verified with the facility before submission.
# ============================================================================

# =============================================================================
# ORIGINAL XPS ANNOTATION STYLE — NC PORTRAIT ASSEMBLY
# This version preserves the original code's annotation MECHANISM:
# black arrows from peak text to the fitted apex of EACH spin-orbit component.
# Original panel positions, panel tags, colours and arrow style are preserved.
# Only chemically constrained fit components, their labels, and source-data
# representation change. Do not interpret the one-doublet Ca fit as a unique
# calcium-phosphate species assignment.
# =============================================================================

# ----------------------------------------------------------------------------
# PACKAGES
# ----------------------------------------------------------------------------
pkgs <- c(
  "readxl", "dplyr", "ggplot2", "purrr", "janitor", "patchwork",
  "minpack.lm", "tibble", "tidyr", "scales", "grid", "stringr", "openxlsx"
)

missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(purrr)
  library(janitor)
  library(patchwork)
  library(minpack.lm)
  library(tibble)
  library(tidyr)
  library(scales)
  library(grid)
  library(stringr)
})

# ----------------------------------------------------------------------------
# INPUT FILES
# Put these files in your working directory
# ----------------------------------------------------------------------------
xps_gp_file   <- "XPS_GP.xls"
xps_peg_file  <- "XPS_PEG.xls"
xrd_file      <- "PEG.txt"
eds_file      <- "SEM_PEG.xlsx"

# All four numerical input files are mandatory; no image inputs are needed.
required_inputs <- c(xps_gp_file, xps_peg_file, xrd_file, eds_file)
missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs)) {
  stop(
    "Figure 2 cannot run: required source file(s) not found: ",
    paste(missing_inputs, collapse = ", "), "\n",
    "Current working directory: ", normalizePath(getwd(), winslash = "/"), "\n",
    "Put the original input files here, or setwd() to their folder, ",
    "then SOURCE THE ENTIRE SCRIPT again."
  )
}

# ----------------------------------------------------------------------------
# OUTPUT
# ----------------------------------------------------------------------------
# Use an explicit, ABSOLUTE output path; figure and Excel save together.
# To change location, change project_dir (the folder holding the input files).
project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
out_dir <- file.path(project_dir, "Final_XRD_XPS_EDS_Figure_NC")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
if (!dir.exists(out_dir)) stop("Unable to create output folder: ", out_dir)
message("FIGURE 2 OUTPUT FOLDER: ", out_dir)
message("EXPECTED SOURCE DATA: ", file.path(out_dir, "Source_Data_Fig2_Teacher_3CaModel.xlsx"))

# Run-status file helps diagnose errors that stop before saveWorkbook().
run_status_path <- file.path(out_dir, "FIG2_RUN_STATUS.txt")
writeLines(c(
  paste("Script version:", FIG2_SCRIPT_VERSION),
  paste("Working directory:", project_dir),
  paste("Output directory:", out_dir),
  "STATUS: RUNNING. Figure, caption, and source-data export are in progress."
), run_status_path)


# Final five-panel layout; no EDS/SEM images are required.
FIG_WIDTH_MM  <- 180
FIG_HEIGHT_MM <- 155
FIG_DPI       <- 600
figure_prefix <- "Fig2_NC_FINAL_5PANELS_Teacher_3CaModel"

# ----------------------------------------------------------------------------
# GLOBAL STYLE
# ----------------------------------------------------------------------------
FONT_FAMILY <- "Arial"  # On Windows; Helvetica is also journal-acceptable.
BS          <- 10.8   # Final-size axis/tick lettering, increased by 20% for clarity
TAG_SIZE    <- 12.0   # Bold lowercase panel identifiers, increased by 20%
PEAK_SIZE   <- 3.36   # ggplot annotation lettering (mm), increased by 20%

# Accessible, non-red/green-dependent reference-pattern colours.
phase_cols <- c(
  "CaSO4_2H2O"      = "#0072B2",
  "CaSO4_05H2O"     = "#56B4E9",
  "SiO2"            = "#009E73",
  "Brushite"        = "#CC79A7",
  "Ca3PO4"          = "#D55E00",
  "CaH2PO4_2H2O"    = "#E69F00"
)

elem_cols <- c(
  "Ca" = "#2166AC",
  "S"  = "#D55E00",
  "P"  = "#7B3294",
  "F"  = "#E69F00",
  "Si" = "#4D4D4D",
  "Fe" = "#4D4D4D",
  "Mg" = "#CC79A7",
  "Al" = "#A6CEE3",
  "O"  = "#FB9A99",
  "C"  = "#FDBF6F",
  "Na" = "#1B9E77",
  "Cl" = "#7570B3",
  "K"  = "#E7298A"
)

get_elem_col <- function(x) {
  ifelse(x %in% names(elem_cols), elem_cols[x], "grey70")
}

pub_theme <- function() {
  theme_classic(base_size = BS, base_family = FONT_FAMILY) +
    theme(
      text              = element_text(family = FONT_FAMILY, face = "plain", colour = "black"),
      axis.line         = element_line(colour = "black", linewidth = 0.40),
      axis.ticks        = element_line(colour = "black", linewidth = 0.38),
      axis.ticks.length = unit(2.0, "pt"),
      axis.text         = element_text(colour = "black", size = BS, face = "plain"),
      axis.title        = element_text(colour = "black", size = BS + 0.4, face = "plain"),
      panel.grid        = element_blank(),
      panel.background  = element_rect(fill = "white", colour = NA),
      plot.background   = element_rect(fill = "white", colour = NA),
      plot.title        = element_blank(),
      plot.margin       = margin(22, 5, 5, 7, unit = "pt"),  # reserve headroom for tags and top y ticks
      legend.position   = "none"
    )
}

# Put panel identifiers (a), (b), (c), (d), (f), (g) in their own
# dedicated header line ABOVE the corresponding plot panel. This is more robust
# than plot.tag.position y > 1, which can clip or collide with y-axis tick labels.
# The only content rendered in the header is the bold panel letter; this does
# not add a figure title. Panel (e) retains its existing tag placement.
tag_panel <- function(p, tag) {
  if (tag == "(e)") {
    return(
      p +
        labs(tag = tag) +
        theme(
          plot.tag = element_text(
            family = FONT_FAMILY,
            size = TAG_SIZE,
            face = "bold",
            colour = "black",
            hjust = 0,
            vjust = 1
          ),
          plot.tag.position = c(0.01, 0.99)
        )
    )
  }
  
  p +
    labs(title = tag, tag = NULL) +
    theme(
      # Plot-title slot is used ONLY as a nonoverlapping panel-label header.
      plot.title = element_text(
        family = FONT_FAMILY,
        size = TAG_SIZE,
        face = "bold",
        colour = "black",
        hjust = 0,
        vjust = 1,
        margin = margin(b = 4, unit = "pt")
      ),
      plot.title.position = "plot",
      plot.tag = element_blank(),
      plot.margin = margin(6, 5, 5, 7, unit = "pt")
    )
}

# ============================================================================
# XPS HELPERS
# ============================================================================
shirley_bg <- function(counts, tol = 1e-6, max_iter = 200) {
  n <- length(counts)
  bg <- numeric(n)
  
  for (k in seq_len(max_iter)) {
    bg_old <- bg
    area <- cumsum(counts - bg)
    bg <- counts[1] + (counts[n] - counts[1]) * area / max(area[n], 1e-10)
    if (max(abs(bg - bg_old)) < tol) break
  }
  
  bg
}

# Physical, area-normalized pseudo-Voigt component (integrated area = 1).
# Mixed Gaussian/Lorentzian fractions share the same full width at half max.
pv_kernel <- function(x, mu, fwhm, eta) {
  g <- sqrt(4 * log(2) / pi) / fwhm *
    exp(-4 * log(2) * (x - mu)^2 / fwhm^2)
  l <- (fwhm / (2 * pi)) / ((x - mu)^2 + (fwhm / 2)^2)
  (1 - eta) * g + eta * l
}

# P 2p: one complete doublet (two shaded components).
# Ca 2p: TWO complete doublets for EXPLORATORY peak-shape decomposition.
# The latter mathematically contains four peaks; the weaker secondary 2p1/2
# partner is included in the full fit but is drawn faintly to maintain the
# approved three-prominent-component panel appearance.
# This extra Ca doublet is NOT proof of a phosphate-specific Ca chemical state:
# similar secondary fitted contributions arise for unmodified gypsum (GP).
# Area ratio 2:1 and shared widths WITHIN each doublet; common Ca split,
# common pseudo-Voigt mixing between all four Ca components.
fit_linked_doublet <- function(be, net, region) {
  stopifnot(region %in% c("P2p Scan", "Ca2p Scan"))
  if (region == "P2p Scan") {
    formula <- net ~ A * (pv_kernel(x, mu, fwhm, eta) +
                            0.5 * pv_kernel(x, mu + 0.86, fwhm, eta))
    lower <- c(A = 0, mu = 133.0, fwhm = 0.35, eta = 0)
    upper <- c(A = Inf, mu = 135.5, fwhm = 4.0, eta = 1)
    center0 <- 134.2
  } else {
    # Complete primary + secondary Ca doublets: four components in the FIT.
    # secondary_3/2 = mu - offset ; secondary_1/2 = mu - offset + delta
    formula <- net ~ A * (pv_kernel(x, mu, fwhm, eta) +
                            0.5 * pv_kernel(x, mu + delta, fwhm, eta)) +
      A2 * (pv_kernel(x, mu - offset, fwhm2, eta) +
              0.5 * pv_kernel(x, mu - offset + delta, fwhm2, eta))
    lower <- c(A = 0, mu = 347.0, fwhm = 0.35, eta = 0,
               delta = 3.40, A2 = 0, offset = 0.25, fwhm2 = 0.35)
    upper <- c(A = Inf, mu = 349.0, fwhm = 4.0, eta = 1,
               delta = 3.70, A2 = Inf, offset = 1.25, fwhm2 = 4.0)
    center0 <- 347.8
  }
  trials <- list()
  k <- 0L
  for (width0 in c(1.1, 1.7, 2.3)) {
    for (eta0 in c(0.20, 0.50, 0.80)) {
      offsets <- if (region == "Ca2p Scan") c(0.35, 0.70, 1.00) else 0
      for (offset0 in offsets) {
        start <- list(A = max(net, na.rm = TRUE) * width0 * 1.5,
                      mu = center0, fwhm = width0, eta = eta0)
        if (region == "Ca2p Scan") {
          start$delta <- 3.53
          start$A2 <- max(net, na.rm = TRUE) * width0 * 0.18
          start$offset <- offset0
          start$fwhm2 <- min(2.5, width0 * 1.3)
        }
        fit <- tryCatch(
          minpack.lm::nlsLM(
            formula, data = data.frame(x = be, net = net), start = start,
            lower = lower, upper = upper,
            control = minpack.lm::nls.lm.control(maxiter = 1200)
          ), error = function(e) NULL
        )
        if (!is.null(fit)) {
          k <- k + 1L
          trials[[k]] <- list(fit = fit, rss = sum(residuals(fit)^2))
        }
      }
    }
  }
  if (!length(trials)) stop("Spin-orbit-constrained XPS fit did not converge for ", region)
  best <- trials[[which.min(vapply(trials, function(t) t$rss, numeric(1)))]]$fit
  co <- coef(best)
  delta <- if (region == "P2p Scan") 0.86 else unname(co[["delta"]])
  pred <- as.numeric(predict(best))
  r2 <- 1 - sum((net - pred)^2) / sum((net - mean(net))^2)
  result <- list(
    model = best, region = region, mu = unname(co[["mu"]]),
    delta = delta, fwhm = unname(co[["fwhm"]]),
    eta = unname(co[["eta"]]), area_3_2 = unname(co[["A"]]),
    r2 = r2, rmse = sqrt(mean((net - pred)^2)),
    second_mu = if (region == "Ca2p Scan") unname(co[["mu"]] - co[["offset"]]) else NA_real_,
    second_fwhm = if (region == "Ca2p Scan") unname(co[["fwhm2"]]) else NA_real_,
    second_area_3_2 = if (region == "Ca2p Scan") unname(co[["A2"]]) else NA_real_,
    second_area_fraction = if (region == "Ca2p Scan")
      unname(co[["A2"]] / (co[["A"]] + co[["A2"]])) else NA_real_
  )
  if (!is.finite(result$r2)) stop("Nonfinite XPS fit quality for ", region)
  if (result$r2 < 0.990) warning("Review XPS residuals: ", region,
                                 "; constrained R2=", signif(result$r2, 4))
  message(region, ": main 2p3/2=", sprintf("%.3f", result$mu),
          "; 2p1/2=", sprintf("%.3f", result$mu + result$delta),
          if (region == "Ca2p Scan") paste0(
            "; exploratory secondary 2p3/2=", sprintf("%.3f", result$second_mu)) else "",
          "; R2=", sprintf("%.5f", result$r2))
  result
}

# Components excluding background, with fixed identifiers.
# P: index 1 = 2p3/2; 2 = 2p1/2.
# Ca: 1 = main 2p1/2; 2 = main 2p3/2;
#     3 = secondary 2p3/2; 4 = its REQUIRED secondary 2p1/2 partner.
linked_component_curves <- function(be, fitted) {
  a <- fitted$area_3_2
  if (fitted$region == "P2p Scan") {
    return(bind_rows(
      tibble(binding_energy = be,
             intensity = a * pv_kernel(be, fitted$mu, fitted$fwhm, fitted$eta),
             peak_id = 1L),
      tibble(binding_energy = be,
             intensity = (a / 2) * pv_kernel(be, fitted$mu + fitted$delta,
                                             fitted$fwhm, fitted$eta),
             peak_id = 2L)
    ))
  }
  a2 <- fitted$second_area_3_2
  bind_rows(
    tibble(binding_energy = be,
           intensity = (a / 2) * pv_kernel(be, fitted$mu + fitted$delta,
                                           fitted$fwhm, fitted$eta), peak_id = 1L),
    tibble(binding_energy = be,
           intensity = a * pv_kernel(be, fitted$mu, fitted$fwhm, fitted$eta),
           peak_id = 2L),
    tibble(binding_energy = be,
           intensity = a2 * pv_kernel(be, fitted$second_mu,
                                      fitted$second_fwhm, fitted$eta), peak_id = 3L),
    tibble(binding_energy = be,
           intensity = (a2 / 2) * pv_kernel(be, fitted$second_mu + fitted$delta,
                                            fitted$second_fwhm, fitted$eta), peak_id = 4L)
  )
}

# R/Avantage background choices can differ. Prefer the exported instrument
# background when it exists and is plausible; otherwise estimate Shirley.
choose_xps_bg <- function(dat) {
  counts <- dat$counts
  exported <- dat$backgnd_counts
  # Instrument Excel export contains a zero BACKGROUND only at its first/last
  # scan row (missing numeric entry in the workbook), not a zero measured count.
  # Fill these two MISSING background endpoints with nearest exported values.
  # Raw experimental counts and original .xls files are never modified.
  repaired_endpoints <- FALSE
  if (length(exported) == length(counts) && length(exported) >= 3L &&
      all(is.finite(exported))) {
    if (exported[1] == 0 && exported[2] > 0) {
      exported[1] <- exported[2]
      repaired_endpoints <- TRUE
    }
    n <- length(exported)
    if (exported[n] == 0 && exported[n - 1L] > 0) {
      exported[n] <- exported[n - 1L]
      repaired_endpoints <- TRUE
    }
  }
  if (length(exported) == length(counts) && all(is.finite(exported)) &&
      mean(exported <= counts, na.rm = TRUE) > 0.85 &&
      max(exported, na.rm = TRUE) > 0) {
    bg_method <- if (repaired_endpoints)
      "Instrument-exported; missing zero endpoints interpolated" else "Instrument-exported"
    return(list(bg = as.numeric(exported), method = bg_method))
  }
  list(bg = shirley_bg(counts), method = "Calculated Shirley")
}

xps_fit_store <- list()
xps_bg_store <- list()

# ==========================================================================
# ============================================================================
# LOAD XPS DATA
# ============================================================================
xps_sheets <- c("XPS Survey", "C1s Scan", "O1s Scan", "Ca2p Scan", "P2p Scan")

read_xps_file <- function(file, sname) {
  if (!file.exists(file)) stop("Missing XPS file: ", file)
  
  map_dfr(xps_sheets, function(sh) {
    x <- tryCatch(
      read_excel(file, sheet = sh, .name_repair = "minimal"),
      error = function(e) NULL
    )
    
    if (is.null(x) || ncol(x) < 2) return(NULL)
    
    x <- x %>%
      clean_names() %>%
      select(where(~ !all(is.na(.))))
    
    nc <- names(x)[sapply(x, function(col) {
      suppressWarnings(!all(is.na(as.numeric(col))))
    })]
    
    if (length(nc) < 2) return(NULL)
    
    # These files export Binding Energy, Counts and often Backgnd Counts.
    # The third column is NOT treated as a third spectral signal.
    bg_name <- names(x)[grepl("backgnd|background|bg_counts", names(x))]
    bg_values <- if (length(bg_name)) suppressWarnings(as.numeric(x[[bg_name[1]]]))
    else rep(NA_real_, nrow(x))
    tibble(
      binding_energy = suppressWarnings(as.numeric(x[[nc[1]]])),
      counts         = suppressWarnings(as.numeric(x[[nc[2]]])),
      backgnd_counts = bg_values,
      sample         = sname,
      sheet          = sh
    ) %>%
      filter(is.finite(binding_energy), is.finite(counts))
  })
}

message("Loading XPS data...")

scan_data <- bind_rows(
  read_xps_file(xps_gp_file,  "GP"),
  read_xps_file(xps_peg_file, "PEG")
)

scan_data$sheet  <- factor(scan_data$sheet,  levels = xps_sheets)
scan_data$sample <- factor(scan_data$sample, levels = c("GP", "PEG"))

# ============================================================================
# PANEL A — XRD
# ============================================================================
read_xrd <- function(file, sname) {
  if (!file.exists(file)) stop("Missing XRD file: ", file)
  
  x <- read.table(file, header = FALSE, comment.char = "#", fill = TRUE)
  
  if (ncol(x) < 2) {
    stop("XRD file must contain at least two columns: 2theta and intensity.")
  }
  
  x[, 1:2] %>%
    setNames(c("two_theta", "intensity")) %>%
    mutate(
      two_theta = suppressWarnings(as.numeric(two_theta)),
      intensity = suppressWarnings(as.numeric(intensity)),
      sample = sname
    ) %>%
    filter(!is.na(two_theta), !is.na(intensity)) %>%
    arrange(two_theta)
}

obs_xrd <- read_xrd(xrd_file, "PEG") %>%
  mutate(y_obs = intensity / max(intensity, na.rm = TRUE))

mkph <- function(id, tt, ri) {
  tibble(
    phase = id,
    two_theta = tt,
    rel_int = ri
  )
}

phase_df <- bind_rows(
  mkph(
    "CaSO4_2H2O",
    c(11.70, 20.80, 23.40, 29.15, 31.10, 33.35, 40.70, 47.85, 50.35),
    c(1.00, 0.12, 0.26, 0.15, 0.06, 0.07, 0.04, 0.05, 0.03)
  ),
  mkph(
    "CaSO4_05H2O",
    c(14.80, 25.50, 29.70, 31.70, 42.40, 55.80),
    c(0.30, 0.45, 1.00, 0.60, 0.20, 0.18)
  ),
  mkph(
    "SiO2",
    c(20.85, 26.65, 36.55, 39.50, 50.15, 59.95),
    c(0.12, 1.00, 0.45, 0.25, 0.20, 0.12)
  ),
  mkph(
    "Brushite",
    c(11.62, 20.95, 23.55, 29.36, 30.50, 34.10, 39.80, 42.00, 48.90),
    c(0.20, 0.28, 0.40, 1.00, 0.55, 0.25, 0.18, 0.16, 0.12)
  ),
  mkph(
    "Ca3PO4",
    c(25.87, 28.96, 31.77, 32.19, 32.90, 34.08, 40.00, 46.67, 49.48),
    c(0.22, 0.18, 0.90, 1.00, 0.78, 0.62, 0.20, 0.18, 0.16)
  ),
  mkph(
    "CaH2PO4_2H2O",
    c(12.05, 21.05, 23.30, 24.25, 29.35, 30.20, 32.05, 34.25, 47.10),
    c(0.38, 0.25, 0.45, 0.30, 1.00, 0.55, 0.35, 0.28, 0.18)
  )
)

yb <- c(
  "CaSO4_2H2O"      = -0.15,
  "CaSO4_05H2O"     = -0.30,
  "SiO2"            = -0.45,
  "Brushite"        = -0.60,
  "Ca3PO4"          = -0.75,
  "CaH2PO4_2H2O"    = -0.90
)

# Reference sticks use their own relative intensity heights. Their 2-theta
# positions and within-phase relative intensities are unchanged; only the
# visual stick height is increased slightly for legibility.
phase_df <- phase_df %>%
  mutate(
    y0 = yb[phase],
    y1 = y0 + rel_int * 0.11
  )

phase_lines <- tibble(
  phase = names(yb),
  y = as.numeric(yb)
)

phase_labels <- tibble(
  phase = names(yb),
  label = c(
    "CaSO₄·2H₂O",
    "CaSO₄·0.5H₂O",
    "SiO₂",
    "CaHPO₄·2H₂O",
    "Ca₃(PO₄)₂",
    "Ca(H₂PO₄)₂·2H₂O"
  ),
  x = rep(43.0, length(yb)),
  y = as.numeric(yb) - 0.080
)

# IMPORTANT:
# Plot reference sticks only in the left/middle region so that no isolated
# coloured peaks appear in the right-side label area.
xrd_ref_cutoff <- 42.4

phase_df_plot <- phase_df %>%
  filter(two_theta <= xrd_ref_cutoff)

# Display the experimental XRD trace on a square-root normalized scale
# to reveal weaker peaks without adding an inset or a broken axis.
obs_xrd_plot <- obs_xrd %>%
  mutate(y_plot = sqrt(pmax(y_obs, 0)))

# Choose a REAL local PEG feature near 43-52 degrees for the arrow endpoint,
# using the square-root display intensity (not the untransformed intensity).
# This restores a short PEG arrow without changing the experimental spectrum.
peg_arrow_point <- obs_xrd_plot %>%
  filter(two_theta >= 43, two_theta <= 52) %>%
  slice_max(order_by = y_plot, n = 1, with_ties = FALSE)

if (nrow(peg_arrow_point) != 1) {
  stop("No PEG XRD data found between 43 and 52 degrees for the arrow.")
}

peg_arrow_x <- peg_arrow_point$two_theta[[1]]
peg_arrow_y <- peg_arrow_point$y_plot[[1]]
peg_label_x <- min(peg_arrow_x + 5.0, 56.0)
peg_label_y <- min(peg_arrow_y + 0.28, 0.78)

p_A <- ggplot() +
  geom_line(
    data = obs_xrd_plot,
    aes(x = two_theta, y = y_plot),
    colour = "black",
    linewidth = 0.62
  ) +
  # Short leader points directly to a measured PEG diffraction feature.
  annotate(
    "segment",
    x = peg_label_x - 0.40,
    y = peg_label_y - 0.025,
    xend = peg_arrow_x + 0.12,
    yend = peg_arrow_y + 0.020,
    colour = "black",
    linewidth = 0.38,
    arrow = grid::arrow(length = grid::unit(0.065, "cm"), type = "closed")
  ) +
  annotate(
    "text",
    x = peg_label_x,
    y = peg_label_y + 0.018,
    label = "PEG",
    hjust = 0,
    vjust = 0,
    size = 3.0,
    colour = "black",
    fontface = "plain",
    family = FONT_FAMILY
  ) +
  geom_segment(
    data = phase_lines,
    aes(x = 10, xend = xrd_ref_cutoff, y = y, yend = y, colour = phase),
    linewidth = 0.38,
    show.legend = FALSE
  ) +
  geom_segment(
    data = phase_df_plot,
    aes(x = two_theta, xend = two_theta, y = y0, yend = y1, colour = phase),
    linewidth = 0.38,
    show.legend = FALSE
  ) +
  # White label backgrounds mask the underlying reference-line region.
  geom_label(
    data = phase_labels,
    aes(x = x, y = y, label = label, colour = phase),
    hjust = 0,
    size = 3.0,
    fontface = "plain",
    family = FONT_FAMILY,
    fill = "white",
    label.size = 0,
    label.padding = grid::unit(0.035, "lines"),
    label.r = grid::unit(0.02, "lines"),
    show.legend = FALSE
  ) +
  scale_colour_manual(values = phase_cols) +
  scale_x_continuous(
    breaks = seq(10, 60, 10),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_y_continuous(
    breaks = c(0, 0.5, 1.0),
    labels = c("0", "0.5", "1.0")
  ) +
  coord_cartesian(
    xlim = c(10, 60),
    ylim = c(-1.08, 1.02),
    clip = "on"
  ) +
  # The square-root normalization is explained in the caption, not on the axis.
  labs(
    x = expression(2*theta~"(°)"),
    y = NULL
  ) +
  pub_theme() +
  theme(axis.title.y = element_blank())

# ============================================================================
# PANEL B — XPS SURVEY
# ============================================================================
make_survey_panel <- function() {
  dat <- scan_data %>%
    filter(sheet == "XPS Survey") %>%
    arrange(desc(binding_energy)) %>%
    group_by(sample) %>%
    mutate(
      y = (counts - min(counts, na.rm = TRUE)) /
        (max(counts, na.rm = TRUE) - min(counts, na.rm = TRUE))
    ) %>%
    ungroup()
  
  # Annotation coordinates in BE (x) and normalized intensity (y).
  # Adjacent Ca/C and S/P lines have deliberately separated LABEL positions.
  # Leaders originate below their labels, never behind the text.
  pk <- tribble(
    ~be,    ~el,      ~label_be, ~label_y,
    531.3,  "O 1s",      530,      0.89,
    347.0,  "Ca 2p",     432,      0.57,
    284.5,  "C 1s",      271,      0.75,
    168.0,  "S 2p",      183,      0.40,
    133.0,  "P 2p",       62,      0.26
  )
  
  ref <- dat %>% filter(sample == "PEG")
  pk <- pk %>%
    rowwise() %>%
    mutate(
      snap_be = ref$binding_energy[which.min(abs(ref$binding_energy - be))],
      peak_y  = ref$y[which.min(abs(ref$binding_energy - be))]
    ) %>%
    ungroup()
  
  ggplot(
    dat,
    aes(x = binding_energy, y = y, colour = sample,
        linetype = sample, linewidth = sample)
  ) +
    geom_line() +
    scale_x_reverse(breaks = seq(0, 1200, 300)) +
    scale_y_continuous(
      limits = c(0, max(1.03, max(dat$y, na.rm = TRUE) * 1.06)),
      breaks = c(0, 0.5, 1.0)
    ) +
    scale_colour_manual(values = c("GP" = "grey40", "PEG" = "#2166AC")) +
    scale_linetype_manual(values = c("GP" = "solid", "PEG" = "solid")) +
    scale_linewidth_manual(values = c("GP" = 0.45, "PEG" = 0.85)) +
    labs(x = "Binding Energy (eV)", y = NULL) +
    pub_theme() +
    theme(
      legend.position = c(0.18, 0.87),
      legend.text = element_text(size = 9, face = "plain"),
      legend.key.size = unit(9, "pt"),
      legend.background = element_rect(
        fill = scales::alpha("white", 0.86), colour = NA
      )
    ) +
    guides(
      colour = guide_legend(
        title = NULL,
        override.aes = list(linewidth = c(0.55, 0.85))
      ),
      linetype = "none", linewidth = "none"
    ) +
    # Leaders stop at measured peak positions; they do not cross label boxes.
    geom_segment(
      data = pk,
      aes(x = label_be, y = label_y - 0.06,
          xend = snap_be, yend = peak_y + 0.016),
      inherit.aes = FALSE,
      colour = "black", linewidth = 0.33,
      arrow = arrow(length = unit(0.045, "cm"), type = "closed")
    ) +
    geom_label(
      data = pk,
      aes(x = label_be, y = label_y, label = el),
      inherit.aes = FALSE,
      size = 2.85,
      colour = "black", fill = "white", label.size = 0.22,
      family = FONT_FAMILY,
      label.padding = unit(0.055, "lines"),
      fontface = "plain"
    )
}

p_B <- make_survey_panel()

# ============================================================================
# PANELS C AND D — HIGH-RESOLUTION XPS; COMPONENT-SPECIFIC ANNOTATIONS
# ============================================================================
# Original figure colours and label positions remain; peak assignments/energies
# are now taken from chemically linked, fitted doublets.
# Each black leader ends at the apex of its OWN fitted component
# (not on the aggregate fitted envelope).
#
# XPS BE axis runs high -> low from left to right. As in the source code:
#   lx / ly_mul: annotation text position, with ly_mul as a fraction of ymax
#   hjust: text alignment
#   each arrow endpoint is obtained from the fitted component maximum.
# Panel (c) annotation x positions are slightly adjusted for zoomed axis.
# Initial energies are STARTING GUESSES ONLY. Plot annotations are created
# from the fitted energies, not these initial values.
p2p_peg <- tibble(
  center = c(134.2, 135.06), sigma = c(0.85, 0.85),
  color = c("#1F78B4", "#0097A7"),
  lx = c(130.95, 138.50), ly_mul = c(0.37, 0.54),
  hjust = c(0.5, 0.5), lead_x = c(132.12, 136.20),
  lead_y_mul = c(0.28, 0.77)
)

# Restore the ORIGINAL THREE prominent labelled Ca components and colours.
# A FOURTH faint component (required spin-orbit partner of #3) is modelled,
# plotted unobtrusively, and kept in the complete fit and Source Data.
# Additional Ca component is descriptive/tentative, NOT assigned to Ca-phosphate.
ca2p_peg <- tibble(
  center = c(351.3, 347.8, 347.5, 351.05),
  sigma = c(0.70, 0.70, 1.20, 1.20),
  color = c("#6A1B9A", "#E31A1C", "#FF7F00", "#FFCF9B"),
  lx = c(356.1, 343.9, 346.2, NA_real_),
  ly_mul = c(0.55, 0.92, 0.36, NA_real_),
  hjust = c(0.0, 0.0, 0.0, 0.0),
  show_label = c(TRUE, TRUE, TRUE, FALSE),
  ribbon_alpha = c(0.35, 0.35, 0.35, 0.075),
  line_alpha = c(1, 1, 1, 0.20)
)

xps_component_label <- function(region, fitted, i) {
  if (region == "P2p Scan") {
    e <- if (i == 1) fitted$mu else fitted$mu + fitted$delta
    state <- if (i == 1) "PO₄³⁻–P 2p₃/₂" else "PO₄³⁻–P 2p₁/₂"
    return(sprintf("%s\n%.2f eV", state, e))
  }
  e <- switch(as.character(i),
              "1" = fitted$mu + fitted$delta,
              "2" = fitted$mu,
              "3" = fitted$second_mu,
              "4" = fitted$second_mu + fitted$delta)
  state <- switch(as.character(i),
                  "1" = "Ca 2p₁/₂",
                  "2" = "Ca 2p₃/₂",
                  "3" = "Ca 2p₃/₂ (II)*",
                  "4" = "Ca 2p₁/₂ (II)*")
  sprintf("%s\n%.2f eV", state, e)
}

make_decon_panel <- function(sheet_name, sample_name, peaks_init, ylab = TRUE) {
  dat <- scan_data %>%
    filter(sheet == sheet_name, sample == sample_name) %>%
    arrange(desc(binding_energy))
  
  if (nrow(dat) == 0) {
    return(
      ggplot() +
        annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = paste("No data:", sheet_name),
          size = 3
        ) +
        theme_void()
    )
  }
  
  be     <- dat$binding_energy
  counts <- dat$counts
  bg_info <- choose_xps_bg(dat)
  bg <- bg_info$bg
  net <- pmax(counts - bg, 0)
  fit <- fit_linked_doublet(be, net, sheet_name)
  crv <- linked_component_curves(be, fit)
  xps_fit_store[[as.character(sheet_name)]] <<- c(fit, list(background_method = bg_info$method))
  xps_bg_store[[as.character(sheet_name)]] <<- bg
  
  dat2  <- tibble(
    binding_energy = be,
    counts_scaled = counts / 1e4
  )
  
  bg_df <- tibble(
    binding_energy = be,
    bg = bg,
    bg_scaled = bg / 1e4
  )
  
  p <- ggplot() +
    geom_point(
      data = dat2,
      aes(x = binding_energy, y = counts_scaled),
      shape = 16,
      size = 0.42,
      colour = "black"
    ) +
    geom_line(
      data = bg_df,
      aes(x = binding_energy, y = bg_scaled),
      colour = "#1565C0",
      linewidth = 0.38,
      linetype = "dashed"
    ) +
    scale_x_reverse() +
    scale_y_continuous(
      labels = label_number(accuracy = 1),
      expand = expansion(mult = c(0.02, 0.18))
    ) +
    labs(
      x = "Binding Energy (eV)",
      y = NULL
    ) +
    pub_theme()
  
  if (!is.null(crv)) {
    crv2 <- crv %>%
      left_join(bg_df, by = "binding_energy") %>%
      mutate(
        y_total_scaled = (intensity + bg) / 1e4,
        bg_scaled = bg / 1e4,
        peak_id = as.integer(peak_id)
      )
    
    env <- crv %>%
      group_by(binding_energy) %>%
      summarise(
        fit_net = sum(intensity),
        .groups = "drop"
      ) %>%
      left_join(bg_df, by = "binding_energy") %>%
      mutate(
        fit_total_scaled = (fit_net + bg) / 1e4
      )
    
    # ORIGINAL SOURCE METHOD: an arrow points exactly at the local maximum
    # of its own resolved XPS component (not a chosen arbitrary shoulder).
    visible_labels <- if ("show_label" %in% names(peaks_init))
      which(peaks_init$show_label) else seq_len(nrow(peaks_init))
    label_df <- map_dfr(
      visible_labels,
      function(i) {
        ci <- crv2 %>% filter(peak_id == i)
        # Arrow tips follow the actual component maximum, not a background-shifted apex.
        peak_row <- ci[which.max(ci$intensity), ]
        ymax <- max(dat2$counts_scaled, na.rm = TRUE)
        
        tibble(
          peak_id = i,
          px = peak_row$binding_energy,
          py = peak_row$y_total_scaled,
          lx = peaks_init$lx[i],
          ly = ymax * peaks_init$ly_mul[i],
          sx = if ("lead_x" %in% names(peaks_init)) peaks_init$lead_x[i] else peaks_init$lx[i],
          sy = ymax * (if ("lead_y_mul" %in% names(peaks_init)) peaks_init$lead_y_mul[i] else peaks_init$ly_mul[i]),
          label = xps_component_label(sheet_name, fit, i),
          hjust = peaks_init$hjust[i]
        )
      }
    )
    
    # Label energies are now computed from the same constrained fit.
    # No independent annotations can silently diverge from model components.
    
    for (i in seq_len(nrow(peaks_init))) {
      ci <- crv2 %>%
        filter(peak_id == i)
      
      p <- p +
        geom_ribbon(
          data = ci,
          aes(
            x = binding_energy,
            ymin = bg_scaled,
            ymax = y_total_scaled
          ),
          fill = peaks_init$color[i],
          alpha = if ("ribbon_alpha" %in% names(peaks_init))
            peaks_init$ribbon_alpha[i] else 0.35,
          colour = NA
        ) +
        geom_line(
          data = ci,
          aes(
            x = binding_energy,
            y = y_total_scaled
          ),
          colour = peaks_init$color[i],
          alpha = if ("line_alpha" %in% names(peaks_init))
            peaks_init$line_alpha[i] else 1,
          linewidth = 0.38
        )
    }
    
    p <- p +
      geom_line(
        data = env,
        aes(
          x = binding_energy,
          y = fit_total_scaled
        ),
        colour = "#D55E00",
        linewidth = 0.60
      ) +
      # Original code: direct black arrows from the text to fitted peak tops.
      # geom_text follows the segments, preserving the original visual style.
      geom_segment(
        data = label_df,
        aes(x = sx, y = sy, xend = px, yend = py),
        inherit.aes = FALSE,
        colour = "black",
        linewidth = 0.38,
        arrow = arrow(length = unit(0.07, "cm"), type = "closed"),
        show.legend = FALSE
      ) +
      geom_text(
        data = label_df,
        aes(x = lx, y = ly, label = label, hjust = hjust),
        inherit.aes = FALSE,
        size = PEAK_SIZE,
        colour = "black",
        fontface = "plain",
        family = FONT_FAMILY,
        lineheight = 0.88,
        show.legend = FALSE
      )
  }
  
  p
}

p_C <- make_decon_panel(
  "P2p Scan",
  "PEG",
  p2p_peg,
  ylab = TRUE
) +
  # Clip traces and ribbons to panel (c); do not draw them into panel (d).
  coord_cartesian(xlim = c(140.5, 129.8), clip = "on")

p_D <- make_decon_panel(
  "Ca2p Scan",
  "PEG",
  ca2p_peg,
  ylab = TRUE
)

# ============================================================================
# PANEL E — PROPORTIONAL STACKED BARS
# ============================================================================
read_eds <- function(file) {
  if (!file.exists(file)) stop("Missing elemental file: ", file)
  
  sheets <- excel_sheets(file)
  
  map_dfr(sheets, function(sh) {
    d <- read_excel(file, sheet = sh)
    names(d) <- str_trim(names(d))
    
    element_col <- names(d)[1]
    wc <- names(d)[names(d) %in% c("Wt%", "Wt.", "wt%", "Wt", "wt")]
    if (!length(wc)) wc <- names(d)[5]
    
    sid <- suppressWarnings(
      as.numeric(
        str_extract(sh, "\\d+")
      )
    )
    
    d %>%
      filter(
        !is.na(.data[[element_col]]),
        .data[[element_col]] != "Element"
      ) %>%
      transmute(
        Element = as.character(.data[[element_col]]),
        Wt = suppressWarnings(
          as.numeric(.data[[wc[1]]])
        ),
        Spectrum_ID = sid,
        Sample = ifelse(
          !is.na(sid) & sid %in% c(1, 2),
          "GP",
          "PEG"
        )
      ) %>%
      filter(
        !is.na(Wt),
        !is.na(Element),
        nchar(Element) > 0,
        Wt > 0
      )
  })
}

message("Loading elemental composition data...")

eds_raw <- read_eds(eds_file)

eds_mean <- eds_raw %>%
  group_by(Sample, Element) %>%
  summarise(
    Wt = mean(Wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(Wt > 0.05) %>%
  mutate(
    Sample = factor(
      Sample,
      levels = c("GP", "PEG")
    ),
    Element = as.character(Element)
  )

element_order <- c(
  "Ca", "S", "P", "F", "Si", "Mg", "Fe",
  "Al", "O", "C", "Na", "Cl", "K"
)

eds_bar <- eds_mean %>%
  mutate(
    Element = factor(
      Element,
      levels = element_order
    ),
    y_pos = ifelse(
      Sample == "GP",
      1.28,
      0.68
    )
  ) %>%
  arrange(
    Sample,
    Element
  ) %>%
  group_by(Sample) %>%
  mutate(
    total = sum(Wt, na.rm = TRUE),
    pct = 100 * Wt / total,
    xmin = lag(
      cumsum(pct),
      default = 0
    ),
    xmax = cumsum(pct),
    xmid = (xmin + xmax) / 2,
    label = paste0(
      as.character(Element),
      " ",
      sprintf("%.1f", Wt),
      "%"
    ),
    inside = case_when(
      Sample == "GP" &
        Element %in% c("Ca", "S") ~ TRUE,
      
      Sample == "PEG" &
        Element %in% c("Ca", "S", "P") ~ TRUE,
      
      TRUE ~ FALSE
    )
  ) %>%
  ungroup()

# Label all small GP/PEG constituents relevant to Fig. 2e, including
# previously unlabelled GP P and PEG F. Only these two label additions
# are new; bar heights, colours, percentages and figure layout stay fixed.
right_labs <- eds_bar %>%
  filter(
    (Sample == "GP" & Element %in% c("F", "P", "Si")) |
      (Sample == "PEG" & Element %in% c("Mg", "F", "Si"))
  ) %>%
  mutate(
    # Top, middle, bottom label positions avoid overlap at the right edge.
    lab_x = 106,
    lab_y = case_when(
      Sample == "GP"  & Element == "F"  ~ y_pos + 0.14,
      Sample == "GP"  & Element == "P"  ~ y_pos,
      Sample == "GP"  & Element == "Si" ~ y_pos - 0.14,
      Sample == "PEG" & Element == "Mg" ~ y_pos + 0.14,
      Sample == "PEG" & Element == "F"  ~ y_pos,
      Sample == "PEG" & Element == "Si" ~ y_pos - 0.14,
      TRUE ~ y_pos
    )
  )

p_E <- ggplot() +
  geom_text(
    data = distinct(
      eds_bar,
      Sample,
      y_pos
    ),
    aes(
      x = -1.8,
      y = y_pos,
      label = Sample
    ),
    hjust = 1,
    size = 3.2,
    colour = "black",
    fontface = "plain",
    family = FONT_FAMILY
  ) +
  geom_rect(
    data = eds_bar,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = y_pos - 0.13,
      ymax = y_pos + 0.13,
      fill = Element
    ),
    colour = "white",
    linewidth = 0.38
  ) +
  geom_text(
    data = eds_bar %>%
      filter(inside),
    aes(
      x = xmid,
      y = y_pos,
      label = label
    ),
    colour = "white",
    size = 3.0,
    fontface = "plain",
    family = FONT_FAMILY
  ) +
  geom_segment(
    data = right_labs,
    aes(
      x = xmax,
      xend = lab_x - 0.65,
      y = y_pos,
      yend = lab_y
    ),
    linewidth = 0.38,
    colour = "black",
    arrow = arrow(
      length = unit(0.040, "cm"),
      type = "closed"
    )
  ) +
  geom_text(
    data = right_labs,
    aes(
      x = lab_x,
      y = lab_y,
      label = label
    ),
    hjust = 0,
    size = 3.0,
    colour = "black",
    fontface = "plain",
    family = FONT_FAMILY
  ) +
  scale_fill_manual(
    values = vapply(
      levels(eds_bar$Element),
      get_elem_col,
      character(1)
    )
  ) +
  scale_x_continuous(
    limits = c(-10.0, 130.0),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0.35, 1.63),
    expand = c(0, 0)
  ) +
  # The legend/caption (not an internal figure title) defines these as
  # EDS elemental weight percentages. The original Wt% values are unaltered.
  theme_void(base_size = BS, base_family = FONT_FAMILY) +
  theme(
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    plot.margin = margin(
      0,
      3,
      0,
      3
    ),
    legend.position = "none"
  )

# ============================================================================
# FINAL ASSEMBLY — FIVE PANELS ONLY
# ============================================================================
message("Assembling final five-panel Figure 2...")

row_1 <- tag_panel(p_A, "(a)") | tag_panel(p_B, "(b)")
row_2 <- tag_panel(p_C, "(c)") | tag_panel(p_D, "(d)")
row_3 <- tag_panel(p_E, "(e)")

final_fig <- (
  row_1 / row_2 / row_3 +
    plot_layout(heights = c(1.10, 1.10, 0.54))
) &
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

# ============================================================================
# EXPORT — 180 mm x 155 mm, VECTOR PDF + 600-dpi TIFF
# ============================================================================
# Prefer Cairo for PDF because the spectral labels use Unicode subscripts.
if (capabilities("cairo")) {
  pdf_device <- grDevices::cairo_pdf
} else {
  pdf_device <- grDevices::pdf
  warning("Cairo PDF support not available; check all Greek and Unicode labels in the PDF manually.")
}

pdf_path <- file.path(out_dir, paste0(figure_prefix, ".pdf"))
tif_path <- file.path(out_dir, paste0(figure_prefix, "_600dpi.tiff"))
png_path <- file.path(out_dir, paste0(figure_prefix, "_preview.png"))

ggsave(
  filename = pdf_path,
  plot = final_fig,
  device = pdf_device,
  width = FIG_WIDTH_MM,
  height = FIG_HEIGHT_MM,
  units = "mm",
  bg = "white",
  limitsize = FALSE
)

ggsave(
  filename = tif_path,
  plot = final_fig,
  device = "tiff",
  width = FIG_WIDTH_MM,
  height = FIG_HEIGHT_MM,
  units = "mm",
  dpi = FIG_DPI,
  compression = "lzw",
  bg = "white",
  limitsize = FALSE
)

# PNG is just for rapid visual review; use the PDF as the editable figure.
ggsave(
  filename = png_path,
  plot = final_fig,
  device = "png",
  width = FIG_WIDTH_MM,
  height = FIG_HEIGHT_MM,
  units = "mm",
  dpi = 300,
  bg = "white",
  limitsize = FALSE
)

message("Output directory: ", normalizePath(out_dir))
message("PDF: ", pdf_path)
message("TIFF (600 dpi): ", tif_path)
message("PNG preview: ", png_path)

# ============================================================================
# FIGURE 2 — MINIMAL SOURCE DATA EXPORT (PANELS a–e ONLY)
# One worksheet. Three blank rows separate consecutive panel sections.
# Export measured inputs and only the derived values needed to reproduce the
# curves/bars actually displayed. No EDS/SEM images or f/g data are exported.
# ============================================================================

sd_file <- file.path(out_dir, "Source_Data_Fig2_Teacher_3CaModel.xlsx")
sd_sheet <- "Fig 2 Source Data"
sd_wb <- openxlsx::createWorkbook(creator = "Figure 2 Source Data")
openxlsx::addWorksheet(sd_wb, sd_sheet, gridLines = FALSE)

sd_title_style <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 14, textDecoration = "bold",
  fontColour = "#FFFFFF", fgFill = "#17365D", valign = "center"
)
sd_panel_style <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 11, textDecoration = "bold",
  fontColour = "#FFFFFF", fgFill = "#234A71", valign = "center"
)
sd_note_style <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 9, fontColour = "#526579",
  textDecoration = "italic", wrapText = TRUE, valign = "center"
)
sd_header_style <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 9, textDecoration = "bold",
  fontColour = "#243344", fgFill = "#E8F0F7", wrapText = TRUE,
  valign = "center", border = "Bottom", borderColour = "#BECBD8"
)
sd_body_style <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 9, fontColour = "#243344", valign = "center"
)
sd_num_style <- openxlsx::createStyle(numFmt = "0.##########")

# Minimal sufficient numerical data for each panel.
# Retain EVERY POINT actually plotted, not unrelated full instrument exports;
# reductions are achieved by excluding redundant transform/intermediate columns.
# XRD normalization and survey normalization are fully defined in the caption.
sd_xrd <- obs_xrd_plot %>%
  filter(two_theta >= 10, two_theta <= 60) %>%
  transmute(Two_theta_deg = two_theta, Measured_intensity = intensity,
            Sqrt_normalized_intensity_plotted = y_plot)

sd_survey <- scan_data %>%
  filter(sheet == "XPS Survey") %>%
  group_by(sample) %>%
  mutate(Normalized_plotted = (counts - min(counts, na.rm = TRUE)) /
           (max(counts, na.rm = TRUE) - min(counts, na.rm = TRUE))) %>%
  ungroup() %>%
  arrange(sample, desc(binding_energy)) %>%
  transmute(Sample = as.character(sample), Binding_energy_eV = binding_energy,
            Measured_counts = counts, Min_max_normalized_plotted = Normalized_plotted)

# Use EXACTLY the already-fitted models shown in the figure, never run the
# optimizer a second time for Source Data; this prevents accidental divergence.
sd_make_decon_source <- function(sheet_name) {
  dat <- scan_data %>% filter(sheet == sheet_name, sample == "PEG") %>%
    arrange(desc(binding_energy))
  if (!nrow(dat)) stop("Missing PEG XPS scan: ", sheet_name)
  fitted <- xps_fit_store[[sheet_name]]
  if (is.null(fitted)) stop("Plot fitting must precede source export: ", sheet_name)
  be <- dat$binding_energy
  bg <- xps_bg_store[[sheet_name]]
  if (is.null(bg) || length(bg) != length(be)) {
    stop("XPS background missing or different from plotted data for: ", sheet_name)
  }
  comps <- linked_component_curves(be, fitted)
  c1 <- comps %>% filter(peak_id == 1L) %>% pull(intensity)
  c2 <- comps %>% filter(peak_id == 2L) %>% pull(intensity)
  stopifnot(length(c1) == length(be), length(c2) == length(be))
  if (sheet_name == "Ca2p Scan") {
    c3 <- comps %>% filter(peak_id == 3L) %>% pull(intensity)
    c4 <- comps %>% filter(peak_id == 4L) %>% pull(intensity)
    stopifnot(length(c3) == length(be), length(c4) == length(be))
    return(tibble(Binding_energy_eV = be,
                  Measured_counts = dat$counts, Background_counts = bg,
                  Main_2p1_2_net_counts = c1, Main_2p3_2_net_counts = c2,
                  Secondary_2p3_2_net_counts = c3, Secondary_2p1_2_partner_net_counts = c4,
                  Fitted_total_counts = bg + c1 + c2 + c3 + c4))
  }
  tibble(Binding_energy_eV = be, Measured_counts = dat$counts,
         Background_counts = bg, P_2p3_2_net_counts = c1,
         P_2p1_2_net_counts = c2, Fitted_total_counts = bg + c1 + c2)
}

sd_p2p <- sd_make_decon_source("P2p Scan")
sd_ca2p <- sd_make_decon_source("Ca2p Scan")

# EDS input observations and group means reproduce the stacked bars. Keep only
# the elemental measurements used; do not include raw SEM images in Excel.
sd_eds_summary <- eds_bar %>%
  mutate(Sample = as.character(Sample), Element = as.character(Element)) %>%
  transmute(Sample, Element, Mean_Wt_percent = Wt)
sd_eds <- eds_raw %>%
  mutate(Sample = as.character(Sample), Element = as.character(Element)) %>%
  inner_join(sd_eds_summary, by = c("Sample", "Element")) %>%
  transmute(Sample, Spectrum_ID, Element,
            Observed_Wt_percent = Wt, Mean_Wt_percent) %>%
  arrange(factor(Sample, levels = c("GP", "PEG")), Spectrum_ID, Element)

source_panels <- list(
  list(
    tag = "a", title = "XRD — measured PEG pattern",
    note = paste0(
      "Measured intensity within displayed 10–60 degree range. ",
      "Plotted curve uses sqrt(I/max(I across the full input scan)); reference sticks are annotations."
    ),
    data = sd_xrd
  ),
  list(
    tag = "b", title = "XPS survey — GP and PEG",
    note = "Measured GP/PEG counts; each survey is min–max normalized individually for plotting.",
    data = sd_survey
  ),
  list(
    tag = "c", title = "PEG P 2p — high-resolution XPS",
    note = "Measured counts, background, linked pseudo-Voigt component curves and full fitted signal (all native counts; panel displays /10000).",
    data = sd_p2p
  ),
  list(
    tag = "d", title = "PEG Ca 2p — high-resolution XPS",
    note = "Measured counts plus four internally paired Ca 2p component curves and fitted total (native counts). Three main components labeled, fourth partner faintly rendered; no unique chemical-state assignment.",
    data = sd_ca2p
  ),
  list(
    tag = "e", title = "EDS elemental composition — GP and PEG",
    note = paste0(
      "Measured EDS wt% and treatment means used in the stacked bars. ",
      "Plotted segments are percentages of the summed displayed group-mean wt%; no SEM images are included."
    ),
    data = sd_eds
  )
)

max_cols <- max(vapply(source_panels, function(x) ncol(x$data), integer(1)))
openxlsx::mergeCells(sd_wb, sd_sheet, cols = seq_len(max_cols), rows = 1)
openxlsx::writeData(
  sd_wb, sd_sheet,
  "FIGURE 2 | REPRODUCIBLE MINIMAL SOURCE DATA | PANELS (a)–(e)",
  startRow = 1, startCol = 1
)
openxlsx::addStyle(sd_wb, sd_sheet, sd_title_style,
                   rows = 1, cols = seq_len(max_cols), gridExpand = TRUE)
openxlsx::setRowHeights(sd_wb, sd_sheet, rows = 1, heights = 28)
openxlsx::mergeCells(sd_wb, sd_sheet, cols = seq_len(max_cols), rows = 2)
openxlsx::writeData(
  sd_wb, sd_sheet,
  "One worksheet; only displayed-range measured points, necessary fit curves, EDS values and two fit-summary rows. No images.",
  startRow = 2, startCol = 1
)
openxlsx::addStyle(sd_wb, sd_sheet, sd_note_style,
                   rows = 2, cols = seq_len(max_cols), gridExpand = TRUE)
openxlsx::setRowHeights(sd_wb, sd_sheet, rows = 2, heights = 24)

last_used_row <- 2L
panel_sequence <- character()
for (i in seq_along(source_panels)) {
  panel <- source_panels[[i]]
  r <- if (i == 1L) 4L else last_used_row + 4L # exactly 3 blank rows between panels
  dat <- as.data.frame(panel$data, check.names = FALSE)
  
  openxlsx::mergeCells(sd_wb, sd_sheet, cols = seq_len(max_cols), rows = r)
  openxlsx::writeData(sd_wb, sd_sheet,
                      paste0("Fig 2(", panel$tag, ") | ", panel$title),
                      startRow = r, startCol = 1)
  openxlsx::addStyle(sd_wb, sd_sheet, sd_panel_style,
                     rows = r, cols = seq_len(max_cols), gridExpand = TRUE)
  openxlsx::setRowHeights(sd_wb, sd_sheet, rows = r, heights = 24)
  
  openxlsx::mergeCells(sd_wb, sd_sheet, cols = seq_len(max_cols), rows = r + 1L)
  openxlsx::writeData(sd_wb, sd_sheet, panel$note,
                      startRow = r + 1L, startCol = 1)
  openxlsx::addStyle(sd_wb, sd_sheet, sd_note_style,
                     rows = r + 1L, cols = seq_len(max_cols), gridExpand = TRUE)
  openxlsx::setRowHeights(sd_wb, sd_sheet, rows = r + 1L, heights = 30)
  
  header_row <- r + 2L
  openxlsx::writeData(sd_wb, sd_sheet, dat,
                      startRow = header_row, startCol = 1,
                      colNames = TRUE, rowNames = FALSE,
                      headerStyle = sd_header_style)
  openxlsx::setRowHeights(sd_wb, sd_sheet, rows = header_row, heights = 36)
  
  if (nrow(dat) > 0L) {
    data_rows <- seq.int(header_row + 1L, header_row + nrow(dat))
    openxlsx::addStyle(sd_wb, sd_sheet, sd_body_style,
                       rows = data_rows, cols = seq_len(ncol(dat)),
                       gridExpand = TRUE, stack = TRUE)
    numeric_cols <- which(vapply(dat, is.numeric, logical(1)))
    if (length(numeric_cols)) {
      openxlsx::addStyle(sd_wb, sd_sheet, sd_num_style,
                         rows = data_rows, cols = numeric_cols,
                         gridExpand = TRUE, stack = TRUE)
    }
  }
  
  last_used_row <- header_row + nrow(dat)
  panel_sequence <- c(panel_sequence, panel$tag)
}

openxlsx::setColWidths(sd_wb, sd_sheet, cols = seq_len(max_cols), widths = 19)
openxlsx::setColWidths(sd_wb, sd_sheet, cols = 1, widths = 25)
openxlsx::freezePane(sd_wb, sd_sheet, firstActiveRow = 4)
openxlsx::pageSetup(sd_wb, sd_sheet, orientation = "landscape",
                    fitToWidth = 1, fitToHeight = 0)

if (!identical(panel_sequence, letters[1:5])) {
  stop("Source-data panel sequence is incomplete: ", paste(panel_sequence, collapse = ", "))
}

# Tiny audit summary BELOW the five panel datasets, on the SAME worksheet.
# Report physically paired centers and fit diagnostics, not a new large dataset.
fit_meta <- dplyr::bind_rows(lapply(c("P2p Scan", "Ca2p Scan"), function(sh) {
  f <- xps_fit_store[[sh]]
  tibble::tibble(
    Region = sh,
    Model = if (sh == "Ca2p Scan") "Two paired doublets (exploratory)" else "One paired doublet",
    BE_2p3_2_eV = f$mu, BE_2p1_2_eV = f$mu + f$delta,
    Secondary_2p3_2_eV = f$second_mu,
    Secondary_2p1_2_eV = if (sh == "Ca2p Scan") f$second_mu + f$delta else NA_real_,
    Secondary_area_fraction_model = f$second_area_fraction,
    Separation_eV = f$delta, Main_FWHM_eV = f$fwhm,
    Secondary_FWHM_eV = f$second_fwhm,
    Lorentz_fraction = f$eta, Doublet_area_ratio = "2:1",
    Background = f$background_method, Fit_R2 = f$r2, Fit_RMSE_counts = f$rmse
  )
}))
fit_row <- last_used_row + 5L
openxlsx::writeData(sd_wb, sd_sheet,
                    "XPS FIT PARAMETERS — model estimates; charge referencing must be verified",
                    startRow = fit_row, startCol = 1)
openxlsx::writeData(sd_wb, sd_sheet, fit_meta, startRow = fit_row + 1L,
                    startCol = 1, headerStyle = sd_header_style)
openxlsx::setColWidths(sd_wb, sd_sheet, cols = 7:11, widths = 18)

openxlsx::saveWorkbook(sd_wb, sd_file, overwrite = TRUE)
if (!file.exists(sd_file) || is.na(file.info(sd_file)$size) || file.info(sd_file)$size <= 0) {
  stop("Figure 2 source-data workbook was not saved: ", sd_file)
}
saved_sheet_names <- openxlsx::getSheetNames(sd_file)
if (!identical(saved_sheet_names, sd_sheet)) {
  stop("Expected exactly one source-data worksheet; found: ", paste(saved_sheet_names, collapse = ", "))
}

# Save the caption alongside the figure so the XRD normalization is explained
# in the caption, not in panel (a)'s y-axis title.
caption_text <- paste0(
  "Figure 2. X-ray diffraction, X-ray photoelectron spectroscopy, and EDS elemental composition of GP and PEG. ",
  "(a) XRD pattern of PEG; the measured intensity is displayed as sqrt(I/Imax), where I is measured intensity and Imax is the maximum measured intensity, to improve visibility of weaker diffraction features. Colored reference sticks indicate selected phase reflections. ",
  "(b) XPS survey spectra of GP and PEG; each spectrum is independently min–max normalized for display. ",
  "(c) High-resolution PEG P 2p spectrum represented by a paired phosphate-associated spin-orbit doublet (2:1 area ratio, 0.86 eV separation). (d) High-resolution PEG Ca 2p spectrum represented by two paired doublets for exploratory line-shape decomposition. Three prominent components are labelled, and the weaker fourth spin-orbit partner is included in the fit and faintly shown. Component (II) is not a uniquely identified calcium-phosphate species; similar fitted shoulders occur in GP. Binding-energy values are model estimates on the instrument-exported scale. ",
  "(e) EDS elemental composition of GP and PEG; segment widths are normalized to the sum of the plotted elemental weight percentages for each sample."
)
caption_path <- file.path(out_dir, "Figure2_caption.txt")
writeLines(caption_text, caption_path, useBytes = TRUE)

writeLines(c(
  paste("Figure 2 script:", FIG2_SCRIPT_VERSION),
  "STATUS: SUCCESS — five-panel figure and minimal source data saved",
  paste("Working directory:", project_dir),
  paste("Output directory:", normalizePath(out_dir, winslash = "/")),
  paste("Source-data workbook:", normalizePath(sd_file, winslash = "/")),
  paste("Worksheet:", saved_sheet_names),
  "Panel sequence: a, b, c, d, e",
  paste("Figure PDF:", normalizePath(pdf_path, winslash = "/", mustWork = FALSE)),
  paste("Figure TIFF:", normalizePath(tif_path, winslash = "/", mustWork = FALSE)),
  paste("Figure PNG:", normalizePath(png_path, winslash = "/", mustWork = FALSE)),
  paste("Caption:", normalizePath(caption_path, winslash = "/", mustWork = FALSE))
), run_status_path)

write.csv(fit_meta, file.path(out_dir, "Fig2_XPS_fit_summary.csv"), row.names = FALSE)
writeLines(c(
  "XPS CAUTION — fitted energies are model estimates on the input instrument binding-energy axis.",
  "Verify adventitious C 1s referencing/charge compensation and vendor background processing.",
  "P 2p uses one complete spin-orbit doublet; Ca 2p uses TWO complete linked doublets for an exploratory three-visible-component display.",
  "The fourth Ca component is the 2p1/2 spin-orbit partner of the orange component (II), included in fit and Source Data.",
  "The secondary Ca component is NOT uniquely phosphate-specific: similar shoulders fit GP too.",
  "If binding-energy calibration changes, regenerate ALL panels and Source Data.",
  "The manuscript must not claim that the original 347.2 eV peak was experimentally resolved as Ca phosphate.",
  "The Ca 2p position shift between GP and PEG requires comparison on the same charge-reference basis."
), file.path(out_dir, "Fig2_XPS_validation_notes.txt"))

message("\n============================================================")
message("FIGURE 2 COMPLETE — FIVE PANELS (a–e)")
message("PDF: ", pdf_path)
message("TIFF (600 dpi): ", tif_path)
message("PNG preview: ", png_path)
message("Source data: ", sd_file)
message("Caption: ", caption_path)
message("============================================================\n")

if (.Platform$OS.type == "windows") {
  try(shell.exec(normalizePath(out_dir, winslash = "\\")), silent = TRUE)
}



# External verification note: energy referencing and any alternative Ca chemical
# environment must be validated separately; this program does not assert that
# the fitted doublet proves a discrete Ca-phosphate mineral phase.

# NOTE: The fourth Ca spin-orbit partner is deliberately faint in panel (d)
# but always included in fitted envelope and Source Data. The three prominently
# colored traces are not three independent Ca chemical states.



