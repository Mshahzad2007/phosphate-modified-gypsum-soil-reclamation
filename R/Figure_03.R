#####20/10/26
# FIGURE 3 — ORIGINAL 3+PHOTO / 3+PHOTO LAYOUT, NARROWER BARS
# First row: (a), (b), (c), (g); second row: (d), (e), (f), continued (g).
# Bars / error bars / significance letters use identical dodge and x-positions.
# All original statistical analyses and raw-value calculations are preserved.
# Use 'sans' + Cairo PDF to avoid PDF CID font problems.

pkgs <- c(
  "ggplot2", "dplyr", "tidyr", "readxl",
  "patchwork", "scales", "multcompView", "openxlsx", "png", "jpeg"
)

to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

invisible(lapply(pkgs, library, character.only = TRUE))

# =========================================================
# 1. SETTINGS
# =========================================================
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

file_in  <- repo_input_path("Conventional_Treatments.xlsx")
sheet_in <- "data"

# Only change the bar widths and the horizontal treatment spacing.
# Numerical positions apply to bars, error bars, statistics and inset strips.
DODGE_WIDTH <- 0.24
BAR_WIDTH   <- 0.15
TREATMENT_X <- c(CK = 1.00, GP = 1.34, PEG = 1.68)
# Trim each x-axis so it ends just beyond the outer bars (no long empty axis tail)
X_OUTER_PAD <- 0.001
X_HALF_SPAN <- DODGE_WIDTH / 4 + BAR_WIDTH / 2 + X_OUTER_PAD
X_LIMITS    <- c(min(TREATMENT_X) - X_HALF_SPAN, max(TREATMENT_X) + X_HALF_SPAN)
x_at <- function(treatment) unname(TREATMENT_X[as.character(treatment)])

# The original 3-graph + right-hand photo arrangement, fitting a 180-mm page.
FIG_WIDTH_MM  <- 180
FIG_HEIGHT_MM <- 145

# Optional original TALL montage from Fig. 3(g).
# Place it beside the Excel file or in the R working directory.
plant_img <- repo_input_path("Plant_Comparison_Tall.png")
if (!file.exists(plant_img)) {
  warning("Figure 03 plant montage is not provided. Panel (g) will contain blank photo placeholders; full visual reproduction requires the original Plant_Comparison_Tall.png.", call. = FALSE)
}

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Minimal colour system
col_bar1  <- "#0B3C6D"
col_bar2  <- "#0A9D8F"
col_line  <- "#7F7F7F"
col_line2 <- "#E66101"   # orange for SOC/STP
col_ph    <- "#8E5DB7"

# =========================================================
# 2. HELPERS
# =========================================================
clean_names_simple <- function(x) {
  x <- gsub("%", "pct", x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

pick_col <- function(df, candidates, required = TRUE) {
  hit <- intersect(candidates, names(df))
  
  if (length(hit) == 0) {
    if (required) {
      stop(
        paste0("Missing required column. Tried: ", paste(candidates, collapse = ", ")),
        call. = FALSE
      )
    } else {
      return(NULL)
    }
  }
  
  hit[1]
}

p_to_star <- function(p) {
  ifelse(
    is.na(p), "",
    ifelse(
      p >= 0.05, "n.s.",
      ifelse(
        p < 0.001, "***",
        ifelse(p < 0.01, "**", "*")
      )
    )
  )
}

summarise_var <- function(dat, var, trt = "treatment") {
  dat %>%
    dplyr::group_by(.data[[trt]]) %>%
    dplyr::summarise(
      n    = sum(!is.na(.data[[var]])),
      mean = mean(.data[[var]], na.rm = TRUE),
      sd   = sd(.data[[var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      sd = ifelse(is.na(sd), 0, sd),
      se = ifelse(n > 0, sd / sqrt(n), 0),
      variable = var
    ) %>%
    dplyr::rename(treatment = dplyr::all_of(trt))
}

get_tukey_p <- function(dat, var, g1, g2, trt = "treatment") {
  tmp <- dat %>%
    dplyr::select(dplyr::all_of(c(trt, var))) %>%
    dplyr::filter(!is.na(.data[[var]])) %>%
    dplyr::mutate(.grp = factor(.data[[trt]], levels = c("CK", "GP", "PEG")))
  
  if (nrow(tmp) == 0 || length(unique(tmp$.grp)) < 2) return(NA_real_)
  
  fit <- tryCatch(stats::aov(tmp[[var]] ~ tmp$.grp), error = function(e) NULL)
  if (is.null(fit)) return(NA_real_)
  
  tk <- tryCatch(stats::TukeyHSD(fit)[[1]], error = function(e) NULL)
  if (is.null(tk)) return(NA_real_)
  
  rn1 <- paste(g2, g1, sep = "-")
  rn2 <- paste(g1, g2, sep = "-")
  
  if (rn1 %in% rownames(tk)) return(tk[rn1, "p adj"])
  if (rn2 %in% rownames(tk)) return(tk[rn2, "p adj"])
  
  NA_real_
}

calc_cld_overlay <- function(dat, var, trt = "treatment") {
  x <- dat[[var]]
  g <- factor(dat[[trt]], levels = c("CK", "GP", "PEG"))
  
  ok <- is.finite(x) & !is.na(g)
  x <- x[ok]
  g <- droplevels(g[ok])
  
  if (length(unique(g)) < 2L || length(unique(x)) <= 1L) {
    return(setNames(rep("a", 3), c("CK", "GP", "PEG")))
  }
  
  fit <- tryCatch(stats::aov(x ~ g), error = function(e) NULL)
  if (is.null(fit)) {
    return(setNames(rep("a", 3), c("CK", "GP", "PEG")))
  }
  
  tk <- tryCatch(stats::TukeyHSD(fit), error = function(e) NULL)
  if (is.null(tk)) {
    return(setNames(rep("a", 3), c("CK", "GP", "PEG")))
  }
  
  pvec <- tk$g[, "p adj"]
  letters <- multcompView::multcompLetters(pvec)$Letters
  
  out <- setNames(rep(NA_character_, 3), c("CK", "GP", "PEG"))
  out[names(letters)] <- letters
  out
}

summarise_overlay_var <- function(dat, var, trt = "treatment") {
  cld_map <- calc_cld_overlay(dat, var, trt)
  
  dat %>%
    dplyr::group_by(.data[[trt]]) %>%
    dplyr::summarise(
      n    = sum(!is.na(.data[[var]])),
      mean = mean(.data[[var]], na.rm = TRUE),
      sd   = sd(.data[[var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      sd = ifelse(is.na(sd), 0, sd),
      se = ifelse(n > 0, sd / sqrt(n), 0),
      variable = var
    ) %>%
    dplyr::rename(treatment = dplyr::all_of(trt)) %>%
    dplyr::mutate(
      treatment = factor(treatment, levels = c("CK", "GP", "PEG")),
      cld = cld_map[as.character(treatment)]
    ) %>%
    dplyr::select(variable, treatment, n, mean, sd, se, cld)
}

scale_overlay_to_panel <- function(d, to_range) {
  from_range <- range(d$mean, na.rm = TRUE)
  
  if (!all(is.finite(from_range)) || diff(from_range) == 0) {
    mid <- mean(to_range)
    return(
      d %>%
        dplyr::mutate(
          y      = mid,
          y_low  = mid,
          y_high = mid
        )
    )
  }
  
  d %>%
    dplyr::mutate(
      y      = scales::rescale(mean,      to = to_range, from = from_range),
      y_low  = scales::rescale(mean - se, to = to_range, from = from_range),
      y_high = scales::rescale(mean + se, to = to_range, from = from_range),
      y_low  = pmax(min(to_range), pmin(max(to_range), y_low)),
      y_high = pmax(min(to_range), pmin(max(to_range), y_high))
    )
}


# =========================================================
# SHARED THREE-SEGMENT INSET: CaCO3 / pH
# =========================================================
# Draws CK|GP|PEG as one adjoining strip. The mean marker and its vertical
# whisker (mean +/- observed SD) use ONE local inset scale. Their positions
# must not be read from the main y-axis.
add_top_inset_bars <- function(p, stats_df, y_upper, title_text,
                               fill_col = "#C58A28", line_col = "#8A5B0B",
                               title_col = fill_col, base_frac = 0.79,
                               top_frac = 0.90, title_x = NULL,
                               title_hjust = 0.5, title_y_frac = 0.983) {
  if (is.null(stats_df) || nrow(stats_df) == 0) return(p)
  if (is.null(title_x)) title_x <- x_at("GP")
  
  d <- stats_df %>%
    dplyr::filter(as.character(treatment) %in% names(TREATMENT_X)) %>%
    dplyr::mutate(
      treatment = factor(as.character(treatment), levels = names(TREATMENT_X)),
      sd = tidyr::replace_na(sd, 0)
    ) %>%
    dplyr::arrange(treatment)
  if (nrow(d) == 0) return(p)
  
  centres <- unname(TREATMENT_X)
  inset_pad_left  <- 0.030
  inset_pad_right <- 0.006
  edges <- c(
    X_LIMITS[1] + inset_pad_left,
    (centres[1] + centres[2]) / 2,
    (centres[2] + centres[3]) / 2,
    X_LIMITS[2] - inset_pad_right
  )
  idx <- match(as.character(d$treatment), names(TREATMENT_X))
  
  val_rng <- range(c(d$mean - d$sd, d$mean + d$sd), na.rm = TRUE)
  if (!all(is.finite(val_rng))) return(p)
  
  title_y   <- y_upper * title_y_frac
  bar_base  <- y_upper * base_frac
  bar_topmx <- y_upper * top_frac
  lab_y     <- bar_base - 0.020 * y_upper
  
  map_y <- function(x) {
    if (diff(val_rng) <= 1e-12) {
      rep((bar_base + bar_topmx) / 2, length(x))
    } else {
      scales::rescale(x,
                      to = c(bar_base + 0.010 * y_upper, bar_topmx - 0.010 * y_upper),
                      from = val_rng
      )
    }
  }
  
  d$xmin <- edges[idx]
  d$xmax <- edges[idx + 1L]
  d$xmid <- (d$xmin + d$xmax) / 2
  d$xsd  <- d$xmax - 0.010
  d$ytop <- map_y(d$mean)
  d$ylo  <- pmax(bar_base + 0.004 * y_upper, map_y(d$mean - d$sd))
  d$yhi  <- pmin(bar_topmx - 0.004 * y_upper, map_y(d$mean + d$sd))
  d$ycld <- pmin(title_y - 0.018 * y_upper, d$ytop + 0.012 * y_upper)
  if (!"cld" %in% names(d)) d$cld <- ""
  
  p +
    annotate("text", x = title_x, y = title_y,
             label = title_text, hjust = title_hjust, vjust = 1,
             size = 2.75, colour = title_col) +
    geom_rect(
      data = d,
      aes(xmin = xmin, xmax = xmax, ymin = bar_base, ymax = ytop),
      inherit.aes = FALSE,
      fill = fill_col, alpha = 0.84,
      colour = "white", linewidth = 0.35
    ) +
    geom_segment(
      data = d,
      aes(x = xsd, xend = xsd, y = ylo, yend = yhi),
      inherit.aes = FALSE,
      colour = line_col, linewidth = 0.75
    ) +
    geom_segment(
      data = d,
      aes(x = xsd - 0.010, xend = xsd + 0.010, y = ylo, yend = ylo),
      inherit.aes = FALSE,
      colour = line_col, linewidth = 0.6
    ) +
    geom_segment(
      data = d,
      aes(x = xsd - 0.010, xend = xsd + 0.010, y = yhi, yend = yhi),
      inherit.aes = FALSE,
      colour = line_col, linewidth = 0.6
    ) +
    geom_text(
      data = d,
      aes(x = xmid, y = lab_y, label = treatment),
      inherit.aes = FALSE,
      size = 2.35, colour = "black", fontface = "plain", vjust = 0.5
    ) +
    geom_text(
      data = d,
      aes(x = xmid, y = ycld, label = cld),
      inherit.aes = FALSE,
      size = 2.45, colour = "black", vjust = 0
    )
}

# =========================================================
# 3. READ DATA
# =========================================================
raw_in <- readxl::read_excel(file_in, sheet = sheet_in)
names(raw_in) <- clean_names_simple(names(raw_in))

if (all(c("response_varibale", "value") %in% names(raw_in))) {
  names(raw_in)[names(raw_in) == "response_varibale"] <- "response_variable"
}

if (all(c("response_variable", "value") %in% names(raw_in))) {
  id_cols_now <- setdiff(names(raw_in), c("response_variable", "value"))
  
  raw_in <- tidyr::pivot_wider(
    raw_in,
    id_cols = dplyr::all_of(id_cols_now),
    names_from = response_variable,
    values_from = value,
    values_fn = function(x) mean(as.numeric(x), na.rm = TRUE)
  )
}

names(raw_in) <- clean_names_simple(names(raw_in))

trt_col <- pick_col(raw_in, c("treatments", "treatment", "trt"))

col_map <- list(
  Sol_Na      = pick_col(raw_in, c("sol_na", "na", "soluble_na")),
  Ex_Na       = pick_col(raw_in, c("ex_na", "e_na", "exchangeable_na", "ena")),
  Sol_Ca      = pick_col(raw_in, c("sol_ca", "ca", "soluble_ca")),
  Ex_Ca       = pick_col(raw_in, c("ex_ca", "e_ca", "exchangeable_ca", "eca")),
  Reactive_Ca = pick_col(raw_in, c("reactive_ca", "rc")),
  Ca_F3       = pick_col(raw_in, c("ca_f3", "caf3", "ca_f_3")),
  HCO3        = pick_col(raw_in, c("hco3", "hco3_")),
  CO3         = pick_col(raw_in, c("co3", "co3_")),
  SO4         = pick_col(raw_in, c("so4", "so4_")),
  Cl          = pick_col(raw_in, c("cl", "cl_")),
  CaCO3       = pick_col(raw_in, c("ca_co3", "caco3", "ca_co_3")),
  SOC         = pick_col(raw_in, c("soc")),
  STP         = pick_col(raw_in, c("stp")),
  SAR         = pick_col(raw_in, c("sar")),
  Biomass     = pick_col(raw_in, c("biomass")),
  pH          = pick_col(raw_in, c("ph", "p_h"), required = FALSE)
)

df_raw <- data.frame(
  treatment = raw_in[[trt_col]],
  stringsAsFactors = FALSE
)

for (nm in names(col_map)) {
  if (!is.null(col_map[[nm]])) {
    df_raw[[nm]] <- suppressWarnings(as.numeric(raw_in[[col_map[[nm]]]]))
  }
}

df_raw$treatment <- as.character(df_raw$treatment)
df_raw$treatment[df_raw$treatment %in% c("PGP")] <- "PEG"
df_raw$treatment[df_raw$treatment %in% c("PEG")] <- "PEG"
df_raw$treatment[df_raw$treatment %in% c("GP")]  <- "GP"
df_raw$treatment[df_raw$treatment %in% c("CK")]  <- "CK"

df_raw <- df_raw %>%
  dplyr::filter(treatment %in% c("CK", "GP", "PEG")) %>%
  dplyr::mutate(treatment = factor(treatment, levels = c("CK", "GP", "PEG")))

if (nrow(df_raw) == 0) {
  stop("No valid treatment rows found after filtering to CK, GP, PEG.", call. = FALSE)
}

# =========================================================
# 4. OUTPUT FOLDER
# =========================================================
out_dir <- file.path("outputs", "Fig3_NC_original_layout_compact_narrow_bars")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# =========================================================
# 5. STATS OBJECTS
# =========================================================
bar_vars <- c(
  "Sol_Na", "Ex_Na", "Sol_Ca", "Ex_Ca",
  "Reactive_Ca", "Ca_F3", "HCO3", "CO3",
  "SO4", "Cl", "SAR", "Biomass"
)
bar_vars <- bar_vars[bar_vars %in% names(df_raw)]

bar_stats <- dplyr::bind_rows(
  lapply(bar_vars, function(v) summarise_var(df_raw, v))
)

overlay_vars <- c("SOC", "STP")
overlay_vars <- overlay_vars[overlay_vars %in% names(df_raw)]

overlay_stats <- dplyr::bind_rows(
  lapply(overlay_vars, function(v) summarise_overlay_var(df_raw, v))
)

has_pH <- "pH" %in% names(df_raw)

if (has_pH) {
  pH_stats <- summarise_var(df_raw, "pH")
} else {
  pH_stats <- NULL
}

# =========================================================
# 6. THEME — NO BOLD TEXT
# =========================================================
theme_fig <- function() {
  # Sizes below are intended for the FINAL 180-mm-wide figure, not a large slide.
  # Use a portable sans-serif family. The default PDF device cannot reliably
  # resolve Arial on every system and may trigger "failed to find PDF CID font".
  theme_classic(base_size = 9.5, base_family = "sans") +
    theme(
      panel.background = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      axis.line = element_line(colour = "#333333", linewidth = 0.40),
      axis.ticks = element_line(colour = "#333333", linewidth = 0.35),
      axis.ticks.length = unit(2.0, "pt"),
      axis.text.x = element_text(size = 8.5, colour = "#222222"),
      axis.text.y = element_text(size = 8.0, colour = "#222222"),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 8.6, colour = "black", margin = margin(r = 3)),
      
      # Place panel tags in their own plot-title header, not over a top y-axis tick.
      plot.title = element_text(size = 10.5, face = "bold", colour = "black",
                                hjust = 0, margin = margin(b = 3, unit = "pt")),
      plot.title.position = "plot",
      plot.tag = element_blank(),
      plot.margin = margin(t = 5, r = 6, b = 5, l = 9, unit = "pt"),
      
      legend.position = "top",
      legend.justification = "center",
      legend.title = element_blank(),
      legend.text = element_text(size = 7.6, colour = "black"),
      legend.key.size = unit(0.28, "cm"),
      legend.spacing.x = unit(0.10, "cm"),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
    )
}

# =========================================================
# 7. MAIN PANEL FUNCTION
# =========================================================
make_panel_treatment_x <- function(panel_vars,
                                   legend_labels,
                                   y_lab,
                                   panel_tag,
                                   overlay_vars_panel = NULL,
                                   overlay_labels = NULL,
                                   add_pH_strip = FALSE,
                                   y_expand = 1.50,
                                   bracket_y = c(0.74, 0.84)) {
  
  pd <- position_dodge(width = DODGE_WIDTH)
  
  st <- bar_stats %>%
    dplyr::filter(variable %in% panel_vars) %>%
    dplyr::mutate(
      treatment = factor(treatment, levels = c("CK", "GP", "PEG")),
      variable = factor(variable, levels = panel_vars),
      x_pos = x_at(treatment)
    )
  
  y_upper <- max(st$mean + st$se, na.rm = TRUE) * y_expand
  if (!is.finite(y_upper) || y_upper <= 0) y_upper <- 1
  
  fill_cols <- setNames(c(col_bar1, col_bar2)[seq_along(panel_vars)], panel_vars)
  
  p <- ggplot(st, aes(x = x_pos, y = mean, fill = variable, colour = variable)) +
    geom_col(
      position = pd,
      width = BAR_WIDTH,
      linewidth = 0.45,
      alpha = 0.95
    ) +
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      position = pd,
      width = 0.075,
      linewidth = 0.45,
      colour = "#2f2f2f"
    ) +
    scale_fill_manual(values = fill_cols, labels = legend_labels) +
    scale_colour_manual(values = fill_cols, labels = legend_labels) +
    scale_x_continuous(
      breaks = unname(TREATMENT_X), labels = names(TREATMENT_X),
      limits = X_LIMITS, expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
    coord_cartesian(ylim = c(0, y_upper), clip = "on") +
    labs(x = NULL, y = y_lab, title = panel_tag, tag = NULL) +
    theme_fig()
  
  cld_df <- dplyr::bind_rows(
    lapply(panel_vars, function(v) {
      cld <- calc_cld_overlay(df_raw, v)
      bar_stats %>%
        dplyr::filter(variable == v) %>%
        dplyr::mutate(
          variable = factor(variable, levels = panel_vars),
          cld = cld[as.character(treatment)],
          x_pos = x_at(treatment),
          y_cld = mean + se + 0.045 * y_upper
        )
    })
  )
  
  p <- p +
    geom_text(
      data = cld_df,
      aes(x = x_pos, y = y_cld, label = cld, group = variable),
      position = pd,
      inherit.aes = FALSE,
      size = 2.9,
      fontface = "plain",
      colour = "black"
    )
  
  bracket_var <- panel_vars[1]
  p_ck_peg <- get_tukey_p(df_raw, bracket_var, "CK", "PEG")
  p_gp_peg <- get_tukey_p(df_raw, bracket_var, "GP", "PEG")
  
  bracket_df <- data.frame(
    x1 = c(TREATMENT_X[["CK"]], TREATMENT_X[["GP"]]),
    x2 = rep(TREATMENT_X[["PEG"]], 2),
    y  = y_upper * bracket_y,
    label = c(p_to_star(p_ck_peg), p_to_star(p_gp_peg))
  ) %>%
    dplyr::filter(label != "")
  
  if (nrow(bracket_df) > 0) {
    p <- p +
      geom_segment(
        data = bracket_df,
        aes(x = x1, xend = x2, y = y, yend = y),
        inherit.aes = FALSE,
        linewidth = 0.50,
        colour = "black"
      ) +
      geom_segment(
        data = bracket_df,
        aes(x = x1, xend = x1, y = y - 0.02 * y_upper, yend = y),
        inherit.aes = FALSE,
        linewidth = 0.50,
        colour = "black"
      ) +
      geom_segment(
        data = bracket_df,
        aes(x = x2, xend = x2, y = y - 0.02 * y_upper, yend = y),
        inherit.aes = FALSE,
        linewidth = 0.50,
        colour = "black"
      ) +
      geom_text(
        data = bracket_df,
        aes(x = (x1 + x2) / 2, y = y + 0.015 * y_upper, label = label),
        inherit.aes = FALSE,
        size = 2.50,
        fontface = "plain",
        colour = "black"
      )
  }
  
  if (!is.null(overlay_vars_panel) && length(overlay_vars_panel) > 0) {
    
    overlay_cols <- c(CaCO3 = col_line, SOC = col_line2, STP = "#B85C1A")
    overlay_lty   <- c(CaCO3 = "dashed", SOC = "solid", STP = "dashed")
    overlay_shape <- c(CaCO3 = 21, SOC = 21, STP = 24)
    
    for (ov in overlay_vars_panel) {
      od <- overlay_stats %>%
        dplyr::filter(variable == ov) %>%
        dplyr::arrange(treatment)
      
      if (nrow(od) == 0) next
      
      od <- od %>%
        scale_overlay_to_panel(to_range = c(y_upper * 0.20, y_upper * 0.70)) %>%
        dplyr::mutate(
          x_num = x_at(treatment),
          y_cld = y_high + 0.040 * y_upper
        )
      
      p <- p +
        geom_line(
          data = od,
          aes(x = x_num, y = y, group = 1),
          inherit.aes = FALSE,
          colour = overlay_cols[[ov]],
          linewidth = 0.45,
          linetype = overlay_lty[[ov]]
        ) +
        geom_errorbar(
          data = od,
          aes(x = x_num, ymin = y_low, ymax = y_high),
          inherit.aes = FALSE,
          width = 0.055,
          linewidth = 0.45,
          colour = overlay_cols[[ov]]
        ) +
        geom_point(
          data = od,
          aes(x = x_num, y = y),
          inherit.aes = FALSE,
          shape = overlay_shape[[ov]],
          size = 2.85,
          stroke = 1.0,
          fill = "white",
          colour = overlay_cols[[ov]]
        )
    }
    
    if (!is.null(overlay_labels)) {
      # Compact legend completely inside the plotting area.
      # IMPORTANT: Orange traces show trends only; they have been independently
      # rescaled for visibility and must not be read against the Ca y-axis.
      legend_y <- y_upper * 0.925
      for (i in seq_along(overlay_vars_panel)) {
        ov <- overlay_vars_panel[i]
        xx <- 0.94 + (i - 1) * 0.42
        p <- p +
          annotate("segment", x = xx, xend = xx + 0.10,
                   y = legend_y, yend = legend_y,
                   colour = overlay_cols[[ov]],
                   linetype = overlay_lty[[ov]], linewidth = 0.50) +
          annotate("point", x = xx + 0.05, y = legend_y,
                   shape = overlay_shape[[ov]], size = 2.35,
                   stroke = 0.75, fill = "white", colour = overlay_cols[[ov]]) +
          annotate("text", x = xx + 0.13, y = legend_y,
                   label = overlay_labels[i], hjust = 0, vjust = 0.5,
                   size = 2.45, colour = "black")
      }
    }
  }
  
  if (isTRUE(add_pH_strip) && has_pH && !is.null(pH_stats) && nrow(pH_stats) > 0) {
    ph_cld <- calc_cld_overlay(df_raw, "pH")
    phs <- pH_stats %>%
      dplyr::mutate(cld = ph_cld[as.character(treatment)])
    
    p <- add_top_inset_bars(
      p = p,
      stats_df = phs,
      y_upper = y_upper,
      title_text = "pH",
      fill_col = col_ph,
      line_col = "#6C3FA4",
      title_col = col_ph,
      base_frac = 0.845,
      top_frac = 0.91,
      title_x = x_at("GP"),
      title_hjust = 0.5,
      title_y_frac = 0.975
    )
  }
  
  p
}

# =========================================================
# 8. PANEL (g) — PLANT PHOTO / CLEAN WHITE PLACEHOLDERS
# =========================================================
# The image spans both rows. If Plant_Comparison_Tall.png is not present,
# show empty white photo slots with light borders for manual image placement.
make_plant_panel <- function(image_file = plant_img) {
  base <- ggplot() +
    coord_cartesian(xlim = c(0, 3), ylim = c(0, 4), expand = FALSE, clip = "on") +
    theme_void(base_family = "sans") +
    labs(title = "(g)") +
    theme(
      plot.title = element_text(size = 10.5, face = "bold", colour = "black",
                                hjust = 0, margin = margin(b = 2, unit = "pt")),
      plot.title.position = "plot",
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  
  if (file.exists(image_file)) {
    ext <- tolower(tools::file_ext(image_file))
    img <- switch(
      ext,
      png  = png::readPNG(image_file),
      jpg  = jpeg::readJPEG(image_file),
      jpeg = jpeg::readJPEG(image_file),
      stop("Plant photo montage must be PNG or JPEG: ", image_file)
    )
    return(base + annotation_raster(img, xmin = 0, xmax = 3, ymin = 0, ymax = 4))
  }
  
  rects <- expand.grid(column = 1:3, row = 1:4) %>%
    dplyr::mutate(
      xmin = column - 1 + 0.025,
      xmax = column - 0.025,
      ymin = 4 - row + 0.025,
      ymax = 5 - row - 0.025
    )
  
  base +
    geom_rect(
      data = rects,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      fill = "white",
      colour = "#D9DDE2",
      linewidth = 0.40
    ) +
    annotate("text", x = c(0.5, 1.5, 2.5), y = 3.85,
             label = c("CK", "GP", "PEG"),
             colour = "#222222", fontface = "bold", size = 3.1)
}

# =========================================================
# 9. BUILD PANELS
# =========================================================
p1 <- make_panel_treatment_x(
  panel_vars = c("Sol_Na", "Ex_Na"),
  legend_labels = c("Sol. Na", "Ex. Na"),
  y_lab = expression("Na"~"(Sol., mmol L"^{-1}*"; Ex., cmol"[c]~"kg"^{-1}*")"),
  panel_tag = "(a)",
  y_expand = 1.55
)

p2 <- make_panel_treatment_x(
  panel_vars = c("Sol_Ca", "Ex_Ca"),
  legend_labels = c("Sol. Ca", "Ex. Ca"),
  y_lab = expression("Ca"~"(Sol., mmol L"^{-1}*"; Ex., cmol"[c]~"kg"^{-1}*")"),
  panel_tag = "(b)",
  overlay_vars_panel = NULL,
  overlay_labels = NULL,
  y_expand = 1.90,
  bracket_y = c(0.66, 0.75)
)

# =========================================================
# PANEL (b) — CaCO3 COMPACT TOP INSET BARS
# =========================================================
p2_caco3 <- summarise_var(df_raw, "CaCO3")
caco3_cld <- tryCatch(calc_cld_overlay(df_raw, "CaCO3"), error = function(e) NULL)
if (!is.null(caco3_cld)) {
  p2_caco3$cld <- caco3_cld[as.character(p2_caco3$treatment)]
} else {
  p2_caco3$cld <- ""
}
p2_upper <- max(
  (bar_stats %>% dplyr::filter(variable %in% c("Sol_Ca", "Ex_Ca")))$mean +
    (bar_stats %>% dplyr::filter(variable %in% c("Sol_Ca", "Ex_Ca")))$se,
  na.rm = TRUE
) * 1.90
p2 <- add_top_inset_bars(
  p = p2,
  stats_df = p2_caco3,
  y_upper = p2_upper,
  title_text = "CaCO3",
  fill_col = "#C58A28",
  line_col = "#8A5B0B",
  title_col = "#A36A12",
  base_frac = 0.855,
  top_frac = 0.925,
  title_x = x_at("GP"),
  title_hjust = 0.5,
  title_y_frac = 1.000
)

p3 <- make_panel_treatment_x(
  panel_vars = c("Reactive_Ca", "Ca_F3"),
  legend_labels = c("RC", "CF"),
  y_lab = expression("Ca pools"~"(cmol"[c]~"kg"^{-1}*")"),
  panel_tag = "(c)",
  overlay_vars_panel = c("SOC", "STP"),
  overlay_labels = c("SOC", "STP"),
  y_expand = 1.62
)

p4 <- make_panel_treatment_x(
  panel_vars = c("HCO3", "CO3"),
  legend_labels = expression(HCO[3]^"-", CO[3]^"2-"),
  y_lab = expression("Carbonate"~"(meq L"^{-1}*")"),
  panel_tag = "(d)",
  add_pH_strip = TRUE,
  y_expand = 1.95,
  bracket_y = c(0.62, 0.72)
)

p5 <- make_panel_treatment_x(
  panel_vars = c("SO4", "Cl"),
  legend_labels = expression(SO[4]^"2-", Cl^"-"),
  y_lab = expression("Anions"~"(mmol L"^{-1}*")"),
  panel_tag = "(e)",
  y_expand = 1.55
)

p6 <- make_panel_treatment_x(
  panel_vars = c("SAR", "Biomass"),
  legend_labels = c("SAR", "Biomass"),
  y_lab = "SAR / Biomass",
  panel_tag = "(f)",
  y_expand = 1.68
)

# The entire bottom row is reserved for a wide plant-photo comparison.
p7 <- make_plant_panel(plant_img)

# =========================================================
# 10. ORIGINAL FIG. 3 LAYOUT: 3 GRAPHS PER ROW + PLANT PHOTOS AT RIGHT
# =========================================================
# The seventh panel, (g), occupies the final column across both rows.
# This preserves your first figure's panel order and placement.
layout_design <- "ABCG\nDEFG"

fig_final <- patchwork::wrap_plots(
  A = p1, B = p2, C = p3,
  D = p4, E = p5, F = p6,
  G = p7,
  design = layout_design,
  widths = c(0.98, 0.98, 0.98, 1.24),
  heights = c(1.00, 1.00)
) &
  theme(plot.background = element_rect(fill = "white", colour = NA))

print(fig_final)

# =========================================================
# 11. EXPORT STATISTICS
# =========================================================
stats_vars <- c(
  "Sol_Na", "Ex_Na",
  "Sol_Ca", "Ex_Ca",
  "Reactive_Ca", "Ca_F3",
  "HCO3", "CO3",
  "SO4", "Cl",
  "SAR", "Biomass",
  "CaCO3", "SOC", "STP"
)

if ("pH" %in% names(df_raw)) {
  stats_vars <- c(stats_vars, "pH")
}

stats_vars <- unique(stats_vars)
stats_vars <- stats_vars[stats_vars %in% names(df_raw)]

summary_stats <- dplyr::bind_rows(
  lapply(stats_vars, function(v) {
    df_raw %>%
      dplyr::group_by(treatment) %>%
      dplyr::summarise(
        n    = sum(!is.na(.data[[v]])),
        mean = mean(.data[[v]], na.rm = TRUE),
        sd   = sd(.data[[v]], na.rm = TRUE),
        se   = sd / sqrt(n),
        min  = min(.data[[v]], na.rm = TRUE),
        max  = max(.data[[v]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        variable = v,
        se = ifelse(is.nan(se), NA_real_, se),
        min = ifelse(is.infinite(min), NA_real_, min),
        max = ifelse(is.infinite(max), NA_real_, max)
      ) %>%
      dplyr::select(variable, treatment, n, mean, sd, se, min, max)
  })
)

anova_results <- dplyr::bind_rows(
  lapply(stats_vars, function(v) {
    tmp <- df_raw %>%
      dplyr::select(treatment, value = dplyr::all_of(v)) %>%
      dplyr::filter(!is.na(value))
    
    if (nrow(tmp) == 0 || length(unique(tmp$treatment)) < 2) {
      return(data.frame(
        variable = v,
        df_treatment = NA,
        df_residual = NA,
        F_value = NA,
        p_value = NA,
        significance = NA
      ))
    }
    
    fit <- tryCatch(aov(value ~ treatment, data = tmp), error = function(e) NULL)
    
    if (is.null(fit)) {
      return(data.frame(
        variable = v,
        df_treatment = NA,
        df_residual = NA,
        F_value = NA,
        p_value = NA,
        significance = NA
      ))
    }
    
    sm <- summary(fit)[[1]]
    
    data.frame(
      variable = v,
      df_treatment = sm["treatment", "Df"],
      df_residual  = sm["Residuals", "Df"],
      F_value      = sm["treatment", "F value"],
      p_value      = sm["treatment", "Pr(>F)"],
      significance = p_to_star(sm["treatment", "Pr(>F)"])
    )
  })
)

tukey_all_pairwise <- dplyr::bind_rows(
  lapply(stats_vars, function(v) {
    tmp <- df_raw %>%
      dplyr::select(treatment, value = dplyr::all_of(v)) %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::mutate(treatment = factor(treatment, levels = c("CK", "GP", "PEG")))
    
    if (nrow(tmp) == 0 || length(unique(tmp$treatment)) < 2) {
      return(data.frame(
        variable = v,
        contrast = NA,
        diff = NA,
        lwr = NA,
        upr = NA,
        p_adj = NA,
        significance = NA
      ))
    }
    
    fit <- tryCatch(aov(value ~ treatment, data = tmp), error = function(e) NULL)
    
    if (is.null(fit)) {
      return(data.frame(
        variable = v,
        contrast = NA,
        diff = NA,
        lwr = NA,
        upr = NA,
        p_adj = NA,
        significance = NA
      ))
    }
    
    tk <- tryCatch(TukeyHSD(fit)$treatment, error = function(e) NULL)
    
    if (is.null(tk)) {
      return(data.frame(
        variable = v,
        contrast = NA,
        diff = NA,
        lwr = NA,
        upr = NA,
        p_adj = NA,
        significance = NA
      ))
    }
    
    out <- as.data.frame(tk)
    out$contrast <- rownames(out)
    rownames(out) <- NULL
    
    out %>%
      dplyr::rename(
        diff = diff,
        lwr = lwr,
        upr = upr,
        p_adj = `p adj`
      ) %>%
      dplyr::mutate(
        variable = v,
        significance = p_to_star(p_adj)
      ) %>%
      dplyr::select(variable, contrast, diff, lwr, upr, p_adj, significance)
  })
)

notes_df <- data.frame(
  item = c(
    "Data file",
    "Treatments",
    "Panel structure",
    "Main visual change",
    "Colour system",
    "Text style",
    "pH display",
    "Statistics",
    "Significance notation"
  ),
  description = c(
    file_in,
    "CK, GP, PEG",
    "Original arrangement: A/B/C in upper row, D/E/F in lower row; G (plant photographs) spans the right column",
    "Compact CK/GP/PEG positions 1.00/1.34/1.68; slim bars 0.15; synchronized SE error bars and CLD; 180 x 145 mm",
    "Dark blue, teal, grey/orange only for overlays, purple for pH",
    "Axis/legend labels regular; only panel identifiers bold",
    "Panel D inset shows original mean pH and Tukey letters; no arbitrary scaled mini bars",
    "Mean, SD, SE, ANOVA, Tukey HSD saved",
    "n.s., P >= 0.05; *P < 0.05; **P < 0.01; ***P < 0.001"
  )
)

stats_xlsx <- file.path(
  out_dir,
  paste0("Fig3_NC_statistics_", stamp, ".xlsx")
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Summary_stats_all")
openxlsx::writeData(wb, "Summary_stats_all", summary_stats)

openxlsx::addWorksheet(wb, "ANOVA_results_all")
openxlsx::writeData(wb, "ANOVA_results_all", anova_results)

openxlsx::addWorksheet(wb, "Tukey_all_pairwise")
openxlsx::writeData(wb, "Tukey_all_pairwise", tukey_all_pairwise)

openxlsx::addWorksheet(wb, "Notes")
openxlsx::writeData(wb, "Notes", notes_df)

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  fgFill = "#D9EAF7",
  border = "Bottom"
)

for (sh in names(wb)) {
  openxlsx::addStyle(
    wb,
    sheet = sh,
    style = header_style,
    rows = 1,
    cols = 1:50,
    gridExpand = TRUE
  )
  openxlsx::freezePane(wb, sheet = sh, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet = sh, cols = 1:50, widths = "auto")
}

openxlsx::saveWorkbook(wb, stats_xlsx, overwrite = TRUE)

# =========================================================
# 12. SAVE FIGURES
# =========================================================
png_file  <- file.path(out_dir, paste0("Fig3_NC_original_layout_compact_", stamp, ".png"))
tiff_file <- file.path(out_dir, paste0("Fig3_NC_original_layout_compact_", stamp, ".tiff"))
pdf_file  <- file.path(out_dir, paste0("Fig3_NC_original_layout_compact_", stamp, ".pdf"))

ggsave(
  filename = png_file,
  plot = fig_final,
  width = FIG_WIDTH_MM,
  height = FIG_HEIGHT_MM,
  units = "mm",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = tiff_file,
  plot = fig_final,
  width = FIG_WIDTH_MM,
  height = FIG_HEIGHT_MM,
  units = "mm",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

# PDF: Cairo supports Unicode labels and uses platform sans-serif fallback.
# Do not use the default pdf() device with an unregistered "Arial" family.
# The PDF remains vector-based for graphs, labels, and annotations.
if (isTRUE(capabilities("cairo"))) {
  ggsave(
    filename = pdf_file,
    plot = fig_final,
    device = grDevices::cairo_pdf,
    family = "sans",
    width = FIG_WIDTH_MM,
    height = FIG_HEIGHT_MM,
    units = "mm",
    bg = "white"
  )
} else {
  warning(
    "Cairo PDF device is unavailable on this R installation. ",
    "PNG and TIFF were saved; PDF export skipped to avoid CID-font errors. ",
    "On Windows, use a standard R build with Cairo support or update R."
  )
}

cat("\nSaved files in:\n")
cat(normalizePath(out_dir, mustWork = FALSE), "\n\n")

cat("Figure PNG:\n")
cat(normalizePath(png_file, mustWork = FALSE), "\n")

cat("Figure TIFF:\n")
cat(normalizePath(tiff_file, mustWork = FALSE), "\n")

if (file.exists(pdf_file)) {
  cat("Figure PDF:\n")
  cat(normalizePath(pdf_file, mustWork = FALSE), "\n")
} else {
  cat("Figure PDF: not generated (Cairo support unavailable).\n")
}

cat("Statistics Excel:\n")
cat(normalizePath(stats_xlsx, mustWork = FALSE), "\n")
# -------------------------------------------------------------------------
# IMPORTANT INTERPRETATION NOTES (no underlying results changed):
# (1) Panel (c): SOC/STP lines are rescaled independently to the Ca bar axis.
#     Their positions are VISUAL overlays, not actual Ca units; explain this
#     explicitly in the caption or replace with separate correctly scaled plots.
# (2) Panels (a), (b), (f) juxtapose variables with different physical units on
#     one visual y-axis. For peer review, consider independent scales/panels.
# (3) Replace panel (g) photo placeholders with the actual high-resolution
#     tall CK/GP/PEG plant montage before publication.
# -------------------------------------------------------------------------


# =============================================================================
# 13. FIGURE 3 — MINIMAL SOURCE DATA EXPORT | ONE WORKSHEET
# =============================================================================
# One compact table contains the actual replicate observations and plotted
# summaries for every numerical variable in panels (a)-(f). A second small
# table contains only the adjusted P values for the significance brackets.
# Panel (g) is photographic; supply the original image separately.
# No figure calculations, statistics, or plotted panels are changed here.
# =============================================================================

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}

source_data_file <- file.path(out_dir, "Source_Data_Fig3.xlsx")
sd_sheet <- "Fig 3 Source Data"
sd_wb <- openxlsx::createWorkbook(creator = "Figure 3 source data")
openxlsx::addWorksheet(sd_wb, sd_sheet, gridLines = FALSE)

# Map each numerical variable to the panel in which it appears.
source_panel_map <- c(
  Sol_Na = "(a)", Ex_Na = "(a)",
  Sol_Ca = "(b)", Ex_Ca = "(b)", CaCO3 = "(b)",
  Reactive_Ca = "(c)", Ca_F3 = "(c)", SOC = "(c)", STP = "(c)",
  HCO3 = "(d)", CO3 = "(d)", pH = "(d)",
  SO4 = "(e)", Cl = "(e)",
  SAR = "(f)", Biomass = "(f)"
)
source_vars <- names(source_panel_map)[names(source_panel_map) %in% names(df_raw)]
if (length(source_vars) == 0L) stop("No Figure 3 source variables were found in df_raw.")

# Preserve the existing figure's unit descriptions; uncertain units are flagged
# rather than silently guessed or corrected in the source workbook.
source_units <- c(
  Sol_Na = "mmol L^-1 (figure label)",
  Ex_Na = "cmol_c kg^-1 (figure label)",
  Sol_Ca = "mmol L^-1 (figure label)",
  Ex_Ca = "cmol_c kg^-1 (figure label)",
  Reactive_Ca = "cmol_c kg^-1",
  Ca_F3 = "cmol_c kg^-1",
  HCO3 = "meq L^-1 (figure label)",
  CO3 = "meq L^-1 (figure label)",
  SO4 = "mmol L^-1 (figure label)",
  Cl = "mmol L^-1 (figure label)",
  SAR = "unitless",
  Biomass = "confirm from Methods",
  CaCO3 = "confirm from Methods",
  SOC = "confirm from Methods",
  STP = "confirm from Methods",
  pH = "unitless"
)

# The summary source is the same object used to draw each plotted variable.
get_source_summary <- function(variable) {
  if (variable %in% c("SOC", "STP")) {
    return(overlay_stats %>% dplyr::filter(.data$variable == .env$variable))
  }
  if (variable == "CaCO3") {
    return(p2_caco3 %>% dplyr::mutate(variable = "CaCO3"))
  }
  if (variable == "pH") {
    if (is.null(pH_stats)) return(NULL)
    return(pH_stats %>% dplyr::mutate(variable = "pH"))
  }
  bar_stats %>% dplyr::filter(.data$variable == .env$variable)
}

# Keep enough replicate columns for the largest observed treatment group.
max_rep <- max(3L, max(vapply(source_vars, function(v) {
  max(vapply(c("CK", "GP", "PEG"), function(trt) {
    as.integer(sum(as.character(df_raw$treatment) == trt & !is.na(df_raw[[v]])))
  }, integer(1)), na.rm = TRUE)
}, integer(1))), na.rm = TRUE)
rep_names <- paste0("Replicate_", seq_len(max_rep))

make_source_rows <- function(variable) {
  sm <- get_source_summary(variable)
  if (is.null(sm) || nrow(sm) == 0L) return(NULL)
  
  # Tukey letters are included only where letters are actually shown in the
  # figure. SOC/STP are line overlays and do not have displayed letter labels.
  letters_for_var <- if (variable %in% c("SOC", "STP")) {
    setNames(rep("", 3), c("CK", "GP", "PEG"))
  } else {
    calc_cld_overlay(df_raw, variable)
  }
  
  do.call(rbind, lapply(c("CK", "GP", "PEG"), function(trt) {
    sm_trt <- sm[as.character(sm$treatment) == trt, , drop = FALSE]
    raw_vals <- df_raw %>%
      dplyr::filter(as.character(.data$treatment) == trt) %>%
      dplyr::pull(dplyr::all_of(variable))
    raw_vals <- as.numeric(raw_vals[!is.na(raw_vals)])
    rep_vals <- rep(NA_real_, max_rep)
    if (length(raw_vals) > 0L) rep_vals[seq_along(raw_vals)] <- raw_vals
    
    rep_df <- as.data.frame(as.list(rep_vals))
    names(rep_df) <- rep_names
    
    get_num <- function(column) {
      if (nrow(sm_trt) == 0L || !column %in% names(sm_trt)) return(NA_real_)
      value <- sm_trt[[column]][1]
      if (length(value) == 0L || is.na(value) || !is.finite(value)) return(NA_real_)
      as.numeric(value)
    }
    
    n_value <- if (nrow(sm_trt) > 0L) as.integer(sm_trt$n[1]) else NA_integer_
    letter <- unname(letters_for_var[trt])
    if (length(letter) == 0L || is.na(letter)) letter <- ""
    
    fixed_cols <- data.frame(
      Panel = unname(source_panel_map[[variable]]),
      Variable = variable,
      Unit = unname(source_units[[variable]]),
      Treatment = trt,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    summary_cols <- data.frame(
      n = n_value,
      Mean = get_num("mean"),
      SD = get_num("sd"),
      SE = get_num("se"),
      Tukey_letters = letter,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    cbind(fixed_cols, rep_df, summary_cols)
  }))
}

source_data <- dplyr::bind_rows(lapply(source_vars, make_source_rows))
if (nrow(source_data) == 0L) stop("The compact Figure 3 source-data table is empty.")

# Compact bracket table: one row per bracket actually drawn in panels (a)-(f).
bracket_specs <- data.frame(
  Panel = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
  Tested_variable = c("Sol_Na", "Sol_Ca", "Reactive_Ca", "HCO3", "SO4", "SAR"),
  stringsAsFactors = FALSE
)
bracket_data <- dplyr::bind_rows(lapply(seq_len(nrow(bracket_specs)), function(i) {
  variable <- bracket_specs$Tested_variable[i]
  p_values <- c(
    get_tukey_p(df_raw, variable, "CK", "PEG"),
    get_tukey_p(df_raw, variable, "GP", "PEG")
  )
  labels <- p_to_star(p_values)
  data.frame(
    Panel = bracket_specs$Panel[i],
    Tested_variable = variable,
    Comparison = c("CK vs PEG", "GP vs PEG"),
    Tukey_adjusted_P = p_values,
    Label_as_plotted = labels,
    stringsAsFactors = FALSE
  )
})) %>%
  dplyr::filter(!is.na(.data$Tukey_adjusted_P), .data$Label_as_plotted != "")

# Simple formatting; no repeated panel-specific tables or transformed display
# coordinates. Replicate observations and their plotted summaries appear once.
sd_ncols <- ncol(source_data)
sd_title_style <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 14, textDecoration = "bold",
  fontColour = "#FFFFFF", fgFill = "#17365D", valign = "center"
)
sd_section_style <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 10.5, textDecoration = "bold",
  fontColour = "#17365D", fgFill = "#E8F0F7", valign = "center"
)
sd_header_style <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 9, textDecoration = "bold",
  fontColour = "#243344", fgFill = "#F3F5F7", wrapText = TRUE,
  valign = "center", border = "Bottom", borderColour = "#BECBD8"
)
sd_body_style <- openxlsx::createStyle(fontName = "Arial", fontSize = 9, valign = "center")
sd_number_style <- openxlsx::createStyle(numFmt = "0.##########")
sd_note_style <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 9, fontColour = "#526579",
  wrapText = TRUE, valign = "center"
)

openxlsx::mergeCells(sd_wb, sd_sheet, cols = 1:sd_ncols, rows = 1)
openxlsx::writeData(sd_wb, sd_sheet, "FIGURE 3 | SOURCE DATA", startRow = 1, startCol = 1)
openxlsx::addStyle(sd_wb, sd_sheet, sd_title_style,
                   rows = 1, cols = 1:sd_ncols, gridExpand = TRUE)
openxlsx::setRowHeights(sd_wb, sd_sheet, rows = 1, heights = 27)

source_note <- paste(
  "Replicate columns are the observations used for the plotted summaries; mean, SD, SE, n and displayed Tukey letters are included.",
  "SOC/STP values are in original units although their panel (c) lines are independently rescaled for display.",
  "Panel (g) is photographic; supply the original plant images separately. Units marked 'confirm' or 'verify' should be checked against the Methods."
)
openxlsx::mergeCells(sd_wb, sd_sheet, cols = 1:sd_ncols, rows = 2)
openxlsx::writeData(sd_wb, sd_sheet, source_note, startRow = 2, startCol = 1)
openxlsx::addStyle(sd_wb, sd_sheet, sd_note_style,
                   rows = 2, cols = 1:sd_ncols, gridExpand = TRUE)
openxlsx::setRowHeights(sd_wb, sd_sheet, rows = 2, heights = 38)

openxlsx::mergeCells(sd_wb, sd_sheet, cols = 1:sd_ncols, rows = 4)
openxlsx::writeData(sd_wb, sd_sheet, "Plotted data: panels (a)-(f)", startRow = 4, startCol = 1)
openxlsx::addStyle(sd_wb, sd_sheet, sd_section_style,
                   rows = 4, cols = 1:sd_ncols, gridExpand = TRUE)
openxlsx::writeData(sd_wb, sd_sheet, source_data, startRow = 5, startCol = 1,
                    colNames = TRUE, rowNames = FALSE, headerStyle = sd_header_style)
openxlsx::setRowHeights(sd_wb, sd_sheet, rows = 5, heights = 32)

source_body_rows <- seq.int(6L, 5L + nrow(source_data))
openxlsx::addStyle(sd_wb, sd_sheet, sd_body_style,
                   rows = source_body_rows, cols = seq_len(ncol(source_data)),
                   gridExpand = TRUE, stack = TRUE)
source_numeric_cols <- which(vapply(source_data, is.numeric, logical(1)))
if (length(source_numeric_cols) > 0L) {
  openxlsx::addStyle(sd_wb, sd_sheet, sd_number_style,
                     rows = source_body_rows, cols = source_numeric_cols,
                     gridExpand = TRUE, stack = TRUE)
}

bracket_title_row <- 7L + nrow(source_data)
openxlsx::mergeCells(sd_wb, sd_sheet,
                     cols = 1:ncol(bracket_data), rows = bracket_title_row)
openxlsx::writeData(sd_wb, sd_sheet, "Significance brackets: panels (a)-(f)",
                    startRow = bracket_title_row, startCol = 1)
openxlsx::addStyle(sd_wb, sd_sheet, sd_section_style,
                   rows = bracket_title_row, cols = 1:ncol(bracket_data), gridExpand = TRUE)
bracket_header_row <- bracket_title_row + 1L
openxlsx::writeData(sd_wb, sd_sheet, bracket_data,
                    startRow = bracket_header_row, startCol = 1,
                    colNames = TRUE, rowNames = FALSE, headerStyle = sd_header_style)
if (nrow(bracket_data) > 0L) {
  bracket_body_rows <- seq.int(bracket_header_row + 1L, bracket_header_row + nrow(bracket_data))
  openxlsx::addStyle(sd_wb, sd_sheet, sd_body_style,
                     rows = bracket_body_rows, cols = seq_len(ncol(bracket_data)),
                     gridExpand = TRUE, stack = TRUE)
  p_col <- which(names(bracket_data) == "Tukey_adjusted_P")
  openxlsx::addStyle(sd_wb, sd_sheet, sd_number_style,
                     rows = bracket_body_rows, cols = p_col,
                     gridExpand = TRUE, stack = TRUE)
}

openxlsx::setColWidths(sd_wb, sd_sheet, cols = 1:sd_ncols, widths = "auto")
openxlsx::setColWidths(sd_wb, sd_sheet, cols = 1, widths = 8)
openxlsx::setColWidths(sd_wb, sd_sheet, cols = 2, widths = 19)
openxlsx::setColWidths(sd_wb, sd_sheet, cols = 3, widths = 31)
openxlsx::setColWidths(sd_wb, sd_sheet, cols = 4, widths = 11)
openxlsx::setColWidths(sd_wb, sd_sheet, cols = 5:(4 + max_rep), widths = 12)
openxlsx::setColWidths(sd_wb, sd_sheet, cols = (5 + max_rep):(8 + max_rep), widths = 12)
openxlsx::setColWidths(sd_wb, sd_sheet, cols = 9 + max_rep, widths = 15)
openxlsx::freezePane(sd_wb, sd_sheet, firstActiveRow = 6)
openxlsx::pageSetup(sd_wb, sd_sheet, orientation = "landscape", fitToWidth = 1, fitToHeight = 0)

openxlsx::saveWorkbook(sd_wb, source_data_file, overwrite = TRUE)
if (!file.exists(source_data_file) || is.na(file.info(source_data_file)$size) ||
    file.info(source_data_file)$size <= 0) {
  stop("Figure 3 source-data workbook was not saved: ", source_data_file)
}
created_sheets <- openxlsx::getSheetNames(source_data_file)
if (!identical(created_sheets, sd_sheet)) {
  stop("Expected one source-data worksheet; found: ", paste(created_sheets, collapse = ", "))
}

cat("\nFIGURE 3 MINIMAL SOURCE DATA EXPORT COMPLETE\n")
cat("File:", normalizePath(source_data_file, mustWork = TRUE), "\n")
cat("Worksheet:", created_sheets, "\n")
cat("Data rows:", nrow(source_data), "\n")
cat("Bracket rows:", nrow(bracket_data), "\n")
cat("Workbook size (bytes):", file.info(source_data_file)$size, "\n")






