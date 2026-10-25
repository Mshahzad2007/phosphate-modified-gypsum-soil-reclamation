# ================================================================
# FIGURE 4 | NATURE COMMUNICATIONS
# FINAL VERSION — ORIGINAL EQUATIONS PRESERVED
# ================================================================
#
# Input:
#   Conventional_Treatments.xlsx
#   Sheet: data
#
# Treatments: CK, GP, PEG
# P+GP and other treatments are excluded.
#
# Panel (a): C/P/Ca fractions
# Panel (b): Aggregates and MWD
# Panel (c): CAI and MDR
# Panel (d): Reclamation radar
# Panel (e): Carbon radar
# Panel (f): Ca-regulation cascade
#
# All scientific calculations are retained from the
# supplied working script.
#
# ================================================================

# 1. PACKAGES AND PATHS -------------------------------------------

packages <- c(
  "readxl", "dplyr", "tidyr",
  "ggplot2", "patchwork", "openxlsx"
)

missing_packages <- packages[
  !vapply(
    packages, requireNamespace,
    logical(1), quietly = TRUE
  )
]

if (length(missing_packages)) {
  stop(
    "Install packages: ",
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
  library(grid)
})

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
sheet_in <- "data"

out_dir <- file.path("outputs", "Fig4_NC_Final_Corrected")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

fig_width_in <- 6.70
fig_height_in <- 8.70
fig_font <- "Arial"

allowed_treatments <- c("CK", "GP", "PEG")

show_local_axis_labels <- TRUE
use_confirmed_silt_reference <- TRUE

# Original display units: verify against laboratory records.
unit_C <- "C (%)"
unit_P <- "P (mg kg⁻¹)"
unit_Ca <- "Ca (cmolc kg⁻¹)"

# 2. COLOURS -------------------------------------------------------

trt_cols <- c(
  CK = "#000000",
  GP = "#008080",
  PEG = "#62C2CC"
)

frac_cols <- c(
  "Macro (250–2000 μm)" = "#0B1F3A",
  "Micro (53–250 μm)" = "#008080",
  "Silt+clay (<53 μm)" = "#D3D3D3"
)

cascade_cols <- c(
  Ca_F3 = "#2B5BA8",
  Ex_Ca = "#E8701A",
  Ca = "#D4A017",
  CaCO3 = "#A0522D",
  CO3 = "#2E8B57",
  HCO3 = "#52B788",
  SO4 = "#20B2AA"
)

cascade_lwd <- c(
  Ca_F3 = 1.175,
  Ex_Ca = 1.025,
  Ca = 0.875,
  CaCO3 = 0.775,
  CO3 = 0.900,
  HCO3 = 0.825,
  SO4 = 0.825
)

cascade_lty <- c(
  Ca_F3 = "solid",
  Ex_Ca = "solid",
  Ca = "solid",
  CaCO3 = "dashed",
  CO3 = "dashed",
  HCO3 = "dashed",
  SO4 = "dotdash"
)

cascade_shape <- c(
  Ca_F3 = 21,
  Ex_Ca = 22,
  Ca = 23,
  CaCO3 = 24,
  CO3 = 25,
  HCO3 = 21,
  SO4 = 22
)

# 3. HELPER FUNCTIONS ----------------------------------------------

clamp01 <- function(x) pmax(0, pmin(1, x))

clean_text <- function(x) {
  z <- as.character(x)
  z <- gsub("[\u00A0\u2007\u202F]", " ", z, perl = TRUE)
  z <- gsub("[\u200B-\u200D\uFEFF]", "", z, perl = TRUE)
  trimws(z)
}

clean_replicate <- function(x) {
  z <- clean_text(x)
  z <- sub(
    "^(?:replicate|rep|r)\\s*[-_#]*\\s*0*([0-9]+)$",
    "\\1", z, ignore.case = TRUE, perl = TRUE
  )
  sub("^0+([0-9]+)$", "\\1", z)
}

find_col <- function(nm, options) {
  key <- tolower(gsub("[^a-z0-9]", "", nm))
  ref <- tolower(gsub("[^a-z0-9]", "", options))
  ix <- which(key %in% ref)
  if (!length(ix)) return(NA_character_)
  nm[ix[1]]
}

canon_var <- function(x) {
  k <- tolower(gsub("[^a-z0-9]", "", as.character(x)))
  
  dplyr::case_when(
    k == "ph" ~ "pH",
    k == "ec" ~ "EC",
    k == "sar" ~ "SAR",
    k == "esp" ~ "ESP",
    k %in% c("na", "solna", "solublena") ~ "Na",
    k %in% c(
      "exna", "ena", "exchangena", "exchangeablena"
    ) ~ "Ex_Na",
    k %in% c("ca", "solca", "solubleca") ~ "Ca",
    k %in% c(
      "exca", "eca", "exchangeca", "exchangeableca"
    ) ~ "Ex_Ca",
    k == "cl" ~ "Cl",
    k == "so4" ~ "SO4",
    k == "co3" ~ "CO3",
    k == "hco3" ~ "HCO3",
    k == "caco3" ~ "CaCO3",
    k %in% c("reactiveca", "rc") ~ "Reactive_Ca",
    k %in% c("caf3", "ca3", "cf", "cafraction3") ~ "Ca_F3",
    k %in% c("sap", "availablep", "olsenp") ~ "SAP",
    k %in% c(
      "stp", "totalp", "soiltotalp", "soiltotalphosphorus"
    ) ~ "STP",
    k %in% c("soc", "soilorganiccarbon") ~ "SOC",
    k %in% c("pomc", "poc") ~ "POM_C",
    k %in% c("maomc", "maoc") ~ "MAOM_C",
    k %in% c("pomp", "pop") ~ "POM_P",
    k %in% c("maomp", "maop") ~ "MAOM_P",
    k %in% c("pomcaf3", "pomcf", "pcf") ~ "POM_Ca_F3",
    k %in% c("maomcaf3", "maomcf", "mcf") ~ "MAOM_Ca_F3",
    grepl(
      "^macro(aggregates?|aggs?|[0-9]|$)",
      k, perl = TRUE
    ) ~ "Macro",
    grepl(
      "^micro(aggregates?|aggs?|[0-9]|$)",
      k, perl = TRUE
    ) ~ "Micro",
    grepl("^silt(and|plus)?clay", k) ~ "Silt_clay",
    k %in% c("mwd", "meanweightdiameter") ~ "MWD",
    TRUE ~ as.character(x)
  )
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

get_tukey_p <- function(data, response, g1, g2) {
  
  tmp <- data %>%
    transmute(
      treatment = factor(
        Treatments, levels = allowed_treatments
      ),
      response = .data[[response]]
    ) %>%
    filter(!is.na(treatment), is.finite(response))
  
  if (length(unique(tmp$treatment)) < 2) {
    return(NA_real_)
  }
  
  fit <- tryCatch(
    aov(response ~ treatment, data = tmp),
    error = function(e) NULL
  )
  
  if (is.null(fit)) return(NA_real_)
  
  tk <- tryCatch(
    TukeyHSD(fit)$treatment,
    error = function(e) NULL
  )
  
  if (is.null(tk)) return(NA_real_)
  
  comparisons <- c(
    paste(g2, g1, sep = "-"),
    paste(g1, g2, sep = "-")
  )
  
  ix <- match(comparisons, rownames(tk))
  ix <- ix[!is.na(ix)]
  
  if (!length(ix)) return(NA_real_)
  
  unname(tk[ix[1], "p adj"])
}

score_mib <- function(x) {
  r <- range(x, na.rm = TRUE)
  if (!all(is.finite(r)) || diff(r) == 0) {
    return(rep(0.5, length(x)))
  }
  (x - r[1]) / diff(r)
}

score_lib <- function(x) {
  r <- range(x, na.rm = TRUE)
  if (!all(is.finite(r)) || diff(r) == 0) {
    return(rep(0.5, length(x)))
  }
  (r[2] - x) / diff(r)
}

summarise_var <- function(data, variable) {
  data %>%
    group_by(Treatments) %>%
    summarise(
      n = sum(!is.na(.data[[variable]])),
      mean = mean(.data[[variable]], na.rm = TRUE),
      sd = sd(.data[[variable]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      sd = replace_na(sd, 0),
      se = sd / sqrt(n),
      variable = variable
    )
}

theme_nc <- function(base_size = 11) {
  theme_classic(
    base_size = base_size,
    base_family = fig_font
  ) +
    theme(
      text = element_text(family = fig_font),
      plot.background = element_rect(
        fill = "white", colour = NA
      ),
      panel.background = element_rect(
        fill = "white", colour = NA
      ),
      axis.line = element_line(
        colour = "black", linewidth = 0.45
      ),
      axis.text = element_text(
        colour = "black", size = 9
      ),
      strip.background = element_blank(),
      legend.title = element_blank()
    )
}

# 4. READ DATA — CK, GP AND PEG ONLY ------------------------------

if (!file.exists(file_in)) {
  stop("Excel file not found: ", file_in)
}

raw_xl <- read_excel(file_in, sheet = sheet_in)

ct <- find_col(
  names(raw_xl), c("Treatments", "Treatment", "Trt")
)

cr <- find_col(
  names(raw_xl), c("Replicates", "Replicate", "Rep")
)

cv <- find_col(
  names(raw_xl),
  c("response.varibale", "response.variable", "Variable")
)

cval <- find_col(
  names(raw_xl), c("value", "Value", "Values")
)

if (any(is.na(c(ct, cr, cv, cval)))) {
  stop(
    "Missing required Excel columns. Found: ",
    paste(names(raw_xl), collapse = ", ")
  )
}

# Do NOT rename P+GP as PEG.
raw_treatments <- toupper(clean_text(raw_xl[[ct]]))

keep_rows <- !is.na(raw_treatments) &
  raw_treatments %in% allowed_treatments

excluded <- unique(
  raw_treatments[!keep_rows & !is.na(raw_treatments)]
)

if (length(excluded)) {
  message(
    "Excluded treatments: ",
    paste(excluded, collapse = ", ")
  )
}

raw_xl <- raw_xl[keep_rows, , drop = FALSE]

if (!nrow(raw_xl)) {
  stop("No CK, GP or PEG observations found.")
}

missing_trt <- setdiff(
  allowed_treatments,
  unique(toupper(clean_text(raw_xl[[ct]])))
)

if (length(missing_trt)) {
  stop(
    "Missing treatments: ",
    paste(missing_trt, collapse = ", ")
  )
}

dat <- raw_xl %>%
  transmute(
    Treatments = factor(
      toupper(clean_text(.data[[ct]])),
      levels = allowed_treatments
    ),
    Replicates = factor(
      clean_replicate(.data[[cr]])
    ),
    response.variable = clean_text(.data[[cv]]),
    value = suppressWarnings(
      as.numeric(.data[[cval]])
    )
  ) %>%
  filter(
    !is.na(Treatments),
    !is.na(Replicates),
    !is.na(response.variable),
    is.finite(value)
  )

dat_long <- dat %>%
  transmute(
    Treatments,
    Replicates,
    original_variable = response.variable,
    variable = canon_var(response.variable),
    value
  )

df <- dat_long %>%
  group_by(Treatments, Replicates, variable) %>%
  summarise(
    value = mean(value),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  )

# IMPORTANT:
# Ex_Ca is NOT required.
# Keep the original Reactive_Ca variable and equations.

required_core <- c(
  "POM_C", "MAOM_C",
  "POM_P", "MAOM_P",
  "POM_Ca_F3", "MAOM_Ca_F3",
  "Reactive_Ca", "CaCO3"
)

missing_core <- setdiff(required_core, names(df))

if (length(missing_core)) {
  stop(
    "Missing variables: ",
    paste(missing_core, collapse = ", ")
  )
}

# ================================================================
# ORIGINAL EQUATIONS — DO NOT MODIFY
# ================================================================

df <- df %>%
  mutate(
    CAI = ifelse(
      Reactive_Ca != 0,
      CaCO3 / Reactive_Ca,
      NA_real_
    ),
    MDR = ifelse(
      POM_C != 0,
      MAOM_C / POM_C,
      NA_real_
    )
  )

# 5. PANEL (a) — C/P/Ca FRACTIONS --------------------------------

frac_vars <- c(
  "POM_C", "MAOM_C",
  "POM_P", "MAOM_P",
  "POM_Ca_F3", "MAOM_Ca_F3"
)

frac_stats <- bind_rows(
  lapply(frac_vars, function(v) summarise_var(df, v))
) %>%
  mutate(
    pool = ifelse(
      grepl("^POM", variable), "POM", "MAOM"
    ),
    analyte = case_when(
      variable %in% c("POM_C", "MAOM_C") ~ "C",
      variable %in% c("POM_P", "MAOM_P") ~ "P",
      TRUE ~ "Ca-F3"
    ),
    analyte = factor(
      analyte,
      levels = c("C", "P", "Ca-F3")
    ),
    x_num = ifelse(pool == "POM", 1, 2)
  )

axis_lab_A <- data.frame(
  analyte = factor(
    rep(c("C", "P", "Ca-F3"), each = 2),
    levels = c("C", "P", "Ca-F3")
  ),
  x_plot = rep(c(1, 2), 3),
  label = c(
    "POC", "MAOC",
    "POP", "MAOP",
    "PCF", "MCF"
  )
)

comp_A <- data.frame(
  g1 = c("CK", "GP"),
  g2 = c("PEG", "PEG"),
  rank = c(1, 2)
)

facet_ymax_A <- frac_stats %>%
  group_by(analyte) %>%
  summarise(
    ymax = max(mean + se, na.rm = TRUE),
    .groups = "drop"
  )

offset_A <- c(
  CK = -0.19,
  GP = 0,
  PEG = 0.19
)

sig_A <- frac_stats %>%
  distinct(variable, analyte, x_num) %>%
  left_join(facet_ymax_A, by = "analyte") %>%
  tidyr::crossing(comp_A) %>%
  rowwise() %>%
  mutate(
    p = get_tukey_p(df, variable, g1, g2),
    label = p_to_star(p),
    x1 = x_num + offset_A[g1],
    x2 = x_num + offset_A[g2],
    xm = (x1 + x2) / 2,
    y_base = max(
      frac_stats$mean[
        frac_stats$variable == variable
      ] +
        frac_stats$se[
          frac_stats$variable == variable
        ],
      na.rm = TRUE
    ),
    y = y_base +
      0.100 * ymax +
      (rank - 1) * 0.165 * ymax,
    tip_h = 0.022 * ymax
  ) %>%
  ungroup()

unit_labs <- c(
  C = unit_C,
  P = unit_P,
  "Ca-F3" = unit_Ca
)

pA <- ggplot(
  frac_stats,
  aes(
    x = x_num,
    y = mean,
    fill = Treatments
  )
) +
  geom_col(
    position = position_dodge(width = 0.58),
    width = 0.36,
    colour = "grey35",
    linewidth = 0.20,
    alpha = 0.88
  ) +
  geom_errorbar(
    aes(
      ymin = mean - se,
      ymax = mean + se,
      group = Treatments
    ),
    position = position_dodge(width = 0.58),
    width = 0.09,
    linewidth = 0.26
  ) +
  geom_segment(
    data = sig_A,
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    linewidth = 0.48
  ) +
  geom_segment(
    data = sig_A,
    aes(
      x = x1, xend = x1,
      y = y - tip_h, yend = y
    ),
    inherit.aes = FALSE,
    linewidth = 0.48
  ) +
  geom_segment(
    data = sig_A,
    aes(
      x = x2, xend = x2,
      y = y - tip_h, yend = y
    ),
    inherit.aes = FALSE,
    linewidth = 0.48
  ) +
  geom_text(
    data = sig_A,
    aes(
      x = xm,
      y = y + 0.016 * ymax,
      label = label
    ),
    inherit.aes = FALSE,
    size = 3.6,
    vjust = 0
  ) +
  geom_text(
    data = axis_lab_A,
    aes(
      x = x_plot,
      y = -Inf,
      label = label
    ),
    inherit.aes = FALSE,
    size = 3.15,
    vjust = 1.55
  ) +
  facet_wrap(
    ~analyte,
    nrow = 1,
    scales = "free_y",
    labeller = as_labeller(unit_labs)
  ) +
  scale_fill_manual(
    values = trt_cols,
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(0.42, 2.58),
    breaks = c(1, 2),
    labels = c("", "")
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.35))
  ) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL) +
  theme_nc(11) +
  theme(
    strip.text = element_text(
      size = 8.4,
      margin = margin(b = 2)
    ),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 8.8),
    legend.position = "none",
    plot.margin = margin(5, 5, 2, 7)
  )

# 6. PANEL (b) — AGGREGATES + MWD --------------------------------

agg_levels <- c(
  "Macro", "Micro", "Silt+clay"
)

dat_agg <- dat_long %>%
  filter(
    variable %in% c(
      "Macro", "Micro", "Silt_clay"
    )
  ) %>%
  transmute(
    Treatments,
    Replicates,
    response.variable = recode(
      as.character(variable),
      "Silt_clay" = "Silt+clay"
    ),
    value
  ) %>%
  group_by(
    Treatments,
    Replicates,
    response.variable
  ) %>%
  summarise(
    value = mean(value),
    .groups = "drop"
  )

# Measured values retained from the original working script.
# Verify them against the laboratory source-data records.

confirmed_silt <- data.frame(
  Treatment = rep(
    c("CK", "GP", "PEG"), each = 3
  ),
  Replicate = rep(
    c("1", "2", "3"), times = 3
  ),
  value = c(
    35.80, 36.56, 35.71,
    11.55, 11.44, 11.83,
    4.04, 3.82, 4.36
  )
)

excel_silt <- dat_agg %>%
  filter(response.variable == "Silt+clay") %>%
  transmute(
    Treatment = as.character(Treatments),
    Replicate = as.character(Replicates),
    Excel_value = value
  )

crosscheck_silt <- inner_join(
  excel_silt,
  confirmed_silt,
  by = c("Treatment", "Replicate")
)

if (any(
  abs(
    crosscheck_silt$Excel_value -
    crosscheck_silt$value
  ) > 0.02,
  na.rm = TRUE
)) {
  stop(
    "Excel silt+clay measurements conflict ",
    "with the original reference values."
  )
}

missing_silt <- anti_join(
  confirmed_silt,
  excel_silt %>%
    select(Treatment, Replicate),
  by = c("Treatment", "Replicate")
)

silt_reference_used <- missing_silt[0, ]

if (nrow(missing_silt)) {
  
  if (!use_confirmed_silt_reference) {
    stop("Missing silt+clay values in workbook.")
  }
  
  extra_silt <- missing_silt %>%
    transmute(
      Treatments = factor(
        Treatment, levels = allowed_treatments
      ),
      Replicates = factor(
        Replicate, levels = levels(dat$Replicates)
      ),
      response.variable = "Silt+clay",
      value
    )
  
  if (anyNA(extra_silt$Replicates)) {
    stop("Replicate identifiers do not match.")
  }
  
  dat_agg <- bind_rows(dat_agg, extra_silt)
  silt_reference_used <- missing_silt
}

observed_agg <- dat_agg %>%
  transmute(
    Treatment = as.character(Treatments),
    Replicate = as.character(Replicates),
    Fraction = response.variable
  ) %>%
  distinct()

if (!nrow(observed_agg)) {
  stop("No aggregate measurements detected.")
}

missing_agg <- crossing(
  observed_agg %>%
    distinct(Treatment, Replicate),
  Fraction = agg_levels
) %>%
  anti_join(
    observed_agg,
    by = c(
      "Treatment", "Replicate", "Fraction"
    )
  )

if (nrow(missing_agg)) {
  print(missing_agg)
  stop("Incomplete aggregate measurements.")
}

raw_totals <- dat_agg %>%
  group_by(Treatments, Replicates) %>%
  summarise(
    raw_total = sum(value),
    .groups = "drop"
  ) %>%
  mutate(
    deviation_from_100 = raw_total - 100
  )

write.csv(
  raw_totals,
  file.path(
    out_dir,
    "Fig4_Aggregate_recovery_QC.csv"
  ),
  row.names = FALSE
)

if (any(
  abs(raw_totals$deviation_from_100) > 5
)) {
  warning(
    "Aggregate totals differ from 100% ",
    "by more than five percentage points."
  )
}

dat_agg_pct <- dat_agg %>%
  group_by(Treatments, Replicates) %>%
  mutate(
    total_agg = sum(value),
    pct = ifelse(
      total_agg > 0,
      100 * value / total_agg,
      NA_real_
    )
  ) %>%
  ungroup() %>%
  mutate(
    fraction = recode(
      response.variable,
      "Macro" = "Macro (250–2000 μm)",
      "Micro" = "Micro (53–250 μm)",
      "Silt+clay" = "Silt+clay (<53 μm)"
    )
  )

if (any(!is.finite(dat_agg_pct$pct))) {
  stop("Invalid aggregate percentages.")
}

sum_agg <- dat_agg_pct %>%
  group_by(Treatments, fraction) %>%
  summarise(
    Mean = mean(pct),
    SD = sd(pct),
    n = n(),
    SE = SD / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    SE = replace_na(SE, 0)
  )

agg_wide <- dat_agg_pct %>%
  select(
    Treatments, Replicates,
    response.variable, pct
  ) %>%
  pivot_wider(
    names_from = response.variable,
    values_from = pct
  )

if (!all(agg_levels %in% names(agg_wide))) {
  stop("Required aggregate fractions missing.")
}

if (anyNA(agg_wide[, agg_levels])) {
  stop("Missing aggregate fraction in wide data.")
}

# ORIGINAL MWD EQUATION — UNCHANGED

dat_mwd <- agg_wide %>%
  mutate(
    MWD =
      1.125 * (.data[["Macro"]] / 100) +
      0.1515 * (.data[["Micro"]] / 100) +
      0.0265 * (.data[["Silt+clay"]] / 100)
  ) %>%
  group_by(Treatments) %>%
  summarise(
    Mean = mean(MWD),
    SD = sd(MWD),
    n = n(),
    SE = SD / sqrt(n),
    .groups = "drop"
  )

mwd_scale <- 90 / max(
  dat_mwd$Mean + dat_mwd$SE
)

xb <- c(
  CK = 1.00,
  GP = 1.42,
  PEG = 1.84
)

sum_agg <- sum_agg %>%
  mutate(
    xnum = unname(
      xb[as.character(Treatments)]
    )
  )

dat_mwd <- dat_mwd %>%
  mutate(
    xnum = unname(
      xb[as.character(Treatments)]
    )
  )

agg_stack <- sum_agg %>%
  mutate(
    fraction = factor(
      fraction, levels = names(frac_cols)
    )
  ) %>%
  group_by(Treatments) %>%
  arrange(fraction, .by_group = TRUE) %>%
  mutate(
    ymin = lag(cumsum(Mean), default = 0),
    ymax = cumsum(Mean),
    xmin = xnum - 0.13,
    xmax = xnum + 0.13
  ) %>%
  ungroup()

mwd_marks <- data.frame(
  MWD = seq(0.25, 1, by = 0.25)
) %>%
  mutate(
    y = MWD * mwd_scale,
    label = sprintf("%.2f", MWD),
    y_label = ifelse(MWD == 1, pmin(y, 96), y)
  ) %>%
  filter(y <= 100)

pB <- ggplot() +
  
  geom_rect(
    data = agg_stack,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = fraction
    ),
    colour = "grey35",
    linewidth = 0.18,
    alpha = 0.92
  ) +
  
  geom_line(
    data = dat_mwd,
    aes(
      x = xnum,
      y = Mean * mwd_scale,
      group = 1
    ),
    colour = "black",
    linewidth = 0.78
  ) +
  
  geom_point(
    data = dat_mwd,
    aes(
      x = xnum,
      y = Mean * mwd_scale
    ),
    colour = "black",
    size = 2.25
  ) +
  
  geom_errorbar(
    data = dat_mwd,
    aes(
      x = xnum,
      ymin = (Mean - SE) * mwd_scale,
      ymax = (Mean + SE) * mwd_scale
    ),
    linewidth = 0.42,
    width = 0.045
  ) +
  
  annotate(
    "segment",
    x = 2.10, xend = 2.10,
    y = 0, yend = 100,
    linewidth = 0.34
  ) +
  
  geom_segment(
    data = mwd_marks,
    aes(
      x = 2.10,
      xend = 2.13,
      y = y,
      yend = y
    ),
    linewidth = 0.28
  ) +
  
  geom_text(
    data = mwd_marks,
    aes(
      x = 2.20,
      y = y_label,
      label = label
    ),
    hjust = 0,
    size = 2.75
  ) +
  
  annotate(
    "text",
    x = 2.43,
    y = 50,
    label = "MWD",
    angle = 90,
    size = 3.4
  ) +
  
  scale_fill_manual(
    values = frac_cols,
    guide = "none"
  ) +
  
  scale_x_continuous(
    breaks = unname(xb),
    labels = names(xb),
    limits = c(0.86, 2.52),
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    breaks = c(0, 25, 50, 75, 100),
    expand = c(0, 0),
    name = "Proportion (%)"
  ) +
  
  coord_cartesian(
    ylim = c(0, 100),
    clip = "off"
  ) +
  
  labs(x = NULL) +
  
  theme_bw(
    base_size = 10,
    base_family = fig_font
  ) +
  
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      colour = "grey88",
      linewidth = 0.25
    ),
    axis.text = element_text(
      size = 9.4,
      colour = "black"
    ),
    axis.title.y = element_text(size = 10),
    legend.position = "none",
    plot.margin = margin(4, 2, 4, 5)
  )

# 7. PANEL (c) — CAI AND MDR --------------------------------------

# Both indices preserve the original raw equations.
# Each is independently min–max normalized.

idx_long <- df %>%
  select(
    Treatments, Replicates, CAI, MDR
  ) %>%
  pivot_longer(
    cols = c(CAI, MDR),
    names_to = "Index",
    values_to = "Raw"
  ) %>%
  filter(is.finite(Raw)) %>%
  group_by(Index) %>%
  mutate(
    Score = clamp01(score_mib(Raw))
  ) %>%
  ungroup()

idx_sum <- idx_long %>%
  group_by(Index, Treatments) %>%
  summarise(
    Mean = mean(Score),
    SD = sd(Score),
    n = n(),
    SE = SD / sqrt(n),
    .groups = "drop"
  )

get_index_p <- function(index_name, g1, g2) {
  get_tukey_p(
    idx_long %>%
      filter(Index == index_name),
    "Score", g1, g2
  )
}

sig_C <- data.frame(
  Index = c("CAI", "CAI", "MDR", "MDR"),
  Comparison = c(
    "CK–PEG", "GP–PEG",
    "CK–PEG", "GP–PEG"
  ),
  p = c(
    get_index_p("CAI", "CK", "PEG"),
    get_index_p("CAI", "GP", "PEG"),
    get_index_p("MDR", "CK", "PEG"),
    get_index_p("MDR", "GP", "PEG")
  )
)

sig_C$label <- p_to_star(sig_C$p)

# Separate independent axes for CAI and MDR.
x0_CAI <- 0.45
x0_MDR <- 2.05

seg_w_C <- 0.60

row_y <- c(
  CK = 2.40,
  GP = 1.70,
  PEG = 1.00
)

bar_h_C <- 0.105

idx_plot <- idx_sum %>%
  mutate(
    y = unname(
      row_y[as.character(Treatments)]
    ),
    x0 = ifelse(
      Index == "CAI", x0_CAI, x0_MDR
    ),
    x1 = x0 + Mean * seg_w_C,
    xmin = x0 + pmax(0, Mean - SE) * seg_w_C,
    xmax = x0 + pmin(1, Mean + SE) * seg_w_C
  )

axes_C <- data.frame(
  Index = c("CAI", "MDR"),
  x = c(x0_CAI, x0_MDR),
  x_end = c(
    x0_CAI + seg_w_C,
    x0_MDR + seg_w_C
  ),
  y_bottom = 0.64,
  y_top = 2.65
)

ticks_x_C <- expand.grid(
  Index = c("CAI", "MDR"),
  Score = c(0, 0.5, 1),
  stringsAsFactors = FALSE
) %>%
  mutate(
    xbase = ifelse(
      Index == "CAI", x0_CAI, x0_MDR
    ),
    x = xbase + Score * seg_w_C,
    y = 0.64,
    label = as.character(Score)
  )

ticks_y_C <- expand.grid(
  Index = c("CAI", "MDR"),
  Treatment = allowed_treatments,
  stringsAsFactors = FALSE
) %>%
  mutate(
    x = ifelse(
      Index == "CAI", x0_CAI, x0_MDR
    ),
    y = unname(row_y[Treatment])
  )

titles_C <- data.frame(
  x = c(
    x0_CAI + seg_w_C / 2,
    x0_MDR + seg_w_C / 2
  ),
  y = 3.12,
  label = c("CAI", "MDR")
)

axis_titles_C <- data.frame(
  x = c(
    x0_CAI + seg_w_C / 2,
    x0_MDR + seg_w_C / 2
  ),
  y = -0.04,
  label = c(
    "Normalized",
    "Normalized"
  )
)

# CK–PEG outer bracket, GP–PEG inner bracket.
brackets_C <- sig_C %>%
  mutate(
    xbase = ifelse(
      Index == "CAI", x0_CAI, x0_MDR
    ),
    x = xbase + seg_w_C +
      ifelse(
        Comparison == "CK–PEG",
        0.23, 0.09
      ),
    y_low = row_y[["PEG"]],
    y_high = ifelse(
      Comparison == "CK–PEG",
      row_y[["CK"]],
      row_y[["GP"]]
    ),
    y_mid = (y_low + y_high) / 2
  )

cap_C <- 0.04

pC <- ggplot() +
  
  geom_rect(
    data = idx_plot,
    aes(
      xmin = x0,
      xmax = x1,
      ymin = y - bar_h_C,
      ymax = y + bar_h_C,
      fill = Treatments
    ),
    colour = "grey35",
    linewidth = 0.22,
    alpha = 0.88
  ) +
  
  geom_segment(
    data = idx_plot,
    aes(
      x = xmin, xend = xmax,
      y = y, yend = y
    ),
    linewidth = 0.36
  ) +
  
  geom_segment(
    data = idx_plot,
    aes(
      x = xmin, xend = xmin,
      y = y - 0.05, yend = y + 0.05
    ),
    linewidth = 0.36
  ) +
  
  geom_segment(
    data = idx_plot,
    aes(
      x = xmax, xend = xmax,
      y = y - 0.05, yend = y + 0.05
    ),
    linewidth = 0.36
  ) +
  
  # Independent vertical axes
  geom_segment(
    data = axes_C,
    aes(
      x = x, xend = x,
      y = y_bottom, yend = y_top
    ),
    linewidth = 0.42
  ) +
  
  # Independent horizontal axes
  geom_segment(
    data = axes_C,
    aes(
      x = x, xend = x_end,
      y = y_bottom, yend = y_bottom
    ),
    linewidth = 0.42
  ) +
  
  # X ticks
  geom_segment(
    data = ticks_x_C,
    aes(
      x = x, xend = x,
      y = y - 0.05, yend = y + 0.05
    ),
    linewidth = 0.36
  ) +
  
  # Y ticks
  geom_segment(
    data = ticks_y_C,
    aes(
      x = x - 0.035, xend = x,
      y = y, yend = y
    ),
    linewidth = 0.36
  ) +
  
  geom_text(
    data = ticks_y_C,
    aes(
      x = x - 0.075,
      y = y,
      label = Treatment
    ),
    hjust = 1,
    size = 3.35
  ) +
  
  geom_text(
    data = titles_C,
    aes(x = x, y = y, label = label),
    size = 4.0
  ) +
  
  # Comparison brackets
  geom_segment(
    data = brackets_C,
    aes(
      x = x, xend = x,
      y = y_low, yend = y_high
    ),
    linewidth = 0.45
  ) +
  
  geom_segment(
    data = brackets_C,
    aes(
      x = x - cap_C, xend = x,
      y = y_low, yend = y_low
    ),
    linewidth = 0.45
  ) +
  
  geom_segment(
    data = brackets_C,
    aes(
      x = x - cap_C, xend = x,
      y = y_high, yend = y_high
    ),
    linewidth = 0.45
  ) +
  
  geom_text(
    data = brackets_C,
    aes(
      x = x + 0.025,
      y = y_mid,
      label = label
    ),
    hjust = 0,
    size = 3.0
  ) +
  
  geom_text(
    data = axis_titles_C,
    aes(
      x = x, y = y, label = label
    ),
    size = 2.85
  ) +
  
  scale_fill_manual(
    values = trt_cols,
    guide = "none"
  ) +
  
  scale_x_continuous(
    limits = c(0, 3.25),
    breaks = NULL,
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    limits = c(-0.16, 3.45),
    breaks = NULL,
    expand = c(0, 0)
  ) +
  
  coord_cartesian(clip = "off") +
  
  labs(x = NULL, y = NULL) +
  
  theme_nc(11) +
  
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.margin = margin(4, 5, 4, 5)
  )

if (show_local_axis_labels) {
  pC <- pC +
    geom_text(
      data = ticks_x_C,
      aes(
        x = x,
        y = y - 0.22,
        label = label
      ),
      size = 2.75
    )
}

# 8. PANELS (d,e) — RADAR PLOTS -----------------------------------

df_radar <- df

if ("Reactive_Ca" %in% names(df_radar)) {
  df_radar$RC <- df_radar$Reactive_Ca
}

if ("Ex_Na" %in% names(df_radar)) {
  df_radar$Ex.Na <- df_radar$Ex_Na
}

# Optional: not required for Fig. 4.
if ("Ex_Ca" %in% names(df_radar)) {
  df_radar$Ex.Ca <- df_radar$Ex_Ca
}

agg_r <- dat_agg_pct %>%
  select(
    Treatments, Replicates,
    response.variable, pct
  ) %>%
  pivot_wider(
    names_from = response.variable,
    values_from = pct
  )

df_radar <- df_radar %>%
  select(
    -any_of(
      c("Macro", "Micro", "Silt+clay", "Silt_clay")
    )
  ) %>%
  left_join(
    agg_r,
    by = c("Treatments", "Replicates")
  )

label_var <- function(x) {
  labels <- c(
    "Ex.Na" = "Ex.Na",
    "Ex.Ca" = "Ex.Ca",
    "Cl" = "Cl⁻",
    "SO4" = "SO₄²⁻",
    "HCO3" = "HCO₃⁻",
    "CO3" = "CO₃²⁻",
    "CaCO3" = "CaCO₃",
    "POM_C" = "POC",
    "MAOM_C" = "MAOC",
    "POM_P" = "POP",
    "MAOM_P" = "MAOP",
    "POM_Ca_F3" = "PCF",
    "MAOM_Ca_F3" = "MCF"
  )
  
  out <- as.character(x)
  ix <- match(out, names(labels))
  has_label <- !is.na(ix)
  
  out[has_label] <- unname(
    labels[ix[has_label]]
  )
  
  out
}

make_radar_scores <- function(data, vars, low_better) {
  
  vars <- intersect(vars, names(data))
  
  if (length(vars) < 3) {
    stop("Insufficient variables for radar plot.")
  }
  
  tmp <- data %>%
    select(
      Treatments, Replicates, all_of(vars)
    )
  
  for (v in vars) {
    tmp[[paste0("S_", v)]] <- clamp01(
      if (v %in% low_better) {
        score_lib(tmp[[v]])
      } else {
        score_mib(tmp[[v]])
      }
    )
  }
  
  score_cols <- paste0("S_", vars)
  
  means <- tmp %>%
    group_by(Treatments) %>%
    summarise(
      across(
        all_of(score_cols),
        ~mean(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
  
  valid <- score_cols[
    colSums(!is.na(means[score_cols])) > 0
  ]
  
  vars <- sub("^S_", "", valid)
  
  rl <- means %>%
    select(
      Treatments, all_of(valid)
    ) %>%
    pivot_longer(
      -Treatments,
      names_to = "Indicator",
      values_to = "Score"
    ) %>%
    mutate(
      Indicator = sub("^S_", "", Indicator),
      Indicator = factor(
        Indicator, levels = vars
      ),
      x = as.numeric(Indicator)
    ) %>%
    filter(is.finite(Score))
  
  closing <- rl %>%
    group_by(Treatments) %>%
    arrange(x, .by_group = TRUE) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(x = length(vars) + 1)
  
  bind_rows(rl, closing)
}

make_radar_plot <- function(data, vars, low_better) {
  
  rd <- make_radar_scores(
    data, vars, low_better
  )
  
  vf <- levels(rd$Indicator)
  
  ggplot(
    rd,
    aes(
      x = x,
      y = Score,
      group = Treatments,
      colour = Treatments,
      fill = Treatments
    )
  ) +
    geom_polygon(
      alpha = 0.10,
      linewidth = 0.65
    ) +
    geom_line(
      linewidth = 0.75
    ) +
    geom_point(
      shape = 21,
      size = 2.50,
      stroke = 0.60
    ) +
    coord_polar(
      start = 0,
      clip = "off"
    ) +
    scale_colour_manual(
      values = trt_cols, guide = "none"
    ) +
    scale_fill_manual(
      values = trt_cols, guide = "none"
    ) +
    scale_x_continuous(
      breaks = seq_along(vf),
      labels = label_var(vf),
      limits = c(1, length(vf) + 1)
    ) +
    scale_y_continuous(
      limits = c(0, 1.02),
      breaks = c(0, 0.5, 1),
      labels = NULL,
      expand = c(0, 0)
    ) +
    labs(x = NULL, y = NULL) +
    theme_minimal(
      base_size = 11,
      base_family = fig_font
    ) +
    theme(
      axis.text.x = element_text(
        size = 8.4,
        colour = "black"
      ),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_line(
        colour = "grey85",
        linewidth = 0.27
      ),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      aspect.ratio = 1,
      plot.margin = margin(8, 9, 8, 9)
    )
}

low_rec <- c(
  "pH", "EC", "SAR",
  "Na", "Ex.Na", "Cl",
  "SO4", "HCO3", "CO3", "CaCO3"
)

pD <- make_radar_plot(
  df_radar,
  c(
    "pH", "EC", "SAR", "Na",
    "Ex.Na", "Cl", "SO4",
    "HCO3", "CO3", "CaCO3",
    "RC", "Ex.Ca", "CAI"
  ),
  low_rec
)

pE <- make_radar_plot(
  df_radar,
  c(
    "SOC", "POM_C", "MAOM_C",
    "POM_P", "MAOM_P",
    "POM_Ca_F3", "MAOM_Ca_F3",
    "Macro", "Micro", "Silt+clay",
    "MWD", "SAP", "STP", "MDR"
  ),
  low_rec
)

# 9. PANEL (f) — CALCIUM REGULATION CASCADE -----------------------

vars_f <- c(
  "Ca_F3", "Ex_Ca", "Ca",
  "CaCO3", "CO3", "HCO3", "SO4"
)

var_display <- c(
  Ca_F3 = "CF",
  Ex_Ca = "Ex.Ca",
  Ca = "Sol.Ca",
  CaCO3 = "CaCO₃",
  CO3 = "CO₃²⁻",
  HCO3 = "HCO₃⁻",
  SO4 = "SO₄²⁻"
)

sum_casc <- dat_long %>%
  filter(variable %in% vars_f) %>%
  group_by(Treatments, variable) %>%
  summarise(
    Mean = mean(value),
    SD = sd(value),
    n = n(),
    SE = SD / sqrt(n),
    .groups = "drop"
  )

missing_cascade <- setdiff(
  vars_f, unique(sum_casc$variable)
)

if (length(missing_cascade)) {
  warning(
    "Optional cascade series not found: ",
    paste(missing_cascade, collapse = ", ")
  )
}

# ORIGINAL NORMALIZATION AND FOLD-CHANGE EQUATIONS

sum_norm_c <- sum_casc %>%
  group_by(variable) %>%
  mutate(
    minM = min(Mean),
    maxM = max(Mean),
    rng = maxM - minM,
    
    CKMean = Mean[
      as.character(Treatments) == "CK"
    ][1],
    
    Norm = ifelse(
      rng == 0,
      0.5,
      (Mean - minM) / rng
    ),
    
    NormLo = clamp01(
      ifelse(
        rng == 0,
        0.5,
        (Mean - SE - minM) / rng
      )
    ),
    
    NormHi = clamp01(
      ifelse(
        rng == 0,
        0.5,
        (Mean + SE - minM) / rng
      )
    ),
    
    FoldChange = ifelse(
      CKMean != 0,
      Mean / CKMean,
      NA_real_
    ),
    
    FoldLabel = ifelse(
      as.character(Treatments) == "CK",
      "",
      ifelse(
        is.finite(FoldChange),
        paste0(
          ifelse(
            abs(FoldChange) >= 10,
            sprintf("%.1f", FoldChange),
            sprintf("%.2f", FoldChange)
          ),
          "×"
        ),
        ""
      )
    )
  ) %>%
  ungroup() %>%
  mutate(
    x_pos = as.numeric(Treatments),
    VarLabel = factor(
      var_display[variable],
      levels = unname(var_display[vars_f])
    )
  )

active_vars <- intersect(
  vars_f, unique(sum_norm_c$variable)
)

active_labels <- unname(
  var_display[active_vars]
)

cols_f <- setNames(
  unname(cascade_cols[active_vars]),
  active_labels
)

lwd_f <- setNames(
  unname(cascade_lwd[active_vars]),
  active_labels
)

lty_f <- setNames(
  unname(cascade_lty[active_vars]),
  active_labels
)

shape_f <- setNames(
  unname(cascade_shape[active_vars]),
  active_labels
)

# Retain the established annotation positions.
label_positions <- data.frame(
  treatment = c(
    rep("GP", 4),
    rep("PEG", 6)
  ),
  variable = c(
    "SO₄²⁻", "Sol.Ca", "CaCO₃", "CF",
    "CF", "CaCO₃", "Sol.Ca",
    "HCO₃⁻", "CO₃²⁻", "SO₄²⁻"
  ),
  x_text = c(
    rep(1.68, 4),
    rep(3.18, 6)
  ),
  y_text = c(
    0.97, 0.80, 0.56, 0.30,
    1.10, 0.96, 0.65,
    0.36, 0.20, 0.05
  )
)

fold_lab_f <- sum_norm_c %>%
  mutate(
    treatment = as.character(Treatments),
    variable_label = as.character(VarLabel)
  ) %>%
  inner_join(
    label_positions,
    by = c(
      "treatment" = "treatment",
      "variable_label" = "variable"
    )
  ) %>%
  filter(FoldLabel != "")

has_exca <- "Ex_Ca" %in% active_vars

arrow_label <- if (has_exca) {
  "CF and Ex.Ca ↑↑"
} else {
  "CF enrichment ↑"
}

pF <- ggplot(
  sum_norm_c,
  aes(
    x = x_pos,
    y = Norm,
    group = VarLabel,
    colour = VarLabel,
    fill = VarLabel,
    shape = VarLabel,
    linetype = VarLabel
  )
) +
  
  annotate(
    "rect",
    xmin = 0.45, xmax = 3.55,
    ymin = 0.58, ymax = 1.205,
    fill = "#FEF3E2",
    alpha = 0.65
  ) +
  
  annotate(
    "rect",
    xmin = 0.45, xmax = 3.55,
    ymin = -0.08, ymax = 0.42,
    fill = "#E8F5F0",
    alpha = 0.65
  ) +
  
  annotate(
    "rect",
    xmin = 0.45, xmax = 3.55,
    ymin = 0.42, ymax = 0.58,
    fill = "#EDE7F6",
    alpha = 0.55
  ) +
  
  annotate(
    "text",
    x = 0.55,
    y = c(1, 0.5, 0),
    label = c("Max", "Mid", "Min"),
    angle = 90,
    fontface = "bold",
    size = 3.2
  ) +
  
  annotate(
    "text",
    x = 0.72,
    y = 1.165,
    label = "Functional Ca pools ↑",
    colour = "#A0522D",
    fontface = "bold",
    size = 3.0,
    hjust = 0
  ) +
  
  annotate(
    "text",
    x = 0.66,
    y = 0.185,
    label = "Anion pools ↓",
    colour = "#2E8B57",
    fontface = "bold",
    size = 3.0,
    hjust = 0
  ) +
  
  geom_ribbon(
    aes(
      ymin = NormLo,
      ymax = NormHi
    ),
    alpha = 0.10,
    colour = NA,
    show.legend = FALSE
  ) +
  
  geom_line(
    aes(linewidth = VarLabel),
    alpha = 0.92
  ) +
  
  geom_point(
    size = 4.35,
    stroke = 0.60,
    colour = "white"
  ) +
  
  geom_point(
    size = 3.85,
    stroke = 0.18
  ) +
  
  geom_label(
    data = fold_lab_f,
    aes(
      x = x_text,
      y = y_text,
      label = FoldLabel,
      colour = VarLabel
    ),
    inherit.aes = FALSE,
    fill = "white",
    label.size = 0,
    label.padding = unit(0.05, "lines"),
    size = 2.6,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  annotate(
    "segment",
    x = 1.43,
    xend = 2.37,
    y = 1.095,
    yend = 1.095,
    colour = "#2B5BA8",
    linewidth = 0.45,
    arrow = grid::arrow(
      type = "closed",
      length = unit(0.06, "inches")
    )
  ) +
  
  annotate(
    "text",
    x = 1.90,
    y = 1.165,
    label = arrow_label,
    colour = "#2B5BA8",
    fontface = "bold",
    size = 2.9
  ) +
  
  scale_colour_manual(
    values = cols_f,
    breaks = active_labels,
    name = NULL
  ) +
  
  scale_fill_manual(
    values = cols_f,
    breaks = active_labels,
    name = NULL
  ) +
  
  scale_shape_manual(
    values = shape_f,
    breaks = active_labels,
    name = NULL
  ) +
  
  scale_linetype_manual(
    values = lty_f,
    breaks = active_labels,
    name = NULL
  ) +
  
  scale_linewidth_manual(
    values = lwd_f,
    guide = "none"
  ) +
  
  guides(
    colour = guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        linewidth = 0.5,
        size = 2.3
      )
    ),
    fill = "none",
    shape = "none",
    linetype = "none"
  ) +
  
  scale_x_continuous(
    breaks = 1:3,
    labels = allowed_treatments,
    limits = c(0.45, 3.55),
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    breaks = c(0, 0.5, 1),
    labels = NULL,
    expand = c(0, 0)
  ) +
  
  coord_cartesian(
    ylim = c(-0.105, 1.205),
    clip = "off"
  ) +
  
  theme_classic(
    base_size = 11,
    base_family = fig_font
  ) +
  
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(
      size = 12, colour = "black"
    ),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.x = element_line(
      colour = "grey88",
      linewidth = 0.30
    ),
    panel.background = element_rect(
      fill = "grey99", colour = NA
    ),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(
      size = 8, face = "bold"
    ),
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.key.size = unit(0.20, "cm"),
    legend.spacing.x = unit(0.15, "mm"),
    plot.margin = margin(2, 7, 1, 2)
  )

# 10. PANEL (a) LEGEND — ABOVE PANEL (a) --------------------------

legend_A_data <- data.frame(
  Treatments = factor(
    allowed_treatments,
    levels = allowed_treatments
  ),
  x = c(0.16, 0.43, 0.70),
  text_x = c(0.22, 0.49, 0.76),
  y = 0.26
)

legend_A <- ggplot(legend_A_data) +
  geom_point(
    aes(
      x = x,
      y = y,
      colour = Treatments
    ),
    shape = 15,
    size = 2.8
  ) +
  geom_text(
    aes(
      x = text_x,
      y = y,
      label = Treatments
    ),
    hjust = 0,
    size = 3.05
  ) +
  scale_colour_manual(
    values = trt_cols,
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  theme_void()

# 11. PANEL (b) LEGEND — TWO ROWS ABOVE PANEL (b) ---------------

# Row 1: Macro and Micro
# Row 2: Silt+clay

legend_B_data <- data.frame(
  fraction = factor(
    names(frac_cols),
    levels = names(frac_cols)
  ),
  x = c(0.01, 0.52, 0.26),
  text_x = c(0.08, 0.59, 0.33),
  y = c(0.78, 0.78, 0.22)
)

legend_B <- ggplot(legend_B_data) +
  geom_point(
    aes(
      x = x,
      y = y,
      colour = fraction
    ),
    shape = 15,
    size = 2.3
  ) +
  geom_text(
    aes(
      x = text_x,
      y = y,
      label = fraction
    ),
    hjust = 0,
    size = 2.3
  ) +
  scale_colour_manual(
    values = frac_cols,
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off") +
  theme_void()

# 12. PANEL LABELS ------------------------------------------------

# Use dedicated tag strips for all major rows so that panel labels
# (a), (c), (d), and (f) align vertically in the portrait layout.

make_tag_strip <- function(tag_text) {
  ggplot() +
    annotate(
      "text",
      x = 0.014,
      y = 0.45,
      label = tag_text,
      hjust = 0,
      fontface = "bold",
      size = 4.6
    ) +
    scale_x_continuous(
      limits = c(0, 1),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      expand = c(0, 0)
    ) +
    theme_void()
}

tag_ab <- (
  make_tag_strip("(a)") +
    make_tag_strip("(b)")
) +
  plot_layout(widths = c(1.65, 1.00))

tag_c <- make_tag_strip("(c)")

tag_de <- (
  make_tag_strip("(d)") +
    make_tag_strip("(e)")
) +
  plot_layout(widths = c(1, 1))

tag_f <- make_tag_strip("(f)")

# 13. FINAL PORTRAIT LAYOUT ---------------------------------------

row_ab <- (pA + pB) +
  plot_layout(
    widths = c(1.65, 1.00)
  )

row_legends <- (legend_A + legend_B) +
  plot_layout(
    widths = c(1.65, 1.00)
  )

row_de <- (pD + pE) +
  plot_layout(
    widths = c(1, 1)
  )

# Each legend remains aligned with its own panel, but appears above it.
fig_final <- (
  tag_ab /
    row_legends /
    row_ab /
    tag_c /
    pC /
    tag_de /
    row_de /
    tag_f /
    pF
) +
  plot_layout(
    heights = c(
      0.23,
      0.56,
      2.27,
      0.23,
      1.36,
      0.23,
      2.26,
      0.34,
      2.09
    )
  ) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(
        fill = "white", colour = NA
      ),
      plot.margin = margin(3, 3, 3, 3)
    )
  )

# 14. SAVE FIGURE -------------------------------------------------

output_base <- file.path(
  out_dir,
  paste0(
    "Fig4_NC_Final_Corrected_", stamp
  )
)

pdf_device <- if (capabilities("cairo")) {
  grDevices::cairo_pdf
} else {
  "pdf"
}

ggsave(
  paste0(output_base, ".pdf"),
  plot = fig_final,
  width = fig_width_in,
  height = fig_height_in,
  units = "in",
  device = pdf_device,
  bg = "white",
  limitsize = FALSE
)

ggsave(
  paste0(output_base, ".png"),
  plot = fig_final,
  width = fig_width_in,
  height = fig_height_in,
  units = "in",
  dpi = 600,
  bg = "white",
  limitsize = FALSE
)

ggsave(
  paste0(output_base, ".tiff"),
  plot = fig_final,
  width = fig_width_in,
  height = fig_height_in,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white",
  limitsize = FALSE
)

# 15. MINIMAL NATURE COMMUNICATIONS SOURCE DATA --------------------
# Figure calculations and figure exports above are unchanged.
# Export only the numerical data needed to support Figure 4.
# One worksheet for this figure; no repeated raw/summary/audit tables.

sd4_file <- file.path(out_dir, "Source_Data_Fig4.xlsx")
sd4_wb <- openxlsx::createWorkbook(creator = "Figure 4 source data")
sd4_sheet <- "Fig 4 Source Data"
openxlsx::addWorksheet(sd4_wb, sd4_sheet, gridLines = FALSE)
sd4_row <- 1L

sd4_style_title <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 12, fontColour = "#FFFFFF",
  fgFill = "#183153", textDecoration = "bold", valign = "center"
)
sd4_style_header <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 10, fontColour = "#FFFFFF",
  fgFill = "#355C7D", textDecoration = "bold", valign = "center"
)
sd4_style_body <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 10, valign = "center"
)
sd4_style_number <- openxlsx::createStyle(numFmt = "0.##########")
sd4_style_note <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 9, fontColour = "#333333",
  wrapText = TRUE, valign = "center"
)

sd4_write_note <- function(text) {
  openxlsx::mergeCells(
    sd4_wb, sd4_sheet, cols = 1:10, rows = sd4_row
  )
  openxlsx::writeData(
    sd4_wb, sd4_sheet, text,
    startRow = sd4_row, startCol = 1, colNames = FALSE
  )
  openxlsx::addStyle(
    sd4_wb, sd4_sheet, style = sd4_style_note,
    rows = sd4_row, cols = 1:10, gridExpand = TRUE, stack = TRUE
  )
  sd4_row <<- sd4_row + 1L
  invisible(NULL)
}

sd4_write_block <- function(title, data) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    warning("No data exported for ", title)
    return(invisible(NULL))
  }
  n_cols <- ncol(data)
  openxlsx::mergeCells(
    sd4_wb, sd4_sheet, cols = seq_len(n_cols), rows = sd4_row
  )
  openxlsx::writeData(
    sd4_wb, sd4_sheet, title,
    startRow = sd4_row, startCol = 1, colNames = FALSE
  )
  openxlsx::addStyle(
    sd4_wb, sd4_sheet, style = sd4_style_title,
    rows = sd4_row, cols = seq_len(n_cols),
    gridExpand = TRUE, stack = TRUE
  )
  openxlsx::setRowHeights(sd4_wb, sd4_sheet, rows = sd4_row, heights = 23)
  sd4_row <<- sd4_row + 1L
  
  openxlsx::writeData(
    sd4_wb, sd4_sheet, x = data,
    startRow = sd4_row, startCol = 1,
    colNames = TRUE, rowNames = FALSE,
    keepNA = FALSE, headerStyle = sd4_style_header
  )
  openxlsx::addStyle(
    sd4_wb, sd4_sheet, style = sd4_style_body,
    rows = seq.int(sd4_row + 1L, sd4_row + nrow(data)),
    cols = seq_len(n_cols), gridExpand = TRUE, stack = TRUE
  )
  numeric_cols <- which(vapply(data, is.numeric, logical(1)))
  if (length(numeric_cols)) {
    openxlsx::addStyle(
      sd4_wb, sd4_sheet, style = sd4_style_number,
      rows = seq.int(sd4_row + 1L, sd4_row + nrow(data)),
      cols = numeric_cols, gridExpand = TRUE, stack = TRUE
    )
  }
  openxlsx::setRowHeights(
    sd4_wb, sd4_sheet,
    rows = seq.int(sd4_row + 1L, sd4_row + nrow(data)), heights = 20
  )
  sd4_row <<- sd4_row + nrow(data) + 2L
  invisible(NULL)
}

# File note: replicate-level values are supplied where the panel is based on
# replicate data; only exact plotted coordinates are supplied for panel (f).
openxlsx::mergeCells(sd4_wb, sd4_sheet, cols = 1:10, rows = sd4_row)
openxlsx::writeData(
  sd4_wb, sd4_sheet, "Source Data for Fig. 4",
  startRow = sd4_row, startCol = 1, colNames = FALSE
)
openxlsx::addStyle(
  sd4_wb, sd4_sheet, style = sd4_style_title,
  rows = sd4_row, cols = 1:10, gridExpand = TRUE, stack = TRUE
)
openxlsx::setRowHeights(sd4_wb, sd4_sheet, rows = sd4_row, heights = 27)
sd4_row <- sd4_row + 1L
sd4_write_note(
  "Numerical data underlying panels (a–f). Replicate-level values are provided where applicable; normalized scores and plotted coordinates are included only where needed to reproduce the displayed transformations. Units follow the figure axes and should be checked against laboratory records."
)
sd4_row <- sd4_row + 1L

# Figure 4a — six measured fractions; replicate values support means, SE and tests.
sd4_a <- df %>%
  dplyr::transmute(
    Treatment = as.character(Treatments),
    Replicate = as.character(Replicates),
    POC_C_percent = POM_C,
    MAOC_C_percent = MAOM_C,
    POP_P_mg_kg = POM_P,
    MAOP_P_mg_kg = MAOM_P,
    PCF_Ca_cmolc_kg = POM_Ca_F3,
    MCF_Ca_cmolc_kg = MAOM_Ca_F3
  ) %>%
  dplyr::arrange(
    match(Treatment, allowed_treatments),
    suppressWarnings(as.numeric(Replicate)), Replicate
  )
sd4_write_block("Figure 4a — Replicate data", sd4_a)

# Figure 4b — exact replicate-level proportions used in the stacked bars,
# plus MWD calculated from those same proportions.
sd4_b <- agg_wide %>%
  dplyr::transmute(
    Treatment = as.character(Treatments),
    Replicate = as.character(Replicates),
    Macro_percent = Macro,
    Micro_percent = Micro,
    Silt_clay_percent = .data[["Silt+clay"]],
    MWD_mm = 1.125 * (Macro / 100) +
      0.1515 * (Micro / 100) +
      0.0265 * (.data[["Silt+clay"]] / 100)
  ) %>%
  dplyr::arrange(
    match(Treatment, allowed_treatments),
    suppressWarnings(as.numeric(Replicate)), Replicate
  )
sd4_write_block("Figure 4b — Plotted aggregate proportions and MWD", sd4_b)
if (exists("silt_reference_used") && nrow(silt_reference_used) > 0L) {
  sd4_write_note(
    "CHECK BEFORE SUBMISSION: one or more silt+clay values used for panel (b) were supplied by the embedded script reference because they were absent from the input workbook. Verify these values against the original laboratory records."
  )
  sd4_row <- sd4_row + 1L
}

# Figure 4c — normalized CAI and MDR scores used for the bars.
sd4_c <- idx_long %>%
  dplyr::select(Treatments, Replicates, Index, Score) %>%
  tidyr::pivot_wider(names_from = Index, values_from = Score) %>%
  dplyr::transmute(
    Treatment = as.character(Treatments),
    Replicate = as.character(Replicates),
    CAI_normalized_score = CAI,
    MDR_normalized_score = MDR
  ) %>%
  dplyr::arrange(
    match(Treatment, allowed_treatments),
    suppressWarnings(as.numeric(Replicate)), Replicate
  )
sd4_write_block("Figure 4c — Normalized index scores", sd4_c)

# Figures 4d and 4e — replicate-level normalized radar scores. The repeated
# closing vertex used only to draw each polygon is deliberately not exported.
sd4_radar_source <- function(candidates) {
  # Keep replicate IDs: make_radar_scores() is intended for plotting and
  # returns treatment means, so it cannot supply replicate-level source data.
  vars <- intersect(candidates, names(df_radar))
  vars <- vars[vapply(vars, function(v) {
    any(is.finite(suppressWarnings(as.numeric(df_radar[[v]]))))
  }, logical(1))]
  
  if (!length(vars)) {
    return(data.frame(
      Treatment = character(), Replicate = character(),
      Indicator = character(), Normalized_score = numeric()
    ))
  }
  
  tmp <- df_radar %>%
    dplyr::select(Treatments, Replicates, dplyr::all_of(vars))
  
  score_cols <- character(length(vars))
  for (i in seq_along(vars)) {
    v <- vars[[i]]
    raw <- suppressWarnings(as.numeric(tmp[[v]]))
    score_name <- paste0("S_", v)
    tmp[[score_name]] <- clamp01(
      if (v %in% low_rec) score_lib(raw) else score_mib(raw)
    )
    score_cols[[i]] <- score_name
  }
  
  tmp %>%
    dplyr::select(Treatments, Replicates, dplyr::all_of(score_cols)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(score_cols),
      names_to = "Indicator",
      values_to = "Normalized_score"
    ) %>%
    dplyr::mutate(
      Indicator = sub("^S_", "", Indicator),
      Indicator = label_var(Indicator)
    ) %>%
    dplyr::arrange(
      match(as.character(Treatments), allowed_treatments), Indicator,
      suppressWarnings(as.numeric(as.character(Replicates))),
      as.character(Replicates)
    ) %>%
    dplyr::transmute(
      Treatment = as.character(Treatments),
      Replicate = as.character(Replicates),
      Indicator,
      Normalized_score
    )
}

sd4_d <- sd4_radar_source(c(
  "pH", "EC", "SAR", "Na", "Ex.Na", "Cl", "SO4",
  "HCO3", "CO3", "CaCO3", "RC", "Ex.Ca", "CAI"
))
sd4_write_block("Figure 4d — Reclamation radar scores", sd4_d)

sd4_e <- sd4_radar_source(c(
  "SOC", "POM_C", "MAOM_C", "POM_P", "MAOM_P",
  "POM_Ca_F3", "MAOM_Ca_F3", "Macro", "Micro",
  "Silt+clay", "MWD", "SAP", "STP", "MDR"
))
sd4_write_block("Figure 4e — Carbon and structure radar scores", sd4_e)

# Figure 4f — exact normalized means and ribbon bounds drawn in the figure.
# Raw means/SD/SE and a second raw-data table are omitted to avoid repetition.
sd4_f <- sum_norm_c %>%
  dplyr::transmute(
    Treatment = as.character(Treatments),
    Indicator = as.character(VarLabel),
    Normalized_mean = Norm,
    Ribbon_lower = NormLo,
    Ribbon_upper = NormHi,
    Fold_label = FoldLabel
  ) %>%
  dplyr::arrange(
    match(Treatment, allowed_treatments), Indicator
  )
sd4_write_block("Figure 4f — Plotted normalized means and ribbon bounds", sd4_f)

# Compact workbook formatting and verification.
openxlsx::freezePane(sd4_wb, sd4_sheet, firstActiveRow = 2)
openxlsx::setColWidths(sd4_wb, sd4_sheet, cols = 1, widths = 14)
openxlsx::setColWidths(sd4_wb, sd4_sheet, cols = 2, widths = 11)
openxlsx::setColWidths(sd4_wb, sd4_sheet, cols = 3:10, widths = 18)
openxlsx::saveWorkbook(sd4_wb, file = sd4_file, overwrite = TRUE)

if (!file.exists(sd4_file) || file.info(sd4_file)$size < 1000) {
  stop("Figure 4 Source Data workbook was not saved successfully: ", sd4_file)
}
if (!identical(openxlsx::getSheetNames(sd4_file), sd4_sheet)) {
  stop("Expected exactly one worksheet named 'Fig 4 Source Data'.")
}

cat("\nFIGURE 4 MINIMAL SOURCE DATA SAVED\n")
cat("Workbook: ", normalizePath(sd4_file, winslash = "/", mustWork = TRUE), "\n", sep = "")
cat("Worksheet: ", sd4_sheet, "\n", sep = "")
cat("Only the Fig. 4 source-data workbook is exported; plot calculations and figure files are unchanged.\n")

if (interactive() && .Platform$OS.type == "windows") {
  try(shell.exec(normalizePath(out_dir, winslash = "\\", mustWork = TRUE)),
      silent = TRUE)
}
