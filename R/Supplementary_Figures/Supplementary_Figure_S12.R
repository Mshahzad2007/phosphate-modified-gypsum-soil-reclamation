# Nature Communications-style Supplementary Fig. S12
# Treatment order: CK -> GP -> P+GP -> PEG
# Layout: 15.5 x 18.4 cm, for an A4 portrait Word page.
# Retains the supplied recovery-score method, mean +/- SE heatmap labels,
# replicate SPRI, one-way ANOVA and Tukey HSD.
# Run in RStudio with Conventional_Treatments.xlsx in your working directory.

## ============================================================
## SUPPLEMENTARY FIG. S12 — SOIL PHYSICAL RECOVERY FINGERPRINT
##
## Fig. S12a:
##   Soil physical recovery fingerprint heatmap
##
## Fig. S12b:
##   Integrated Soil Physical Recovery Index, SPRI
##
## Variables:
##   MWD  = higher is better
##   Ksat = higher is better
##   TP   = Total porosity, higher is better
##   BD   = lower is better, so direction is inverted
##
## Input:
##   Conventional_Treatments.xlsx
##   sheet = data
##
## Expected long format:
##   Treatments / Treatment
##   Replicates / Replicate
##   response.varibale / response.variable / Variable
##   value / Value
## ============================================================

rm(list = ls())
graphics.off()

## ----------------------------
## 0) Packages
## ----------------------------

pkgs <- c(
  "readxl", "dplyr", "tidyr", "stringr", "ggplot2",
  "multcompView", "scales", "openxlsx", "tibble",
  "patchwork", "grid"
)

to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

suppressPackageStartupMessages({
  invisible(lapply(pkgs, library, character.only = TRUE))
})

## ============================================================
## 1) Input / output
## ============================================================

file_in  <- "Conventional_Treatments.xlsx"
sheet_in <- "data"

out_dir <- "Fig_S12_NC_Portrait_Physical_Recovery"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

## ============================================================
## 2) Colours
## ============================================================

trt_cols_known <- c(
  "CK"   = "#C9C9C9",
  "GP"   = "#5F6A72",
  "P+GP" = "#999999",
  "PEG"  = "#62C2CC"
)

heat_low  <- "#D8D8D8"
heat_mid  <- "white"
heat_high <- "#62C2CC"

## ============================================================
## 3) Helpers
## ============================================================

norm_key <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\u00A0", " ")
  x <- stringr::str_squish(x)
  gsub("[^a-z0-9]+", "", tolower(x))
}

find_col <- function(nm, possible) {
  nm_key <- norm_key(nm)
  possible_key <- norm_key(possible)
  hit <- which(nm_key %in% possible_key)
  
  if (length(hit) == 0) return(NA_character_)
  nm[hit[1]]
}

find_sheet_trim <- function(file, target_sheet) {
  sh <- readxl::excel_sheets(file)
  hit <- sh[stringr::str_trim(sh) == target_sheet]
  
  if (length(hit) == 0) {
    stop(
      paste0(
        "Sheet not found: ", target_sheet,
        "\nAvailable sheets: ",
        paste(sh, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  hit[1]
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

canon_treatment <- function(x) {
  x <- stringr::str_replace_all(as.character(x), "\u00A0", " ")
  x <- stringr::str_squish(x)
  
  key <- norm_key(x)
  dplyr::case_when(
    key == "pgp" ~ "P+GP",  # Important: P+GP is distinct from PEG
    key == "peg" ~ "PEG",
    key == "gp"  ~ "GP",
    key == "ck"  ~ "CK",
    TRUE ~ x
  )
}

canon_var <- function(x) {
  x_raw <- stringr::str_replace_all(as.character(x), "\u00A0", " ")
  x_raw <- stringr::str_squish(x_raw)
  key <- norm_key(x_raw)
  
  dplyr::case_when(
    key %in% c(
      "mwd",
      "meanweightdiameter",
      "meanweighteddiameter"
    ) ~ "MWD",
    
    key %in% c(
      "ksat",
      "kst",
      "saturatedhydraulicconductivity",
      "hydraulicconductivity",
      "saturatedconductivity"
    ) ~ "Ksat",
    
    key %in% c(
      "tp",
      "tporosity",
      "totalporosity",
      "totalpore",
      "porosity",
      "totalporositypercent",
      "totalporositypct",
      "totalporositypercentage"
    ) ~ "Total_porosity",
    
    key %in% c(
      "bd",
      "bulkdensity",
      "bulkdensitygcm3",
      "bulkdensitygcm",
      "bulkdensitygcc"
    ) ~ "BD",
    
    TRUE ~ x_raw
  )
}

calc_cld <- function(dsub, response_col = "SPRI") {
  
  dsub <- dsub %>%
    dplyr::filter(!is.na(.data[[response_col]]), !is.na(Treatments)) %>%
    dplyr::mutate(Treatments = droplevels(factor(Treatments)))
  
  trt_levels <- levels(dsub$Treatments)
  
  fallback <- tibble::tibble(
    Treatments = trt_levels,
    CLD = "a"
  )
  
  if (length(trt_levels) < 2) return(fallback)
  if (dplyr::n_distinct(dsub[[response_col]]) <= 1) return(fallback)
  
  fit <- tryCatch(
    aov(stats::as.formula(paste(response_col, "~ Treatments")), data = dsub),
    error = function(e) NULL
  )
  
  if (is.null(fit)) return(fallback)
  
  tk <- tryCatch(
    TukeyHSD(fit),
    error = function(e) NULL
  )
  
  if (is.null(tk)) return(fallback)
  if (!"Treatments" %in% names(tk)) return(fallback)
  
  pvals <- tk$Treatments[, "p adj"]
  
  letters <- tryCatch(
    multcompView::multcompLetters(pvals)$Letters,
    error = function(e) NULL
  )
  
  if (is.null(letters)) return(fallback)
  
  tibble::tibble(
    Treatments = names(letters),
    CLD = as.character(letters)
  )
}

get_anova <- function(dsub, response_col = "SPRI") {
  
  dsub <- dsub %>%
    dplyr::filter(!is.na(.data[[response_col]]), !is.na(Treatments)) %>%
    dplyr::mutate(Treatments = droplevels(factor(Treatments)))
  
  if (dplyr::n_distinct(dsub$Treatments) < 2 ||
      dplyr::n_distinct(dsub[[response_col]]) <= 1) {
    return(tibble::tibble(
      term = "Treatments",
      df = NA_real_,
      sumsq = NA_real_,
      meansq = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_
    ))
  }
  
  fit <- aov(stats::as.formula(paste(response_col, "~ Treatments")), data = dsub)
  
  a <- as.data.frame(summary(fit)[[1]])
  a$term <- rownames(a)
  rownames(a) <- NULL
  
  a %>%
    dplyr::rename(
      df = Df,
      sumsq = `Sum Sq`,
      meansq = `Mean Sq`,
      statistic = `F value`,
      p.value = `Pr(>F)`
    ) %>%
    dplyr::select(term, df, sumsq, meansq, statistic, p.value)
}

get_tukey <- function(dsub, response_col = "SPRI") {
  
  dsub <- dsub %>%
    dplyr::filter(!is.na(.data[[response_col]]), !is.na(Treatments)) %>%
    dplyr::mutate(Treatments = droplevels(factor(Treatments)))
  
  if (dplyr::n_distinct(dsub$Treatments) < 2 ||
      dplyr::n_distinct(dsub[[response_col]]) <= 1) {
    return(tibble::tibble(
      contrast = NA_character_,
      diff = NA_real_,
      lwr = NA_real_,
      upr = NA_real_,
      p_adj = NA_real_
    ))
  }
  
  fit <- aov(stats::as.formula(paste(response_col, "~ Treatments")), data = dsub)
  tk <- TukeyHSD(fit)
  
  if (!"Treatments" %in% names(tk)) {
    return(tibble::tibble(
      contrast = NA_character_,
      diff = NA_real_,
      lwr = NA_real_,
      upr = NA_real_,
      p_adj = NA_real_
    ))
  }
  
  out <- as.data.frame(tk$Treatments)
  out$contrast <- rownames(out)
  rownames(out) <- NULL
  
  out %>%
    dplyr::rename(
      diff = diff,
      lwr = lwr,
      upr = upr,
      p_adj = `p adj`
    ) %>%
    dplyr::select(contrast, diff, lwr, upr, p_adj)
}

## ============================================================
## 4) Theme
## ============================================================

BS <- 10.5

theme_clean <- theme_classic(base_size = BS) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    
    plot.title = element_blank(),
    
    axis.title.x = element_text(
      size = 10,
      face = "plain",
      colour = "black",
      margin = margin(t = 5)
    ),
    axis.title.y = element_text(
      size = 10,
      face = "plain",
      colour = "black",
      margin = margin(r = 6)
    ),
    
    axis.text.x = element_text(
      size = 9.5,
      face = "plain",
      colour = "black"
    ),
    axis.text.y = element_text(
      size = 9.5,
      face = "plain",
      colour = "black"
    ),
    
    axis.line = element_line(linewidth = 0.55, colour = "black"),
    axis.ticks = element_line(linewidth = 0.50, colour = "black"),
    axis.ticks.length = unit(2.5, "pt"),
    
    panel.grid = element_blank(),
    panel.border = element_blank(),
    
    legend.title = element_text(size = 9.5, face = "plain", colour = "black"),
    legend.text  = element_text(size = 9, face = "plain", colour = "black"),
    legend.key.height = unit(4, "mm"),
    legend.key.width  = unit(8, "mm"),
    
    plot.margin = margin(8, 10, 8, 10)
  )

## ============================================================
## 5) Read data
## ============================================================

if (!file.exists(file_in)) {
  stop(
    paste0(
      "File not found: ", file_in,
      "\nPut the Excel file in your working directory or edit file_in."
    ),
    call. = FALSE
  )
}

sheet_real <- find_sheet_trim(file_in, sheet_in)

raw <- readxl::read_excel(
  file_in,
  sheet = sheet_real,
  .name_repair = "minimal"
)

names(raw) <- names(raw) %>%
  stringr::str_replace_all("\u00A0", " ") %>%
  stringr::str_squish()

col_treat <- find_col(names(raw), c("Treatments", "Treatment", "Trt"))
col_rep   <- find_col(names(raw), c("Replicates", "Replicate", "Rep"))
col_var   <- find_col(names(raw), c(
  "response.varibale",
  "response.variable",
  "Variable",
  "Response",
  "Parameter",
  "Variables"
))
col_val   <- find_col(names(raw), c("value", "Value", "Values"))

if (any(is.na(c(col_treat, col_rep, col_var, col_val)))) {
  stop(
    paste0(
      "Could not detect required columns.\n\n",
      "Need columns equivalent to:\n",
      "Treatments | Replicates | response.varibale | value\n\n",
      "Available columns:\n",
      paste(names(raw), collapse = ", ")
    ),
    call. = FALSE
  )
}

dat_long <- raw %>%
  dplyr::transmute(
    Treatments   = canon_treatment(.data[[col_treat]]),
    Replicates   = factor(.data[[col_rep]]),
    Raw_variable = stringr::str_squish(
      stringr::str_replace_all(
        stringr::str_trim(as.character(.data[[col_var]])),
        "\u00A0",
        " "
      )
    ),
    Variable     = canon_var(.data[[col_var]]),
    Value        = safe_numeric(.data[[col_val]])
  ) %>%
  dplyr::filter(
    !is.na(Treatments),
    !is.na(Replicates),
    !is.na(Raw_variable),
    !is.na(Variable),
    !is.na(Value)
  ) %>%
  dplyr::mutate(
    Variable = dplyr::case_when(
      toupper(Raw_variable) == "TP" ~ "Total_porosity",
      norm_key(Raw_variable) == "tp" ~ "Total_porosity",
      stringr::str_detect(norm_key(Raw_variable), "totalporosity") ~ "Total_porosity",
      TRUE ~ Variable
    )
  )

## ------------------------------------------------------------
## Optional additional P+GP sheet (same long-format structure).
## Used only for P+GP rows not already present in sheet "data".
## If data already contains P+GP, the original measurements are kept.
## ------------------------------------------------------------
p_gp_sheet <- readxl::excel_sheets(file_in)
p_gp_sheet <- p_gp_sheet[tolower(trimws(p_gp_sheet)) == "p+gp"]

if (length(p_gp_sheet) > 0) {
  # col_names=FALSE handles both labelled and headerless four-column tables;
  # a potential header row fails the numeric Value filter naturally.
  pg_raw <- readxl::read_excel(
    file_in, sheet = p_gp_sheet[1], col_names = FALSE,
    col_types = "text", .name_repair = "minimal"
  )
  if (ncol(pg_raw) < 4) {
    stop("The 'P+GP' sheet has fewer than four columns.", call. = FALSE)
  }
  names(pg_raw)[1:4] <- c("src_treatment", "src_replicate", "src_variable", "src_value")
  pg_clean <- pg_raw %>%
    dplyr::transmute(
      Treatments   = canon_treatment(src_treatment),
      Replicates   = factor(stringr::str_squish(src_replicate)),
      Raw_variable = stringr::str_squish(src_variable),
      Variable     = canon_var(src_variable),
      Value        = safe_numeric(src_value)
    ) %>%
    dplyr::filter(Treatments == "P+GP", !is.na(Replicates),
                  !is.na(Variable), is.finite(Value))
  
  if (nrow(pg_clean) > 0) {
    dat_long <- dplyr::bind_rows(
      dat_long %>% dplyr::mutate(Replicates = as.character(Replicates)),
      pg_clean %>% dplyr::mutate(Replicates = as.character(Replicates))
    )
    message("P+GP measurements detected in additional sheet: ", p_gp_sheet[1])
  }
}

# If both sheets contain a measurement, never count it twice.
# Conflicting duplicate values stop the analysis instead of being averaged.
dat_long <- dat_long %>%
  dplyr::mutate(Replicates = factor(as.character(Replicates))) %>%
  dplyr::filter(Treatments %in% c("CK", "GP", "P+GP", "PEG"))

# Only the four physical variables enter S12. Ignore duplicate records for
# unrelated outcomes elsewhere in the workbook.
physical_keys <- c("MWD", "Ksat", "Total_porosity", "BD")
physical_data <- dat_long %>% dplyr::filter(Variable %in% physical_keys)

conflicts <- physical_data %>%
  dplyr::group_by(Treatments, Replicates, Variable) %>%
  dplyr::summarise(n_distinct_values = dplyr::n_distinct(Value), .groups = "drop") %>%
  dplyr::filter(n_distinct_values > 1)
if (nrow(conflicts) > 0) {
  print(conflicts)
  stop("Different values for the same treatment, replicate and variable: check the two Excel sheets.",
       call. = FALSE)
}

# Deduplicate identical physical records only. Keep unrelated workbook
# variables intact for the raw-data audit export.
physical_data <- physical_data %>%
  dplyr::distinct(Treatments, Replicates, Variable, .keep_all = TRUE)
dat_long <- dplyr::bind_rows(
  dat_long %>% dplyr::filter(!Variable %in% physical_keys),
  physical_data
)

message("Raw variables detected from Excel:")
print(sort(unique(dat_long$Raw_variable)))

message("Cleaned variables detected:")
print(sort(unique(dat_long$Variable)))

## ============================================================
## 6) Keep physical variables
## ============================================================

s12_vars <- c(
  "MWD",
  "Ksat",
  "Total_porosity",
  "BD"
)

dat_s12 <- dat_long %>%
  dplyr::filter(Variable %in% s12_vars)

missing_vars <- setdiff(s12_vars, unique(dat_s12$Variable))

if (length(missing_vars) > 0) {
  stop(
    paste0(
      "These required S12 variables are missing after cleaning: ",
      paste(missing_vars, collapse = ", "),
      "\n\nRaw variables available in Excel are:\n",
      paste(sort(unique(dat_long$Raw_variable)), collapse = ", "),
      "\n\nCleaned variables available are:\n",
      paste(sort(unique(dat_long$Variable)), collapse = ", ")
    ),
    call. = FALSE
  )
}

preferred_treat_order <- c("CK", "GP", "P+GP", "PEG")
present_treats <- unique(dat_s12$Treatments)
missing_treats <- setdiff(preferred_treat_order, present_treats)
if (length(missing_treats) > 0) {
  stop(paste0("Missing treatment(s) for Fig. S12: ",
              paste(missing_treats, collapse = ", "),
              ". Check the 'data' or 'P+GP' sheet."), call. = FALSE)
}
treat_levels <- preferred_treat_order

coverage <- dat_s12 %>%
  dplyr::group_by(Treatments, Variable) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop")
if (nrow(coverage) != 16) {
  print(coverage)
  stop("Missing treatment x variable combinations (expected four treatments x four variables).", call. = FALSE)
}


dat_s12 <- dat_s12 %>%
  dplyr::mutate(
    Treatments = factor(Treatments, levels = treat_levels),
    Variable = factor(Variable, levels = s12_vars)
  )

trt_cols <- trt_cols_known[treat_levels]

if (any(is.na(trt_cols))) {
  missing_col_treats <- names(trt_cols)[is.na(trt_cols)]
  extra_cols <- scales::hue_pal()(length(missing_col_treats))
  names(extra_cols) <- missing_col_treats
  trt_cols[missing_col_treats] <- extra_cols
}

## ============================================================
## 7) Direction-corrected recovery scores
## ============================================================

var_labels <- c(
  "MWD" = "MWD",
  "Ksat" = "Ksat",
  "Total_porosity" = "Total porosity",
  "BD" = "Low BD"
)

direction_table <- tibble::tibble(
  Variable = factor(s12_vars, levels = s12_vars),
  Direction = c("higher", "higher", "higher", "lower")
)

score_long <- dat_s12 %>%
  dplyr::left_join(direction_table, by = "Variable") %>%
  dplyr::group_by(Variable) %>%
  dplyr::mutate(
    Var_mean = mean(Value, na.rm = TRUE),
    Var_sd   = sd(Value, na.rm = TRUE),
    Z_raw = ifelse(
      is.na(Var_sd) | Var_sd == 0,
      0,
      (Value - Var_mean) / Var_sd
    ),
    Recovery_score = ifelse(Direction == "lower", -Z_raw, Z_raw)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Variable_label = factor(
      var_labels[as.character(Variable)],
      levels = var_labels[s12_vars]
    )
  )

## ============================================================
## 8) Panel A table — heatmap
## ============================================================

heat_df <- score_long %>%
  dplyr::group_by(Treatments, Variable, Variable_label) %>%
  dplyr::summarise(
    n = sum(!is.na(Value)),
    raw_mean = mean(Value, na.rm = TRUE),
    raw_sd = sd(Value, na.rm = TRUE),
    raw_se = raw_sd / sqrt(n),
    recovery_mean = mean(Recovery_score, na.rm = TRUE),
    recovery_sd = sd(Recovery_score, na.rm = TRUE),
    recovery_se = recovery_sd / sqrt(n),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    raw_se = ifelse(is.na(raw_se), 0, raw_se),
    recovery_se = ifelse(is.na(recovery_se), 0, recovery_se),
    tile_label = paste0(
      sprintf("%.2f", raw_mean),
      "\n±",
      sprintf("%.2f", raw_se)
    ),
    Treatments = factor(Treatments, levels = rev(treat_levels))
  )

score_limit <- max(abs(heat_df$recovery_mean), na.rm = TRUE)
if (!is.finite(score_limit) || score_limit == 0) score_limit <- 1
score_limit <- ceiling(score_limit * 10) / 10

## ============================================================
## 9) Panel B table — integrated index
## ============================================================

spri_rep <- score_long %>%
  dplyr::select(Treatments, Replicates, Variable, Recovery_score) %>%
  tidyr::pivot_wider(
    names_from = Variable,
    values_from = Recovery_score
  ) %>%
  dplyr::mutate(
    SPRI = rowMeans(
      dplyr::across(dplyr::all_of(s12_vars)),
      na.rm = TRUE
    )
  ) %>%
  dplyr::filter(is.finite(SPRI)) %>%
  dplyr::mutate(
    Treatments = factor(Treatments, levels = treat_levels)
  )

spri_sum <- spri_rep %>%
  dplyr::group_by(Treatments) %>%
  dplyr::summarise(
    n = sum(!is.na(SPRI)),
    mean = mean(SPRI, na.rm = TRUE),
    sd = sd(SPRI, na.rm = TRUE),
    se = sd / sqrt(n),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    sd = ifelse(is.na(sd), 0, sd),
    se = ifelse(is.na(se), 0, se)
  )

spri_cld <- calc_cld(spri_rep, response_col = "SPRI") %>%
  dplyr::mutate(Treatments = factor(Treatments, levels = treat_levels))

spri_plot <- spri_sum %>%
  dplyr::left_join(spri_cld, by = "Treatments") %>%
  dplyr::mutate(
    y_rng = max(mean + se, na.rm = TRUE) - min(mean - se, na.rm = TRUE),
    y_rng = ifelse(!is.finite(y_rng) | y_rng == 0, 1, y_rng),
    lab_y = mean + se + 0.08 * y_rng
  )

## ============================================================
## 10) Panel A — fingerprint heatmap
## ============================================================

pA_heatmap <- ggplot(
  heat_df,
  aes(x = Variable_label, y = Treatments, fill = recovery_mean)
) +
  geom_tile(
    colour = "white",
    linewidth = 0.65,
    width = 0.96,
    height = 0.88
  ) +
  geom_text(
    aes(label = tile_label),
    size = 3.0,
    colour = "black",
    lineheight = 0.95
  ) +
  scale_fill_gradient2(
    low = heat_low,
    mid = heat_mid,
    high = heat_high,
    midpoint = 0,
    limits = c(-score_limit, score_limit),
    name = "Recovery score"
  ) +
  labs(
    x = NULL,
    y = NULL,
    tag = "(a)"
  ) +
  theme_minimal(base_size = BS) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    
    panel.grid = element_blank(),
    
    axis.text.x = element_text(
      size = 9.5,
      face = "plain",
      colour = "black",
      angle = 0,
      vjust = 0.5
    ),
    axis.text.y = element_text(
      size = 10,
      face = "plain",
      colour = "black"
    ),
    
    legend.position = "right",
    legend.title = element_text(size = 9, face = "plain", colour = "black"),
    legend.text = element_text(size = 9, face = "plain", colour = "black"),
    legend.key.height = unit(14, "mm"),
    legend.key.width = unit(4, "mm"),
    
    plot.tag = element_text(
      size = 11.5,
      face = "bold",
      colour = "black"
    ),
    plot.tag.position = c(0.01, 0.99),
    
    plot.margin = margin(8, 12, 8, 8)
  )

## ============================================================
## 11) Panel B — integrated recovery index
## ============================================================

pB_index <- ggplot(
  spri_plot,
  aes(x = Treatments, y = mean)
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.45,
    linetype = "dashed",
    colour = "grey45"
  ) +
  geom_segment(
    aes(x = Treatments, xend = Treatments, y = 0, yend = mean),
    linewidth = 0.85,
    colour = "grey45"
  ) +
  geom_point(
    aes(fill = Treatments),
    shape = 21,
    size = 4.2,
    colour = "black",
    stroke = 0.35
  ) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.08,
    linewidth = 0.45,
    colour = "black"
  ) +
  geom_jitter(
    data = spri_rep,
    aes(x = Treatments, y = SPRI, fill = Treatments),
    shape = 21,
    size = 2.3,
    width = 0.08,
    alpha = 0.70,
    colour = "black",
    stroke = 0.25,
    inherit.aes = FALSE
  ) +
  geom_text(
    aes(y = lab_y, label = CLD),
    size = 3.6,
    fontface = "plain",
    colour = "black",
    vjust = 0
  ) +
  scale_fill_manual(values = trt_cols, drop = FALSE) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 5),
    expand = expansion(mult = c(0.08, 0.10))
  ) +
  labs(
    x = NULL,
    y = "Integrated soil physical recovery index",
    tag = "(b)"
  ) +
  theme_clean +
  theme(
    legend.position = "none",
    plot.tag = element_text(
      size = 11.5,
      face = "bold",
      colour = "black"
    ),
    plot.tag.position = c(0.01, 0.99)
  )

## ============================================================
## 12) Combine Fig. S12
## ============================================================

# Full-width stacked layout is legible at 15.5 cm in an A4 portrait Word page.
# Heatmap remains above the integrated-index plot.
pS12 <- (pA_heatmap / pB_index) +
  patchwork::plot_layout(heights = c(1.05, 1.00)) &
  theme(plot.background = element_rect(fill = "white", colour = NA))

print(pS12)

## ============================================================
## 13) Statistics tables
## ============================================================

spri_anova <- get_anova(spri_rep, response_col = "SPRI")
spri_tukey <- get_tukey(spri_rep, response_col = "SPRI")

variable_anova <- score_long %>%
  dplyr::group_by(Variable) %>%
  dplyr::group_modify(~ {
    tmp <- .x %>%
      dplyr::rename(SPRI = Recovery_score)
    get_anova(tmp, response_col = "SPRI")
  }) %>%
  dplyr::ungroup()

variable_tukey <- score_long %>%
  dplyr::group_by(Variable) %>%
  dplyr::group_modify(~ {
    tmp <- .x %>%
      dplyr::rename(SPRI = Recovery_score)
    get_tukey(tmp, response_col = "SPRI")
  }) %>%
  dplyr::ungroup()

## ============================================================
## 14) Save figure
## ============================================================

png_out  <- file.path(out_dir, paste0("Fig_S12_NC_Portrait_", stamp, ".png"))
tiff_out <- file.path(out_dir, paste0("Fig_S12_NC_Portrait_", stamp, ".tiff"))
pdf_out  <- file.path(out_dir, paste0("Fig_S12_NC_Portrait_", stamp, ".pdf"))

ggsave(
  filename = png_out,
  plot     = pS12,
  width    = 155,
  height   = 184,
  units    = "mm",
  dpi      = 600,
  bg       = "white"
)

ggsave(
  filename = tiff_out,
  plot     = pS12,
  width    = 155,
  height   = 184,
  units    = "mm",
  dpi      = 600,
  compression = "lzw",
  bg       = "white"
)

ggsave(
  filename = pdf_out,
  plot     = pS12,
  width    = 155,
  height   = 184,
  units    = "mm",
  device   = if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf,
  bg       = "white"
)

## ============================================================
## 15) Save data/statistics workbook
## ============================================================

stats_out <- file.path(out_dir, paste0("Fig_S12_NC_Statistics_", stamp, ".xlsx"))

openxlsx::write.xlsx(
  list(
    "Raw_clean_all" = dat_long,
    "Raw_S12_variables" = dat_s12,
    "Direction_corrected_scores" = score_long,
    "Heatmap_table" = heat_df,
    "SPRI_replicates" = spri_rep,
    "SPRI_summary" = spri_plot,
    "SPRI_ANOVA" = spri_anova,
    "SPRI_Tukey" = spri_tukey,
    "Variable_score_ANOVA" = variable_anova,
    "Variable_score_Tukey" = variable_tukey
  ),
  file = stats_out,
  overwrite = TRUE
)

message("Supplementary Fig. S12 saved:")
message("PNG:  ", png_out)
message("TIFF: ", tiff_out)
message("PDF:  ", pdf_out)
message("Tables: ", stats_out)

pS12