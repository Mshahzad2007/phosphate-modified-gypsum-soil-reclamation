# ============================================================ 
# Supplementary Fig. S9 — Ca pool redistribution: ECa vs Ca_F3  (Day 7 vs Day 30) 
# Original figure design preserved; only portrait-page sizing and text scale adjusted.
# Bars start at x-axis; Day strips sit beneath the panels. 
# Saves all outputs (figures + statistics) into a folder. 
# ============================================================ 

rm(list = ls()) 
graphics.off() 
cat("\014") 

library(readxl) 
library(dplyr) 
library(ggplot2) 
library(grid) 
library(cowplot) 
library(tidyr) 
library(writexl) 
library(tibble) 

# ------------------------------------------------------------ 
# 0. Output folder 
# ------------------------------------------------------------ 
out_dir <- "Fig_S9_NC_Portrait_Original_Style" 
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE) 

# ------------------------------------------------------------ 
# 1. Read data 
# ------------------------------------------------------------ 
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

df <- read_excel( 
  repo_input_path("Conventional_Treatments.xlsx"), 
  sheet = "Ca_7_30" 
) 

# ------------------------------------------------------------ 
# 2. Mean ± SD (per Day / Treatment / Pool) 
# ------------------------------------------------------------ 
sum_df <- df %>% 
  group_by(Day, Treatment, Pool) %>% 
  summarise( 
    n    = dplyr::n(), 
    Mean = mean(`Ca (cmol(+)/kg)`, na.rm = TRUE), 
    SD   = stats::sd(`Ca (cmol(+)/kg)`, na.rm = TRUE), 
    SE   = SD / sqrt(n), 
    .groups = "drop" 
  ) 

# ------------------------------------------------------------ 
# 3. Separate pools 
# ------------------------------------------------------------ 
ECa  <- sum_df %>% filter(Pool == "ECa") 
CaF3 <- sum_df %>% filter(Pool == "Ca_F3") 

# ------------------------------------------------------------ 
# 4. Treatment order + fixed x positions 
# ------------------------------------------------------------ 
ECa$Treatment  <- factor(ECa$Treatment,  levels = c("CK", "GP", "PEG")) 
CaF3$Treatment <- factor(CaF3$Treatment, levels = c("CK", "GP", "PEG")) 

ECa  <- ECa  %>% arrange(Day, Treatment) %>% mutate(x = c(1, 2, 3, 5, 6, 7)) 
CaF3 <- CaF3 %>% arrange(Day, Treatment) %>% mutate(x = c(1, 2, 3, 5, 6, 7)) 

# ------------------------------------------------------------ 
# 5. Scale Ca_F3 to the secondary axis (primary 0–5, sec 0–6) 
# ------------------------------------------------------------ 
factor_axis <- 5 / 6 

CaF3 <- CaF3 %>% 
  mutate( 
    Mean2 = Mean * factor_axis, 
    SD2   = SD   * factor_axis 
  ) 

# ------------------------------------------------------------ 
# 6. Main plot (bars start exactly at y = 0) 
# ------------------------------------------------------------ 
p <- ggplot() + 
  
  # ---- ECa bars ---- 
geom_col( 
  data = ECa, 
  aes(x = x - 0.18, y = Mean, fill = "ECa"), 
  width = 0.32 
) + 
  
  geom_errorbar( 
    data = ECa, 
    aes(x = x - 0.18, ymin = Mean - SD, ymax = Mean + SD), 
    width = 0.08 
  ) + 
  
  geom_line( 
    data = ECa, 
    aes(x = x - 0.18, y = Mean, colour = "ECa", group = Day), 
    linetype = "dashed", 
    linewidth = 1 
  ) + 
  
  geom_point( 
    data = ECa, 
    aes(x = x - 0.18, y = Mean, colour = "ECa"), 
    size = 3 
  ) + 
  
  # ---- Ca_F3 bars ---- 
geom_col( 
  data = CaF3, 
  aes(x = x + 0.18, y = Mean2, fill = "Ca_F3"), 
  width = 0.32 
) + 
  
  geom_errorbar( 
    data = CaF3, 
    aes(x = x + 0.18, ymin = Mean2 - SD2, ymax = Mean2 + SD2), 
    width = 0.08 
  ) + 
  
  geom_line( 
    data = CaF3, 
    aes(x = x + 0.18, y = Mean2, colour = "Ca_F3", group = Day), 
    linetype = "dashed", 
    linewidth = 1 
  ) + 
  
  geom_point( 
    data = CaF3, 
    aes(x = x + 0.18, y = Mean2, colour = "Ca_F3"), 
    shape = 15, 
    size = 3 
  ) + 
  
  # ---- Axes ---- 
scale_x_continuous( 
  breaks = c(1, 2, 3, 5, 6, 7), 
  labels = c("CK", "GP", "PEG", "CK", "GP", "PEG"), 
  expand = expansion(mult = c(0.08, 0.08)) 
) + 
  
  scale_y_continuous( 
    name   = expression("Ex.Ca (cmol(+)" * phantom(0) * "kg"^-1 * ")"), 
    limits = c(0, 5), 
    breaks = seq(0, 5, 1), 
    expand = expansion(mult = c(0, 0)), 
    sec.axis = sec_axis( 
      ~ . / factor_axis, 
      name   = expression("CF (cmol(+)" * phantom(0) * "kg"^-1 * ")"), 
      breaks = seq(0, 6, 1) 
    ) 
  ) + 
  
  # ---- Colours and legend order (Ex.Ca first, CF second) ---- 
scale_fill_manual( 
  values = c("ECa" = "#4F81BD", "Ca_F3" = "#ED7D31"), 
  breaks = c("ECa", "Ca_F3"), 
  labels = c("ECa"   = "Ex.Ca", 
             "Ca_F3" = "CF") 
) + 
  
  scale_colour_manual( 
    values = c("ECa" = "#174A8B", "Ca_F3" = "#B65A12"), 
    guide  = "none" 
  ) + 
  
  # ---- Labels (no x-axis title) ---- 
labs( 
  x    = NULL, 
  fill = NULL 
) + 
  
  # ---- Theme ---- 
# Generic sans avoids missing Windows/CID font errors on PDF export.
theme_classic(base_size = 10.5, base_family = "sans") + 
  
  theme( 
    legend.position  = "top", 
    legend.direction = "horizontal", 
    legend.text      = element_text(size = 9.5, face = "plain"), 
    legend.key       = element_blank(), 
    
    axis.title.x       = element_blank(), 
    axis.title.y.left  = element_text(size = 10, face = "plain", 
                                      colour = "#174A8B"), 
    axis.title.y.right = element_text(size = 10, face = "plain", 
                                      colour = "#B65A12", angle = 90), 
    
    axis.text.x       = element_text(size = 10, face = "plain", 
                                     colour = "black"), 
    axis.text.y.left  = element_text(size = 9.5, colour = "#174A8B"), 
    axis.text.y.right = element_text(size = 9.5, colour = "#B65A12"), 
    
    axis.line.x       = element_line(linewidth = 0.7, colour = "black"), 
    axis.line.y.left  = element_line(linewidth = 0.7, colour = "#174A8B"), 
    axis.line.y.right = element_line(linewidth = 0.7, colour = "#B65A12"), 
    
    axis.ticks        = element_line(linewidth = 0.6, colour = "black"), 
    axis.ticks.length = unit(3, "pt"), 
    
    plot.margin = margin(5, 10, 5, 10) 
  ) 

# ------------------------------------------------------------ 
# 7. Day strips drawn as a separate cowplot strip below the plot 
# ------------------------------------------------------------ 
day_strip <- ggplot() + 
  annotate("rect", xmin = 0.55, xmax = 3.45, 
           ymin = 0, ymax = 1, fill = "grey90", colour = NA) + 
  annotate("rect", xmin = 4.55, xmax = 7.45, 
           ymin = 0, ymax = 1, fill = "grey90", colour = NA) + 
  annotate("text", x = 2, y = 0.5, label = "Day 7", 
           size = 3.6, fontface = "bold") + 
  annotate("text", x = 6, y = 0.5, label = "Day 30", 
           size = 3.6, fontface = "bold") + 
  scale_x_continuous(limits = c(0.4, 7.6), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 1),  expand = c(0, 0)) + 
  theme_void() 

# ------------------------------------------------------------ 
# 8. Stack plot + day strip 
# ------------------------------------------------------------ 
p_final <- cowplot::plot_grid( 
  p, 
  day_strip, 
  ncol        = 1, 
  rel_heights = c(1, 0.075), 
  align       = "v", 
  axis        = "lr" 
) 

# ------------------------------------------------------------ 
# 9. Display 
# ------------------------------------------------------------ 
print(p_final) 

# ------------------------------------------------------------ 
# 10. Save figures at 15.5 x 11.2 cm (fits portrait Word-page width) 
# ------------------------------------------------------------ 
ggsave( 
  filename = file.path(out_dir, "Fig_S9_Ca_pool_redistribution_NC_Portrait.png"), 
  plot     = p_final, 
  width    = 155, height = 112, units = "mm", 
  dpi      = 600, bg = "white" 
) 

ggsave( 
  filename = file.path(out_dir, "Fig_S9_Ca_pool_redistribution_NC_Portrait.tiff"), 
  plot     = p_final, 
  width    = 155, height = 112, units = "mm", 
  dpi      = 600, compression = "lzw", bg = "white" 
) 

ggsave( 
  filename = file.path(out_dir, "Fig_S9_Ca_pool_redistribution_NC_Portrait.pdf"), 
  plot     = p_final, 
  width    = 155, height = 112, units = "mm", 
  device   = grDevices::pdf, 
  family   = "Helvetica",  # built-in PDF font; no external Arial/CID font needed
  useDingbats = FALSE,
  bg       = "white" 
) 

# ------------------------------------------------------------ 
# 11. Statistics tables 
# ------------------------------------------------------------ 

# Summary table (mean ± SD ± SE per Day × Treatment × Pool) 
summary_stats <- sum_df %>% 
  mutate( 
    Mean_SD = paste0(round(Mean, 3), " ± ", round(SD, 3)), 
    Mean_SE = paste0(round(Mean, 3), " ± ", round(SE, 3)) 
  ) %>% 
  select(Day, Treatment, Pool, n, Mean, SD, SE, Mean_SD, Mean_SE) 

# Wide table (mean ± SD pivoted by Pool) 
summary_wide <- summary_stats %>% 
  select(Day, Treatment, Pool, Mean_SD) %>% 
  pivot_wider(names_from = Pool, values_from = Mean_SD) 

# ------------------------------------------------------------ 
# ANOVA + Tukey per Day × Pool (Treatment effect) 
#   - Returns data frames only (no lists) so group_modify works. 
#   - TukeyHSD output is converted manually (no broom::tidy). 
# ------------------------------------------------------------ 

run_anova_tukey <- function(d) { 
  
  d$Treatment <- factor(d$Treatment, levels = c("CK", "GP", "PEG")) 
  fit <- aov(`Ca (cmol(+)/kg)` ~ Treatment, data = d) 
  
  # --- ANOVA table --- 
  an <- as.data.frame(summary(fit)[[1]]) 
  an$Term <- rownames(an) 
  rownames(an) <- NULL 
  an <- an %>% 
    select(Term, Df, `Sum Sq`, `Mean Sq`, `F value`, `Pr(>F)`) %>% 
    mutate(Stat_type = "ANOVA") 
  
  # --- Tukey HSD table --- 
  tk <- TukeyHSD(fit)$Treatment 
  tk <- as.data.frame(tk) 
  tk$Comparison <- rownames(tk) 
  rownames(tk) <- NULL 
  tk <- tk %>% 
    select(Comparison, diff, lwr, upr, `p adj`) %>% 
    mutate(Stat_type = "Tukey") 
  
  # --- Combine into one data frame --- 
  # Pad columns so both tables share the same schema 
  an_pad <- an %>% 
    transmute( 
      Stat_type, 
      Term         = Term, 
      Comparison   = NA_character_, 
      Df           = Df, 
      Sum_Sq       = `Sum Sq`, 
      Mean_Sq      = `Mean Sq`, 
      F_value      = `F value`, 
      p_value      = `Pr(>F)`, 
      diff         = NA_real_, 
      lwr          = NA_real_, 
      upr          = NA_real_, 
      p_adj        = NA_real_ 
    ) 
  
  tk_pad <- tk %>% 
    transmute( 
      Stat_type, 
      Term         = NA_character_, 
      Comparison   = Comparison, 
      Df           = NA_real_, 
      Sum_Sq       = NA_real_, 
      Mean_Sq      = NA_real_, 
      F_value      = NA_real_, 
      p_value      = NA_real_, 
      diff         = diff, 
      lwr          = lwr, 
      upr          = upr, 
      p_adj        = `p adj` 
    ) 
  
  bind_rows(an_pad, tk_pad) 
} 

stats_all <- df %>% 
  group_by(Day, Pool) %>% 
  group_modify(~ run_anova_tukey(.x)) %>% 
  ungroup() 

# Split into separate tidy tables for the Excel workbook 
anova_table <- stats_all %>% 
  filter(Stat_type == "ANOVA") %>% 
  select(Day, Pool, Term, Df, Sum_Sq, Mean_Sq, F_value, p_value) 

tukey_table <- stats_all %>% 
  filter(Stat_type == "Tukey") %>% 
  select(Day, Pool, Comparison, diff, lwr, upr, p_adj) 

# ------------------------------------------------------------ 
# 12. Save Excel statistics 
# ------------------------------------------------------------ 
xlsx_out <- file.path(out_dir, "Fig_S9_Ca_pool_redistribution_statistics.xlsx") 

writexl::write_xlsx( 
  list( 
    Summary      = summary_stats, 
    Summary_wide = summary_wide, 
    Raw_data     = df, 
    ANOVA        = anova_table, 
    Tukey_HSD    = tukey_table 
  ), 
  path = xlsx_out 
) 

# ------------------------------------------------------------ 
# 13. Done 
# ------------------------------------------------------------ 
message("Finished successfully.") 
message("All outputs saved in: ", normalizePath(out_dir)) 
message(" - Fig_S9_Ca_pool_redistribution_NC_Portrait.png") 
message(" - Fig_S9_Ca_pool_redistribution_NC_Portrait.tiff") 
message(" - Fig_S9_Ca_pool_redistribution_NC_Portrait.pdf") 
message(" - Fig_S9_Ca_pool_redistribution_statistics.xlsx") 