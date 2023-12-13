## This script creates all figures that AMP needs for AGU
## mercilessly copied from other scripts to keep em all in the same place
## 
## 2023-11-20
## Peter Regier

# 1. Setup ---------------------------------------------------------------------

require(pacman)
p_load(tidyverse,
       ggpubr,
       strucchange,
       segmented,
       cowplot)

## Set ggplot theme and color palette
theme_set(theme_bw())
#fire_colors = c("#F5C63C", "#F47F6B", "#BB5098")
severity_colors = c("#3B9AB2", "#E1AF00", "#F21A00")


# 2. Read in data --------------------------------------------------------------

df <- read_csv("data/outputs/2023-10-05_output_datset_L1.csv") %>% 
  filter(percent != "base") %>% 
  mutate(percent = as.factor(as.numeric(percent)), 
         severity = factor(severity, levels = c("LOW", "MOD", "HIGH")))


# 3. Make percent version of Fig 3 ---------------------------------------------

df_means <- read_csv("data/outputs/2023-10-10_output_datset_L2_means.csv") %>% 
  mutate(severity = fct_relevel(severity, "LOW", "MOD", "HIGH"))

mean_ <- function(...){mean(..., na.rm = T)}

make_geom_col_abs <- function(var, ref, y_label){
  
  nofire_ref <- mean_(df_means %>% pull({{ref}}))
  
  ggplot(df_means, aes(as.factor(percent), {{var}}, fill = severity)) + 
    geom_col(position = "dodge", color = "black", alpha = 0.5) + 
    geom_hline(yintercept = nofire_ref) + 
    scale_fill_manual(values = severity_colors) + 
    labs(x = "Simulated % of watershed burned", 
         y = y_label, 
         fill = "Fire \n Severity") + 
    theme(legend.title = element_text(hjust = 0.5))
}

model <- lm(doc_mg_l_fire ~ percent, data = df_means %>% filter(severity == "HIGH"))
breakpoints_model <- breakpoints(model)

# Print and plot results
summary(breakpoints_model)
plot(breakpoints_model)

ggplot(df_means %>% filter(severity == "HIGH"), 
       aes(as.factor(percent), doc_mg_l_fire)) + 
  geom_point()

df_means %>% filter(severity == "HIGH") %>% 
  select(percent, doc_mg_l_fire)

seg_model <- function(var, psi_value){
 
  formula = as.formula(paste(var, "~ percent"))
  
  low_model <- lm(formula, data = df_means %>% filter(severity == "LOW"))
  mod_model <- lm(formula, data = df_means %>% filter(severity == "MOD"))
  high_model <- lm(formula, data = df_means %>% filter(severity == "HIGH"))
  
  low_seg_model <- segmented(low_model, seg.Z = ~percent, psi = c(psi_value))
  mod_seg_model <- segmented(mod_model, seg.Z = ~percent, psi = c(psi_value))
  high_seg_model <- segmented(high_model, seg.Z = ~percent, psi = c(psi_value))
  
  as_tibble(summary(low_seg_model)[[12]]) %>% 
    add_row(as_tibble(summary(mod_seg_model)[[12]])) %>% 
    add_row(as_tibble(summary(mod_seg_model)[[12]])) %>% 
    mutate(severity = c("LOW", "MOD", "HIGH")) %>% 
    mutate(var = var)
}

bind_rows(seg_model("sed_mg_l_fire", 50), 
          seg_model("nit_mg_l_fire", 50), 
          seg_model("doc_mg_l_fire", 50))


model <- lm(doc_mg_l_fire ~ percent, data = df_means %>% filter(severity == "LOW"))
seg_model <- segmented(model, seg.Z = ~percent, psi = c(60))

# Print the segmented regression model
x <- summary(seg_model)

x[[12]]


p_width = 6
p_height = 2.5

plot_grid(make_geom_col_abs(sed_mg_l_fire, sed_mg_l_nofire, "TSS (mg/L)"),
          make_geom_col_abs(nit_mg_l_fire, nit_mg_l_nofire, "NO3 (mg/L)"),
          make_geom_col_abs(doc_mg_l_fire, doc_mg_l_nofire, "DOC (mg/L)"),
          ncol = 1, align = "hv", 
          labels = c("A", "B", "C"))
ggsave("figures/agu_amp/3_Fig3_absolute_changes.png", width = 6, height = 7)

make_geom_col_abs(sed_mg_l_fire, sed_mg_l_nofire, "TSS (mg/L)")
ggsave("figures/agu_amp/3a_Fig3_tss.png", width = p_width, height = p_height)

make_geom_col_abs(nit_mg_l_fire, nit_mg_l_nofire, "NO3 (mg/L)")
ggsave("figures/agu_amp/3b_Fig3_nit.png", width = p_width, height = p_height)

make_geom_col_abs(doc_mg_l_fire, doc_mg_l_nofire, "DOC (mg/L)")
ggsave("figures/agu_amp/3c_Fig3_doc.png", width = p_width, height = p_height)

make_geom_col_perc <- function(var, y_label){
  ggplot(df_means, aes(as.factor(percent), {{var}}, fill = severity)) + 
    geom_col(position = "dodge", color = "black", alpha = 0.5) + 
    scale_fill_manual(values = severity_colors) + 
    labs(x = "Simulated % of watershed burned", 
         y = y_label, 
         fill = "Fire \n Severity") + 
    theme(legend.title = element_text(hjust = 0.5))
}

plot_grid(make_geom_col_perc(sed_perc, "% Change (TSS, mg/L)"), 
          make_geom_col_perc(nitrate_perc, "% Change (NO3, mg/L)"), 
          make_geom_col_perc(doc_perc, "% Change (DOC, mg/L)"), 
          ncol = 1, align = "hv", 
          labels = c("A", "B", "C"))
ggsave("figures/agu_amp/S1_percent_changes.png", width = 6, height = 7)

make_geom_col_perc(sed_perc, "% Change (TSS, mg/L)")
ggsave("figures/agu_amp/S1a_Fig3_tss.png", width = p_width, height = p_height)

make_geom_col_perc(nitrate_perc, "% Change (NO3, mg/L)")
ggsave("figures/agu_amp/S1b_Fig3_nit.png", width = p_width, height = p_height)

make_geom_col_perc(doc_perc, "% Change (DOC, mg/L)")
ggsave("figures/agu_amp/S1c_Fig3_doc.png", width = p_width, height = p_height)


# 4. Calculate percent exceedences ---------------------------------------------
## big props to chatgpt for helping sort out this workflow!

## Function to calculate the number of dates where the no-fire and fire values for 
## a given
calculate_exceedences <- function(my_scenario, 
                                  col_prefix) {
  
  data <- df %>% 
    filter(scenario == my_scenario)
  
  # Construct regular expressions to match the column names
  nofire_pattern <- paste0("^", col_prefix, ".*_nofire$")
  fire_pattern <- paste0("^", col_prefix, ".*_fire$")
  
  # Find the column names that match the patterns
  col_nofire <- grep(nofire_pattern, names(data), value = TRUE)
  col_fire <- grep(fire_pattern, names(data), value = TRUE)
  
  if (length(col_nofire) == 0 || length(col_fire) == 0) {
    stop("No matching columns found for the specified prefix.")
  }
  
  # Calculate the 75th quantile for the _nofire column
  upper_quantile_value <- quantile(data %>% pull(col_nofire), 0.75)
  lower_quantile_value <- quantile(data %>% pull(col_nofire), 0.25)
  
  # Calculate the number of rows in the data
  n_total <- nrow(data)
  
  # Calculate the number of rows exceeding the 75th quantile for _nofire and _fire
  n_75_nofire <- nrow(data %>% filter(!!sym(col_nofire) > upper_quantile_value))
  n_75_fire <- nrow(data %>% filter(!!sym(col_fire) > upper_quantile_value))
  n_25_nofire <- nrow(data %>% filter(!!sym(col_nofire) < lower_quantile_value))
  n_25_fire <- nrow(data %>% filter(!!sym(col_fire) < lower_quantile_value))
  
  # Calculate the percentage of exceedence for _nofire and _fire
  per_ex_75_nofire <- (n_75_nofire / n_total) * 100
  per_ex_75_fire <- (n_75_fire / n_total) * 100
  per_ex_25_nofire <- (n_25_nofire / n_total) * 100
  per_ex_25_fire <- (n_25_fire / n_total) * 100
  
  # Extract "percent" and "severity" from my_scenario using stringr
  percent <- as.numeric(str_extract(my_scenario, "\\d+\\.?\\d*"))
  severity <- str_extract(my_scenario, "([A-Z]+)$")
  
  # Create a data frame with the results
  result_df <- tibble(
    percent = percent, 
    severity = severity, 
    column_prefix = col_prefix,
    q25 = lower_quantile_value,
    q75 = upper_quantile_value,
    n_total = n_total,
    n_75_nofire = n_75_nofire,
    n_75_fire = n_75_fire,
    per_ex_75_nofire = per_ex_75_nofire,
    per_ex_75_fire = per_ex_75_fire, 
    n_25_nofire = n_25_nofire,
    n_25_fire = n_25_fire,
    per_ex_25_nofire = per_ex_25_nofire,
    per_ex_25_fire = per_ex_25_fire
  )
  
  return(result_df)
}

all_scenarios <- expand_grid(scenario = unique(df$scenario), 
                             col_name = c("sed", "nit", "doc"))

percents <- pmap_df(list(all_scenarios$scenario, all_scenarios$col_name), calculate_exceedences) %>% 
  mutate(percent = as.factor(as.numeric(percent)), 
         severity = factor(severity, levels = c("LOW", "MOD", "HIGH"))) %>% 
  mutate(diff_fire_nofire = per_ex_75_fire - per_ex_75_nofire) %>% 
  mutate(var = fct_relevel(column_prefix, "sed", "nit", "doc"))

ggplot(percents) + 
  geom_col(aes(x = as.factor(percent), y = per_ex_75_fire, fill = severity), 
           position = "dodge", color = "black", alpha = 0.5) + 
  geom_hline(aes(yintercept = per_ex_75_nofire)) + 
  facet_wrap(~var, ncol = 1, scale = "free_y") + 
  scale_fill_manual(values = severity_colors) + 
  labs(x = "Simulated % of watershed burned", 
       y = "% days exceeding 75th quantile (no fire)", 
       fill = "Fire \n severity") + 
  ggtitle("Days exceeding the 75th quantile of the unburned simulation") + 
  theme(legend.title = element_text(hjust = 0.5))
ggsave("figures/agu_amp/4_fig4_quantile_exceedence_75th.png", width = 7, height = 8)


ggplot(percents) + 
  geom_col(aes(x = as.factor(percent), y = per_ex_25_fire, fill = severity), 
           position = "dodge", color = "black", alpha = 0.5) + 
  geom_hline(aes(yintercept = per_ex_25_nofire)) + 
  facet_wrap(~var, ncol = 1, scale = "free_y") + 
  scale_fill_manual(values = severity_colors) + 
  labs(x = "Simulated % of watershed burned", 
       y = "% days below 25th quantile (no fire)", 
       fill = "Fire \n severity") + 
  ggtitle("Days below the 25th quantile of the unburned simulation") + 
  theme(legend.title = element_text(hjust = 0.5))
ggsave("figures/agu_amp/4_fig4_quantile_exceedence_25th.png", width = 7, height = 8)


## Just DOC 
ggplot(percents %>% filter(column_prefix == "doc")) + 
  geom_col(aes(x = as.factor(percent), y = per_ex_75_fire, fill = severity), 
           position = "dodge", color = "black", alpha = 0.5) + 
  geom_hline(aes(yintercept = per_ex_75_nofire)) + 
  facet_wrap(~var, ncol = 1, scale = "free_y") + 
  scale_fill_manual(values = severity_colors) + 
  labs(x = "Simulated % of watershed burned", 
       y = "% days exceeding 75th quantile (no fire)", 
       fill = "Fire \n severity") + 
  ggtitle("Days exceeding the 75th quantile of the unburned simulation") + 
  theme(legend.title = element_text(hjust = 0.5))
ggsave("figures/agu_amp/4a_fig4_quantile_exceedence_75th_doc.png", width = 7, height = 2.5)


ggplot(percents %>% filter(column_prefix == "doc")) + 
  geom_col(aes(x = as.factor(percent), y = per_ex_25_fire, fill = severity), 
           position = "dodge", color = "black", alpha = 0.5) + 
  geom_hline(aes(yintercept = per_ex_25_nofire)) + 
  facet_wrap(~var, ncol = 1, scale = "free_y") + 
  scale_fill_manual(values = severity_colors) + 
  labs(x = "Simulated % of watershed burned", 
       y = "% days below 25th quantile (no fire)", 
       fill = "Fire \n severity") + 
  ggtitle("Days below the 25th quantile of the unburned simulation") + 
  theme(legend.title = element_text(hjust = 0.5))
ggsave("figures/agu_amp/4b_fig4_quantile_exceedence_25th_doc.png", width = 7, height = 2.5)


## Calculate fluxes figure for DOC ---------------------------------------------

percent_burn_colors = c("#99d98c","#76c893","#52b69a","#005F73","#0A9396","#94D2BD","#E9D8A6","#EE9B00","#CA6702","#BB3E03","#AE2012","#9B2226")


calculate_fluxes <- function(flow_var, conc_var){
  
}

## DOC
df %>% 
  group_by(severity, percent) %>% 
  select(dates, flow_m3s_fire, doc_mg_l_fire) %>% 
  mutate(solute_kg_m3 = doc_mg_l_fire * (1e-6) * 1000, 
         flow_m3_d = flow_m3s_fire * (60*60*24)) %>% 
  mutate(solute_kg_d = solute_kg_m3 * flow_m3_d) %>% 
  mutate(cusum = cumsum(solute_kg_d)) %>% 
  ggplot(aes(dates, cusum, color = percent)) + 
  geom_path(lwd = 1.5, alpha = 0.3) + 
  geom_path() + 
  scale_color_manual(values = percent_burn_colors) + 
  facet_wrap(~severity, ncol = 1) + 
  labs(x = "", y = "DOC loads (kg/d)", color = "Percent \n burned") + 
  theme(legend.title = element_text(hjust = 0.5))
ggsave("figures/agu_amp/5_doc_fluxes.png", width = 7, height = 6)


