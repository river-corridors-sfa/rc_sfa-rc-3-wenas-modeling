## This script explores how to identify thresholds through a couple different
## methods. The first approach will be 
##
## 2023-10-17
## Peter Regier
## 
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

require(pacman)
p_load(tidyverse,
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


# 3. Calculate percent exceedences ---------------------------------------------
## big props to chatgpt for helping sort out this workflow!

## Function to calculate the number of dates where the no-fire and fire values for 
## a given
calculate_exceedence <- function(my_scenario, 
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
  quantile_value <- quantile(data %>% pull(col_nofire), 0.75)
  
  # Calculate the number of rows in the data
  n_total <- nrow(data)
  
  # Calculate the number of rows exceeding the 75th quantile for _nofire and _fire
  n_nofire <- nrow(data %>% filter(!!sym(col_nofire) > quantile_value))
  n_fire <- nrow(data %>% filter(!!sym(col_fire) > quantile_value))
  
  # Calculate the percentage of exceedence for _nofire and _fire
  per_ex_nofire <- (n_nofire / n_total) * 100
  per_ex_fire <- (n_fire / n_total) * 100
  
  # Extract "percent" and "severity" from my_scenario using stringr
  percent <- as.numeric(str_extract(my_scenario, "\\d+\\.?\\d*"))
  severity <- str_extract(my_scenario, "([A-Z]+)$")
  
  # Create a data frame with the results
  result_df <- tibble(
    percent = percent, 
    severity = severity, 
    column_prefix = col_prefix,
    quantile_value = quantile_value,
    n_total = n_total,
    n_nofire = n_nofire,
    n_fire = n_fire,
    per_ex_nofire = per_ex_nofire,
    per_ex_fire = per_ex_fire
  )
  
  return(result_df)
}

all_scenarios <- expand_grid(scenario = unique(df$scenario), 
                             col_name = c("flow", "sed", "nit", "doc"))

percents <- pmap_df(list(all_scenarios$scenario, all_scenarios$col_name), calculate_exceedence) %>% 
  mutate(percent = as.factor(as.numeric(percent)), 
         severity = factor(severity, levels = c("LOW", "MOD", "HIGH"))) %>% 
  mutate(diff_fire_nofire = per_ex_fire - per_ex_nofire) %>% 
  mutate(var = fct_relevel(column_prefix, "flow", "sed", "nit", "doc"))



ggplot(percents) + 
  geom_col(aes(x = as.factor(percent), y = diff_fire_nofire, fill = severity), 
           position = "dodge", alpha = 0.5) + 
  # geom_hline(data = percents %>% ) + 
  facet_wrap(~var) + 
  scale_fill_manual(values = severity_colors) + 
  labs(x = "Percent", y = "% (fire) - % (no fire)")
ggsave("figures/4_fig4_quantile_exceedence_v1.png", width = 10, height = 7)



