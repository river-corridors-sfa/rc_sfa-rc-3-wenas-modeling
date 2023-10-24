## This script creates bar charts showing how changes in burn severity and 
## percentage impact flow, TSS, NO3, and DOC
##
## 2023-09-26
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

df <- read_csv("data/outputs/2023-10-10_output_datset_L2_means.csv") %>% 
  mutate(severity = fct_relevel(severity, "LOW", "MOD", "HIGH"))


# 3. Make absolute value plots ---------------------------------------------------

mean_ <- function(...){mean(..., na.rm = T)}

make_geom_col_abs <- function(var, ref, y_label){
  
  nofire_ref <- mean_(df %>% pull({{ref}}))
  
  ggplot(df, aes(as.factor(percent), {{var}}, fill = severity)) + 
    geom_col(position = "dodge", color = "black", alpha = 0.5) + 
    geom_hline(yintercept = nofire_ref) + 
    scale_fill_manual(values = severity_colors) + 
    labs(x = "Percent of watershed burned", 
         y = y_label, 
         fill = "Fire \n Severity") + 
    theme(legend.title = element_text(hjust = 0.5))
}

plot_grid(make_geom_col_abs(sed_mg_l_fire, sed_mg_l_nofire, "TSS (mg/L)"),
          make_geom_col_abs(nit_mg_l_fire, nit_mg_l_nofire, "NO3 (mg/L)"),
          make_geom_col_abs(doc_mg_l_fire, doc_mg_l_nofire, "DOC (mg/L)"),
          ncol = 1, align = "hv", 
          labels = c("A", "B", "C"))
ggsave("figures/3_Fig3_absolute_changes.png", width = 6, height = 7)


# 3. Make percent plots --------------------------------------------------------

make_geom_col_perc <- function(var, y_label){
  ggplot(df, aes(as.factor(percent), {{var}}, fill = severity)) + 
    geom_col(position = "dodge", color = "black", alpha = 0.5) + 
    scale_fill_manual(values = severity_colors) + 
    labs(x = "Percent of watershed burned", 
         y = y_label, 
         fill = "Fire \n Severity") + 
    theme(legend.title = element_text(hjust = 0.5))
}

plot_grid(make_geom_col_perc(sed_perc, "% Change (TSS, mg/L)"), 
          make_geom_col_perc(nitrate_perc, "% Change (NO3, mg/L)"), 
          make_geom_col_perc(doc_perc, "% Change (DOC, mg/L)"), 
          ncol = 1, align = "hv", 
          labels = c("A", "B", "C"))
ggsave("figures/S1_percent_changes.png", width = 6, height = 7)




