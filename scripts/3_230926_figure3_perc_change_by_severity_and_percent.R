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

## Helper function
mean_ <- function(...) mean(..., na.rm=T)

## Since we're focused on burn severity/percent, filter out other categories
df_perc <- read_csv("data/outputs/2023-09-26_output_datset_L1.csv") %>% 
  filter(percent != "base") %>% 
  mutate(percent = as.factor(as.numeric(percent)), 
         severity = factor(severity, levels = c("LOW", "MOD", "HIGH"))) %>% 
  group_by(severity, percent) %>% 
  summarize(flow_perc = mean_(flow_perc), 
            sed_perc = mean_(sed_perc), 
            nitrate_perc = mean_(nitrate_perc), 
            doc_perc = mean_(doc_perc))

df_abs <- read_csv("data/outputs/2023-09-26_output_datset_L1.csv") %>% 
  filter(percent != "base") %>% 
  mutate(percent = as.factor(as.numeric(percent)), 
         severity = factor(severity, levels = c("LOW", "MOD", "HIGH"))) %>% 
  group_by(severity, percent) %>% 
  summarize(flow_fire = mean_(flow_m3s_fire), 
            flow_nofire = mean_(flow_m3s_nofire), 
            sed_fire = mean_(sed_mg_l_fire), 
            sed_nofire = mean_(sed_mg_l_nofire), 
            nit_fire = mean_(nit_mg_l_fire), 
            nit_nofire = mean_(nit_mg_l_nofire), 
            doc_fire = mean_(doc_mg_l_fire), 
            doc_nofire = mean_(doc_mg_l_nofire))

# 3. Make percent plots --------------------------------------------------------

make_geom_col_perc <- function(var, y_label){
  ggplot(df_perc, aes(percent, {{var}}, fill = severity)) + 
    geom_col(position = "dodge", color = "black", alpha = 0.5) + 
    scale_fill_manual(values = severity_colors) + 
    labs(x = "Percent of watershed burned", 
         y = y_label, 
         fill = "Fire \n Severity")
}

plot_grid(make_geom_col_perc(flow_perc, "% Change (Flow, m3/s)"),
           make_geom_col_perc(sed_perc, "% Change (TSS, mg/L)"), 
          make_geom_col_perc(nitrate_perc, "% Change (NO3, mg/L)"), 
          make_geom_col_perc(doc_perc, "% Change (DOC, mg/L)"), 
          ncol = 1, align = "hv")
ggsave("figures/230927_Fig3_percents.png", width = 6, height = 8)


## Make absolute value plots ---------------------------------------------------

make_geom_col_abs <- function(var, ref, y_label){
  
  nofire_ref <- mean_(df_abs %>% pull({{ref}}))
  
  ggplot(df_abs, aes(percent, {{var}}, fill = severity)) + 
    geom_col(position = "dodge", color = "black", alpha = 0.5) + 
    geom_hline(yintercept = nofire_ref) + 
    scale_fill_manual(values = severity_colors) + 
    labs(x = "Percent of watershed burned", 
         y = y_label, 
         fill = "Fire \n Severity")
}

plot_grid(make_geom_col_abs(flow_fire, flow_nofire, "Flow (m3/s)"),
          make_geom_col_abs(sed_fire, sed_nofire, "TSS (mg/L)"),
          make_geom_col_abs(nit_fire, nit_nofire, "NO3 (mg/L"),
          make_geom_col_abs(doc_fire, doc_nofire, "DOC (mg/L)"),
          ncol = 1, align = "hv")
ggsave("figures/230927_Fig3_absolute.png", width = 6, height = 8)

