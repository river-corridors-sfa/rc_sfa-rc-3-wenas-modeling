## ECDF plots for all solutes
## This is hard to look at so, try a different way to plot the data
##Following: https://vt-hydroinformatics.github.io/fdcs.html

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

plot_ecdf <- function(var){
  df %>%
    filter(!scenario == "base_burned") %>%
    filter(!percent == "base") %>%
    ggplot(aes({{var}}, color = percent))+
    stat_ecdf()+
    scale_x_log10()+
    #geom_vline(xintercept = median(no_fire_flow$flow_m3s_nofire), color = "black")+
    #geom_vline(xintercept = quantile(no_fire_flow$flow_m3s_nofire)[4], color = "black",linetype="dashed") +
    #scale_color_manual(values = severity_colors)+
    theme_bw() + 
    facet_wrap(~severity, nrow = 1)
}


plot_ecdf(sed_mg_l_fire)
plot_ecdf(nit_mg_l_fire)
plot_ecdf(doc_mg_l_fire)





