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
  #filter(percent != "base") %>% 
  mutate(percent = as.factor(as.numeric(percent)), 
         severity = factor(severity, levels = c("LOW", "MOD", "HIGH")))

plot_ecdf <- function(var){
  
  x <- df %>%
    filter(scenario != "base_burned") %>%
    filter(percent != "base")
  
    ggplot(x, aes({{var}}, color = percent)) +
    stat_ecdf() + 
    scale_x_log10() +
    #geom_vline(xintercept = median(no_fire_flow$flow_m3s_nofire), color = "black")+
    #geom_vline(xintercept = quantile(no_fire_flow$flow_m3s_nofire)[4], color = "black",linetype="dashed") +
    #scale_color_manual(values = severity_colors)+
    geom_vline(xintercept = mean(x %>% pull({{var}}), na.rm = T), linetype = "dashed") + 
    theme_bw() + 
    facet_wrap(~severity, nrow = 1)
}


plot_grid(plot_ecdf(sed_mg_l_fire) + ylab("ECDF: Sed"),
          plot_ecdf(nit_mg_l_fire)  + ylab("ECDF: NO3"),
          plot_ecdf(doc_mg_l_fire)  + ylab("ECDF: DOC"),
          ncol = 1)
ggsave("figures/231031_solute_ecdfs.png", width = 9, height = 10)


plot_distribution <- function(var){
  
  x <- df %>%
    filter(scenario != "base_burned") %>%
    filter(percent != "base")
  
  y <- df %>%
    filter(scenario == "base")
  
  ggplot() +
    geom_density(data = x, aes({{var}}, color = percent, fill = percent), alpha = 0.05) + 
    scale_x_log10() +
    #geom_vline(xintercept = median(no_fire_flow$flow_m3s_nofire), color = "black")+
    #geom_vline(xintercept = quantile(no_fire_flow$flow_m3s_nofire)[4], color = "black",linetype="dashed") +
    #scale_color_manual(values = severity_colors)+
    geom_vline(xintercept = mean(x %>% pull({{var}}), na.rm = T), linetype = "dashed") + 
    theme_bw() +
    facet_wrap(~severity, nrow = 1) + 
    geom_density(data = y, aes({{var}}), alpha = 0.5, color = "black") 
    
}

plot_distribution(nit_mg_l_fire)





