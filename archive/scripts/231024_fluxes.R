## Calculate fluxes
## We're going out on a limb to see if this is worthwhile. The
## underlying question is: do we see clear patterns in the differences
## in fluxes or yields between unburned and severity or percentage
## gradients for our fluxes? Would be impactful for managers!
##

# 1. Setup ---------------------------------------------------------------------

require(pacman)
p_load(tidyverse,
       ggpubr,
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


# 3. Calculate fluxes

calculate_fluxes <- function(flow_var, conc_var){
  
}

## DOC
p_doc <- df %>% 
  group_by(severity, percent) %>% 
  select(dates, flow_m3s_fire, doc_mg_l_fire) %>% 
  #(mg/L) * (kg / 1e6 mg) * (1000 L / m3) 
  mutate(solute_kg_m3 = doc_mg_l_fire * (1e-6) * 1000, 
         # (m3/s) * (60*60*24 s / d)
         flow_m3_d = flow_m3s_fire * (60*60*24)) %>% 
  mutate(solute_kg_d = solute_kg_m3 * flow_m3_d) %>% 
  summarize(solute_kg_yr = sum(solute_kg_d, na.rm = T)) %>% 
  ggplot(aes(percent, solute_kg_yr, fill = severity)) + 
  geom_col(position = "dodge") + 
  ylab("DOC")


## NO3
p_no3 <- df %>% 
  group_by(severity, percent) %>% 
  select(dates, flow_m3s_fire, nit_mg_l_fire) %>% 
  #(mg/L) * (kg / 1e6 mg) * (1000 L / m3) 
  mutate(solute_kg_m3 = nit_mg_l_fire * (1e-6) * 1000, 
         # (m3/s) * (60*60*24 s / d)
         flow_m3_d = flow_m3s_fire * (60*60*24)) %>% 
  mutate(solute_kg_d = solute_kg_m3 * flow_m3_d) %>% 
  summarize(solute_kg_yr = sum(solute_kg_d, na.rm = T)) %>% 
  ggplot(aes(percent, solute_kg_yr, fill = severity)) + 
  geom_col(position = "dodge") + 
  ylab("NO3")

## TSS
p_sed <- df %>% 
  group_by(severity, percent) %>% 
  select(dates, flow_m3s_fire, sed_mg_l_fire) %>% 
  #(mg/L) * (kg / 1e6 mg) * (1000 L / m3) 
  mutate(solute_kg_m3 = sed_mg_l_fire * (1e-6) * 1000, 
         # (m3/s) * (60*60*24 s / d)
         flow_m3_d = flow_m3s_fire * (60*60*24)) %>% 
  mutate(solute_kg_d = solute_kg_m3 * flow_m3_d) %>% 
  summarize(solute_kg_yr = sum(solute_kg_d, na.rm = T)) %>% 
  ggplot(aes(percent, solute_kg_yr, fill = severity)) + 
  geom_col(position = "dodge") + 
  ylab("TSS")

plot_grid(p_sed, p_no3, p_doc, ncol = 1)
ggsave("figures/231024_fluxes_for_fig3.png", width = 6, height = 8)




p_sed2 <- df %>% 
  group_by(severity, percent) %>% 
  select(dates, flow_m3s_fire, sed_mg_l_fire) %>% 
  #(mg/L) * (kg / 1e6 mg) * (1000 L / m3) 
  mutate(solute_kg_m3 = sed_mg_l_fire * (1e-6) * 1000, 
         # (m3/s) * (60*60*24 s / d)
         flow_m3_d = flow_m3s_fire * (60*60*24)) %>% 
  mutate(solute_kg_d = solute_kg_m3 * flow_m3_d) %>% 
  mutate(cusum = cumsum(solute_kg_d)) %>% 
  ggplot(aes(dates, cusum, color = percent)) + 
  geom_path() + 
  facet_wrap(~severity, ncol = 1) + 
  ylab("TSS (kg/d)")
  
p_nit2 <- df %>% 
  group_by(severity, percent) %>% 
  select(dates, flow_m3s_fire, nit_mg_l_fire) %>% 
  #(mg/L) * (kg / 1e6 mg) * (1000 L / m3) 
  mutate(solute_kg_m3 = nit_mg_l_fire * (1e-6) * 1000, 
         # (m3/s) * (60*60*24 s / d)
         flow_m3_d = flow_m3s_fire * (60*60*24)) %>% 
  mutate(solute_kg_d = solute_kg_m3 * flow_m3_d) %>% 
  mutate(cusum = cumsum(solute_kg_d)) %>% 
  ggplot(aes(dates, cusum, color = percent)) + 
  geom_path() + 
  facet_wrap(~severity, ncol = 1) + 
  ylab("NO3 (kg/d)")

p_doc2 <- df %>% 
  group_by(severity, percent) %>% 
  select(dates, flow_m3s_fire, doc_mg_l_fire) %>% 
  #(mg/L) * (kg / 1e6 mg) * (1000 L / m3) 
  mutate(solute_kg_m3 = doc_mg_l_fire * (1e-6) * 1000, 
         # (m3/s) * (60*60*24 s / d)
         flow_m3_d = flow_m3s_fire * (60*60*24)) %>% 
  mutate(solute_kg_d = solute_kg_m3 * flow_m3_d) %>% 
  mutate(cusum = cumsum(solute_kg_d)) %>% 
  ggplot(aes(dates, cusum, color = percent)) + 
  geom_path() + 
  facet_wrap(~severity, ncol = 1) + 
  ylab("DOC (kg/d)")

plot_grid(p_sed2, p_nit2, p_doc2, nrow = 1)
ggsave("figures/231024_c_fluxes.png", width = 15, height = 6)




