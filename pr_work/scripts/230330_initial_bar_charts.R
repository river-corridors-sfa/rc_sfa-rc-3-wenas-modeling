## This script reads in model results and starts creating some plots that might be
## useful.
##
## pjr, 2/30/23
##
###############

# 1. Setup ---------------------------------------------------------------------

require(pacman)
p_load(tidyverse,
       PNWColors,
       plotly,
       cowplot,
       googledrive)

## Set ggplot theme and color palette
theme_set(theme_bw())
color_scheme <- PNWColors::pnw_palette("Bay", 4)

## Set data path
summary_path <- "https://drive.google.com/drive/folders/1co4DYxPuvZrQ0jX6dM_aYohYmoq-U0aI"
local_path <- "pr_work/data/"


# 2. Read in data --------------------------------------------------------------

## Authorize GDrive
options(gargle_oauth_email = "peter.regier@pnnl.gov")

## Pull files
files <- drive_ls(summary_path) %>% 
  filter(grepl("stats_summaries", name))

## Download files to /data
for(i in 1:nrow(files)){
  drive_download(files$id[[i]], paste0(local_path, files$name[[i]]), overwrite = T)
}

## Now they're local, read them in and label em
read_data <- function(file){
  read_csv(paste0(local_path, file)) %>% 
    mutate(metric = ifelse(metric == "standard deviation", "sd", metric), 
           metric = ifelse(metric == "75% Quantile", "q75", metric), 
           metric = ifelse(metric == "25% Quantile", "q25", metric)) %>% 
    filter(metric == "mean" | metric == "median" | metric == "q25" | metric == "q75") %>% 
    select(scenario, flow, sed, nitrate, doc, metric) %>% 
    pivot_wider(values_from = c("flow", "sed", "nitrate", "doc"), names_from = "metric")
}

df_raw <- files %>% 
  filter(grepl("PER_", name)) %>% 
  pull(name) %>% 
  map(read_data) %>% 
  bind_rows() %>% 
  distinct()

df_base <- df_raw %>% 
  filter(scenario == "base_unburned")

fire_colors = c("#F5C63C", "#F47F6B", "#BB5098")

df_fires <- df_raw %>% 
  filter(scenario != "base_unburned") %>% 
  separate(scenario, c("x", "percent", "scenario"), sep = "_") %>% 
  mutate(percent = fct_relevel(as.factor(percent), c("10", "15", "20", "25", "30", "40", "45", "50", "60", "75", "90", "100")), 
         scenario = fct_relevel(as.factor(scenario), c("LOW", "MOD", "HIGH"))) %>% 
  select(-x)

make_geom_col <- function(variable_of_interest){
   
  var <- paste0(variable_of_interest, "_mean")
  
  x <- df_fires %>% 
    select(scenario, percent, var)
  
  ggplot(x, aes_string(x = "percent", y = var, fill = "scenario")) +
     geom_col(position = "dodge", color = "black", alpha = 0.5) + 
    geom_hline(yintercept = df_base$flow_mean) +
    scale_fill_manual(values = fire_colors)

}

plot_grid(make_geom_col("flow") + ylab("Flow (m3/s)"),
          make_geom_col("sed") + ylab("Sediment"), 
          make_geom_col("nitrate") + ylab("Nitrate"), 
          make_geom_col("doc") + ylab("DOC (mg/L)"), 
          ncol = 1)
ggsave("figures/230330_fire_characteristics_bar_charts.png", width = 6, height = 12)


flow <- ggplot(df_fires, aes(x = percent, fill = scenario)) + 
  geom_col(aes(y = flow_mean), position = "dodge", color = "black", alpha = 0.5) + 
  geom_hline(yintercept = df_base$flow_mean) +
  scale_fill_manual(values = fire_colors)






