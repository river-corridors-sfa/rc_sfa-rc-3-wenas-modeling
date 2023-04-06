## This script reads in model results and starts creating some plots that might be
## useful.
##
## pjr, 3/30/23
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

## Create raw dataset for fire scenarios
fires_raw <- files %>% 
  filter(grepl("PER_", name)) %>% 
  pull(name) %>% 
  map(read_data) %>% 
  bind_rows() %>% 
  distinct()

## Create raw dataset for watershed scenarios
ws_raw <- files %>% 
  filter(!grepl("PER_", name)) %>% 
  filter(!grepl("old_swat", name)) %>% #manually remove problem file
  filter(!grepl("nofire|unburned", name)) %>% # remove nofire: not needed and not formatted correctly
  pull(name) %>% 
  map(read_data) %>% 
  bind_rows() %>% 
  distinct()

fire_base <- fires_raw %>% 
  filter(scenario == "base_unburned")

fire_colors = c("#F5C63C", "#F47F6B", "#BB5098")

df_fires <- fires_raw %>% 
  filter(scenario != "base_unburned") %>% 
  separate(scenario, c("x", "percent", "scenario"), sep = "_") %>% 
  mutate(percent = fct_relevel(as.factor(percent), c("10", "15", "20", "25", "30", "40", "45", "50", "60", "75", "90", "100")), 
         scenario = fct_relevel(as.factor(scenario), c("LOW", "MOD", "HIGH"))) %>% 
  select(-x)


# 3. Make fire plots -----------------------------------------------------------

make_geom_col_fire <- function(variable_of_interest){
   
  var <- paste0(variable_of_interest, "_mean")
  
  x <- df_fires %>% 
    select(scenario, percent, var)
  
  ggplot(x, aes_string(x = "percent", y = var, fill = "scenario")) +
     geom_col(position = "dodge", color = "black", alpha = 0.5) + 
    geom_hline(yintercept = fire_base$flow_mean) +
    scale_fill_manual(values = fire_colors)

}

plot_grid(make_geom_col_fire("flow") + ylab("Flow (m3/s)"),
          make_geom_col_fire("sed") + ylab("Sediment"), 
          make_geom_col_fire("nitrate") + ylab("Nitrate"), 
          make_geom_col_fire("doc") + ylab("DOC (mg/L)"), 
          ncol = 1)
ggsave("figures/230330_fire_characteristics_bar_charts.png", width = 6, height = 12)


flow <- ggplot(df_fires, aes(x = percent, fill = scenario)) + 
  geom_col(aes(y = flow_mean), position = "dodge", color = "black", alpha = 0.5) + 
  geom_hline(yintercept = df_base$flow_mean) +
  scale_fill_manual(values = fire_colors)


# 4. Make watershed characteristics plots --------------------------------------

## First, set up classifications
df_ws <- ws_raw %>% 
  separate(scenario, c("ID", "scenario"), sep = "_", extra = "merge") %>% 
  separate(scenario, c("scenario", "SLP"), sep = "_", extra = "merge")

## Next, set up the plotting function
make_geom_col_ws <- function(variable_of_interest){
  
  var <- paste0(variable_of_interest, "_mean")
  
  x <- df_ws %>% 
    select(scenario, percent, var)
  
  ggplot(x, aes_string(x = "percent", y = var, fill = "scenario")) +
    geom_col(position = "dodge", color = "black", alpha = 0.5) + 
    geom_hline(yintercept = fire_base$flow_mean) +
    scale_fill_manual(values = fire_colors)
  
}


colnames(df_ws)

## For SLP, because names have different #s of things, we need to get creative
df_ws %>% filter(scenario == "SLP") %>% 
  mutate(sign = ifelse(grepl("neg", SLP), "neg", "pos")) %>% # Create a col for sign
  mutate(SLP = str_remove(SLP, "neg_")) %>% 
  separate(SLP, c("slope", "fire"), sep = "_")


ggplot(df_ws, aes(ID, ))



