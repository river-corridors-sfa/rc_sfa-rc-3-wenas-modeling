## This script creates summaries because stat_summary files aren't correct on GDrive
##
## pjr, 4/10/23
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
path <- "https://drive.google.com/drive/folders/1co4DYxPuvZrQ0jX6dM_aYohYmoq-U0aI"
local_path <- "pr_work/data/"


# 2. Read in data --------------------------------------------------------------

## Authorize GDrive
options(gargle_oauth_email = "peter.regier@pnnl.gov")

## Pull files
files <- drive_ls(path) %>% 
  filter(grepl("_results", name)) %>% 
  filter(!grepl("PER_", name)) %>% 
  filter(!grepl("MOD|HIGH|LOW", name))

## Download files to /data
for(i in 1:nrow(files)){
  drive_download(files$id[[i]], paste0(local_path, files$name[[i]]), overwrite = T)
}

## Now they're local, read them in and label em
read_data <- function(file){
  read_csv(paste0(local_path, file)) %>% 
    mutate(source = file) %>% 
    arrange(source)
}

df_raw <- files$name %>% 
  map(read_data) %>% 
  bind_rows() %>% 
  filter(dates > "2019-08-30")

ggplot(df_raw, aes(dates, flow_m3s_fire, color = source)) + geom_line()


## Summary dataframe
df_stats <- df_raw %>% 
  group_by(source) %>% 
  summarize(mean_flow_fire = mean(flow_m3s_fire, na.rm = T), 
            mean_flow_nofire = mean(flow_m3s_nofire, na.rm = T), 
            mean_sed_fire = mean(sed_kgday_fire, na.rm = T), 
            mean_sed_nofire = mean(sed_kgday_nofire, na.rm = T), 
            mean_no3_fire = mean(nit_kgday_fire, na.rm = T), 
            mean_no3_nofire = mean(nit_kgday_nofire, na.rm = T)) %>% 
  mutate(diff_flow = ((mean_flow_fire - mean_flow_nofire) / mean_flow_nofire) * 100, 
         diff_sed = ((mean_sed_fire - mean_sed_nofire) / mean_sed_nofire) * 100, 
         diff_no3 = ((mean_no3_fire - mean_no3_nofire) / mean_no3_nofire) * 100) %>% 
  filter(!grepl("base_burned", source)) %>% 
  mutate(source = str_remove_all(source, "_fire_results.csv|HRU_")) %>% #Remove unneeded text before parsing
  separate(source, into = c("source", "slp"), "_", extra = "merge") %>% 
  mutate(slp = ifelse(grepl("neg_", slp), str_replace(slp, "neg_", "-"), slp)) %>% 
  mutate(type = case_when(!is.na(slp) ~ "Slope", 
                          source == "DRY" | source == "WET" ~ "Climate", 
                          grepl("Q", source) ~ "Flow", 
                          TRUE ~"Vegetation")) %>% 
  mutate(source = ifelse(!is.na(slp), slp, source))

## Function to make plots
create_plot <- function(var, y_label){
  ggplot(df_stats, aes_string("source", var)) + 
    geom_col(fill = "lightblue", color = "black") + 
    facet_grid(~type, scales = "free_x", space = "free") + 
    labs(x = "", y = y_label, 
         title = str_to_upper(str_remove(var, "diff_")))
}

## Make figure
plot_grid(create_plot("diff_flow", "Change in flow (%)"), 
          create_plot("diff_sed", "Change in sediment (%)") + scale_y_log10(), 
          create_plot("diff_no3", "Change in NO3 (%)"), 
          ncol = 1, align = "hv")
ggsave("pr_work/figures/230410_scenario_percent_changes.png", width = 8, height = 9)

