## This script reads in model results and starts creating some plots that might be
## useful.
##
## pjr, 2/28/23
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
path <- "https://drive.google.com/drive/folders/1tl1Kf0PxVHbS7wPL4W071pvS1Af3Ji1i"
local_path <- "pr_work/data/"

# 2. Read in data --------------------------------------------------------------

## Authorize GDrive
options(gargle_oauth_email = "peter.regier@pnnl.gov")

## Pull files
files <- drive_ls(path) %>% 
  filter(grepl("_results", name))

## Download files to /data
for(i in 1:nrow(files)){
  drive_download(files$id[[i]], paste0(local_path, files$name[[i]]), overwrite = T)
}

## Now they're local, read them in and label em
read_data <- function(file){
  read_csv(paste0(local_path, file)) %>% 
    mutate(severity = stringr::word(file, 1, 1, sep = "_"))
}

df_raw <- files$name %>% 
  map(read_data) %>% 
  bind_rows()

## Because it doesn't look like much is going on prior to 2019, trim out data
df_trim <- df_raw %>% 
  filter(dates > "2019-08-30") #%>% 
  #filter(severity != "base")


# 2. TS plots ------------------------------------------------------------------

ggplotly(ggplot(df_trim, aes(dates, `flow_m3s_%chg`, color = severity)) + 
  geom_line() + 
  scale_color_manual(values = color_scheme))

ggplot(df_trim, aes(severity, `flow_m3s_%chg`, fill = severity)) + 
  geom_boxplot() 


c_flows <- df_trim %>% 
  group_by(severity) %>% 
  mutate(c_flow_fire = cumsum(flow_m3s_fire), 
         c_flow_nofire = cumsum(flow_m3s_nofire), 
         delta_c_flow = ((c_flow_fire - c_flow_nofire)/c_flow_nofire) * 100) 

plot_grid(ggplot(c_flows, aes(dates, c_flow_fire, color = severity)) +
  geom_line(), 
  ggplot(c_flows, aes(dates, delta_c_flow, color = severity)) +
    geom_line(), 
  ncol = 1)


cusum_plots <- function(fire_var, no_fire_var){

}

cusum_plots(flow_m3s_fire, flow_m3s_nofire)


c_df <- df_trim %>% 
  group_by(severity) %>% 
  mutate(c_fire = cumsum(flow_m3s_fire - flow_m3s_nofire), 
         per_change = `flow_m3s_%chg`)

plot_grid(ggplot(c_df, aes(dates, c_fire, color = severity)) +
            geom_line() + ggtitle("test"), 
          ggplot(c_df, aes(dates, per_change, color = severity)) +
            geom_line(), 
          ncol = 1)




# To-do list: what is a nice time-series that shows 1) the variation in flow
# throughout the modeled period, and 2) the differences in flow by scenario? 
# Differences aren't big, so not visually obvious that there are big changes. I
# would like to do the climate change scenarios aesthetic a la 
# https://www.nature.com/articles/s43247-022-00391-z and 
# https://www.nature.com/articles/s41558-023-01604-9, but that requires being
# able to see differences between scenarios


## Another task: once that's understood, what would boxplots look like: divide
## by season? Divide by flow regime?





