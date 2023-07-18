## There have been several iterations of coding to calculate the stats and make the 
## plots needed for the various different decisions on how we're going to analyze
## these data. Because we'll likely be reframing these data a number of times, it 
## would be good to have a simpler script that can be used to jump through the
## initial hurdles, so I can focus less on importing/cleaning/reshaping and more
## on which stats to use, and how we want to portray data. There are two types
## of data that might be useful: 1) the actual datasets, and 2) the stats summaries.
## There are also two types of datasets to consider: Burn severity datasets, which
## all have a common baseline value, and watershed characteristics datasets, which
## each have unique baseline values. The goal of this script, therefore, is to 
## create simple, labeled datasets that include all info that we might want in
## an easy-to-manipulate format (and stored locally as a .csv). I was initially 
## doing this via RMarkdown, and could easily transfer, but I find straight code
## more readable and easier to troubleshoot, so for importing and prepping, we'll
## just use .R for now.
##
## 2023-07-17
## Peter Regier
##
# ########## #
# ########## #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse,
       PNWColors,
       plotly,
       cowplot,
       janitor, 
       googledrive)

## Set data paths
gdrive_path <- "https://drive.google.com/drive/folders/1tl1Kf0PxVHbS7wPL4W071pvS1Af3Ji1i"
local_path <- "pr_work/data/new_model/"

## Authorize GDrive
options(gargle_oauth_email = "peter.regier@pnnl.gov")


# 2. Download files to local ---------------------------------------------------

## First, list all files
files <- drive_ls(gdrive_path, pattern = ".csv$")

## Create lists for results (raw data) and summaries (stats)
results_files <- files %>% 
  filter(grepl("_results.", name))

summary_files <- files %>% 
  filter(grepl("stats_summaries", name))

## Set paths to read in files
results_path = paste0(local_path, "results/")
summary_path <- paste0(local_path, "summaries/")


## Create download function - doesn't work for some reason
download_data <- function(files, output_path){
  for(i in 1:nrow(files)){
    message("Downloading ", files$name[[i]])
    drive_download(files$id[[i]], overwrite = T, path = paste0(output_path, files$name[[i]]))
  }
}

## Download files
download_data(results_files, results_path)
download_data(summary_files, summary_path)


# 3. Read in results files -----------------------------------------------------

## Make a function to read in data and clean uop filenames
read_results <- function(file){
  read_csv(paste0(results_path, file)) %>% 
    mutate(file = str_remove(file, "_fire_results.csv")) %>% 
    mutate(file = str_remove(file, "_results.csv")) %>% 
    mutate(file = str_remove(file, "PER_")) %>% 
    mutate(file = str_remove(file, "HRU_SLP_")) %>% 
    clean_names() %>% 
    select(dates, file, contains("_fire"), contains("_nofire")) %>% 
    select(!contains("_kgday"))
}

## Read the data in 
results_raw <- results_files$name %>% 
  map(read_results) %>% 
  bind_rows()

## Clean up and categorize the files -------------------------------------------

file_values_to_scrub = c("base_burned", "LOW", "MOD", "HIGH")

percent_change = function(var, baseline){
  (({{var}} - {{baseline}}) / {{baseline}}) * 100
}

results_data <- results_raw %>% 
  filter(dates > "2019-12-31") %>% 
  filter(!(file %in% file_values_to_scrub)) %>% 
  mutate(file = case_when(file == "1" ~ "pos_1", 
                          file == "0.1" ~ "pos_0.1",
                          file == "0.5" ~ "pos_0.5",
                          TRUE ~ file)) %>% 
  mutate(category = case_when(grepl("LOW", file) | grepl("MOD", file) | grepl("HIGH", file) ~ "severity", 
                              file == "WET" | file == "DRY" ~ "precipitation", 
                              file == "GWQ" | file == "LATQ" | file == "SURQ" ~ "flowpath",
                              file == "deciduous" | file == "shrub" | file == "coniferous" | file == "grass" ~ "vegetation",
                              file == "base_burned" ~ "base",
                              TRUE ~ "slope")) %>% 
  mutate(flow_perc = percent_change(flow_m3s_fire, flow_m3s_nofire), 
         sed_perc = percent_change(sed_mg_l_fire, sed_mg_l_nofire), 
         nitrate_perc = percent_change(nit_mg_l_fire, nit_mg_l_nofire), 
         doc_perc = percent_change(doc_mg_l_fire, doc_mg_l_nofire))

median_ = function(...){median(..., na.rm = T)}
mean_ = function(...){mean(..., na.rm = T)}

results_stats <- results_data %>% 
  group_by(file) %>% 
  summarize(category = first(category), 
            flow_median = median_(flow_perc), 
            sed_median = median_(sed_perc), 
            nitrate_median = median_(nitrate_perc), 
            doc_median = median_(doc_perc))

severity_stats <- results_stats %>% 
  filter(category == "severity") %>%
  separate(file, into = c("percent", "severity")) %>% 
  mutate(percent = as.factor(as.numeric(percent))) %>% #simple way to reorder as a factor
  mutate(severity = fct_relevel(severity, c("LOW", "MOD", "HIGH")))

ggplot(severity_stats, aes(percent, flow_median, fill = severity)) + 
  geom_col()


  facet_wrap(~category, nrow = 1, scales = "free") + 
  scale_x_continuous(trans = pseudolog10_trans)









