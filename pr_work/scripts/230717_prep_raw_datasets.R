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


# 3. Read in 




