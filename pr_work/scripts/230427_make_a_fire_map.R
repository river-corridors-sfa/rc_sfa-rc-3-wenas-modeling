## Example script for working with spatial features in R
##
## pjr, 2023-04-27
##
# ############## #
# ############## #


# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, #keep it tidy
       raster, # work with rasters, NOTE: masks dplyr::select
       janitor, # clean_names()
       ggthemes, # theme_map()
       ggsflabel, # add labels to sf objects
       ggnewscale, # set multiple color scales
       nhdplusTools, # get watershed boundary/flowlines
       elevatr, # pull elevation maps
       sf) # tidy spatial

## Set data path
local_path <- "pr_work/data/mtbs/wa4685412079920200831/"

## Set common coordinate reference system
common_crs = 4326


# 2. Read in datasets ----------------------------------------------------------

## Data needed: 1) burn boundary, 2) burn severity, 3) Wenas Creek boundary, and 
## 4) Wenas Creek flowlines

## Read in burn boundary (#1)
boundary <- read_sf(paste0(local_path, "wa4685412079920200831_20200830_20200909_burn_bndy.shp")) %>% 
  st_transform(common_crs)

## Read in burn severity
severity_raw <-  raster::raster(paste0(local_path, "wa4685412079920200831_20200830_20200909_dnbr6.tif"))

## Re-project severity to match boundary
severity_reproject <- projectRaster(severity_raw ,crs = common_crs)

## Mask (crop in essence) severity to fire boundary
severity_crop <- mask(severity_reproject, boundary)

## Convert to dataframe (#2)
severity <- as.data.frame(severity_crop, xy = T) %>% 
  as_tibble() %>% 
  rename("long" = x, 
         "lat" = y, 
         "severity" = Layer_1) %>% 
  filter(!is.na(severity)) %>% 
  mutate(f_severity = as.factor(round(severity, 0)))