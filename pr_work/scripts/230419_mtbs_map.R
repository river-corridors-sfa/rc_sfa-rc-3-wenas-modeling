## Visualize fire and burn severity based on MTBS dataset
##

## Load packages
require(pacman)
p_load(tidyverse, 
       raster,
       ggthemes,
       sf)

## Set data path
local_path <- "pr_work/data/mtbs/wa4685412079920200831/"

## Set common coordinate reference system
common_crs = 4326

## Read shapefile in to sf object "boundary"
boundary <- read_sf(paste0(local_path, "wa4685412079920200831_20200830_20200909_burn_bndy.shp")) %>% 
  st_transform(common_crs)
  


## Read in burn severity
severity_raw <-  raster::raster(paste0(local_path, "wa4685412079920200831_20200830_20200909_dnbr6.tif"))

## Reproject to match boundary
severity_reproject <- projectRaster(severity_raw ,crs = common_crs)

severity_crop <- mask(severity_reproject, boundary)

## Convert to dataframe
severity <- as.data.frame(severity_crop, xy = T) %>% 
  as_tibble() %>% 
  rename("long" = x, 
         "lat" = y, 
         "severity" = Layer_1) %>% 
  filter(!is.na(severity))


ggplot() + 
  geom_raster(data = severity, 
              aes(long, lat, fill = severity), alpha = 0.7) + 
  geom_sf(data = boundary, fill = NA, color = "black") + 
  scale_fill_viridis_c() + 
  theme_bw() 
ggsave("pr_work/figures/230419_evans_canyon_fire.png", 
       width = 7, height = 6)


