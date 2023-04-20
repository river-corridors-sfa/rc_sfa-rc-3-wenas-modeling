## Map a map of Evans Creek burn severity with Wenas Creek watershed highlighted
##
## pjr, 2023-04-20
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
       ggsflabel, 
       ggnewscale, 
       nhdplusTools, # get watershed boundary/flowlines
       elevatr, 
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

## Pull Wenas sites from local
wenas_sites <- read_csv("/Users/regi350/Library/CloudStorage/OneDrive-PNNL/Documents/projects/RC/rc3/data/RC3_All_Site_Details_and_Permit_Status.csv") %>% 
  clean_names() %>% 
  mutate(id = str_sub(site_name_rc3_study_code_site_code, 6, n())) %>% 
  filter(river_name == "Wenas Creek") %>% 
  dplyr::select(lat, long, id)  

## Convert to sf
## NOTE: something is off with W20, only use W10 for now
wenas_sites_sf <- st_as_sf(wenas_sites %>% filter(id == "W10"), 
                           coords = c("long", "lat"), crs =  common_crs)

## Pull Wenas boundary
wenas_huc12 <- get_huc(AOI = wenas_sites_sf, 
                      type = "huc10")

## Pull flowlines
wenas_flowlines <- get_nhdplus(AOI = wenas_huc12)


## Get DEM
elevation_raw <- get_elev_raster(wenas_huc12, z = 10)

## Crop DEM (don't need to reproject because default is WGS84)
elevation_crop <- mask(elevation_raw, wenas_huc12)

## Convert to dataframe
elevation <- as.data.frame(elevation_crop, xy = T) %>% 
  as_tibble() %>% 
  rename("long" = x, 
         "lat" = y, 
         "elevation" = 3) %>% 
  filter(!is.na(elevation))

# 3. Make map ------------------------------------------------------------------

## Test map
ggplot() + 
  geom_raster(data = elevation, aes(long, lat, alpha = elevation)) +
  geom_sf(data = wenas_huc12, fill = NA, color = "black") 

## Set up severity color scheme
fire_colors <- c("#54A266", "#F5F54F", "#F8B75B", "#FA8B63", "#FC543D", "#FF0F0F")

## Make map
ggplot() + 
  geom_sf(data = wenas_huc12, fill = NA, color = "black") +
  geom_raster(data = elevation, aes(long, lat, fill = elevation), show.legend = F, alpha = 0.8) +
  scale_fill_gradientn(colors = c("#EFF7EA", "#ADD7A1", "#5FA965", "#245836", "#911C43")) +
  new_scale_fill() + 
  geom_sf(data = wenas_flowlines, color = "blue", alpha = 0.3) +
  geom_raster(data = severity, 
              aes(long, lat, fill = f_severity), alpha = 0.95) + 
  geom_sf(data = boundary, fill = NA, color = "red", lwd = 0.5) + 
  scale_fill_manual(values = fire_colors) + 
  #geom_sf(data = wenas_sites_sf, size = 2) + 
  #geom_sf_text_repel(data = wenas_sites_sf, aes(label = id)) +
  #scale_alpha(range = c(0.1, 0.5)) +
  theme_map() + 
  labs(x = "", y = "", fill = "Severity \n (dNBR)")

## Save figure
ggsave("pr_work/figures/230420_evans_canyon_fire.png",  width = 7, height = 6)


