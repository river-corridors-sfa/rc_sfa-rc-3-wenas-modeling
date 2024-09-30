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
       ggspatial, # add north arrow and scale bar
       nhdplusTools, # get watershed boundary/flowlines
       elevatr, # pull elevation maps
       sf) # tidy spatial

## Set data path
local_path <- "pr_work/data/mtbs/wa4685412079920200831/"

## Set common coordinate reference system
common_crs = 4326





# 2. Read in fire burn boundary ------------------------------------------------

## Read in burn boundary (#1)
boundary <- read_sf(paste0(local_path, "wa4685412079920200831_20200830_20200909_burn_bndy.shp")) %>% 
  st_transform(common_crs)


# 3. Plot the burn boundary ----------------------------------------------------

ggplot() + 
  geom_sf(data = boundary)


# 4. Create an sf object (W10) -------------------------------------------------

## For simplicity, creating instead of reading
w10 <- tibble(lat = 46.86752, long = -120.7744, site_id = "W10") %>% 
  st_as_sf(coords = c("long", "lat"), crs = common_crs)

ggplot() + 
  geom_sf(data = boundary) + 
  geom_sf(data = w10)


# 5. Pull NHD data -------------------------------------------------------------

## Pull Wenas boundary
wenas_huc10 <- get_huc(AOI = w10, 
                       type = "huc10")

## Pull flowlines
wenas_flowlines <- get_nhdplus(AOI = wenas_huc10)

## add to plot
ggplot() + 
  geom_sf(data = wenas_huc10, fill = "lightgreen", alpha = 0.5) +
  geom_sf(data = wenas_flowlines, fill = "blue") + 
  geom_sf(data = boundary, fill = "red", 
          color = "orange", alpha = 0.8) + 
  geom_sf(data = w10, size = 2)


# 6. Read in raster data -------------------------------------------------------

## Read in burn severity
severity_raw <-  raster::raster(paste0(local_path, "wa4685412079920200831_20200830_20200909_dnbr6.tif"))

## Re-project severity to match boundary
severity_reproject <- projectRaster(severity_raw ,crs = common_crs)

## Mask (crop in essence) severity to fire boundary
severity_crop <- mask(severity_reproject, boundary)

## Convert to dataframe
severity <- as.data.frame(severity_crop, xy = T) %>% 
  as_tibble() %>% 
  rename("long" = x, 
         "lat" = y, 
         "severity" = Layer_1) %>% 
  filter(!is.na(severity)) %>% 
  mutate(f_severity = as.factor(round(severity, 0)))


# 7. Plot raster ---------------------------------------------------------------

## Set up severity color scheme
fire_colors <- c("#54A266", "#F5F54F", "#F8B75B", "#FA8B63", "#FC543D", "#FF0F0F")

ggplot() + 
  geom_sf(data = wenas_huc10, fill = "lightgreen", color = "black") +
  geom_sf(data = wenas_flowlines, color = "blue", alpha = 0.3) +
  geom_raster(data = severity, 
              aes(long, lat, fill = f_severity), alpha = 0.95) + 
  geom_sf(data = boundary, fill = NA, color = "red", lwd = 0.5) + 
  scale_fill_manual(values = fire_colors) + 
  theme_map() + 
  labs(x = "", y = "", fill = "Severity \n (dNBR)")


# 9. Pull elevation data for weanus --------------------------------------------

## Get DEM
elevation_raw <- get_elev_raster(wenas_huc10, z = 10)

## Crop DEM (don't need to reproject because default is WGS84)
elevation_crop <- mask(elevation_raw, wenas_huc12)

## Convert to dataframe
elevation <- as.data.frame(elevation_crop, xy = T) %>% 
  as_tibble() %>% 
  rename("long" = x, 
         "lat" = y, 
         "elevation" = 3) %>% #column index > name (changing resolution changes colname)
  filter(!is.na(elevation))


# 10. Make final map ------------------------------------------------------------------

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
  theme_map() + 
  labs(x = "", y = "", fill = "Severity \n (dNBR)") + 
  ggspatial::annotation_scale(
    location = "bl",
    pad_x = unit(0.8, "in"),
    bar_cols = c("black", "white")) +
  ggspatial::annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(1.1, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("black", "white"),
      line_col = "grey20"))

## Save figure
ggsave("pr_work/figures/230420_evans_canyon_fire.png",  width = 7, height = 6)
