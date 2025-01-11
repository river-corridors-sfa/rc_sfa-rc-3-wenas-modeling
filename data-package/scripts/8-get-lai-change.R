## Download LAI data from MODIS and use it to determine the average maximum LAI change following fire fore each burn severity
  #written by Katie A. Wampler on 11/8/2024 

#set defaults 
  model <- "tule"
  
  if(model == "american"){
    basin_file <- "C:/SWAT/American River Simp2/American River Simp2/Watershed/shapes/subs1.shp" #subbasin file
    hdf_path <- "~/1_Research/4_Wenas_Thresholds/data/American River/MODIS/MCD15A3H_061-20250102_032116" #location of LAI data
    landuse <- raster("~/1_Research/4_Wenas_Thresholds/data/American River/projected_spatial/landcover.tif") #landuse data
    blai <- data.frame(landuse=c("FRSE", "RNGE", "RNGB", "WETF", "FRSD"), blai=c(3.97324,2.5,2.120652, 5,5)) #land use with the blai values (calibrated)
    plant <- "C:/SWAT/American River Simp2/American River Simp2/scenarios/american river simp2.sufi2.swatcup/plant.dat" #location of plant.dat in calibrated model
  }else if(model == "tule"){
    basin_file <- "C:/SWAT/Tule River Simp2/Tule River Simp2/Watershed/shapes/subs1.shp"
    hdf_path <- "~/1_Research/4_Wenas_Thresholds/data/Tule_River/MODIS/MCD15A3H_061-20241106_235852"
    landuse <- raster("~/1_Research/4_Wenas_Thresholds/data/Tule_River/projected_spatial/landcover.tif")
    blai <- data.frame(landuse=c("FRSE", "RNGE", "RNGB", "WETF","FRSD"), blai=c(4.592654, 1.947813, 2.476754, 5,4.308355))
    plant <- "C:/SWAT/tule River Simp2/tule River Simp2/scenarios/tule river simp2.sufi2.swatcup/plant.dat"
    
  }

#section 0: load functions and libraries ------ 
  library(raster)
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(exactextractr)
  library(stringr)
  library(readr)
  library(lubridate)
  library(tidyr)
  library(doParallel)
  library(foreach) 

#' Clean MTBS wildfire files
#'
#' MTBS downloads come in multiple zipped folders and the file names are long and meaningless. This
#' will pull all files in a single folder and rename files to be more user friendly.
  #'
  #'@importFrom sf st_read
  #'@importFrom stringr str_detect str_replace_all str_split regex
  
  #' @param wd the location with the raw MTBS files
  #' @param save_loc the location to save all the files
  #' @export
  #' @examples
  #' \dontrun{
  #' wd <- "Z:/3_GIS-GPS/Wildfire/MTBS/McKenzie/"
  #' save_loc <- "Z:/3_GIS-GPS/Wildfire/MTBS/McKenzie"
  #' clean_mtbs(wd, save_loc)}
  
  clean_mtbs <- function(wd, save_loc){
    stopifnot(file.exists(wd), file.exists(save_loc))
    
    #get zipped files of fires
    files <- list.files(wd, recursive = T)
    files <- files[stringr::str_detect(files, ".zip")]
    
    lapply(1:length(files), function(f){
      
      x <- files[f]
      
      #get place to put zip files
      exdir <- file.path(wd, gsub(".zip", "", x))
      
      #upzip file
      unzip(paste(wd, x, sep="/"), exdir=exdir)
      
      #find shape file to extract fire name
      zip_files <- list.files(exdir)
      shp_file <- zip_files[stringr::str_detect(zip_files, "burn_bndy.shp")]
      
      #read file to get fire name
      name <- sf::st_read(paste(exdir, shp_file, sep="/"))
      filename <- name$Incid_Name
      
      #format name for file
      filename <- tolower(filename)
      filename <- stringr::str_replace_all(filename, " ", "_")
      filename <- gsub("[][()]", "", filename)
      
      #rename files
      #get dates of data
      dates <- unlist(stringr::str_split(shp_file, "_"))[2:3]
      
      #get header, remove files from other fires
      header <- stringr::str_split_i(shp_file, "_", i=1)
      zip_files <- zip_files[stringr::str_detect(zip_files, stringr::regex(header, ignore_case=T))]
      
      #if both, remove completely
      new_files <- gsub(paste("_", paste(dates, collapse ="_"), sep=""), "", zip_files)
      
      #if just one use pre or post
      new_files <- gsub(dates[1], "pre", new_files)
      new_files <- gsub(dates[2], "post", new_files)
      
      #replace long name with fire name
      new_files <- gsub(header, filename, new_files)
      
      #replace metadata name
      new_files <- gsub(toupper(header), filename, new_files)
      
      #rename files
      file.rename(paste(exdir, zip_files, sep="/"), paste(save_loc, new_files, sep="/"))
    })
  }
  
  #' Clip raster to shapefile
  #'
  #' Function will ensure the raster and shapefile are in the same coordinate reference
  #' system, then will clip to the shapefile boundry.
  #'
  #' @importFrom raster rasterToPoints
  #' @importFrom sf st_buffer
  #' @param raster the raster you want to clip
  #' @param sf the shapefile you want to use to clip the raster
  #' @param type either 'numeric' or 'categorical' depending on if your raster is discrete or continuous values
  #' @param res the resolution of the projected raster, if not specified with default to 30m
  #' @param return either 'df' or 'raster' to specify the form of the returned raster
  #'
  #' @return if 'return' is df it will return the raster as a dataframe suitable for
  #' plotting in ggplot2. If 'return' is raster it will return the raster as a
  #' rasterLayer object
  #' @export
  #'
  clean_raster <- function(raster, sf, type="numeric",
                           res=NULL, return="df"){
    stopifnot(class(raster) == "RasterLayer", class(sf)[1] == c("sf"),
              type %in% c("numeric", "categorical"),
              return %in% c("df", "raster"))
    #get units of raster to set buffer
    unit <- sf::st_crs(sf, parameters = TRUE)$units_gdal
    buffer <- ifelse(unit == "degree", 0.1,5000)
    res <- ifelse(unit == "degree", 0.0003280119,30)
    
    method <- ifelse(type == "numeric", "bilinear", "ngb") #set summary type
    
    #ensure it's in the right projection, if yes just clips to basin and formats as df
    if(compareCRS(raster, sf) == T){
      raster_crop <- raster::crop(raster, sf)
      raster_crop <- raster::mask(raster_crop, sf)
      raster_df <- as.data.frame(raster::rasterToPoints(raster_crop))
      colnames(raster_df) <- c("x", "y", "val")
    }else{
      #if not it will clip to smaller area before projecting to save time
      sf_prj <- convert_crs(sf::st_buffer(sf, dist=buffer), raster)
      raster_crop <- raster::crop(raster, sf_prj)
      raster_crop <- raster::mask(raster_crop, sf_prj)
      
      #reproject raster
      raster_prj <- raster::projectRaster(raster_crop, crs=crs(sf), method=method, res=res)
      
      #clip to actual basin area
      raster_crop <- raster::crop(raster_prj, sf)
      raster_crop <- raster::mask(raster_crop, sf)
      #format as df
      raster_df <- as.data.frame(raster::rasterToPoints(raster_crop))
      colnames(raster_df) <- c("x", "y", "val")
    }
    if(return == "df"){
      raster_df
    } else{raster_crop}
  } 
  
  #' Convert the projection geospatial data
  #'
  #' Converts a raster or shapefile into the same coordinate reference system as
  #' another raster or shapefile.Helpful when trying to use multiple datasets.
  #'
  #' @importFrom terra res
  #' @importFrom sf st_crs st_transform
  #' @importFrom raster projectRaster crs
  #' @param input the shapefile or raster you want to convert to a new coordinate reference system
  #' @param goal the shapefile or raster with the coordinate reference system desired
  #' @param type either 'numeric' or 'categorical' depending on if your raster is discrete or continuous values
  #' @param res the resolution of the projected raster, if not specified with default to 30m
  #' @return the input object with the coordinate reference system of the goal object
  #' @export
  #'
  #' @examples
  #' \dontrun{
  #' convert_crs(basin, DEM)
  #' }
  convert_crs <-  function(input, goal, type="numeric", res=NULL){
    stopifnot(class(input)[1] %in% c("sf", "RasterLayer"),
              class(goal)[1] %in% c("sf", "RasterLayer"),
              type %in% c("numeric", "categorical"))
    
    #check first to see if a conversion is needed
    if(raster::compareCRS(goal, input) == T){
      message("CRS already matches that of your goal object")
      return(input)
    }else{
      if(class(input)[1] == "RasterLayer"){ #rasters need to be transformed differently
        #figure out the resolution of the projected raster
        if(is.null(res)){
          if(class(goal)[1] == "RasterLayer"){
            res <- terra::res(goal)[1]
          }else{
            #if a shapefile get the units of that to know how to set 30m
            unit <- sf::st_crs(goal, parameters = TRUE)$units_gdal
            res <- ifelse(unit == "degree", 0.0003280119,30)
          }
        }
        method <- ifelse(type == "numeric", "bilinear", "ngb") #set summary type
        #project raster
        input_prj <- raster::projectRaster(input, crs=raster::crs(goal), method=method, res=res)
      }else{
        #project shapefile
        input_prj <- sf::st_transform(input, raster::crs(goal))
      }
      return(input_prj)
    }} 
#section 1: load LAI data and tidy ----- 
  #get wildfire data 
    #tidy mtbs file names (download data from MTBS website)
    wd <- "~/1_Research/4_Wenas_Thresholds/data/mtbs"  
    #clean_mtbs(wd, wd) only run once
    
    #load wildfires
    american <- raster(file.path(wd, "american_dnbr6.tif"))
    american_shp <- read_sf(file.path(wd, "american_burn_bndy.shp"))
    american <- clean_raster(american, american_shp, return="raster")
    
    norse <- raster(file.path(wd, "norse_peak_dnbr6.tif"))
    norse_shp <- read_sf(file.path(wd, "norse_peak_burn_bndy.shp"))
    norse <- clean_raster(norse, norse_shp, return="raster")
    
    pier <- raster(file.path(wd, "pier_dnbr6.tif"))
    pier_shp <- read_sf(file.path(wd, "pier_burn_bndy.shp"))
    pier <- clean_raster(pier, pier_shp, return="raster")
    
    schaeffer <- raster(file.path(wd, "schaeffer_dnbr6.tif"))
    schaeffer_shp <- read_sf(file.path(wd, "schaeffer_burn_bndy.shp"))
    schaeffer <- clean_raster(schaeffer, schaeffer_shp, return="raster")
    
    WA_fires <-mosaic(american, norse, fun=max) #merge into a single file 
    CA_fires <- mosaic( pier, schaeffer, fun=max) #merge into a single file 
    
    WA_fires_shp <- rbind(american_shp, norse_shp)
    CA_fires_shp <- rbind(pier_shp, schaeffer_shp)
  
    if(model == "american"){
      fires <- WA_fires
    }else if(model == "tule"){
      fires <- CA_fires
    }
    
  #download MODIS LAI data here (MCD15A3H_061): https://search.earthdata.nasa.gov/search?q=C2222147000-LPCLOUD 
  #get basin data 
  basin <-  st_read(basin_file)
  basin <- st_union(basin) %>% st_sf()
  
  if(model == "tule"){
    basin <- CA_fires_shp
  }
  
  basin_pj <- st_transform(basin, "EPSG:4326")
  
#set path to downloaded data
  hdf_files <- list.files(hdf_path, pattern=".hdf$")
  
  setwd(hdf_path )
  process_LAI_file <- function(infile, basin_pj){
    str1<-str_sub(infile, 13, 17)
    modis <- stack(infile)
    
    # Extract the data
    LAI_raster <- modis[["Lai_500m"]] 
    
    #clip to basin first since projection takes time
    basin_clip <- st_transform(basin_pj, crs=crs(LAI_raster))
    LAI <- thorloki::clean_raster(LAI_raster, basin_clip, return = "raster")
    
    #projection
    LAI_p <- projectRaster(LAI, crs = "EPSG:4326") 
    
    #get data.frame
    LAI <- as.data.frame(raster::rasterToPoints(LAI_p))
    
    #add date 
    date_str <- str_split(infile, "[.]", simplify = TRUE)[2]
    date <- as.Date(gsub("A", "", date_str), format = "%Y%j")
    LAI$date <- date
    
    #return data
    return(list(LAI = LAI))
  }
  
  #run in parallel otherwise takes several hours
  nCores <- parallelly::availableCores() - 1
  cl <- parallel::makeCluster(nCores)
  doSNOW::registerDoSNOW(cl)
  pb <- txtProgressBar(max = length(hdf_files), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  results <- foreach (i=1:length(hdf_files), .options.snow = opts, 
                      .packages=c("sf", "raster", "stringr", "thorloki")) %dopar%{
                        output <-  process_LAI_file(hdf_files[i], basin_pj)
                        return(output) 
                      }
  
  lai_data <- do.call(rbind, lapply(results, `[[`, "LAI"))
  lai_data$Lai_500m <- lai_data$Lai_500m * 0.1 #apply scaling factor
  
  #save data so you don't have to rerun
  data.table::fwrite(lai_data, "clean_lai.csv") 
  
#section 2: get average maximum LAI change per site ------- 
  setwd(hdf_path)
  #get basin data 
  basin <-  st_read(basin_file)
  basin <- st_union(basin) %>% st_sf()
  if(model == "tule"){
    basin <- CA_fires_shp
  }
  basin_pj <- st_transform(basin, "EPSG:4326")
  
  lai_data <- data.table::fread(file.path(hdf_path, "clean_lai.csv"))
  lai_data$date <- as.Date(lai_data$date)
  lai_sites <- unique(lai_data[,1:2]) %>% mutate(site=paste0("site_",1:nrow(unique(lai_data[,1:2]))))
  lai_data <- merge(lai_sites, lai_data, by=c("x", "y"))
  lai_sites <- as.data.frame(lai_sites)
  lai_sites$x <- trimws(lai_sites$x) #remove extra white spaces that apparrently exist to successfully merge
  lai_sites$y <- trimws(lai_sites$y)
  #get raster for sites
  hdf_files <- list.files(hdf_path, pattern=".hdf$")
  LAI <- process_LAI_file(hdf_files[1], basin_pj)[[1]] %>% dplyr::select(-date)
  LAI_raster <- rasterFromXYZ(LAI, crs="EPSG:4326")
  
  #we want data just in the growing seasons which was defined here: as max * 0.3 10.22541/au.171053013.30286044/v2 
  grow_lai <- lai_data %>% mutate(year = year(date)) %>% filter(!(year %in% c(2012, 2017)) & Lai_500m < 6) %>% 
    group_by(site, year) %>% mutate(grow_tresh = max(Lai_500m * 0.3))
  grow_lai$grow <- ifelse(grow_lai$Lai_500m > grow_lai$grow_tresh, T, F)
  grow_lai <- subset(grow_lai, grow_lai$grow == T)
  
  #check data      
  ggplot(lai_data %>% filter(site %in% sample(lai_sites$site, 5)), aes(x=date, y=Lai_500m)) + 
    geom_line() + facet_wrap(~site)
  
  #get maximum LAI for each year for each site (except 2017 since that was during the fire)
  annual_lai <- grow_lai %>%  group_by(year, site) %>% mutate(max_value = max(Lai_500m),
                                                              max_date = date[which.max(Lai_500m)])
  annual_lai <- annual_lai %>% filter(date >= (max_date - 8) & date <= (max_date + 8)) %>%
    group_by(site, year) %>% summarise(adj_max = mean(Lai_500m),
                                       sd = sd(Lai_500m),
                                       count= n()) %>% filter(count >= 2)
  #get average max change
  annual_lai$timing <-"pre"
  annual_lai$timing[annual_lai$year == 2018] <- "post"  
  
  fire_lai <- annual_lai %>% group_by(site, timing) %>% summarise(avg_lai = mean(adj_max),
                                                                  count=n()) %>% 
    filter((timing == "pre"  & count > 2) | timing == "post") %>% dplyr::select(-count) %>% 
    pivot_wider(names_from=timing, values_from=avg_lai) %>% mutate(change = post/pre) %>% na.omit()
  
#get fire and landuse data ------- 
  
    fires <- clean_raster(fires, basin_pj, type="categorical", return="raster")

  #get basin landuse 
    landuse <- clean_raster(landuse, basin_pj, type="categorical", return="raster") 
    if(model == "tule"){
      landcover <- raster(FedData::get_nlcd(template=basin_pj, label="basin", year=2016))
      landuse <- clean_raster(landcover, basin_pj, type="categorical", return="raster") 
      
    }
#section 4: link max LAI to burn severity (classified) ------- 
    #resample the burn severity raster to match LAI data 
    fire_rs <- resample(fires, LAI_raster, method="ngb")
    
    #resample vegetation raster to match LAI data
    landuse_rs <- resample(landuse, LAI_raster, method="ngb")
    
    #link burn severity to LAI change data 
    fire_df <- clean_raster(fire_rs, basin_pj, type="categorical")
    fire_df <-merge(lai_sites, fire_df, by=c("x", "y"))

    #link vegetation type to LAI change data
    land_df <- clean_raster(landuse_rs, basin_pj, type="categorical")
    land_df <- merge(lai_sites, land_df, by=c("x", "y"))
    
#section 5: get average LAI change for each burn severity group -------
    #merge together 
    fire_lai <- merge(fire_lai, fire_df[,3:4], by="site")
    fire_lai <- merge(fire_lai, land_df[,3:4], by="site")
    fire_lai <- fire_lai %>% rename(sev = val.x, veg = val.y)
    sev_lai <- fire_lai %>% group_by(sev) %>% summarise(avg_change = mean(change), count=n()) %>% mutate(r_change = (1-avg_change)*-1)    
    
    sev_veg_lai <-  fire_lai %>% group_by(sev,veg) %>% summarise(avg_change = mean(change), 
                                                                  count=n()) 
    
#section 6: get changes for each landuse ------- 
    blai$ls <- blai$blai * sev_lai$avg_change[sev_lai$sev == 2]
    blai$ms <- blai$blai * sev_lai$avg_change[sev_lai$sev == 3]
    blai$hs <- blai$blai * sev_lai$avg_change[sev_lai$sev == 4]

    blai <- blai%>% dplyr::select(-blai) %>% pivot_longer(ls:hs, names_to = "severity", values_to = "change")
#section 7: create new plant.dat ------ 
  df <- readLines(plant)[1:640]
  names <- paste(rep(c("LS","MS","HS"), 5), rep(c("FE","RG", "RB","WT","FD"), each=3), sep="")
  titles <- data.frame(number = 129:143, names=names, type=c(rep(7,3), rep(6,6), rep(7,6)), value=blai$change)
  titles$header <- paste(" ", titles$number,"  ", titles$names, "   ", titles$type, sep="")
  count <- 1
  for(x in 1:5){
    line <- grep(blai$landuse[x], df)
    lines <- seq(from=line, to=line+4)
    for(y in 1:3){
      new <- df[lines] 
      #replace header
      new[1] <- titles$header[count]
      
      #replace value
      values <- str_split2(new[2],"   ")
      values[4] <- round(titles$value[count],5)
      new[2] <- paste(values, collapse = "  ")
      df <- c(df, new)
      count <- count + 1
    }
  }

write_lines(df, plant)     
