## This code is used to download MODIS PET data, clean it, and use it to write a PET.txt 
  #file to use read-in PET data for the SWAT model 
  #written by Katie A. Wampler

#set defaults 
  ETwd <- "~/1_Research/4_Wenas_Thresholds/data/American River/"
    #create space to put ET data
    dir.create(paste(ETwd, "MODIS",sep=""), showWarnings = F)

  #location of model shapefile 
  basin_shp <- "C:/SWAT/American River Simp2/American River Simp2/Watershed/shapes/subs1.shp"
  
  #place to write pet.txt file (suggest the txtinout folder for the model)
  save_loc <- "C:/SWAT/American River Simp2/American River Simp2/Scenarios/Default/TxtInOut/"
  
 #STEP 1: go here: https://search.earthdata.nasa.gov/search?q=C2222147000-LPCLOUD
    #select the region you want, and download the data layers: MOD16A2GF for 
    #the dates you want the model to run if you're using read in PET, 
    #I would recommend starting in 2000 or whenever you start calibrating. 
    
#section 0: load libraries and functions ------- 
  library(zoo)
  library(stringr)
  library(lubridate)
  library(data.table)
  library(raster)
  library(foreach)
  library(parallely)
  library(doSNOW)
  library(dplyr)
  library(ggplot2)
  library(sf)
  
  #used to clip a raster to a shapefile and return either a raster or df
  clean_raster <- function (raster, sf, type = "numeric", res = NULL, return = "df"){
    stopifnot(class(raster) == "RasterLayer", class(sf)[1] == 
                c("sf"), type %in% c("numeric", "categorical"), return %in% 
                c("df", "raster"))
    unit <- sf::st_crs(sf, parameters = TRUE)$units_gdal
    buffer <- ifelse(unit == "degree", 0.1, 5000)
    res <- ifelse(unit == "degree", 0.0003280119, 30)
    method <- ifelse(type == "numeric", "bilinear", "ngb")
    if (compareCRS(raster, sf) == T) {
      raster_crop <- raster::crop(raster, sf)
      raster_crop <- raster::mask(raster_crop, sf)
      raster_df <- as.data.frame(raster::rasterToPoints(raster_crop))
      colnames(raster_df) <- c("x", "y", "val")
    }
    else {
      sf_prj <- convert_crs(sf::st_buffer(sf, dist = buffer), 
                            raster)
      raster_crop <- raster::crop(raster, sf_prj)
      raster_crop <- raster::mask(raster_crop, sf_prj)
      raster_prj <- raster::projectRaster(raster_crop, crs = crs(sf), 
                                          method = method, res = res)
      raster_crop <- raster::crop(raster_prj, sf)
      raster_crop <- raster::mask(raster_crop, sf)
      raster_df <- as.data.frame(raster::rasterToPoints(raster_crop))
      colnames(raster_df) <- c("x", "y", "val")
    }
    if (return == "df") {
      raster_df
    }
    else {
      raster_crop
    }
  }
  
  #used to interpolate 8 day ET to daily ET for PET record
  fill_ET <- function(site, block = 30, pmiss = 90, model = "trend", smooth = TRUE){
    dataset <- data[data$site == site,]
    pck <- is.na(dataset$val)
    percent <- 0
    max.mis <- 0
    if (sum(pck) > 0) {
      percent <- round((sum(pck)/length(dataset$val)) * 100, 
                       digits = 2)
      my.message <- paste("There are ", sum(pck), "missing values for", 
                          dataset$staid[1], "or", percent, "percent of the data.", 
                          sep = " ")
      message(my.message)
      rles <- rle(is.na(dataset$val))
      max.mis <- max(rles$lengths[rles$values])
      my.message2 <- paste("The maximum block of missing", 
                           dataset$staid[1], "data is", max.mis, "days long.", 
                           sep = " ")
      message(my.message2)
    }
    else {
      noMis <- paste("No missing values for", dataset$staid[1], 
                     sep = " ")
      message(noMis)
    }
    if (percent >= pmiss | max.mis >= block) {
      tooMuch <- paste("Too much missing data for", dataset$staid[1], 
                       "Cannot fill in missing values.", sep = " ")
      message(tooMuch)
    }
    else {
      dataset$fill_val <- na.approx(dataset$val, na.rm=F)
      
      ggplot() + geom_line(data=na.omit(dataset), aes(x=calendar_date, y=value),color="black", linewidth=1) +
        geom_line(data=dataset, aes(x=calendar_date, y=fill_val), color="green", linewidth=0.25) + 
        labs(x="Observation",y= "Observed and estimated times series") +
        scale_x_date(limits=c(as.Date("2001-01-01"), as.Date("2001-12-31")))
      
      fillMess <- paste("Filled in", sum(pck), "values for", 
                        dataset$site[1], sep = " ")
      message(fillMess)
    }
    dataset
}
  
  #used to merge multiple dataframes together
  comb_files <- function(file_list){
    pb <- txtProgressBar(max = length(file_list), style = 3)
    
    for(x in 1:length(file_list)){
      setTxtProgressBar(pb, x)
      df <- data.table::fread(file_list[x])
      if(x == 1){
        full_df <- df
      }else{full_df <- rbind(full_df, df)}}
    close(pb)
    return(full_df)
  }
  
  #used to take raw .hdf files, clip to a basin shape and extract to a data.frame
  process_ET_file <- function(infile, basin){
    #used to clip a raster to a shapefile and return either a raster or df
    clean_raster <- function (raster, sf, type = "numeric", res = NULL, return = "df"){
      stopifnot(class(raster) == "RasterLayer", class(sf)[1] == 
                  c("sf"), type %in% c("numeric", "categorical"), return %in% 
                  c("df", "raster"))
      unit <- sf::st_crs(sf, parameters = TRUE)$units_gdal
      buffer <- ifelse(unit == "degree", 0.1, 5000)
      res <- ifelse(unit == "degree", 0.0003280119, 30)
      method <- ifelse(type == "numeric", "bilinear", "ngb")
      if (compareCRS(raster, sf) == T) {
        raster_crop <- raster::crop(raster, sf)
        raster_crop <- raster::mask(raster_crop, sf)
        raster_df <- as.data.frame(raster::rasterToPoints(raster_crop))
        colnames(raster_df) <- c("x", "y", "val")
      }
      else {
        sf_prj <- convert_crs(sf::st_buffer(sf, dist = buffer), 
                              raster)
        raster_crop <- raster::crop(raster, sf_prj)
        raster_crop <- raster::mask(raster_crop, sf_prj)
        raster_prj <- raster::projectRaster(raster_crop, crs = crs(sf), 
                                            method = method, res = res)
        raster_crop <- raster::crop(raster_prj, sf)
        raster_crop <- raster::mask(raster_crop, sf)
        raster_df <- as.data.frame(raster::rasterToPoints(raster_crop))
        colnames(raster_df) <- c("x", "y", "val")
      }
      if (return == "df") {
        raster_df
      }
      else {
        raster_crop
      }
    }
    
    
    str1<-str_sub(infile, 13, 17)
    modis <- stack(infile)
    
    # Extract the data
    ET_raster <- modis[["ET_500m"]] 
    PET_raster <- modis[["PET_500m"]] 
    
    #clip to basin first since projection takes time
    basin_clip <- st_transform(basin, crs=crs(ET_raster))
    ET <- clean_raster(ET_raster, basin_clip, return = "raster")
    PET <- clean_raster(PET_raster, basin_clip, return = "raster")
    
    #projection
    ET_p <- projectRaster(ET, crs = "EPSG:4326") 
    PET_p <- projectRaster(PET, crs = "EPSG:4326") 
    
    #get data.frame
    ET <- as.data.frame(raster::rasterToPoints(ET_p))
    PET <- as.data.frame(raster::rasterToPoints(PET_p))
    
    #add date 
    date_str <- str_split(infile, "[.]", simplify = TRUE)[2]
    date <- as.Date(gsub("A", "", date_str), format = "%Y%j")
    ET$date <- date
    PET$date <- date
    
    #return data
    return(list(ET = ET, PET = PET))
  }
  
#section 1: tidy MODIS data (PET and ET), write into csv for each site in basin area ------ 
  et <- grep("MOD16A2GF", list.dirs(file.path(ETwd, "MODIS"), full.names = T), value=T)
  et_file <- list.files(et, pattern=".hdf$")

  basin <- st_union(basin) %>% st_sf()
  basin_pj <- st_transform(basin, "EPSG:4326")
  
  #get ET data
  setwd(et)
 
  #run in parallel otherwise takes several hours
  nCores <- parallelly::availableCores() - 1
  cl <- parallel::makeCluster(nCores)
  doSNOW::registerDoSNOW(cl)
  pb <- txtProgressBar(max = length(et_file), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  results <- foreach (i=1:length(et_file), .options.snow = opts, 
                      .packages=c("sf", "raster", "stringr")) %dopar%{
                        output <-  process_ET_file(et_file[i], basin_pj)
                        return(output) 
                      }
  
  ET_data <- do.call(rbind, lapply(results, `[[`, "ET"))
  PET_data <- do.call(rbind, lapply(results, `[[`, "PET"))
  
  #save as individual files 
  sites <- unique(ET_data %>% dplyr::select(x, y))
  sites$name <- paste0("site_", 1:nrow(sites))
  for(x in 1:nrow(sites)){
    sub_et <- ET_data[ET_data$x == sites$x[x] & ET_data$y ==sites$y[x],]
    sub_pet <- PET_data[PET_data$x == sites$x[x] & PET_data$y ==sites$y[x],]
    
    sub_df <- merge(sub_et, sub_pet, by=c("date","x","y"))
    sub_df$site <- sites$name[x]
    
    file_name <- paste0(ETwd, "MODIS/",sites$name[x], "_MOD16A2GF_ET_500m.csv")
    data.table::fwrite(sub_df, file_name)
  }
  
#section 2: create PET.txt file by interpolating and averaging ------ 
  #download cleaned PET files
    files <- list.files(paste(ETwd, "MODIS", sep=""))
    PET_file <- paste(ETwd, "MODIS/", files[grep("site_[0-9]+_MOD16A2GF_ET_500m", files)] , sep="")
    PET_dat <- comb_files(PET_file)

  #fix values (divide by number of days, adjust for end of year)
    PET_dat <- PET_dat %>% mutate(doy = yday(date),leap=leap_year(date), 
                                  conv = 0.1/8)
    PET_dat$conv[PET_dat$doy == 361 & PET_dat$leap==T] <- 0.1/6 #for leap years  
    PET_dat$conv[PET_dat$doy == 361 & PET_dat$leap==F] <- 0.1/5 #for nonleap years
    PET_dat$val <- PET_dat$PET_500m * PET_dat$conv
    PET_dat$date <- as.Date(PET_dat$date)

  #put in missing dates as NA 
    start <- min(PET_dat$date)
    end <- max(PET_dat$date)
    dates <- seq(from=start, to=end, by="day")
    dates <- data.frame(date = rep(dates, each=length(unique(PET_dat$site))),
                        site = rep(paste0("site_", 1:length(unique(PET_dat$site))),length(dates)))
    data <- dates %>% left_join(PET_dat, by=c( "site", "date")) %>% arrange()

  #interpolate sites 
    filled_PET <- lapply(unique(data$site), fill_ET)
    PET_dat <- do.call(rbind, filled_PET)
  
  #get average across basin
    PET_dat <- PET_dat %>% group_by(date) %>% 
      summarise(value=mean(fill_val)) 
  
  #fill in ending days 
    PET_dat$value[is.na(PET_dat$value) ==T] <- PET_dat$value[PET_dat$date == "2022-12-27"]
  
  #write txt file 
    PET_dat$doy <- str_pad(yday(PET_dat$date), 3, "left", "0")
    PET_dat$year <- year(PET_dat$date)
    PET_dat$value_form <- str_pad(format(round(PET_dat$value, 1), nsmall = 1, trim=T),5, "left", "0")
    
    PET_dat$line <- paste(PET_dat$year, PET_dat$doy, PET_dat$value_form, sep="")
    PET <- data.frame(PET=PET_dat$line)
    write.table(PET, paste(save_loc, "PET.txt", sep=""), col.names = T, row.names = F, quote=F)
    
