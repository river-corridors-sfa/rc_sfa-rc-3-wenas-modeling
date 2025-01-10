## script to get the data needed for a swat run based on an HRU or coordinates 
  #written by Katie A. Wampler on 2024-03-21 

#set up information about model you're building
  #select basin for the model
    site <- "12488500" #"11204100"  #USGS site used for outlet
    
    #need to pull lat and long from USGS site to get huc
    huc <- get_huc(AOI=st_as_sf(data.frame(site=site, lat=36.02411648, long=-118.8134258), 
                                coords=c("long","lat"), crs="EPSG:4269"))  %>% st_drop_geometry()
    huc <-str_sub(huc[11], 1, 10) #american: 1703000201 #tule:1803000603
    huc <- "1703000201" #if you already know huc you can put it here

  #set location for saving data
    wd <- "Z:/1_Research/4_Wenas_Thresholds/Data/American River"

  #set dates for calibration (used for downloading climate data) 
    start <- as.Date("1980-01-01")
    end <- as.Date("2022-01-01")
    
  #landuse year (if you have a fire in your basin, choose before the fire)
    lu_year <- 2016  #options are 1992, 2001, 2006, 2011, 2016, 2019, and 2021 

#section 0: load libraries and functions ------- 
  library(nhdplusTools) #hydro data
  library(elevatr) #source for digital elevation models
  library(soilDB) #soil data 
  library(FedData) #get NLCD 
  library(daymetr) #get daymet data 
  library(waterData) #get observed data
  library(dataRetrieval) #get chemical data 
  library(sf) #for working with shapefiles
  library(raster) #for working with  rasters
  library(ggplot2) #for plotting
  library(stringr) #for dealing with strings
  library(terra) #for working with rasters
  library(thorloki) #for spatial functions
  library(foreign) #for writing attribute table files
  library(MODISTools) #for modis ET 
  library(readr) #for reading and writing files
  library(lubridate) #for dealing with dates 
  library(dplyr) #for dealing with data

  #set working directory 
    dir.create(wd, showWarnings = F)
    setwd(wd)

  #final crs 
    crs <- "EPSG:32610"
    crs_geo <- "EPSG:4326" 

  #to add in dec 31 to lead years 
    insertday <- function(df, dec30_r) {
      r <- dec30_r + 1
      #get row before and after to linearly interpolate 
      newval <- df$value[dec30_r:(dec30_r+1)]
      newval <- sum(newval)/2
      newrow <- df[dec30_r,]
      newrow$yday <- 366 
      newrow$date <- as.Date(paste(newrow$year, newrow$yday, sep="-"), format="%Y-%j")
      newrow$value <- newval
      df[seq(r+1,nrow(df)+1),] <- df[seq(r,nrow(df)),]
      df[r,] <- newrow
      df
    } 
    
  #read in and combine et and lia data 
    comb_files <- function(file_list){
      for(x in file_list){
        df <- read.csv(x)
        if(x == file_list[1]){
          full_df <- df
        }else{full_df <- rbind(full_df, df)}}
      return(df)
    }
    
     
#section 1: download data layers -------  
    #section 1.1: get the basin shapefile and streams
      bsn <- get_huc(id=huc, type=paste("huc", str_pad(nchar(huc), 2, pad="0"),sep=""))
      streams <- get_nhdplus(AOI = bsn)
      bsn <- st_transform(bsn, crs)
      streams <- st_transform(streams,crs)
      bsnbuf <- st_buffer(bsn, dist = 200) #create a buffer around shapefile to clip
      bsnbuf_geo <- st_transform(bsnbuf, crs=crs_geo)
      
      ggplot(bsnbuf) + geom_sf() + geom_sf(data=streams) #check the buffering
      
      #get outlet point
      nldi_nwis <- list(featureSource = "nwissite", featureID = paste("USGS-", site, sep=""))
      nldi_feature <- nhdplusTools::get_nldi_feature(nldi_nwis)
      
    #section 1.2: get the DEM
      dem <- get_elev_raster(bsnbuf, src = "gl1")
      dem <- projectRaster(dem, crs=crs)
      
    #section 1.3: get the soil data 
      soil <- mukey.wcs(aoi = bsnbuf, db = 'statsgo')
      soil <- project(soil, crs) 
      
      soil_ssurgo <- mukey.wcs(aoi = bsnbuf, db = 'gSSURGO')
      soil_ssurgo <- project(soil_ssurgo, crs) 
      
    #section 1.4: get the land cover 
      landcover <- get_nlcd(template=bsnbuf, label=huc, year=lu_year)
      landcover <- project(landcover, crs)
      
    #section 1.5: get daymet data (use single pixel extraction tool across each)
      #get grid for daymet data
      grid <- raster(crs = crs(bsnbuf), vals = 0, resolution = c(5000,5000), ext = extent(bsnbuf))
      grid <- clean_raster(grid, bsnbuf, return = "raster") # in projected
      grid_df <- clean_raster(grid, bsnbuf)
      grid_sf <- st_as_sf(x = grid_df, coords = c("x", "y"),crs = crs)
      
      #get lat and long
      grid <- projectRaster(grid, crs=crs_geo)
      grid <- clean_raster(grid, st_transform(bsnbuf, crs=crs_geo))
      dir.create("daymet", showWarnings = F)
      locations <- data.frame(site = paste("site",1:nrow(grid), sep=""),lat = grid$y,lon = grid$x)
      
      #check locations 
      ggplot() + geom_sf(data=bsnbuf) + geom_sf(data=streams) + geom_sf(data=grid_sf, color="red")
      write.csv(locations, paste0("daymet/locations.csv"), row.names = FALSE, quote = FALSE)
      
      #get data (this can take a minute)
      daymet <- download_daymet_batch(file_location= "daymet/locations.csv", 
                                      start=1980, end=2022, internal=T, simplify = T)
  #find observed data locations 
      #section 1.6: get observed data 
      #get streamflow data
      site_f <- whatNWISsites(huc=str_sub(huc, 1, 8),parameterCd = c("00060"),hasDataTypeCd = "dv")
      
      #clip to hru since function only lets you go to HUC 8 
      site_f <- st_as_sf(x = site_f, coords = c("dec_long_va", "dec_lat_va"),crs = crs_geo)
      ggplot(bsnbuf) + geom_sf() + geom_sf(data=streams) + 
        geom_sf(data=site_f)#check the buffering
      
      site_f <- st_intersection(site_f, bsnbuf_geo)
      
      #get wq data 
      site_wq <- whatNWISsites(huc=str_sub(huc, 1, 8),parameterCd = c("00620","00681","80154"))
      site_wq <- st_as_sf(x = site_wq, coords = c("dec_long_va", "dec_lat_va"),crs = crs_geo)
      ggplot(bsnbuf) + geom_sf() + geom_sf(data=streams) + 
        geom_sf(data=site_wq)#check the sites
      
      site_wq <- st_intersection(site_wq, bsnbuf_geo)
      
      #get data 
      wqdata <- readNWISqw(siteNumbers = site_wq$site_no, parameterCd =c("00620","00681","80154"))
      #reformat parameters 
      wqdata$parm_cd[wqdata$parm_cd=="00620"] <- "nitrate_mgL"
      wqdata$parm_cd[wqdata$parm_cd=="00681"] <- "DOC_mgL"
      wqdata$parm_cd[wqdata$parm_cd=="80154"] <- "TSS_mgL"
      

#section 2: save the shape file and rasters ------ 
  dir.create("projected_spatial", showWarnings = F)
  write_sf(bsnbuf, "projected_spatial/basin.shp")
  write_sf(streams, "projected_spatial/streams.shp")
  write_sf(st_transform(site_f, crs=crs), "projected_spatial/gauge.shp")
  write_sf(st_transform(site_wq, crs=crs), "projected_spatial/wq.shp")
  write_sf(st_transform(nldi_feature, crs=crs), "projected_spatial/outlet.shp")
  
  writeRaster(dem, "projected_spatial/dem.tif", overwrite=T)
  writeRaster(landcover, "projected_spatial/landcover.tif", overwrite=T, datatype='INT2U')
  writeRaster(soil, "projected_spatial/soil.tif", overwrite=T)
  writeRaster(soil_ssurgo, "projected_spatial/soil_ssurgo.tif", overwrite=T)
  
  #format subbasin and reach data to manually add [to do project]
    #https://swat.tamu.edu/media/114678/existing-watershedv1.pdf
  
#section 3: get slope bands (optional) ------ 
  slope <- terrain(dem, opt="slope", unit="radians")
  slopeval <- clean_raster(slope, bsnbuf)
  slopeval$val <- tan(slopeval$val)*100
  ggplot(slopeval, aes(x=val)) + geom_histogram()
  
  quantile(slopeval$val, c(0.25,0.5,0.75))
  
#section 4: format the climate data (daymet removes 12/31 for leap years)-----
  #fixed missing dec 31 by adding back in, as the average between the two days on either side
  stats <- unique(daymet[,1:5])
  dir.create("daymet/precip", showWarnings = F)
  dir.create("daymet/temp", showWarnings = F)
  pcp <- data.frame("ID"=1:nrow(stats),"NAME"=stats$site,"LAT"=stats$latitude,
                    "LONG"=stats$longitude,"ELEVATION"=stats$altitude)
  write.csv(pcp, "daymet/precip/pcp.txt", row.names=F, quote = F)
  write.csv(pcp, "daymet/temp/tmp.txt", row.names=F, quote = F)
  
  #write individual files 
    for(x in stats$site){
      sub <- subset(daymet, daymet$site ==x)
      
      #add leap year data 
      sub$date <- as.Date(paste(sub$year, sub$yday, sep="-"), format="%Y-%j")
      sub$leap <- leap_year(sub$date)
        add <- which(sub$yday == 365 & sub$leap == T)
        for(y in add){
          sub <- insertday(sub, y)
        }
      precip <- subset(sub, sub$measurement == "prcp..mm.day.")
      tmax <- subset(sub, sub$measurement == "tmax..deg.c.") 
      tmin <- subset(sub, sub$measurement == "tmin..deg.c.")
        
      #write files 
      head <- "19800101"
      precip <- data.frame("19800101"=precip$value)
      write.table(head,  paste("daymet/precip/",x,".txt",sep=""), col.names = F, row.names = F, quote=F)
      write.table(precip, paste("daymet/precip/",x,".txt",sep=""), sep = ",", col.names=F, append = T, row.names = F, quote=F)
      
      temp <- data.frame(tmax=tmax$value, tmin=tmin$value)
      write.table(head,  paste("daymet/temp/",x,".txt",sep=""), col.names = F, row.names = F, quote=F)
      write.table(temp, paste("daymet/temp/",x,".txt",sep=""), sep = ",", col.names=F, append = T, row.names = F, quote=F)
    }
  
