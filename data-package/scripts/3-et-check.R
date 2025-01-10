#code used to examine MODIS ET data for different SWAT model HRU's to ensure consistency. 
  #written by Katie A. Wampler

# set basin info
  #list potential HRU numbers, the landuse values, and the landuse name
    et_sites <- data.frame(hru=c(112,48), 
                        landuse=c(52,42), 
                        name=c("RNGB", "FRSE"))

  
  #file paths for needed data
  ETwd <- "Z:/1_Research/4_Wenas_Thresholds/data/American River/" #location of MODIS ET data
  hru_shp <- "C:/SWAT/American River Simp2/American River Simp2/Watershed/shapes/hru2.shp" #HRU shapefile
  
#section 0: load libraries and functions ------ 
  library(sf)
  library(raster)
  library(data.table)
  library(terra)
  library(ggplot2) 
  library(dplyr)
  sf_use_s2(FALSE)
  
  
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
  
#section 1: get sites that are needed ------ 
  #download cleaned ET files
  files <- list.files(paste(ETwd, "MODIS", sep=""))
  ET_file <- paste(ETwd, "MODIS/", files[grep("site_[0-9]+_MOD16A2GF_ET_500m", files)] , sep="")
  ET_dat <- comb_files(ET_file)
  
  sites <- unique(ET_dat %>% dplyr::select(x, y, site))

  #determine the sites I need for each HRU 
    #get ET site locations for different landuses 
    shp <- read_sf(hru_shp)
    shp <- shp[order(shp$HRUGIS, decreasing=F),] #need to give numbers to HRU's and they don't start out in order
    shp$HRU_ID <- 1:nrow(shp)
    landuse <- raster(paste(ETwd, "projected_spatial/landcover.tif", sep=""))
    modis_sites <- st_as_sf(x = sites, coords = c("x", "y"),crs = "EPSG:4326")
    plots <- list() #put maps on points across hru here for checking
    rm("et_loc")
    for(x in 1:nrow(et_sites)){
      print(x)
      subshp <- subset(shp, shp$HRU_ID == et_sites$hru[x])
      subshp <- st_transform(subshp, crs = "EPSG:4326")
      
      #figure out what sites are in HRU
      subsites <- st_intersection(subshp, modis_sites) 
      
      #check locations 
      p1<-ggplot() + geom_sf(data=subshp)  + geom_sf(data=subsites, color="red")
      plots[[x]] <- p1
      
      if(nrow(subsites) > 0){
        #check landuse 
        landuse <- projectRaster(landuse, crs="EPSG:4326", method="ngb")
        subsites$landuse <- terra::extract(landuse, subsites)
        
        #remove any incorrect landuses 
       subsites <- subset(subsites, subsites$landuse == et_sites$landuse[x])
        
        if(nrow(subsites) >0){
          #make site names unique 
          row.names(subsites) <- 1:nrow(subsites)
          
          if(exists("et_loc") == F){
            et_loc <- subsites }else{et_loc <- rbind(et_loc,subsites)}
        }else{cat(paste("hru", et_sites$hru[x], "had no ET spots"))}
        
      }
      
    }
  
#get data and plot for each set of sites ------- 
  #get just data for sites of interest 
  ET_dat2 <- merge(et_loc, ET_dat, by="site")

  #plot by site 
  ggplot(ET_dat2, aes(x=date, y=ET_500m, color=as.factor(HRU_ID))) + geom_line() + facet_wrap(~site) + 
    theme(legend.position = "none")
  ggplot(ET_dat2, aes(x=date, y=ET_500m, color=site)) + geom_line() + facet_wrap(~HRU_ID, ncol=1) + 
    theme(legend.position = "none")
  
  #get average and plot
  ET_avg <- ET_dat2 %>% group_by(HRU_ID,date) %>% summarise(avg = mean(ET_500m))
  ggplot(ET_avg, aes(x=date, y=avg, color=as.factor(HRU_ID))) + geom_line()
