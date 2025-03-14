## script used to prepare the wild_fire.txt file for the burn scenarios 
  #written by Katie A. Wampler on 2024-10-24 

#section 0: load libraries and functions ------- 
  library(raster)
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(exactextractr)
  library(stringr)
  library(readr)
  
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
  
  #' Interpolate missing data in MTBS rasters 
  #'
  #' @param raster the raster you want to interpolate
  #' @param NA_val the value of the NA values 
  #'
  #' @return a filled raster
  #'
  #' @examples
  interp_mtbs <- function(raster, NA_val=-32768){
    #make large negatives into NA's 
    raster_na <- reclassify(raster, cbind(NA_val-1, NA_val+1, NA), right=FALSE)
   
    #interpolate 
    # Convert raster to a data frame of coordinates and values
    xyz <- as.data.frame(raster_na, xy = TRUE)
    xy.est <- xyz[is.na(xyz[,3]) == T,1:2]
    xyz <- xyz[is.na(xyz[,3]) == F,]
    colnames(xyz) <- c("x", "y", "z")
    
    # Perform interpolation using mba.points
    interp_result <- MBA::mba.points(
      xyz, #existing data
      xy.est, #missing data
    )
    
    # Convert interpolated results back to a raster
    raster_filled <- rbind(xyz, interp_result$xyz.est)
    raster_filled <- raster::rasterFromXYZ(raster_filled, crs = crs(raster)) 
    
    #check 
    plot(raster_filled)
    
    return(raster_filled)
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
  
  #' Determine wildfires scenarios 
  #' 
  #' This function takes the average dNBR for each HRU in a SWAT model and provides a burn simulation for the HRU's to burn
  #' based on where the initial wildfire occured and the percentage specified
  #'
  #' @param data a data.frame with dnbr, sub, hru_gis, and area_ha
  #' @param perc the goal percentage burned
  #' @param seed seed number for replication
  #'
  #' @return a list where the first item is the data.frame "data" with an added column of sev showing burned or unburned, 
  #' the second item is the actual percentage of area burned.
  #'
  burn_area  <- function(data, perc, seed=9){
    set.seed(seed) #make reproducible
    perc_start <- sum(na.omit(data$area_ha[data$dnbr > 0])) / sum(data$area_ha) * 100
    data$sev <- ifelse(data$dnbr >0, "burned", "unburned")
    
    #remove basins if percent is less than starting
    if(perc_start > perc){
      #initialized
      fire_per <- perc_start
      burned <- subset(data, data$dnbr >0 )
      
      #remove HRU's until down to burned area
      while(fire_per > perc){
        hru_rm <- burned$hru_gis[burned$dnbr == min(burned$dnbr)]
        row1 <- which(data$hru_gis %in% hru_rm)
        row2 <- which(burned$hru_gis %in% hru_rm)
        data$sev[row1] <- "unburned"
        burned <- burned[-row2,]
        fire_per <- sum(na.omit(data$area_ha[data$sev != "unburned"])) / sum(data$area_ha) * 100
      }
    }
    
    #add basins if percent is more than starting 
    if(perc_start < perc){
      #initialized
      fire_per <- perc_start #get starting percentage burned
      
      #create df with subbasins (all subbasins)
      burned <- data
      burned$sub <- as.numeric(burned$sub)
      
      #create df of basins not burned
      unburned <- subset(burned, burned$sev == "unburned")
      
      #get list of basins to check to add
      sub_add <- sample(unique(unburned$sub)) 
      
      #while not enough has burned
      while(fire_per < perc){ 
        row <- 0
        #get basins labeled as burned
        burned_sub <- subset(data, data$sev == "burned")
        
        #find a subbasin that is near ones already burned
        check <- F
        while(check == F){ #gets random list of basins to add, but checks on only adds if near one that has already burned
          row <- row+1
          add <- sub_add[row]
          if((add + 1) %in% burned_sub$sub | add %in% burned_sub$sub | (add - 1) %in% burned_sub$sub){
            check <-  T
          }
        }
        
        hru_add <- unburned$hru_gis[unburned$sub == add]
        if(length(hru_add) > 0){
          #sample a random hru from subbasin (don't want to add all and overshoot the area)
          hru_add <- sample(hru_add, 1)
          row1 <- which(data$hru_gis %in% hru_add)
          row2 <- which(unburned$hru_gis %in% hru_add)
          data$sev[row1] <- "burned"
          unburned <- unburned[-row2,] #remove from potential unburned basins 
        } else{
          #remove to prevent it from sampling again
          row <- which(sub_add == add)
          sub_add <- sub_add[-row]}
        
        fire_per <- sum(data$area_ha[data$sev == "burned"]) / sum(data$area_ha) * 100
      }
      
    } 
    return(list(data, fire_per))}#returns modified dataframe with less basins burned 
  
  #' Get dNBR cutoffs for a certain percentage of the basin burned a each severity level
  #'
  #' @param data file with column "dnbr" 
  #' @param unburn a guess for the dNBR value threshold for unburned
  #' @param low a guess for the dNBR value threshold for low severity
  #' @param mod a guess for the dNBR value threshold for moderate severity
  #'
  #' @return 
  #' @export
  #'
  #' @examples
  hru_burn <- function(data, unburn, low, mod){
    data$sev[data$dnbr <= unburn] <- "unburned"
    data$sev[data$dnbr > unburn & data$dnbr <= low] <- "low"
    data$sev[data$dnbr > low & data$dnbr <= mod] <- "moderate"
    data$sev[data$dnbr > mod] <- "high"
    
    model_per <- data %>%  group_by(sev) %>% summarise(area = sum(area_ha)) %>% ungroup() %>% mutate(per = area/ sum(area)*100)
    model_per$sev <- factor(model_per$sev, levels =c("unburned", "low", "moderate", "high"), ordered=T) 
    model_per <- model_per[order(model_per$sev),]
    
    scenario <- merge(model_per, fire_per %>% dplyr::select(sev, percent) %>% rename(real_per = percent), by="sev", sort=F)
   
    total <- data.frame(sev = "total", area=sum(scenario$area), per=sum(scenario$per),real_per=sum(scenario$real_per))
    scenario <- rbind(scenario, total)
    output <- list(scenario, data)
    output
  }

#section 1: load data ------- 
  #download MTBS data from here: https://www.mtbs.gov/direct-download
  
  #tidy mtbs file names 
  wd <- "~/1_Research/4_Wenas_Thresholds/data/mtbs"  #location of MTBS data 
  clean_mtbs(wd, wd)

  #load wildfires
    american <- raster(file.path(wd, "american_dnbr.tif"))
    american_shp <- read_sf(file.path(wd, "american_burn_bndy.shp"))
    american <- clean_raster(american, american_shp, return="raster")
    
    norse <- raster(file.path(wd, "norse_peak_dnbr.tif"))
    norse_shp <- read_sf(file.path(wd, "norse_peak_burn_bndy.shp"))
    norse <- interp_mtbs(norse)
    norse <- clean_raster(norse, norse_shp, return="raster")
    
    pier <- raster(file.path(wd, "pier_dnbr.tif"))
    pier_shp <- read_sf(file.path(wd, "pier_burn_bndy.shp"))
    pier <- clean_raster(pier, pier_shp, return="raster")
    
    schaeffer <- raster(file.path(wd, "schaeffer_dnbr.tif"))
    schaeffer_shp <- read_sf(file.path(wd, "schaeffer_burn_bndy.shp"))
    schaeffer <- clean_raster(schaeffer, schaeffer_shp, return="raster")
    
    WA_fires <-mosaic(american, norse, fun=max) #merge into a single file 
    CA_fires <- mosaic( pier, schaeffer, fun=max) #merge into a single file 

#section 2: create wild_fire.txt for American watershed ------- 
  #section 2.1: get average dNBR for each hru in american basin 
    setwd("C:/SWAT/American River Simp2/American River Simp2/")
    
    #load shapefile of basin to get extent
    buffer <- st_read("Watershed/shapes/subs1.shp")
    buffer <- st_union(buffer) %>% st_sf()
    buffer_prj <- st_transform(buffer, crs(WA_fires))
    
    #extend raster
    dnbr_buf <- extend(WA_fires, extent(buffer_prj), value=0)
    
    #convert NA to 0's to fill in
    dnbr_buf[is.na(dnbr_buf[])] <- 0 
    
    #check layers
    test <- as.data.frame(raster::rasterToPoints(dnbr_buf))
    colnames(test) <- c("x", "y", "val")
    
    ggplot() +
      geom_raster(data=test, aes(x=x, y=y, fill=val)) + 
      geom_sf(data=buffer_prj, fill=NA, color="yellow") 
    
    #load hru file to get average dnbr for each hru 
    hru <- convert_crs(st_read("Watershed/shapes/hru2.shp"), dnbr_buf)
    hru <- hru[order(hru$HRUGIS),]
    hru$HRU_ID <- 1:nrow(hru)
    
    #perform zonal statistics using the full_hru layer 
    #extract data
    values <- exact_extract(dnbr_buf,hru,"mean", progress=F)
    
    df <- data.frame(hru = hru$HRU_ID, dnbr=values, hru_gis=hru$HRUGIS)
    
    #get hru info 
    hru_files <- list.files("Scenarios/American River Simp2.Sufi2.SwatCup/Backup")[str_detect(list.files("Scenarios/American River Simp2.Sufi2.SwatCup/Backup"), ".hru")]
    hru_files <- hru_files[str_detect(hru_files, "00")]
    hru_areas <- as.data.frame(matrix(data=NA, nrow=length(hru_files), ncol=3))
    colnames(hru_areas) <- c("hru", "sub", "per_sub")
    for(x in hru_files){
      row <- which(x == hru_files)
      lines <- read_lines(file.path("Scenarios/American River Simp2.Sufi2.SwatCup/Backup",x))
      per <- as.numeric(str_split_i(lines[2], "   ", i=3))
      sub <- as.numeric(str_split_i(str_split_i(lines[1], "Subbasin:", i=2), " ", i=1))
      hru_name <- str_split_i(x, ".hru", i=1)
      hru_areas[row,] <- c(hru_name, sub, per)
      print(x)
    }
    
    #get sub info 
    sub_files <- list.files("Scenarios/American River Simp2.Sufi2.SwatCup/Backup")[str_detect(list.files("Scenarios/American River Simp2.Sufi2.SwatCup/Backup"), ".sub")]
    sub_files <- sub_files[str_detect(sub_files, "00")]
    sub_areas <- as.data.frame(matrix(data=NA, nrow=length(sub_files), ncol=2))
    colnames(sub_areas) <- c("sub", "area_km2")
    for(x in sub_files){
      row <- which(x == sub_files)
      lines <- read_lines(file.path("Scenarios/American River Simp2.Sufi2.SwatCup/Backup",x))
      sub <- as.numeric(str_split_i(str_split_i(lines[1], "Subbasin:", i=2), "1/2", i=1))
      area <- as.numeric(str_split_i(lines[2], "   ", i=3))
      sub_areas[row,] <- c(sub, area)
      print(x)
    }
    
    hru_areas <- merge(hru_areas, sub_areas, by="sub")
    hru_areas$area_ha <- as.numeric(hru_areas$per_sub) * as.numeric(hru_areas$area_km2) *100    
    
    data <- merge(df, hru_areas, by.x="hru_gis", by.y="hru")
    
    #save data
    write.csv(data, "~/1_Research/4_Wenas_Thresholds/data/American River/hru_sev_American.csv", quote=F, row.names=F)

  #section 2.2: determine real fire percentages 
    perc <- data.frame(goal_perc = seq(from=5, to=100, by=5), real_perc = NA)
    savewd <- "~/1_Research/4_Wenas_Thresholds/data/American River/wild_fire files"
    for(y in 1:nrow(perc)){
      scenario <- burn_area(data, perc$goal_perc[y], seed=y)
      perc$real_perc[y] <- scenario[[2]]
      df <- scenario[[1]] %>% mutate(scenario = perc$goal_perc[y])
      if(y ==1){
        hru_burned <- df
      }else{hru_burned <- rbind(hru_burned, df)}
      
      #write wild_fire.txt files
      wildfire <- data.frame(ihu = 1:nrow(hru_areas), fire=0)

      wildfire <- merge(wildfire, scenario[[1]], by.x = "ihu", by.y = "hru", all=T)
    
      #make file based on hru's that are burned (low, moderate, high scenarios)
      wildfire$fire[wildfire$sev == "burned"] <- 1
      wildfire_l <- wildfire %>% dplyr::select(ihu, fire)
      
      wildfire$fire[wildfire$sev == "burned"] <- 2
      wildfire_m <- wildfire %>% dplyr::select(ihu, fire)
      
      wildfire$fire[wildfire$sev == "burned"] <- 3
      wildfire_h <- wildfire %>% dplyr::select(ihu, fire)
      
      name <- paste0("PER_", perc$goal_perc[y])
      dir.create(paste("~/1_Research/4_Wenas_Thresholds/data/American River/Scenario Changes/", name, sep=""), showWarnings = F)
      dir.create(paste("~/1_Research/4_Wenas_Thresholds/data/American River/Scenario Changes/", name, "/LOW", sep=""), showWarnings = F)
      dir.create(paste("~/1_Research/4_Wenas_Thresholds/data/American River/Scenario Changes/", name, "/MOD", sep=""), showWarnings = F)
      dir.create(paste("~/1_Research/4_Wenas_Thresholds/data/American River/Scenario Changes/", name, "/HIGH", sep=""), showWarnings = F)
      
      write.table(wildfire_l, paste("~/1_Research/4_Wenas_Thresholds/data/American River/Scenario Changes/", name, "/LOW/wild_fire.txt", sep=""), quote=F, row.names=F, sep="\t")   
      write.table(wildfire_m, paste("~/1_Research/4_Wenas_Thresholds/data/American River/Scenario Changes/", name, "/MOD/wild_fire.txt", sep=""), quote=F, row.names=F, sep="\t")   
      write.table(wildfire_h, paste("~/1_Research/4_Wenas_Thresholds/data/American River/Scenario Changes/", name, "/HIGH/wild_fire.txt", sep=""), quote=F, row.names=F, sep="\t")   
      
    }

    write.csv(hru_burned, file.path(savewd, "hru_burn_scenarios.csv"), quote=F, row.names=F) 

  #section 2.3: check scenarios 
    plot_hru <- merge(hru, hru_burned, by.y="hru_gis", by.x="HRUGIS") 
    plot_hru$name <- factor(paste(plot_hru$scenario, "Percent"), levels=paste(seq(from=5, to=100, by=5), "Percent"), ordered=T)
    plot_hru$sev[plot_hru$sev == "unburned"] <- NA
    plot_hru <- na.omit(plot_hru)
    p1 <- ggplot() + geom_sf(data=buffer_prj, fill=NA, color="black") +
      geom_sf(data=plot_hru, fill= "darkred", color=NA) +
      facet_wrap(~name) + theme_bw() + 
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
    
    png("~/1_Research/4_Wenas_Thresholds/figures/pub figures/american_burn_scenarios.png", units="cm", height = 20, width=40, res=300)
    p1    
    dev.off()
 
#section 3: create wild_fire.txt for Tule watershed ------- 
    #section 2.1: get average dNBR for each hru in american basin 
    setwd("C:/SWAT/Tule River Simp2/Tule River Simp2/")
    
    #load shapefile of basin to get extent
    buffer <- st_read("Watershed/shapes/subs1.shp")
    buffer <- st_union(buffer) %>% st_sf()
    buffer_prj <- st_transform(buffer, crs(CA_fires))
    
    #extend raster
    dnbr_buf <- extend(CA_fires, extent(buffer_prj), value=0)
    
    #convert NA to 0's to fill in
    dnbr_buf[is.na(dnbr_buf[])] <- 0 
    
    #check layers
    test <- as.data.frame(raster::rasterToPoints(dnbr_buf))
    colnames(test) <- c("x", "y", "val")
    
    ggplot() +
      geom_raster(data=test, aes(x=x, y=y, fill=val)) + 
      geom_sf(data=buffer_prj, fill=NA, color="yellow") 
    
    #load hru file to get average dnbr for each hru 
    hru <- convert_crs(st_read("Watershed/shapes/hru2.shp"), dnbr_buf)
    hru <- hru[order(hru$HRUGIS),]
    hru$HRU_ID <- 1:nrow(hru)
    
    #perform zonal statistics using the full_hru layer 
    #extract data
    values <- exact_extract(dnbr_buf,hru,"mean", progress=F)
    
    df <- data.frame(hru = hru$HRU_ID, dnbr=values, hru_gis=hru$HRUGIS)
    
    #get hru info 
    hru_files <- list.files("Scenarios/Tule River Simp2.Sufi2.SwatCup/Backup")[str_detect(list.files("Scenarios/Tule River Simp2.Sufi2.SwatCup/Backup"), ".hru")]
    hru_files <- hru_files[str_detect(hru_files, "00")]
    hru_areas <- as.data.frame(matrix(data=NA, nrow=length(hru_files), ncol=3))
    colnames(hru_areas) <- c("hru", "sub", "per_sub")
    for(x in hru_files){
      row <- which(x == hru_files)
      lines <- read_lines(file.path("Scenarios/Tule River Simp2.Sufi2.SwatCup/Backup",x))
      per <- as.numeric(str_split_i(lines[2], "   ", i=3))
      sub <- as.numeric(str_split_i(str_split_i(lines[1], "Subbasin:", i=2), " ", i=1))
      hru_name <- str_split_i(x, ".hru", i=1)
      hru_areas[row,] <- c(hru_name, sub, per)
      print(x)
    }
    
    #get sub info 
    sub_files <- list.files("Scenarios/Tule River Simp2.Sufi2.SwatCup/Backup")[str_detect(list.files("Scenarios/Tule River Simp2.Sufi2.SwatCup/Backup"), ".sub")]
    sub_files <- sub_files[str_detect(sub_files, "00")]
    sub_areas <- as.data.frame(matrix(data=NA, nrow=length(sub_files), ncol=2))
    colnames(sub_areas) <- c("sub", "area_km2")
    for(x in sub_files){
      row <- which(x == sub_files)
      lines <- read_lines(file.path("Scenarios/Tule River Simp2.Sufi2.SwatCup/Backup",x))
      sub <- as.numeric(str_split_i(str_split_i(lines[1], "Subbasin:", i=2), "12/10", i=1))
      area <- as.numeric(str_split_i(lines[2], "   ", i=3))
      sub_areas[row,] <- c(sub, area)
      print(x)
    }
    
    hru_areas <- merge(hru_areas, sub_areas, by="sub")
    hru_areas$area_ha <- as.numeric(hru_areas$per_sub) * as.numeric(hru_areas$area_km2) *100    
    
    data <- merge(df, hru_areas, by.x="hru_gis", by.y="hru")
    
    #save data
    write.csv(data, "~/1_Research/4_Wenas_Thresholds/data/Tule_River/hru_sev_Tule.csv", quote=F, row.names=F)
    
    #section 2.2: determine real fire percentages 
    perc <- data.frame(goal_perc = seq(from=5, to=100, by=5), real_perc = NA)
    savewd <- "~/1_Research/4_Wenas_Thresholds/data/Tule_River/wild_fire files"
    for(y in 1:nrow(perc)){
      scenario <- burn_area(data, perc$goal_perc[y], seed=y)
      perc$real_perc[y] <- scenario[[2]]
      df <- scenario[[1]] %>% mutate(scenario = perc$goal_perc[y])
      if(y ==1){
        hru_burned <- df
      }else{hru_burned <- rbind(hru_burned, df)}
      
      #write wild_fire.txt files
      wildfire <- data.frame(ihu = 1:nrow(hru_areas), fire=0)
      
      wildfire <- merge(wildfire, scenario[[1]], by.x = "ihu", by.y = "hru", all=T)
      
      #make file based on hru's that are burned (low, moderate, high scenarios)
      wildfire$fire[wildfire$sev == "burned"] <- 1
      wildfire_l <- wildfire %>% dplyr::select(ihu, fire)
      
      wildfire$fire[wildfire$sev == "burned"] <- 2
      wildfire_m <- wildfire %>% dplyr::select(ihu, fire)
      
      wildfire$fire[wildfire$sev == "burned"] <- 3
      wildfire_h <- wildfire %>% dplyr::select(ihu, fire)
      
      name <- paste0("PER_", perc$goal_perc[y])
      dir.create(paste("~/1_Research/4_Wenas_Thresholds/data/Tule_River/Scenario Changes/", name, sep=""), showWarnings = F)
      dir.create(paste("~/1_Research/4_Wenas_Thresholds/data/Tule_River/Scenario Changes/", name, "/LOW", sep=""), showWarnings = F)
      dir.create(paste("~/1_Research/4_Wenas_Thresholds/data/Tule_River/Scenario Changes/", name, "/MOD", sep=""), showWarnings = F)
      dir.create(paste("~/1_Research/4_Wenas_Thresholds/data/Tule_River/Scenario Changes/", name, "/HIGH", sep=""), showWarnings = F)
      
      write.table(wildfire_l, paste("~/1_Research/4_Wenas_Thresholds/data/Tule_River/Scenario Changes/", name, "/LOW/wild_fire.txt", sep=""), quote=F, row.names=F, sep="\t")   
      write.table(wildfire_m, paste("~/1_Research/4_Wenas_Thresholds/data/Tule_River/Scenario Changes/", name, "/MOD/wild_fire.txt", sep=""), quote=F, row.names=F, sep="\t")   
      write.table(wildfire_h, paste("~/1_Research/4_Wenas_Thresholds/data/Tule_River/Scenario Changes/", name, "/HIGH/wild_fire.txt", sep=""), quote=F, row.names=F, sep="\t")   
      
    }
    
    write.csv(hru_burned, file.path(savewd, "hru_burn_scenarios.csv"), quote=F, row.names=F) 
    
    #section 2.3: check scenarios 
    plot_hru <- merge(hru, hru_burned, by.y="hru_gis", by.x="HRUGIS") 
    plot_hru$name <- factor(paste(plot_hru$scenario, "Percent"), levels=paste(seq(from=5, to=100, by=5), "Percent"), ordered=T)
    plot_hru$sev[plot_hru$sev == "unburned"] <- NA
    plot_hru <- na.omit(plot_hru)
    p1 <- ggplot() + geom_sf(data=buffer_prj, fill=NA, color="black") +
      geom_sf(data=plot_hru, fill= "darkred", color=NA) +
      facet_wrap(~name) + theme_bw() + 
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
    
    png("~/1_Research/4_Wenas_Thresholds/figures/pub figures/tule_burn_scenarios.png", units="cm", height = 20, width=40, res=300)
    p1    
    dev.off()
    