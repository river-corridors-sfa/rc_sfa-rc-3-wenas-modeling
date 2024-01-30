#code supporting the thresholds paper (name tbd??) 

#written on 2024-01-30 by Katie A. Wampler 

#section 0: load libraries and functions ------ 
  library(raster) #to work with rasters 
  library(dplyr) #working with datasets 
  library(sf) #work with geospatial data
  library(thorloki) #custom functions to work with geospatial data (available on github "katiewampler/thorloki)
  library(ggplot2) #for plotting
  library(exactextractr) #for zonal stats
  library(readr) #loading large datasets
  library(stringr) #working with strings
  library(tidyr) #for separate function

  #set up a folder to keep the files need to initialize each scenario 
    scenariowd <- paste("C:/SWAT/Wenas Creek_Hyunwoo/Scenario Changes/")

#section 1: replace post-fire precip with average precipitation year ------
  #function to replace precip post-fire with a given replacement year 
  rep_precip <- function(repl_year, file){
    setwd("C:/SWAT/Wenas Creek_Hyunwoo") 
    pcp <- read.table("pcp1.pcp", skip=4)
    pcp <- pcp %>% separate(col=V1,into=c("Date", "pcp1"),sep=7)
    pcp <- pcp %>% separate(col=pcp1,into=c("pcp1", "pcp2"),sep=5)
    pcp <- pcp %>% separate(col=pcp2,into=c("pcp2", "pcp3"),sep=5)
    pcp <- pcp %>% separate(col=pcp3,into=c("pcp3", "pcp4"),sep=5)
    pcp <- pcp %>% separate(col=pcp4,into=c("pcp4", "pcp5"),sep=5)
    
    pcp$Date <- as.Date(pcp$Date, format="%Y%j")
    
    #define fire
    fire_date <- as.Date("2019-08-31")
    fire_month <- 8
    fire_day <- 31
    dates <- seq(fire_date, as.Date("2020-08-31"), by="day") 
    
    #replace with historical years 
    pcp_start <- as.Date(paste(repl_year, fire_month, fire_day, sep="-"))
    pcp_repl <- seq(pcp_start, length.out = length(dates), by="day")
    
    pcp[pcp$Date %in% dates,2:6] <- pcp[pcp$Date %in% pcp_repl,2:6]
    
    #reformat to save 
    pcp$Date <- strftime(pcp$Date, "%Y%j")
    line1 <- "Station  pcp_5,pcp_4,pcp_3,pcp_2,pcp_1,"
    line2 <- "Lati    47.0 46.8 46.8 46.8 46.7"
    line3 <- "Long    -121 -121 -121 -121 -120"
    line4 <- "Elev    1619  793  802  620  403"
    
    for(x in 2:6){
      pcp[nchar(pcp[,x]) == 3, x] <- paste(pcp[nchar(pcp[,x]) == 3,], ".0", sep="")
      
    }
    header <- paste(line1, line2, line3, line4, sep="\n")
    
    write.table(header, paste(scenariowd,file, "/pcp1.pcp", sep=""), quote=F, row.names=F, col.names=F, sep="")
    write.table(pcp, paste(scenariowd,file, "/pcp1.pcp", sep=""), quote=F, row.names=F, col.names=F, sep="", append = TRUE)
  } 
  
  #make base precip scenario
  rep_precip(2017, "BASE_PCP")

#section 2: determine which HRU's to burn based on the real burn severity map ------ 
  #section 2.1: get buffered burn severity map
    #load dnbr raster
    dnbr <- raster("Z:/3_GIS-GPS/Wildfire/MTBS/Wenas/evans_canyon_dnbr.tif")
    
    #clip dnbr to fire outline 
    outline <- st_read("Z:/3_GIS-GPS/Wildfire/MTBS/Wenas/evans_canyon_burn_bndy.shp")
    dnbr <- clean_raster(dnbr, outline, type="numeric", return="raster")
    
    #load shapefile of basin to get extent
    buffer <- st_read("C:/SWAT/Wenas Creek_Hyunwoo/shp_files/mask.shp")
    buffer_prj <- st_transform(buffer, crs(dnbr))
    
    #extend raster
    dnbr_buf <- extend(dnbr, extent(buffer_prj), value=0)
    
    #convert NA to 0's to fill in
    dnbr_buf[is.na(dnbr_buf[])] <- 0 
    
    test <- as.data.frame(raster::rasterToPoints(dnbr_buf))
    colnames(test) <- c("x", "y", "val")
    
    basin <- convert_crs(st_read("C:/SWAT/Wenas Creek_Hyunwoo/shp_files/subs1.shp"), dnbr_buf)
   
  #section 2.2: get the average dNBR for each HRU
    hru <- convert_crs(st_read("C:/SWAT/Wenas Creek_Hyunwoo/shp_files/hru1.shp"), dnbr_buf)
    
    #perform zonal statistics using the full_hru layer 
    #extract data
    values <- exact_extract(dnbr_buf,hru,"mean", progress=F)
    
    #save data
    df <- data.frame(hru = hru$HRU_ID, dnbr=values, hru_gis=hru$HRU_GIS)
    write.csv(df, "Z:/1_Research/3_Wenas Creek SWAT/Data/hru_sev_hyunwoo.csv", quote=F, row.names=F)
    
  #sections 2.3: determine the burn severity classification for each HRU
    #load csv 
    data <- read_csv("Z:/1_Research/3_Wenas Creek SWAT/Data/hru_sev_hyunwoo.csv", col_types = cols(hru_gis = col_character()))
    data$sev <- NA
    data$hru_gis  <- str_pad(data$hru_gis, 9, pad="0", side="left")

    #load hru info, gets the percent of subbasin for each HRU 
      #it reads each file so it takes a bit to run
    setwd("C:/SWAT/Wenas Creek_Hyunwoo")
    hru_files <- list.files()[str_detect(list.files(), ".hru")]
    hru_files <- hru_files[str_detect(hru_files, "00")]
    hru_areas <- as.data.frame(matrix(data=NA, nrow=length(hru_files), ncol=3))
    colnames(hru_areas) <- c("hru", "sub", "per_sub")
    for(x in hru_files){
      row <- which(x == hru_files)
      lines <- as.numeric(str_split2(read_lines(x)[2], "   ", piece=3))
      sub <- as.numeric(str_split2(str_split2(read_lines(x)[1], "Subbasin:", piece=2), " ", piece=1))
      hru_name <- str_split2(x, ".hru", piece=1)
      hru_areas[row,] <- c(hru_name, sub, lines)
      print(x)
    }
    
    #get sub info, grabs the area of each subbasin 
    sub_files <- list.files()[str_detect(list.files(), ".sub")]
    sub_files <- sub_files[str_detect(sub_files, "00")]
    sub_areas <- as.data.frame(matrix(data=NA, nrow=length(sub_files), ncol=2))
    colnames(sub_areas) <- c("sub", "area_km2")
    for(x in sub_files){
      row <- which(x == sub_files)
      read_lines(x)
      sub <- as.numeric(str_split2(str_split2(read_lines(x)[1], "Subbasin:", piece=2), "1.28", piece=1))
      area <- as.numeric(str_split2(read_lines(x)[2], "   ", piece=3))
      sub_areas[row,] <- c(sub, area)
      print(x)
    }
    
    #get the area of each HRU
    hru_areas <- merge(hru_areas, sub_areas, by="sub")
    hru_areas$area_ha <- as.numeric(hru_areas$per_sub) * as.numeric(hru_areas$area_km2) *100    
    
    #save since it takes a bit to run
    write.csv(hru_areas, "Z:/1_Research/3_Wenas Creek SWAT/Data/hyunwoo model/hru_areas.csv", quote=F, row.names=F)
    
    #prep table to link HRU's to burn severity groups
    data <- merge(data, hru_areas[,c(2,which(colnames(hru_areas)=="area_ha"))], by.x="hru_gis", by.y="hru")
    
    #get actual percentages from wildfire (determined in GIS)
    real_per <- data.frame(sev = c("unburned","low", "moderate", "high"), 
                           pixels = c(325870, 132708, 20811, 9159))
    real_per$percent <- real_per$pixels / sum(real_per$pixels) * 100
    
    #function to determine burn severity for each HRU
    hru_burn <- function(data, unburn, low, mod){
      data$sev[data$dnbr <= unburn] <- "unburned"
      data$sev[data$dnbr > unburn & data$dnbr <= low] <- "low"
      data$sev[data$dnbr > low & data$dnbr <= mod] <- "moderate"
      data$sev[data$dnbr > mod] <- "high"
      
      unburn_area <- subset(data, data$sev == "unburned")
      unburn_area <- sum(unburn_area$area_ha)
      low_area <- subset(data, data$sev == "low")
      low_area <- sum(low_area$area_ha)
      mod_area <- subset(data, data$sev == "moderate")
      mod_area <- sum(mod_area$area_ha)
      high_area <- subset(data, data$sev == "high")
      high_area <- sum(high_area$area_ha)
      total <- sum(unburn_area, low_area, mod_area, high_area)
      model_per <- data.frame(sev = c("unburned","low", "moderate", "high"), area = c(unburn_area, low_area, mod_area, high_area))
      model_per$percent <- model_per$area / sum(model_per$area) * 100
      
      scenario <- data.frame(sev = model_per$sev, model = model_per$percent, real = real_per$percent)
      total <- data.frame(sev = "total", model=sum(scenario$model),real=sum(scenario$real))
      scenario <- rbind(scenario, total)
      output <- list(scenario, data)
      output
    }
    ggplot(data, aes(dnbr)) + geom_histogram(bins=100)
    
    #specify cutoff (guess and check trying to match real percentages)
    df <- hru_burn(data, 69.7, 269.4, 438) #cutoff numbers final (69.7, 269.4, 438, 600)
    df[[1]]
    data <- df[[2]]
    
    #write data 
    write_csv(data[,c(1,4)], "Z:/1_Research/3_Wenas Creek SWAT/Data/Parameter Testing/fire_hru_base.txt")
    
    
#section 3: create other burn severity scenarios -------
  #section 3.1: load data that's needed and merge with dataframes
    hru <- st_read("C:/SWAT/Wenas Creek_Hyunwoo/shp_files/hru1.shp")
    basin <- st_read("C:/SWAT/Wenas Creek_Hyunwoo/shp_files/subs1.shp")
    
    base_sev$hru_gis  <- str_pad(base_sev$hru_gis, 9, pad="0", side="left")
    hru_areas$hru  <- str_pad(hru_areas$hru, 9, pad="0", side="left")
    
    base_sev <- merge(base_sev, as.data.frame(hru)[,c(1,3)], by.x="hru_gis", by.y="HRU_GIS")
    data <- merge(data, base_sev, by="hru_gis")
    data <- merge(data, hru_areas[,c(1,2,which(colnames(hru_areas)=="area_ha"))], by.x="hru_gis", by.y="hru")
    hru_sev <- data

  #section 3.2: function to determine scenarios 
      #' Determine HRU's to burn
      #' 
      #' Function to generate burn severity scenarios, it uses average dNBR as much as possible to
      #' add and remove hru's. Once all the hru's with a dNBR of 0 are used, it randomly chooses HRU's
      #' that are next to the existing simulated burn perimeter until it reaches the desired
      #' burn percentage. Saves three wild_fire.txt files at low, moderate, and high severity for use in modeling
      #'
      #'
      #' @param hru_sev a file with the HRU's and the average dNBR
      #' @param perc the goal percentage burned as a number
      #'
      #' @return a list with (1) a plot showing the area burned and actual percentage burned (2) the 
      #' data used to generate the plot 
      #' 
          burn_basin <- function(hru_sev, perc){
            #get tables of hru's that are burned or not
            burned <- hru_sev %>% filter(sev != "unburned")
            unburned <- hru_sev %>% filter(sev == "unburned")
            
            #figure out the starting percent area burned
            perc_start <- sum(na.omit(hru_sev$area_ha[hru_sev$sev != "unburned"])) / sum(hru_sev$area_ha) * 100
            fire_perc <- perc_start  #saving as a different variable to protect
            total_area <- sum(hru_sev$area_ha, na.rm=T) #total area 
            
            #Situation 1: Goal percentage is less than starting percentage 
            #need to remove hru's 
            if(perc < perc_start){
              burned <- burned[order(burned$dnbr, decreasing=F),]
              while(perc < fire_perc){
                #remove lowest DNBR HRU 
                hru_rm <- burned$hru[1]
                unburned <- rbind(unburned, burned %>% filter(hru %in% hru_rm))
                burned <- burned %>% filter(!(hru %in% hru_rm))
                
                #get new area burned 
                fire_perc <- sum(burned$area_ha) / (sum(burned$area_ha) + sum(unburned$area_ha)) * 100
                print(fire_perc)
              }
            }
            
            #Situation 2: Goal percentage is more than the starting percentage 
            if(perc > perc_start){
              #add hru's with highest DBR that are marked as unburned
              unburned <- unburned[order(unburned$dnbr, decreasing=T),]
              while(perc > fire_perc){
                #add the highest DNBR HRU if DNBR is greater than 0
                if(unburned$dnbr[1] > 0){
                  hru_ad <- unburned$hru[1]
                  burned <- rbind(burned, unburned %>% filter(hru %in% hru_ad))
                  unburned <- unburned %>% filter(!(hru %in% hru_ad))
                }else{
                  #get list of basins to check to add
                  ## should have set a seed here but didn't, so can't get exact scenarios back 
                   #set.seed(9)
                  sub_add <- sample(unburned$sub)
                  #if all hru's with nonzero DNBR's have been burned 
                  row <- 0
                  check <- F
                  #gets random list of basins to add, but checks on only adds if near one that has already burned
                  while(check == F){ 
                    row <- row+1
                    add <- sub_add[row]
                    if((add + 1) %in% burned$sub | (add - 1) %in% burned$sub){
                      check <-  T
                    }
                  }
                  ##variable add is the subbasin that is near the rest of the burn
                  hru_ad <- unburned$hru[unburned$sub == add]
                  if(length(hru_ad) > 0){
                    burned <- rbind(burned, unburned %>% filter(hru %in% hru_ad))
                    unburned <- unburned %>% filter(!(hru %in% hru_ad))
                  }}
                
                
                #get new area burned 
                fire_perc <- sum(burned$area_ha) / (sum(burned$area_ha) + sum(unburned$area_ha)) * 100
                print(fire_perc)
              }
            }
            
            #update severity for plotting
            if(nrow(unburned) >0){
              unburned$sev <- "unburned"
            }
            burned$sev <- "moderate"
            
            #plot to check 
            plot_test <- rbind(burned, unburned)
            hru_plot <- merge(hru, plot_test, by.x="HRU_GIS", by.y="hru_gis")
            hru_plot <- hru_plot %>% filter(sev != "unburned")
            plot <- ggplot() + geom_sf(data=basin) + geom_sf(data=hru_plot, color=NA, fill="red") + 
              theme_bw() +
              theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +  
              labs(title=paste("Goal: ", perc, "%, Actual: ", round(fire_perc,3), "%", sep="")) + 
              theme(legend.position=c(0.8,0.8)) 
            
            #write wild_fire.txt file 
            wildfire <- data.frame(ihu = 1:5210, fire=0)
            wildfire$fire[wildfire$ihu %in% burned$hru] <- 1
            
            wildfire$fire[wildfire$fire !=0] <- 1
            wildfire_l <- wildfire
            
            wildfire$fire[wildfire$fire !=0] <- 2
            wildfire_m <- wildfire 
            
            wildfire$fire[wildfire$fire !=0] <- 3
            wildfire_h <- wildfire  
            
            name <- paste("PER_", perc, sep="")
            
            dir.create(paste("C:/SWAT/Wenas Creek_Hyunwoo/Scenario Changes/", name, "/LOW", sep=""), showWarnings = F)
            dir.create(paste("C:/SWAT/Wenas Creek_Hyunwoo/Scenario Changes/", name, "/MOD", sep=""), showWarnings = F)
            dir.create(paste("C:/SWAT/Wenas Creek_Hyunwoo/Scenario Changes/", name, "/HIGH", sep=""), showWarnings = F)
            
            write.table(wildfire_l, paste("C:/SWAT/Wenas Creek_Hyunwoo/Scenario Changes/", name, "/LOW/wild_fire.txt", sep=""), quote=F, row.names=F, sep="\t")   
            write.table(wildfire_m, paste("C:/SWAT/Wenas Creek_Hyunwoo/Scenario Changes/", name, "/MOD/wild_fire.txt", sep=""), quote=F, row.names=F, sep="\t")   
            write.table(wildfire_h, paste("C:/SWAT/Wenas Creek_Hyunwoo/Scenario Changes/", name, "/HIGH/wild_fire.txt", sep=""), quote=F, row.names=F, sep="\t")   
            
            return(list(plot, plot_test))
          }
  
  #section 3.3: create all the scenarios 
    output_10 <- burn_basin(hru_sev, 10)
    output_15 <- burn_basin(hru_sev, 15)
    output_20 <- burn_basin(hru_sev, 20)
    output_25 <- burn_basin(hru_sev, 25)
    output_30 <- burn_basin(hru_sev, 30)
    output_40 <- burn_basin(hru_sev, 40)
    output_45 <- burn_basin(hru_sev, 45)
    output_50 <- burn_basin(hru_sev, 50)
    output_60 <- burn_basin(hru_sev, 60)
    output_75 <- burn_basin(hru_sev, 75)
    output_90 <- burn_basin(hru_sev, 90)
    output_100 <- burn_basin(hru_sev, 100)
  
     
#section 4: run scenarios in swat ------ 
  #section 4.1: functions to run the model 
    #save files somewhere else, needs to be in quotes
      save_files <- function(filename){
      setwd("D:/Wenas Creek Scenarios")
      dir.create(filename, showWarnings = F)
      setwd(paste(headwd, sep=""))
      files <- c("output.hru", "output.rch", "output.std", "output.sub", "wild_fire.txt", "basins.bsn")
      file.copy(paste(headwd, files, sep=""), paste("D:/Wenas Creek Scenarios/", filename ,sep=""), overwrite = T)
      }
    #paste fresh files (ensure the parameters are correct)
      fresh_files <- function(){
      setwd(paste(headwd, "Fresh Files",sep=""))
      files <- list.files()
      invisible(file.copy(files, paste(headwd ,sep=""), overwrite = T))
    } 
    #start fire (turn fire on in basin file, and put in base wild_fire.txt file)
      start_fire <- function(){
      setwd(headwd)
      basin <- readLines("basins.bsn")
      basin[132] <- "               1    | fire: for wildfire scenarios: 1 fire, 0 nofire"
      write.table(basin, "basins.bsn", quote=F, row.names=F, col.names = F)
      file.copy(paste(scenariowd, "BASE_FIRE/wild_fire.txt", sep=""), headwd, overwrite = T)
      
    }
    #remove fire  (turn fire off in basin file)
      remove_fire <- function(){
      setwd(headwd)
      basin <- readLines("basins.bsn")
      basin[132] <- "               0    | fire: for wildfire scenarios: 1 fire, 0 nofire"
      write.table(basin, "basins.bsn", quote=F, row.names=F, col.names = F)} 
      
  #section 4.2: specify model information 
    headwd <- "C:/SWAT/Wenas Creek_Hyunwoo/"
    scenariowd <- "C:/SWAT/Wenas Creek_Hyunwoo/Scenario Changes/"
      
    subbasin <- 313 #number of subbasins
    hru_num <- 5210 #numer of hrus
      
    fire_date <- "2019-08-31" #shifted a year earlier to account for lack of met data in 2021
    fire_date <- strftime(fire_date, format="%j")
      
    #structure for basin file for wildfire
      #12    | NBYR : Number of years simulated (includes 5 years pre-fire)
      #2009    | IYR : Beginning year of simulation 
  
  #section 4.3: run the scenarios 
    #run normal unburned scenario
      fresh_files()
      remove_fire()
      file.copy(paste(scenariowd, "BASE_PCP/pcp1.pcp", sep=""), headwd, overwrite = T)
      setwd(headwd)
      system2("swat.exe")
      save_files("base_unburned")
    
    #run the burn severity scenarios
      scenarios <- c(paste("PER_", c(10,15,20,25,30,40,45,50,60,75,90,100), sep=""))
      for(s in scenarios){
        for(sev in c("LOW", "MOD", "HIGH")){
          start_fire()
          
          #change the burned areas
          file.copy(paste(scenariowd, s, "/", sev, "/wild_fire.txt", sep=""), headwd, overwrite = T)
          
          #run swat
          print("running SWAT")
          setwd(headwd)
          system2("swat.exe")
          
          #save files 
          print("saving files")
          save_files(paste(s, sev, sep="_"))
          
          print(paste(s, sev, sep="_"))
        }
      }
    
    
      