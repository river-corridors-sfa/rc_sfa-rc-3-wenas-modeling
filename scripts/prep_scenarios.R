##script to determine and create scenarios for wenas creek swat runs 

#written by Katie A. Wampler 2022-07-05 

#install libraries 
  library(ggplot2)
  library(scales)
  library(ggthemes)
  library(gridExtra)
  library(readr)
  library(stringr)
  library(raster)
  library(sf)
  library(thorloki)
  library(exactextractr)
  library(tools)
  
  

#determine dry and wet years -------
  low <- 2000   #choose low 
  high <- 1996    #choose high 
  
  rep_precip <- function(repl_year, file){
    setwd("C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo") 
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
  
  scenariowd <- paste("C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/Scenario Changes/")
  
  #base 
  rep_precip(2017, "BASE_PCP")
  
  #low
  rep_precip(low, "DRY")
  
  #high
  rep_precip(high, "WET")
  
  #test precip changes 
  for(x in c("BASE_PCP", "DRY", "WET")){
    setwd("C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek/Scenarios/wenascreek.Sufi2.SwatCup/Scenario Changes") 
    pcp <- read.table(paste(x, "/pcp1.pcp", sep=""), skip=4)
    pcp <- pcp %>% separate(col=V1,into=c("Date", "pcp1"),sep=7)
    pcp <- pcp %>% separate(col=pcp1,into=c("pcp1", "pcp2"),sep=5)
    pcp <- pcp %>% separate(col=pcp2,into=c("pcp2", "pcp3"),sep=5)
    pcp <- pcp %>% separate(col=pcp3,into=c("pcp3", "pcp4"),sep=5)
    pcp <- pcp %>% separate(col=pcp4,into=c("pcp4", "pcp5"),sep=5)
    pcp$Date <- as.Date(pcp$Date, format="%Y%j")
    pcp[2:6] <- sapply(pcp[2:6],as.numeric)
    
    pcp$Scenario <- x
    
    pcp <- gather(pcp, station, precip, pcp1:pcp5)
    if(x == "BASE_PCP"){
      plot_pcp <- pcp
    } else{ plot_pcp <- rbind(plot_pcp, pcp)}
  }
  
  plot <- ggplot(filter(plot_pcp, Date >= "2019-08-01" & Date <= "2020-10-31"), aes(x=Date, y=precip, color=station)) + geom_line() + 
    facet_wrap(~Scenario, ncol=1)  + theme_clean() + theme(plot.background = element_blank(), plot.title = element_text(size=10)) + 
    labs(x="Date", y="Precip (mm)", title="BASE: 555.2 mm; DRY: 411.8 mm; WET: 885.6 mm")
  png(filename=paste("Z:/1_Research/3_Wenas Creek SWAT/Figures/Precip_scenarios.png", sep=""), 
      width=14, height = 20, units = 'cm', res = 300)
  grid.arrange(plot)
  dev.off()
  
  mean_pcp <- plot_pcp %>% group_by(Date, Scenario) %>% summarise(precip = mean(precip))
  plot <- ggplot(filter(plot_pcp, Date >= "2019-08-01" & Date <= "2020-10-31"), aes(x=Date, y=precip, color=Scenario)) + geom_line() + 
    facet_wrap(~Scenario, ncol=1)  + theme_clean() + theme(plot.background = element_blank(), plot.title = element_text(size=10)) + 
    labs(x="Date", y="Precip (mm)", title="BASE: 555.2 mm; DRY: 411.8 mm; WET: 885.6 mm")
  png(filename=paste("Z:/1_Research/3_Wenas Creek SWAT/Figures/Precip_scenarios_avg.png", sep=""), 
      width=14, height = 20, units = 'cm', res = 300)
  grid.arrange(plot)
  dev.off()
  
#determine HRU's to burn --------
  #get buffered burn severity map
    #load dnbr raster
    dnbr <- raster("Z:/3_GIS-GPS/Wildfire/MTBS/Wenas/evans_canyon_dnbr.tif")
    
    #clip dnbr to fire outline 
    outline <- st_read("Z:/3_GIS-GPS/Wildfire/MTBS/Wenas/evans_canyon_burn_bndy.shp")
    dnbr <- clean_raster(dnbr, outline, type="numeric", return="raster")
    
    #load shapefile of basin to get extent
    buffer <- st_read("C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/shp_files/mask.shp")
    buffer_prj <- st_transform(buffer, crs(dnbr))
    
    #extend raster
    dnbr_buf <- extend(dnbr, extent(buffer_prj), value=0)
    
    #convert NA to 0's to fill in
    dnbr_buf[is.na(dnbr_buf[])] <- 0 
    
    test <- as.data.frame(raster::rasterToPoints(dnbr_buf))
    colnames(test) <- c("x", "y", "val")
    
    basin <- convert_crs(st_read("C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/shp_files/subs1.shp"), dnbr_buf)
    ggplot() +  geom_sf(data=basin) +
      geom_raster(data=test, aes(x=x, y=y, fill=val))
    
    hru <- convert_crs(st_read("C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/shp_files/hru1.shp"), dnbr_buf)
    
    #perform zonal statistics using the full_hru layer 
        #extract data
        values <- exact_extract(dnbr_buf,hru,"mean", progress=F)
        
        df <- data.frame(hru = hru$HRU_ID, dnbr=values, hru_gis=hru$HRU_GIS)
        write.csv(df, "Z:/1_Research/3_Wenas Creek SWAT/Data/hru_sev_hyunwoo.csv", quote=F, row.names=F)
    
  #load csv 
  data <- read_csv("Z:/1_Research/3_Wenas Creek SWAT/Data/hru_sev_hyunwoo.csv", col_types = cols(hru_gis = col_character()))
  data$sev <- NA
  data$hru_gis  <- str_pad(data$hru_gis, 9, pad="0", side="left")
  #data <- subset(data, data$MAJORITY != 0) #remove non burned areas
  
  #load hru info 
  #get hru info 
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

    #get sub info 
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
    
    hru_areas <- merge(hru_areas, sub_areas, by="sub")
    hru_areas$area_ha <- as.numeric(hru_areas$per_sub) * as.numeric(hru_areas$area_km2) *100    

    write.csv(hru_areas, "Z:/1_Research/3_Wenas Creek SWAT/Data/hyunwoo model/hru_areas.csv", quote=F, row.names=F)
    
    data <- merge(data, hru_areas[,c(2,which(colnames(hru_areas)=="area_ha"))], by.x="hru_gis", by.y="hru")
  
  #get actual percentages 
  real_per <- data.frame(sev = c("unburned","low", "moderate", "high"), 
                         pixels = c(325870, 132708, 20811, 9159))
  real_per$percent <- real_per$pixels / sum(real_per$pixels) * 100
  real_fire <- raster("Z:/3_GIS-GPS/Wildfire/MTBS/Wenas/evans_canyon_dnbr6.tif")
  real_fire <- clean_raster(real_fire, basin,type="catagorical", return="df")
  real_fire[real_fire$val == 0,] <- NA
  fire_key <- data.frame(val = c(1:6), sev = c(NA, "Low", 
                                               "Moderate", "High", NA, NA))
  fire_cols <- c("Low" = "#ffffbf", "Moderate" = "#FDAE61", 
                 "High" = "#D7191C")
  real_fire <- merge(real_fire, fire_key, by="val")
  real_fire$sev <- factor(real_fire$sev, levels=c("Low", "Moderate", "High"), ordered=T)
  
  real_fire <- subset(real_fire, is.na(real_fire$sev) == F)
  real_plot <- ggplot() + geom_sf(data=basin) +
    geom_raster(data=real_fire, aes(x=x, y=y, fill=sev)) + 
    theme_bw() +
    scale_fill_manual(values=fire_cols) + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +  
    labs(fill="Burn Severity") + theme(legend.position = "none")
  
  #function 
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
  
  #specify cutoff
  df <- hru_burn(data, 69.7, 269.4, 438) #cutoff numbers final (69.7, 269.4, 438, 600)
  df[[1]]
  data <- df[[2]]
  
  #write data 
  write_csv(data[,c(1,4)], "Z:/1_Research/3_Wenas Creek SWAT/Data/Parameter Testing/fire_hru_base.txt")

  #write wild_fire.txt 
  wildfire <- data.frame(ihu = 1:5210, fire=0)
  wildfire <- merge(wildfire, data[,c(2,4)], by.x = "ihu", by.y = "hru", all=T)
  wildfire$fire[wildfire$sev == "low"] <- 1
  wildfire$fire[wildfire$sev == "moderate"] <- 2
  wildfire$fire[wildfire$sev == "high"] <- 3  
  wildfire <- wildfire[,c(1:2)]

  write.table(wildfire, "C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/Scenario Changes/BASE_FIRE/wild_fire.txt", quote=F, row.names=F, sep="\t")   
  hru <- merge(hru, wildfire, by.x="HRU_ID", by.y="ihu")
  colnames(hru)[which(colnames(hru)=="fire")] <- "BASE_FIRE"
  hru_plot <- subset(hru, hru$BASE_FIRE != 0)
  BASE_BURN <- ggplot() + geom_sf(data=basin) + geom_sf(data=hru_plot, aes(fill=as.factor(BASE_FIRE)), color=NA) + 
    scale_fill_manual(values=c("#ffffbf","#FDAE61","#D7191C"), 
                      labels=c("Low", "Moderate", "High")) + 
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +  
    labs(fill="Burn Severity") + theme(legend.position=c(0.8,0.8))
  grid.arrange(real_plot, BASE_BURN, ncol=2)

#write other burn severity scenarios -------
  wildfire$fire[wildfire$fire !=0] <- 1
  write.table(wildfire, "C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/Scenario Changes/LOW_FIRE/wild_fire.txt", quote=F, row.names=F, sep="\t")   
  
  wildfire$fire[wildfire$fire !=0] <- 2
  write.table(wildfire, "C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/Scenario Changes/MOD_FIRE/wild_fire.txt", quote=F, row.names=F, sep="\t")   
  
  wildfire$fire[wildfire$fire !=0] <- 3
  write.table(wildfire, "C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/Scenario Changes/HIGH_FIRE/wild_fire.txt", quote=F, row.names=F, sep="\t")   

#get percentages burned (method 2, choose random subbasins to remove)------ 
  #load data 
  data <- read_csv("Z:/1_Research/3_Wenas Creek SWAT/Data/hru_sev_hyunwoo.csv", col_types = cols(hru_gis = col_character()))
  base_sev <- read.csv("Z:/1_Research/3_Wenas Creek SWAT/Data/Parameter Testing/fire_hru_base.txt")
  base_sev$hru_gis  <- str_pad(base_sev$hru_gis, 9, pad="0", side="left")
  base_sev <- merge(base_sev, as.data.frame(hru)[,c(1,3)], by.x="hru_gis", by.y="HRU_GIS")
  data <- merge(data, base_sev, by="hru_gis")
  data <- merge(data, hru_areas[,c(1,2,which(colnames(hru_areas)=="area_ha"))], by.x="hru_gis", by.y="hru")

  #get actual percentages 
  real_per <- data.frame(sev = c("low", "moderate", "high"), 
                         pixels = c(132708, 20811, 9159))
  real_per$percent <- real_per$pixels / sum(real_per$pixels) * 100
  
  reduce_area2 <- function(perc){
    set.seed(13) #make reproducible
    perc_start <- sum(na.omit(data$area_ha[data$sev != "unburned"])) / sum(data$area_ha) * 100
    
    #remove basins if percent is less than starting
    if(perc_start > perc){
      #initialized
      fire_per <- perc_start
      burned <- subset(data, data$sev != "unburned")
      
      #remove HRU's until down to burned area
      while(fire_per > perc){
        rm <- sample(burned$sub, 1)
        hru_rm <- burned$hru_gis[burned$sub == rm]
        row1 <- which(data$hru_gis %in% hru_rm)
        row2 <- which(burned$hru_gis %in% hru_rm)
        data$sev[row1] <- "unburned"
        burned <- burned[-row2,]
        fire_per <- sum(na.omit(data$area_ha[data$sev != "unburned"])) / sum(data$area_ha) * 100
      }
    }
    
    #add basins if percent is more than starting 
    if(perc_start < perc){}
  }
  
  
  reduce_area  <- function(perc){
    set.seed(13) #make reproducible
    perc_start <- sum(na.omit(data$area_ha[data$sev != "unburned"])) / sum(data$area_ha) * 100
    
    #remove basins if percent is less than starting
    if(perc_start > perc){
      #initialized
      fire_per <- perc_start
      burned <- subset(data, data$sev != "unburned")

      #remove HRU's until down to burned area
      while(fire_per > perc){
        rm <- sample(burned$sub, 1)
        hru_rm <- burned$hru_gis[burned$sub == rm]
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
      burned <- merge(burned, hru, by.x= "HRUGIS", by.y="hru_id")
      burned$SUBBASIN <- as.numeric(burned$SUBBASIN)
      set.seed(9)
      #get list of basins to check to add
      sub_add <- sample(burned$SUBBASIN) 
      
      #create df of basins not burned
      unburned <- subset(burned, burned$sev == "unburned")
      unburned <- unburned[order(unburned$MEAN, decreasing=T),]
      
      while(fire_per < perc){ #while not enough has burned
        if(sum(unburned$MEAN) > 0){ #are there basins left that were affected by the fire?
          add <- unburned[1,] #get most burned basin that hasn't been labeled as burned
          row1 <- which(data$HRUGIS %in% add) #see where it is in the data file
          data$sev[row1] <- "low" #relabel basin as burned
          unburned <- unburned[-1,] #remove that basin from the unburned file
        } else{# if you've run out of basins affected by the fire
          row <- 0
          #get basins labeled as burned
          burned_sub <- subset(data, data$sev != "unburned")
          burned_sub <- merge(burned_sub, hru, by.x= "HRUGIS", by.y="hru_id")
          
          check <- F
          while(check == F){ #gets random list of basins to add, but checks on only adds if near one that has already burned
            row <- row+1
            add <- sub_add[row]
            if((add + 1) %in% burned_sub$SUBBASIN | (add - 1) %in% burned_sub$SUBBASIN){
              check <-  T
            }
          }
          
          hru_add <- unburned$HRUGIS[unburned$SUBBASIN == add]
          if(length(hru_add) > 0){
            row1 <- which(data$HRUGIS %in% hru_add)
            row2 <- which(unburned$HRUGIS %in% hru_add)
            data$sev[row1] <- "low"
            unburned <- unburned[-row2,] #remove from potential unburned basins 
            row <- which(sub_add == add) #remove from potential sites to add (already fully added)
            sub_add <- sub_add[-row]
          } else{
            row <- which(sub_add == add)
            sub_add <- sub_add[-row]}
        }
          
        
        fire_per <- sum(data$AREA[data$sev != "unburned"]) / sum(data$AREA) * 100
      }
    }
    
    output <- list(data, fire_per)
  } #returns modified dataframe with less basins burned 
  
  hru_burn <- function(data, perc, low, mod, high){
    set.seed(9) #set seed
    
    #reduce hru's to burn 
    raw_data <- reduce_area(perc)
    perc <- round(raw_data[[2]], 2)
    raw_data <- raw_data[[1]]
    
    #remove unburned 
    df <- subset(raw_data, raw_data$sev != "unburned")
    
    df$sev[df$dnbr <= low] <- "low"
    df$sev[df$dnbr > low & df$dnbr <= mod] <- "moderate"
    df$sev[df$dnbr > mod & df$dnbr <= high ] <- "high"
    
    low_area <- sum(as.numeric(df$area_ha[df$sev == "low"]))
    mod_area <- sum(as.numeric(df$area_ha[df$sev == "moderate"]))
    high_area <- sum(as.numeric(df$area_ha[df$sev == "high"]))
    total <- sum(low_area, mod_area, high_area)
    model_per <- data.frame(sev = c("low", "moderate", "high"), area = c(low_area, mod_area, high_area))
    model_per$percent <- model_per$area / sum(model_per$area) * 100
    
    scenario <- data.frame(sev = model_per$sev, model = model_per$percent, real = real_per$percent)
    total <- data.frame(sev = paste("total_", perc, sep=""), model=sum(scenario$model),real=sum(scenario$real))
    scenario <- rbind(scenario, total)
    output <- list(scenario, df)
    output
  }
  write_wildfire <- function(data, report, name){ 
    #add in unburned 
    full_data <- merge(data, base_sev, by = "hru_gis", all=T)
    
    full_data$sev.y[is.na(full_data$sev.x) == F] <- full_data$sev.x[is.na(full_data$sev.x) == F]
    data <- full_data[, c(1,3)]
    colnames(data) <- c("hru_gis", "sev")
    
    write_csv(data, paste("Z:/1_Research/3_Wenas Creek SWAT/Data/Parameter Testing/fire_hru_",name, ".txt", sep=""))
    
    #write wild_fire.txt 
    wildfire <- data.frame(ihu = 1:5210, fire=0)
    data <- merge(data, full_data, by.x="HRU_ID", by.y="HRU_ID")
    
    wildfire <- merge(wildfire, data, by.x = "ihu", by.y = "OID", all=T)
    wildfire$fire[wildfire$sev == "low"] <- 1
    wildfire$fire[wildfire$sev == "moderate"] <- 2
    wildfire$fire[wildfire$sev == "high"] <- 3  
    wildfire <- wildfire[,c(1:2)] 
    
    wildfire$fire[wildfire$fire !=0] <- 1
    wildfire_l <- wildfire

    wildfire$fire[wildfire$fire !=0] <- 2
    wildfire_m <- wildfire 
    
    wildfire$fire[wildfire$fire !=0] <- 3
    wildfire_h <- wildfire 
    
    dir.create(paste("C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/Scenario Changes/", name, sep=""), showWarnings = F)
    dir.create(paste("C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/Scenario Changes/", name, "/LOW", sep=""), showWarnings = F)
    dir.create(paste("C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/Scenario Changes/", name, "/MOD", sep=""), showWarnings = F)
    dir.create(paste("C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/Scenario Changes/", name, "/HIGH", sep=""), showWarnings = F)
    
    write.table(wildfire_l, paste("C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/Scenario Changes/", name, "/LOW/wild_fire.txt", sep=""), quote=F, row.names=F, sep="\t")   
    write.table(wildfire_m, paste("C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/Scenario Changes/", name, "/MOD/wild_fire.txt", sep=""), quote=F, row.names=F, sep="\t")   
    write.table(wildfire_h, paste("C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/Scenario Changes/", name, "/HIGH/wild_fire.txt", sep=""), quote=F, row.names=F, sep="\t")   
    
    #write percentages report 
    write.csv(report, paste("C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/Scenario Changes/", name, "/Percentages.csv", sep=""), quote=F, row.names=F)
  }
  burn_scenario <- function(data, perc, report, name){
    set.seed(9) #set seed
    
    #reduce hru's to burn 
    raw_data <- reduce_area(perc)
    real_perc <- round(raw_data[[2]], 2)
    raw_data <- raw_data[[1]] 
    
    data_clip <- raw_data[, c(5,4)]
    colnames(data_clip) <- c("hru_gis", "sev")
    
    write_csv(data_clip, paste("Z:/1_Research/3_Wenas Creek SWAT/Data/Parameter Testing/fire_hru_",name, ".txt", sep=""))
    
    #write wild_fire.txt 
    wildfire <- data.frame(ihu = 1:5210, fire=0)

    wildfire <- merge(wildfire, data_clip, by.x = "ihu", by.y = "hru_gis", all=T)
    wildfire$fire[wildfire$sev == "low"] <- 1
    wildfire$fire[wildfire$sev == "moderate"] <- 2
    wildfire$fire[wildfire$sev == "high"] <- 3  
    wildfire <- wildfire[,c(1:2)] 
    
    wildfire$fire[wildfire$fire !=0] <- 1
    wildfire_l <- wildfire
    
    wildfire$fire[wildfire$fire !=0] <- 2
    wildfire_m <- wildfire 
    
    wildfire$fire[wildfire$fire !=0] <- 3
    wildfire_h <- wildfire 
    
    dir.create(paste("C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/Scenario Changes/", name, sep=""), showWarnings = F)
    dir.create(paste("C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/Scenario Changes/", name, "/LOW", sep=""), showWarnings = F)
    dir.create(paste("C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/Scenario Changes/", name, "/MOD", sep=""), showWarnings = F)
    dir.create(paste("C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/Scenario Changes/", name, "/HIGH", sep=""), showWarnings = F)
    
    write.table(wildfire_l, paste("C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/Scenario Changes/", name, "/LOW/wild_fire.txt", sep=""), quote=F, row.names=F, sep="\t")   
    write.table(wildfire_m, paste("C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/Scenario Changes/", name, "/MOD/wild_fire.txt", sep=""), quote=F, row.names=F, sep="\t")   
    write.table(wildfire_h, paste("C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/Scenario Changes/", name, "/HIGH/wild_fire.txt", sep=""), quote=F, row.names=F, sep="\t")   
    
    #create plot
    hru <- merge(hru, wildfire, by.x="HRU_ID", by.y="ihu")
    hru_plot <- hru[(hru$fire != 0),]
      plot <-  ggplot() + geom_sf(data=basin) + geom_sf(data=hru_plot, aes(fill=as.factor(fire)), color=NA) + 
        scale_fill_manual(values=c("#ffffbf","#FDAE61","#D7191C"), 
                          labels=c("Low", "Moderate", "High")) + 
        theme_bw() +
        theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +  
        labs(fill="Burn Severity", title=paste("Goal Percentage: ", perc, ". Actual Percentage: ", real_perc, sep="")) + 
        theme(legend.position=c(0.8,0.8)) 
      
    png(paste("Z:/1_Research/3_Wenas Creek SWAT/Data/Parameter Testing/fire_hru_",name, ".png", sep=""))
    plot
    dev.off()
  }
  ##testing 
    perc <- 10
    low <- 200
    mod <- 400
    high <- 600
  #10 percent area
  df <- hru_burn(data, 10, 200,400,600) #cutoff numbers final (1.32,2.02,3)
  report <- df[[1]]
  report
  mod_data <- df[[2]]
  
  write_wildfire(mod_data, report, "PER_10")
  
  #15 percent area
  df <- hru_burn(data, 15, 1.3,2.03,3) #cutoff numbers final (1.3,2.03,3)
  report <- df[[1]]
  report
  mod_data <- df[[2]]
  
  write_wildfire(mod_data, report, "PER_15")
  
  #20 percent area
  df <- hru_burn(data, 20, 1.43,2.025,3) #cutoff numbers final (1.43,2.025,3)
  report <- df[[1]]
  report
  mod_data <- df[[2]]
  
  write_wildfire(mod_data, report, "PER_20")

  #25 percent area 
  df <- hru_burn(data, 25, 1.42,2.02,3) #cutoff numbers final (1.42,2.02,3)
  report <- df[[1]]
  report
  mod_data <- df[[2]]
  
  write_wildfire(mod_data,report, "PER_25")

  #30 percent area 
  df <- hru_burn(data, 30, 1.33,2.02,3) #cutoff numbers final (1.33,2.02,3)
  report <- df[[1]]
  report
  mod_data <- df[[2]]
  
  write_wildfire(mod_data,report, "PER_30")

  #40 percent area 
  df <- hru_burn(data, 40, 1.26 ,2,3)
  report <- df[[1]]
  report
  mod_data <- df[[2]]
  
  write_wildfire(mod_data,report, "PER_40")
  
  #45 percent area 
  df <- hru_burn(data, 45, 1.2,1.94,3)
  report <- df[[1]]
  report
  mod_data <- df[[2]]
  
  write_wildfire(mod_data,report, "PER_45")
  
  #50 percent area 
  df <- hru_burn(data, 50, 1.18,1.95,3)
  report <- df[[1]]
  report
  mod_data <- df[[2]]
  
  write_wildfire(mod_data,report, "PER_50")

  #60 percent area 
  df <- hru_burn(data, 60, 1.15, 2, 3)
  report <- df[[1]]
  report
  mod_data <- df[[2]]
  
  write_wildfire(mod_data,report, "PER_60")

  #75 percent area 
  df <- hru_burn(data, 75, 1.01, 1.7, 3)
  report <- df[[1]]
  report
  mod_data <- df[[2]]
  
  write_wildfire(mod_data,report, "PER_75")

  #90 percent area 
  df <- hru_burn(data, 90, 0.99, 1.4, 3)
  report <- df[[1]]
  report
  mod_data <- df[[2]]
  
  write_wildfire(mod_data,report, "PER_90")
  
  #100 percent area 
  df <- hru_burn(data, 100, 0.99, 1.4, 3)
  report <- df[[1]]
  report
  mod_data <- df[[2]]
  
  write_wildfire(mod_data,report, "PER_100")
   
#place fire near main stem and near headwaters ------- 
  #load subbasins on the main stem 
    sub <- read.csv("Z:/2_Data/2_PNNL_DOM/Wenas Creek/wenas_subbasins.csv")
    nodes <- read.csv("Z:/2_Data/2_PNNL_DOM/Wenas Creek/flow_nodes.csv")
    
    #go opposite way (start with basins flowing directly into wenas)
    wenas <- sub$Subbasin 
    wenas_plus <- wenas
    for(x in sub$Subbasin){
      flowin <- filter(nodes, TO_NODE == x)
      wenas_plus <- c(wenas_plus, flowin$Subbasin)
    }
    
    wenas_plusplus <- wenas_plus
    for(x in wenas_plus){
      flowin <- filter(nodes, TO_NODE == x)
      wenas_plusplus <- c(wenas_plusplus, flowin$Subbasin)
    }
    
    main <- data.frame(sub=wenas_plusplus)
    write.csv(main, "Z:/2_Data/2_PNNL_DOM/Wenas Creek/near_stream_basins.csv")
    
  #starting point(get headwater basins)
    nodes$numdrainto <- NA
    for(n in 1:nrow(nodes)){
      sub <- nodes$Subbasin[n]
      nodes$numdrainto[n] <- nrow(filter(nodes, nodes$TO_NODE == sub))
    }

  headwater <- filter(nodes, numdrainto == 0)
  headwater <- subset(headwater, !(headwater$Subbasin %in% wenas_plus))
  colnames(headwater)[3] <- "sub"
  write.csv(headwater, "Z:/2_Data/2_PNNL_DOM/Wenas Creek/far_stream_basins.csv")
  
  #figure out basins burned 
  #function 
  hru_burn <- function(data, burned, unburn, low, mod, high){
    burn_data <- subset(data, data$SUBBASIN %in% burned$sub)
    
    #give random burn severities
    set.seed(9)
    dummy_sev <- sample(seq(from=0.5,to=3, by=0.01), nrow(burn_data), replace=T)
    data$MEAN[!(data$HRUGIS %in% burn_data$HRUGIS)] <- 0
    data$MEAN[data$HRUGIS %in% burn_data$HRUGIS] <- dummy_sev
    
    data$sev[data$MEAN <= unburn] <- "unburned"
    data$sev[data$MEAN > unburn & data$MEAN <= low] <- "low"
    data$sev[data$MEAN > low & data$MEAN <= mod] <- "moderate"
    data$sev[data$MEAN > mod & data$MEAN <= high ] <- "high"
    
    unburn_area <- sum(as.numeric(data$AREA[data$sev == "unburned"]))
    low_area <- sum(as.numeric(data$AREA[data$sev == "low"]))
    mod_area <- sum(as.numeric(data$AREA[data$sev == "moderate"]))
    high_area <- sum(as.numeric(data$AREA[data$sev == "high"]))
    total <- sum(unburn_area, low_area, mod_area, high_area)
    model_per <- data.frame(sev = c("unburned","low", "moderate", "high"), area = c(unburn_area, low_area, mod_area, high_area))
    model_per$percent <- model_per$area / sum(model_per$area) * 100
    
    scenario <- data.frame(sev = model_per$sev, model = model_per$percent, real = real_per$percent)
    total <- data.frame(sev = "total", model=sum(scenario$model),real=sum(scenario$real))
    scenario <- rbind(scenario, total)
    output <- list(scenario, data)
    output
  }
  write_wildfire <- function(data, name){ 
    #write data 
    data <- data[,-c(2:15, 17:25)]
    
    dummy <- full_data[1,]
    dummy$HRUGIS <- "Null"
    data <- rbind(dummy, data)
    write_csv(data, paste("Z:/1_Research/3_Wenas Creek SWAT/Data/Parameter Testing/fire_hru_",name, ".txt", sep=""))
    
    #write wild_fire.txt 
    wildfire <- data.frame(ihu = 1:2293, fire=0)
    data <-  data[-1,]
    data <- merge(data, hru, by.x="HRUGIS", by.y="hru_id")
    
    wildfire <- merge(wildfire, data, by.x = "ihu", by.y = "OID", all=T)
    wildfire$fire[wildfire$sev == "low"] <- 1
    wildfire$fire[wildfire$sev == "moderate"] <- 2
    wildfire$fire[wildfire$sev == "high"] <- 3  
    wildfire <- wildfire[,c(1:2)]
    dir.create(paste("C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/Scenario Changes/", name, sep=""), showWarnings = F)
    write.table(wildfire, paste("C:/SWAT/ArcSWAT/ArcSWAT Projects/Wenas Creek_Hyunwoo/Scenario Changes/", name, "/wild_fire.txt", sep=""), quote=F, row.names=F, sep="\t")   
  }
  
  #load csv 
  data <- read_csv("Z:/2_Data/2_PNNL_DOM/Wenas Creek/hru_burn_sev.csv", col_types = cols(HRUGIS = col_character()))
  data$sev <- NA
  data$HRUGIS  <- str_pad(data$HRUGIS, 9, pad="0", side="left")
  
  #load hru info 
  hru <- read_csv("Z:/2_Data/2_PNNL_DOM/Wenas Creek/hru_data.csv",col_types = cols(SUBBASIN = col_character()))
  hru$hru_id <- paste(hru$SUBBASIN, hru$HRU, sep="")
  hru$hru_id   <- str_pad(hru$hru_id, 9, pad="0", side="left")
  
  data <- merge(data, hru, by.x="HRUGIS", by.y="hru_id")
  
  #get actual percentages 
  real_per <- data.frame(sev = c("unburned","low", "moderate", "high"), 
                         pixels = c(325870, 132708, 20811, 9159))
  real_per$percent <- real_per$pixels / sum(real_per$pixels) * 100
  
  #specify cutoffs for near scenario 
  df <- hru_burn(data, main, 1.1,2.63,2.85,3) #cutoff numbers final (0.65,1.32,2.02,3)
  df[[1]]
  subdata <- df[[2]]
  write_wildfire(subdata, "NEAR")
  
  #specify cutoffs for far scenario 
  df <- hru_burn(data, headwater, 1.34,2.66,2.87,3) #cutoff numbers final (0.65,1.32,2.02,3)
  df[[1]]
  subdata <- df[[2]]
  write_wildfire(subdata, "FAR")
  
   

#change the slopes ------ 
  fresh_files <- function(file_ext=NULL){
    if(length(file_ext) > 0){
      setwd(paste(headwd, "Fresh Files",sep=""))
      files <- list.files()
      files <- files[file_ext(files) %in% file_ext]
      invisible(file.copy(files, paste(headwd ,sep=""), overwrite = T))
    }else{
      setwd(paste(headwd, "Fresh Files",sep=""))
      files <- list.files()
      invisible(file.copy(files, paste(headwd ,sep=""), overwrite = T))
    }
    
    
  } 
  
  #for function testing
  file_ext <- "hru"
  type <- "r"
  change <- 0.1
  par <- "HRU_SLP"
  
  change_swat_par <- function(file_ext, type, change, par, layer="all"){
    #get group of files to change
    files <- list.files("C:/SWAT/Wenas Creek_Hyunwoo")
    file_group <- files[file_ext(files)==file_ext]
    file_group <- file_group[file_group != paste("output.", file_ext, sep="")]
    
    pb = txtProgressBar(min = 0, max = length(file_group), initial = 0,style=3) 
    
    #for normal/soil files 
    if(file_ext == "sol"){
      #load files and alter 
      for(i in 1:length(file_group)){
        y <- file_group[i]
        file <- readLines(paste("C:/SWAT/Wenas Creek_Hyunwoo/", y, sep=""))
        
        name_lookup <- data.frame(param=c("SOL_AWC", "SOL_K"), text =c("Ave. AW Incl. Rock Frag", "Ksat."))
        name <- subset(name_lookup, name_lookup$param == par)
        line <- which(str_detect(file, paste(" ", name$text, sep="")) == T)
        if(length(line) != 1){
          stop("Parameter was not found (or it found multiple parameters)")
        }
        org_line <- file[line]
        org_value <- str_split2(org_line, ": ")[2]
        org_value <- na.omit(as.numeric(str_split2(org_value, "[ ]")))
        if(type == "r"){
          new_value <- as.numeric(org_value) * (1 + change)
        } else if(type == "a"){
          new_value <- as.numeric(org_value) + change
        } else if(type=="v"){
          new_value <- change
        } else{
          stop("Please choose v for replacement, a for additive, or r for relative changes")
        }
        
        if(!(layer %in% c("all", 1))){
          stop("code not designed to change only bottom layers")
        }
        if(layer != "all"){
          new_value <- c(new_value[layer], org_value[-1])
        }
        if(is.na(new_value)[1] == T){
          stop("error, new value is NA")
        }
        org_text <- str_split2(org_line, "[:]")[1]
        new_value <- as.character(new_value)
        
        #accounts for multiple soil layers
        value_part <- ""
        for(x in 1:length(new_value)){
            front_space <- paste(rep(" ", (11-nchar(new_value[x]))), collapse="")
          value_part <- paste(value_part, paste(front_space, new_value[x], sep=""), collapse="")
        }
        new_line <- paste(org_text, ":", value_part, sep="")
        
        file[line] <- new_line
        write_lines(file, paste("C:/SWAT/Wenas Creek_Hyunwoo/", y, sep=""))
        setTxtProgressBar(pb,i)
      }
    }else{
      #load files and alter 
      for(i in 1:length(file_group)){
        y <- file_group[i]
        file <- readLines(paste("C:/SWAT/Wenas Creek_Hyunwoo/", y, sep=""))
        line <- which(str_detect(file, paste(" ", par, sep="")) == T)
        if(length(line) != 1){
          stop("Parameter was not found (or it found multiple parameters)")
        }
        org_line <- file[line]
        org_value <- str_split2(gsub(" ", "", org_line), "[|]")[1]
        if(type == "r"){
          new_value <- as.numeric(org_value) * (1 + change)
        } else if(type == "a"){
          new_value <- as.numeric(org_value) + change
        } else if(type=="v"){
          new_value <- change
        } else{
          stop("Please choose v for replacement, a for additive, or r for relative changes")
        }
        org_text <- str_split2(org_line, "[|]")[2]
        new_value <- as.character(new_value)
        front_space <- paste(rep(" ", (16-nchar(new_value))), collapse="")
        new_line <- paste(front_space, new_value, "    |", org_text, sep="")
        
        file[line] <- new_line
        write_lines(file, paste("C:/SWAT/Wenas Creek_Hyunwoo/", y, sep=""))
        setTxtProgressBar(pb,i)
      }
    }
   
    close(pb)
  }

  files <- list.files("C:/SWAT/Wenas Creek_Hyunwoo")
  hru_files <- files[file_ext(files)=="hru"]
  hru_files <- hru_files[hru_files != "output.hru"]
  
  slope_change <- c(-1, -0.5, -0.1, 0.1, 0.5, 1)

  for(x in slope_change){
    #make sure original files are there
   cat("restoring fresh files \n")
     fresh_files("hru") 
    
    names <- data.frame(change=c(-1, -0.5, -0.1, 0.1, 0.5, 1), 
                        name=paste("HRU_SLP_",c("neg_1", "neg_0.5", "neg_0.1", "0.1","0.5","1"), sep=""))
    cat("rewriting files \n")
    
    change_swat_par("hru", "r", x, "HRU_SLP")

    #move files 
    cat("copying files \n")
    
    name <- subset(names,names$change==x)
    name <- name$name
    invisible(file.copy(paste("C:/SWAT/Wenas Creek_Hyunwoo/", hru_files, sep=""), 
              paste("C:/SWAT/Wenas Creek_Hyunwoo/Scenario Changes/", name, sep=""), overwrite=T))
    
    print(x)
  }
  
#change the flow mechanisms-----
  #SURQ 
    fresh_files("mgt") 
    
    change_swat_par("mgt", "r", 0.5, "CN2")
    
    #move files 
    files <- list.files("C:/SWAT/Wenas Creek_Hyunwoo")
    hru_files <- files[file_ext(files)=="mgt"]
    hru_files <- hru_files[hru_files != "output.mgt"]
    invisible(file.copy(paste("C:/SWAT/Wenas Creek_Hyunwoo/", hru_files, sep=""), 
                        paste("C:/SWAT/Wenas Creek_Hyunwoo/Scenario Changes/", SURQ, sep=""), overwrite=T))
  
  #LATQ 
    fresh_files(c("mgt", "sol")) 
    
    change_swat_par("mgt", "r", 0.5, "CN2")
    change_swat_par("sol", "r", 0.7, "SOL_K", layer=1)
    change_swat_par("sol", "r", 0.7, "SOL_AWC")
    
    #move files 
    files <- list.files("C:/SWAT/Wenas Creek_Hyunwoo")
    hru_files <- files[file_ext(files) %in% c("mgt", "sol")]
    hru_files <- hru_files[hru_files != "output.sol"]
    invisible(file.copy(paste("C:/SWAT/Wenas Creek_Hyunwoo/", hru_files, sep=""), 
                        paste("C:/SWAT/Wenas Creek_Hyunwoo/Scenario Changes/", "LATQ", sep=""), overwrite=T))
  #GWQ
    fresh_files(c("mgt", "sol", "gw")) 
    
    change_swat_par("mgt", "r", -0.5, "CN2")
    change_swat_par("sol", "r", -0.7, "SOL_K")
    change_swat_par("sol", "r", -0.7, "SOL_AWC")
    change_swat_par("gw", "v", 2000, "GW_DELAY")
    
    
    #move files 
    files <- list.files("C:/SWAT/Wenas Creek_Hyunwoo")
    hru_files <- files[file_ext(files) %in% c("mgt", "sol", "gw")]
    hru_files <- hru_files[hru_files != "output.sol"]
    invisible(file.copy(paste("C:/SWAT/Wenas Creek_Hyunwoo/", hru_files, sep=""), 
                        paste("C:/SWAT/Wenas Creek_Hyunwoo/Scenario Changes/", "GWQ", sep=""), overwrite=T))
  