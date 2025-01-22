## create burn scenario based on desired burn percentage 
#step 1: do you need to go up or down? 
#step 2: can you remove areas just by dnbr? 
#step 3: grab a random subbasin (add or remove if connected to burn area based on numbers)

## can I just figure out which ones are touching the edge and remove / add a random one?
library(stringr)
library(readr)
library(raster)
library(sf)
library(dplyr)

#load data 
data <- read_csv("Z:/1_Research/3_Wenas Creek SWAT/Data/hru_sev_hyunwoo.csv", col_types = cols(hru_gis = col_character()))
base_sev <- read.csv("Z:/1_Research/3_Wenas Creek SWAT/Data/Parameter Testing/fire_hru_base.txt")
hru <- st_read("C:/SWAT/Wenas Creek_Hyunwoo/shp_files/hru1.shp")
hru_areas <- read.csv("Z:/1_Research/3_Wenas Creek SWAT/Data/hyunwoo model/hru_areas.csv")

base_sev$hru_gis  <- str_pad(base_sev$hru_gis, 9, pad="0", side="left")
hru_areas$hru  <- str_pad(hru_areas$hru, 9, pad="0", side="left")

base_sev <- merge(base_sev, as.data.frame(hru)[,c(1,3)], by.x="hru_gis", by.y="HRU_GIS")
data <- merge(data, base_sev, by="hru_gis")
data <- merge(data, hru_areas[,c(1,2,which(colnames(hru_areas)=="area_ha"))], by.x="hru_gis", by.y="hru")
hru_sev <- data

#test plot 
  plot_test <- rbind(burned, unburned)
  hru_plot <- merge(hru, plot_test, by="HRU_ID")
  hru_plot <- hru_plot %>% filter(sev != "unburned")
  ggplot() + geom_sf(data=basin) + geom_sf(data=hru_plot, color=NA, fill="red") + 
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +  
    labs(title=paste("Goal Percentage: ", perc, ". Actual Percentage: ", fire_perc, sep="")) + 
    theme(legend.position=c(0.8,0.8)) 
  
burn_basin <- function(hru_sev, perc, hru, name){
  #get tables of hru's that are burned or not
  burned <- hru_sev %>% filter(sev != "unburned")
  unburned <- hru_sev %>% filter(sev == "unburned")
  burn_hru <- hru %>% filter(HRU_ID %in% burned$hru)
  unburn_hru <- hru %>% filter(HRU_ID %in% unburned$hru)
  
  #figure out the starting percent area burned
  perc_start <- sum(na.omit(hru_sev$area_ha[hru_sev$sev != "unburned"])) / sum(hru_sev$area_ha) * 100
  fire_perc <- perc_start  #saving as a different variable to protect
  total_area <- sum(hru_sev$area_ha, na.rm=T) #total area 
  
  #SITUATION 1: Desired percent burned is less than starting percentage 
    #Need to remove burned basins
  if(perc < fire_perc){
    #figure out approximate number to do in ~10 steps
    sub_area <- hru_sev %>% group_by(sub) %>% summarise(area_ha = sum(area_ha)) #find area for each subbasin
    avg_area <- median(sub_area$area_ha) #get average subbasin area
    area_dif <- (total_area * fire_perc/100) - (total_area * perc/100) #area that needs to be removed from burn
    sub_num <- area_dif / avg_area #figure out the average number of subbasin to remove
    #samp_num <- round(diff(displease::seq_ease(x1=1, x2=sub_num*1.3, n=10, type='cubic-out')))
    count <- 1
    #iterates until enough subbasins have been removed
    while(perc < fire_perc){
      #finds hru's that overlap with unburned area so it doesn't pull out a HRU from the middle
      check <- st_intersects(burn_hru, unburn_hru)  
      burn_rm <- burn_hru[lengths(check)!=0,] #removes HRU's which are surrounded by burned areas
      burn_rm$overlap <- lengths(check)[lengths(check)!=0]
      #orders from the most overlap to the littlest
      burn_rm <- as.data.frame(burn_rm)
      burn_rm <- burn_rm[,-4]
      burn_rm <- burn_rm[order(burn_rm$overlap, decreasing=T),]
      
      #group amount of overlap by subbasin
      burn_rm <- merge(burn_rm, hru_sev[,c(5,6)], by="OBJECTID")
      subs <- burn_rm %>% group_by(sub) %>% summarise(count=n())
      subs <- subs[order(subs$count, decreasing=T),]
      
      #chooses a few potential subbasins to remove
      area_dif <- (total_area * fire_perc/100) - (total_area * perc/100)
      sub_num <- area_dif / avg_area  
      samp <- ifelse(ceiling(sub_num *.2) < 1, 1, ceiling(sub_num *.2))
      set.seed(9)
      subs_semi_rd <- subs[(1:round(samp*2)),] #gives some range to not always pick one with least
      
      #choose random overlapping hru (if far from percentage choose more)
      
      #hru_rm <- burn_rm$HRU_ID[(1:samp)]
      #hru_rm <- sample(burn_rm$HRU_ID,samp)
      #sub_rm <- hru_sev$sub[hru_sev$HRU_ID %in% hru_rm]
      sub_rm <- sample(subs_semi_rd$sub, samp, replace=T) #have some randomness
      hru_rm <- burned$HRU_ID[burned$sub %in% sub_rm]
      count <- count + 1
      #move from burned to unburned 
      unburned <- rbind(unburned, burned %>% filter(HRU_ID %in% hru_rm))
      burned <- burned %>% filter(!(HRU_ID %in% hru_rm))
      
      unburn_hru <- rbind(unburn_hru, burn_hru %>% filter(HRU_ID %in% hru_rm))
      burn_hru <- burn_hru %>% filter(!(HRU_ID %in% hru_rm))
      
      #get new area burned 
      fire_perc <- sum(burned$area_ha) / (sum(burned$area_ha) + sum(unburned$area_ha)) * 100
      cat(fire_perc, sep="\n")
    }
    unburned$sev <- "unburned"
    }
  }
  
  
  
#check function
ggplot() + geom_sf(data=basin) + geom_sf(data=burn_rm, color="red")




burn_basin <- function(hru_sev, perc, name){
  set.seed(9) #for reproducability 
  
  #get the starting area burned
  perc_start <- sum(na.omit(hru_sev$area_ha[hru_sev$sev != "unburned"])) / sum(hru_sev$area_ha) * 100
  
  #initialize the things to do loops 
    burned <- hru_sev %>% filter(sev != "unburned")
    burned <- burned[order(burned$dnbr, decreasing = T),]
    
    unburned <- hru_sev %>% filter(sev == "unburned")
    unburned <- unburned[order(unburned$dnbr, decreasing = T),]
    
    fire_perc <- perc_start 
    
    #get subbasin severity 
    sub_sev <- hru_sev %>% group_by(sub) %>% summarise(sub_sev = mean(dnbr))
    sub_sev <- sub_sev[order(sub_sev$sub_sev, decreasing=T),]
  
  #need to remove basins
  if(perc < perc_start){
    while(fire_perc > perc){
      #get order to remove basins
      set.seed(9)
      sub_rm <- data.frame(subs=as.numeric(unique(sample(burned$sub))))
      sub_rm$low <- sub_rm$subs-1
      sub_rm$high <- sub_rm$subs+1
      sub_rm$near <- sub_rm$subs %in% unburned$sub
      sub_rm$low_near <- sub_rm$low %in% unburned$sub
      sub_rm$high_near <- sub_rm$high %in% unburned$sub
      
      sub <- sub_rm[which(sub_rm$near == T),]
      sub_num <- sub$subs[1]
      
      rm <- burned %>% filter(sub == sub_num)
      rm$sev <- "unburned"
      
      burned <- burned %>% filter(!(hru %in% rm$HRU_ID))
      unburned <- rbind(unburned, rm)
      
      fire_perc <- sum(burned$area_ha) / total_area * 100
    }
  }
  
  #need to add basins 
  if(perc > perc_start){
  } 
  
  
}



burn_scenario <- function(hru_sev, perc, name){
  set.seed(9) #set seed for reproducibility 
  
  #determine which hru's are burned 
    #get the starting area burned
      perc_start <- sum(na.omit(hru_sev$area_ha[hru_sev$sev != "unburned"])) / sum(hru_sev$area_ha) * 100
  
    #if percentage desired in more than original 
      if(perc > perc_start){
        fire_per <- perc_start #initialize 
        burned <- hru_sev #create copy to make changes to
        
        #get list of basins to check to add (should be all but gives random order)
        sub_add <- sample(hru_sev$sub) 
        
        #create df of basins not burned in order of dnbr
        unburned <- subset(burned, burned$sev == "unburned")
        unburned <- unburned[order(unburned$dnbr, decreasing=T),] 
        
        #add basins to burn until percentage has been met 
        while(fire_per < perc){
          #are there any more basins affected by the fire?
          if(sum(unburned$dnbr) > 0){
            add <- unburned$HRU_ID[1] #get most burned basin that hasn't been labeled as burned
            row1 <- which(data$HRU_ID %in% add) #see where it is in the data file
            
          }
        }
      }
      
    #if percentage desired is less than original 
      if(perc < perc_start){
        burned <- subset(data, data$sev != "unburned") #keep track of edits
        fire_per <- perc_start 
        
        #remove HRUs until down to desired area 
        
        
      }
  
}
