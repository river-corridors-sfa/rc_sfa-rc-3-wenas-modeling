## get WQ data in the region to get a better sense of DOC and nitrate measurements for WQ "calibration"
#written by Katie A. Wampler

library(dataRetrieval)
library(dplyr)

#tule river -------
  #get doc/nitrate
    sites <- whatWQPsites(stateCd = "California",
                          countyCd = "Tulare", 
                        characteristicName=c("Organic carbon", "Nitrate")) 
    
    sites <- sites %>% filter(MonitoringLocationTypeName == "Stream")
    data <- readWQPqw(sites$MonitoringLocationIdentifier, c("00681", "00631"))
    
    data <- data %>% dplyr::select(ActivityStartDate, MonitoringLocationIdentifier,CharacteristicName:ResultMeasure.MeasureUnitCode) %>% 
      mutate(site = gsub("USGS-", "",MonitoringLocationIdentifier))
    
  #get flow 
    flow_data <- readNWISdv(gsub("USGS-", "",unique(sites$MonitoringLocationIdentifier)),parameterCd="00060",statCd = "00003")

    data <- data %>% left_join(flow_data, by=c("site" = "site_no", "ActivityStartDate" = "Date"))
    
  #get wq data with flow data 
    data <- data %>% na.omit()
    
    flow_sites <- unique(data$site)
    
  #determine flows that are baseflow and flows that are peak flows 
    #testing quantile limits
      site <- flow_sites[5]
  
      flow_sub <- flow_data %>% filter(site_no == site)  
      
      ggplot(flow_sub, aes(x=Date, y=X_00060_00003)) + geom_line() + geom_hline(yintercept = quantile(flow_sub$X_00060_00003,0.50)) + 
        geom_hline(yintercept = quantile(flow_sub$X_00060_00003,0.90))
      
    #mark values as peak/baseflow or middle for each site and get average/range 
      flow_quant <- flow_data %>% group_by(site_no) %>% summarise(q50 = quantile(X_00060_00003,0.50, na.rm=T), 
                                                                  q90 = quantile(X_00060_00003,0.90, na.rm=T))
      
      data <- data %>% left_join(flow_quant, by=c("site" = "site_no"))
      
      data$flow_level <- "midflow"
      data$flow_level[data$X_00060_00003 <= data$q50] <- "baseflow"
      data$flow_level[data$X_00060_00003 >= data$q90] <- "highflow"
      
    #summarise
      wq_sum <- data %>% group_by(flow_level, site, CharacteristicName) %>% summarise(mean = mean(ResultMeasureValue),
                                                                                      median = median(ResultMeasureValue),
                                                                                      min = min(ResultMeasureValue),
                                                                                      max = max(ResultMeasureValue),
                                                                                      count = n()) 
      
    #try with all the wq data 
      data <- readWQPqw(sites$MonitoringLocationIdentifier, c("00681", "00631"))
      
      data <- data %>% dplyr::select(ActivityStartDate, MonitoringLocationIdentifier,CharacteristicName:ResultMeasure.MeasureUnitCode) %>% 
        mutate(site = gsub("USGS-", "",MonitoringLocationIdentifier))
      wq_sum <- data %>% na.omit() %>% group_by(CharacteristicName) %>% summarise(mean = mean(ResultMeasureValue),
                                                                                      median = median(ResultMeasureValue),
                                                                                      min = min(ResultMeasureValue),
                                                                                      max = max(ResultMeasureValue),
                                                                                      q90 = quantile(ResultMeasureValue,0.90),
                                                                                      q10 = quantile(ResultMeasureValue,0.00),
                                                                                      count = n()) 
#american river -------
      #get doc/nitrate
      sites <- whatWQPsites(bBox = c(-121.771822,46.012263,-120.991087,47.389367), 
                            characteristicName=c("Organic carbon", "Nitrate")) #not a lot of sites, so use bounding box instead

      sites <- sites %>% filter(MonitoringLocationTypeName == "Stream")
      data <- readWQPqw(sites$MonitoringLocationIdentifier, c("00681", "00631"))
      
      data <- data %>% dplyr::select(ActivityStartDate, MonitoringLocationIdentifier,CharacteristicName:ResultMeasure.MeasureUnitCode) %>% 
        mutate(site = gsub("USGS-", "",MonitoringLocationIdentifier))
      
      #get flow 
      flow_data <- readNWISdv(gsub("USGS-", "",unique(sites$MonitoringLocationIdentifier)),parameterCd="00060",statCd = "00003")
      
      data <- data %>% left_join(flow_data, by=c("site" = "site_no", "ActivityStartDate" = "Date"))
      
      #get wq data with flow data 
      data <- data %>% na.omit()
      
      flow_sites <- unique(data$site)
      
      #determine flows that are baseflow and flows that are peak flows 
      #testing quantile limits
      site <- flow_sites[1]
      
      flow_sub <- flow_data %>% filter(site_no == site)  
      
      ggplot(flow_sub, aes(x=Date, y=X_00060_00003)) + geom_line() + geom_hline(yintercept = quantile(flow_sub$X_00060_00003,0.50)) + 
        geom_hline(yintercept = quantile(flow_sub$X_00060_00003,0.95))
      
      #mark values as peak/baseflow or middle for each site and get average/range 
      flow_quant <- flow_data %>% group_by(site_no) %>% summarise(q50 = quantile(X_00060_00003,0.50, na.rm=T), 
                                                                  q90 = quantile(X_00060_00003,0.95, na.rm=T))
      
      data <- data %>% left_join(flow_quant, by=c("site" = "site_no"))
      
      data$flow_level <- "midflow"
      data$flow_level[data$X_00060_00003 <= data$q50] <- "baseflow"
      data$flow_level[data$X_00060_00003 >= data$q90] <- "highflow"
      
      #summarise
      wq_sum <- data %>% group_by(flow_level, CharacteristicName) %>% summarise(mean = mean(ResultMeasureValue),
                                                                                      median = median(ResultMeasureValue),
                                                                                      min = min(ResultMeasureValue),
                                                                                      max = max(ResultMeasureValue),
                                                                                      count = n()) 
      
      #try with all the wq data 
      data <- readWQPqw(sites$MonitoringLocationIdentifier, c("00681", "00631"))
      
      data <- data %>% dplyr::select(ActivityStartDate, MonitoringLocationIdentifier,CharacteristicName:ResultMeasure.MeasureUnitCode) %>% 
        mutate(site = gsub("USGS-", "",MonitoringLocationIdentifier))
      wq_sum <- data %>% na.omit() %>% group_by(CharacteristicName) %>% summarise(mean = mean(ResultMeasureValue),
                                                                                  median = median(ResultMeasureValue),
                                                                                  min = min(ResultMeasureValue),
                                                                                  max = max(ResultMeasureValue),
                                                                                  q90 = quantile(ResultMeasureValue,0.90),
                                                                                  q10 = quantile(ResultMeasureValue,0.00),
                                                                                  count = n()) 
      