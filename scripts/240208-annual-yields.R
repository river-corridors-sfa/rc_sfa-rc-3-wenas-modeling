## code to get peak values and annual yields from L1 thresholds data 

#written by Katie A. Wampler on 2024-02-08 

library(dplyr)
library(ggplot2)

#load data
load("C:/Users/russ143/OneDrive - PNNL/Documents/1_Research/4_Wenas_Thresholds/2023-10-05_output_datset_L1.rda")

#load data with full year scenario 
full_outputs <- read.csv("C:/Users/russ143/OneDrive - PNNL/Documents/1_Research/4_Wenas_Thresholds/full_L1_data.csv")
full_outputs$year <- year(full_outputs$dates)
full_outputs$day <- day(full_outputs$dates)
full_outputs$month <- month(full_outputs$dates)
full_outputs$month_day <- 
  as.Date(paste("1900", full_outputs$month, full_outputs$day, sep="-"))
pr_wy <- which(full_outputs$month_day < as.Date("1900-08-31"))
full_outputs$year[pr_wy] <- full_outputs$year[pr_wy] - 1

#subset to only burn severity scenarios 
per_data <- subset(outputs_final, outputs_final$scenario_group == "Area Burned x Severity")
full_outputs <- subset(full_outputs, full_outputs$scenario_group== "Area Burned x Severity")

#convert flow to mm/day 
conv_flow <- function(flow, area_m2=496994100){
  #seconds to day (m3/day)
  flow <- flow * 60*60*24 
  
  #divide by area (m/day)
  flow <- flow / area_m2 
  
  #convert to mm/day 
  flow <- flow * 1000
  
  return(flow)
}
per_data$flow_mm_day_fire <- conv_flow(per_data$flow_m3s_fire)
per_data$flow_mm_day_nofire <- conv_flow(per_data$flow_m3s_nofire)

full_outputs$flow_mm_day_fire <- conv_flow(full_outputs$flow_m3s_fire)
full_outputs$flow_mm_day_nofire <- conv_flow(full_outputs$flow_m3s_nofire)

#convert nitrate, tss, and doc to kg/day
conv_wq <- function(wq, flow_m3_s){
  #convert flow from m3/s to L/s 
  flow_L <- flow_m3_s * 1000
  
  #mg/L to mg/s 
  wq <- wq * flow_L
  
  #seconds to day (mg/day)
  wq <- wq * 60*60*24 
  
  #mg to kg (kg/day)
  wq <- wq / 1e6 
  
  return(wq)
}
full_outputs$sed_kg_day_fire <- conv_wq(full_outputs$sed_mg_l_fire, full_outputs$flow_m3s_fire)
full_outputs$sed_kg_day_nofire <- conv_wq(full_outputs$sed_mg_l_nofire, full_outputs$flow_m3s_nofire)
full_outputs$nit_kg_day_fire <- conv_wq(full_outputs$nit_mg_l_fire, full_outputs$flow_m3s_fire)
full_outputs$nit_kg_day_nofire <- conv_wq(full_outputs$nit_mg_l_nofire, full_outputs$flow_m3s_nofire)
full_outputs$doc_kg_day_fire <- conv_wq(full_outputs$doc_mg_l_fire, full_outputs$flow_m3s_fire)
full_outputs$doc_kg_day_nofire <- conv_wq(full_outputs$doc_mg_l_nofire, full_outputs$flow_m3s_nofire)

per_data$sed_kg_day_fire <- conv_wq(per_data$sed_mg_l_fire, per_data$flow_m3s_fire)
per_data$sed_kg_day_nofire <- conv_wq(per_data$sed_mg_l_nofire, per_data$flow_m3s_nofire)
per_data$nit_kg_day_fire <- conv_wq(per_data$nit_mg_l_fire, per_data$flow_m3s_fire)
per_data$nit_kg_day_nofire <- conv_wq(per_data$nit_mg_l_nofire, per_data$flow_m3s_nofire)
per_data$doc_kg_day_fire <- conv_wq(per_data$doc_mg_l_fire, per_data$flow_m3s_fire)
per_data$doc_kg_day_nofire <- conv_wq(per_data$doc_mg_l_nofire, per_data$flow_m3s_nofire)

#get annual yields 
yields <- per_data %>% group_by(scenario) %>% 
  summarise(flow_mm_yr_fire = sum(flow_mm_day_fire, na.rm=T),
            flow_mm_yr_nofire = sum(flow_mm_day_nofire, na.rm=T),
            sed_kg_yr_fire = sum(sed_kg_day_fire, na.rm=T),
            sed_kg_yr_nofire = sum(sed_kg_day_nofire, na.rm=T),
            nit_kg_yr_fire = sum(nit_kg_day_fire, na.rm=T),
            nit_kg_yr_nofire = sum(nit_kg_day_nofire, na.rm=T),
            doc_kg_yr_fire = sum(doc_kg_day_fire, na.rm=T),
            doc_kg_yr_nofire = sum(doc_kg_day_nofire, na.rm=T), 
            percent = last(percent),
            severity = last(severity)) 

yields_full <- full_outputs %>% filter(!(year %in% c(2013,2020))) %>%
  group_by(scenario, year) %>% 
  summarise(flow_mm_yr_fire = sum(flow_mm_day_fire, na.rm=T),
            flow_mm_yr_nofire = sum(flow_mm_day_nofire, na.rm=T),
            sed_kg_yr_fire = sum(sed_kg_day_fire, na.rm=T),
            sed_kg_yr_nofire = sum(sed_kg_day_nofire, na.rm=T),
            nit_kg_yr_fire = sum(nit_kg_day_fire, na.rm=T),
            nit_kg_yr_nofire = sum(nit_kg_day_nofire, na.rm=T),
            doc_kg_yr_fire = sum(doc_kg_day_fire, na.rm=T),
            doc_kg_yr_nofire = sum(doc_kg_day_nofire, na.rm=T), 
            percent = last(percent),
            severity = last(severity))

#get peak values 
peaks <- per_data %>% group_by(scenario) %>% 
  summarise(peak_flow_fire = max(flow_m3s_fire, na.rm=T),
            peak_flow_nofire = max(flow_m3s_nofire, na.rm=T),
            peak_sed_fire = max(sed_mg_l_fire, na.rm=T),
            peak_sed_nofire = max(sed_mg_l_nofire, na.rm=T),
            peak_nit_fire = max(nit_mg_l_fire, na.rm=T),
            peak_nit_nofire = max(nit_mg_l_nofire, na.rm=T),
            peak_doc_fire = max(doc_mg_l_fire, na.rm=T),
            peak_doc_nofire = max(doc_mg_l_nofire, na.rm=T),
            percent = last(percent),
            severity = last(severity)) 

peaks_full <- full_outputs %>% group_by(scenario, year) %>% 
  filter(!(year %in% c(2013,2020))) %>%
  summarise(peak_flow_fire = max(flow_m3s_fire, na.rm=T),
            peak_flow_nofire = max(flow_m3s_nofire, na.rm=T),
            peak_sed_fire = max(sed_mg_l_fire, na.rm=T),
            peak_sed_nofire = max(sed_mg_l_nofire, na.rm=T),
            peak_nit_fire = max(nit_mg_l_fire, na.rm=T),
            peak_nit_nofire = max(nit_mg_l_nofire, na.rm=T),
            peak_doc_fire = max(doc_mg_l_fire, na.rm=T),
            peak_doc_nofire = max(doc_mg_l_nofire, na.rm=T),
            percent = last(percent),
            severity = last(severity))

#explore annual yields
    #sediment
    ggplot(yields, aes(x=as.numeric(percent), y=sed_kg_yr_fire/10000, color=severity)) + 
      geom_point() + geom_line() + 
      geom_hline(yintercept=max(yields$sed_kg_yr_nofire)/10000, linetype="dashed")
    
    ggplot() + 
      geom_point(yields, mapping=aes(x=as.numeric(percent), y=sed_kg_yr_fire/10000, color=severity)) + geom_line() + 
      geom_rect(mapping=aes(xmin=10, xmax=100,
                            ymin=min(yields_full$sed_kg_yr_nofire)/10000,
                            ymax=max(yields_full$sed_kg_yr_nofire)/10000), 
                alpha=0.5) + 
      geom_hline(yintercept=max(yields$sed_kg_yr_nofire)/10000, linetype="dashed")


    
    ggplot(yields, aes(x=as.numeric(percent), y=nit_kg_yr_fire/10000, color=severity)) + 
      geom_point() + geom_line()  + 
      geom_hline(yintercept=max(yields$nit_kg_yr_nofire)/10000, linetype="dashed")
    
    ggplot() + 
      geom_point(yields, mapping=aes(x=as.numeric(percent), y=nit_kg_yr_fire/10000, color=severity)) + geom_line() + 
      geom_rect(mapping=aes(xmin=10, xmax=100,
                            ymin=min(yields_full$nit_kg_yr_nofire)/10000,
                            ymax=max(yields_full$nit_kg_yr_nofire)/10000), 
                alpha=0.5) + 
      geom_hline(yintercept=max(yields$nit_kg_yr_nofire)/10000, linetype="dashed")
    
    
    ggplot(yields, aes(x=as.numeric(percent), y=doc_kg_yr_fire/10000, color=severity)) + 
      geom_point() + geom_line() + 
      geom_hline(yintercept=max(yields$doc_kg_yr_nofire)/10000, linetype="dashed")
    ggplot() + 
      geom_point(yields, mapping=aes(x=as.numeric(percent), y=doc_kg_yr_fire/10000, color=severity)) + geom_line() + 
      geom_rect(mapping=aes(xmin=10, xmax=100,
                            ymin=min(yields_full$doc_kg_yr_nofire)/10000,
                            ymax=max(yields_full$doc_kg_yr_nofire)/10000), 
                alpha=0.5) + 
      geom_hline(yintercept=max(yields$doc_kg_yr_nofire)/10000, linetype="dashed")
    
    ggplot(yields, aes(x=as.numeric(percent), y=flow_mm_yr_fire, color=severity)) + 
      geom_point() + geom_line() +  geom_hline(yintercept=max(yields$flow_mm_yr_nofire), linetype="dashed")
    
    
    ggplot() + 
      geom_point(yields, mapping=aes(x=as.numeric(percent), y=flow_mm_yr_fire, color=severity)) + geom_line() + 
      geom_rect(mapping=aes(xmin=10, xmax=100,
                            ymin=min(yields_full$flow_mm_yr_nofire),
                            ymax=max(yields_full$flow_mm_yr_nofire)), 
                alpha=0.5) + 
      geom_hline(yintercept=max(yields$flow_mm_yr_nofire), linetype="dashed")


#explore peak values
  #sediment
  ggplot(peaks, aes(x=as.numeric(percent), y=peak_sed_fire, color=severity)) + 
    geom_point() + geom_line() + 
    geom_hline(yintercept=max(peaks$peak_sed_nofire), linetype="dashed")
  
  ggplot() + 
    geom_point(peaks, mapping=aes(x=as.numeric(percent), y=peak_sed_fire, color=severity)) + geom_line() + 
    geom_rect(mapping=aes(xmin=10, xmax=100,
                          ymin=min(peaks_full$peak_sed_nofire),
                          ymax=max(peaks_full$peak_sed_nofire)), 
              alpha=0.5) + 
    geom_hline(yintercept=max(peaks$peak_sed_nofire), linetype="dashed")



  ggplot(peaks, aes(x=as.numeric(percent), y=peak_nit_fire, color=severity)) + 
    geom_point() + geom_line() + 
    geom_hline(yintercept=max(peaks$peak_nit_nofire), linetype="dashed")
  
  ggplot() + 
    geom_point(peaks, mapping=aes(x=as.numeric(percent), y=peak_nit_fire, color=severity)) + geom_line() + 
    geom_rect(mapping=aes(xmin=10, xmax=100,
                          ymin=min(peaks_full$peak_nit_nofire),
                          ymax=max(peaks_full$peak_nit_nofire)), 
              alpha=0.5) + 
    geom_hline(yintercept=max(peaks$peak_nit_nofire), linetype="dashed")
  
  ggplot(peaks, aes(x=as.numeric(percent), y=peak_doc_fire, color=severity)) + 
    geom_point() + geom_line() + 
    geom_hline(yintercept=max(peaks$peak_doc_nofire), linetype="dashed")
  
  ggplot() + 
    geom_point(peaks, mapping=aes(x=as.numeric(percent), y=peak_doc_fire, color=severity)) + geom_line() + 
    geom_rect(mapping=aes(xmin=10, xmax=100,
                          ymin=min(peaks_full$peak_doc_nofire),
                          ymax=max(peaks_full$peak_doc_nofire)), 
              alpha=0.5) + 
    geom_hline(yintercept=max(peaks$peak_doc_nofire), linetype="dashed")
  

ggplot(peaks, aes(x=as.numeric(percent), y=peak_flow_fire, color=severity)) + 
  geom_point() + geom_line() +  geom_hline(yintercept=max(peaks$peak_flow_nofire), linetype="dashed")

ggplot() + 
  geom_point(peaks, mapping=aes(x=as.numeric(percent), y=peak_flow_fire, color=severity)) + geom_line() + 
  geom_rect(mapping=aes(xmin=10, xmax=100,
                        ymin=min(peaks_full$peak_flow_nofire),
                        ymax=max(peaks_full$peak_flow_nofire)), 
            alpha=0.5) + 
  geom_hline(yintercept=max(peaks$peak_flow_nofire), linetype="dashed")



