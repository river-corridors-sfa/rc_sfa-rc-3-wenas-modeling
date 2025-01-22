### script to generate the plots used in the thresholds modeling paper (excluding map which was made on OSU desktop) 
  #written by Katie A. Wampler on 2024-02-14 

#section 0: load libraries and functions ------- 
  library(ggplot2) #for plotting
  library(dplyr) #for dealing with data
  library(thorloki) #for theme_pub
  library(cowplot) #for saving plots
  library(lubridate) #for dealing with dates 
  library(readr) #for loading large datasets
  library(stats) #for regression fitting
  library(waterData) #for getting USGS flow data
  library(dataRetrieval) #for identifying gauges

  #calculate richard baker flashiness index (Baker et al. 2004)
  rb_flashiness <- function(timeseries, solute){
    df <- timeseries %>% select(dates, any_of(solute))
    
    #replace NA's with 0 
    df[is.na(df[,2]) == T,2] <- 0
    vals <- unlist(as.vector(df[-nrow(df),2]))
    df$shift <- c(NA, vals)
    df$dif <- abs(df[,2]- df[,3])
    rb_index <- sum(df$dif, na.rm = T) / sum(df[,2])
    
    return(rb_index)
  } 
  
  #rename severity names 
  update_severity <- function(column){
    column[column == "UNBURNED"] <- "Unburned"
    column[column == "LOW"] <- "Low"
    column[column == "MOD"] <- "Moderate"
    column[column == "HIGH"] <- "High"
   return(column) 
  }

#section 1: load data (L1 data and L2 annual yields for full time period)------- 
  #section 1.1: get the outputs data 
    load("C:/Users/russ143/OneDrive - PNNL/Documents/1_Research/4_Wenas_Thresholds/clean data/2023-10-05_output_datset_L1.rda")
    outputs_final$severity <- update_severity(outputs_final$severity)
  
  #section 1.2: load annual yields 
    yields <- read.csv("C:/Users/russ143/OneDrive - PNNL/Documents/1_Research/4_Wenas_Thresholds/clean data/2024-02-08_L2_annual_yields.csv") 
    yields$severity <- update_severity(yields$severity)
    yields$severity <- factor(yields$severity, levels=c("Low", "Moderate", "High"), ordered=T) 
    
    yields_full <- read.csv("C:/Users/russ143/OneDrive - PNNL/Documents/1_Research/4_Wenas_Thresholds/clean data/2024-02-15_L2_30years_annual_yields.csv")
    yields_full$severity <- update_severity(yields_full$severity)
    yields_full$severity <- factor(yields_full$severity, levels=c("Low", "Moderate", "High"), ordered=T) 
  
  #section 1.3: load level 1 data with 30 pre-fire years
    #format to "fire-year"
    df_full <- read_csv("C:/Users/russ143/OneDrive - PNNL/Documents/1_Research/4_Wenas_Thresholds/clean data/2024-02-15_output_dataset_30years_L1.csv")
    df_full$year <- year(df_full$dates)
    df_full$day <- day(df_full$dates)
    df_full$month <- month(df_full$dates)
    df_full$month_day <- 
      as.Date(paste("1900", df_full$month, df_full$day, sep="-"))
    pr_wy <- which(df_full$month_day < as.Date("1900-08-31"))
    df_full$year[pr_wy] <- df_full$year[pr_wy] - 1 
    df_full$severity <- update_severity(df_full$severity)
    
#section 2: FIG2, make partial dependence like plot similar to Wampler et al. 2023 ------ 
  #section 2.1: first get percent change from base scenario 
    nofire_yield <- yields$flow_mm_yr_nofire[1] #get first since they're all the same 
    
    yields$flow_change <- (yields$flow_mm_yr_fire - nofire_yield) /nofire_yield * 100 
    
  #section 2.2: plot as a function of percent burned 
    p2 <- ggplot(yields, aes(x=percent, y=flow_change, color=severity)) + geom_line(size=1) + 
      geom_point(size=4, alpha=0.9) + 
      theme_pub() + theme(axis.title = element_text(face="bold")) + 
      theme(legend.position = "bottom") + scale_color_manual(values=c("#3B9AB2",  "#E1AF00", "#F21A00")) + 
      labs(x="Area Burned (%)", y="Change in Annual Water Yield (%)", 
           color="Burn Severity") 
    
    png("C:/Users/russ143/OneDrive - PNNL/Documents/1_Research/4_Wenas_Thresholds/figures/FIG2_flow_pd_plot.png",
        res=300, units="cm", width=25, height=16)
    plot(p2)
    dev.off()
     
#section 3: FIG3, show inner 90% of pre-fire scenarios to compare to scenarios ------ 
  #section 3.1: add unburned scenario as a 0% burned 
    yields_base <- data_frame(scenario="0_BASE",  flow_mm_yr_fire=yields$flow_mm_yr_nofire[1],flow_mm_yr_nofire=yields$flow_mm_yr_nofire[1], 
                              sed_kg_yr_fire=yields$sed_kg_yr_nofire[1], sed_kg_yr_nofire=yields$sed_kg_yr_nofire[1], 
                              nit_kg_yr_fire=yields$nit_kg_yr_nofire[1], nit_kg_yr_nofire=yields$nit_kg_yr_nofire[1], 
                              doc_kg_yr_fire=yields$doc_kg_yr_nofire[1], doc_kg_yr_nofire=yields$doc_kg_yr_nofire[1],
                              percent=0, severity=c("Unburned", "Low","Moderate", "High"))
    yields <- rbind(yields, yields_base)
    
    yields$severity <- factor(yields$severity, levels=c("Unburned", "Low", "Moderate", "High"), ordered=T)
    
  #section 3.2: get inner 90% for nitrate and DOC 
    nit_unburned <- unique(yields_full$nit_kg_yr_nofire) #lots of repeats across scenarios
    nit_unburned_quant <- quantile(nit_unburned, c(0.05,0.5, 0.95)) #get lower 5%, median, and upper 95%
    
    doc_unburned <- unique(yields_full$doc_kg_yr_nofire)
    doc_unburned_quant <- quantile(doc_unburned, c(0.05,0.5,0.95)) #get lower 5%, median, and upper 95%
    
    thresh <- data.frame(solute=c("Dissolved Organic Carbon","Nitrate"),
                         per5 =c(doc_unburned_quant[1], nit_unburned_quant[1]),
                         median=c(doc_unburned_quant[2], nit_unburned_quant[2]),
                         per95=c(doc_unburned_quant[3], nit_unburned_quant[3]))
  #section 3.3: make regression models 
    #make data easier to model
      yields_long <- yields %>% pivot_longer(c(nit_kg_yr_fire,doc_kg_yr_fire),
                                             names_to="solute", values_to = "load") %>%
        select(severity, percent, solute, load) 
      
      yields_long$severity <- factor(yields_long$severity, levels=c("Unburned", "Low","Moderate","High"), ordered=T)
      yields_long$solute[yields_long$solute == "doc_kg_yr_fire"] <- "Dissolved Organic Carbon"
      yields_long$solute[yields_long$solute == "nit_kg_yr_fire"] <- "Nitrate" 
      
    #try different models 
    best_mod <- function(analyte, sev, threshold){
      df <- filter(yields_long, solute == analyte & severity==sev)
      #remove 0 point for testing DOC
      #df <- filter(df, percent >0)
      m1 <- lm(load ~ percent, df) #linear 
      m2 <- lm(load ~ I(percent^3), df)
      m3 <- lm(load ~ percent + I(percent^2), df)
      m4 <- lm(load ~ exp(percent), df)
      m5 <- lm(load ~ I(percent^2)+ I(percent^3), df)
      m6 <- lm(load ~ I(percent^4), df)
      m7 <- lm(load ~ percent + I(percent^2)+ I(percent^3), df)
      models <- list(m1,m2,m3,m4,m5,m6,m7)
      AICs <- AIC(m1,m2,m3,m4,m5,m6,m7) #only works if response is the same
      #print(AICs)
      best <- which(AICs$AIC == min(AICs$AIC))
      #r2_fit <- vector()
      #for(x in 1:length(models)){
      #  r2 <- summary(models[[x]])$r.squared
      #  r2_fit <- c(r2_fit, r2)
      #} 
      #best <- which(r2_fit == max(r2_fit))
      
      r2 <- summary(models[[best]])$r.squared
      p_val <- anova(models[[best]])$'Pr(>F)'[1]
      F_stat <- summary(models[[best]])$fstatistic[1]
      mods <- c("linear","x3","x+x2", "exp(x)", "x2+x3","x4", "cubic")
      
      predictions <- data.frame(severity=sev, solute=analyte, 
                                percent=0:100)
      predictions$fit_val <- predict(models[[best]], predictions)
      
      #if(best == 7){predictions$fit_val <- exp(predictions$fit_val)}
      
      thresh_data <- data.frame(severity=sev, solute=analyte, 
                                percent=seq(0,100, by=0.1))
      thresh_data$fit_val <- predict(models[[best]], thresh_data)
      #if(best == 7){thresh_data$fit_val <- exp(thresh_data$fit_val)}
      
      per_thresh <- thresh_data$percent[min(which(thresh_data$fit_val > threshold))]
      
      return(list(c(analyte, sev, mods[best], r2, F_stat, p_val), predictions, per_thresh))}
  
  #section 3.4: get model data
    fits <- as.data.frame(rbind(best_mod("Dissolved Organic Carbon", "Low", thresh$per95[1])[[1]],
                                best_mod("Dissolved Organic Carbon", "Moderate", thresh$per95[1])[[1]],
                                best_mod("Dissolved Organic Carbon", "High", thresh$per95[1])[[1]],
                                best_mod("Nitrate", "Low", thresh$per95[2])[[1]],
                                best_mod("Nitrate", "Moderate", thresh$per95[2])[[1]],
                                best_mod("Nitrate", "High", thresh$per95[2])[[1]]))
    colnames(fits) <- c("Analyte","Severity","Best_Form","R2","F-Stat","p-val")
    
    best_fits <- rbind(rbind(best_mod("Dissolved Organic Carbon", "Low", thresh$per95[1])[[2]],
                             best_mod("Dissolved Organic Carbon", "Moderate", thresh$per95[1])[[2]],
                             best_mod("Dissolved Organic Carbon", "High", thresh$per95[1])[[2]],
                             best_mod("Nitrate", "Low", thresh$per95[2])[[2]],
                             best_mod("Nitrate", "Moderate", thresh$per95[2])[[2]],
                             best_mod("Nitrate", "High", thresh$per95[2])[[2]])) 
    
    #section 3.5: plot data
    yields_long$solute <- factor(yields_long$solute, levels=c("Nitrate", "Dissolved Organic Carbon"), ordered=T)
    best_fits$solute <- factor(best_fits$solute, levels=c("Nitrate", "Dissolved Organic Carbon"), ordered=T)
    thresh$solute <- factor(thresh$solute, levels=c("Nitrate", "Dissolved Organic Carbon"), ordered=T)
    
    yields_plot <- subset(yields_long, !(yields_long$percent == 0 & yields_long$severity != "Unburned"))
    
    p4 <- ggplot() + 
      geom_point(yields_plot, mapping=aes(x=percent, y=load/10000, color=severity), size=4, alpha=0.9) +
      geom_line(best_fits, mapping=aes(x=percent, y=fit_val/10000, color=severity)) + 
      scale_color_manual(values=c("darkgreen", "#3B9AB2",  "#E1AF00", "#F21A00")) + 
      labs(x="Area Burned (%)", y=expression(bold(paste("Load (",10^4," kg ", yr^-1,")"))), 
           color="Burn Severity") + theme_pub() + theme(legend.position = "bottom") + 
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      facet_wrap(~solute, scales="free_y", ncol=1) +
      geom_rect(thresh, mapping=aes(xmin=-Inf, xmax=Inf, ymin=per5/10000, 
                                    ymax=per95/10000), alpha=0.4, fill="gray40") +
      geom_hline(thresh, mapping=aes(yintercept=median/10000), linetype="dashed") + 
      theme( strip.background = element_rect(color="black", fill="gray80", linetype=1, linewidth = 1) ,
             strip.text.x = element_text(margin = margin(0.25,0,0.25,0, "cm")))
    
    png("C:/Users/russ143/OneDrive - PNNL/Documents/1_Research/4_Wenas_Thresholds/figures/FIG3_wqthresholds_30y_bestfitlines.png",
        res=300, units="cm", width=25, height=30)
    plot_grid(p4, ncol=1, label_size = 24)
    dev.off()    
    
#section 4: TABLE 1, Get threshold values ------ 
  #section 4.1: get thresholds from models determined in section 3 
    threshold_vals <- data.frame(analyte=rep(c("Dissolved Organic Carbon", "Nitrate"), each=3), 
                                 severity=rep(c("Low","Moderate","High"), times=2), 
                                 threshold=c(best_mod("Dissolved Organic Carbon", "Low", thresh$per95[1])[[3]],
                                             best_mod("Dissolved Organic Carbon", "Moderate", thresh$per95[1])[[3]],
                                             best_mod("Dissolved Organic Carbon", "High", thresh$per95[1])[[3]],
                                             best_mod("Nitrate", "Low", thresh$per95[2])[[3]],
                                             best_mod("Nitrate", "Moderate", thresh$per95[2])[[3]],
                                             best_mod("Nitrate", "High", thresh$per95[2])[[3]]))
    
  #section 4.2: format nicer 
    thresholds <- pivot_wider(threshold_vals, names_from=analyte, values_from=threshold)
  write.csv(thresholds, "C:/Users/russ143/OneDrive - PNNL/Documents/1_Research/4_Wenas_Thresholds/figures/threshold_table.csv", row.names=F)
   
#section 5: FIG4, flashiness index ----- 
 #section 5.1: get indices for year scenario 
    df <- filter(outputs_final, scenario_group == "Area Burned x Severity")
    
    for(x in unique(df$scenario)){
      sub_df <- filter(df, scenario == x)
      doc_index <- rb_flashiness(sub_df, "doc_mg_l_fire")
      nit_index <- rb_flashiness(sub_df, "nit_mg_l_fire")
      if(x == unique(df$scenario)[1]){
        doc_index_ub <- rb_flashiness(sub_df, "doc_mg_l_nofire")
        nit_index_ub <- rb_flashiness(sub_df, "nit_mg_l_nofire")
        indices <- data.frame(scenario="unburned", severity="Unburned", percent=0,
                              doc=doc_index_ub, 
                              nit=nit_index_ub)}
      index <- data.frame(scenario=x, severity=sub_df$severity[1],
                          percent=sub_df$percent[1], doc=doc_index, 
                          nit=nit_index)
      indices <- rbind(indices, index)
    } 
  
  #section 5.2: get indices for 30 unburned years 
    unburned <- filter(df_full, scenario == "100_MOD") #get a single set of unburned years
    
    #get complete years 
    year_check <- unburned %>% group_by(year) %>% summarise(count=n())
    good_years <- year_check$year[year_check$count >= 365]
    
    #get index for doc and nitrate
    for(x in good_years){
      sub_df <- filter(unburned, year == x)
      doc_index <- rb_flashiness(sub_df, "doc_mg_l_nofire")
      nit_index <- rb_flashiness(sub_df, "nit_mg_l_nofire")
      if(x == good_years[1]){
        indices_ub <- data.frame(year=x, doc=doc_index, 
                                 nit=nit_index)
      }
      index <- data.frame(year=x, doc=doc_index, 
                          nit=nit_index)
      indices_ub <- rbind(indices_ub, index)
    }
  
  #section 5.3: make data long to plot as facets and get sd of unburned for error bars on unburned 
    #make long for facet plotting 
    indices_long <- indices %>% pivot_longer(doc:nit)
    
    #add error for error bars
    indices_long$error <- NA 
    indices_long$error[indices_long$severity == "Unburned" & indices_long$name == "doc"] <- sd(indices_ub$doc)
    indices_long$error[indices_long$severity == "Unburned" & indices_long$name == "nit"] <- sd(indices_ub$nit)
    
    #plot
    indices_long$severity <- factor(indices_long$severity, levels=c("Unburned", "Low","Moderate","High"), ordered=T)
    indices_long$name[indices_long$name == "doc"] <- "Dissolved Organic Carbon"
    indices_long$name[indices_long$name == "nit"] <- "Nitrate" 
    indices_long$name <- factor(indices_long$name, levels=c("Nitrate", "Dissolved Organic Carbon"))
    
  #section 5.4: plot indices 
    p1 <- ggplot(indices_long, aes(x=as.numeric(percent), y=value, color=severity)) + 
      geom_point(size=4, alpha=0.9)+ geom_line()+
      labs(x="Area Burned (%)", y="Richard-Baker Flashiness Index", 
           color="Burn Severity") + theme_pub() +
      scale_color_manual(values=c("darkgreen", "#3B9AB2",  "#E1AF00", "#F21A00")) + 
      geom_errorbar(aes(ymin=value-error, ymax=value+error)) + 
      facet_wrap(~name, scales="free_y") + theme(legend.position="bottom") + 
      theme( strip.background = element_rect(color="black", fill="gray80", linetype=1, linewidth = 1) ,
             strip.text.x = element_text(margin = margin(0.25,0,0.25,0, "cm")))
    
    png("C:/Users/russ143/OneDrive - PNNL/Documents/1_Research/4_Wenas_Thresholds/figures/FIG4_flashiness.png",
        res=300, units="cm", width=35, height=20)
    plot_grid(p1, ncol=1, label_size = 24)
    dev.off()  
    
    
#section 6: FIGA2, thresholds of flow ------ 
  #section 6.1: get inner 90% for flow to prove our scenario for precip was normal
  flow_unburned <- unique(yields_full$flow_mm_yr_nofire) #lots of repeats across scenarios
  flow_unburned_quant <- quantile(flow_unburned, c(0.05,0.5, 0.95)) #get lower 5%, median, and upper 95%
 
  #section 6.2: make plots 
  p6 <- ggplot() + 
    geom_bar(yields, mapping=aes(x=percent, y=as.numeric(flow_mm_yr_fire), fill=severity),
             stat="identity",position="dodge") +
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=flow_unburned_quant[1], 
             ymax=flow_unburned_quant[3], alpha=0.4, fill="gray40") +
    geom_hline(yintercept=flow_unburned_quant[2], linetype="dashed") +
    scale_fill_manual(values=c("darkgreen", "#3B9AB2",  "#E1AF00", "#F21A00")) + 
    labs(x="Area Burned (%)", y=expression(bold(paste("Annual Water Yield ("," mm ", yr^-1,")"))), 
         fill="Burn Severity") + theme_pub() + theme(legend.position = "bottom") 
  
  png("C:/Users/russ143/OneDrive - PNNL/Documents/1_Research/4_Wenas_Thresholds/figures/FIGA1_flowthresholds.png",
      res=300, units="cm", width=25, height=16)
  plot_grid(p6, ncol=1, label_size = 24)
  dev.off()  
   
#section 7: FIGA3, annual water yields for USGS gauges ----- 
  #section 7.1: find stations 
    stats <- whatNWISdata(parameterCd="00060", bBox=c(-120.918811, 46.616579, -120.449965,  47.346106)) 
    stats <- subset(stats, stats$count_nu > 365) # need at least a year of data  
    stations <- unique(stats$site_no) 
    
    #get basin area of stations
    stat_data <- readNWISsite(stations)
    stat_data$area_m2 <- stat_data$drain_area_va * 2.59e+6
    
  #section 7.2: download data and convert units
    for(s in stations){
      flow_data <- importDVs(s)
      flow_fill <- fillMiss(flow_data, block = 30)
      
      area <- unique(stat_data$area_m2[stat_data$site_no == s])
      
      #convert flow to m3/day 
      flow_fill$flow_m3s <- flow_fill$val * 2446.58 #to m3/day from cfs 
      flow_fill$flow_mm_day <- flow_fill$flow_m3s / area * 1000
      
      #get annual yield 
      flow_fill$year <- year(flow_fill$dates) 
      annual_yield <- flow_fill %>% group_by(year) %>% dplyr::summarise(annual_yield = sum(flow_mm_day, na.rm=T), 
                                                                        count=n(), 
                                                                        na_count=sum(is.na(flow_mm_day)))
      
      #remove uncomplete years 
      annual_yield <- filter(annual_yield, count >= 365)
      annual_yield$site <- s 
      
      if(s == stations[1]){
        all_yields <- annual_yield
      }else{all_yields <- rbind(all_yields, annual_yield)}
    }
    
  #section 7.3: summarise
    all_yields <- filter(all_yields, na_count == 0) #only years with no missing data
  
    check <- all_yields %>% group_by(site) %>% dplyr::summarise(mean=mean(annual_yield, na.rm = T))
    mean(all_yields$annual_yield, na.rm=T)
    sd(all_yields$annual_yield, na.rm=T)  
    
    nice_yields <- all_yields %>% select(site,year, annual_yield)
    write.csv(nice_yields, "C:/Users/russ143/OneDrive - PNNL/Documents/1_Research/4_Wenas_Thresholds/figures/annual_yields.csv", quote=F, row.names=F)  
    
    p <- ggplot(all_yields, aes(x=site, y=annual_yield, fill=site)) + geom_boxplot() + theme_pub() + 
      theme(legend.position = "none") + 
      labs(x="USGS Gauge Number", y=expression(bold(paste("Annual Water Yield (", mm, " ", yr^{-1}, ")")))) + 
      geom_hline(yintercept= mean(all_yields$annual_yield, na.rm=T), linetype="dashed") +
      theme(axis.text.x =element_text(size=14, angle=45, hjust=1))
    
    png("C:/Users/russ143/OneDrive - PNNL/Documents/1_Research/4_Wenas_Thresholds/figures/FIGA2_averageflow.png",
        res=300, units="cm", width=20, height=12)
    plot_grid(p, ncol=1, label_size = 24)
    dev.off()  
    
    