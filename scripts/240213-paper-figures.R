### script to generate the plots used in the thresholds modeling paper (excluding map which was made on OSU desktop) 
  #written by Katie A. Wampler on 2024-02-14 

#section 0: load libraries and functions ------- 
  library(ggplot2) #for plotting
  library(dplyr) #for dealing with data
  library(thorloki) #for theme_pub
  library(cowplot) #for saving plots
  library(lubridate) #for dealing with dates 

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
                              percent=0, severity="Unburned", flow_change=0)
    yields <- rbind(yields, yields_base)

    yields$percent <- factor(yields$percent, levels=c(0,10,15,20,25,30,40,45,50,60,75,90,100), ordered=T) 
    yields$severity <- factor(yields$severity, levels=c("Unburned", "Low", "Moderate", "High"), ordered=T)

  #section 3.2: get inner 90% for nitrate and DOC 
    nit_unburned <- unique(yields_full$nit_kg_yr_nofire) #lots of repeats across scenarios
    nit_unburned_quant <- quantile(nit_unburned, c(0.05,0.5, 0.95)) #get lower 5%, median, and upper 95%
    
    doc_unburned <- unique(yields_full$doc_kg_yr_nofire)
    doc_unburned_quant <- quantile(doc_unburned, c(0.05,0.5,0.95)) #get lower 5%, median, and upper 95%

  #section 3.3: make plots 
    p4 <- ggplot() + 
      geom_bar(yields, mapping=aes(x=percent, y=as.numeric(nit_kg_yr_fire)/10000, fill=severity),
               stat="identity",position="dodge") +
      annotate("rect", xmin=-Inf, xmax=Inf, ymin=nit_unburned_quant[1]/10000, 
               ymax=nit_unburned_quant[3]/10000, alpha=0.4, fill="gray40") +
      geom_hline(yintercept=nit_unburned_quant[2]/10000, linetype="dashed") +
      scale_fill_manual(values=c("darkgreen", "#3B9AB2",  "#E1AF00", "#F21A00")) + 
      labs(x="Area Burned (%)", y=expression(bold(paste("Nitrate Load (",10^4," kg ", yr^-1,")"))), 
           fill="Burn Severity") + theme_pub() + theme(legend.position = c(0.2,0.8)) + 
      theme(axis.title.y = element_text(size=14))
    
    p5 <- ggplot() + 
      geom_bar(yields, mapping=aes(x=percent, y=as.numeric(doc_kg_yr_fire)/10000, fill=severity),     stat="identity",position="dodge") +
      annotate("rect", xmin=-Inf, xmax=Inf, ymin=doc_unburned_quant[1]/10000, 
               ymax=doc_unburned_quant[3]/10000, alpha=0.4, fill="gray40") +
      geom_hline(yintercept=doc_unburned_quant[2]/10000, linetype="dashed") +
      scale_fill_manual(values=c("darkgreen","#3B9AB2",  "#E1AF00", "#F21A00")) + 
      labs(x="Area Burned (%)", y=expression(bold(paste("Dissolved Organic Carbon Load (",10^4," kg ", yr^-1,")"))), 
           fill="Burn Severity") + theme_pub() + theme(legend.position = c(0.2,0.8)) +
      theme(axis.title.y = element_text(size=14))
   
     png("C:/Users/russ143/OneDrive - PNNL/Documents/1_Research/4_Wenas_Thresholds/figures/FIG3_wqthresholds_30y.png",
        res=300, units="cm", width=25, height=30)
    plot_grid(p4, p5, labels="auto", ncol=1, label_size = 24)
    dev.off()  
#section 4: TABLE 1, Get threshold values ------ 
  for (x in c("Low", "Moderate", "High")){
     nit_thresh <- nit_unburned_quant[3]
     doc_thresh <- doc_unburned_quant[3]
     
     nit_per <- min(as.numeric(as.character(yields$percent[which(yields$nit_kg_yr_fire > nit_thresh & 
                                                                   yields$severity == x)])))
     
     doc_per <- min(as.numeric(as.character(yields$percent[which(yields$doc_kg_yr_fire > doc_thresh & 
                                                                   yields$severity == x)])))
     
     sub_thresh <- data.frame(severity=x, nitrate_threshold=nit_per, doc_threshold=doc_per)
     
     if(x == "Low"){
       thresholds <- sub_thresh
     }else{thresholds <- rbind(thresholds, sub_thresh)}
    
  }
  
  #correct for values that don't pass threshold
    thresholds$nitrate_threshold[is.infinite(thresholds$nitrate_threshold)==T] <- NA 
    thresholds$doc_threshold[is.infinite(thresholds$doc_threshold)==T] <- NA
    
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
    
    
#section 6: FIGA1, thresholds of flow ------ 
  #section 3.2: get inner 90% for flow to prove our scenario for precip was normal
  flow_unburned <- unique(yields_full$flow_mm_yr_nofire) #lots of repeats across scenarios
  flow_unburned_quant <- quantile(flow_unburned, c(0.05,0.5, 0.95)) #get lower 5%, median, and upper 95%
 
  #section 3.3: make plots 
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
  