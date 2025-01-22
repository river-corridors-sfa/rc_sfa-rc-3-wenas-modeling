## script to get richards-baker flashiness index for our fire simulations 

#written by Katie A. Wampler on 2024-02-15 

#section 0: load libraries and functions ----- 
  library(dplyr)
  library(ggplot2)
  library(thorloki)
  library(data.table)

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

#section 1: load data and get indices------ 
  load("C:/Users/russ143/OneDrive - PNNL/Documents/1_Research/4_Wenas_Thresholds/clean data/2023-10-05_output_datset_L1.rda")
  df <- filter(outputs_final, scenario_group == "Area Burned x Severity")
  
  for(x in unique(df$scenario)){
    sub_df <- filter(df, scenario == x)
    doc_index <- rb_flashiness(sub_df, "doc_mg_l_fire")
    nit_index <- rb_flashiness(sub_df, "nit_mg_l_fire")
    if(x == unique(df$scenario)[1]){
      doc_index_ub <- rb_flashiness(sub_df, "doc_mg_l_nofire")
      nit_index_ub <- rb_flashiness(sub_df, "nit_mg_l_nofire")
      indices <- data.frame(scenario="unburned", severity="UNBURNED", percent=0,
                            doc=doc_index_ub, 
                            nit=nit_index_ub)}
    index <- data.frame(scenario=x, severity=sub_df$severity[1],
                        percent=sub_df$percent[1], doc=doc_index, 
                        nit=nit_index)
    indices <- rbind(indices, index)
  }

  #section 1.2: load full 30 year unburned dataset 
  df_full <- read_csv("C:/Users/russ143/OneDrive - PNNL/Documents/1_Research/4_Wenas_Thresholds/clean data/2024-02-15_output_dataset_30years_L1.csv")
  df_full$year <- year(df_full$dates)
  df_full$day <- day(df_full$dates)
  df_full$month <- month(df_full$dates)
  df_full$month_day <- 
    as.Date(paste("1900", df_full$month, df_full$day, sep="-"))
  pr_wy <- which(df_full$month_day < as.Date("1900-08-31"))
  df_full$year[pr_wy] <- df_full$year[pr_wy] - 1
  
  unburned <- filter(df_full, scenario == "100_MOD")
  
  #get complete years 
  year_check <- unburned %>% group_by(year) %>% summarise(count=n())
  good_years <- year_check$year[year_check$count >= 365]
  
  for(x in good_years){
    sub_df <- filter(unburned, year == x)
    doc_index <- rb_flashiness(sub_df, "doc_mg_l_fire")
    nit_index <- rb_flashiness(sub_df, "nit_mg_l_fire")
    if(x == good_years[1]){
      indices_ub <- data.frame(year=x, doc=doc_index, 
                          nit=nit_index)
     }
    index <- data.frame(year=x, doc=doc_index, 
                        nit=nit_index)
    indices_ub <- rbind(indices_ub, index)
  }

#section 2: plot ----- 
  #make long for facet plotting 
  indices_long <- indices %>% pivot_longer(doc:nit)
  
  #add error for error bars
  indices_long$error <- NA 
  indices_long$error[indices_long$severity == "UNBURNED" & indices_long$name == "doc"] <- sd(indices_ub$doc)
  indices_long$error[indices_long$severity == "UNBURNED" & indices_long$name == "nit"] <- sd(indices_ub$nit)
  
  #plot
  indices_long$severity <- factor(indices_long$severity, levels=c("UNBURNED", "LOW","MOD","HIGH"), ordered=T)
  indices_long$name[indices_long$name == "doc"] <- "Dissolved Organic Carbon"
  indices_long$name[indices_long$name == "nit"] <- "Nitrate"
  
  
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
  