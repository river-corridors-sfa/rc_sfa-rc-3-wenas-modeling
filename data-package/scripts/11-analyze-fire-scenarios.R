## analyze the wildfire scenarios output from run-fire-scenarios 
  #written by Katie A. Wampler on 10-25-2024 

#section 0: load libraries and functions -------
  library(tools)
  library(readr)
  library(stringr)
  library(data.table)
  library(pbapply)
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(tidyr)
  library(cowplot)
  

  #calculate richard baker flashiness index (Baker et al. 2004)
  rb_flashiness <- function(timeseries, solute){
    df <- timeseries %>% dplyr::select(dates, any_of(solute))
    
    #replace NA's with 0 
    #df[is.na(df[,2]) == T,2] <- 0
    vals <- unlist(as.vector(df[-nrow(df),2]))
    df$shift <- c(NA, vals)
    df$dif <- abs(df[,2]- df[,3])
    rb_index <- sum(df$dif, na.rm = T) / sum(df[,2])
    
    return(rb_index)
  } 

  #section 5.1: extract data of interest from output.rch file
  #' Clean flow data from SWAT 
  #' 
  #' function that reads the output.rch file from SWAT and extracts the columns of interest and
  #' converts them from loads to concentrations and adds dates
  #'
  #' @param file_name the name of the output.rch file
  #' @param start the starting date of the results
  #' @param end the ending date of the results
  #' @param n_sub the number of subbasins in the model
  #' @param outlet the subbasin number associated with the outlet of the basin 
  #' @param area the area of the entire basin in km2
  #' @param fire the date of the wildfire
  #'
  #'
  ## to do: make parallel
  clean_flow <- function(file_name, n_sub, start="2005-01-01", end="2018-12-31", outlet, area, fire){
    #load data
      reach <- fread(file_name, skip = 9)
      #reach <- read_table(file_name, col_names = FALSE, skip = 9)
      reach <- reach[,c(2,7,11,12,14,16,18,24,34)]
      colnames(reach) <- c("subbasin", "flow", "sed", "sed_con", "org_n", "org_p", 
                           "nitrate","othop","doc")
      
      dates <- seq(from=as.Date(start), to=as.Date(end), by="day")
      dates <- rep(dates, n_sub)
      reach$dates <- dates[order(dates)]
    
    #get concentrations
      reach$flow_L <- reach$flow * 60*60*24* 1000 #flow in L per day  
      reach$org_n_mgL <- reach$org_n * 1e6 / reach$flow_L 
      reach$org_p_mgL <- reach$org_p * 1e6 / reach$flow_L 
      reach$nitrate_mgL <- reach$nitrate * 1e6 / reach$flow_L 
      reach$othop_mgL <- reach$othop * 1e6 / reach$flow_L
      reach$doc_mgL <- reach$doc * 1e6 / reach$flow_L 
      reach$sed_conc_check <- reach$sed  * 1e6 / reach$flow_L  *1000 
    
    #add info about scenario 
      if(grepl("UNBURN", file_name)){
        reach$scenario <- str_split_i(file_name, "_", i=1)
        reach$sev <- str_split_i(file_name, "_", i=1) 
        reach$year <- str_split_i(file_name, "_", i=2) 
      }else{
        reach$scenario <- paste("PER", str_split_i(file_name, "_", i=2), sep="_")
        reach$sev <- str_split_i(file_name, "_", i=3) 
        reach$year <- str_split_i(file_name, "_", i=4) 
      }
    
    #write level 1 dataset (daily values)
      fwrite(reach, file.path("~/1_Research/4_Wenas_Thresholds/clean data/rch_files_lvl1/", paste(reach$scenario[1], reach$sev[1], reach$year[1],  "clean_rch.csv", sep="_")))
      #write_csv(reach, file.path("rch_files_lvl1/", paste(reach$scenario[1], reach$sev[1], reach$year[1],  "clean_rch.csv", sep="_"))) 
      
    #get annual values 
      #subset to post-fire year at outlet
        reach_fire <- reach[reach$dates >= as.Date(fire) & reach$dates < (as.Date(fire) + years(1)) & reach$subbasin == outlet,]
      
      #convert flow to mm/day 
        #seconds to day (m3/day)
        reach_fire$flow_mm_d <- reach_fire$flow  * 60*60*24 
        
        #divide by area (m/day)
        area_m2 <- area * 1000000
        reach_fire$flow_mm_d   <- reach_fire$flow_mm_d   / area_m2 
        
        #convert to mm/day 
        reach_fire$flow_mm_d <- reach_fire$flow_mm_d * 1000
        
      #get richard baker flashiness 
        rb_doc <- rb_flashiness(reach_fire, "doc_mgL")
        rb_nit <- rb_flashiness(reach_fire, "nitrate_mgL")
      
      #sum to mm/yr and kg/yr 
        annual <- reach_fire %>% group_by(scenario, sev, year) %>% 
          summarise(flow_mm_yr = sum(flow_mm_d), nitrate_kg_yr = sum(nitrate), doc_kg_yr = sum(doc), 
                    doc_rb = rb_doc, nitrate_rb = rb_nit, .groups = "drop")
      
      return(annual) #output so we can merge together and save
  }

  
  #' to get the fire year from a set of dates
  #'
  #' @param date_col a vector or dataframe column with dates to transform
  #' @param fire_date the date of the wildfire
  #'
  #' @return a vector of "wildfire" years
    fire_year <- function(date_col, fire_date){
      year <- lubridate::year(date_col)
      fire_date <- as.Date(fire_date)
      month_day <- as.Date(paste("1900",lubridate::month(date_col), lubridate::day(date_col), sep="-"))
      fire_month_day <- as.Date(paste("1900",lubridate::month(fire_date), lubridate::day(fire_date), sep="-"))
      prev_year <- which(month_day < fire_month_day)
      year[prev_year] <- year[prev_year] - 1
      return(year)
    } 
  
  #theme for nice consistent plotting
    theme_pub <- function (){
      ggthemes::theme_clean() %+replace% ggplot2::theme(plot.background = element_rect(fill = "white", 
        colour = "white"), legend.background = element_rect(fill = "white", 
            colour = "white"), axis.title = element_text(face = "bold", 
             size = 20), legend.title = element_text(face = "bold", 
            size = 20), axis.text = element_text(size = 16), legend.text = element_text(size = 16), 
         strip.text = element_text(size = 20))} 
    
#' function to test different model structures and report the best one, with the fit, best fit line data for plotting, and then the line crosses a threshold value
#'
#' @param data dataset (here likely annuals) with the data needs columns "sev", "perc" and the metric you want to fit
#' @param severity a burn severity group
#' @param metric the metric you want to fit best fit lines to
#' @param threshold a value for the upper threshold to determine when the line crosses the threshold
#'
#' @return
#' @export
#'
#' @examples
  best_mod <- function(data, severity, metric, threshold){
      df <- data %>% dplyr::select(any_of(c("sev", "real_per", metric))) %>% filter(sev %in% c(severity, "UNBURN")) %>% 
       group_by(real_per) %>% summarise(mean_val = mean(.data[[metric]]))
      
      m1 <- lm(mean_val ~ real_per, df) #linear 
      m2 <- lm(mean_val ~ I(real_per^3), df) #x3
      m3 <- lm(mean_val ~ real_per + I(real_per^2), df) #x + x2 
      m4 <- lm(mean_val ~ exp(real_per), df) #exp(x)
      m5 <- lm(mean_val ~ I(real_per^2)+ I(real_per^3), df) #x2 + x3
      m6 <- lm(mean_val ~ I(real_per^4), df) #x4
      m7 <- lm(mean_val ~ real_per + I(real_per^2)+ I(real_per^3), df) #cubic
      models <- list(m1,m2,m3,m4,m5,m6,m7)
      AICs <- AIC(m1,m2,m3,m4,m5,m6,m7) #only works if response is the same
      best <- which(AICs$AIC == min(AICs$AIC))
      
      r2 <- summary(models[[best]])$r.squared
      p_val <- anova(models[[best]])$'Pr(>F)'[1]
      F_stat <- summary(models[[best]])$fstatistic[1]
      mods <- c("linear","x3","x+x2", "exp(x)", "x2+x3","x4", "cubic")
      int <- summary(models[[best]])$coefficients[1,1]
      coeff1 <- summary(models[[best]])$coefficients[2,1]
      if(nrow(summary(models[[best]])$coefficients)==3){
        coeff2 <- summary(models[[best]])$coefficients[3,1]
      }else{coeff2 <- NA}
      if(nrow(summary(models[[best]])$coefficients)==4){
        coeff3 <- summary(models[[best]])$coefficients[4,1]
      }else{coeff3 <- NA}
      
      predictions <- data.frame(severity=severity, 
                                real_per=0:100)
      predictions$fit_val <- predict(models[[best]], predictions)
      

      thresh_data <- data.frame(severity=severity, 
                                real_per=seq(0,100, by=0.1))
      thresh_data$fit_val <- predict(models[[best]], thresh_data)

      per_thresh <- thresh_data$real_per[min(which(thresh_data$fit_val > threshold))]
      
      return(list(c(severity, mods[best], r2, F_stat, p_val, int, coeff1, coeff2, coeff3), predictions, per_thresh))}

  
  
#section 1: load and tidy data and get annual loads / flow depth  ------- 
  #section 2.1: american  
    #specify data info for running clean_flow function
      setwd("~/1_Research/4_Wenas_Thresholds/data/american_scenario_outputs") #location of the data
      outlet <- 1 #subbasin number for the outlet
      area <- 206.834425 #km2
      fire <- "2017-08-11" #date of fire
    
    #get file names 
      files <- list.files()
      rch <- files[file_ext(files) == "rch"]

    #run cleaning script (this can take an hour or so)
      annuals <- bind_rows(pblapply(rch, clean_flow, n_sub=21, outlet=outlet, area=area, fire=fire))
      
      annuals <- annuals[annuals$year != "1987",] #ran an extra year for the american, remove that to get 30 years
    
    #load precip data 
      n_stats <- 10 #number of precip stations
      setwd("C:/SWAT/American River Simp2/American River Simp2/Scenarios/American River Simp2.Sufi2.SwatCup/Backup/")
      pcp <- read.table("pcp1.pcp", skip=4)
      header <- read_lines("pcp1.pcp", n_max = 4)
      col <- c(7, rep(5,n_stats)) #seven precip stations used
      names(col) <- c("date", paste0("pcp", 1:n_stats))
      pcp <- pcp %>% separate_wider_position(V1, widths=col)
      pcp$date <- as.Date(pcp$date, format="%Y%j") 
      pcp <-  pcp %>% mutate_if(is.character, as.numeric) #ensure values are numeric 
      pcp$avg <- rowMeans(pcp[,-1]) #get average across all sites
      pcp$fire_year <- fire_year(pcp$date, fire) #get fire year
      
      annual_precip <- pcp %>% group_by(fire_year) %>% summarise(precip_mm = sum(avg))
      
      annuals <- merge(annuals, annual_precip, by.x="year", by.y="fire_year")
      write_csv(annuals, "~/1_Research/4_Wenas_Thresholds/clean data/rch_files_lvl2/annual_loads_rch_american.csv") 
      
      #if already run, load file so you don't have to clean data again 
      annuals <- read_csv("~/1_Research/4_Wenas_Thresholds/clean data/rch_files_lvl2/annual_loads_rch_american.csv")
      annuals$perc <- ifelse(annuals$scenario == "UNBURN", 0, as.numeric(str_split_i(annuals$scenario, "_", i=2)))
      annuals$sev <- factor(annuals$sev, levels=c("UNBURN", "LOW", "MOD", "HIGH"), ordered=T)
      
    #load burn scenarios 
      hru_burn <- read.csv("~/1_Research/4_Wenas_Thresholds/data/American River/wild_fire files/hru_burn_scenarios.csv")
      hru_burn <- subset(hru_burn, hru_burn$sev == "burned")
      
      burn_scenarios <- hru_burn %>% group_by(scenario) %>% summarise(area_ha = sum(area_ha)) %>% mutate(real_per = area_ha / (area *100)*100 )
      
      annuals <- annuals %>% left_join(burn_scenarios, by=c("perc" = "scenario")) 
      annuals$real_per[annuals$perc == 0] <- 0
  
    #section 2.2: tule 
      #specify data info for running clean_flow function
      setwd("~/1_Research/4_Wenas_Thresholds/data/tule_scenario_outputs")
      outlet <- 1 
      area <- 249.9807#km2
      fire <- "2017-08-11"
      
      #get file names 
      files <- list.files()
      rch <- files[file_ext(files) == "rch"]
      #rch <- rch[c(869:899, 32:62,218:248, 1861:1891)]
      
      #run cleaning script (this can take a bit)
      annuals <- bind_rows(pblapply(rch, clean_flow, n_sub=21, outlet=outlet, area=area, fire=fire))
      
      annuals <- annuals[annuals$year != "1987",] #ran an extra year for the american, remove that to get 30 years
      
      #load precip data 
      n_stats <- 10 #number of precip stations
      setwd("C:/SWAT/tule River Simp2/tule River Simp2/Scenarios/tule River Simp2.Sufi2.SwatCup/Backup/")
      pcp <- read.table("pcp1.pcp", skip=4)
      header <- read_lines("pcp1.pcp", n_max = 4)
      col <- c(7, rep(5,n_stats)) #seven precip stations used
      names(col) <- c("date", paste0("pcp", 1:n_stats))
      pcp <- pcp %>% separate_wider_position(V1, widths=col)
      pcp$date <- as.Date(pcp$date, format="%Y%j") 
      pcp <-  pcp %>% mutate_if(is.character, as.numeric) #ensure values are numeric 
      pcp$avg <- rowMeans(pcp[,-1]) #get average across all sites
      pcp$fire_year <- fire_year(pcp$date, fire) #get fire year
      
      annual_precip <- pcp %>% group_by(fire_year) %>% summarise(precip_mm = sum(avg))
      
      annuals <- merge(annuals, annual_precip, by.x="year", by.y="fire_year")
      write_csv(annuals, "~/1_Research/4_Wenas_Thresholds/clean data/rch_files_lvl2/annual_loads_rch_tule.csv") 
      
      #if already run, load file so you don't have to clean data again 
      annuals <- read_csv("~/1_Research/4_Wenas_Thresholds/clean data/rch_files_lvl2/annual_loads_rch_tule.csv")
      annuals$perc <- ifelse(annuals$scenario == "UNBURN", 0, as.numeric(str_split_i(annuals$scenario, "_", i=2)))
      annuals$sev <- factor(annuals$sev, levels=c("UNBURN", "LOW", "MOD", "HIGH"), ordered=T)
      
      #load burn scenarios 
      hru_burn <- read.csv("~/1_Research/4_Wenas_Thresholds/data/tule_River/wild_fire files/hru_burn_scenarios.csv")
      hru_burn <- subset(hru_burn, hru_burn$sev == "burned")
      
      burn_scenarios <- hru_burn %>% group_by(scenario) %>% summarise(area_ha = sum(area_ha)) %>% mutate(real_per = area_ha / (area *100)*100 )
      
      annuals <- annuals %>% left_join(burn_scenarios, by=c("perc" = "scenario")) 
      annuals$real_per[annuals$perc == 0] <- 0
      
#section 2: figure 1: map of the two basins with landuse/dem --------
#section 3: figure 2: annual water yields and runoff ratios as a function of burn severity and area burned -------- 
    #plot average water yields 
      #get average for each scenario set with uncertainty (average across years)
      flow_plot <- annuals %>% group_by(real_per, sev) %>%  
        summarise(mean = mean(flow_mm_yr),sd = sd(flow_mm_yr), n  = n()) %>%
        mutate(se= sd / sqrt(n),
               lower.ci = mean- qt(1 - (0.05 / 2), n - 1) * se,
               upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
      
      #get quantiles for unburned
      thresh <- subset(annuals, annuals$sev == "UNBURN") 
      thresh <-  quantile(thresh$flow_mm_yr, c(0.05,0.5,0.95))
      
      #determine best fit lines 
      for(x in c("LOW", "MOD", "HIGH")){
        output <- best_mod(annuals, x, "flow_mm_yr", thresh[3])
        if(x == "LOW"){
          best_fit <- output[[2]]
          thresh_cross <- output[[3]]
        }else{
          best_fit <- rbind(best_fit, output[[2]])
          thresh_cross <- c(thresh_cross,output[[3]])
        }
      }
      
      #make plot (still need best fit lines)
     p1 <- ggplot() + 
        geom_point(flow_plot, mapping=aes(x=real_per, y=mean, color=sev), size=4, alpha=0.9) +
        scale_color_manual(values=c("darkgreen", "#3B9AB2",  "#E1AF00", "#F21A00"),labels = c("Unburned", "Low", "Moderate", "High")) + 
        labs(x="Area Burned (%)", y=expression(bold(paste("Annual Water Yield (", mm, " ", yr^{-1}, ")"))), 
             color="Burn Severity") + theme_pub() + theme(legend.position = "bottom") + 
        theme(axis.title = element_text(size=18)) + 
       geom_line(best_fit, mapping=aes(x=real_per, y=fit_val, color=severity))
      
    #plot average runoff ratios
      #get runoff ratios 
      annuals$rr <- annuals$flow_mm_yr / annuals$precip_mm 
      
      RR_plot <- annuals %>% group_by(real_per, sev) %>%  
        summarise(mean = mean(rr),sd = sd(rr), n  = n()) %>%
        mutate(se= sd / sqrt(n),
               lower.ci = mean- qt(1 - (0.05 / 2), n - 1) * se,
               upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

      #get quantiles for unburned
      thresh <- subset(annuals, annuals$sev == "UNBURN") 
      thresh <-  quantile(thresh$rr, c(0.05,0.5,0.95))
      
      #determine best fit lines 
      for(x in c("LOW", "MOD", "HIGH")){
        output <- best_mod(annuals, x, "rr", thresh[3])
        if(x == "LOW"){
          best_fit <- output[[2]]
          thresh_cross <- output[[3]]
        }else{
          best_fit <- rbind(best_fit, output[[2]])
          thresh_cross <- c(thresh_cross,output[[3]])
        }
      }
      
      p2 <- ggplot() + 
        geom_point(RR_plot, mapping=aes(x=real_per, y=mean, color=sev), size=4, alpha=0.9) +
        geom_errorbar(data=RR_plot, aes(x=real_per, ymin=lower.ci, ymax=upper.ci, color=sev), width=0, size=1) + 
        scale_color_manual(values=c("darkgreen", "#3B9AB2",  "#E1AF00", "#F21A00"), labels = c("Unburned", "Low", "Moderate", "High")) + 
        labs(x="Area Burned (%)", y="Annual Runoff Ratio", 
             color="Burn Severity") + theme_pub() + theme(legend.position = "bottom") + 
        theme(axis.title = element_text(size=18)) + 
        geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=thresh[1], 
                                      ymax=thresh[3]), alpha=0.4, fill="gray40") +
        geom_hline( mapping=aes(yintercept=thresh[2]), linetype="dashed") + 
        theme(axis.title = element_text(size=18)) + 
        geom_line(best_fit, mapping=aes(x=real_per, y=fit_val, color=severity))
        
      
      png("~/1_Research/4_Wenas_Thresholds/figures/pub figures/RR_thresholds.png",
          res=300, units="cm", width=25, height=40)
      plot_grid(p1, p2, ncol=1, labels="auto", label_size = 30)
      dev.off()    
      
#section 4: figure 3: nitrate and doc with thresholds ------- 
    #get mean and CI for nitrate and DOC
      annuals_long <- pivot_longer(annuals, cols=nitrate_kg_yr:doc_kg_yr, names_to="analyte", values_to="load_kg_yr" )
      
      WQ_plot <- annuals_long %>% group_by(real_per, sev, analyte) %>%  
        summarise(mean = mean(load_kg_yr),sd = sd(load_kg_yr), n  = n()) %>%
        mutate(se= sd / sqrt(n),
               lower.ci = mean- qt(1 - (0.05 / 2), n - 1) * se,
               upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
      
      #get quantiles for unburned
      thresh <- subset(annuals_long, annuals_long$sev == "UNBURN") 
      thresh <-  thresh %>% group_by(analyte) %>% summarise(l.95 = quantile(load_kg_yr, 0.05), 
                                                            median = quantile(load_kg_yr, 0.5), 
                                                            u.95 = quantile(load_kg_yr, 0.95))
      
      #determine best fit lines 
      for(y in unique(annuals_long$analyte)){
        data <- na.omit(subset(annuals_long, annuals_long$analyte == y))
        for(x in c("LOW", "MOD", "HIGH")){
          output <- best_mod(data, x, "load_kg_yr", thresh[3])
          fit <- output[[2]]
          fit$analyte <- y
          if(x == "LOW" & y == "nitrate_kg_yr"){
            best_fit <- fit
            thresh_cross <- output[[3]]
          }else{
            best_fit <- rbind(best_fit, fit)
            thresh_cross <- c(thresh_cross,output[[3]])
          }
        }
      }
     
      
      #nicer names for facets
      facet_labels <- c(
        "nitrate_kg_yr" = "Nitrate",
        "doc_kg_yr" = "Dissolved Organic Carbon")
      
      #make plot
      p2 <- ggplot() + 
        geom_point(WQ_plot, mapping=aes(x=real_per, y=mean/10000, color=sev), size=4, alpha=0.9) +
        geom_errorbar(data=WQ_plot, aes(x=real_per, ymin=lower.ci/10000, ymax=upper.ci/10000, color=sev), width=0, size=1) + 
        scale_color_manual(values=c("darkgreen", "#3B9AB2",  "#E1AF00", "#F21A00"), labels = c("Unburned", "Low", "Moderate", "High")) + 
        facet_wrap(~analyte, scale="free_y", labeller = as_labeller(facet_labels)) +
        labs(x="Area Burned (%)", y="Annual Load (10\u2074 kg)", 
             color="Burn Severity") + theme_pub() + theme(legend.position = "bottom") + 
        theme(axis.title = element_text(size=18)) + 
        geom_rect(data = thresh, mapping=aes(xmin=-Inf, xmax=Inf, ymin=l.95/10000, 
                              ymax=u.95/10000), alpha=0.4, fill="gray40") +
        geom_hline(data=thresh, mapping=aes(yintercept=median/10000), linetype="dashed") + 
        theme(axis.title = element_text(size=18)) + 
        geom_line(best_fit, mapping=aes(x=real_per, y=fit_val/10000, color=severity)) +
        theme(strip.background = element_rect("gray40")) +        
        theme(strip.text = element_text(margin = ggplot2:::margin(t = 2, r = 0, b =7, l = 0)))

      png("~/1_Research/4_Wenas_Thresholds/figures/pub figures/WQ_thresholds.png",
          res=300, units="cm", width=40, height=20)
      p2
      dev.off()    
      
#section 5: figure 4: Richard baker flashiness index ------- 
  #get rb flashiness for each scenario 
      #get mean and CI for nitrate and DOC
      annuals_long <- pivot_longer(annuals, cols=doc_rb:nitrate_rb, names_to="analyte", values_to="index" )
      
      WQ_plot <- annuals_long %>% group_by(real_per, sev, analyte) %>%  
        summarise(mean = mean(index),sd = sd(index), n  = n()) %>%
        mutate(se= sd / sqrt(n),
               lower.ci = mean- qt(1 - (0.05 / 2), n - 1) * se,
               upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
      
      #get quantiles for unburned
      thresh <- subset(annuals_long, annuals_long$sev == "UNBURN") 
      thresh <-  thresh %>% group_by(analyte) %>% summarise(l.95 = quantile(index, 0.05), 
                                                            median = quantile(index, 0.5), 
                                                            u.95 = quantile(index, 0.95))
      
      #determine best fit lines 
      for(y in unique(annuals_long$analyte)){
        data <- na.omit(subset(annuals_long, annuals_long$analyte == y))
        for(x in c("LOW", "MOD", "HIGH")){
          output <- best_mod(data, x, "index", thresh[3])
          fit <- output[[2]]
          fit$analyte <- y
          if(x == "LOW" & y == "doc_rb"){
            best_fit <- fit
            thresh_cross <- output[[3]]
          }else{
            best_fit <- rbind(best_fit, fit)
            thresh_cross <- c(thresh_cross,output[[3]])
          }
        }
      }
      
      
      #nicer names for facets
      facet_labels <- c(
        "nitrate_rb" = "Nitrate",
        "doc_rb" = "Dissolved Organic Carbon")
      
      #make plot
      p2 <- ggplot() + 
        geom_point(WQ_plot, mapping=aes(x=real_per, y=mean, color=sev), size=4, alpha=0.9) +
        geom_errorbar(data=WQ_plot, aes(x=real_per, ymin=lower.ci, ymax=upper.ci, color=sev), width=0, size=1) + 
        scale_color_manual(values=c("darkgreen", "#3B9AB2",  "#E1AF00", "#F21A00"), labels = c("Unburned", "Low", "Moderate", "High")) + 
        facet_wrap(~analyte, scale="free_y", labeller = as_labeller(facet_labels)) +
        labs(x="Area Burned (%)", y="Richard Baker Flashiness Index", 
             color="Burn Severity") + theme_pub() + theme(legend.position = "bottom") + 
        theme(axis.title = element_text(size=18)) + 
        geom_rect(data = thresh, mapping=aes(xmin=-Inf, xmax=Inf, ymin=l.95, 
                                             ymax=u.95), alpha=0.4, fill="gray40") +
        geom_hline(data=thresh, mapping=aes(yintercept=median), linetype="dashed") + 
        theme(axis.title = element_text(size=18)) + 
        geom_line(best_fit, mapping=aes(x=real_per, y=fit_val, color=severity)) +
        theme(strip.background = element_rect("gray40")) +        
        theme(strip.text = element_text(margin = ggplot2:::margin(t = 2, r = 0, b =7, l = 0)))
      
      png("~/1_Research/4_Wenas_Thresholds/figures/pub figures/flashiness_thresholds.png",
          res=300, units="cm", width=40, height=20)
      p2
      dev.off()    
      
