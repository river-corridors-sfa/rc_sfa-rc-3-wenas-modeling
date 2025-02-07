## analyze the wildfire scenarios output from fire scenarios after cleaning with 11-clean-model-outputs 
  #written by Katie A. Wampler on 10-25-2024 

  fig_save_path <-  "~/1_Research/0_Misc/rc_sfa-rc-3-wenas-modeling/data-package/outputs/figures"
  data_save_path <-  "~/1_Research/0_Misc/rc_sfa-rc-3-wenas-modeling/data-package/outputs/summary-outputs"

#section 0: load libraries and functions -------
  library(tools)
  library(readr)
  library(stringr)
  library(data.table)
  library(lubridate)
  library(ggplot2)
  library(tidyr)
  library(cowplot)
  library(ggh4x)
  library(PNWColors)
  library(ggpubr)
  library(sf)
  library(raster) 
  library(patchwork)
  library(ggnewscale)
  library(dplyr)

  
  #theme for nice consistent plotting
  theme_pub <- function (){
    ggthemes::theme_clean() %+replace% ggplot2::theme(plot.background = element_rect(fill = "white", 
                                                                                     colour = "white"), 
                                                      legend.background = element_rect(fill = "white", 
                                                                                       colour = "white"), 
                                                      axis.title = element_text(face = "bold",
                                                                                size = 20), 
                                                      legend.title = element_text(face = "bold",
                                                                                  size = 20), 
                                                      axis.text = element_text(size = 16), 
                                                      legend.text = element_text(size = 16), 
                                                      strip.text = element_text(size = 20,margin = ggplot2:::margin(t = 7, r = 0, b =7, l = 0)),
                                                      strip.background = element_rect(fill="gray70"))} 

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
    basin <- data$basin[1]
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
    if(nrow(summary(models[[best]])$coefficients)>=3){
      coeff2 <- summary(models[[best]])$coefficients[3,1]
    }else{coeff2 <- NA}
    if(nrow(summary(models[[best]])$coefficients)==4){
      coeff3 <- summary(models[[best]])$coefficients[4,1]
    }else{coeff3 <- NA}
    
    predictions <- data.frame(severity=severity, 
                              real_per=0:100,
                              basin = basin)
    predictions$fit_val <- predict(models[[best]], predictions)
    
    
    thresh_data <- data.frame(severity=severity, 
                              real_per=seq(0,100, by=0.1),
                              basin=basin)
    thresh_data$fit_val <- predict(models[[best]], thresh_data)
    
    per_thresh <- thresh_data$real_per[min(which(thresh_data$fit_val > as.numeric(threshold)))]
    
    return(list(data.frame(basin=basin, sev=severity, model=mods[best], r2=r2, 
                           F_stat=F_stat, p_val=p_val, int=int, c1=coeff1, 
                           c2=coeff2, c3=coeff3), predictions, per_thresh))}

  #automates making threshold plots, making the limits for the two basins the same, and determining the thresholds
  threshold_plot <- function(data, metric, label, legend.pos=NULL){
    plot_data <- data %>% group_by(real_per, sev, basin) %>%  
      summarise(mean = mean(get(metric)),sd = sd(get(metric)), n  = n()) %>%
      mutate(se= sd / sqrt(n),
             lower.ci = mean- qt(1 - (0.05 / 2), n - 1) * se,
             upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
    
    
    #get quantiles for unburned
    #bootstrap to get more clear idea of distribution 
    thresh <- subset(data, data$sev == "UNBURN")
    set.seed(9)
    for(x in unique(thresh$basin)){
      subdata <- subset(thresh, thresh$basin == x)
      mean_lst <- list()
      B <- 1000
      N <- nrow(subdata)
      for(i in 1:B){
        boot_dat <- slice_sample(subdata, n=N, replace = TRUE)
        M <- boot_dat %>% summarise(mean(get(metric))) %>% as.numeric()
        mean_lst[[i]] <- M}
      mean_summ <- tibble(means = unlist(mean_lst))
      mean_summ$basin <- x 
      if(x == unique(thresh$basin)[1]){
        bootstrap <- mean_summ
      }else{bootstrap <- rbind(bootstrap, mean_summ)}
    }
    
    
    thresh <-  bootstrap %>% group_by(basin) %>% summarise(
      q_0.05 = quantile(means, 0.05),
      q_0.5  = quantile(means, 0.5),
      q_0.95 = quantile(means, 0.95)) 
    
    #determine best fit lines 
    for(y in unique(data$basin)){
      for(x in c("LOW", "MOD", "HIGH")){
        suppressWarnings(output <- best_mod(subset(data, data$basin ==y), x, metric, 
                                            thresh[thresh$basin==y,4]))
        
        if(x == "LOW" & y == unique(data$basin)[1]){
          fits <- output[[1]]
          best_fit <- output[[2]]
          thresh_cross <- output[[3]]
        }else{
          fits <- rbind(fits, output[[1]])
          best_fit <- rbind(best_fit, output[[2]])
          thresh_cross <- c(thresh_cross,output[[3]])
        }}} 
    
    fits$thresh <- thresh_cross
    write.csv(fits, file.path(data_save_path,paste0("thresholds_", metric, ".csv")), row.names=F)
    
    #calculate limits 
    #get values (thresholds and points)
    thresh_finder <- plot_data %>% ungroup() %>% dplyr::select(basin, lower.ci, upper.ci)
    thresh_finder <- as.data.frame(mapply(c, thresh_finder, rbind(thresh %>% dplyr::select(basin, q_0.05, q_0.95)))) 
    thresh_finder[,2:3] <- sapply(thresh_finder[,2:3],as.numeric)
    
    limits <- thresh_finder %>% group_by(basin) %>% summarise(min = min(lower.ci),
                                                              max = max(upper.ci)) %>% mutate(median = thresh$q_0.5,
                                                                                              range = max- min,
                                                                                              min_med = median -min,
                                                                                              max_med = max-median,
                                                                                              lower=NA, 
                                                                                              upper=NA)
    ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)
    
    range <- ceiling_dec(max(limits$range)*1.2,2)
    
    #if max is futher from median start there, otherwise start from the bottom
    for(x in 1:2){
      if(limits$max_med[x] > limits$min_med[x]){
        limits$upper[x] <- limits$max[x] * 1.05
        limits$lower[x] <- limits$upper[x] - range
        if(limits$lower[x] < 0){
          limits$lower[x] <- 0
          limits$upper[x] <- range
        }
      }else{
        limits$lower[x] <- limits$min[x] * 0.95
        limits$upper[x] <- limits$lower[x] + range 
      }
    }
    
    
    #limits are set so the same amount of area is shown in both
    p2 <- ggplot() + 
      geom_point(plot_data, mapping=aes(x=real_per, y=mean, color=sev), size=4, alpha=0.9) +
      geom_errorbar(data=plot_data, aes(x=real_per, ymin=lower.ci, ymax=upper.ci, color=sev), width=0, linewidth=1) + 
      scale_color_manual(values=c("darkgreen", "#3B9AB2",  "#E1AF00", "#F21A00"), labels = c("Unburned", "Low", "Moderate", "High")) + 
      labs(x="Area Burned (%)", y=label, 
           color="Burn Severity") + theme_pub() + 
      theme(axis.title = element_text(size=18)) + 
      geom_rect(thresh, mapping=aes(xmin=-Inf, xmax=Inf, ymin=q_0.05, 
                                    ymax=q_0.95), alpha=0.4, fill="gray40") +
      geom_hline(thresh, mapping=aes(yintercept=q_0.5), linetype="dashed") + 
      geom_line(best_fit, mapping=aes(x=real_per, y=fit_val, color=severity)) + 
      facet_wrap(~basin, scale="free_y") +
      facetted_pos_scales(
        y = list(
          basin == "Humid, Forested Basin" ~ scale_y_continuous(limits=c(limits$lower[1], limits$upper[1])), 
          basin == "Semi-Arid, Mixed Land Use Basin" ~ scale_y_continuous(limits=c(limits$lower[2], limits$upper[2]))))
    
    if(is.null(legend.pos) == F){
      p2 <- p2 + theme(legend.position = legend.pos) 
    }
    
    return(p2)
  }

  #ensure two geospatial objects have the same crs
  convert_crs <- function (input, goal, type = "numeric", res = NULL) {
    stopifnot(class(input)[1] %in% c("sf", "RasterLayer"), class(goal)[1] %in% 
                c("sf", "RasterLayer"), type %in% c("numeric", "categorical"))
    if (raster::compareCRS(goal, input) == T) {
      message("CRS already matches that of your goal object")
      return(input)
    }
    else {
      if (class(input)[1] == "RasterLayer") {
        if (is.null(res)) {
          if (class(goal)[1] == "RasterLayer") {
            res <- terra::res(goal)[1]
          }
          else {
            unit <- sf::st_crs(goal, parameters = TRUE)$units_gdal
            res <- ifelse(unit == "degree", 0.0003280119, 
                          30)
          }
        }
        method <- ifelse(type == "numeric", "bilinear", "ngb")
        input_prj <- raster::projectRaster(input, crs = raster::crs(goal), 
                                           method = method, res = res)
      }
      else {
        input_prj <- sf::st_transform(input, raster::crs(goal))
      }
      return(input_prj)
    }
  }
  
  #clean and clip rasters for plotting
  clean_raster <- function (raster, sf, type = "numeric", res = NULL, return = "df") {
    stopifnot(class(raster) == "RasterLayer", class(sf)[1] == 
                c("sf"), type %in% c("numeric", "categorical"), return %in% 
                c("df", "raster"))
    unit <- sf::st_crs(sf, parameters = TRUE)$units_gdal
    buffer <- ifelse(unit == "degree", 0.1, 5000)
    res <- ifelse(unit == "degree", 0.0003280119, 30)
    method <- ifelse(type == "numeric", "bilinear", "ngb")
    if (compareCRS(raster, sf) == T) {
      raster_crop <- raster::crop(raster, sf)
      raster_crop <- raster::mask(raster_crop, sf)
      raster_df <- as.data.frame(raster::rasterToPoints(raster_crop))
      colnames(raster_df) <- c("x", "y", "val")
    }
    else {
      sf_prj <- convert_crs(sf::st_buffer(sf, dist = buffer), 
                            raster)
      raster_crop <- raster::crop(raster, sf_prj)
      raster_crop <- raster::mask(raster_crop, sf_prj)
      raster_prj <- raster::projectRaster(raster_crop, crs = crs(sf), 
                                          method = method, res = res)
      raster_crop <- raster::crop(raster_prj, sf)
      raster_crop <- raster::mask(raster_crop, sf)
      raster_df <- as.data.frame(raster::rasterToPoints(raster_crop))
      colnames(raster_df) <- c("x", "y", "val")
    }
    if (return == "df") {
      raster_df
    }
    else {
      raster_crop
    }
  } 
  
  #extract basin metrics from model and shapefile data (shapefiles generated in 1-create-swat-files.R)
  summarise_table <- function(basin){
    if(basin == "American"){
      sf_path <- "~/1_Research/4_Wenas_Thresholds/data/American River/"
      model_path <- "C:/SWAT/American River Simp2/American River Simp2/Scenarios/American River Simp2.Sufi2.SwatCup" #where model lives
      basin_path <- "C:/SWAT/American River Simp2/American River Simp2/Watershed/shapes/subs1.shp" #subbasin file
      sum_start <- 5977 #where   AVE ANNUAL BASIN VALUES starts in output.std
    }else if(basin == "Tule"){
      sf_path <- "~/1_Research/4_Wenas_Thresholds/data/Tule_River/"
      model_path <- "C:/SWAT/Tule River Simp2/Tule River Simp2/Scenarios/Tule River Simp2.Sufi2.SwatCup" #where model lives
      basin_path <- "C:/SWAT/Tule River Simp2/Tule River Simp2/Watershed/shapes/subs1.shp"
      sum_start <- 6029 
    }
    
    #get basin shape
    basin_shp <- st_read(basin_path)
    basin_shp <- st_union(basin_shp) %>% st_sf()
    
    #get slope
    dem <- raster(file.path(sf_path, "projected_spatial/dem.tif"))
    elev <- clean_raster(dem, basin_shp)
    elev <- mean(elev$val)
    slope <- terrain(dem, opt="slope", unit="radians")
    slopeval <- clean_raster(slope, basin_shp)
    slopeval$val <- tan(slopeval$val)*100 
    slope <- round(mean(slopeval$val),1)
    
    #aridity index 
    #download data here: https://doi.org/10.6084/m9.figshare.7504448.v3
    aridity <- raster("Z:/3_GIS-GPS/Random Forest Datasets/ai_et0/ai_et0.tif")
    aridity <- clean_raster(aridity, basin_shp) 
    aridity$val <- aridity$val /10000
    aridity <- round(mean(aridity$val),2)
    
    #annual precip, flow, et
    std <- read_lines(file.path(model_path, "output.std"))
    precip <- as.numeric(str_split_1(str_split_i(std[sum_start+2], "=", i=2), "MM"))[1]
    flow <- as.numeric(str_split_1(str_split_i(std[sum_start+ 14], "=", i=2), "MM"))[1]
    et <-as.numeric(str_split_1(str_split_i(std[sum_start + 16], "=", i=2), "MM"))[1]
    
    RR <- flow/precip
    
    #flowpaths
    paths <- as.numeric(str_split_i(str_split_i(std[sum_start + c(6,7,9,10)], "=", i=2), "MM",i=1))
    paths <- data.frame(type=c("surface","lateral","shallow gw", "deep gw"), value=paths)
    paths$percent <- paths$value/flow * 100
    
    #landuse (pull from HruLanduseSoilSlopeRepSwat.txt)
    
    #average streamflow, nitrate, doc 
    start <- as.Date("2005-01-01")
    end <- as.Date("2018-12-31")
    dates <- seq(from=start, to=end, by="day")
    rch <- fread(file.path(model_path, "output.rch"), skip = 9)
    rch <- rch[,c(2,4,7,11,12,14,18,34,32,36)]
    colnames(rch) <- c("SUB", "MONTH","FLOW","SED_TON","SED_CON","ORG_N","NO3", "DOC", "POC", "TOC")
    rch$date <- rep(dates, each=21)
    out <- filter(rch, SUB == 1)
    
    #get more metrics by outlet area 
    out$flow_L <- out$FLOW * 86400* 1000 #flow in L per day 
    out$nitrate_mgL <- out$NO3 * 1000000 / out$flow_L
    out$doc_mgL <- (out$DOC * 1000000 / out$flow_L)
    
    flow <- mean(out$FLOW)
    nit <- mean(out$nitrate_mgL)
    doc <- mean(out$doc_mgL)
    
    #get summary of metrics
    
    metrics <- data.frame(metric=c("basin", "elevation", "slope", "aridity",
                                   "precip","flow","et", "RR",
                                   "surQ","latQ","sgwQ","dgwQ",
                                   "avg_flow","avg_nit","avg_doc"),
                          value=c(basin, elev, slope, aridity, precip, flow,
                                  et, RR, paths$percent[1],
                                  paths$percent[2],paths$percent[3],
                                  paths$percent[4],flow,nit,doc))
    return(metrics)
  }
  
  #load annual summary data for plotting 
  load_annual <- function(data_save_path){
    outlet <- 1 #subbasin number for the outlet
    fire <- "2017-08-11" #date of fire 
    
    #american 
    area <- 206.834425 #km2
    annuals_a <- read_csv(file.path("~/1_Research/0_Misc/rc_sfa-rc-3-wenas-modeling/data-package/outputs/data", "annual_loads_rch_american.csv"))
    annuals_a$perc <- ifelse(annuals_a$scenario == "UNBURN", 0, as.numeric(str_split_i(annuals_a$scenario, "_", i=2)))
    annuals_a$sev <- factor(annuals_a$sev, levels=c("UNBURN", "LOW", "MOD", "HIGH"), ordered=T)
    
    #load burn scenarios 
    hru_burn <- read.csv("~/1_Research/4_Wenas_Thresholds/data/American River/wild_fire files/hru_burn_scenarios.csv")
    hru_burn <- subset(hru_burn, hru_burn$sev == "burned")
    
    burn_scenarios <- hru_burn %>% group_by(scenario) %>% summarise(area_ha = sum(area_ha)) %>% mutate(real_per = area_ha / (area *100)*100 )
    
    annuals_a <- annuals_a %>% left_join(burn_scenarios, by=c("perc" = "scenario")) 
    annuals_a$real_per[annuals_a$perc == 0] <- 0
    annuals_a$basin <- "Humid, Forested Basin" 
    
    #tule 
    area <- 249.9807#km2
    annuals_t <- read_csv(file.path("~/1_Research/0_Misc/rc_sfa-rc-3-wenas-modeling/data-package/outputs/data", "annual_loads_rch_tule.csv"))
    annuals_t$perc <- ifelse(annuals_t$scenario == "UNBURN", 0, as.numeric(str_split_i(annuals_t$scenario, "_", i=2)))
    annuals_t$sev <- factor(annuals_t$sev, levels=c("UNBURN", "LOW", "MOD", "HIGH"), ordered=T)
    
    #load burn scenarios 
    hru_burn <- read.csv("~/1_Research/4_Wenas_Thresholds/data/tule_River/wild_fire files/hru_burn_scenarios.csv")
    hru_burn <- subset(hru_burn, hru_burn$sev == "burned")
    
    burn_scenarios <- hru_burn %>% group_by(scenario) %>% summarise(area_ha = sum(area_ha)) %>% mutate(real_per = area_ha / (area *100)*100 )
    
    annuals_t <- annuals_t %>% left_join(burn_scenarios, by=c("perc" = "scenario")) 
    annuals_t$real_per[annuals_t$perc == 0] <- 0
    annuals_t$basin <- "Semi-Arid, Mixed Land Use Basin"
    
    #merge data together
    data <- rbind(annuals_a, annuals_t)
    
    return(data)
  }
  
  #get percent change from unburned for a metric 
  perc_change <- function(data, metric){
    #basic percent change function
    change <- function(old, new){
      per <- (new - old) / old * 100
      return(per)
    } 
    
    annual_change <- data %>% select(any_of(c("basin", "year","sev", "real_per", metric))) %>% 
      pivot_wider(names_from=sev, values_from=!!sym(metric)) %>% 
      fill(UNBURN, .direction="up") %>% 
      mutate(LOW = change(UNBURN, LOW), 
             MOD = change(UNBURN, MOD),
             HIGH = change(UNBURN, HIGH),
             UNBURN = change(UNBURN, UNBURN)) %>% select(basin, year,real_per, LOW, MOD, HIGH, UNBURN) %>% 
      pivot_longer(LOW:UNBURN, values_to = metric,  names_to = "sev") 
    
    #remove scenarios that don't make sense (0 percent burn but not burned, greater than 0 burn but unburned)
    annual_change <- annual_change[!(annual_change$real_per == 0 & annual_change$sev != "UNBURN"),]
    annual_change <- annual_change[!(annual_change$real_per > 0 & annual_change$sev== "UNBURN"),]
    annual_change$sev <- factor(annual_change$sev, levels=c("UNBURN","LOW", "MOD", "HIGH"), ordered = T)
    
    annual_change <- annual_change %>% group_by(basin, sev, real_per) %>%
      summarise(
        q_0.05 = quantile(get(metric), 0.05),
        q_0.25 = quantile(get(metric), 0.25),
        q_0.5  = quantile(get(metric), 0.5),
        q_0.75 = quantile(get(metric), 0.75),
        q_0.95 = quantile(get(metric), 0.95),
        mean = mean(get(metric)),
        sd= sd(get(metric)))
    
    annual_sum <- annual_change %>% group_by(basin, sev) %>% 
      summarise(min = min(mean),
                max = max(mean))
    write.csv(annual_sum, file.path(data_save_path, paste0(metric, "_perc_change.csv")), row.names=F)
    
    return(annual_change)
  }
  
  #get absolute change from unburned for a metric 
  abs_change <- function(data, metric){
    #basic percent change function
    change <- function(old, new){
      abs <- new -old
      return(abs)
    } 
    
    annual_change <- data %>% select(any_of(c("basin", "year","sev", "real_per", metric))) %>% 
      pivot_wider(names_from=sev, values_from=!!sym(metric)) %>% 
      fill(UNBURN, .direction="up") %>% 
      mutate(LOW = change(UNBURN, LOW), 
             MOD = change(UNBURN, MOD),
             HIGH = change(UNBURN, HIGH),
             UNBURN = change(UNBURN, UNBURN)) %>% select(basin, year,real_per, LOW, MOD, HIGH, UNBURN) %>% 
      pivot_longer(LOW:UNBURN, values_to = metric,  names_to = "sev") 
    
    #remove scenarios that don't make sense (0 percent burn but not burned, greater than 0 burn but unburned)
    annual_change <- annual_change[!(annual_change$real_per == 0 & annual_change$sev != "UNBURN"),]
    annual_change <- annual_change[!(annual_change$real_per > 0 & annual_change$sev== "UNBURN"),]
    annual_change$sev <- factor(annual_change$sev, levels=c("UNBURN","LOW", "MOD", "HIGH"), ordered = T)
    
    annual_change <- annual_change %>% group_by(basin, sev, real_per) %>%
      summarise(
        q_0.05 = quantile(get(metric), 0.05),
        q_0.25 = quantile(get(metric), 0.25),
        q_0.5  = quantile(get(metric), 0.5),
        q_0.75 = quantile(get(metric), 0.75),
        q_0.95 = quantile(get(metric), 0.95),
        mean = mean(get(metric)),
        sd= sd(get(metric)),
        max = max(get(metric)),
        min = min(get(metric)))
    
    annual_sum <- annual_change %>% group_by(basin, sev) %>% 
      summarise(min = min(mean),
                max = max(mean))
    write.csv(annual_sum, file.path(data_save_path, paste0(metric, "_abs_change.csv")), row.names=F)
    
    return(annual_change)
  }
  
  
#section 1: figure 1: map of the two basins with landuse/dem --------
    #data for coloring landuse 
    landuse_cols <- c("Water" = "#476BA0", "Developed" = "#D89382", "Barren" = "#B2ADA3", 
                      "Mixed/Deciduous Forest" = "#B5C98E", "Evergreen Forest" = "#1C6330", 
                      "Shrub" = "#CCBA7C", "Grassland" = "#E2E2C1", "Cultivated" = "#DBD83D", 
                      "Wetlands" = "#70A3BA")
    lookup <- data.frame(number=c(11,12,21:24, 31,41:43, 52,71,81,82,90,95), 
                         landuse=c("Water", "Water", "Developed", "Developed", "Developed", "Developed", 
                                   "Barren", "Mixed/Deciduous Forest", "Evergreen Forest", "Mixed/Deciduous Forest", 
                                   "Shrub", "Grassland", "Cultivated", "Cultivated", "Wetlands", "Wetlands"))
    
  #american plot 
    modelwd <- "C:/SWAT/American River Simp2/American River Simp2/"
    basin <- st_read(file.path(modelwd, "Watershed/shapes/subs1.shp"))
    basin <- st_union(basin) %>% st_sf()
    dem <- raster(file.path(modelwd, "Source/hillshade.tif"))
    landuse <- raster(file.path(modelwd, "Source/crop/landcover_2016.tif"))
    streams <- st_read(file.path(modelwd, "Watershed/shapes/riv1.shp"))
    
    #clip and make ready for plotting 
    dem <- clean_raster(dem, basin)
    landuse <- clean_raster(landuse, basin, type="categorical")
    
    #reclass to make less landuses 
    landuse <- merge(landuse, lookup, by.x="val", by.y="number")
    
    #get limits for text label 
    xjust <- 0.8
    yjust <- 0.03
    ext <- extent(dem)
    x <- ext[1] + (ext[2]-ext[1])*xjust
    y <- ext[3] + (ext[4]-ext[3])*yjust
    p1 <- ggplot()+
      geom_raster(data=dem, aes(x=x, y=y,fill=val), show.legend = F) + 
      scale_fill_gradient(low="gray10", high="gray90")+ 
      geom_sf(data=basin, fill=NA, color=NA, linewidth=2) +
      new_scale_fill() + geom_raster(data=landuse, aes(x=x, y=y, fill=landuse), 
                                     alpha=0.5) + 
      geom_sf(data=streams, color="#00008b", linewidth=0.75) + theme_bw() + 
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust=1)) + 
      scale_fill_manual(values=landuse_cols) + labs(fill="Land Cover") + 
      annotate(geom = "text", x = x, y = y, label = "Humid, Forested Basin",
               size=5, fontface="bold") 
  
  
  #plot tule
    modelwd <- "C:/SWAT/Tule River Simp2/Tule River Simp2/"
    basin <- st_read(file.path(modelwd, "Watershed/shapes/subs1.shp"))
    basin <- st_union(basin) %>% st_sf()
    dem <- raster(file.path(modelwd, "Source/hillshade.tif"))
    landuse <- raster(file.path(modelwd, "Source/crop/landcover_2016.tif"))
    streams <- st_read(file.path(modelwd, "Watershed/shapes/riv1.shp"))
    
    #clip and make ready for plotting 
    dem <- clean_raster(dem, basin)
    landuse <- clean_raster(landuse, basin, type="categorical")
    
    #reclass to make less landuses 
    landuse <- merge(landuse, lookup, by.x="val", by.y="number")
    
    #get limits for text label
    xjust <- 0.7
    yjust <- 0.03
    ext <- extent(dem)
    x <- ext[1] + (ext[2]-ext[1])*xjust
    y <- ext[3] + (ext[4]-ext[3])*yjust
    
    p2 <- ggplot()+
      geom_raster(data=dem, aes(x=x, y=y,fill=val), show.legend = F) + 
      scale_fill_gradient(low="gray10", high="gray90")+ 
      geom_sf(data=basin, fill=NA, color=NA, linewidth=2) +
      new_scale_fill() + geom_raster(data=landuse, aes(x=x, y=y, fill=landuse), 
                                     alpha=0.5) + 
      geom_sf(data=streams, color="#00008b", linewidth=0.75) + theme_bw() + 
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust=1)) + 
      scale_fill_manual(values=landuse_cols) + labs(fill="Land Cover")+ 
      annotate(geom = "text", x = x, y = y, label = "Semi-Arid, Mixed Land Use Basin",
               size=5, fontface="bold") 
     
    png(file.path(fig_save_path, "fig1-basin_maps.png"), units="cm", height = 15, width=30, res=300)
    ggarrange(p1, p2, ncol=2, labels="auto", common.legend = T, legend="bottom",
              font.label = list(size=30), align=c("h"), label.x=0.11, label.y=0.98)
    dev.off()
    
#section 2: table 1: extract basin descriptions -- IMPORTANT: Have correct unburned model ouputs run in SWAT projects -------
    #IMPORTANT: before running, run the base scenario for each model
      sum_american <- summarise_table("American")
      sum_tule <-  summarise_table("Tule")
      
      summary <- cbind(sum_american, sum_tule[,2])
      colnames(summary) <- c("metric", "basin1", "basin2")
      write.csv(summary, file.path(data_save_path, "table1-basin_summary.csv"), row.names=F, quote=F)
       
#section 3: figure 2: flow and RR ratio changes ------- 
  #load annual reach summaries 
    data <- load_annual(data_save_path)
      
  #get annual water yields 
      flow_plot <- data %>% group_by(real_per, sev, basin) %>%  
        summarise(mean = mean(flow_mm_yr),sd = sd(flow_mm_yr), n  = n()) %>%
        mutate(se= sd / sqrt(n),
               lower.ci = mean- qt(1 - (0.05 / 2), n - 1) * se,
               upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
      
      #get quantiles for unburned
      thresh <- subset(data, data$sev == "UNBURN") 
      thresh <-  thresh %>% group_by(basin) %>% summarise(
        q_0.05 = quantile(flow_mm_yr, 0.05),
        q_0.5  = quantile(flow_mm_yr, 0.5),
        q_0.95 = quantile(flow_mm_yr, 0.95))
      
      #determine best fit lines 
      for(y in unique(data$basin)){
        for(x in c("LOW", "MOD", "HIGH")){
          output <- best_mod(subset(data, data$basin ==y), x, "flow_mm_yr", 
                             thresh[thresh$basin==y,4])
          if(x == "LOW" & y == unique(data$basin)[1]){
            best_fit <- output[[2]]
            thresh_cross <- output[[3]]
          }else{
            best_fit <- rbind(best_fit, output[[2]])
            thresh_cross <- c(thresh_cross,output[[3]])
          }}}
      
      flow_sum <- flow_plot %>% group_by(basin, sev) %>% summarise(min = min(mean), max=max(mean)) 
      flow_sum$unburn <- rep(flow_sum$min[c(1,5)], each=4)
      flow_sum <- flow_sum %>% mutate(min_dif = min-unburn, max_dif = max-unburn)
      
      #calculate limits 
      #get values (thresholds and points)
      thresh_finder <- flow_plot %>% ungroup() %>% dplyr::select(basin, mean)

      limits <- thresh_finder %>% group_by(basin) %>% summarise(min = min(mean),
                                                                max = max(mean),
                                                                median = median(mean)) %>% mutate(
                                                                                                range = max- min,
                                                                                                min_med = median -min,
                                                                                                max_med = max-median,
                                                                                                lower=NA, 
                                                                                                upper=NA)
      ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)
      
      range <- ceiling_dec(max(limits$range)*1.5,0)
      
      #if max is futher from median start there, otherwise start from the bottom
      for(x in 1:2){
        if(limits$max_med[x] > limits$min_med[x]){
          limits$upper[x] <- limits$max[x] * 1.01
          limits$lower[x] <- limits$upper[x] - range
          if(limits$lower[x] < 0){
            limits$lower[x] <- 0
            limits$upper[x] <- range
          }
        }else{
          limits$lower[x] <- limits$min[x] * 0.95
          limits$upper[x] <- limits$lower[x] + range 
        }
      }
      
      #make plot
      p1 <- ggplot() + 
        geom_point(flow_plot, mapping=aes(x=real_per, y=mean, color=sev), size=4, alpha=0.9) +
        scale_color_manual(values=c("darkgreen", "#3B9AB2",  "#E1AF00", "#F21A00"),labels = c("Unburned", "Low", "Moderate", "High")) + 
        labs(x="Area Burned (%)", y=expression(bold(paste("Annual Water Yield (", mm, " ", yr^{-1}, ")"))), 
             color="Burn Severity") + theme_pub() +
        geom_line(best_fit, mapping=aes(x=real_per, y=fit_val, color=severity)) + 
        facet_wrap(~basin, scale="free_y") +
        facetted_pos_scales(
          y = list(
            basin == "Humid, Forested Basin" ~ scale_y_continuous(limits=c(limits$lower[1], limits$upper[1]),
                                                                  breaks = seq(0, 2000, 20)), 
            basin == "Semi-Arid, Mixed Land Use Basin" ~ scale_y_continuous(limits=c(limits$lower[2], limits$upper[2]),
                                                                            breaks = seq(0, 2000, 20))))
      

  #get change in annual yields (absolute)
      flow_change <- abs_change(data, "flow_mm_yr") %>% select("basin", "sev", "real_per", "mean")
      flow_change <- perc_change(data, "flow_mm_yr")
      
  #get runoff ratios 
      data$rr <- data$flow_mm_yr / data$precip_mm 
      p2 <- threshold_plot(data, "rr", "Annual Runoff Ratio")
      
  #combine 
      png(paste0(fig_save_path, "/fig2-flow_rr_changes.png"),
          res=300, units="cm", width=30, height=30)
      ggpubr::ggarrange(p1,p2, ncol=1,  common.legend = TRUE, legend="bottom",
                        labels="auto", font.label = list(size=30))
      dev.off()   
      
#section 4: figure 3: relative change in flow paths -------
    #load data
      df <- read.csv(file.path("~/1_Research/0_Misc/rc_sfa-rc-3-wenas-modeling/data-package/outputs/data", "hru_summary_american.csv"))
      df$basin <- "Humid, Forested Basin"
      
      df2 <- read.csv(file.path("~/1_Research/0_Misc/rc_sfa-rc-3-wenas-modeling/data-package/outputs/data", "hru_summary_tule.csv"))
      df2$basin <- "Semi-Arid, Mixed Land Use Basin"
      flow <- rbind(df, df2)
      flow <- flow[flow$year != 1987,] #remove the extra year
      
      #merge to get total flow 
      data <- load_annual(data_save_path)
      data$scenario[data$sev == "UNBURN"] <- "PER_0"
      
      flow <- flow %>% left_join(data, by=c("basin", "year", "sev", "scenario"))
      
      flow$DGWQ <- flow$flow_mm_yr - flow$SURQ -flow$LATQ - flow$GWQ #get deep groundwater flow 
      
      #get average values for each scenario for plotting
      plot_data <- flow %>% mutate(across(c(SURQ:GWQ, DGWQ), ~.x/flow_mm_yr*100)) %>% 
        pivot_longer(c(SURQ:GWQ, DGWQ), names_to="path", values_to="amount") %>% 
        mutate(PER = as.numeric(gsub("PER_", "", scenario))) %>% group_by(basin, path, sev, PER) %>% 
        summarise(amount = mean(amount))
      
      #add unburned for other scenarios
      unburn <- subset(plot_data,plot_data$sev == "UNBURN")
      unburned <- do.call("rbind", replicate(3, unburn, simplify = FALSE))
      unburned$sev <- c(rep("LOW", nrow(unburn)), rep("MOD", nrow(unburn)),rep("HIGH", nrow(unburn)))
      plot_data <- rbind(plot_data, unburned) 
      plot_data <- subset(plot_data, plot_data$sev != "UNBURN")
      
      #make factors 
      plot_data$path <- factor(plot_data$path, levels=c("SURQ", "LATQ", "GWQ", "DGWQ"),
                          labels=c("Surface", "Lateral", "Shallow Groundwater", "Deep Groundwater"), 
                          ordered=T)
      plot_data$sev <- factor(plot_data$sev, levels=c("LOW", "MOD", "HIGH"), 
                         labels=c("Low", "Moderate", "High"), ordered=T)
      
      #plot
      p1 <-  ggplot(plot_data, aes(x=PER, y=amount, fill=path)) + geom_bar(stat="identity") +
        facet_grid(sev ~ basin) + 
        scale_fill_manual(values=rev(pnw_palette("Cascades", n=4))) + 
        labs(x="Area Burned (%)", y="Water Yield (%)", 
             fill="Flow Path") + theme_pub() + 
        theme(legend.position = "bottom") + 
        theme(axis.title = element_text(size=18)) + 
        theme(strip.background = element_rect(fill="gray70")) +
        theme(strip.text = element_text(margin = ggplot2:::margin(t = 7, r = 7, b =7, l = 7),
                                        size=14)) +guides(fill=guide_legend(nrow=2))
      
      png(paste0(fig_save_path, "/fig3-flow_paths.png"),
          res=300, units="cm", width=20, height=20)
      p1      
      dev.off()  
      
      #get percentage of flow for each
      paths <- plot_data %>% pivot_wider(names_from = "PER", names_prefix = "PER_",
                                         values_from = "amount")
      unburn <- paths %>% ungroup() %>% select(c(basin, path, PER_0)) %>% unique()
      annual_change <- paths %>% select(-PER_0) %>% 
        left_join(unburn, by=c("basin", "path"))  %>%
        mutate(across(PER_5:PER_0, ~(.x - PER_0))) %>% 
        pivot_longer(PER_10:PER_0, names_to = "scenario", values_to = "change")
        
      flow_paths <- annual_change %>% group_by(basin, sev, path) %>%
        filter(scenario != "PER_0") %>%
        summarise(min = min(change),
                  max= max(change)) %>% pivot_wider(names_from=sev,values_from=c(min, max))
     
      write.csv(flow_paths, file.path(data_save_path, "flowpath_change.csv"), row.names=F)
      
      
#section 5: figure 4: nitrate load and concentration ------ 
  #load annual reach summaries 
    data <- load_annual(data_save_path)
  
  #get annual nitrate loads
    p1 <- threshold_plot(data %>% mutate(nitrate_kg_yr = nitrate_kg_yr / 10000), 
                           "nitrate_kg_yr", "Annual Nitrate Load (10\u2074 kg)")  
    
  #get concentration
    p2 <- threshold_plot(data, "avg_nitrate_mgL", "Average Nitrate Concentration (mg L\u207B\u00B9)")  
  
  #combine and save 
    png(file.path(fig_save_path, "fig4-nitrate_load_con.png"),
        res=300, units="cm", width=30, height=30)
    ggpubr::ggarrange(p2,p1, ncol=1,  common.legend = TRUE, legend="bottom",
                      labels="auto", font.label = list(size=30))
    dev.off() 

  #get perc change in concentration for results 
    nitrate_change <- perc_change(data, "avg_nitrate_mgL")
    
#section 6: figure 5: doc load and concentration ------- 
  #load annual reach summaries 
    data <- load_annual(data_save_path)
    
  #get annual doc loads
    p1 <- threshold_plot(data %>% mutate(doc_kg_yr = doc_kg_yr / 10000), 
                         "doc_kg_yr", "Annual DOC Load (10\u2074 kg)")  
    
  #get average concentraiton 
    p2 <- threshold_plot(data, "avg_doc_mgL", "Average DOC Concentration (mg L\u207B\u00B9)") 
    
    #combine and save 
    png(file.path(fig_save_path, "fig5-doc_load_con.png"),
        res=300, units="cm", width=30, height=30)
    ggpubr::ggarrange(p2,p1, ncol=1,  common.legend = TRUE, legend="bottom",
                      labels="auto", font.label = list(size=30))
    dev.off() 
    
  #get perc change in concentration for results 
    doc_change <- perc_change(data, "avg_doc_mgL")
    
#section 7: figure A1: wildfire scenarios ------ 
  #plot american 
    savewd <- "~/1_Research/4_Wenas_Thresholds/data/American River/wild_fire files"
    modelwd <- "C:/SWAT/American River Simp2/American River Simp2/"
    hru_burned <- read.csv(file.path(savewd, "hru_burn_scenarios.csv")) 
    buffer <- st_read(file.path(modelwd, "Watershed/shapes/subs1.shp"))
    buffer <- st_union(buffer) %>% st_sf()
    hru <- st_read(file.path(modelwd, "Watershed/shapes/hru2.shp"))
    hru <- hru[order(hru$HRUGIS),]
    hru$HRU_ID <- 1:nrow(hru)
    
    plot_hru <- merge(hru, hru_burned, by.x="HRU_ID", by.y="hru") 
    plot_hru$name <- factor(paste(plot_hru$scenario, "Percent"), levels=paste(seq(from=5, to=100, by=5), "Percent"), ordered=T)
    plot_hru$sev[plot_hru$sev == "unburned"] <- NA
    plot_hru <- na.omit(plot_hru)
    p1 <- ggplot() + geom_sf(data=buffer, fill=NA, color="black") +
      geom_sf(data=plot_hru, fill= "#dd4124", color=NA) +
      facet_wrap(~name) + theme_bw() + 
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 45, hjust=1)) 
    
    
  #plot tule
    savewd <- "~/1_Research/4_Wenas_Thresholds/data/tule_River/wild_fire files"
    modelwd <- "C:/SWAT/Tule River Simp2/Tule River Simp2/"
    hru_burned <- read.csv(file.path(savewd, "hru_burn_scenarios.csv")) 
    buffer <- st_read(file.path(modelwd, "Watershed/shapes/subs1.shp"))
    buffer <- st_union(buffer) %>% st_sf()
    hru <- st_read(file.path(modelwd, "Watershed/shapes/hru2.shp"))
    hru <- hru[order(hru$HRUGIS),]
    hru$HRU_ID <- 1:nrow(hru)
    
    plot_hru <- merge(hru, hru_burned, by.x="HRU_ID", by.y="hru") 
    plot_hru$name <- factor(paste(plot_hru$scenario, "Percent"), levels=paste(seq(from=5, to=100, by=5), "Percent"), ordered=T)
    plot_hru$sev[plot_hru$sev == "unburned"] <- NA
    plot_hru <- na.omit(plot_hru)
    p2 <- ggplot() + geom_sf(data=buffer, fill=NA, color="black") +
      geom_sf(data=plot_hru, fill= "#dd4124", color=NA) +
      facet_wrap(~name) + theme_bw() + 
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            axis.text.x = element_text(angle = 45, hjust=1)) 
    
    png(file.path(fig_save_path, "figA1-burn_scenarios.png"), units="cm", height = 35, width=20, res=300)
    plot_grid(p1, p2, ncol=1, labels = "auto", rel_heights =c(0.94, 1), label_size=30)
    dev.off()
    
#section 8: figure A2: Change in ET for each scenario ------ 
    #load data
    area <- 206.834425 #km2
    annuals_a <- read.csv(file.path("~/1_Research/0_Misc/rc_sfa-rc-3-wenas-modeling/data-package/outputs/data", "hru_summary_american.csv"))
    annuals_a$perc <- ifelse(annuals_a$scenario == "UNBURN", 0, as.numeric(str_split_i(annuals_a$scenario, "_", i=2)))
    annuals_a$sev <- factor(annuals_a$sev, levels=c("UNBURN", "LOW", "MOD", "HIGH"), ordered=T)
    
    #load burn scenarios 
    hru_burn <- read.csv("~/1_Research/4_Wenas_Thresholds/data/American River/wild_fire files/hru_burn_scenarios.csv")
    hru_burn <- subset(hru_burn, hru_burn$sev == "burned")
    
    burn_scenarios <- hru_burn %>% group_by(scenario) %>% summarise(area_ha = sum(area_ha)) %>% mutate(real_per = area_ha / (area *100)*100 )
    
    annuals_a <- annuals_a %>% left_join(burn_scenarios, by=c("perc" = "scenario")) 
    annuals_a$real_per[annuals_a$perc == 0] <- 0
    annuals_a$basin <- "Humid, Forested Basin" 
    
    
    area <- 249.9807#km2
    annuals_t <- read.csv(file.path("~/1_Research/0_Misc/rc_sfa-rc-3-wenas-modeling/data-package/outputs/data", "hru_summary_tule.csv"))
    annuals_t$perc <- ifelse(annuals_t$scenario == "UNBURN", 0, as.numeric(str_split_i(annuals_t$scenario, "_", i=2)))
    annuals_t$sev <- factor(annuals_t$sev, levels=c("UNBURN", "LOW", "MOD", "HIGH"), ordered=T)
    
    #load burn scenarios 
    hru_burn <- read.csv("~/1_Research/4_Wenas_Thresholds/data/tule_River/wild_fire files/hru_burn_scenarios.csv")
    hru_burn <- subset(hru_burn, hru_burn$sev == "burned")
    
    burn_scenarios <- hru_burn %>% group_by(scenario) %>% summarise(area_ha = sum(area_ha)) %>% mutate(real_per = area_ha / (area *100)*100 )
    
    annuals_t <- annuals_t %>% left_join(burn_scenarios, by=c("perc" = "scenario")) 
    annuals_t$real_per[annuals_t$perc == 0] <- 0
    annuals_t$basin <- "Semi-Arid, Mixed Land Use Basin"
    
    flow <- rbind(annuals_a, annuals_t)
    flow <- flow[flow$year != 1987,] #remove the extra year
    p1 <- threshold_plot(flow, "ET", "Annual Evapotranspiration (mm)")
    
    png(file.path(fig_save_path, "figA2-ET-changes.png"),
        res=300, units="cm", width=30, height=15)
    p1
    dev.off() 
#section 8: table A2: best fit line fits and thresholds ------ 
  #pull all individual threshold files and clean/organize 
    files <- list.files(data_save_path, pattern="threshold")  
  
    for(x in files){
      df <- read.csv(file.path(data_save_path, x))
      metric <- gsub(".csv", "", gsub("thresholds_", "", x))
      df$metric <- metric
      if(x == files[1]){
        thresholds <- df
      }else{
        thresholds <- rbind(thresholds, df)
      }} 
    
    #make equations look nice
    changeSciNot <- function(n) {
      if(is.na(n) == T){
        output <- NA
      }else if(n < 1000 & n > 0.1){
        output <- signif(n, 2)
      }else{
        unicode <- data.frame(num = as.character(1:11),
                              uni = c("\U00B9","\U00B2", "\U00B3","\U2074","\U2075",
                                      "\U2076", "\U2077", "\U2078", "\U2079", "\U00B9\U00B9", 
                                      "\U00B9\U2070"))
        output <- format(n, scientific = TRUE) #Transforms the number into scientific notation even if small
        output <- sub("e", "x10^", output) #Replace e with 10^
        sub <- str_split_i(output, "[/^]", i=2)
        start <- str_split_i(output, "[/^]", i=1)
        
       
        #convert supercript to unicode 
        sub<- sub("\\+0?", "", sub) #Remove + symbol and leading zeros on exponent, if > 1
        sub <- sub("-0?", "-", sub) #Leaves - symbol but removes leading zeros on exponent, if < 1
        
        exp <- sub("-", "",sub)
        exp <-  unicode$uni[unicode$num == exp]
        sub <- paste0("\U207B", exp)
        output <- paste0(start,sub)
      }
      
      output
    }
    convert_sci <- function(col){
      sapply(col, changeSciNot)
        
    }
    
    #create best fit lines from coefficients
    write_equation <- function(model, int, c1, c2,c3){
      if(model == "cubic"){
        eq <- paste0(int, " + ", c1, "P", " + ", c2, "P\U00B2", " + ", c3,"P\U00B3")
      }else if(model == "x+x2"){
        eq <- paste0(int, " + ", c1, "P", " + ", c2,"P\U00B2")
      }else if(model == "x2+x3"){
        eq <- paste0(int, " + ", c1, "P\U00B2", " + ", c2,"P\U00B3")
      }else if(model == "x4"){
        eq <- paste0(int, " + ", c1, "P\U2074")
      }else if(model == "x3"){
        eq <- paste0(int, " + ", c1, "P\U00B3")
      }else if(model == "linear"){
        eq <- paste0(int, " + ", c1, "P")
      }}
    
  #clean the table so it's closer to the right formatting for paper 
   df <- thresholds %>% select(metric, basin, sev, thresh, model, 
                               int, c1,c2,c3,r2,F_stat, p_val) %>% 
     mutate_at(vars(int:c3), ~ signif(.,2)) %>% 
     mutate_at(vars(r2, p_val), ~ signif(.,3)) %>% 
     mutate_at(vars(F_stat), ~ signif(.,3)) %>%
     mutate_at(vars(int:c3), ~ convert_sci(.))
   
   df$p_val[df$p_val < 0.001] <- "p < 0.001"
   df$basin <- gsub(",", ";", df$basin)
   df$thresh[is.na(df$thresh)] <- "-"
   
   df$equation <- NA
   for(x in 1:nrow(df)){
     df$equation[x] <- write_equation(df$model[x], df$int[x], df$c1[x], df$c2[x], df$c3[x])
   }
   
   df <- df %>% select(metric, basin, sev, thresh, equation,r2,F_stat, p_val)
  
   #make metric names nicer 
   df$metric <-   factor(df$metric, levels=c("rr", "avg_nitrate_mgL","nitrate_kg_yr","nitrate_rb",
                              "avg_doc_mgL","doc_kg_yr","doc_rb"), 
                      labels=c("Runoff Ratio",
                               "Average Nitrate Concentration",
                               "Annual Nitrate Load (10\u2074)",
                               "Nitrate Richard Baker Flashiness Index", 
                               "Average DOC Concentration",
                               "Annual DOC Load (10\u2074)",
                               "DOC Richard Baker Flashiness Index"), ordered = T)
   df <- df[order(df$metric),]
   
   df$sev[df$sev == "LOW"] <- "Low"
   df$sev[df$sev == "MOD"] <-  "Moderate"
   df$sev[df$sev == "HIGH"] <- "High"

   readr::write_excel_csv(df, file.path(data_save_path, "tablea2-theshold_fits.csv"))

#section 9: create table of calibration parameters (for data package)--------
  #load calibration parameters
   american <- "C:/SWAT/American River Simp2/American River Simp2/Scenarios/american river simp2.Sufi2.SwatCup"
   american_it <- "1-6-best-cal2-daily" #iteration file with calibrated parameters
   american_pars <- read_table(file.path(american, "iterations", american_it, "Sufi2.in/par_inf.txt"),col_names=F, skip=2)
   
   tule <- "C:/SWAT/Tule River Simp2/Tule River Simp2/Scenarios/tule river simp2.Sufi2.SwatCup"
   tule_it <- "1-17-best-cal-daily"
   tule_pars <- read_table(file.path(tule, "iterations", tule_it, "Sufi2.in/par_inf.txt"),col_names=F, skip=2)
   
  #tidy and combines 
    pars <- american_pars %>% select(-X3) %>% rename(parm = X1, american=X2) %>% 
      full_join(tule_pars %>% select(-X3) %>% rename(parm = X1, tule=X2), by="parm") 
    
    pars$change <- str_split_i(pars$parm, "__", i=1) #get type of change
    pars$name <- str_split_i(pars$parm, "__|[.]", i=2) #get parameter name
    pars$file <- paste0(".", str_split_i(pars$parm, "__|[.]", i=3)) #get file location
    pars$landuse <- str_split_i(pars$name, "[{]|[}]", i=2) #get landuse
    pars$name <- str_split_i(pars$name, "[{]|[}]", i=1) #get name without landuse
    
  #rename values for clarity
    change_table <- data.frame(change=c("r", "v", "a"), nice =c("relative (r)", "replace (v)", "absolute (a)"))
    landuse_table <- data.frame(landuse=c(7,8,15,16), nice=c("FRSD", "FRSE", "RNGE", "RNGB") )

    pars$change <- change_table$nice[match(pars$change, change_table$change)]    
    pars$landuse <- landuse_table$nice[match(pars$landuse, landuse_table$landuse)]
    pars$landuse[is.na(pars$landuse)] <- "All"
  
  #rearrange and sort 
    pars <- pars %>% select(change, file, name, landuse, american, tule)
    pars <- pars[order(pars$file, pars$landuse, pars$name),]
    
  #save for some manual manipulation 
    write.csv(pars, file.path("~/1_Research/0_Misc/rc_sfa-rc-3-wenas-modeling/data-package/inputs", "calibration-parameters.csv"), quote=F, row.names=F)
    