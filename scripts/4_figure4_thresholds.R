## This script explores how to identify thresholds through a couple different
## methods. The first approach will be 
##
## 2023-10-17
## Peter Regier
## 
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

require(pacman)
p_load(tidyverse,
       cowplot)

## Set ggplot theme and color palette
theme_set(theme_bw())
#fire_colors = c("#F5C63C", "#F47F6B", "#BB5098")
severity_colors = c("#3B9AB2", "#E1AF00", "#F21A00")


# 2. Read in data --------------------------------------------------------------

df <- read_csv("data/outputs/2023-10-10_output_datset_L2_means.csv") 