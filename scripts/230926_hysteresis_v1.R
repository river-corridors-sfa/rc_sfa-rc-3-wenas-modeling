## This script seeks to understand potential hysteresis between flow and materials
## (sediments, DOC NO3) to see if different fire treatments result in different
## hysteresis patterns (ie do we see shifts changes in rotation direction?)

require(pacman)
p_load(tidyverse, 
       lubridate,
       parsedate)

theme_set(theme_bw())

df <- read_csv("data/outputs/230729_output_datset_L1.csv") %>% 
  filter(percent != "base")
  
ggplot(df, aes(dates, flow_m3s_fire, color = percent)) + 
  geom_line() + 
  facet_wrap(~severity, ncol = 1)


## ID some storms
df1 <- df %>% 
  filter(dates > "2019-10-17" & 
           dates < "2019-10-28") %>% 
  mutate(day = as.numeric(dates - min(dates)))

df1_high = df1 %>% 
  filter(severity == "HIGH")

ggplot(df1_high, aes(dates, ))

ggplot(df1_high, aes(flow_m3s_fire, nit_mg_l_fire)) + 
  geom_path(aes(alpha = percent)) + 
  geom_point(aes(color = day), size = 2) + 
  scale_color_viridis_c(option = "A")







