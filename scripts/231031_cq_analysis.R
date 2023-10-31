## C-Q analyses to see if there are different relationships
##

# 1. Setup ---------------------------------------------------------------------

require(pacman)
p_load(tidyverse,
       lubridate,
       cowplot)

## Set ggplot theme and color palette
theme_set(theme_bw())
#fire_colors = c("#F5C63C", "#F47F6B", "#BB5098")
severity_colors = c("#3B9AB2", "#E1AF00", "#F21A00")


# 2. Read in data --------------------------------------------------------------

df <- read_csv("data/outputs/2023-10-05_output_datset_L1.csv") %>% 
  filter(percent != "base") %>% 
  mutate(percent = as.factor(as.numeric(percent)), 
         severity = factor(severity, levels = c("LOW", "MOD", "HIGH")))

df %>% 
  mutate(month = month(dates)) %>% 
  group_by(percent, severity, month) %>% 
  summarize(flow_fire = mean(flow_m3s_fire), 
            sed_fire = mean(sed_mg_l_fire)) %>% 
ggplot(aes(flow_fire, sed_fire, color = percent)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = F) + 
  facet_wrap(~severity) + 
  scale_x_log10() + 
  scale_y_log10()
ggsave("figures/231031_cq_sed_does_not_vary.png", 
       width = 10, height = 4)


