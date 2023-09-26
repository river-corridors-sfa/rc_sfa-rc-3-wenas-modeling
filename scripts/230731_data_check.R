
require(pacman)
p_load(tidyverse)

load(file = "230729_output_datset_L1.rda")

burn_severity <- outputs_final %>% 
  group_by(percent, severity) %>% 
  summarize(x = mean(flow_m3s_fire, na.rm = T)) 

burn_severity_ordered <- burn_severity %>% 
  mutate(percent = as.numeric(percent)) %>% #simple way to reorder as a factor
  mutate(percent = as.factor(percent)) %>% 
  filter(!is.na(percent)) %>%
  mutate(severity = fct_relevel(severity, c("LOW", "MOD", "HIGH")))

ggplot(burn_severity_ordered, aes(percent, x, fill = severity)) + 
  geom_col(position = "dodge")


df <- outputs_final %>% 
  filter(percent == "10" | percent == "100") 

ggplot(df, aes(flow_m3s_fire, doc_mg_l_fire)) + 
  geom_point() + 
  facet_wrap(~severity) + 
  scale_x_log10() + 
  scale_y_log10()

ggplot(df, aes(doc_mg_l_fire, color = severity, fill = severity)) + 
  geom_density(alpha = 0.2) + 
  scale_x_log10() + 
  facet_wrap(~percent)




