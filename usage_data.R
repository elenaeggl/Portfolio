usage_data <- read_csv("C:\\Users\\elena\\Documents\\R\\R_Files\\CaseStudyBellabeat\\usage_data_joined.csv")

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

usage_data_long <- usage_data %>% 
  mutate(Id_new = 1:n()) %>% 
  select(Id_new, count_activity_days, count_sleep_days, count_weight_days) %>% 
  gather("log_type", "no_days_logged", -Id_new)

ggplot(data=usage_data_long, aes(x=Id_new, y=no_days_logged, fill=log_type)) + 
  geom_col(position="dodge") +
  labs(title = "Number of usage days for different logs", x = "User Id", y = "Number of days logged") +
  scale_fill_manual(values = c("#000066", "#6699FF", "#00FFFF"))

ggplot(data = usage_data_long, aes(x=no_days_logged, color=log_type)) +
  geom_histogram(position="dodge", color="blue", fill="blue", binwidth=3, alpha=0.7) +
  facet_wrap(~log_type)

