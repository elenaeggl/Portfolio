activities_original <- read_csv("C:\\Users\\elena\\Documents\\R\\R_Files\\CaseStudyBellabeat\\tables_joined.csv")

activities <- activities_original %>% 
  mutate(Id_new = case_when(
    Id == 1503960366 ~ "1",
    Id == 1624580081 ~ "2",
    Id == 1644430081 ~ "3",
    Id == 1844505072 ~ "4",
    Id == 1927972279 ~ "5",
    Id == 2022484408 ~ "6",
    Id == 2026352035 ~ "7",
    Id == 2320127002 ~ "8",
    Id == 2347167796 ~ "9",
    Id == 2873212765 ~ "10",
    Id == 3372868164 ~ "11",
    Id == 3977333714 ~ "12",
    Id == 4020332650 ~ "13",
    Id == 4057192912 ~ "14",
    Id == 4319703577 ~ "15",
    Id == 4388161847 ~ "16",
    Id == 4445114986 ~ "17",
    Id == 4558609924 ~ "18",
    Id == 4702921684 ~ "19",
    Id == 5553957443 ~ "20",
    Id == 5577150313 ~ "21",
    Id == 6117666160 ~ "22",
    Id == 6290855005 ~ "23",
    Id == 6775888955 ~ "24",
    Id == 6962181067 ~ "25",
    Id == 7007744171 ~ "26",
    Id == 7086361926 ~ "27",
    Id == 8053475328 ~ "28",
    Id == 8253242879 ~ "29",
    Id == 8378563200 ~ "30",
    Id == 8583815059 ~ "31",
    Id == 8792009665 ~ "32",
    Id == 8877689391 ~ "33"))

activities %>% 
  select(ActivityDate, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes) %>% 
  group_by(ActivityDate) %>% 
  summarize(mean4_very_active = mean(VeryActiveMinutes), mean3_fairly_active = mean(FairlyActiveMinutes), 
            mean2_lightly_active = mean(LightlyActiveMinutes), mean1_sedentary = mean(SedentaryMinutes)) %>% 
  gather("intensity", "minutes_logged", -ActivityDate) %>% 
  ggplot(aes(x= ActivityDate, y = minutes_logged, fill=intensity)) + geom_area()

activities %>% 
  group_by(dayofweek) %>% 
  summarize(mean_total_distance = mean(TotalDistance), mean_total_steps = mean(TotalSteps), 
            mean_calories = mean(Calories)) %>% 
  gather("measure", "value", -dayofweek) %>% 
  ggplot(aes(x=dayofweek, y=value, color=measure, fill = measure)) + 
  geom_col() + facet_wrap(~measure, scale = "free")

activities %>% 
  group_by(Id_new) %>% 
  select(Id_new, TotalSteps) %>%
  summarize(mean_steps_per_day = mean(TotalSteps)) %>% 
  ggplot(aes(x=Id_new, y=mean_steps_per_day)) + 
  geom_col() +
  labs(title = "Mean Steps per Day by Participant", x = "User Id", y = "Number of steps")

activities %>% 
  group_by(Id_new) %>% 
  select(Id_new, TotalSteps) %>%
  summarize(mean_steps_per_day = mean(TotalSteps), count_logged = n()) %>% 
  ggplot(aes(x=count_logged, y=mean_steps_per_day)) + 
  geom_point() +
  stat_smooth(method="lm") +
  labs(title = "Mean Steps per Day vs. Number of Days logged", x = "Number of days logged", y = "Number of steps")

activities %>% 
  select(Id, ActivityDate, dayofweek, TotalMinutesAsleep, TotalTimeInBed) %>% 
  drop_na() %>% 
  ggplot(aes(x=TotalTimeInBed, y=TotalMinutesAsleep)) + 
  geom_point() +
  stat_smooth(method="lm") +
  labs(title = "Minutes asleep vs. time in bed", x = "Minutes in bed", y = "Minutes asleep")


activities %>% 
  select(Id_new, ActivityDate, dayofweek, TotalMinutesAsleep, TotalTimeInBed) %>% 
  drop_na() %>% 
  group_by(dayofweek) %>% 
  summarize(mean_sleep_hours = mean(TotalMinutesAsleep)/60) %>% 
  ggplot(aes(x=dayofweek, y=mean_sleep_hours)) + 
  geom_col() +
  labs(title = "Sleep hours by day of the week", x = "Day of the Week", y = "Sleep hours")


test <- activities %>% 
  select(Id_new, ActivityDate, dayofweek, TotalMinutesAsleep, TotalTimeInBed) %>% 
  drop_na() %>% 
  group_by(Id_new) %>% 
  summarize(mean_sleep_hours = mean(TotalMinutesAsleep)/60) %>% 
  ggplot(aes(x=Id_new, y=mean_sleep_hours)) + 
  geom_col() +
  labs(title = "Sleep hours by participant", x = "Id", y = "Sleep hours")

activities %>% 
  select(Id_new, ActivityDate, WeightKg) %>% 
  arrange(Id_new) %>% 
  drop_na() %>% 
  ggplot(aes(x=ActivityDate, y=WeightKg, color=Id_new)) +
  geom_point() +
  labs(title = "Weight log by participant", x = "Date", y = "Weight in Kg")

  