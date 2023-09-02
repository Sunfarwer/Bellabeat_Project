## insatall package
install.packages(c("tidyverse", "ggplot2", "dpyr"), repos = "http://cran.us.r-project.org")
library(tidyverse)
library(lubridate)
library(ggplot2)


##Import dataset
daily_activity <- read_csv("daily_activity_cleaned.csv")
sleep_day <- read_csv("sleep_day_cleaned.csv")
weight_log <- read_csv("weight_log_info_cleaned.csv")


##Explore data table
colnames(daily_activity)
head(daily_activity)
str(daily_activity)

colnames(sleep_day)
head(sleep_day)
str(sleep_day)

colnames(weight_log)
head(weight_log)
str(weight_log)

##check and clean data

names(daily_activity) <- tolower(names(daily_activity))
names(sleep_day) <- tolower(names(sleep_day))
names(weight_log) <- tolower(names(weight_log))

n_distinct(daily_activity$id)
n_distinct(sleep_day$id)
n_distinct(weight_log$id)

which(is.na(daily_activity))
which(is.na(sleep_day))
which(is.na(weight_log))

daily_activity <- daily_activity %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))

sleep_day <- sleep_day %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))

weight_log <- weight_log %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))

activity_sleep_daily <- merge(daily_activity, sleep_day, by=c('id', 'date'))
head(activity_sleep_daily)

activity_weight <- merge(daily_activity, weight_log, by=c('id', 'date'))
head(activity_weight)

## Analyze

### daily_activity

daily_activity %>%
  select(totalsteps,
         sedentaryminutes,
         calories) %>%
  summary()

### plot Total Steps vs Sedentary Minutes

ggplot(data = daily_activity)+
  geom_point(mapping = aes(x=totalsteps,y=sedentaryminutes, color=calories))+
  geom_smooth(mapping = aes(x=totalsteps,y=sedentaryminutes))+
  labs(x="Total Steps", y="Sedentary Minutes", title = "Total Steps vs Sedentary Minutes")

### sleep_day

sleep_day %>%
  select(totalsleeprecords,
         totalminutesasleep,
         totaltimeinbed) %>%
  summary()

### add weekday

activity_sleep_weekday <- activity_sleep_daily %>%
  mutate(weekday = weekdays(date))
activity_sleep_weekday$weekday <- ordered(activity_sleep_weekday$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                                                     "Friday", "Saturday", "Sunday"))
activity_weekday <- activity_sleep_weekday %>%
  group_by(weekday) %>%
  summarize (daily_steps = mean(totalsteps), daily_sleep = mean(totalminutesasleep), daily_calories=mean(calories))
head(activity_weekday)

activity_weigth_weekday <- activity_weight%>%
  mutate(weekday = weekdays(date))
activity_weigth_weekday$weekday <- ordered(activity_weigth_weekday$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                                                       "Friday", "Saturday", "Sunday"))
weight_ac_weekday <- activity_weigth_weekday %>%
  group_by(weekday) %>%
  summarize (daily_steps = mean(totalsteps), daily_calories=mean(calories), daily_bmi=mean(bmi))
head(activity_weekday)


### plot weekday

ggplot(data = activity_weekday)+
  geom_col(mapping = aes(weekday, daily_steps), fill="orange") +
  labs(x="Weekday", y="Daily Steps", title = "Weekday vs Daily Steps")+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.7, hjust = 0.5))

ggplot(data = activity_weekday)+
  geom_col(mapping = aes(weekday, daily_sleep), fill="blue") +
  labs(x="Weekday", y="Daily Sleep", title = "Weekday vs Daily sleep")+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.7, hjust = 0.5))

ggplot(data = activity_weekday)+
  geom_col(mapping = aes(weekday, daily_calories), fill="maroon") +
  labs(x="Weekday", y="Daily Calories", title = "Weekday vs Daily Calories")+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.7, hjust = 0.5))

ggplot(data = weight_ac_weekday)+
  geom_col(mapping = aes(weekday, daily_steps), fill="yellow") +
  labs(x="Weekday", y="Daily Steps", title = "Weekday vs Daily Steps")+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.7, hjust = 0.5))

ggplot(data = weight_ac_weekday)+
  geom_col(mapping = aes(weekday, daily_calories), fill="skyblue") +
  labs(x="Weekday", y="Daily Calories", title = "Weekday vs Daily Calories")+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.7, hjust = 0.5))

ggplot(data = activity_weigth_weekday)+
  geom_point(mapping = aes(x=totalsteps,y=bmi, color=calories))+
  labs(x="Total Steps", y="BMI", title = "Total Steps vs BMI")

ggplot(data = weight_ac_weekday)+
  geom_histogram(mapping = aes(weekday, daily_bmi), fill="maroon") +
  labs(x="Weekday", y="BMI", title = "Weekday vs BMI")+
  
  ### plot Total Steps vs BMI
  
  ggplot(data = activity_weight)+
  geom_point(mapping = aes(x=totalsteps,y=bmi), color="green")+
  labs(x="Total Steps", y="BMI", title = "Total Steps vs BMI (Body Mass Index)")