
library(tidyverse) 
library(wesanderson) 
library(lubridate) 
fulldata <- read_csv("~/Desktop/CSP_571_Project/final_data.csv", col_types = cols()) 
weather_data <- read_csv("~/Desktop/CSP_571_Project/weather_data_final.csv", col_types = cols()) 

fulldata 

## PRIMARY TYPE vs. COUNT 
others <- unique((fulldata %>% 
  group_by(Primary.Type) %>% 
  summarize(Count = n()) %>% 
  filter(Count <= 10))$Primary.Type) 

fulldata %>% 
  group_by(Primary.Type) %>% 
  summarize(Count = n()) %>% 
  mutate(Primary.Type = ifelse(Primary.Type %in% others, "OTHER", Primary.Type)) %>% 
  ggplot(aes(x = reorder(Primary.Type, Primary.Type, function(x) ifelse(x == "OTHER", 1000000000, -nrow(fulldata[fulldata$Primary.Type == x, ]))), 
             y = Count, fill = Primary.Type)) + 
    geom_col() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") + 
    labs(title = "Primary Type by Crime Count", 
          y = "Count", 
          x = "Primary Type (Count > 10)") 

## LOCATION DESCRIPTION vs. COUNT 
others <- unique((fulldata %>% 
            group_by(Location.Description) %>% 
            summarize(Count = n()) %>% 
            filter(Count <= 10))$Location.Description) 

fulldata %>% 
  group_by(Location.Description) %>% 
  summarize(Count = n()) %>% 
  mutate(Location.Description = ifelse(Location.Description %in% others, "OTHER", Location.Description)) %>% 
  ggplot(aes(x = reorder(Location.Description,-Count), y = Count, fill = Location.Description)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") + 
  labs(title = "Location by Crime Count", 
    y = "Count", 
    x = "Location (Count > 10)") 

## UNEMPLOYMENT RATE EFFECT ON NUMBER OF CRIMES 
unemployment_data <- fulldata %>% 
  group_by(Month, Year, Unemployment.Rate) %>% 
  summarize(Count = n()) 

unemployment_data %>% 
  ggplot(aes(x = Unemployment.Rate, y = Count)) + 
    geom_point() + 
    geom_smooth(method='lm',formula=y~x) + 
    labs(title = "Unemployment Rate vs. Crime Count", 
      subtitle = paste0("r = ", round(cor(unemployment_data$Unemployment.Rate, unemployment_data$Count), 3)), 
      y = "Monthly Crime Count", 
      x = "Monthly Unemployment Rate") 

## COMMON TYPES AND LOCATIONS 
common <- fulldata %>% 
  group_by(Primary.Type, Location.Description) %>% 
  summarize(Count = n()) %>% 
  arrange(-Count) %>% 
  filter(Count > 10) 

## CRIMES PER SEASON 
fulldata %>% 
  group_by(Season, Year) %>% 
  summarize(Count = n()) %>% 
  ggplot(aes(x = Season, y = Count, fill = Season)) + 
    geom_col() + 
    theme(legend.position = "none") + 
    scale_fill_manual(values = c("#CC5543", "#99CC00", "#FFFE6F", "#50A3C6")) + 
    labs(title = "Crime Count per Season") 

## ARRESTS AND LOCATIONS 
fulldata %>% 
  filter(Arrest == 1) %>% 
  group_by(Location.Description, Arrest) %>% 
  summarize(Count = n()) %>% 
  arrange(-Count) %>% 
  filter(Count > 10) %>% 
  ggplot(aes(x = reorder(Location.Description,-Count), y = Count, fill = Location.Description)) + 
    geom_col() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") + 
    labs(title = "Arrests per Location", 
         x = "Location", 
         y = "") 

## WEATHER? 
fulldata %>% 
  group_by(Date, avg_temp) %>% 
  summarize(Count = n()) %>% 
  ggplot(aes(x = avg_temp, y = Count)) + 
    geom_point() 

fulldata %>% 
  group_by(Date, avg_temp) %>% 
  summarize(Count = n()) %>% 
  filter(Count > 1) %>% 
  arrange(avg_temp) 

fulldata %>% 
  ggplot(aes(y = avg_temp)) + 
    geom_boxplot() 

fulldata %>% 
  ggplot(aes(x = avg_temp)) + 
  geom_density() + 
  labs(title = "Crime Density for Average Temperature", 
       y = "Density") + 
  xlab(expression(paste('Average Temperature [',''^'o','F',']'))) 

weather_data %>% 
  ggplot(aes(x = avg_temp)) + 
  geom_density() + 
  labs(title = "Weather Density for Average Temperature", 
       y = "Density") + 
  xlab(expression(paste('Average Temperature [',''^'o','F',']'))) 

fulldata %>% 
  ggplot(aes(y = max_temp)) + 
    geom_boxplot() 

fulldata %>% 
  ggplot(aes(x = max_temp)) + 
    geom_density() 

weather_data %>% 
  ggplot(aes(x = max_temp)) + 
  geom_density() 

fulldata %>% 
  ggplot(aes(y = min_temp)) + 
    geom_boxplot() 

fulldata %>% 
  ggplot(aes(x = min_temp)) + 
    geom_density() 

weather_data %>% 
  ggplot(aes(x = min_temp)) + 
  geom_density() 

## ENROLLMENT 
fulldata %>% 
  group_by(Semester, Year, Enrollment) %>% 
  mutate(Count = n()) %>% 
  ggplot(aes(x = Date, y = Enrollment)) + 
    geom_point() 

fulldata %>% 
  group_by(Month, Year) %>% 
  mutate(Count = n()) %>% 
  ggplot(aes(x = Date, y = Count)) + 
    geom_point() 

fulldata %>% 
  group_by(Semester, Year, Enrollment) %>% 
  mutate(Count = n()) %>% 
  ggplot(aes(x = Date, y = Enrollment)) + 
    geom_smooth() 

fulldata %>% 
  group_by(Month, Year) %>% 
  mutate(Count = n()) %>% 
  ggplot(aes(x = Date, y = Count)) + 
    geom_smooth() 

## QUADRANTS 
sum_of_crimes <- sum((fulldata %>% 
  group_by(Quadrant) %>% 
  summarize(Count = n()))$Count) 

fulldata %>% 
  group_by(Quadrant) %>% 
  summarize(Count = n(), CountP = n() / sum_of_crimes * 100) 

## PROPHET 
library(prophet) 

fullfulldata <- read_csv("~/Desktop/CSP_571_Project/full_final_data.csv", col_types = cols()) 
fullfulldata <- fullfulldata %>% 
  mutate(Year = year(Date)) 

## Create a training dataset. 
final_2016_2017 <- fullfulldata %>% 
  filter(Year == 2016 | Year == 2017) %>% 
  group_by(week(Date), Month, year(Date)) %>% 
  mutate(Count = n()) %>% 
  arrange(Date) 

## Create a testing dataset. 
testing_data <- fullfulldata %>% 
  filter(Year == 2018) %>% 
  group_by(week(Date), Month, year(Date)) %>% 
  mutate(Count = n()) %>% 
  arrange(Date) 

## Pipe the data into Prophet. 
pmod <- final_2016_2017 %>% 
  select(ds = Date, y = Count) %>% 
  prophet() 

## Generate a dataframe containing two years of dates more. 
future <- make_future_dataframe(pmod, periods = 365*1) 

## Predict some data! 
forecast <- predict(pmod, future) 

## Merge the testing datasets. 
test_forecast <- forecast %>% 
  filter(year(ds) == 2018) %>% 
  mutate(ds = as.Date(ds)) %>% 
  select(Date = ds, YHat = yhat, YHatLower = yhat_lower, YHatUpper = yhat_upper) 
test_forecast <- as.tibble(test_forecast) 
testing_data$Date <- as.Date(testing_data$Date) 
total_testing <- merge(testing_data, test_forecast, by = "Date") 

## Plot the data. 
total_testing %>% 
  ggplot(aes(x = Date)) + 
  geom_smooth(aes(y = Count, linetype = "Actual"), se = FALSE) + 
  geom_smooth(aes(y = YHat, linetype = "Predicted"), se = FALSE) + 
  theme(legend.title = element_blank(), legend.position="bottom") + 
  labs(title = "Actual vs. Predicted Trends using Prophet", 
       x = "", 
       y = "Crime Count per Week") 

rmse <- sqrt(sum((total_testing$YHat - total_testing$Count)^2) / nrow(total_testing)) 
rmse 
