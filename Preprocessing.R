
library(tidyverse) 
library(lubridate) 
final <- read_csv("~/Desktop/CSP_571_Project/Crimes.csv", col_types = cols()) 
summary(final) 
head(final) 


## REMOVING UNECESSARY FEATURES 
features_to_keep <- list("ID", "Case Number", "IUCR", "Beat", "District", "Ward", "Community Area", "FBI Code", "Year", "Updated On", 
          "Latitude", "Longitude", "Location", "Block") 
final <- final[, !(colnames(final) %in% features_to_keep)] # data without unnecessary features 
names(final) <- c("Date", "Primary.Type", "Description", "Location.Description", "Arrest", "Domestic", "X.Coordinate", "Y.Coordinate") 


## ONLY IIT 
final <- subset(final, X.Coordinate > 1175454 & X.Coordinate < 1177743 & Y.Coordinate > 1881708 & Y.Coordinate < 1884911) 


## QUADRANTS 
x_min <- min(final$X.Coordinate) 
x_max <- max(final$X.Coordinate) 
x_mid <- round((x_min + x_max)/2) 

y_min <- min(final$Y.Coordinate) 
y_max <- max(final$Y.Coordinate) 
y_mid_1 <- y_min + round((y_max - y_min)/3) 
y_mid_2 <- y_min + round(2*(y_max - y_min)/3) 

final$Quadrant <- -1 
final$Quadrant<- ifelse(final$X.Coordinate >= x_min & final$X.Coordinate < x_mid & 
                         final$Y.Coordinate >= y_mid_2 & final$Y.Coordinate <= y_max, 1, final$Quadrant) 
final$Quadrant<- ifelse(final$X.Coordinate >= x_min & final$X.Coordinate < x_mid & 
                         final$Y.Coordinate >= y_mid_1 & final$Y.Coordinate < y_mid_2, 2, final$Quadrant) 
final$Quadrant<- ifelse(final$X.Coordinate >= x_min & final$X.Coordinate < x_mid & 
                         final$Y.Coordinate >= y_min & final$Y.Coordinate < y_mid_1, 3, final$Quadrant) 
final$Quadrant<- ifelse(final$X.Coordinate >= x_mid & final$X.Coordinate <= x_max & 
                         final$Y.Coordinate >= y_mid_2 & final$Y.Coordinate <= y_max, 4, final$Quadrant) 
final$Quadrant<- ifelse(final$X.Coordinate >= x_mid & final$X.Coordinate <= x_max & 
                         final$Y.Coordinate >= y_mid_1 & final$Y.Coordinate < y_mid_2, 5, final$Quadrant) 
final$Quadrant<- ifelse(final$X.Coordinate >= x_mid & final$X.Coordinate <= x_max & 
                         final$Y.Coordinate >= y_min & final$Y.Coordinate < y_mid_1, 6, final$Quadrant) 

final <- final[, !(colnames(final) %in% c("X.Coordinate", "Y.Coordinate"))] # we don't need these anymore - they have served their purpose 
head(final) 


## PARAMETER SPECIFICATION 
final$Primary.Type <- as.character(final$Primary.Type) 
final$Description <- as.character(final$Description) 
final$Location.Description <- as.character(final$Location.Description) 
final$Arrest <- as.factor(final$Arrest) 
levels(final$Arrest) <- c(0, 1) 
final$Domestic <- as.factor(final$Domestic) 
levels(final$Domestic) <- c(0, 1) 
final$Quadrant <- as.factor(final$Quadrant) 

final <- na.omit(final) 
final 


## DATE CONVERSION 
final$Date 
final$Date <- as.POSIXct(final$Date, format="%m/%d/%Y %I:%M:%S %p") 
final$Day <- as.factor(weekdays(final$Date, abbreviate = TRUE)) 
final$Month <- as.factor(months(final$Date, abbreviate = TRUE)) 


## CONDENSING PRIMARY TYPE 
final$Primary.Type <- ifelse(final$Primary.Type %in% c("CRIM SEXUAL ASSAULT",
                                                             "PROSTITUTION", "SEX OFFENSE"), 'SEX', final$Primary.Type) 
final$Primary.Type <- ifelse(final$Primary.Type %in% c("MOTOR VEHICLE THEFT"), "GTA", final$Primary.Type) 
final$Primary.Type <- ifelse(final$Primary.Type %in% c("GAMBLING", "INTERFERE WITH PUBLIC OFFICER", "INTERFERENCE WITH PUBLIC OFFICER", 
                                                             "INTIMIDATION", "LIQUOR LAW VIOLATION", "OBSCENITY", "NON-CRIMINAL", "PUBLIC PEACE VIOLATION", 
                                                             "PUBLIC INDECENCY", "STALKING", "NON-CRIMINAL (SUBJECT SPECIFIED)"), 
                                "NON-VIOLENT", final$Primary.Type) 
final$Primary.Type <- ifelse(final$Primary.Type == "CRIMINAL DAMAGE", "DAMAGE", final$Primary.Type) 
final$Primary.Type <- ifelse(final$Primary.Type == "CRIMINAL TRESPASS", "TRESPASS", final$Primary.Type) 
final$Primary.Type <- ifelse(final$Primary.Type %in% c("NARCOTICS", "OTHER
                                                      NARCOTIC VIOLATION", "OTHER NARCOTIC VIOLATION"), "DRUGS", final$Primary.Type) 
final$Primary.Type <- ifelse(final$Primary.Type == "DECEPTIVE PRACTICE", "FRAUD", final$Primary.Type) 
final$Primary.Type <- ifelse(final$Primary.Type %in% c("BATTERY", "KIDNAPPING", "WEAPONS VIOLATION", "OFFENSE INVOLVING CHILDREN"), 
                                "VIOLENT", final$Primary.Type) 
final$Primary.Type <- ifelse(final$Primary.Type %in% c("OTHER OFFENSE", "OTHER OFFENSE"), "OTHER", final$Primary.Type) 

## final condensing 
final$Primary.Type <- ifelse(final$Primary.Type %in% c("FRAUD", "THEFT", "DRUGS", "TRESPASS", "VIOLENT"), final$Primary.Type, "OTHER") 

unique(final$Primary.Type) 

final %>% 
  group_by(Primary.Type) %>% 
  summarize(Count = n()) %>% 
  filter(Count < 10) %>% 
  arrange(Count) 


## CONDENSING DESCRIPTION 
final$Description <- ifelse(grepl("AGG", final$Description), "AGGRAVATED", final$Description) 
final$Description <- ifelse(grepl("ARM", final$Description), "ARMED", final$Description) 
final$Description <- ifelse(grepl("POSS", final$Description), "POSSESSION", final$Description) 
final$Description <- ifelse(grepl("NARCO", final$Description), "POSSESSION", final$Description) 
final$Description <- ifelse(grepl("ATT", final$Description), "ATTACK", final$Description) 
final$Description <- ifelse(grepl("GUN OFFENDER", final$Description), "GUN OFFENDER", final$Description) 
final$Description <- ifelse(grepl("THEFT", final$Description), "THEFT", final$Description) 
final$Description <- ifelse(grepl("POCKET", final$Description), "THEFT", final$Description) 
final$Description <- ifelse(grepl("STOLEN", final$Description), "THEFT", final$Description) 
final$Description <- ifelse(grepl("MANU", final$Description), "DRUG DELIVERY", final$Description) 
final$Description <- ifelse(final$Description %in% c("EXTORTION", "PURSE-SNATCHING"), "THEFT", final$Description) 
final$Description <- ifelse(final$Description %in% c("COMPUTER FRAUD", "COUNTERFEIT CHECK", "FRAUD OR CONFIDENCE GAME", "COUNTERFEITING DOCUMENT", 
                                                                       "CREDIT CARD FRAUD", "DECEPTIVE COLLECTION PRACTICES"), 
                                     "FRAUD", final$Description) 
final$Description <- ifelse(final$Description %in% c("GUN OFFENDER", "OTHER WEAPONS VIOLATION", "UNLAWFUL USE/SALE AIR RIFLE", 
                                                     "UNLAWFUL USE HANDGUN", "UNLAWFUL USE OTHER DANG WEAPON"), 
                            "WEAPON", final$Description) 
final$Description <- ifelse(final$Description %in% c("$500 AND UNDER", "OVER $500"), "$ DAMAGE", final$Description) 
final$Description <- ifelse(final$Description %in% c("AUTOMOBILE", "CYCLE, SCOOTER, BIKE NO VIN", "VEHICULAR HIJACKING", "FALSE/STOLEN/ALTERED TRP", 
                                                     "OTHER VEHICLE OFFENSE", "TRUCK, BUS, MOTOR HOME", "VEHICLE TITLE/REG OFFENSE"), 
                            "$ DAMAGE", final$Description) 
final$Description <- ifelse(final$Description %in% c("ANIMAL ABUSE/NEGLECT", "FINAN EXPLOIT-ELDERLY/DISABLED", "FORFEIT PROPERTY", 
                                                     "HOME INVASION", "INTIMIDATION", "MOB ACTION", "OBSCENE TELEPHONE CALLS", 
                                                     "PAROLE VIOLATION",  "UNAUTHORIZED VIDEOTAPING", "PUBLIC INDECENCY", 
                                                     "OBSTRUCTING IDENTIFICATION", "VIOLATE ORDER OF PROTECTION"), 
                            "OTHER", final$Description) 
final$Description <- ifelse(grepl("CHILD", final$Description), "MINOR", final$Description) 
final$Description <- ifelse(grepl("MURDER", final$Description), "MURDER", final$Description) 
final$Description <- ifelse(grepl("HAND", final$Description), "VIOLENCE", final$Description) 
final$Description <- ifelse(grepl("MINOR", final$Description), "MINOR", final$Description) 
final$Description <- ifelse(grepl("SEX", final$Description), "SEX", final$Description) 
final$Description <- ifelse(grepl("THREAT", final$Description), "THREAT", final$Description) 
final$Description <- ifelse(grepl("HARASSMENT", final$Description), "HARASSMENT", final$Description) 
final$Description <- ifelse(grepl("OTHER", final$Description), "OTHER", final$Description) 
final$Description <- ifelse(grepl("UNLAW", final$Description), "UNLAWFUL", final$Description) 
final$Description <- ifelse(grepl("TO ", final$Description), "TO SOMETHING", final$Description) 
final$Description <- ifelse(grepl("FROM ", final$Description), "FROM SOMETHING", final$Description) 

sort(unique(final$Description)) 

final %>% 
  group_by(Description) %>% 
  summarize(Count = n()) %>% 
  filter(Count < 10) %>% 
  arrange(Count) 



## CONDENSING LOCATION DESCRIPTION 
final$Location.Description <- ifelse(grepl("CTA", final$Location.Description), "CTA", final$Location.Description) 
final$Location.Description <- ifelse(grepl("SCHOOL", final$Location.Description), "SCHOOL", final$Location.Description) 
final$Location.Description <- ifelse(grepl("COLLEGE", final$Location.Description), "SCHOOL", final$Location.Description) 
final$Location.Description <- ifelse(grepl("LIBRARY", final$Location.Description), "SCHOOL", final$Location.Description) 
final$Location.Description <- ifelse(grepl("SIDEWALK", final$Location.Description), "STREET", final$Location.Description) 
final$Location.Description <- ifelse(final$Location.Description %in% c("VEHICLE NON-COMMERCIAL", "TAXICAB", 
                                                                       "VEHICLE - OTHER RIDE SHARE SERVICE (E.G., UBER, LYFT)", "VEHICLE-COMMERCIAL"), 
                                     "VEHICLE", final$Location.Description) 
final$Location.Description <- ifelse(final$Location.Description %in% c("DEPARTMENT STORE", "APPLIANCE STORE", "DRUG STORE", 
                                                                       "CONVENIENCE STORE", "GROCERY FOOD STORE", "SMALL RETAIL STORE"), 
                                     "STORE", final$Location.Description) 
final$Location.Description <- ifelse(grepl("STADIUM", final$Location.Description), "SOX STADIUM", final$Location.Description) 
final$Location.Description <- ifelse(final$Location.Description %in% c("APARTMENT", "RESIDENCE PORCH/HALLWAY", "CHA HALLWAY/STAIRWELL/ELEVATOR", 
                                                                       "CHA APARTMENT", "RESIDENTIAL YARD (FRONT/BACK)", "RESIDENCE",
                                                                       "NURSING HOME/RETIREMENT HOME", "RESIDENCE-GARAGE"), 
                                     "HOUSING", final$Location.Description) 
final$Location.Description <- ifelse(final$Location.Description %in% c("BAR OR TAVERN", "TAVERN/LIQUOR STORE"), "BAR", final$Location.Description) 
final$Location.Description <- ifelse(final$Location.Description %in% c("MEDICAL/DENTAL OFFICE", "COMMERCIAL / BUSINESS OFFICE"), "BAR", final$Location.Description) 
final$Location.Description <- ifelse(final$Location.Description %in% c("ATHLETIC CLUB", "POOL ROOM"), "GYM", final$Location.Description) 
final$Location.Description <- ifelse(final$Location.Description %in% c("CHA PARKING LOT/GROUNDS", "PARKING LOT/GARAGE(NON.RESID.)"), 
                                     "PARKING LOT", final$Location.Description) 
final$Location.Description <- ifelse(final$Location.Description %in% c("ANIMAL HOSPITAL", "HOSPITAL BUILDING/GROUNDS"), "MEDICAL", final$Location.Description) 
final$Location.Description <- ifelse(final$Location.Description %in% c("GOVERNMENT BUILDING/PROPERTY", "FEDERAL BUILDING"), 
                                     "GOVERNMENT", final$Location.Description) 
final$Location.Description <- ifelse(grepl("ATM", final$Location.Description), "ATM", final$Location.Description) 
final$Location.Description <- ifelse(grepl("OTHER", final$Location.Description), "OTHER", final$Location.Description) 
final$Location.Description <- ifelse(grepl("FACTORY", final$Location.Description), "OTHER", final$Location.Description) 
final$Location.Description <- ifelse(grepl("BOAT", final$Location.Description), "OTHER", final$Location.Description) 
final$Location.Description <- ifelse(grepl("FIRE", final$Location.Description), "OTHER", final$Location.Description) 
final$Location.Description <- ifelse(grepl("HIGHWAY", final$Location.Description), "HIGHWAY", final$Location.Description) 
final$Location.Description <- ifelse(grepl("PARK", final$Location.Description), "PARK", final$Location.Description) 
final$Location.Description <- ifelse(grepl("HOTEL", final$Location.Description), "HOTEL", final$Location.Description) 

unique(final$Location.Description) 

final %>% 
  group_by(Location.Description) %>% 
  summarize(Count = n()) %>% 
  filter(Count < 10) %>% 
  arrange(Count) 



## ADDING SEASON 
final$Season <- as.factor(ifelse(final$Month %in% c("Mar", "Apr", "May"), "Spring", 
                                      ifelse(final$Month %in% c("Jun", "Jul", "Aug"), "Summer", 
                                      ifelse(final$Month %in% c("Sep", "Oct", "Nov"), "Fall", "Winter")))) 



## ADDING TIME 
final$Time <- as.factor(ifelse(hour(final$Date) >= 6 & hour(final$Date) < 18, "Day", "Night")) 

final <- final %>% arrange(Date) 

write.csv(final, file = "~/Desktop/CSP_571_Project/full_final_data.csv", row.names=FALSE) 

## MERGING WITH WEATHER DATA 
weather <- read_csv("~/Desktop/CSP_571_Project/chicago_weather_data.csv", col_types = cols()) 
head(weather) 
names(weather) <- c("STATION", "NAME", "Date", "max_temp", "min_temp", "TOBS") 
weather <- weather[, !(colnames(weather) %in% c("STATION", "NAME", "TOBS"))] # bye bye features we don't need 
weather$avg_temp <- (weather$max_temp + weather$min_temp) / 2 
weather$Date <- as.POSIXct(weather$Date, format="%m/%d/%y") 
weather$max_temp <- as.integer(weather$max_temp) 
weather$min_temp <- as.integer(weather$min_temp) 
weather$avg_temp <- as.integer(weather$avg_temp) 

weather 

write.csv(weather, file = "~/Desktop/CSP_571_Project/weather_data_final.csv", row.names=FALSE) 

final$Date_mod <- format(final$Date, '%Y-%m-%d') 
weather$Date_mod <- format(weather$Date,'%Y-%m-%d') 

final <- left_join(final, weather, by=c("Date_mod")) 

final <- final[, !(colnames(final) %in% c("Date_mod", "Date.y"))] # bye bye features we don't need 
colnames(final)[1] <- "Date" 

final <- na.omit(final) 
final 



## MERGING WITH UNEMPLOYMENT RATE 
unemployment <- read_csv("~/Desktop/CSP_571_Project/unemployment_rate.csv", col_types = cols()) 
head(unemployment) 
unemployment_gathered <- gather(unemployment, Month, Unemployment.Rate, -Year) 
unemployment_gathered$Month <- match(unemployment_gathered$Month, month.abb) 
tail(unemployment_gathered) 

final$Month <- as.integer(month(final$Date)) 
final$Year <- as.integer(year(final$Date)) 

final <- left_join(final, unemployment_gathered, by=c("Month", "Year")) 

final <- na.omit(final) 
final 



## IIT SEMESTER AND STUDENT ENROLLMENT 
final$Semester <- "NONE?" 
final$Semester[month(final$Date) >= 1 & month(final$Date) <= 5] <- "Spring" 
final$Semester[(day(final$Date) < 16 & month(final$Date) == 8) |  month(final$Date) >= 6 & month(final$Date) <= 7] <- "Summer" 
final$Semester[(day(final$Date) >= 16 & month(final$Date) == 8) |  month(final$Date) >= 9 & month(final$Date) <= 12] <- "Fall" 

final$Semester <- as.factor(final$Semester) 
levels(final$Semester) <- c("Spring", "Summer", "Fall") 

final$Enrollment <- -1 

year <- 2008 
final[final$Semester == "Spring" & final$Year == year, ]$Enrollment <- 6971 # Spring 
final[final$Semester == "Summer" & final$Year == year, ]$Enrollment <- 2106 # Summer 
final[final$Semester == "Fall" & final$Year == year, ]$Enrollment <- 7613 # Fall 

year <- 2009 
final[final$Semester == "Spring" & final$Year == year, ]$Enrollment <- 7135 # Spring 
final[final$Semester == "Summer" & final$Year == year, ]$Enrollment <- 1974 # Summer 
final[final$Semester == "Fall" & final$Year == year, ]$Enrollment <- 7707 # Fall 

year <- 2010 
final[final$Semester == "Spring" & final$Year == year, ]$Enrollment <- 7290 # Spring 
final[final$Semester == "Summer" & final$Year == year, ]$Enrollment <- 2120 # Summer 
final[final$Semester == "Fall" & final$Year == year, ]$Enrollment <- 7774 # Fall 

year <- 2011 
final[final$Semester == "Spring" & final$Year == year, ]$Enrollment <- 7366 # Spring 
final[final$Semester == "Summer" & final$Year == year, ]$Enrollment <- 2224 # Summer 
final[final$Semester == "Fall" & final$Year == year, ]$Enrollment <- 7787 # Fall 

year <- 2012 
final[final$Semester == "Spring" & final$Year == year, ]$Enrollment <- 7466 # Spring 
final[final$Semester == "Summer" & final$Year == year, ]$Enrollment <- 2363 # Summer 
final[final$Semester == "Fall" & final$Year == year, ]$Enrollment <- 7684 # Fall 

year <- 2013 
final[final$Semester == "Spring" & final$Year == year, ]$Enrollment <- 7482 # Spring 
final[final$Semester == "Summer" & final$Year == year, ]$Enrollment <- 2319 # Summer 
final[final$Semester == "Fall" & final$Year == year, ]$Enrollment <- 7819 # Fall 

year <- 2014 
final[final$Semester == "Spring" & final$Year == year, ]$Enrollment <- 7547 # Spring 
final[final$Semester == "Summer" & final$Year == year, ]$Enrollment <- 2260 # Summer 
final[final$Semester == "Fall" & final$Year == year, ]$Enrollment <- 4799 # Fall 

year <- 2015 
final[final$Semester == "Spring" & final$Year == year, ]$Enrollment <- 4583 # Spring 
final[final$Semester == "Summer" & final$Year == year, ]$Enrollment <- 1426 # Summer 
final[final$Semester == "Fall" & final$Year == year, ]$Enrollment <- 4801 # Fall 

year <- 2016 
final[final$Semester == "Spring" & final$Year == year, ]$Enrollment <- 4471 # Spring 
final[final$Semester == "Summer" & final$Year == year, ]$Enrollment <- 1324 # Summer 
final[final$Semester == "Fall" & final$Year == year, ]$Enrollment <- 7809 # Fall 

year <- 2017 
final[final$Semester == "Spring" & final$Year == year, ]$Enrollment <- 4441 # Spring 
final[final$Semester == "Summer" & final$Year == year, ]$Enrollment <- 1266 # Summer 
final[final$Semester == "Fall" & final$Year == year, ]$Enrollment <- 7266 # Fall 

year <- 2018 
final[final$Semester == "Spring" & final$Year == year, ]$Enrollment <- 6792 # Spring 
final[final$Semester == "Summer" & final$Year == year, ]$Enrollment <- 1886 # Summer 
final[final$Semester == "Fall" & final$Year == year, ]$Enrollment <- 6840 # Fall 

unique(final$Semester) 
unique(final$Enrollment) 



## WRITING TO CSV FILE 
write.csv(final, file = "~/Desktop/CSP_571_Project/final_data.csv", row.names=FALSE) 
