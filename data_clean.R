setwd("/Users/linlin/OneDrive/causal/project/")

home <- read.csv("City_Zri_AllHomesPlusMultifamily.csv")
airbnb <- read.csv("airbnb_listings_req.csv")

library(dplyr)
library(ggplot2)
library(MatchIt)
library(plm)
library(tidyr)
library(tidyverse)
library(sqldf)

#delete the Metro and Countyname column
home <- subset(home,select=-c(Metro,CountyName,SizeRank))

#gather the data
home1 <- gather(home,y_m, RI,-c(RegionID,RegionName,State))

#transform the y_m
home1$y_m <- str_sub(home1$y_m,-5,-1)

#tranform the city into lower case
home1$RegionName <- tolower(home1$RegionName)
home1$State <- toupper(home1$State)

home1$RegionName <- gsub("[[:space:]]*$","", home1$RegionName)
home1$State <- gsub("[[:space:]]*$","", home1$State)

#change the column name
colnames(home1)[2] <- "City"

#change the host since into date type
class(airbnb$Host.Since)
airbnb$Host.Since <- as.Date(airbnb$Host.Since,format = "%d-%m-%Y")

#remove NA
airbnb <- airbnb[!is.na(airbnb$Host.Since),]
airbnb <- airbnb[!is.na(airbnb$City),]

#change the case of cities
airbnb$City <- tolower(airbnb$City)
airbnb$State <- toupper(airbnb$State)

#remove the remaining words after  , / () - 
airbnb$City<-gsub("-.*","",airbnb$City)
airbnb$City<-gsub("/.*","",airbnb$City)
airbnb$City<-gsub("\\(.*","",airbnb$City)
airbnb$City<-gsub("\\).*","",airbnb$City)
airbnb$City<-gsub(",.*","",airbnb$City)

#remove the last space after the words
airbnb$City <- gsub("[[:space:]]*$","", airbnb$City)
airbnb$State <- gsub("[[:space:]]*$","", airbnb$State)

#getting the smallest host since for each city
airbnb_city <- airbnb %>% select(City,State,Host.Since) %>% group_by(City,State) %>% arrange() %>% slice(1L) %>% ungroup()

#merge the home1 and airbnb
#label the treatment
home_join1 <- sqldf('select h.*,ac.`host.since` from home1 h left join airbnb_city ac on h.City=ac.City and h.State=ac.State')

#label the time of treatment
#replace NA with 0
home_join1$Host.Since <- ifelse(is.na(home_join1$Host.Since) == TRUE, 999999999, home_join1$Host.Since)
options(digits = 4)
home_join1$y_m <- as.double(home_join1$y_m)
#change the host since into the float type
home_join1$time <- str_sub(home_join1$Host.Since,3,7)
#replace - with .
home_join1$time <- gsub("-",".",home_join1$time)
#change string to float
options(digits = 4)
home_join1$time <- as.double(home_join1$time)
home_join1$yr <- str_sub(home_join1$y_m,1,2)

#----- filter out those cities which have NA before 16.08 -------------------------
#filter out 10.09 - 11.07
home_join2 <- home_join1 %>% filter(y_m >= 11.08)

#first, how many cities in our dataset
city <- home_join2 %>% group_by(City) %>% count()
#7038 cities totally

city_na <- home_join2[is.na(home_join2$RI),]
city_na_count <- city_na %>% group_by(City) %>% count()
#5122 cities have NAs

#before 16.08, how many cities have NAs in RI
home_join_cut <- home_join2 %>% filter(y_m <= 16.08)
city_na_cut_count <- home_join_cut[is.na(home_join_cut$RI),]
city_na_cut_count <- city_na_cut_count %>% group_by(City) %>% count()
#4331 cities have NAs

#remove these 4331 cities from the dataset
cities <- city_na_cut_count$City
city_final <- home_join2[!home_join2$City %in% cities, ]
#check the number of cities left
city_count_check <- city_final %>% group_by(City) %>% count()
#2707 cities are left

#------for the NAs after the cut-off date---------------------------------------------
#check how many cities have NAs after this cut-off date
city_na_check <- city_final[is.na(city_final$RI),] %>% group_by(City) %>% count()
#791 cities have NAs

#first method, just impute the NAs with the average RI in that year

#second method, just delete those cities

#i will try the second method first

#-------delete the cities which have NAs--------------
#it should be 1916 cities left
cities2 <- city_na_check$City
city_left <- city_final[!city_final$City %in% cities2,]
city_left_check <- city_left %>% group_by(City) %>% count()
#1916



#fill RI NA with 0
#home_join1$RI <-  ifelse(is.na(home_join1$RI) == TRUE,0,home_join1$RI)

#home_join1[is.na(home_join1$RI),]

#city_mean <-sqldf('select City, State, yr, avg(RI) as avg_RI from home_join1 group by City, State, yr')
#merge it
#home_join <- sqldf('select h.*,c.avg_RI from home_join1 as h 
                   #left join city_mean as c 
                   #on h.city=c.city and h.state=c.state and h.yr=c.yr')
#home_join$RI <- ifelse(home_join$RI == 0, home_join$avg_RI,home_join$RI)

#write csv
write_csv(city_left,"city_left.csv")




