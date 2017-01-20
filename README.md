# NYC-Taxi-Data-Analysis

Using NYC Taxi data available on the NYC Taxi and Limousine Commission website, we intend to analyze the geography of pickup and drop-offs made by the cabs during peak hours of the day. The data set includes 11.2 million trips made during the month of June 2016 and drills down each trip to particulars like pickup and drop-off dates, time, longitude, latitude, trip distance, fare amount, etc. We aim to identify the area that is the busiest during the peak hours and target the average commute time during weekdays and weekends. This would give us a sense of how much time it takes for a person to get from Point A to Point B with and without traffic (assuming weekends have little or no traffic as compared to weekdays).

The data has been extracted from http://www.nyc.gov/html/tlc/html/about/trip_record_data.shtml

Using NYC Taxi data available on the NYC Taxi and Limousine Commission website, we intend to analyze the geography of pickup and drop-offs made by the cabs during peak hours of the day. The data set includes 11.2 million trips made during the month of June 2016 and drills down each trip to particulars like pickup and drop-off dates, time, longitude, latitude, trip distance, fare amount, etc. We aim to identify the area that is the busiest during the peak hours and target the average commute time during weekdays and weekends. This would give us a sense of how much time it takes for a person to get from Point A to Point B with and without traffic (assuming weekends have little or no traffic as compared to weekdays).

####Install packages: "ggplot2" and "lubridate"
options(warn=-1)
library(ggplot2)
library(hexbin)
library(readr)
library(dplyr)
library(extrafont)
library(scales)
library(grid)
library(RColorBrewer)
library(digest)
library(stringr)
library(methods) 
library(lubridate)

options(repr.plot.mimetypes = 'image/png', repr.plot.width=4, repr.plot.height=3, repr.plot.res=300)

df <- read.csv("C:/Users/Ushnik/Downloads/yellow_tripdata_2016-06.csv", header=T)
str(df)
head(df)
The data set includes 11135470 obs. of 19 variables

Data Filtering

We filter the data set to analyze peak hours of the morning. Assuming 8 AM to 9 AM to be the peak travelling hour for people going to work, we rewrite the data set with the filter. We then convert the pick up and drop-off dates to the day of the week. Finally we include the relevant data and create the data set we work with. The resulting data has a few latitude and longitude outliers, and we therefore establish the limits of our plot within NYC coordinates: Minimum Latitude = 40.5774, Maximum Latitude = 40.9176, Minimum Longitude = -74.15, Maximum Longitude = -73.7004.

df<- with(df,df[ hour (df$tpep_pickup_datetime)>=8 & hour(df$tpep_pickup_datetime)< 9 & hour (df$tpep_dropoff_datetime)>=8 & hour(df$tpep_dropoff_datetime)< 9 , ] )
head(df,10)
summary(df)
View(df)

df$day<- strptime(df[,2],"%Y-%m-%d")
df$day<-weekdays(df$day)
df1 <- subset(df,df$weekday == "Saturday" | df$weekday == "Sunday")
df2 <- subset(df,df$weekday != "Saturday" & df$weekday != "Sunday")
View(df)

df<-data.frame(df[c(2,3,5:7,10,11,19,20)])
head(df,10)

df<- df %>% filter(df$trip_distance < 5)
View(df) 

sprintf("# of Rows in Dataframe: %s", nrow(df)) 
sprintf("Dataframe Size: %s", format(object.size(df), units = "MB")) 

min_lat <- 40.5774 
max_lat <- 40.9176 
min_long <- -74.15
max_long <- -73.7004
The filtered data set now consists of 9.4 million entries.

Establishing Plot themes and parameters

We create a theme for our plots with a black background and establish plot margins, title sizes and font sizes, and legend particulars

theme_map_dark <- function(palate_color = "Greys") { 

  palate <- brewer.pal(palate_color, n=12)
  color.background = "black"
  color.grid.minor = "black"
  color.grid.major = "black"
  color.axis.text = palate[1]
  color.axis.title = palate[1]
  color.title = palate[1]

  font.title <- "Source Sans Pro"
  font.axis <- "Open Sans Condensed Bold"

  theme_bw(base_size=5) +
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    theme(panel.grid.major=element_blank()) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    theme(plot.title=element_text(colour=color.title,family=font.title, size=10, face = 'bold')) +
    theme(axis.text.x=element_blank()) +
    theme(axis.text.y=element_blank()) +
    theme(axis.title.y=element_blank()) +
    theme(axis.title.x=element_blank()) +
    theme(plot.margin = unit(c(0.0, 0.5, 1, 0.75), "mm")) +
    theme(strip.background = element_rect(fill=color.background, color=color.background),strip.text=element_text(size=7,colour=color.axis.title,family=font.title))

}
Data Analysis and Plot

We analyze the data on the basis of pickup, drop-offs and whether it is a weekday or a weekend and plot 4 graphs which identify places in Manhattan that are the busiest.

######WEEKDAY#######

dfweekday<- df %>% filter(df$day!="Saturday" & df$day!= "Sunday")

#Pickup

plot1 <- ggplot(data=dfweekday , aes(x=dfweekday$pickup_longitude, y=dfweekday$pickup_latitude, z=dfweekday$tpep_pickup_datetime)) +
  geom_point(size=0.06, color="#777777") +
  scale_x_continuous(limits=c(-74.0224, -73.8521)) +
  scale_y_continuous(limits=c(40.6959, 40.8348)) +
  theme_map_dark() +
  labs(title = "NYC Taxi: Pickup Location during weekdays in June 2016 between 8 AM to 9 AM") +
  coord_equal() 
print(plot1)
The plot above shows a high concentration of Taxi Pickups in Midtown East, Sutton Place, and areas between 6th and 8th Avenues and W42nd and W58th Streets.

#Drop-Off

plot2 <- ggplot(data=dfweekday , aes(x=dfweekday$dropoff_longitude, y=dfweekday$dropoff_latitude, z=dfweekday$tpep_dropoff_datetime )) +
  geom_point(size=0.06, color="#777777") +
  stat_summary_hex(fun = dfweekday$tpep_dropoff_datetime, bins= 100, alpha=0.5) +
  scale_x_continuous(limits=c(-74.0224, -73.8521)) +
  scale_y_continuous(limits=c(40.6959, 40.8348)) +
  theme_map_dark() +
  scale_fill_gradient(low="#CCCCCC", high="#8E44AD", trans="log", breaks=c("00:30")) +
  labs(title = "NYC Taxi: Drop-off Location during weekdays in June 2016 between 8 AM to 9 AM") +
  coord_equal()
print(plot2)
The plot above shows a high concentration of Taxi Drop-offs in Central and Eastern Midtown, some of which include the Garment District, Murray Hill, the Times Square Area, Columbus Circle, etc.

#####WEEKEND######

dfweekend<- df %>% filter(df$day=="Saturday" | df$day== "Sunday")

#Pickup

plot3 <- ggplot(data=dfweekend , aes(x=dfweekend$pickup_longitude, y=dfweekend$pickup_latitude, z=dfweekend$tpep_pickup_datetime)) +
  geom_point(size=0.06, color="#777777") +
  stat_summary_hex(fun = dfweekend$tpep_pickup_datetime, bins= 100, alpha=0.5) +
  scale_x_continuous(limits=c(-74.0224, -73.8521)) +
  scale_y_continuous(limits=c(40.6959, 40.8348)) +
  theme_map_dark() +
  scale_fill_gradient(low="#FFFFFF", high="#E74C3C", trans="log", breaks=c("00:30")) +
  labs(title = "NYC Taxi: Pickup Location during weekends in June 2016 between 8 AM to 9 AM") +
  coord_equal()
print(plot3)
The weekend pickups show a relatively sparse distribution. Some of the areas with comparatively high pickups include the Times Square Area (a lot of hotels) and Upper Midtown on 5th Avenue (shopping).

#Drop-Off

plot4 <- ggplot(data=dfweekend , aes(x=dfweekend$dropoff_longitude, y=dfweekend$dropoff_latitude, z=dfweekend$tpep_dropoff_datetime )) +
  geom_point(size=0.06, color="#777777") +
  stat_summary_hex(fun = dfweekend$tpep_dropoff_datetime, bins= 100, alpha=0.5) +
  scale_x_continuous(limits=c(-74.0224, -73.8521)) +
  scale_y_continuous(limits=c(40.6959, 40.8348)) +
  theme_map_dark() +
  scale_fill_gradient(low="#FFFFFF", high="#E74C3C", trans="log", breaks=c("00:30")) +
  labs(title = "NYC Taxi: Drop-ff Location during weekends in June 2016 between 8 AM to 9 AM") +
  coord_equal()
print(plot4)

Drop-offs during the weekends are concentrated mostly around the tourist attractions, some of which include Columbus Circle, Times Square and the Rockerfeller Center. People also travel to the St. Patricks Cathedral area on 50th street and Madison Avenue.

Calculation of time in spent in traffic

We consider 3 neighborhoods for this analysis - people who travel from Midtown East to the Garment District and from Midtown East to the Times Square area on weekdays and weekends respectively.

We define these areas according to the following coordinates:

#### Midtown East = Min Long: -73.9808, Max Long: -73.9591 & Min Lat: 40.7480, Max Lat: 40.7643 #### Garment District = Min Long: -73.9963, Max Long: -73.9841 & Min Lat: 40.7478, Max Lat: 40.7583 #### Times Square = Min Long: -73.9916, Max Long:-73.9808 & Min Lat: 40.7542, Max Lat: 40.7613

We calculate the average time differences between these coordinates on weekdays and weekends. Assuming that there is little traffic during the weekends, the average difference would give us the extra time a person spends sitting in traffic during the weekdays.

##Creating a new column for pickup and dropp-off times during weekdays and weekends

Hours1 <- format(as.POSIXct(strptime(dfweekday$tpep_pickup_datetime,"%Y-%m-%d  %H:%M:%S",tz="")) ,format = "%H:%M")
head(Hours1)
dfweekday$pickuptime<-Hours1

Hours2 <- format(as.POSIXct(strptime(dfweekday$tpep_dropoff_datetime,"%Y-%m-%d  %H:%M:%S",tz="")) ,format = "%H:%M")
head(Hours2)
dfweekday$dropofftime<-Hours2

Hours3 <- format(as.POSIXct(strptime(dfweekend$tpep_pickup_datetime,"%Y-%m-%d  %H:%M:%S",tz="")) ,format = "%H:%M")
head(Hours3)
dfweekend$pickuptime<-Hours3

Hours4 <- format(as.POSIXct(strptime(dfweekend$tpep_dropoff_datetime,"%Y-%m-%d  %H:%M:%S",tz="")) ,format = "%H:%M")
head(Hours4)
dfweekend$dropofftime<-Hours4


## Converting time to Hour and Minute format to be read by R and creating a column for time differences

###Weekdays

pickupWeekday<-as.POSIXct(dfweekday$pickuptime,format="%H:%M")
dropoffWeekday<-as.POSIXct(dfweekday$dropofftime,format="%H:%M")

dfweekday$diffinmin <- difftime(dropoffWeekday,pickupWeekday,tz,units = "mins")

###Weekends

pickupWeekend<-as.POSIXct(dfweekend$pickuptime,format="%H:%M")
dropoffWeekend<-as.POSIXct(dfweekend$dropofftime,format="%H:%M")

dfweekend$diffinmin <- difftime(dropoffWeekend,pickupWeekend,tz,units = "mins")
Midtown East to Times Square

#For Weekdays (Monday to Friday)

M_T_Weekday<- subset(dfweekday,subset= dfweekday$pickup_longitude >=-73.9808 & dfweekday$pickup_longitude <=-73.9591 
             & dfweekday$pickup_latitude >= 40.7480 & dfweekday$pickup_latitude <= 40.7643
             & dfweekday$dropoff_longitude >= -73.9916  & dfweekday$dropoff_longitude <= -73.9808 
             & dfweekday$dropoff_latitude >= 40.7542 & dfweekday$dropoff_latitude <= 40.7613
)

head(M_T_Weekday)
mean(M_T_Weekday$trip_distance)

#For Weekends (Saturday and Sunday)

M_T_Weekend<- subset(dfweekend,subset= dfweekend$pickup_longitude >=-73.9808 & dfweekend$pickup_longitude <=-73.9591 
             & dfweekend$pickup_latitude >=40.7480 & dfweekend$pickup_latitude <=40.7643
             & dfweekend$dropoff_longitude >=-73.9916  & dfweekend$dropoff_longitude <=-73.9808 
             & dfweekend$dropoff_latitude >=40.7542 & dfweekend$dropoff_latitude <=40.7613
)
head(M_T_Weekend)
mean(M_T_Weekend$trip_distance)


##Calculating extra time taken during weekdays

mean(M_T_Weekend$diffinmin)
mean(M_T_Weekday$diffinmin)
Time_Difference1 <- mean(M_T_Weekday$diffinmin) - mean(M_T_Weekend$diffinmin)
Time_Difference1
It takes an average of 3.22 mins more to get to Times Square from Midtown East on a weekday between 8AM and 9AM.

Midtown to Garmet District

#For Weekdays (Monday to Friday)

M_G_Weekday<- subset(dfweekday,subset= dfweekday$pickup_longitude >=-73.9808 & dfweekday$pickup_longitude <=-73.9591 
             & dfweekday$pickup_latitude >=40.7480 & dfweekday$pickup_latitude <=40.7643
             & dfweekday$dropoff_longitude >=-73.9963  & dfweekday$dropoff_longitude <=-73.9841 
             & dfweekday$dropoff_latitude >=40.7478 & dfweekday$dropoff_latitude <=40.7583
)
head(M_G_Weekday)
mean(M_G_Weekday$trip_distance)


#For Weekends (Saturday and Sunday)

M_G_Weekend <- subset(dfweekend,subset= dfweekend$pickup_longitude >= -73.9808 & dfweekend$pickup_longitude <= -73.9591 
             & dfweekend$pickup_latitude >= 40.7480 & dfweekend$pickup_latitude <= 40.7643
             & dfweekend$dropoff_longitude >= -73.9963  & dfweekend$dropoff_longitude <= -73.9841 
             & dfweekend$dropoff_latitude >= 40.7478 & dfweekend$dropoff_latitude <= 40.7583
)

head(M_G_Weekend)
View(M_G_Weekend)
mean(M_G_Weekend$trip_distance)


mean(M_G_Weekend$diffinmin)
mean(M_G_Weekday$diffinmin)
Time_Difference2 <- mean(M_G_Weekday$diffinmin)-mean(M_G_Weekend$diffinmin)
Time_Difference2
It takes an average of 3.82 mins longer on a weekday between 8AM and 9AM, to reach the Garment District from Midtown East.
