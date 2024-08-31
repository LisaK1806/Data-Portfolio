install.packages(c("readr", "dplyr", "lubridate", "class", "ggplot2", "viridis", "leaflet", "sf", "htmlwidgets", "webshot", "patchwork", "FNN", "scales"))
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(FNN)
library(class)
library(viridis)
library(leaflet)
library(sf)
library(htmlwidgets)
library(webshot)
library(patchwork)
library(scales)
library(tidyr)

install.packages("tinytex")
tinytex::install_tinytex()
install.packages(c("knitr", "rmarkdown"))
library(knitr)
library(rmarkdown)

jan_23 <- read.csv("202301-divvy-tripdata.csv", sep=",", header=TRUE)
feb_23 <- read.csv("202302-divvy-tripdata.csv", sep=",", header=TRUE)

mar_23 <- read.csv("202303-divvy-tripdata.csv", sep=",", header=TRUE)
abr_23 <- read.csv("202304-divvy-tripdata.csv", sep=",", header=TRUE)
may_23 <- read.csv("202305-divvy-tripdata.csv", sep=",", header=TRUE)

jul_23 <- read.csv("202307-divvy-tripdata.csv", sep = ",", header = TRUE)
jun_23 <- read.csv("202306-divvy-tripdata.csv", sep=",", header = TRUE)
aug_23 <- read.csv("202308-divvy-tripdata.csv", sep=",", header=TRUE)

sep_23 <- read.csv("202309-divvy-tripdata.csv", sep=",", header=TRUE)
oct_23 <- read.csv("202310-divvy-tripdata.csv", sep=",", header=TRUE)
nov_23 <- read.csv("202311-divvy-tripdata.csv", sep=",", header=TRUE)
dec_23 <- read.csv("202312-divvy-tripdata.csv", sep=",", header=TRUE)

trips <- bind_rows(jan_23, feb_23, mar_23, abr_23, may_23, jun_23, jul_23, aug_23, sep_23, oct_23, nov_23, dec_23)
write.csv(trips, file = "trips.csv", row.names = FALSE)
trips <- read.csv("trips.csv", sep=",", header=TRUE)

stations <- trips %>%
  select(ride_id, start_station_name, start_station_id, end_station_name, end_station_id, start_lat, start_lng, end_lat, end_lng)

stations <- bind_rows(
  stations %>%
    select(ride_id, start_station_name, start_station_id, start_lat, start_lng) %>%
    rename(
      station = start_station_name,
      id_station = start_station_id,
      latitude = start_lat,
      longitude = start_lng),
 stations %>%
  select(ride_id, end_station_name, end_station_id, end_lat, end_lng) %>%
    rename(
      station = end_station_name,
      id_station = end_station_id,
      latitude = end_lat,
      longitude = end_lng))

stations$coordinate <- paste(stations$latitude, stations$longitude, sep = " ")


stations_all <- stations %>%
  select(ride_id, station, latitude, longitude)
write.csv(stations_all, "stations_all.csv", row.names = FALSE)

unique_stations <- stations %>%
  distinct(station, .keep_all = TRUE)

stations_top <- stations %>%
  group_by(coordinate) %>%
  summarise(count = n(),
            station = first(station)) %>%
  arrange(desc(count)) %>%
  slice_head(n = 21)

top <- stations_top %>%
  separate(coordinate, into = c("latitude", "longitude"), sep = " ")

#unknown station
stations_with_missing <- data.frame(
  lat = c(41.89, 41.91),
  lng = c(-87.63, -87.63),
  station = c(NA)
)
#knn function
fill_station_name_knn <- function(unknown_coords, unique_stations, k = 3) {
  knn_result <- knn(
    train = unique_stations[, c("latitude", "longitude")],
    test = unknown_coords,
    cl = unique_stations$station,
    k = k
  )
  return(knn_result)
}

unknown_coords <- stations_with_missing %>%
  filter(is.na(station)) %>%
  select(lat, lng)

filled_station_name <- fill_station_name_knn(unknown_coords, unique_stations)

filled_station <- data.frame(
  lat = unknown_coords$lat,
  lng = unknown_coords$lng,
  filled_station_name = filled_station_name
)

stations_top <- top %>%
  mutate(station = ifelse(latitude == 41.89 & longitude == -87.63, "Wabash Ave & Grand Ave", station)) %>%
  mutate(station = ifelse(latitude == 41.91 & longitude == -87.63, "DuSable Lake Shore Dr & North Blvd", station))

stations_top <- stations_top %>%
  group_by(station) %>%
  summarise(count = sum(count),
            latitude = first(latitude),
            longitude = first(longitude)) %>%
  arrange(desc(count)) 

dates_trips <- trips %>%
  select(ride_id, rideable_type, started_at, ended_at, member_casual)

dates_trips$started_at <- ymd_hms(dates_trips$started_at)
dates_trips$ended_at <- ymd_hms(dates_trips$ended_at)
dates_trips$ride_length <- dates_trips$ended_at - dates_trips$started_at
dates_trips <- dates_trips %>% filter(ride_length > 0)
dates_trips$day_of_week <- wday(dates_trips$started_at, label = TRUE, week_start = 7)

dates_trips <- dates_trips %>%
  mutate(month = month(started_at, label = TRUE),
         hour = factor(hour(started_at), 
                             levels = 0:23, 
                             labels = c("12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM", 
                                        "12 PM", "1 PM", "2 PM", "3 PM", "4 PM", "5 PM", "6 PM", "7 PM", "8 PM", "9 PM", "10 PM", "11 PM")))


analisis <- dates_trips %>%
  select(ride_id, rideable_type, member_casual, ride_length, day_of_week, month, hour)
write.csv(analisis, "analisis.csv", row.names = FALSE)
write.csv(stations_top, "stations_top.csv", row.names = FALSE)
analisis <- read.csv("analisis.csv", sep=",", header=TRUE)
####-------------------------------------------------GRAPHS-----------------------
colors <- c("classic_bike" = "cyan3", "docked_bike" = "darkslategray1", "electric_bike"="deepskyblue2", "member" = "darkblue", "casual"="deepskyblue")
labels_biketype <- c("classic_bike" = "classic bike", "docked_bike" = "docked bike", "electric_bike"="electric bike", "1" = "casual", "2" = "member")
ggplot(analisis, aes(x = "", fill = rideable_type)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(values = colors, labels = labels_biketype) +
  labs(fill = "Bike Types") +
  labs(title = "Frequency of rides by bike type")


analisis$ride_length_min <- analisis$ride_length/60  

type_total_length <- analisis%>%
  group_by(rideable_type) %>%
  summarise(length = sum(ride_length_min))

type_avg_length <- analisis %>%
  group_by(rideable_type) %>%
  summarise(mean_length = mean(ride_length_min))

graph1 <- ggplot(type_total_length, aes(x = "", y = length, fill = rideable_type)) +
  geom_bar(width = 1, stat = "identity", color = "transparent") +
  coord_polar("y", start = 0) +
  labs(subtitle = "Total Length",
       x = NULL,
       y = NULL,
       fill = "Type") +
  scale_fill_manual(values = colors, labels = labels_biketype) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), panel.border = element_blank(),
        panel.grid = element_blank())

graph2 <- ggplot(type_avg_length, aes(x = "", y = mean_length, fill = rideable_type)) +
  geom_bar(width = 1, stat = "identity", color = "transparent") +
  coord_polar("y", start = 0) +
  labs(subtitle = "Average Length",
       x = NULL,
       y = NULL,
       fill = "Type") +
  scale_fill_manual(values = colors, labels = labels_biketype) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), panel.border = element_blank(),
        panel.grid = element_blank())

grpah_ride_length <- graph1 + graph2 + labs(title = "Ride length by bike type") + plot_layout(guides = 'collect') & theme(legend.position = "bottom")
print(grpah_ride_length)

#--------------------------------Membership general------------------------------
ggplot(analisis, aes(x = "", fill = member_casual)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(values = colors, labels = labels_biketype) +
  labs(fill = "Membership") +
  labs(title = "Distribution of Casual Users and Annual Members")

members <- analisis %>%
  filter(member_casual == "member")

casual <- analisis %>%
  filter(member_casual == "casual")

graph3 <- ggplot(members, aes(x = "", fill = rideable_type)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(values = colors) +
  labs(subtitle = "Annual Members") +
  theme(legend.position = "none")
print(graph3)

graph4 <- ggplot(casual, aes(x = "", fill = rideable_type)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(values = colors, labels = labels_biketype) +
  labs(fill = "Bike Types") + 
  labs(subtitle = "Casual Users") 
print(graph4)

grpah_type_membership <- (graph3 + graph4) + (plot_layout(guides = 'collect') & theme(legend.position = "bottom"))
print(grpah_type_membership)

mean_length <- analisis %>%
  group_by(member_casual) %>%
  summarise(mean = mean(ride_length_min))

mean_length$length_min <- paste0(round(mean_length$mean), " min")

ggplot(mean_length, aes(x = "", y = mean, fill = member_casual)) +
  geom_bar(width = 1, stat = "identity", color = "transparent") +
  coord_polar("y", start = 0) +
  labs(subtitle = "Average Ride Length: Casual vs. Annual Members",
       x = NULL,
       y = NULL,
       fill = "Type") +
  scale_fill_manual(values = colors, labels = labels_biketype) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), panel.border = element_blank(),
        panel.grid = element_blank())


ggplot(mean_length, aes(x = member_casual, y = mean, fill = member_casual)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = colors) +
  labs(title = "Average Ride Length: Casual vs. Annual Members",
       x = "Membership",
       y = "Length") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(angle = 90, hjust = 1))


colors <- c("classic_bike" = "cyan3", "docked_bike" = "darkslategray1", "electric_bike"="deepskyblue2", "member" = "darkblue", "casual"="deepskyblue")
labels_biketype <- c("classic_bike" = "classic bike", "docked_bike" = "docked bike", "electric_bike"="electric bike", "1" = "casual", "2" = "member")
custom_order <- c("Sun", "Sat", "Fri", "Thu", "Wed",  "Tue", "Mon")
count_data <- analisis %>%
  group_by(member_casual, day_of_week) %>%
  summarize(count = n(), .groups = 'drop') 

count_data$day_of_week <- factor(count_data$day_of_week, levels = custom_order)

ggplot(count_data, aes(fill=member_casual, y=count , x=day_of_week )) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = colors) +
  labs(y = "Day of Week", x = "Number of rides", title = "Total rides per days by Membership Type: Casual vs. Annual Members", fill = "Membership") +
  theme_minimal() +
  coord_flip() +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())

custom_hour <- c("12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM", "12 PM", "1 PM", "2 PM", "3 PM", "4 PM", "5 PM", "6 PM", "7 PM", "8 PM", "9 PM", "10 PM", "11 PM")
hour_data <- analisis %>%
  group_by(member_casual, hour) %>%
  summarize(count = n(), .groups = 'drop') 
hour_data$hour <- factor(hour_data$hour, levels = custom_hour)

hour_general <- analisis %>%
  group_by(hour) %>%
  summarise(count=n())
hour_general$hour <- factor(hour_general$hour, levels = custom_hour)

#----------------------------------------------
analisis$hour <- factor(analisis$hour, levels = c("11 PM", "10 PM", "9 PM", "8 PM", "7 PM", "6 PM", "5 PM", "4 PM", "3 PM", "2 PM", "1 PM", "12 PM", "11 AM", "10 AM", "9 AM", "8 AM", "7 AM", "6 AM", "5 AM", "4 AM", "3 AM", "2 AM", "1 AM", "12 AM"))
analisis$day_of_week <- factor(analisis$day_of_week, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

analisis_freq <- analisis %>%
  count(day_of_week, hour, name = "Freq")

ggplot(analisis_freq, aes(day_of_week, hour, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "aliceblue", high = "darkblue") +
  theme_minimal()
#---------------------------------------------
ggplot() +
  geom_line(data = hour_general, aes(x = hour, y = count, group = 1), color = "cornflowerblue", size = 1) + 
  labs(y = "Total Number of Trips", x = "Hour", title = "Total Number of Trips Throughout the Day") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 90))


ggplot(hour_data, aes(x=hour, y=count, color=member_casual, group = member_casual )) +
  geom_line(size=1) +
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = comma) +
  labs(color = "Membership", title="Number of rides by hour", x="Number of rides", y="Hour") +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 90))


monthly <- analisis %>%
  group_by(member_casual, month) %>%
  summarise(count = n(), mean_ride_length = mean(ride_length_min, na.rm = TRUE))

month <- analisis %>%
  group_by(month) %>%
  summarise(count=n(), mean_ride_length = mean(ride_length_min, na.rm = TRUE))

custom_month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
month$month <- factor(month$month, levels = custom_month)
month$mean_ride_length <- round(month$mean_ride_length)

monthly$month <- factor(monthly$month, levels = custom_month)
monthly$mean_ride_length <- round(monthly$mean_ride_length)

ggplot(monthly, aes(fill=member_casual, y=count , x=month )) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = colors) +
  scale_y_continuous(labels = comma) +
  labs(y = "Number of rides", x = "Month", title = "Monthly Distribution of Bike Rides", fill = "Membership") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())

ggplot(monthly, aes(fill=member_casual, y=count, x=month)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = colors) +
  scale_y_continuous(labels = comma) +
  labs(y = "Number of rides", x = "Month", title = "Monthly Distribution of Bike Rides", fill = "Membership") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())

ggplot(month, aes(x = month, y = count)) + 
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  scale_y_continuous(labels = comma) +
  labs(y = "Total number of trips", x = "Month", title = "Average Time of Trip") +
  theme_minimal()

member_average <- members %>%
  select(ride_length_min, month) %>%
  group_by(month) %>%
  summarise(count=n(), mean_ride_length = mean(ride_length_min))
member_average$month <- factor(member_average$month, levels = custom_month)
member_average$mean_ride_length <- round(member_average$mean_ride_length)

casual_average <- casual %>%
  select(ride_length_min, month) %>%
  group_by(month) %>%
  summarise(count=n(), mean_ride_length = mean(ride_length_min))
casual_average$month <- factor(casual_average$month, levels = custom_month)
casual_average$mean_ride_length <- round(casual_average$mean_ride_length)

ggplot() +
  geom_line(data = member_average, aes(x = month, y = mean_ride_length, group=2), color="darkblue", size=0.5) +
  geom_line(data = casual_average, aes(x = month, y = mean_ride_length, group = 3), color="deepskyblue", size = 0.5) +
  labs(y = "Average length of trip", x = "Month", title = "Average Trip Length by Membership Type: Casual, Annual, and Combined", fill = "Membership")

###-----------------------------------------------------------------------------

stations <- read.csv("stations_top.csv", sep=",", header=TRUE)

stations_sf <- st_as_sf(stations_top, coords = c("longitude", "latitude"), crs = 4326)

library(leaflet)
pal_cont <- colorFactor("viridis", levels = stations_sf$count)

pal <- colorNumeric(palette = viridis(20), domain = stations_top$count)

legend_entries <- paste0(
  '<div style="background-color: ', pal_cont(stations_top$count), '; height: 20px; width: 20px; display: inline-block;"></div> ',
  stations_top$station, '<br>'
)
legend_html <- paste0(
  '<div style="background-color: white; padding: 10px; border: 1px solid #ccc; width: 300px;">',
  '<b>Stations Legend</b><br>',
  paste(legend_entries, collapse = ''),
  '</div>'
)

stations_top$latitude <- as.numeric(stations_top$latitude)
stations_top$longitude <- as.numeric(stations_top$longitude)

leaflet_map <- leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -87.63, lat = 41.87, zoom = 12) %>%
  addCircleMarkers(data = stations_top, color = ~pal_cont(count), radius = ~sqrt(count) * 0.01, fillOpacity = 0.7, popup=~paste(station,"<br>", count)) %>%
  addControl(
    html = legend_html,
    position = "topright"
  )
print(leaflet_map)
simple_map <- leaflet(data = stations_sf) %>%
  addTiles() %>%
  addCircleMarkers(~longitude, ~latitude, popup = ~station)


install.packages("webshot")
webshot::install_phantomjs()
saveWidget(leaflet_map, "map.html", selfcontained = TRUE)
