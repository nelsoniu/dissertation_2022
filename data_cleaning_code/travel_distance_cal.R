# In this r file, I perform real time travel distance matrix using Rapid Realistic Routing
library(r5r)
library(sf)
library(data.table)
library(ggplot2)
library(mapview)
mapviewOptions(platform = 'leafgl')

options(java.parameters = "-Xmx32G")
path <- ("data.acc") #here is the GTFS and pbf data
list.files(path)
r5r_core <- setup_r5(data_path = path, verbose = FALSE)


# transportation mode
mode<-c("WALK", "TRANSIT")

# walk distance
max_walk_dist <- 1000

# trip duration
max_trip_duration<- 300

# travel date
departure_datetime<-as.POSIXct("27-03-2018 11:00:00", format="%d-%m-%Y %H:%M:%S")


############################
house_final <- read.csv("house_complete_v4.csv")
bus <- fread("bus.csv")
cbd <- fread("cbd.csv")
hospital <- fread("hospitals.csv")
house <- fread("house_coordinates.csv")
malls <- fread("malls.csv")
parks <- fread("parks.csv")
train <- fread("train.csv")
university <- fread("university.csv")
house_small <- fread("house_small.csv")



############################
dis_train <- travel_time_matrix(r5r_core = r5r_core,
                          origins = house,
                          destinations = train,
                          mode = mode,
                          departure_datetime = departure_datetime,
                          max_walk_dist = 8000,
                          max_trip_duration = max_trip_duration,
                          verbose = FALSE)

dis_cbd <- travel_time_matrix(r5r_core = r5r_core,
                                origins = house,
                                destinations = cbd,
                                mode = c("CAR", "TRANSIT"),
                                departure_datetime = departure_datetime,
                                max_trip_duration = max_trip_duration,
                                verbose = FALSE)

dis_bus <- travel_time_matrix(r5r_core = r5r_core,
                              origins = bus,
                              destinations = house,
                              mode = mode,
                              departure_datetime = departure_datetime,
                              max_walk_dist = 1000,
                              max_trip_duration = max_trip_duration,
                              verbose = FALSE)

dis_park <- travel_time_matrix(r5r_core = r5r_core,
                              origins = house,
                              destinations = parks,
                              mode = mode,
                              departure_datetime = departure_datetime,
                              max_walk_dist = 8000,
                              max_trip_duration = max_trip_duration,
                              verbose = FALSE)

dis_hospital <- travel_time_matrix(r5r_core = r5r_core,
                               origins = house,
                               destinations = hospital,
                               mode = c("CAR", "TRANSIT"),
                               departure_datetime = departure_datetime,
                               max_trip_duration = max_trip_duration,
                               verbose = FALSE)

dis_mall <- travel_time_matrix(r5r_core = r5r_core,
                                   origins = house,
                                   destinations = malls,
                                   mode = c("CAR", "TRANSIT"),
                                   departure_datetime = departure_datetime,
                                   max_trip_duration = max_trip_duration,
                                   verbose = FALSE)

dis_uni <- travel_time_matrix(r5r_core = r5r_core,
                               origins = house,
                               destinations = university,
                               mode = c("CAR", "TRANSIT"),
                               departure_datetime = departure_datetime,
                               max_trip_duration = max_trip_duration,
                               verbose = FALSE)


dis_mall <- travel_time_matrix(r5r_core = r5r_core,
                               origins = house,
                               destinations = malls,
                               mode = mode,
                               departure_datetime = departure_datetime,
                               max_walk_dist = max_walk_dist,
                               max_trip_duration = max_trip_duration,
                               verbose = FALSE)


test <- travel_time_matrix(r5r_core = r5r_core,
                               origins = house_small,
                               destinations = cbd,
                               mode = mode,
                               departure_datetime = departure_datetime,
                               max_trip_duration = max_trip_duration,
                               verbose = FALSE)

test2 <- travel_time_matrix(r5r_core = r5r_core,
                           origins = house_small,
                           destinations = cbd,
                           mode = "BUS",
                           departure_datetime = departure_datetime,
                           max_trip_duration = max_trip_duration,
                           verbose = FALSE)






length(unique(dis_mall$fromId))

#####################################################################################

# Train merge
min_train <- aggregate(x = dis_train$travel_time,
                        by = list(dis_train$fromId),
                        FUN = min)
names(min_train)[names(min_train) == "x"] <- "rrr_train"
names(min_train)[names(min_train) == "Group.1"] <- "add"
house_final <- merge(house_final, min_train, by.x = "add", by.y = "add")

# mall merge
min_mall <- aggregate(x = dis_mall$travel_time,
                       by = list(dis_mall$fromId),
                       FUN = min)
names(min_mall)[names(min_mall) == "x"] <- "rrr_mall"
names(min_mall)[names(min_mall) == "Group.1"] <- "add"
house_final <- merge(house_final, min_mall, by.x = "add", by.y = "add")



# park merge
min_park <- aggregate(x = dis_park$travel_time,
                      by = list(dis_park$fromId),
                      FUN = min)
names(min_park)[names(min_park) == "x"] <- "rrr_park"
names(min_park)[names(min_park) == "Group.1"] <- "add"
house_final <- merge(house_final, min_park, by.x = "add", by.y = "add")

# bus merge
min_bus <- aggregate(x = dis_bus$travel_time,
                      by = list(dis_bus$toId),
                      FUN = min)
names(min_bus)[names(min_bus) == "x"] <- "rrr_bus"
names(min_bus)[names(min_bus) == "Group.1"] <- "add"
house_final <- merge(house_final, min_bus, by.x = "add", by.y = "add")

# cbd merge
min_cbd <- aggregate(x = dis_cbd$travel_time,
                     by = list(dis_cbd$fromId),
                     FUN = min)
names(min_cbd)[names(min_cbd) == "x"] <- "rrr_cbd"
names(min_cbd)[names(min_cbd) == "Group.1"] <- "add"
house_final <- merge(house_final, min_cbd, by.x = "add", by.y = "add")

# hospital merge
min_hospital <- aggregate(x = dis_hospital$travel_time,
                     by = list(dis_hospital$fromId),
                     FUN = min)
names(min_hospital)[names(min_hospital) == "x"] <- "rrr_hospital"
names(min_hospital)[names(min_hospital) == "Group.1"] <- "add"
house_final <- merge(house_final, min_hospital, by.x = "add", by.y = "add")

# university merge
min_uni <- aggregate(x = dis_uni$travel_time,
                          by = list(dis_uni$fromId),
                          FUN = min)
names(min_uni)[names(min_uni) == "x"] <- "rrr_university"
names(min_uni)[names(min_uni) == "Group.1"] <- "add"
house_final <- merge(house_final, min_uni, by.x = "add", by.y = "add")

# export 
write.csv(house_final, file = "house_complete_v5.csv")


# Save data
save.image(file = "realtime.RData")



