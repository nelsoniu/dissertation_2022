# Data cleaning for real time travel distance computation. 
# In this R file, I perform data cleaning to transform my point data to the format required by Rapid Realistic Routing


# filter unique HDB resale flat locations 
house <- read.csv("house_complete_v4.csv")
house <- house %>% distinct(house$add,.keep_all = T)
house <- house %>% dplyr::select("add", "lon", "lat")
names(house)[names(house) == "add"] <- "id"
write.csv(house, file = "house_coordinates.csv")


# train data preparation
train <- read_sf("lta-mrt-station-exit-kml.kml")
train_pp <- train[,c("geometry")]

train <- train_pp %>% mutate(lat = unlist(map(train_pp$geometry,1)),
                              long = unlist(map(train_pp$geometry, 2)))
train <- as.data.frame(train)
train <- train %>% dplyr::select("long", "lat")
names(train)[names(train) == "lat"] <- "lon"
names(train)[names(train) == "long"] <- "lat"
train$id <- seq.int(nrow(train))
write.csv(train, file = "train.csv")

# park
parks <- read_sf("RelaxSG.kml")
parks_pp <- parks[,c("geometry")]
parks <- parks %>% mutate(lat = unlist(map(parks_pp$geometry,1)),
                             long = unlist(map(parks_pp$geometry, 2)))
parks <- as.data.frame(parks)
parks <- parks %>% dplyr::select("long", "lat")
names(parks)[names(parks) == "lat"] <- "lon"
names(parks)[names(parks) == "long"] <- "lat"
parks$id <- seq.int(nrow(parks))
write.csv(parks, file = "parks.csv")


# shopping malls
mall <- read.csv("sg_malls.csv")
names(mall)[names(mall) == "add"] <- "id"
mall <- mall %>% dplyr::select(-"X")
write.csv(mall, file = "malls.csv")

# hospitals
hospital <- read.csv("hospital_final.csv")
hospital <- hospital %>% dplyr::select("name", "lon", "lat")
names(hospital)[names(hospital) == "name"] <- "id"
write.csv(hospital, file = "hospitals.csv")

# CBD 
id <- "cdb"
lon <- "103.85255323664575"
lat <- "1.2786774240431218" 
CBD <- data.frame(id, lat, lon)
write.csv(CBD, file = "cbd.csv")

# Bus 
bus_pp <- read_sf("BusStopLocation_Jan2022")
bus_pp <- bus_pp[,c("geometry", "LOC_DESC")]
bus_pp = st_transform(bus_pp, "EPSG:4326")
bus_pp <- bus_pp %>% mutate(lat = unlist(map(bus_pp$geometry,1)),
                          long = unlist(map(bus_pp$geometry, 2)))
bus_pp <- as.data.frame(bus_pp)
bus <- bus_pp[,c("LOC_DESC","long", "lat")]
names(bus)[names(bus) == "LOC_DESC"] <- "id"
names(bus)[names(bus) == "lat"] <- "lon"
names(bus)[names(bus) == "long"] <- "lat"
write.csv(bus, file = "bus.csv")


# University 
uni <- as.data.frame(uni_pp)
uni <- uni[,c("long", "lat")]
uni$id <- seq.int(nrow(uni))
write.csv(uni, file = "university.csv")



#### Save data #######
save.image(file = "amenity.pp.RData")




