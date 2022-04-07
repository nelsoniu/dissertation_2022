library(dplyr)
library(tidyverse)
library(here)
library(magrittr)
library(sf)
library(tmap)
library(janitor)
library(spdep)
library(RColorBrewer)
library(tmap)
library(spgwr)
library(sp)
library(rgdal)
library(rgeos)
library(tmap)
library(osmdata)
library(raster)
library(rosm)
library(dbscan)
library(leaflet)
library(maptools)
library(sp)
library(adehabitatHR)
library(tmaptools)
library(shiny)
library(spatstat)
library(grid)
library(gridExtra)
library(GGally)

# Read in Sinapore digital boundary files
sg_pa <- read_sf("PA") #ward
subzone <- read_sf("SUBZONE")


####
house <- read.csv("house_complete_v5.csv")
pp <- house %>% dplyr::select(lon,lat) %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326)


# identify CBD area
cbd <- c(32)
cbd <- sg_pa[cbd,]


####

# Map 1: Introductory map
tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 0.1) +
  tm_shape(west_sp) + tm_polygons(col = "#FCF3CF", alpha = 1) +
  tm_shape(north_sp) + tm_polygons(col = "#E8DAEF", alpha = 1) +
  tm_shape(north_east_sp) + tm_polygons(col = "#D4EFDF", alpha = 1) +
  tm_shape(east_sp) + tm_polygons(col = "#FDEDEC", alpha = 1) +
  tm_shape(sg_pa) + tm_polygons(col = "white", alpha = 0, border.alpha = 0) +
  #tm_text("PLN_AREA_N", col = "black", size = 0.4, alpha = 1, fontface = "bold") +
  tm_shape(central_sp) + tm_polygons(col = "#F0B27A", alpha = 1) +
  tm_compass(type="arrow", position = c("left", "bottom")) +
  tm_scale_bar(breaks = c(0, 5, 10)) +
  tm_add_legend(type = "fill", lwd = 2, col = c("#FCF3CF", "#E8DAEF","#D4EFDF", "#FDEDEC", "#F0B27A"), labels = c("West", "North", "North East","East","Central"), title = "Regions")+
  tm_shape(subzone) + tm_polygons(col = "#F9E79F", alpha = 0.0, border.alpha = 0.3) +
  tm_add_legend(type = "line", lwd = c(1, 0.8), col = c("black", "#BDC3C7"), labels = c("Planning Zone", "Subzone"), title = "") +
  tm_shape(cbd) + tm_polygons(border.col = "red", col = "#F0B27A", alpha = 0) 


#world map
map <- read_sf("world-administrative-boundaries") #ward
which(map$name == "Singapore", arr.ind = T)
sg<- c(175)
sg <- map[sg,]

tm_shape(map) + tm_polygons(col = "blue", alpha = 0.3, border.alpha = 0.5) +
  tm_layout(frame = F) +
  tm_shape(sg) + tm_dots(col = "red", size = 5)

 
  ### Splitting regions #####
west  <- c(4, 18, 9, 11, 43, 49, 48, 3, 42, 8, 50, 34)
west_sp <- sg_pa[west,]
north <- c(37, 19, 30, 20, 25, 46, 28, 51)
north_sp <- sg_pa[north,]
north_east <- c(5, 27, 26, 22, 24, 17,2)
north_east_sp <- sg_pa[north_east,]
east <- c(6, 31, 21, 16, 29, 41)
east_sp <- sg_pa[east,]
central <- c(12, 23, 33, 15, 35, 53, 1, 13, 14, 7, 10, 32, 36, 38, 39, 40, 44, 45,
             47, 52, 54,55)
central_sp <- sg_pa[central,]
#########################################################################################

# Map 2: Unique HDB housing 
tm_shape(subzone) + tm_polygons(col = "white", border.alpha = 1) +
  tm_shape(west_sp) + tm_polygons(col = "yellow", alpha = 0.4) +
  tm_shape(north_sp) + tm_polygons(col = "blue", alpha = 0.1) +
  tm_shape(north_east_sp) + tm_polygons(col = "green", alpha = 0.1) +
  tm_shape(east_sp) + tm_polygons(col = "red", alpha = 0.1) +
  tm_shape(central_sp) + tm_polygons(col = "orange", alpha = 0.4) +
  tm_shape(pp) + tm_dots(col = "blue", size = 0.001, alpha = 0.2) +
  tm_compass(type="arrow", position = c("right", "bottom")) +
  tm_scale_bar(breaks = c(0, 5, 10))


tm_shape(sg_pa) + tm_polygons(col = "TOTAL", palette = "Greys", border.alpha = 1, title = "Population")+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp) + tm_dots(col = "blue", size = 0.0005, alpha = 0.2) +
  tm_compass(type="arrow", position = c("left", "bottom")) +
  tm_scale_bar(breaks = c(0, 5, 10)) +
  tm_add_legend(type = "line", lwd = c(1, 0.8), col = c("black", "#BDC3C7"), labels = c("Planning Area", "Subzone"), title = "")


  tm_shape(subzone) + tm_polygons(col = "TOTAL", palette = "Greys", style = "jenks", border.alpha = 0.1, title = "Population", title.size = 1, font.face = "1") +
    tm_shape(sg_pa) + tm_polygons(col = "white", alpha = 0, border.alpha = 0.4)+
  tm_shape(pp) + tm_dots(col = "#D2042D", style = "fisher", size = 0.0005, alpha = 0.5, title = "Average Price per Square Meter", title.size = 2) +
    tm_layout(legend.show = T, legend.outside = F, legend.title.size = 1) +
  tm_compass(type="arrow", position = c("left", "bottom")) +
  tm_scale_bar(breaks = c(0, 5, 10), position = c("right", "bottom")) +
  tm_add_legend(type = "line", lwd = c(1, 0.8), col = c("black", "#BDC3C7"), labels = c("Planning Area", "Subzone"), title = "") +
    tm_shape(cbd) + tm_polygons(col = "blue", alpha = 0.2, border.col = "grey") +
    tm_add_legend(type = "fill",  col = c("#E6E6FA"), labels = c("CBD"), title = "")

# all HDB mean price distribution
pp <- cbind(pp, house_complete_case$mean_price_per_m2)
tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.3) +
  tm_shape(pp) + tm_dots(col = "house_complete_case.mean_price_per_m2", style = "fisher", size = 0.005, alpha = 0.2, palette = "YlOrRd", title = "Price per Square Meter (SGD)") +
  tm_compass(type="arrow", position = c("left", "bottom")) +
  tm_scale_bar(breaks = c(0, 5, 10))

#2014 price
map1 <- tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.3) +
  tm_shape(pp2014) + tm_dots(col = "mean_area", style = "cont", breaks = c(2500,5000,7500,11000),size = 0.0001, alpha = 1, palette = "YlOrRd", title = "Price per Square Meter (SGD)") +
  tm_layout(legend.show = T)

#2015
map2 <- tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.3) +
  tm_shape(pp2015) + tm_dots(col = "house2015.mean_price_per_m2",style = "fixed", breaks = c(0,2500,5000,7500,11000), size = 0.0001, alpha = 1, palette = "YlOrRd", title = "Price per Square Meter (SGD)") +
  tm_layout(legend.show = F)

#2016
map3 <- tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.3) +
  tm_shape(pp2016) + tm_dots(col = "house2016.mean_price_per_m2", style = "fixed", breaks = c(0,2500,5000,7500,11000), size = 0.0001, alpha = 1, palette = "YlOrRd", title = "Price per Square Meter (SGD)") +
  tm_layout(legend.show = F)

#2017
map4 <- tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.3) +
  tm_shape(pp2017) + tm_dots(col = "house2017.mean_price_per_m2", style = "fixed", breaks = c(0,2500,5000,7500,11000), size = 0.0001, alpha = 1, palette = "YlOrRd", title = "Price per Square Meter (SGD)") +
  tm_layout(legend.show = F)

#2018
map5 <- tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.3) +
  tm_shape(pp2018) + tm_dots(col = "house2018.mean_price_per_m2", style = "fixed", breaks = c(0,2500,5000,7500,11000), size = 0.0001, alpha = 1, palette = "YlOrRd", title = "Price per Square Meter (SGD)") +
  tm_layout(legend.show = F)

#2019
map6 <- tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.3) +
  tm_shape(pp2019) + tm_dots(col = "house2019.mean_price_per_m2", style = "fixed", breaks = c(0,2500,5000,7500,11000), size = 0.0001, alpha = 1, palette = "YlOrRd", title = "Price per Square Meter (SGD)") +
  tm_layout(legend.show = F)

#2020
map7 <- tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.3) +
  tm_shape(pp2020) + tm_dots(col = "house2020.mean_price_per_m2", style = "fixed", breaks = c(0,2500,5000,7500,11000), size = 0.0001, alpha = 1, palette = "YlOrRd", title = "Price per Square Meter (SGD)") +
  tm_layout(legend.show = F)

#2021
map8 <- tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.3) +
  tm_shape(pp2021) + tm_dots(col = "house2021.mean_price_per_m2", style = "fixed", breaks = c(0,2500,5000,7500,11000), size = 0.0001, alpha = 1, palette = "YlOrRd", title = "Price per Square Meter (SGD)") +
  tm_layout(legend.show = F)

tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.3) +
  tm_shape(pp2021) + tm_dots(col = "house2021.mean_price_per_m2", style = "fixed", breaks = c(0,2500,5000,7500,11000), size = 0.0001, alpha = 1, palette = "YlOrRd", title = "Price per Square Meter (SGD)") +
  tm_layout(legend.show = F) 

grid.newpage()T
# assigns the cell size of the grid, in this case 2 by 2
pushViewport(viewport(layout=grid.layout(4,2)))
print(map1, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(map2, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(map3, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
print(map4, vp=viewport(layout.pos.col = 2, layout.pos.row =2))
print(map5, vp=viewport(layout.pos.col = 1, layout.pos.row =3))
print(map6, vp=viewport(layout.pos.col = 2, layout.pos.row =3))
print(map7, vp=viewport(layout.pos.col = 1, layout.pos.row =4))
print(map6, vp=viewport(layout.pos.col = 2, layout.pos.row =4))

hist(house2014$mean_price_per_m2)



####################################################

# Map 3: Number of house per polygon 
tm_shape(subzone) + tm_polygons(col = "p_polygon", style = "jenks", palette = "YlOrBr", border.alpha = 0.3, title = "Number of HDB Transactions") +
  tm_layout(legend.show = T,legend.outside = F, frame = T, legend.position = c("right", "bottom"), legend.text.size = 1)+
  tm_compass(type="arrow", position = c("left", "bottom")) +
  tm_scale_bar(breaks = c(0, 5, 10)) 


######################################################

# HOSPITAL
hosp <- read.csv("hospital.csv")
hosp_pp <- hosp %>% dplyr::select(lon,lat) %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326)


tm_shape(subzone) + tm_polygons(col = "TOTAL", alpha = 1, style = "jenks", palette = "Greys", border.alpha = 0.2, title = "Population") +
  tm_shape(sg_pa) + tm_polygons(col = "white", alpha = 0,1, border.alpha = 0.2)+
  tm_shape(hosp_pp) + tm_dots(col = "red", size = 0.1, alpha = 1, shape = 3) +
  tm_shape(range75) + tm_borders(alpha=.7, col = "#fb6a4a", lwd = 2) + tm_fill(alpha=.1, col = "#fb6a4a")+
  tm_shape(range50) + tm_borders(alpha=.7, col = "#de2d26", lwd = 2) + tm_fill(alpha=.1, col = "#de2d26")+
  tm_shape(range25) + tm_borders(alpha=.7, col = "#a50f15", lwd = 2) + tm_fill(alpha=.1, col = "#a50f15") +
  tm_compass(type="arrow", position = c("left", "bottom")) +
  tm_scale_bar(breaks = c(0, 5, 10)) +
  tm_add_legend(type = "line", lwd = 2, col = c("#fb6a4a", "#de2d26","#a50f15"), labels = c("75%", "50%", "25%"), title = "Public Hospitals KDE Range") +
  tm_shape(cbd) + tm_polygons(col = "blue", alpha = 0.2, border.col = "grey") +
  tm_add_legend(type = "fill",  col = c("#E6E6FA"), labels = c("CBD"), title = "")


# PARK
parks <- read_sf("RelaxSG.kml")
parks_pp <- parks[,c("geometry")]


  tm_shape(subzone) + tm_polygons(col = "TOTAL", palette = "Greys", style = "jenks", alpha = 1, border.alpha = 0.2, title = "Population") +
    tm_shape(sg_pa) + tm_polygons(col = "white", alpha = 0, border.alpha = 0.3)+
  tm_shape(parks_pp) + tm_dots(col = "#32CD32", size = 0.2, alpha = 1, shape = 15) +
  tm_compass(type="arrow", position = c("left", "bottom")) +
  tm_scale_bar(breaks = c(0, 5, 10), position = c("center", "bottom")) +
  tm_add_legend(type = "line", lwd = c(1, 0.8), col = c("black", "#BDC3C7"), labels = c("Planning Area", "Subzone"), title = "")


# TRANSPORTATION
train <- read_sf("lta-mrt-station-exit-kml.kml")
train_pp <- train[,c("geometry")]

tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.3) +
  tm_shape(train_pp) + tm_dots(col = "#000080", size = 0.1, alpha = 1, shape = 18) +
  tm_compass(type="arrow", position = c("left", "bottom")) +
  tm_scale_bar(breaks = c(0, 5, 10)) 
  


# University
uni_pp <- uni_coord %>% dplyr::select(long,lat) %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326)
tm_shape(subzone) + tm_polygons(col = "TOTAL", alpha = 1, style = "jenks", palette = "Greys", border.alpha = 0.2, title = "Population") +
  tm_shape(sg_pa) + tm_polygons(col = "white", alpha = 0,1, border.alpha = 0.2)+
  tm_shape(uni_pp) + tm_dots(col = "orange", size = 0.01, alpha = 1) +
  tm_shape(range75) + tm_borders(alpha=.7, col = "#fb6a4a", lwd = 2) + tm_fill(alpha=.1, col = "#fb6a4a")+
  tm_shape(range50) + tm_borders(alpha=.7, col = "#de2d26", lwd = 2) + tm_fill(alpha=.1, col = "#de2d26")+
  tm_shape(range25) + tm_borders(alpha=.7, col = "#a50f15", lwd = 2) + tm_fill(alpha=.1, col = "#a50f15") +
  tm_compass(type="arrow", position = c("left", "bottom")) +
  tm_scale_bar(breaks = c(0, 5, 10)) +
  tm_add_legend(type = "line", lwd = 2, col = c("#fb6a4a", "#de2d26","#a50f15"), labels = c("75%", "50%", "25%"), title = "University KDE Range")


# BUS and MRT
bus_pp <- bus[,c("geometry")]
tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.3) +
  tm_shape(bus_pp) + tm_dots(col = "#000080", size = 0.03, alpha = 1, shape = 18) +
  tm_shape(train_pp) + tm_dots(col = "#8B0000", size = 0.1, alpha = 1, shape = 18) +
  tm_compass(type="arrow", position = c("left", "bottom")) +
  tm_scale_bar(breaks = c(0, 5, 10)) +
  tm_add_legend(type = "symbol", lwd = 2, col = c("#000080", "#8B0000"), labels = c("Bus Station", "MRT Station")) +
  tm_shape(cbd) + tm_polygons(col = "blue", alpha = 0.2, border.col = "grey") +
  tm_add_legend(type = "fill",  col = c("#E6E6FA"), labels = c("CBD"), title = "")



# Malls

mall_pp <- mall %>% dplyr::select(lon,lat) %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326)

tm_shape(subzone) + tm_polygons(col = "TOTAL", alpha = 1, style = "jenks", palette = "Greys", border.alpha = 0.2, title = "Population") +
  tm_shape(sg_pa) + tm_polygons(col = "white", alpha = 0,1, border.alpha = 0.2)+
  tm_shape(mall_pp) + tm_dots(col = "#008080", size = 0.3, alpha = 0.8, shape = 18) +
  tm_shape(range75) + tm_borders(alpha=.7, col = "#fb6a4a", lwd = 2) + tm_fill(alpha=.1, col = "#fb6a4a")+
  tm_shape(range50) + tm_borders(alpha=.7, col = "#de2d26", lwd = 2) + tm_fill(alpha=.1, col = "#de2d26")+
  tm_shape(range25) + tm_borders(alpha=.7, col = "#a50f15", lwd = 2) + tm_fill(alpha=.1, col = "#a50f15") +
  tm_compass(type="arrow", position = c("left", "bottom")) +
  tm_scale_bar(breaks = c(0, 5, 10)) +
  tm_add_legend(type = "line", lwd = 2, col = c("#fb6a4a", "#de2d26","#a50f15"), labels = c("75%", "50%", "25%"), title = "Shopping Mall KDE Range")


# All map

tm_shape(subzone) + tm_polygons(col = "TOTAL", alpha = 1, style = "jenks", palette = "Greys", border.alpha = 0.2, title = "Population") +
  tm_shape(sg_pa) + tm_polygons(col = "white", alpha = 0,1, border.alpha = 0.2)+
  tm_shape(hosp_pp) + tm_dots(col = "red", size = 0.1, alpha = 1, shape = 3) +
  tm_shape(uni_pp) + tm_dots(col = "red", size = 0.1, alpha = 1, shape = 3) +
  tm_shape(park_pp) + tm_dots(col = "red", size = 0.1, alpha = 1, shape = 3) +
  tm_shape(mall_pp) + tm_dots(col = "red", size = 0.1, alpha = 1, shape = 3) +
  tm_compass(type="arrow", position = c("left", "bottom")) +
  tm_scale_bar(breaks = c(0, 5, 10)) +
  tm_add_legend(type = "line", lwd = 2, col = c("#fb6a4a", "#de2d26","#a50f15"), labels = c("75%", "50%", "25%"), title = "Public Hospitals KDE Range")

 ############################################

# KDE map
sg_sp <- readOGR("SUBZONE")
window <- as.owin(sg_sp)
parks_pp <- parks_pp %>% st_coordinates()
coords_sp <- SpatialPoints(parks_pp, proj4string=CRS("+init=epsg:4326"))
kde.output <- kernelUD(coords_sp, h="href", grid = 1000)
# Set percentage
range75 <- getverticeshr(kde.output, percent = 75)
range50 <- getverticeshr(kde.output, percent = 50)
range25 <- getverticeshr(kde.output, percent = 25)
# output
tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "TOTAL", palette = "Greys", style = "jenks", border.alpha = 0.2, title = "Population") +
  tm_shape(range75) + tm_borders(alpha=.7, col = "#fb6a4a", lwd = 2) + tm_fill(alpha=.1, col = "#fb6a4a")+
  tm_shape(range50) + tm_borders(alpha=.7, col = "#de2d26", lwd = 2) + tm_fill(alpha=.1, col = "#de2d26")+
  tm_shape(range25) + tm_borders(alpha=.7, col = "#a50f15", lwd = 2) + tm_fill(alpha=.1, col = "#a50f15") +
  tm_compass(type="arrow", position = c("left", "bottom")) +
  tm_scale_bar(breaks = c(0, 5, 10)) +
  tm_add_legend(type = "line", lwd = 2, col = c("#fb6a4a", "#de2d26","#a50f15"), labels = c("75%", "50%", "25%"), title = "HDB resale flat transactions KDE Range")


sg_sp <- readOGR("SUBZONE")
window <- as.owin(sg_sp)
uni_pp <- uni_pp$geometry %>% st_coordinates()
coords_sp <- SpatialPoints(uni_pp, proj4string=CRS("+init=epsg:4326"))
kde.output <- kernelUD(coords_sp, h="href", grid = 1000)
# Set percentage
range75 <- getverticeshr(kde.output, percent = 75)
range50 <- getverticeshr(kde.output, percent = 50)
range25 <- getverticeshr(kde.output, percent = 25)

# Public Hospitals

  
#### park
  tm_shape(subzone) + tm_polygons(col = "TOTAL", alpha = 1, style = "jenks", palette = "Greys", border.alpha = 0.15, title = "Population") +
    tm_shape(sg_pa) + tm_polygons(col = "white", alpha = 0,1, border.alpha = 0.3)+
    tm_shape(parks_pp) + tm_dots(col = "#32CD32", size = 0.2, alpha = 1, shape = 15) +
    tm_shape(range75) + tm_borders(alpha=.7, col = "#fb6a4a", lwd = 2) + tm_fill(alpha=.1, col = "#fb6a4a")+
    tm_shape(range50) + tm_borders(alpha=.7, col = "#de2d26", lwd = 2) + tm_fill(alpha=.1, col = "#de2d26")+
    tm_shape(range25) + tm_borders(alpha=.7, col = "#a50f15", lwd = 2) + tm_fill(alpha=.1, col = "#a50f15") +
    tm_compass(type="arrow", position = c("left", "bottom")) +
    tm_scale_bar(breaks = c(0, 5, 10)) +
    tm_add_legend(type = "line", lwd = 2, col = c("#fb6a4a", "#de2d26","#a50f15"), labels = c("75%", "50%", "25%"), title = "Public Parks KDE Range")+
    tm_add_legend(type = "line", lwd = c(1, 0.8), col = c("black", "#BDC3C7"), labels = c("Planning Area", "Subzone"), title = "")


parks_pp = st_transform(parks_pp, "EPSG:4326")
train_pp = st_transform(train_pp, "EPSG:4326")
crs(train_pp)
transport.pp <- rbind(bus_pp, train_pp)
##################################################

# GWR R squared for Haversine distance

#2014
gwr2014 <- tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2014) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 1, size = 0.0005, title = "Local R Squared") +
  tm_layout(legend.show = F)

#2015
gwr2015 <- tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2015) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 1, size = 0.0005, title = "Local R Squared")+
  tm_layout(legend.show = F)


#2016
gwr2016 <- tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2016) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 1, size = 0.0005, title = "Local R Squared")+
  tm_layout(legend.show = F)


#2017
gwr2017 <- tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2017) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 1, size = 0.0005, title = "Local R Squared")+
  tm_layout(legend.show = F)

#2018
gwr2018 <- tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2018) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 1, size = 0.0005, title = "Local R Squared")+
  tm_layout(legend.show = F)


#2019
gwr2019 <- tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2019) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 1, size = 0.0005, title = "Local R Squared")+
  tm_layout(legend.show = F)



#2020
gwr2020 <- tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2020) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 1, size = 0.0005, title = "Local R Squared")+
  tm_layout(legend.show = F)


#2021
gwr2021 <- tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2021) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 1, size = 0.0005, title = "Local R Squared")+
  tm_layout(legend.show = F)



grid.newpage()
# assigns the cell size of the grid, in this case 2 by 2
pushViewport(viewport(layout=grid.layout(4,2)))
print(gwr2014, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(gwr2015, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(gwr2016, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
print(gwr2017, vp=viewport(layout.pos.col = 2, layout.pos.row =2))
print(gwr2018, vp=viewport(layout.pos.col = 1, layout.pos.row =3))
print(gwr2019, vp=viewport(layout.pos.col = 2, layout.pos.row =3))
print(gwr2020, vp=viewport(layout.pos.col = 1, layout.pos.row =4))
print(gwr2021, vp=viewport(layout.pos.col = 2, layout.pos.row =4))



####################

# GWR coef for real time travel distance

tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2021.rrr) + tm_dots(col = "rrr_cbd", style = "quantile", midpoint = 0,  alpha = 1, size = 0.001, title = "Distance to CBD coefficient") +
  tm_layout(legend.outside = F)

cbd_coef1 <-tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2021.rrr) + tm_dots(col = "remaining_lease_final", style = "quantile", midpoint = 0, alpha = 0.4, size = 0.001, title = "Remaining lease coefficient")+
  tm_layout(legend.outside = F, legend.show = T)


cbd_coef2 <-tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2021.rrr) + tm_dots(col = "rrr_train", style = "quantile", midpoint = 0,  alpha = 1, size = 0.001, title = "Distance to MRT coefficient") +
  tm_layout(legend.outside = F, legend.show = F)

cbd_coef3 <-tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2021.rrr) + tm_dots(col = "rrr_university", style = "quantile", midpoint = 0, alpha = 1, size = 0.1, title = "Distance to university coefficient")+
  tm_layout(legend.outside = T, legend.show = T)

cbd_coef4 <-tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2021.rrr) + tm_dots(col = "rrr_park", style = "quantile", midpoint = 0,  alpha = 1, size = 0.001, title = "Distance to park coefficient")+
  tm_layout(legend.outside = F, legend.show = F)

cbd_coef5 <-tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2021.rrr) + tm_dots(col = "rrr_mall",  style = "quantile", midpoint = 0, palette = "RdYlGn", alpha = 1, size = 0.001, title = "Distance to mall coefficient")+
  tm_layout(legend.outside = F, legend.show = F)

cbd_coef6 <-tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2021.rrr) + tm_dots(col = "rrr_hospital",  style = "quantile", midpoint = 0, palette = "RdYlGn", alpha = 1, size = 0.1, title = "Distance to hospital coefficient")+
  tm_layout(legend.outside = T, legend.show = T)

cbd_coef7 <-tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2021.rrr) + tm_dots(col = "rrr_bus",  style = "quantile", midpoint = 0, palette = "RdYlGn", alpha = 1, size = 0.1, title = "Distance to bus coefficient")+
  tm_layout(legend.outside = T, legend.show = T)

cbd_coef8 <-tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2021.rrr) + tm_dots(col = "mean_area",  style = "quantile", midpoint = 0, palette = "RdYlGn", alpha = 1, size = 0.1, title = "Floor area coefficient")+
  tm_layout(legend.outside = T, legend.show = T)

grid.newpage()
# assigns the cell size of the grid, in this case 2 by 2
pushViewport(viewport(layout=grid.layout(4,2)))
print(cbd_coef1, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(cbd_coef2, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(cbd_coef3, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
print(cbd_coef4, vp=viewport(layout.pos.col = 2, layout.pos.row =2))
print(cbd_coef5, vp=viewport(layout.pos.col = 1, layout.pos.row =3))
print(cbd_coef6, vp=viewport(layout.pos.col = 2, layout.pos.row =3))
print(cbd_coef7, vp=viewport(layout.pos.col = 1, layout.pos.row =4))
print(cbd_coef8, vp=viewport(layout.pos.col = 2, layout.pos.row =4))



# Local R squared
#2014
gwr2014.rrr <- tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2014.rrr) + tm_dots(col = "localR2", style = "cont", midpoint = NA, breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 1, size = 0.0005, title = "Local R Squared") +
  tm_layout(legend.show = T)

#2015
gwr2015.rrr <- tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2015.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 1, size = 0.0005, title = "Local R Squared")+
  tm_layout(legend.show = F)


#2016
gwr2016.rrr <- tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2016.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 1, size = 0.0005, title = "Local R Squared")+
  tm_layout(legend.show = F)


#2017
gwr2017.rrr <- tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2017.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 1, size = 0.0005, title = "Local R Squared")+
  tm_layout(legend.show = F)

#2018
gwr2018.rrr <- tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2018.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 1, size = 0.0005, title = "Local R Squared")+
  tm_layout(legend.show = F)


#2019
gwr2019.rrr <- tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2019.rrr) + tm_dots(col = "localR2", style = "cont", midpoint = NA, breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 1, size = 0.0005, title = "Local R Squared")+
  tm_layout(legend.show = T)



#2020
gwr2020.rrr <- tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2020.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 1, size = 0.0005, title = "Local R Squared")+
  tm_layout(legend.show = F)


#2021
gwr2021.rrr <- tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.2) +
  tm_shape(pp2021.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 1, size = 0.0005, title = "Local R Squared")+
  tm_layout(legend.show = T, legend.outside = T)

grid.newpage()
# assigns the cell size of the grid, in this case 2 by 2
pushViewport(viewport(layout=grid.layout(4,2)))
print(gwr2014.rrr, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(gwr2015.rrr, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(gwr2016.rrr, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
print(gwr2017.rrr, vp=viewport(layout.pos.col = 2, layout.pos.row =2))
print(gwr2018.rrr, vp=viewport(layout.pos.col = 1, layout.pos.row =3))
print(gwr2019.rrr, vp=viewport(layout.pos.col = 2, layout.pos.row =3))
print(gwr2020.rrr, vp=viewport(layout.pos.col = 1, layout.pos.row =4))
print(gwr2021.rrr, vp=viewport(layout.pos.col = 2, layout.pos.row =4))


##### Regional GWR maps ######@

### PA
west  <- c(4, 18, 9, 11, 43, 42, 34)
west_sp <- sg_pa[west,]
north <- c(37, 25, 46, 20, 51)
north_sp <- sg_pa[north,]
north_east <- c(5, 27, 17, 26, 22)
north_east_sp <- sg_pa[north_east,]
east <- c(31, 16, 6)
east_sp <- sg_pa[east,]
central <- c(12, 23, 33, 15, 35, 53, 1, 13, 14, 7, 10, 32, 36, 38, 39, 40, 44, 45, 52, 54,55, 47)
central_sp <- sg_pa[central,]


### west subzone
west.subzone <- c(59,		67,		110,		151,		165,		170,		178,		179,		186,		201,		136,		138,		163,		169,		175,		188,		194,		251,		252,
                  48,		52,		97,		99,		119,		128,		146,		181,		253, 
                  206,		238,		243,		247,		270,		277,
                  176,		228,		237,		241,		256,		291,		292,
                  144,	173,	187,	195,	196,	197,	215,	219,	230)

west.subzone <- subzone[west.subzone,]

west_sp <- st_transform(west_sp, "EPSG:4326")
west.pp2014.rrr <- pp2014.rrr[west_sp, ]
west.pp2015.rrr <- pp2015.rrr[west_sp, ]
west.pp2016.rrr <- pp2016.rrr[west_sp, ]
west.pp2017.rrr <- pp2017.rrr[west_sp, ]
west.pp2018.rrr <- pp2018.rrr[west_sp, ]
west.pp2019.rrr <- pp2019.rrr[west_sp, ]
west.pp2020.rrr <- pp2020.rrr[west_sp, ]
west.pp2021.rrr <- pp2021.rrr[west_sp, ]


west1 <- tm_shape(west_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1)+
  tm_shape(west.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(west.pp2014.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

west2 <- tm_shape(west_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1)+
  tm_shape(west.subzone) + tm_polygons(col = "white", alpha = 0, border.alpha = 0.2)+
  tm_shape(west.pp2015.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

west3 <- tm_shape(west_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1)+
  tm_shape(west.subzone) + tm_polygons(col = "white", alpha = 0, border.alpha = 0.2)+
  tm_shape(west.pp2016.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

west4 <- tm_shape(west_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1)+
  tm_shape(west.subzone) + tm_polygons(col = "white", alpha = 0, border.alpha = 0.2)+
  tm_shape(west.pp2017.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

west5 <- tm_shape(west_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1)+
  tm_shape(west.subzone) + tm_polygons(col = "white", alpha = 0, border.alpha = 0.2)+
  tm_shape(west.pp2018.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

west6 <- tm_shape(west_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1)+
  tm_shape(west.subzone) + tm_polygons(col = "white", alpha = 0, border.alpha = 0.2)+
  tm_shape(west.pp2019.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

west7 <- tm_shape(west_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1)+
  tm_shape(west.subzone) + tm_polygons(col = "white", alpha = 0, border.alpha = 0.2)+
  tm_shape(west.pp2020.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

west8 <- tm_shape(west_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1)+
  tm_shape(west.subzone) + tm_polygons(col = "white", alpha = 0, border.alpha = 0.2)+
  tm_shape(west.pp2021.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = T)

tm_shape(sg_pa) + tm_polygons(col = "gray", alpha = 0.5, border.alpha = 0.5) +
  tm_shape(subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.1) +
  tm_shape(west_sp) + tm_polygons(col = "blue", alpha = 0.2, border.alpha = 0.1)

grid.newpage()
# assigns the cell size of the grid, in this case 2 by 2
pushViewport(viewport(layout=grid.layout(2,4)))
print(west1, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(west2, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(west3, vp=viewport(layout.pos.col = 3, layout.pos.row =1))
print(west4, vp=viewport(layout.pos.col = 4, layout.pos.row =1))
print(west5, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
print(west6, vp=viewport(layout.pos.col = 2, layout.pos.row =2))
print(west7, vp=viewport(layout.pos.col = 3, layout.pos.row =2))
print(west8, vp=viewport(layout.pos.col = 4, layout.pos.row =2))


###### north subzone
north.subzone <- c(223,	224,	261,	264,	265,	266,	267,	268,	280,	281,	282,	
                  283,	286,	287,	289,	295,	297,	298,	300,	301,	304,	305,	306,	309,	310,	312,	313,	314,	315,	316,	317,	318,	319,	320)
north.subzone <- subzone[north.subzone,]

north_sp <- st_transform(north_sp, "EPSG:4326")
north.pp2014.rrr <- pp2014.rrr[north_sp, ]
north.pp2015.rrr <- pp2015.rrr[north_sp, ]
north.pp2016.rrr <- pp2016.rrr[north_sp, ]
north.pp2017.rrr <- pp2017.rrr[north_sp, ]
north.pp2018.rrr <- pp2018.rrr[north_sp, ]
north.pp2019.rrr <- pp2019.rrr[north_sp, ]
north.pp2020.rrr <- pp2020.rrr[north_sp, ]
north.pp2021.rrr <- pp2021.rrr[north_sp, ]


north1 <- tm_shape(north_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(north.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(north.pp2014.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

north2 <- tm_shape(north_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(north.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(north.pp2015.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)


north3 <- tm_shape(north_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(north.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(north.pp2016.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)


north4 <- tm_shape(north_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(north.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(north.pp2017.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)


north5 <- tm_shape(north_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(north.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(north.pp2018.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)


north6 <- tm_shape(north_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(north.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(north.pp2019.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)


north7 <- tm_shape(north_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(north.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(north.pp2020.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

north8 <- tm_shape(north_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(north.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(north.pp2021.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

tm_shape(sg_pa) + tm_polygons(col = "gray", alpha = 0.5, border.alpha = 0.5) +
  tm_shape(subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.1) +
  tm_shape(north_sp) + tm_polygons(col = "blue", alpha = 0.2, border.alpha = 0.1)


grid.newpage()
# assigns the cell size of the grid, in this case 2 by 2
pushViewport(viewport(layout=grid.layout(2,4)))
print(north1, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(north2, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(north3, vp=viewport(layout.pos.col = 3, layout.pos.row =1))
print(north4, vp=viewport(layout.pos.col = 4, layout.pos.row =1))
print(north5, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
print(north6, vp=viewport(layout.pos.col = 2, layout.pos.row =2))
print(north7, vp=viewport(layout.pos.col = 3, layout.pos.row =2))
print(north8, vp=viewport(layout.pos.col = 4, layout.pos.row =2))

######## northeast subzone ########
northeast.subzone <- c(137,	141,	156,	157,	158,	159,	160,	172,	198,	199,	203,	204,	
                       208,	209,	210,	211,	213,	220,	226,	233,	234,	235,	236,	239,	245,	246,	257,	259,	260,	273,	276,	279,	288,	293,	296,	299)
northeast.subzone <- subzone[northeast.subzone,]

north_east_sp <- st_transform(north_east_sp, "EPSG:4326")
northeast.pp2014.rrr <- pp2014.rrr[north_east_sp, ]
northeast.pp2015.rrr <- pp2015.rrr[north_east_sp, ]
northeast.pp2016.rrr <- pp2016.rrr[north_east_sp, ]
northeast.pp2017.rrr <- pp2017.rrr[north_east_sp, ]
northeast.pp2018.rrr <- pp2018.rrr[north_east_sp, ]
northeast.pp2019.rrr <- pp2019.rrr[north_east_sp, ]
northeast.pp2020.rrr <- pp2020.rrr[north_east_sp, ]
northeast.pp2021.rrr <- pp2021.rrr[north_east_sp, ]


northeast1 <- tm_shape(north_east_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(northeast.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(northeast.pp2014.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

northeast2 <- tm_shape(north_east_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(northeast.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(northeast.pp2015.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

northeast3 <- tm_shape(north_east_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(northeast.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(northeast.pp2016.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

northeast4 <- tm_shape(north_east_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(northeast.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(northeast.pp2017.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

northeast5 <- tm_shape(north_east_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(northeast.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(northeast.pp2018.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

northeast6 <- tm_shape(north_east_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(northeast.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(northeast.pp2019.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

northeast7 <- tm_shape(north_east_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(northeast.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(northeast.pp2020.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

northeast8 <- tm_shape(north_east_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(northeast.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(northeast.pp2021.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

tm_shape(sg_pa) + tm_polygons(col = "gray", alpha = 0.5, border.alpha = 0.5) +
  tm_shape(subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.1) +
  tm_shape(north_east_sp) + tm_polygons(col = "blue", alpha = 0.2, border.alpha = 0.1)


grid.newpage()
# assigns the cell size of the grid, in this case 2 by 2
pushViewport(viewport(layout=grid.layout(2,4)))
print(northeast1, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(northeast2, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(northeast3, vp=viewport(layout.pos.col = 3, layout.pos.row =1))
print(northeast4, vp=viewport(layout.pos.col = 4, layout.pos.row =1))
print(northeast5, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
print(northeast6, vp=viewport(layout.pos.col = 2, layout.pos.row =2))
print(northeast7, vp=viewport(layout.pos.col = 3, layout.pos.row =2))
print(northeast8, vp=viewport(layout.pos.col = 4, layout.pos.row =2))

####### east subzone #####
east.subzone <- c(51,	105,	120,	127,	143,	149,	183,	184,	185,	189,	192,	193,	202,	205,	207,	214,	227,	229,	244,	269,	278)
east.subzone <- subzone[east.subzone,]

east_sp <- st_transform(east_sp, "EPSG:4326")
east.pp2014.rrr <- pp2014.rrr[east_sp, ]
east.pp2015.rrr <- pp2015.rrr[east_sp, ]
east.pp2016.rrr <- pp2016.rrr[east_sp, ]
east.pp2017.rrr <- pp2017.rrr[east_sp, ]
east.pp2018.rrr <- pp2018.rrr[east_sp, ]
east.pp2019.rrr <- pp2019.rrr[east_sp, ]
east.pp2020.rrr <- pp2020.rrr[east_sp, ]
east.pp2021.rrr <- pp2021.rrr[east_sp, ]

east1 <- tm_shape(east_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(east.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(east.pp2014.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

east2 <- tm_shape(east_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(east.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(east.pp2015.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

east3 <- tm_shape(east_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(east.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(east.pp2016.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

east4 <- tm_shape(east_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(east.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(east.pp2017.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

east5 <- tm_shape(east_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(east.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(east.pp2018.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

east6 <- tm_shape(east_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(east.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(east.pp2019.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

east7 <- tm_shape(east_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(east.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(east.pp2020.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

east8 <- tm_shape(east_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(east.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(east.pp2021.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.02, title = "Local R Squared") +
  tm_layout(legend.show = F)

tm_shape(sg_pa) + tm_polygons(col = "gray", alpha = 0.5, border.alpha = 0.5) +
  tm_shape(subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.1) +
  tm_shape(east_sp) + tm_polygons(col = "blue", alpha = 0.2, border.alpha = 0.1)



grid.newpage()
# assigns the cell size of the grid, in this case 2 by 2
pushViewport(viewport(layout=grid.layout(2,4)))
print(east1, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(east2, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(east3, vp=viewport(layout.pos.col = 3, layout.pos.row =1))
print(east4, vp=viewport(layout.pos.col = 4, layout.pos.row =1))
print(east5, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
print(east6, vp=viewport(layout.pos.col = 2, layout.pos.row =2))
print(east7, vp=viewport(layout.pos.col = 3, layout.pos.row =2))
print(east8, vp=viewport(layout.pos.col = 4, layout.pos.row =2))


#### Central area subzone ####
central.subzone <- c(1,	2,	3,	5,	6,	7,	14,	15,	17,	20,	21,	24,	25,	27,	28,	29,	30,	32,	34,	35,	36,	37,	38,	43,	46,	47,
                     50,	53,	54,	55,	56,	57,	60,	61,	62,	63,	64,	66,	69,	72,	73,	74,	75,	76,	77,	78,	79,	80,	81,	82,	83,	84,
                     85,	86,	87,	88,	89,	90,	91,	92,	93,	95,	96,	98,	100,	101,	102,	103,	104,	106,	107,	108,	109,	111,	114,
                     115,	116,	118,	123,	124,	125,	129,	130,	131,	132,	133,	134,	135,	142,	145,	147,	148,	150,
                     152,	153,	154,	155,	161,	162,	164,	166,	167,	168,	171,	174,	177,	180,	182,	190,	191,	218,	249,	250,	254,	255,
                     4,	13,	16,	18,	19,	22,	23,	26,	31,	33,	39,	40,	41,	42,	44,	70,	71)
central.subzone <- subzone[central.subzone,]

central_sp <- st_transform(central_sp, "EPSG:4326")
central.pp2014.rrr <- pp2014.rrr[central_sp, ]
central.pp2015.rrr <- pp2015.rrr[central_sp, ]
central.pp2016.rrr <- pp2016.rrr[central_sp, ]
central.pp2017.rrr <- pp2017.rrr[central_sp, ]
central.pp2018.rrr <- pp2018.rrr[central_sp, ]
central.pp2019.rrr <- pp2019.rrr[central_sp, ]
central.pp2020.rrr <- pp2020.rrr[central_sp, ]
central.pp2021.rrr <- pp2021.rrr[central_sp, ]

central1 <- tm_shape(central_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(central.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(central.pp2014.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.01, title = "Local R Squared") +
  tm_layout(legend.show = F) +
  tm_shape(cbd) + tm_polygons(col = "red", alpha = 0.2, border.col = "grey") +
  tm_add_legend(type = "fill", col = "red", labels = "CBD")

central2 <- tm_shape(central_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(central.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(central.pp2015.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.01, title = "Local R Squared") +
  tm_layout(legend.show = F) +
  tm_shape(cbd) + tm_polygons(col = "red", alpha = 0.2, border.col = "grey") +
  tm_add_legend(type = "fill", col = "red", labels = "CBD")


central3 <- tm_shape(central_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(central.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(central.pp2016.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.01, title = "Local R Squared") +
  tm_layout(legend.show = F) +
  tm_shape(cbd) + tm_polygons(col = "red", alpha = 0.2, border.col = "grey") +
  tm_add_legend(type = "fill", col = "red", labels = "CBD")


central4 <- tm_shape(central_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(central.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(central.pp2017.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.01, title = "Local R Squared") +
  tm_layout(legend.show = F) +
  tm_shape(cbd) + tm_polygons(col = "red", alpha = 0.2, border.col = "grey") +
  tm_add_legend(type = "fill", col = "red", labels = "CBD")


central5 <- tm_shape(central_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(central.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(central.pp2018.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.01, title = "Local R Squared") +
  tm_layout(legend.show = F) +
  tm_shape(cbd) + tm_polygons(col = "red", alpha = 0.2, border.col = "grey") +
  tm_add_legend(type = "fill", col = "red", labels = "CBD")


central6 <- tm_shape(central_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(central.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(central.pp2019.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.01, title = "Local R Squared") +
  tm_layout(legend.show = F) +
  tm_shape(cbd) + tm_polygons(col = "red", alpha = 0.2, border.col = "grey") +
  tm_add_legend(type = "fill", col = "red", labels = "CBD")


central7 <- tm_shape(central_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(central.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(central.pp2020.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.01, title = "Local R Squared") +
  tm_layout(legend.show = F) +
  tm_shape(cbd) + tm_polygons(col = "red", alpha = 0.2, border.col = "grey") +
  tm_add_legend(type = "fill", col = "red", labels = "CBD")

central8 <- tm_shape(central_sp) + tm_polygons(col = "gray", alpha = 0.2, border.alpha = 1) +
  tm_shape(central.subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.3)+
  tm_shape(central.pp2021.rrr) + tm_dots(col = "localR2", style = "fixed", breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9,1), palette = "GnBu", alpha = 0.5, size = 0.01, title = "Local R Squared") +
  tm_layout(legend.show = F) +
  tm_shape(cbd) + tm_polygons(col = "red", alpha = 0.2, border.col = "grey") +
  tm_add_legend(type = "fill", col = "red", labels = "CBD")

tm_shape(sg_pa) + tm_polygons(col = "gray", alpha = 0.5, border.alpha = 0.5) +
  tm_shape(subzone) + tm_polygons(col = "gray", alpha = 0, border.alpha = 0.1) +
  tm_shape(central_sp) + tm_polygons(col = "blue", alpha = 0.2, border.alpha = 0.1)


grid.newpage()
# assigns the cell size of the grid, in this case 2 by 2
pushViewport(viewport(layout=grid.layout(3,3)))
print(central1, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(central2, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(central3, vp=viewport(layout.pos.col = 3, layout.pos.row =1))
print(central4, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
print(central5, vp=viewport(layout.pos.col = 2, layout.pos.row =2))
print(central6, vp=viewport(layout.pos.col = 3, layout.pos.row =2))
print(central7, vp=viewport(layout.pos.col = 1, layout.pos.row =3))
print(central8, vp=viewport(layout.pos.col = 2, layout.pos.row =3))

save.image(file = "maps3.RData")
