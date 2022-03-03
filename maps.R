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
house <- read.csv("house_complete_v4.csv")
house <- na.omit(house)
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
  tm_text("PLN_AREA_N", col = "black", size = 0.4, alpha = 1, fontface = "bold") +
  tm_shape(central_sp) + tm_polygons(col = "#F0B27A", alpha = 1) +
  tm_shape(cbd) + tm_polygons(col = "red", alpha = 1) +
  tm_compass(type="arrow", position = c("left", "bottom")) +
  tm_scale_bar(breaks = c(0, 5, 10)) +
  tm_add_legend(type = "fill", lwd = 2, col = c("#FCF3CF", "#E8DAEF","#D4EFDF", "#FDEDEC", "#F0B27A"), labels = c("West", "North", "North East","East","Central"), title = "Regions")+
  tm_shape(subzone) + tm_polygons(col = "#F9E79F", alpha = 0.0, border.alpha = 0.3) +
  tm_add_legend(type = "line", lwd = c(1, 0.8), col = c("black", "#BDC3C7"), labels = c("Planning Zone", "Subzone"), title = "") +
  tm_add_legend(type = "fill",  col = c("red"), labels = c("CBD"), title = "")


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
  tm_shape(pp2014) + tm_dots(col = "house2014.mean_price_per_m2", style = "fixed", breaks = c(0,2500,5000,7500,11000),size = 0.0001, alpha = 1, palette = "YlOrRd", title = "Price per Square Meter (SGD)") +
  tm_layout(legend.show = F)

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
house_pp <- pp %>% st_coordinates()
coords_sp <- SpatialPoints(house_pp, proj4string=CRS("+init=epsg:4326"))
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
  tm_shape(subzone) + tm_polygons(col = "TOTAL", alpha = 1, style = "jenks", palette = "Greys", border.alpha = 0.2, title = "Population") +
    tm_shape(sg_pa) + tm_polygons(col = "white", alpha = 0,1, border.alpha = 0.2)+
    tm_shape(parks_pp) + tm_dots(col = "#32CD32", size = 0.2, alpha = 1, shape = 15) +
    tm_shape(range75) + tm_borders(alpha=.7, col = "#fb6a4a", lwd = 2) + tm_fill(alpha=.1, col = "#fb6a4a")+
    tm_shape(range50) + tm_borders(alpha=.7, col = "#de2d26", lwd = 2) + tm_fill(alpha=.1, col = "#de2d26")+
    tm_shape(range25) + tm_borders(alpha=.7, col = "#a50f15", lwd = 2) + tm_fill(alpha=.1, col = "#a50f15") +
    tm_compass(type="arrow", position = c("left", "bottom")) +
    tm_scale_bar(breaks = c(0, 5, 10)) +
    tm_add_legend(type = "line", lwd = 2, col = c("#fb6a4a", "#de2d26","#a50f15"), labels = c("75%", "50%", "25%"), title = "Public Parks KDE Range")


parks_pp = st_transform(parks_pp, "EPSG:4326")
train_pp = st_transform(train_pp, "EPSG:4326")
crs(train_pp)
transport.pp <- rbind(bus_pp, train_pp)
##################################################

# GWR R squared 
#2014
tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.3) +
  tm_shape(pp2014) + tm_dots(col = "results2014.localR2", style = "fisher", palette = "Reds", alpha = 1, size = 0.005, title = "Local R Squared") +
  tm_compass(type="arrow", position = c("left", "bottom")) +
  tm_scale_bar(breaks = c(0, 5, 10))

#2015
tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.3) +
  tm_shape(pp2015) + tm_dots(col = "results2015.localR2", style = "fisher", palette = "Reds", alpha = 1, size = 0.005, title = "Local R Squared") +
  tm_compass(type="arrow", position = c("left", "bottom")) +
  tm_scale_bar(breaks = c(0, 5, 10))

#2016
tm_shape(sg_pa) + tm_polygons(col = "white", border.alpha = 1)+
  tm_shape(subzone) + tm_polygons(col = "grey", alpha = 0.4, border.alpha = 0.3) +
  tm_shape(pp2016) + tm_dots(col = "results2016.localR2", style = "fisher", palette = "Reds", alpha = 1, size = 0.005, title = "Local R Squared") +
  tm_compass(type="arrow", position = c("left", "bottom")) +
  tm_scale_bar(breaks = c(0, 5, 10))

# population per amenity
st_crs(subzone) <- 4326
st_crs(park_pp) <- 4326
park_pp = st_transform(park_pp, "EPSG:4326") # set planar CRS
subzone = st_transform(subzone, "EPSG:4326") # set planar CRS
subzone$park <- lengths(st_intersects(subzone, park_pp))


save.image(file = "maps3.RData")
