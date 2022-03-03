library(GGally)

house <- read.csv("house_complete_v3.csv")

mean(house2014$price_per_m2, na.rm = T)
mean(house2015$price_per_m2, na.rm = T)
mean(house2016$price_per_m2, na.rm = T)
mean(house2017$price_per_m2, na.rm = T)
mean(house2018$price_per_m2, na.rm = T)
mean(house2019$price_per_m2, na.rm = T)
mean(house2020$price_per_m2, na.rm = T)
mean(house2021$price_per_m2, na.rm = T)

sg_pa <- st_transform(sg_pa, "EPSG:4326")
st_crs(pp) <- 4326

pp2014 <- house2014 %>% dplyr::select(lon,lat) %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326)

pp2015 <- house2015 %>% dplyr::select(lon,lat) %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326)

pp2016 <- house2016 %>% dplyr::select(lon,lat) %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326)

pp2017 <- house2017 %>% dplyr::select(lon,lat) %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326)

pp2018 <- house2018 %>% dplyr::select(lon,lat) %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326)

pp2019 <- house2019 %>% dplyr::select(lon,lat) %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326)

pp2020 <- house2020 %>% dplyr::select(lon,lat) %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326)

pp2021 <- house2021 %>% dplyr::select(lon,lat) %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326)

pp2014 <- cbind(pp2014, house2014$mean_price_per_m2)

pp2015 <- cbind(pp2015, house2015$mean_price_per_m2)

pp2016 <- cbind(pp2016, house2016$mean_price_per_m2)

pp2017 <- cbind(pp2017, house2017$mean_price_per_m2)

pp2018 <- cbind(pp2018, house2018$mean_price_per_m2)

pp2019 <- cbind(pp2019, house2019$mean_price_per_m2)

pp2020 <- cbind(pp2020, house2020$mean_price_per_m2)

pp2021 <- cbind(pp2021, house2021$mean_price_per_m2)


sg_pa$p_polygon2014 <- lengths(st_intersects(sg_pa, pp2014))
sg_pa$p_polygon2015 <- lengths(st_intersects(sg_pa, pp2015))
sg_pa$p_polygon2016 <- lengths(st_intersects(sg_pa, pp2016))
sg_pa$p_polygon2017 <- lengths(st_intersects(sg_pa, pp2017))
sg_pa$p_polygon2018 <- lengths(st_intersects(sg_pa, pp2018))
sg_pa$p_polygon2019 <- lengths(st_intersects(sg_pa, pp2019))
sg_pa$p_polygon2020 <- lengths(st_intersects(sg_pa, pp2020))
sg_pa$p_polygon2021 <- lengths(st_intersects(sg_pa, pp2021))

mean(house$bus_distance, na.rm = T)
hist(house$bus_distance, xlab = "Distance to the nearest bus stop (meter)")
summary(house$bus_distance)


amenity_distance <- house %>% dplyr::select("bus_distance",
                                     "cbd_distance",
                                     "train_distance",
                                     "mall_distance",
                                     "park_distance",
                                     "uni_distance",
                                     "hospital_distance")

# multinearity
ggpairs(amenity_distance[,1:7], lower = list(continuous=wrap("points",position=position_jitter(height=.2, width=.2), alpha = 0.2, diag = list(continuous = "barDiag"))))


