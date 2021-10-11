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

PA <- read_sf("PA")

tm_shape(PA) + tm_polygons(col = "white", border.alpha = 0.5) 

amk <- c(5)
amk_town <- PA[amk,]

bedok <-c(6)
bedok_town <- PA[bedok,]

bishan <-c(7)
bishan_town <- PA[bishan,]

bt <-c(9)
bt_town <- PA[bt,]

bm <-c(10)
bm_town <- PA[bm,]

bp <-c(11)
bp_town <- PA[bp,]

cck <-c(42)
cck_town <- PA[cck,]

clementi <-c(43)
clementi_town <- PA[clementi,]

geylang <-c(13)
geylang_town <- PA[geylang,]

hougang <-c(17)
hougang_town <- PA[hougang,]

jw <-c(4)
jw_town <- PA[jw,]

je <-c(18)
je_town <- PA[je,]

kallang <-c(14)
kallang_town <- PA[kallang,]

pr <-c(16)
pr_town <- PA[pr,]

punggol <-c(22)
punggol_town <- PA[punggol,]

queenstown <-c(23)
queenstown_town <- PA[queenstown,]

sembawang <-c(25)
sembawang_town <- PA[sembawang,]

sengkang <-c(26)
sengkang_town <- PA[sengkang,]

serangoon <-c(27)
serangoon_town <- PA[serangoon,]

tampines <-c(31)
tampines_town <- PA[tampines,]

tp <-c(35)
tp_town <- PA[tp,]

woodlands <-c(37)
woodlands_town <- PA[woodlands,]

yishun <-c(51)
yishun_town <- PA[yishun,]



tm_shape(PA) + tm_polygons(col = "white", border.alpha = 0.5) + 
  tm_shape(amk_town) + tm_polygons(alpha = 0.1, col = "blue", border.alpha = 1, border.col = "blue") +
  tm_shape(bedok_town) + tm_polygons(alpha = 0.1, col = "blue", border.alpha = 1, border.col = "blue") +
  tm_shape(bm_town) + tm_polygons(alpha = 0.1, col = "blue", border.alpha = 1, border.col = "blue")+
  tm_shape(bp_town) + tm_polygons(alpha = 0.1, col = "blue", border.alpha = 1, border.col = "blue")+
  tm_shape(bt_town) + tm_polygons(alpha = 0.1, col = "blue", border.alpha = 1, border.col = "blue")+
  tm_shape(cck_town) + tm_polygons(alpha = 0.1, col = "blue", border.alpha = 1, border.col = "blue")+
  tm_shape(clementi_town) + tm_polygons(alpha = 0.1, col = "blue", border.alpha = 1, border.col = "blue")+
  tm_shape(geylang_town) + tm_polygons(alpha = 0.1, col = "blue", border.alpha = 1, border.col = "blue")+
  tm_shape(hougang_town) + tm_polygons(alpha = 0.1, col = "blue", border.alpha = 1, border.col = "blue")+
  tm_shape(je_town) + tm_polygons(alpha = 0.1, col = "blue", border.alpha = 1, border.col = "blue")+
  tm_shape(jw_town) + tm_polygons(alpha = 0.1, col = "blue", border.alpha = 1, border.col = "blue")+
  tm_shape(kallang_town) + tm_polygons(alpha = 0.1, col = "blue", border.alpha = 1, border.col = "blue")+
  tm_shape(pr_town) + tm_polygons(alpha = 0.1, col = "blue", border.alpha = 1, border.col = "blue")+
  tm_shape(punggol_town) + tm_polygons(alpha = 0.1, col = "blue", border.alpha = 1, border.col = "blue")+
  tm_shape(queenstown_town) + tm_polygons(alpha = 0.1, col = "blue", border.alpha = 1, border.col = "blue")+
  tm_shape(sembawang_town) + tm_polygons(alpha = 0.1, col = "blue", border.alpha = 1, border.col = "blue")+
  tm_shape(sengkang_town) + tm_polygons(alpha = 0.1, col = "blue", border.alpha = 1, border.col = "blue")+
  tm_shape(serangoon_town) + tm_polygons(alpha = 0.1, col = "blue", border.alpha = 1, border.col = "blue")+
  tm_shape(tampines_town) + tm_polygons(alpha = 0.1, col = "blue", border.alpha = 1, border.col = "blue") +
  tm_shape(tp_town) + tm_polygons(alpha = 0.1, col = "blue", border.alpha = 1, border.col = "blue")+
  tm_shape(woodlands_town) + tm_polygons(alpha = 0.1, col = "blue", border.alpha = 1, border.col = "blue")+
  tm_shape(yishun_town) + tm_polygons(alpha = 0.1, col = "blue", border.alpha = 1, border.col = "blue")



which(PA$PLN_AREA_N == "ANG MO KIO", arr.ind = T) # 5
which(PA$PLN_AREA_N == "BEDOK", arr.ind = T) #6
which(PA$PLN_AREA_N == "BISHAN", arr.ind = T) #7
which(PA$PLN_AREA_N == "BUKIT BATOK", arr.ind = T) #9
which(PA$PLN_AREA_N == "Ang Mo Kio", arr.ind = T)
which(PA$PLN_AREA_N == "Ang Mo Kio", arr.ind = T)



####### Data cleaning: joining datasets

price4 <- read.csv("resale-flat-prices-based-on-registration-date-from-jan-2017-onwards.csv")
price3 <- read.csv("resale-flat-prices-based-on-registration-date-from-jan-2015-to-dec-2016.csv")
price2 <- read.csv("resale-flat-prices-based-on-registration-date-from-mar-2012-to-dec-2014.csv")

price2012 <- price2 %>% filter(month == "2012-03" | month == "2012-04" | month == "2012-05" | month == "2012-06" | month == "2012-07" | month == "2012-08" | month == "2012-09" 
                               | month == "2012-10" | month == "2012-11" | month == "2012-12")

price2013 <- price2 %>% filter(month == "2013-01" | month == "2013-02" | month == "2013-03" | month == "2013-04" | month == "2013-05" | month == "2013-06" | month == "2013-07" 
                               | month == "2013-08" | month == "2013-09" | month == "2013-10" | month == "2013-11" | month == "2013-12")
                            

price2014 <- price2 %>% filter(month == "2014-01" | month == "2014-02" | month == "2014-03" | month == "2014-04" | month == "2014-05" | month == "2014-06" | month == "2014-07" 
                               | month == "2014-08" | month == "2014-09" | month == "2014-10" | month == "2014-11" | month == "2014-12")


price2012$remaining_lease = (99 - (2012 - price2012$lease_commence_date))
price2013$remaining_lease = (99 - (2013 - price2013$lease_commence_date))
price2014$remaining_lease = (99 - (2014 - price2014$lease_commence_date))
price2_final <- rbind(price2012, price2013, price2014)

rm(price2012)
rm(price2013)
rm(price2014)

housing_price <- rbind(price4, price3)
housing_price_final <- rbind(housing_price, price2_final)

housing_price_final$add <- paste("Block", housing_price_final$block, housing_price_final$street_name, "Singapore")



subset(housing_price_final, duplicated(m))





