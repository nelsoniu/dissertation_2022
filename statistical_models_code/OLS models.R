library(stargazer)
library(lm.beta)
house <- read.csv("house_complete_v4.csv")


table(house$year)

# 2014 2015 2016 2017 2018 2019 2020 2021 
# 6006 6711 6946 7011 7190 7330 7446 7401 

house2014 <- house[house$year == 2014,]
house2015 <- house[house$year == 2015,]
house2016 <- house[house$year == 2016,]
house2017 <- house[house$year == 2017,]
house2018 <- house[house$year == 2018,]
house2019 <- house[house$year == 2019,]
house2020 <- house[house$year == 2020,]
house2021 <- house[house$year == 2021,]

house2014 <- na.omit(house2014)
house2015 <- na.omit(house2015)
house2016 <- na.omit(house2016)
house2017 <- na.omit(house2017)
house2018 <- na.omit(house2018)
house2019 <- na.omit(house2019)
house2020 <- na.omit(house2020)
house2021 <- na.omit(house2021)



ols2014 <- lm(mean_price_per_m2 ~ cbd_distance + uni_distance + 
             mall_distance + train_distance + park_distance + hospital_distance + bus_distance + remaining_lease_final + mean_area, data = house2014)
ols2015 <- lm(mean_price_per_m2 ~ cbd_distance + uni_distance + 
             mall_distance + train_distance + park_distance + hospital_distance + bus_distance + remaining_lease_final + mean_area, data = house2015)
ols2016 <-lm(mean_price_per_m2 ~ cbd_distance + uni_distance + 
             mall_distance + train_distance + park_distance + hospital_distance + bus_distance + remaining_lease_final + mean_area, data = house2016)
ols2017 <-lm(mean_price_per_m2 ~ cbd_distance + uni_distance +
             mall_distance + train_distance + park_distance + hospital_distance + bus_distance + remaining_lease_final + mean_area, data = house2017)
ols2018 <-lm(mean_price_per_m2 ~ cbd_distance + uni_distance + 
             mall_distance + train_distance + park_distance + hospital_distance + bus_distance + remaining_lease_final + mean_area, data = house2018)
ols2019 <-lm(mean_price_per_m2 ~ cbd_distance + uni_distance + 
             mall_distance + train_distance + park_distance + hospital_distance + bus_distance + remaining_lease_final + mean_area, data = house2019)
ols2020 <-lm(mean_price_per_m2 ~ cbd_distance + uni_distance + 
             mall_distance + train_distance + park_distance + hospital_distance + bus_distance + remaining_lease_final + mean_area, data = house2020)
ols2021 <-lm(mean_price_per_m2 ~ cbd_distance + uni_distance + 
             mall_distance + train_distance + park_distance + hospital_distance + bus_distance + remaining_lease_final + mean_area, data = house2021)


coef_lmbeta <- lm.beta(ols2014)
coef_lmbeta

summary(ols2016)

stargazer(ols2014,ols2015,ols2016,ols2017,ols2018,ols2019,ols2020,ols2021, type = "text")



##### experiment
house.subset <- house2014[1:100,]
summary(lm(mean_price_per_m2 ~ cbd_distance + uni_distance + mall_distance + train_distance + 
             park_distance + hospital_distance + bus_distance + remaining_lease_final, data = house.subset))

house.subset@data

coordinates(house.subset) <- ~ lon + lat
GWRbandwidth <- gwr.sel(mean_price_per_m2 ~ cbd_distance + uni_distance + mall_distance + train_distance + park_distance + hospital_distance + bus_distance +
                              remaining_lease_final, data = house.subset, adapt=T)

gwr <- gwr(mean_price_per_m2 ~ cbd_distance + uni_distance + mall_distance + train_distance + park_distance + hospital_distance + bus_distance +
      remaining_lease_final, data=house.subset, adapt= GWRbandwidth, hatmatrix=TRU3E, se.fit=F) 

gwr


