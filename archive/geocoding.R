library(ggmap)
register_google(key = "*****")

# df1 and df2 include list of cities that hosted competitions
dfnew <- rbind(df1, df2)
cities <- dfnew %>% distinct(City) %>% t() %>% as.character()

locations <- geocode(cities)

cityCoord <- cbind.data.frame(cities, locations, stringsAsFactors = F)
cityCoord[which(cityCoord$cities == "El Djezair"), 2:3] <- geocode("Algiers")
cityCoordFinal <- cityCoord[-which(cityCoord$cities == ""), ]
colnames(cityCoordFinal)[1] <- "City"

write.csv(cityCoordFinal, "data/city_coord.csv", row.names = F, fileEncoding = "UTF-8")