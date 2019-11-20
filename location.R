options(stringsAsFactors=FALSE)
#clean station data
stations <- read.csv(file="stops.txt", head=TRUE,sep=",")
stations = stations %>%
  select(stop_id, stop_name, stop_lat, stop_lon)
stations = stations[!is.na(as.numeric(stations$stop_id)), ]
stations$stop_id = as.character(stations$stop_id)
stations$stop_id = as.numeric(stations$stop_id)
stations = stations %>% na.omit()
#select out subway stations
station = stations[which ((stations$stop_id >= 70000) & (stations$stop_id <= 70279) ),]
colnames(station)[3] <- "latitude"
colnames(station)[4] <- "longitude"

#Function for calculate euclidean distance
dist.eu <- function(lat1,long1,lat2,long2) {
  R = 6378 # Earth radius in km
  c = sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2)*cos(long1-long2)
  d = R*acos(c)*pi/180
  return(d) 
}
#Use previous function to calculate manhattan distance
dist.ma <-function(lat1,long1,lat2,long2){
  v_dist = dist.eu(lat1,long1,lat1,long2)
  h_dist = dist.eu(lat1,long2,lat2,long2)
  v_dist+h_dist
}
#Find the distance to nearest station
near_station = numeric()
for (i in 1:nrow(listings)) {
  temp = vector()
  for (j in 1:nrow(station)) {
    temp[j] = dist.ma(listings[i,'latitude'], listings[i,'longitude'], station[j,'latitude'], station[j,'longitude'])
  } 
  near_station[i] = min(unlist(temp))
}
