library(dplyr)
### Extract sus stations and subway stations near bicycle roads
## Load bicycle road locations, bus stations and subway stations
bhc <- read.csv("bike_half_compl.csv")
bus <- read.csv("bus3_2.csv")
sub <- read.csv("sub3_2.csv")

## Split by Seoul districts
bhc_list  <-split(bhc, bhc$지번주소)
bus_list <- split(bus, bus$addr)
sub_list <- split(sub, sub$addr)

## Make list which is bus stations and subway stations near bicycle road
bus_bhc_list <- list(1:25)
sub_bhc_list <- list(1:25)

## Preprocessing the bus statons data
# Get the bus stations where near the bicycle road
for(k in 1:25){
  for(i in 1:nrow(bus_list[[k]])){
    print(i)
    for(j in 1:nrow(bhc_list[[k]])){
      if(((bus_list[[k]]$bus3.long[i] > bhc_list[[k]]$long[j]*999999/1000000) & 
          (bus_list[[k]]$bus3.long[i] < bhc_list[[k]]$long[j]*100001/100000)) & 
         ((bus_list[[k]]$bus3.lat[i] > bhc_list[[k]]$lat[j]*999999/1000000) & 
          (bus_list[[k]]$bus3.lat[i] < bhc_list[[k]]$lat[j]*1000001/1000000)))
      {
        df<-data.frame(long=bus_list[[k]]$bus3.long[i],lat=bus_list[[k]]$bus3.lat[i], loc=bus_list[[k]]$addr)
        bus_bhc_list[[k]] <- rbind(bus_bhc_list[[k]],df)
        break
      }
    }
  }
}

# Remove the duplicate data
for(i in 1:25){bus_bhc_list[[i]] <- unique(bus_bhc_list[[i]])}

# Remove the null data
for(i in 1:25){bus_bhc_list[[i]]<-na.omit(bus_bhc_list[[i]])}

# Check whether the bus stations which were gathered are near the bicycle road or not
plot(bus_bhc_list[[1]]$long, bus_bhc_list[[1]]$lat)
points(bus_list[[1]]$bus3.long, bus_list[[1]]$bus3.lat, col = "red")
bus_bhc_df <- rbind(bus_bhc_list[[1]], bus_bhc_list[[2]])
for(i in 3:25){
  bus_bhc_df <- rbind(bus_bhc_df, bus_bhc_list[[i]])
}
plot(x = bus_bhc_list[[1]]$long, y= bus_bhc_list[[1]]$lat)

## Preprocessing the subway statons data
# Get the subway stations where near the bicycle road
for(k in 1:25){
  for(i in 1:nrow(sub_list[[k]])){
    print(i)
    for(j in 1:nrow(bhc_list[[k]])){
      if(((sub_list[[k]]$long[i] > bhc_list[[k]]$long[j]*999999/1000000) & 
          (sub_list[[k]]$long[i] < bhc_list[[k]]$long[j]*1000001/1000000)) & 
         ((sub_list[[k]]$lat[i] > bhc_list[[k]]$lat[j]*999999/1000000) & 
          (sub_list[[k]]$lat[i] < bhc_list[[k]]$lat[j]*1000001/1000000)))
      {
        df<-data.frame(long=sub_list[[k]]$long[i], lat=sub_list[[k]]$lat[i], loc=sub_list[[k]]$addr)
        sub_bhc_list[[k]] <- rbind(sub_bhc_list[[k]],df)
        break
      }
    }
  }
}

# Remove the duplicate data
for(i in 1:25){sub_bhc_list[[i]] <- unique(sub_bhc_list[[i]])}

# Remove the null data
for(i in 1:25){sub_bhc_list[[i]]<-na.omit(sub_bhc_list[[i]])}


# Check whether the subway stations which were gathered are near the bicycle road or not
plot(sub_bhc_list[[10]]$long, sub_bhc_list[[10]]$lat)
points(sub_list[[10]]$long, sub_list[[10]]$lat, col = "red")
sub_bhc_df <- rbind(sub_bhc_list[[1]], sub_bhc_list[[2]])
for(i in 3:25){
  sub_bhc_df <- rbind(sub_bhc_df, sub_bhc_list[[i]])
}
plot(x = sub_list[[1]]$long, y = sub_list[[1]]$lat)
points(x= bhc_list[[1]]$long, y= bhc_list[[1]]$lat, col="red")
sub_bhc_list[[1]]<-na.omit(sub_bhc_list[[1]])
plot(x = sub_bhc_list[[1]]$long, y= sub_bhc_list[[1]]$lat)

# Classify the data with Seoul districts
sub_bhc_df_num <- data.frame(table(sub_bhc_df$loc))
bus_bhc_df_num <- data.frame(table(bus_bhc_df$loc))
