### Preprocessing the ETC data
library(sf)
library(raster)
library(ggplot2)
library(reprex)
library(tidyverse)
library(rgdal)
library(dplyr)
library(doBy)

## 1.Preprocess bike road coordinates
#-----------------------------------
bike<-readOGR("bcycl_track_W.shp") # Load Seoul bike road shp file
plot(bike)
bike_location<-fortify(bike) # Convert to Data frame

# Add address in bike_location data frame using Geocoder-Xr 2019 v3.5
# ---------------------------------------------------------------------------
bike_location<-read.csv("bike_location.shp.csv") # Bike road coordinates with address

# extract part of address
bike_location_gu<-data.frame(long=bike_location$long, lat=bike_location$lat, gu=substr(bike_location$?„ë¡œëª…ì£¼ì†Œ,1,9))

# Except "ê²½ê¸°?„"
bike_location<-bike_location%>%filter(substr(?„ë¡œëª…ì£¼ì†Œ,1,3)!="ê²½ê¸°?„")

# extract district name
bike_location_gu<-data.frame(long=bike_location$long, lat=bike_location$lat, gu=substr(bike_location$?„ë¡œëª…ì£¼ì†Œ,7,9))
col_gu<-colors()[50:90]
# visualize bike road by district
plot(x=bike_location_gu$long,y=bike_location_gu$lat, col=col_gu[bike_location_gu$gu], cex=0.5)

# Sampling bike road coordinates for data minimize.
# --------------------------------------------------
# 41094 -> 20000 Sampling
idx<-sample(nrow(bike_location),20000) 
bike_loc_0.5<-bike_location[idx,]
plot(bike_loc_0.5$long, bike_loc_0.5$lat) # too many coordinates

# 41094 -> 10000 Sampling
idx<-sample(nrow(bike_loc_gu), 10000)
bike10000<-bike_loc_gu[idx,]
plot(bike10000$long, bike10000$lat)
plot(bike_half$long, bike_half$lat) # too many loss

# More sampling using 20000 sampling data
bike_loc_0.5<-ifelse(row(bike_location)%%5==0,c(bike_location$long,bike_location$lat),NA)
bike_loc_0.5<-na.omit(bike_loc_0.5)
df<-data.frame()
df<-rbind(bike_loc_0.5[,c(1,2)],bike_loc_0.5[,c(3,4)])
df<-rbind(df,bike_loc_0.5[,c(5,6)])
colnames(df)<-c("long","lat")
df<-as.data.frame(df)
plot(df$long,df$lat) # high loss, too

# Sample by doby library (extraction by id or group) 
train_gu <- sampleBy(~ gu, frac = 0.5, replace = FALSE, data = bike_loc_gu) # using district
train_id <- sampleBy(~ id, frac = 0.5, systematic = TRUE, data = bike_location) # using shp polygon id
train_gr <- sampleBy(~ group, frac = 0.5, systematic = TRUE, data = bike_location) # using shp plygon group

plot(train_gu$long,train_gu$lat)
plot(train_id$long,train_id$lat) # The most even distribution is shown.
plot(train_gr$long,train_gr$lat)

#write.csv(train_id,"bike_half.csv",row.names = F)

# Bike road width and area data extract
# ---------------------------------------------------------------
bike_width_area<-read.csv("bike_road_width_area.csv")
bike_wa<-bike_width_area[2:4]
bike_wa_g<-bike_wa%>%group_by(?ì¹˜êµ¬ëª?)%>%summarise(length=sum(?„ë¡œê¸¸?´),width=mean(?„ë¡œí­))
#write.csv(bike_wa_g,"êµ¬ë³„_?? „ê±°ë„ë¡?_ê¸¸ì´_?­.csv",row.names=F)


## 2.Classified by the distance of bike roads and parks.
#-----------------------------------
# Load parks of Seoul coordinates data.
park1 <- read.csv("?„œ?š¸?‹œ ì£¼ìš” ê³µì›?˜„?™©.csv")
park1$ì§€?—­ <- as.character(park1$ì§€?—­)
park1_loc <- park1 %>% select(k2, ê³µì›ëª?, ë©´ì , ì§€?—­, ê²½ë„=Xì¢Œí‘œ.WGS84., ?œ„?„=Yì¢Œí‘œ.WGS84.)
park1_loc<-as.data.frame(park1_loc)

# Load bike road coordinates data.
library(dplyr)
bike<-read.csv("bike_location.csv")
bike_loc<-bike%>%select(long,lat,id)

# Clustering bike road coordinates by 25.
km<-kmeans(bike_loc[-3],25,iter.max = 10000)

# Extract centers
bi_cen <- data.frame(km$centers)

# Create bike road coordinate matrix.
bi_all<-matrix(ncol=2,nrow=0,byrow=T)
for(i in 1:nrow(bi_cen)){
  bi_all<-rbind(bi_all,c(bi_cen[i,1],bi_cen[i,2]))
}

# Create parks coordinate matrix.
pk_all<-matrix(ncol=2,nrow=0,byrow=T)
for(i in 1:nrow(park1_loc)){
  pk_all<-rbind(pk_all,c(park1_loc[i,5],park1_loc[i,6]))
}
pk_all<-pk_all[1:131,]

# Distance calculation and matrix creation of distances and coordinates.
d<-matrix(ncol=5,nrow=0,byrow=T)
for(i in 1:nrow(bi_all)){
  for(j in 1:nrow(pk_all)){
    d<-rbind(d,c(dist(matrix(c(bi_all[i,],pk_all[j,]),nrow=2,ncol=2,byrow=T),method="euclidean"),bi_all[i,1],bi_all[i,2],pk_all[j,1],pk_all[j,2]))
  }
}

# Convert to data frame.
dis_df<-data.frame(distance=d[,1],bi_long=d[,2],bi_lat=d[,3],pk_long=d[,4],pk_lat=d[,5])

# Extract distances for the first coordinate of the bike road.
dis_df_1<-dis_df[1:131,]

# Sort by distance.
dis_df_1<-dis_df_1[c(order(dis_df_1$distance)),]

# Bike road coordinates: red, 10 nearby park coordinates: black, Other park coordinates: green
plot(x=dis_df_1$pk_long[11:nrow(dis_df_1)],y=dis_df_1$pk_lat[11:nrow(dis_df_1)],col="green")
points(x=dis_df_1$pk_long[1:10], y=dis_df_1$pk_lat[1:10])
points(x=dis_df_1$bi_long[1],y=dis_df_1$bi_lat[1],col="red")

#----------------------------------------------------------------------

# Store all bi_cens through a list.
dis_list<-list()
c=1
for(i in 1:nrow(bi_cen)){
  d=c+130
  dis_list[[i]]<-dis_df[c:d,]
  c=c+131
}
dis_list[[1]]

# Sort by distance all
for(i in 1:nrow(bi_cen)){
  dis_list[[i]]<-dis_list[[i]][c(order(dis_list[[i]]$distance)),]
}

# Visualization
dis_list[[1]]$pk_long[1:5]
plot(map)
plot(x=dis_df$pk_long,y=dis_df$pk_lat,col="chocolate4")
for(i in 1:nrow(bi_cen)){
  points(x=dis_list[[i]]$pk_long[1:5], y=dis_list[[i]]$pk_lat[1:5],col=i,pch=i)
}
points(x=dis_df$bi_long,y=dis_df$bi_lat,col="red",cex=1.2,pch=19)


## 3. Make a population density file: floating population density, resident population density, working population density.
#----------------------------------------------------------------------

# Load popuatlion density data of Seoul(2018).
all_pop_seoul <- read.csv("./datas/seoul_density_2018(km2).csv", header=T, sep = ',')

colnames(all_pop_seoul) <- c('year', 'location', 'population', 'area(km^2)', 'population_density_km^2')
all_pop_seoul <- all_pop_seoul[-1, ]     #Delete an unnecessary row(total).


# Load the number of floating population, resident population, working population datas.
# And spearate them respectively.
library(dplyr)
kind_of_population <- read.csv("./datas/float,resident,working_population_2018(ha).csv", header=T, sep=',')
kind_of_population <- kind_of_population[-1, ]     #Delete an unnecessary row(total).

# Floating population density
floating_pop_density <- kind_of_population[, c(1, 2, 3, 4, 5)]
floating_pop_density[, c(2:5)] <- floating_pop_density[, c(2:5)] * 100     #Convert the unit of fraction: hectare(h) -> square kilometer(km^2)
floating_pop_density <- mutate(floating_pop_density, 
                               density = (X181Q?œ ?™?¸êµ? + X182Q?œ ?™?¸êµ? + X183Q?œ ?™?¸êµ? + X184Q?œ ?™?¸êµ?) / 4)

# Resident population density
resident_pop_density <- kind_of_population[, c(1, 6, 7, 8, 9)]
resident_pop_density[, c(2:5)] <- resident_pop_density[, c(2:5)] * 100     #Convert the unit of fraction: hectare(h) -> square kilometer(km^2)
resident_pop_density <- mutate(resident_pop_density, 
                               density = (X181Qì£¼ê±°?¸êµ? + X182Qì£¼ê±°?¸êµ? + X183Qì£¼ê±°?¸êµ? + X184Qì£¼ê±°?¸êµ?) / 4)

# Working population density
working_pop_density <- kind_of_population[, c(1, 10, 11, 12, 13)]
working_pop_density[, c(2:5)] <- working_pop_density[, c(2:5)] * 100     #Convert the unit of fraction: hectare(h) -> square kilometer(km^2)
working_pop_density <- mutate(working_pop_density, 
                              density = (X181Qì§ì¥?¸êµ? + X182Qì§ì¥?¸êµ? + X183Qì§ì¥?¸êµ? + X184Qì§ì¥?¸êµ?) / 4)

# Merge 3 kinds of population density into one file
population <- cbind(all_pop_seoul, working_pop_density[, 6], floating_pop_density[, 6], resident_pop_density[, 6])

colnames(population)[6] <- c("working_pop_density_km^2")   #Revising column names.
colnames(population)[7] <- c("floating_pop_density_km^2")
colnames(population)[8] <- c("resident_pop_density_km^2")
rownames(population) <- c(1:25)                            #Revising row numbers.

population <- arrange(population, location)      #Sorting rows by 'location' column.

# 'population_density.csv''s column information: 
#   location: District of Seoul city
#   population: The number of local people + foreigner
#   area: Area of the district(km^2)
#   population_density_km^2: The population density of the district(persons/km^2)
#   working_pop_density_km^2: The working population density of the district(persons/km^2)
#   floating_pop_density_km^2: The floating population density of the district(persons/km^2)
#   resident_pop_density_km^2: The resident population density of the district(persons/km^2)
# Make an integrated csv file: population_density.csv
write.csv(population, "population_density.csv")


## 4. Classified the security light data
#-----------------------------------
# Extract the security light around the bike road
# Load the coordinate of security light.csv and coordindate of buildings in Seoul
light<-read.csv("security_light.csv")
gu<-read.csv("seoul_city_centre_day.csv")
gu<-gu[c(order(gu$loc)),]
gu

# Reverse the Security light data
light<-cbind(light,loc = gu$loc)
func<-function(x){
  x = 19344 - x  # maxinum number of security light - current security light in each Seoul district
  return(x)
}
light$x<-lapply(light$x, function(x) { x = 19344 - x; return(x) })
colnames(light)<-c("loc","count")
light<-as.data.frame(light)

#write.csv(light,"security_light_reverse.csv",row.names = F)


## 5. Classified the building data
#-----------------------------------
read.csv("bh_sp_2.csv")
bike_half_compl<-read.csv("bike_half_compl.csv")
bh_loc<-read.csv("bh_loc_2.csv")

## Split coordindate of buildings by Seoul district
bh_loc$gu<-as.character(bh_loc$gu)
bh_loc_spl<-split(bh_loc,bh_loc$gu)

## Split coordindate of bike roads by Seoul district
bh_sp_2<-data.frame()
bike_half_list<-split(bike_half_compl,bike_half_compl$ì§€ë²ˆì£¼?†Œ)

# Make the list for coordindate of buildings 
nrow(bh_loc_spl[[1]])
height_list <- list(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

# Get coordindate of buildings where near the bicycle road
colnames(height)<-c("long","lat","height")
for(k in 1:25){
  for(i in 1:nrow(bh_loc_spl[[k]])){
    print(i)
    for(j in 1:nrow(bike_half_list[[k]])){
      if(((bh_loc_spl[[k]]$long[i] > bike_half_list[[k]]$long[j]*24999/25000) & 
          (bh_loc_spl[[k]]$long[i] < bike_half_list[[k]]$long[j]*25001/25000)) & 
         ((bh_loc_spl[[k]]$lat[i] > bike_half_list[[k]]$lat[j]*24999/25000) & 
          (bh_loc_spl[[k]]$lat[i] < bike_half_list[[k]]$lat[j]*25001/25000)))
      {
        df<-data.frame(long=bh_loc_spl[[k]]$long[i],lat=bh_loc_spl[[k]]$lat[i],height=bh_loc_spl[[k]]$height[i])
        height_list[[k]] <- rbind(height_list[[k]],df)
        break
      }
    }
  }
}

# Remove the temporary data '1'
hl<-height_list
for(i in 1:25){
  hl[[i]]<-hl[[i]][-1,]
}

# Add the Seoul district as number in the coordinate of buildings and make it as dataframe
hl_df<-list.cbind(hl)
for(i in 1:25){
  hl[[i]]$id<-i
}
hl_df<-list.rbind(hl)
#write.csv(hl_df,"hl_df.csv",row.names = F)


## 6. Classified the bus station and subway station data
#-----------------------------------
library(dplyr)
### Extract sus stations and subway stations near bicycle roads
## Load bicycle road locations, bus stations and subway stations
bhc <- read.csv("bike_half_compl.csv")
bus <- read.csv("bus3_2.csv")
sub <- read.csv("sub3_2.csv")

## Split by Seoul districts
bhc_list  <-split(bhc, bhc$Áö¹øÁÖ¼Ò)
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