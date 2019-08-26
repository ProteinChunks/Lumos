### Preprocessing the ETC data
## Preprocessing the park data
#1. Classified by the distance of bike roads and parks.
#-----------------------------------
# Load parks of Seoul coordinates data.
park1 <- read.csv("서울시 주요 공원현황.csv")
park1$지역 <- as.character(park1$지역)
park1_loc <- park1 %>% select(k2, 공원명, 면적, 지역, 경도=X좌표.WGS84., 위도=Y좌표.WGS84.)
park1_loc<-as.data.frame(park1_loc)

#3. Load bike road coordinates data.
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

#----------------------------------------------------------------------
# 2. Make a population density file: floating population density, resident population density, working population density.


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
                               density = (X181Q유동인구 + X182Q유동인구 + X183Q유동인구 + X184Q유동인구) / 4)

# Resident population density
resident_pop_density <- kind_of_population[, c(1, 6, 7, 8, 9)]
resident_pop_density[, c(2:5)] <- resident_pop_density[, c(2:5)] * 100     #Convert the unit of fraction: hectare(h) -> square kilometer(km^2)
resident_pop_density <- mutate(resident_pop_density, 
                               density = (X181Q주거인구 + X182Q주거인구 + X183Q주거인구 + X184Q주거인구) / 4)

# Working population density
working_pop_density <- kind_of_population[, c(1, 10, 11, 12, 13)]
working_pop_density[, c(2:5)] <- working_pop_density[, c(2:5)] * 100     #Convert the unit of fraction: hectare(h) -> square kilometer(km^2)
working_pop_density <- mutate(working_pop_density, 
                              density = (X181Q직장인구 + X182Q직장인구 + X183Q직장인구 + X184Q직장인구) / 4)

# Merge 3 kinds of population density into one file
population <- cbind(all_pop_seoul, working_pop_density[, 6], floating_pop_density[, 6], resident_pop_density[, 6])

colnames(population)[6] <- c("working_pop_density_km^2")   #Revising column names.
colnames(population)[7] <- c("floating_pop_density_km^2")
colnames(population)[8] <- c("resident_pop_density_km^2")
rownames(population) <- c(1:25)                            #Revising row numbers.

population <- arrange(population, location)      #Sorting rows by 'location' column.

# Make an integrated csv file: population_density.csv
write.csv(population, "population_density.csv")


# 'population_density.csv''s column information: 
#   location: District of Seoul city
#   population: The number of local people + foreigner
#   area: Area of the district(km^2)
#   population_density_km^2: The population density of the district(persons/km^2)
#   working_pop_density_km^2: The working population density of the district(persons/km^2)
#   floating_pop_density_km^2: The floating population density of the district(persons/km^2)
#   resident_pop_density_km^2: The resident population density of the district(persons/km^2)