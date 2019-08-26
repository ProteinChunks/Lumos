# Classified by the distance of bike roads and parks.
#-----------------------------------
# Load parks of Seoul coordinates data.
park1 <- read.csv("서울시 주요 공원현황.csv")
park1$지역 <- as.character(park1$지역)
park1_loc <- park1 %>% select(k2, 공원명, 면적, 지역, 경도=X좌표.WGS84., 위도=Y좌표.WGS84.)
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

# Visualizatino
dis_list[[1]]$pk_long[1:5]
plot(map)
plot(x=dis_df$pk_long,y=dis_df$pk_lat,col="chocolate4")
for(i in 1:nrow(bi_cen)){
  points(x=dis_list[[i]]$pk_long[1:5], y=dis_list[[i]]$pk_lat[1:5],col=i,pch=i)
}
points(x=dis_df$bi_long,y=dis_df$bi_lat,col="red",cex=1.2,pch=19)