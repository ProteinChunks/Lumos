################### visualize using rank system
library(ggmap)
library(rgdal)
seoul_visual<-read.csv("seoul_for_visualize.csv")
training<-read.csv("kmeans_train.csv")
testing<-read.csv("kmeans_test.csv")

district<-rbind(training,testing)
district<-cbind(seoul_visual[,c(2:3)],district) # insert coordinates in kemans data frame

# rank system
rank_df<-data.frame(district$loc)
for(i in 2:ncol(training)){
  rank_df<-cbind(rank_df,rank(district[,i]))
}
final_rank<-data.frame(loc = district$loc, rank_sum = rowSums(rank_df[-1]))
final_rank<-cbind(final_rank,f_rank = rank(final_rank[2]))
final_rank<-final_rank[c(order(-final_rank$f_rank)),] # order by final rank
rownames(final_rank)<-NULL

# visualize
library(zoo)
library(ggplot2)
shp<-readOGR("seoul_municipalities.shp")
bikeroad40000 <- read.csv("bike_loc_gu.csv", header=T, sep=',')

# transform coordinates
from_crs = CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel +units=m")
to_crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
proj4string(shp)<-from_crs
shp<-spTransform(shp,to_crs)

seoul_df<-fortify(shp)

ggplot(seoul_df,aes(x=long, y=lat))+geom_point()+
  geom_point(data=bikeroad40000,aes(x=long,y=lat), size=0.5, alpha = 0.05) +
  geom_point(data=district, aes(x=long, y=lat, color=cluster), size = rank(final_rank[2]), alpha = 0.5) +
  ggtitle("최적 입지") + theme(plot.title = element_text(face = "bold", size = 20, vjust = 1))
