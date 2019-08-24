library(rgdal)
library(sp)
library(sf)
library(raster)
library(ggplot2)
library(gstat)
library(automap)
shp<-readOGR("seoul_municipalities.shp")
plot(shp)

# make grid using sf
shp_sf <- st_as_sf(shp)
ggplot() +
  geom_sf(data = shp_sf)
grid <- shp_sf %>% 
  st_make_grid(cellsize = c(200,200), what = "centers") %>% # grid of points
  st_intersection(shp_sf)# only within the polygon
ggplot() +
  geom_sf(data = shp_sf) + 
  geom_sf(data = grid)

# make grid using rgdal
plot(shp)
cs <- c(3.28084, 3.28084)*6000
grdpts <- makegrid(shp, cellsize = c(250,250))
spgrd <- SpatialPoints(grdpts, proj4string = CRS(proj4string(shp)))
spgrdWithin <- SpatialPixels(spgrd[shp,])
plot(spgrdWithin, add = T)
spgrdWithin <- as(spgrdWithin, "SpatialGrid")

# chage type of grid
library(tidyverse)
seal_coords <- do.call(rbind, st_geometry(grid)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
coordinates(seal_coords)<- ~lon+lat

# change CRS(coordinates) of grid and other files 
bike_loc<-read.csv("bike_half.csv")
seoul_center<-read.csv("서울_구_중심.csv")
observatory_df<-read.csv("관측소좌표_좌표추출.csv")
all_df_sc<-read.csv("all_df_all_sc(완).csv")
seoul_df<-fortify(shp)[1:2]
  
observatory_df<-observatory_df[c(order(observatory_df$loc)),]

seoul_center<-cbind(observatory_df,all_df_sc[,-1])
observatory_df
observatory_df<-observatory_df[-4]

proj4string(seal_coords)<-CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel +units=m")
d<-spTransform(seal_coords, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
coordinates(seoul_center) <- ~ long + lat
coordinates(seoul_df) <- ~long + lat

proj4string(d)<- CRS("+init=epsg:28992")
proj4string(seoul_df) <- CRS("+init=epsg:28992")
proj4string(seoul_center) <- CRS("+init=epsg:28992")

coordinates(bike_loc) <- ~long+lat
proj4string(bike_loc) <- CRS("+init=epsg:28992")

d # gird coordinates
bike_loc # bike road coordinates
seoul_df # Seoul full coordinates 
seoul_center # coordinates of districts of Seoul

plot(d)# grid plot

####### use IDW interpolation and visualization
# 1. use IDW for bike road coordinates
idw_bike<-idw(formula=avghum_10_1~1, locations=seoul_center, newdata=bike_loc,
          nmax = Inf, nmin = 0, omax = 0, maxdist = Inf,
          na.action = na.pass, idp = 3, debug.level = 1)

# 2. use IDW for the entire Seoul grid (grid).
idw_grid<-idw(formula=avghum_10_1~1, locations=seoul_center, newdata=d,
          nmax = Inf, nmin = 0, omax = 0, maxdist = Inf,
          na.action = na.pass, idp = 3, debug.level = 1)

spplot(idw_bike,"var1.pred")
spplot(idw_grid,"var1.pred")

# check observatory location
bike_location<-read.csv("bike_half.csv")
plot(bike_location$long,bike_location$lat,col="gray",cex=0.5)
points(observatory_df$long,observatory_df$lat,col="red",cex=3)

######## create DataFrame for use with Kmeans
all_df_all<-read.csv("all_df_all.csv")
seoul_center_bike<-cbind(observatory_df,all_df_all[,-1])
all_df_bike<-data.frame()
idw_bike_list<-list()

coordinates(seoul_center_bike) <- ~ long + lat
proj4string(seoul_center_bike) <- CRS("+init=epsg:28992")

# 1. use IDW for bike road coordinates -> list
idw_bike_list[[1]]<-idw(formula=avgtmp_10_1~1, locations=seoul_center_bike, newdata=bike_loc,
                    nmax = Inf, nmin = 0, omax = 0, maxdist = Inf,
                    na.action = na.pass, idp = 25, debug.level = 1)

spplot(idw_bike_list[[1]],"var1.pred")
View(all_df_all[,2])

# 2. use IDW for the entire Seoul grid (grid) -> list
for (i in 1:543){
  print(i)
  c=i+1
  idw_bike_list[[i]]<-idw(formula=all_df_all[,c]~1, locations=seoul_center_bike, newdata=bike_loc,
                          nmax = Inf, nmin = 0, omax = 0, maxdist = Inf,
                          na.action = na.pass, idp = 3, debug.level = 1)
} 
View(idw_bike_list[[1]])

### parts of colum names
name = c("avgtmp","rain","avghum","pm10","solar")
name[1]

all_df_bike<-cbind(long=bike_loc$long, lat=bike_loc$lat)
all_df_bike<-cbind(all_df_bike, avgtmp_10_1 = idw_bike_list[[1]]$var1.pred)
all_df_bike

all_df_all<-all_df_all[-c(542,543,544)]
length(all_df_all)
for(i in 1:541){
  print(i)
  all_df_bike<-cbind(all_df_bike, idw_bike_list[[i]]$var1.pred)
}
all_df_bike
View(all_df_all[544])
View(idw_bike_list[[543]]$var1.pred)
count=3
for(i in 10:18){
  for(j in 1:12){
    for(k in 1:5){
      print(count) # check progress
      colnames(all_df_bike)[count]<-paste0(name[k],"_",i,"_",j)
      count=count+1
    }
  }
}
all_df_bike_fin<-all_df_bike[,-543]
#write.csv(all_df_bike_fin,"IDW_bike_location.csv",row.names = F)
