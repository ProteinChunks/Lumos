library(dplyr)
library(ggplot2)
library(rlist)

### Extract buildings around bicycle road to calculate shaded bike roads
## Load the coordinate of bike roads and coordindate of buildings in Seoul
bh_sp<-read.csv("bh_sp_2.csv")
bike_half_compl<-read.csv("bike_half_compl.csv")
bh_loc<-read.csv("bh_loc_2.csv")

## Split coordindate of buildings by Seoul district
bh_loc$gu<-as.character(bh_loc$gu)
bh_loc_spl<-split(bh_loc,bh_loc$gu)

## Split coordindate of bike roads by Seoul district
bh_sp_2<-data.frame()
bike_half_list<-split(bike_half_compl,bike_half_compl$지번주소)

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
