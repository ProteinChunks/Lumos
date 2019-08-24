################### load population density, light, park, bus, subway data
pop<-read.csv("population_density.csv")
light_park<-read.csv("light_park.csv")
bike_half_compl<-read.csv("bike_half_compl.csv")
sub<-read.csv("sub_bhc_df_num.csv")
bus<-read.csv("bus_bhc_df_num.csv")

################### preprocessing for clustering
pop_df<-data.frame()
for(i in 1:25){
  pop_df<-rbind(pop_df,woking_pop=pop$area.km.2.[i] * pop$working_pop_density_km.2[i])
}
colnames(pop_df)<-"working_pop"
pop_df2<-data.frame()
for(i in 1:25){
  pop_df2<-rbind(pop_df2,pop$area.km.2.[i] * pop$floating_pop_density_km.2[i])
}
colnames(pop_df2)<-"floating_pop"
pop_df3<-data.frame()
for(i in 1:25){
  pop_df3<-rbind(pop_df3,woking_pop=pop$area.km.2.[i] * pop$resident_pop_density_km.2[i])
}
colnames(pop_df3)<-"resident_pop"

final_df<-cbind(pop_df,pop_df2)
final_df<-cbind(final_df,pop_df3)

final_df<-cbind(loc=names(table(bike_half_compl$지번주소)),final_df)
final_df<-cbind(final_df,light_park)

sub<-sub[3]
bus<-bus[3]
final_df<-cbind(final_df,sub=sub)
final_df<-cbind(final_df,bus=bus)
colnames(final_df)[7]<-"sub"
colnames(final_df)[8]<-"bus"
gen<-read.csv("kw_gu_total_2.csv")
gen<-gen[,c(-1,-2)]
final_df<-cbind(final_df,gen)
df<-final_df

################### k number determination for hclust and kmeans
library(cluster)
df_sc <- scale(df[-1])
plot(silhouette(cutree(hc,k=10),dist=dist(df_sc)),col=1:10) # 7 is optimal k number

################### hierarchical clustering - hclust
hc <- hclust(dist(final_df[-1]), method="ave")
plot(hc, hang=-1, labels=final_df$loc)
rect.hclust(hc, k=7)
groups <- cutree(hc, k=7)           

################### kmeans
library(caret)
set.seed(1712)

# train/test set
inTrain <- sample(nrow(df),nrow(df)*0.10)
training <- df[-inTrain,]
testing <- df[inTrain,]
training

# normalize
training.data <- scale(training[-1])
summary(training.data)

# kmeans
km <- kmeans(training.data, centers = 7, iter.max = 10000)
training$cluster <- as.factor(km$cluster)

# create model
training.data <- as.data.frame(training.data)
modFit <- train(x = training.data, 
                y = training$cluster,
                method = "rpart")

# classfication test data
testing.data <- as.data.frame(scale(testing[-1]))
testClusterPred <- predict(modFit, testing.data) 
table(testClusterPred)
testing$cluster<-testClusterPred

# save kmeans result
write.csv(training,"kmeans_train.csv",row.names = F)
write.csv(testing,"kmeans_test.csv",row.names = F)