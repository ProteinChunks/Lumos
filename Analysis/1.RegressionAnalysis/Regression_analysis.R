library(psych)
library(cran)
library(dplyr)
library(plyr)
library(gtools)
library(PerformanceAnalytics)
library(car)
library(sp)
library(sf)
library(ggplot2)

### Regression analysis(Insolation data and Other weather data)
## Data Preprocessing
# Get the Whole scaled data of weather of each Seoul district
write.csv(all_df_all_na_2, "all_df_all_na_2.csv")
all_df_all_na <- all_df_all
all_df_all_na<-all_df_all_na[-1]
all_df_all_na_2 <- all_df_all_na
colnames(all_df_all_na_2)<-NA

all_df<-rbind(all_df_all_na_2[1:5],all_df_all_na_2[6:10])
head(all_df_all)

for(i in seq(11,(length(all_df_all_na_2)),5)){
  print(i)
  j=i+4
  all_df<-rbind(all_df,all_df_all_na_2[i:j])
}
head(all_df)

str(all_df_all_2)

all_df_all_2 <- all_df_all
all_df_all_2$loc <- as.character(all_df_all_2$loc_10_1)

all_df <- cbind(all_df,rep(c(all_df_all_2$loc),108))

colnames(all_df)<-c("temp","rain", "humid","pm10", "solar","loc")

head(all_df)

all_df_sort<-all_df[c(order(all_df$loc)),]
head(all_df_sort)

table(all_df$loc)

## Regression analysis of Seoul district
# Split to train weather data
trainweather <- data.frame()
trainweather <- all_df[1:2100,]
colnames(trainweather) <- c("temp","rain", "humid","pm10", "solar")

# Split to test weather data
testweather <- data.frame()
testweather <- all_df[2101:2700,]
colnames(testweather) <- c("temp","rain", "humid","pm10", "solar")

# Correlation of train weather data
pairs.panels(trainweather[-6])
chart.Correlation(trainweather[-6], histogram = T, pch = 10)
trainmodel <- lm(formula = solar ~ temp + humid + rain + pm10, data = trainweather)
summary(trainmodel)
summary(trainmodel)$coefficients # Insolation is effected by all other weather data as less than 0.5 in p-value

# Train weather data Multicollinearity
vif(trainmodel)
#     temp    humid     rain     pm10 
#   1.734007 1.974120 1.825037 1.551452 
sqrt(vif(trainmodel))>2 # All data can be seen to False so it could be said there have no Multicollinearity
# temp humid  rain  pm10 
#FALSE FALSE FALSE FALSE 

# Prediction from train weather data
head(testweather)
pre_trainmodel <- predict(trainmodel, newdata = testweather[c(-5,-6)])
pre_trainmodel <- as.data.frame(pre_trainmodel)

# Print and Compare from ggplot
gg <- ggplot(pre_trainmodel, aes(x = index(pre_trainmodel), y= pre_trainmodel))+
  geom_line(color = "red") +
  geom_line(aes(x = index(pre_trainmodel), y = testweather$solar), color = "blue")
gg
plot(testweather$solar)

## Regression anaylsis in each Seoul district
# Put inside each Seoul district weather data in gu_list (108 sets)
gu_list<-list()
for(i in seq(1,2700,108)){
  print(i)
  j=(i-1)/108+1
  k=i+107
  gu_list[[j]]<-all_df_sort[c(i:k),]  
}
View(all_df_sort[c(1:108),])
View(gu_list[[1]])

# Put Seoul district weather data as consonant order in train_gu_list and test_gu_list (train: 78 sets, test: 30 sets) 
train_gu_list <- list()
test_gu_list <- list()

train_gu_list[[1]] <- gu_list[[1]][1:84,]
test_gu_list[[1]] <- gu_list[[1]][85:108,]

for(i in 2:25){
  train_gu_list[[i]] <- gu_list[[i]][1:84,]
  test_gu_list[[i]] <- gu_list[[i]][85:108,]
}

## Regression anaylsis Test
# Check the each Seoul district train data as graph
pairs.panels(train_gu_list[[1]][-6])
chart.Correlation(train_gu_list[[7]][-6], histogram = T, pch = 10)
head(train_gu_list[[1]])

# Put Regression values from training data in train_gu_model_list
# Put Regression value summaries from regression values in summary_train_gu_model_list
train_gu_model_list <- list()
summary_train_gu_model_list <- list()
for(i in 1:25){
  train_gu_model_list[[i]] <- lm(formula = solar ~ temp + humid + rain + pm10, data = train_gu_list[[i]])
  summary_train_gu_model_list[[i]] <- summary(train_gu_model_list[[i]])
}

# check the data is right in there or not
train_gu_model_list[[7]]
summary_train_gu_model_list[[7]]

# Check the Multicollinearity of train models and put these into list 
vif_tgml = list()
sqrt_vif_tgml = list()
for(i in 1:25){
  vif_tgml[[i]] <- vif(train_gu_model_list[[i]])
  sqrt_vif_tgml[[i]] <- sqrt(vif(train_gu_model_list[[i]]))>2 # All data can be seen to False so it could be said there have no Multicollinearity
}


for(i in 1:25){ # All data can be seen to False
  print(sqrt_vif_tgml[[i]])
}


# Predicion from train weather data in each Seoul district
head(test_gu_list[[1]])
pred_train_model = list()

for(i in 1:25){
  pred_train_model[[i]] <- predict(train_gu_model_list[[i]], newdata = test_gu_list[[i]][c(-5,-6)])
  pred_train_model[[i]] <- as.data.frame(pred_train_model[[i]])
}

# Compare Prediction data and test data using graph
pred_gg <- list()
for(i in 1:25){
  print(i)
  colnames(pred_train_model[[i]])<-"solar_pred"
}
for(i in 1:25){
  pred_gg[[i]] <- ggplot(pred_train_model[[i]], aes(x = index(pred_train_model[[i]]), y= pred_train_model[[i]]$solar_pred))+
    geom_line(color = "red") +
    geom_line(aes(x = index(pred_train_model[[i]]), y = test_gu_list[[i]]$solar), color = "blue")
}

pred_gg[[7]]

ggplot(pred_train_model[[7]],aes(x = index(pred_train_model[[7]]), y=pred_train_model[[7]]$solar_pred))+
  geom_line(color = "red") +
  geom_line(aes(x = index(pred_train_model[[7]]), y = test_gu_list[[7]]$solar), color = "blue")

library(gridExtra)
grid.arrange(pred_gg[[1]],pred_gg[[2]],pred_gg[[3]],pred_gg[[4]],pred_gg[[5]],pred_gg[[6]],pred_gg[[7]],pred_gg[[8]],pred_gg[[9]],pred_gg[[10]],pred_gg[[11]],
             pred_gg[[12]],pred_gg[[13]],pred_gg[[14]],pred_gg[[15]],pred_gg[[16]],pred_gg[[17]],pred_gg[[18]],pred_gg[[19]],pred_gg[[20]],
             pred_gg[[21]],pred_gg[[22]],pred_gg[[23]],pred_gg[[24]],pred_gg[[25]],ncol=5, nrow =5)


# Use regression analysis on weather data obtained through idw interpolation.
# ----------------------------------------------------------------------------

# Data processing for Regression analysis
all_cl<-read.csv("IDW_bike_location.csv") # IDW weather data from bike road coordinates
all_cl_na<-all_cl[-c(1,2)]
colnames(all_cl_na)<-NA
all_df<-rbind(all_cl_na[1:5],all_cl_na[6:10])

for(i in seq(11,(length(all_cl_na)),5)){
  print(i)
  j=i+4
  all_df<-rbind(all_df,all_cl_na[i:j])
}

colnames(all_df)<-c("temp","rain","humid","pm10","solar") # Convet to 5 column Data frame
head(all_df)

# Create train/test set
idx<-sample(nrow(all_df),nrow(all_df)*0.7)
(nrow(all_df)*0.3)*3
train<-all_df[idx,]
test<-all_df[-idx,]

# Create model
model<-lm(solar~.,data=train)
summary(model)

# Check and remove multicollinearity.
library(car)
vif(model)
sqrt(vif(model))>2

# Prediction
pre_solar<-predict(model,newdata = test[-5])
pre_solar<-as.data.frame(pre_solar)

# Visualization
library(ggplot2)
library(dplyr)
library(zoo)# Package for using the index function.

dev.off()
# Regression analysis predictions are not good.
ggplot(pre_solar,aes(x=index(pre_solar),y=pre_solar))+ geom_line(color="red")+
  geom_line(aes(x=index(pre_solar),y=test$solar),color="blue")