#Row data: 2009.06 ~ 2019.06 weather variables(temperature, rainfalls, humidity, PM10, cloud amouts)
#Cut the row data into a yearly(2010~2018) & quarterly(1Q ~ 4Q) datas, and cluster them.

Seoul_Weather <- read.csv("Seoul_Weather(09_19)(-sun).csv", header = T, sep = ',')
Seoul_Weather
str(Seoul_Weather)

#1. First, cut datas by 3 years period(2010~2012, 2013~2015, 2016~2018)
df1<-data.frame()  #2010~2012
df2<-data.frame()  #2013~2015
df3<-data.frame()  #2016~2018

#Handling outlier
df1<-ifelse(substr(Seoul_Weather$date,3,4)=="10"|substr(Seoul_Weather$date,3,4)=="11"|substr(Seoul_Weather$date,3,4)=="12",row(Seoul_Weather),NA)
df1<-na.omit(df1)
df2<-ifelse(substr(Seoul_Weather$date,3,4)=="13"|substr(Seoul_Weather$date,3,4)=="14"|substr(Seoul_Weather$date,3,4)=="15",row(Seoul_Weather),NA)
df2<-na.omit(df2)
df3<-ifelse(substr(Seoul_Weather$date,3,4)=="16"|substr(Seoul_Weather$date,3,4)=="17"|substr(Seoul_Weather$date,3,4)=="18",row(Seoul_Weather),NA)
df3<-na.omit(df3)
Seoul_Weather1<-Seoul_Weather[df1,]    #2010.01 ~ 2012.12
Seoul_Weather2<-Seoul_Weather[df2,]    #2013.01 ~ 2015.12
Seoul_Weather3<-Seoul_Weather[df3,]    #2016.01 ~ 2018.12

#2. Divide #1's datasets by year by year.
w_list=list()  #Make an empty list.
w_list[[1]]<-ifelse(substr(Seoul_Weather$date,3,4)=="10",row(Seoul_Weather),NA) #2010
w_list[[1]]<-na.omit(w_list[[1]])
Seoul_Weather1<-Seoul_Weather[w_list[[1]],]
w_list[[2]]<-ifelse(substr(Seoul_Weather$date,3,4)=="11",row(Seoul_Weather),NA) #2011
w_list[[2]]<-na.omit(w_list[[2]])
Seoul_Weather2<-Seoul_Weather[w_list[[2]],]
w_list[[3]]<-ifelse(substr(Seoul_Weather$date,3,4)=="12",row(Seoul_Weather),NA) #2012
w_list[[3]]<-na.omit(w_list[[3]])
Seoul_Weather3<-Seoul_Weather[w_list[[3]],]
w_list[[4]]<-ifelse(substr(Seoul_Weather$date,3,4)=="13",row(Seoul_Weather),NA) #2013
w_list[[4]]<-na.omit(w_list[[4]])
Seoul_Weather4<-Seoul_Weather[w_list[[4]],]
w_list[[5]]<-ifelse(substr(Seoul_Weather$date,3,4)=="14",row(Seoul_Weather),NA) #2014
w_list[[5]]<-na.omit(w_list[[5]])
Seoul_Weather5<-Seoul_Weather[w_list[[5]],]
w_list[[6]]<-ifelse(substr(Seoul_Weather$date,3,4)=="15",row(Seoul_Weather),NA) #2015
w_list[[6]]<-na.omit(w_list[[6]])
Seoul_Weather6<-Seoul_Weather[w_list[[6]],]
w_list[[7]]<-ifelse(substr(Seoul_Weather$date,3,4)=="16",row(Seoul_Weather),NA) #2016
w_list[[7]]<-na.omit(w_list[[7]])
Seoul_Weather7<-Seoul_Weather[w_list[[7]],]
w_list[[8]]<-ifelse(substr(Seoul_Weather$date,3,4)=="17",row(Seoul_Weather),NA) #2017
w_list[[8]]<-na.omit(w_list[[8]])
Seoul_Weather8<-Seoul_Weather[w_list[[8]],]
w_list[[9]]<-ifelse(substr(Seoul_Weather$date,3,4)=="18",row(Seoul_Weather),NA) #2018
w_list[[9]]<-na.omit(w_list[[9]])
Seoul_Weather9<-Seoul_Weather[w_list[[9]],]

#Save #2 as csv files.
write.csv(Seoul_Weather1,"Seoul_Weather(10).csv",row.names = F)
write.csv(Seoul_Weather2,"Seoul_Weather(11).csv",row.names = F)
write.csv(Seoul_Weather3,"Seoul_Weather(12).csv",row.names = F)
write.csv(Seoul_Weather4,"Seoul_Weather(13).csv",row.names = F)
write.csv(Seoul_Weather5,"Seoul_Weather(14).csv",row.names = F)
write.csv(Seoul_Weather6,"Seoul_Weather(15).csv",row.names = F)
write.csv(Seoul_Weather7,"Seoul_Weather(16).csv",row.names = F)
write.csv(Seoul_Weather8,"Seoul_Weather(17).csv",row.names = F)
write.csv(Seoul_Weather9,"Seoul_Weather(18).csv",row.names = F)

#3. Divide yearly datas by quarter.
##2010
w_list_q = list()
w_list_q[[1]]<-ifelse(substr(Seoul_Weather1$date,6,7)=="01"|
                        substr(Seoul_Weather1$date,6,7)=="02"|
                        substr(Seoul_Weather1$date,6,7)=="03",row(Seoul_Weather1),NA) #2010 1Q
w_list_q[[1]]<-na.omit(w_list_q[[1]])
Seoul_Weather1_1<-Seoul_Weather1[w_list_q[[1]],]

w_list_q[[2]]<-ifelse(substr(Seoul_Weather1$date,6,7)=="04"|
                        substr(Seoul_Weather1$date,6,7)=="05"|
                        substr(Seoul_Weather1$date,6,7)=="06",row(Seoul_Weather1),NA) #2010 2Q
w_list_q[[2]]<-na.omit(w_list_q[[2]])
Seoul_Weather1_2<-Seoul_Weather1[w_list_q[[2]],]

w_list_q[[3]]<-ifelse(substr(Seoul_Weather1$date,6,7)=="07"|
                        substr(Seoul_Weather1$date,6,7)=="08"|
                        substr(Seoul_Weather1$date,6,7)=="09",row(Seoul_Weather1),NA) #2010 3Q
w_list_q[[3]]<-na.omit(w_list_q[[3]])
Seoul_Weather1_3<-Seoul_Weather1[w_list_q[[3]],]

w_list_q[[4]]<-ifelse(substr(Seoul_Weather1$date,6,7)=="10"|
                        substr(Seoul_Weather1$date,6,7)=="11"|
                        substr(Seoul_Weather1$date,6,7)=="12",row(Seoul_Weather1),NA) #2010 4Q   #Same below.
w_list_q[[4]]<-na.omit(w_list_q[[4]])
Seoul_Weather1_4<-Seoul_Weather1[w_list_q[[4]],]

##2011
w_list_q[[5]]<-ifelse(substr(Seoul_Weather2$date,6,7)=="01"|
                        substr(Seoul_Weather2$date,6,7)=="02"|
                        substr(Seoul_Weather2$date,6,7)=="03",row(Seoul_Weather2),NA)
w_list_q[[5]]<-na.omit(w_list_q[[5]])
Seoul_Weather2_1<-Seoul_Weather2[w_list_q[[5]],]

w_list_q[[6]]<-ifelse(substr(Seoul_Weather2$date,6,7)=="04"|
                        substr(Seoul_Weather2$date,6,7)=="05"|
                        substr(Seoul_Weather2$date,6,7)=="06",row(Seoul_Weather2),NA)
w_list_q[[6]]<-na.omit(w_list_q[[6]])
Seoul_Weather2_2<-Seoul_Weather2[w_list_q[[6]],]

w_list_q[[7]]<-ifelse(substr(Seoul_Weather2$date,6,7)=="07"|
                        substr(Seoul_Weather2$date,6,7)=="08"|
                        substr(Seoul_Weather2$date,6,7)=="09",row(Seoul_Weather2),NA)
w_list_q[[7]]<-na.omit(w_list_q[[7]])
Seoul_Weather2_3<-Seoul_Weather2[w_list_q[[7]],]

w_list_q[[8]]<-ifelse(substr(Seoul_Weather2$date,6,7)=="10"|
                        substr(Seoul_Weather2$date,6,7)=="11"|
                        substr(Seoul_Weather2$date,6,7)=="12",row(Seoul_Weather2),NA)
w_list_q[[8]]<-na.omit(w_list_q[[8]])
Seoul_Weather2_4<-Seoul_Weather2[w_list_q[[8]],]

##2012
w_list_q[[9]]<-ifelse(substr(Seoul_Weather3$date,6,7)=="01"|
                        substr(Seoul_Weather3$date,6,7)=="02"|
                        substr(Seoul_Weather3$date,6,7)=="03",row(Seoul_Weather3),NA)
w_list_q[[9]]<-na.omit(w_list_q[[9]])
Seoul_Weather3_1<-Seoul_Weather3[w_list_q[[9]],]

w_list_q[[10]]<-ifelse(substr(Seoul_Weather3$date,6,7)=="04"|
                         substr(Seoul_Weather3$date,6,7)=="05"|
                         substr(Seoul_Weather3$date,6,7)=="06",row(Seoul_Weather3),NA)
w_list_q[[10]]<-na.omit(w_list_q[[10]])
Seoul_Weather3_2<-Seoul_Weather3[w_list_q[[10]],]

w_list_q[[11]]<-ifelse(substr(Seoul_Weather3$date,6,7)=="07"|
                         substr(Seoul_Weather3$date,6,7)=="08"|
                         substr(Seoul_Weather3$date,6,7)=="09",row(Seoul_Weather3),NA)
w_list_q[[11]]<-na.omit(w_list_q[[11]])
Seoul_Weather3_3<-Seoul_Weather3[w_list_q[[11]],]

w_list_q[[12]]<-ifelse(substr(Seoul_Weather3$date,6,7)=="10"|
                         substr(Seoul_Weather3$date,6,7)=="11"|
                         substr(Seoul_Weather3$date,6,7)=="12",row(Seoul_Weather3),NA)
w_list_q[[12]]<-na.omit(w_list_q[[12]])
Seoul_Weather3_4<-Seoul_Weather3[w_list_q[[12]],]

##2013
w_list_q[[13]]<-ifelse(substr(Seoul_Weather4$date,6,7)=="01"|
                         substr(Seoul_Weather4$date,6,7)=="02"|
                         substr(Seoul_Weather4$date,6,7)=="03",row(Seoul_Weather4),NA)
w_list_q[[13]]<-na.omit(w_list_q[[13]])
Seoul_Weather4_1<-Seoul_Weather4[w_list_q[[13]],]

w_list_q[[14]]<-ifelse(substr(Seoul_Weather4$date,6,7)=="04"|
                         substr(Seoul_Weather4$date,6,7)=="05"|
                         substr(Seoul_Weather4$date,6,7)=="06",row(Seoul_Weather4),NA)
w_list_q[[14]]<-na.omit(w_list_q[[14]])
Seoul_Weather4_2<-Seoul_Weather4[w_list_q[[14]],]

w_list_q[[15]]<-ifelse(substr(Seoul_Weather4$date,6,7)=="07"|
                         substr(Seoul_Weather4$date,6,7)=="08"|
                         substr(Seoul_Weather4$date,6,7)=="09",row(Seoul_Weather4),NA)
w_list_q[[15]]<-na.omit(w_list_q[[15]])
Seoul_Weather4_3<-Seoul_Weather4[w_list_q[[15]],]

w_list_q[[16]]<-ifelse(substr(Seoul_Weather4$date,6,7)=="10"|
                         substr(Seoul_Weather4$date,6,7)=="11"|
                         substr(Seoul_Weather4$date,6,7)=="12",row(Seoul_Weather4),NA)
w_list_q[[16]]<-na.omit(w_list_q[[16]])
Seoul_Weather4_4<-Seoul_Weather4[w_list_q[[16]],]

##2014
w_list_q[[17]]<-ifelse(substr(Seoul_Weather5$date,6,7)=="01"|
                         substr(Seoul_Weather5$date,6,7)=="02"|
                         substr(Seoul_Weather5$date,6,7)=="03",row(Seoul_Weather5),NA)
w_list_q[[17]]<-na.omit(w_list_q[[17]])
Seoul_Weather5_1<-Seoul_Weather5[w_list_q[[17]],]

w_list_q[[18]]<-ifelse(substr(Seoul_Weather5$date,6,7)=="04"|
                         substr(Seoul_Weather5$date,6,7)=="05"|
                         substr(Seoul_Weather5$date,6,7)=="06",row(Seoul_Weather5),NA)
w_list_q[[18]]<-na.omit(w_list_q[[18]])
Seoul_Weather5_2<-Seoul_Weather5[w_list_q[[18]],]

w_list_q[[19]]<-ifelse(substr(Seoul_Weather5$date,6,7)=="07"|
                         substr(Seoul_Weather5$date,6,7)=="08"|
                         substr(Seoul_Weather5$date,6,7)=="09",row(Seoul_Weather5),NA)
w_list_q[[19]]<-na.omit(w_list_q[[19]])
Seoul_Weather5_3<-Seoul_Weather5[w_list_q[[19]],]

w_list_q[[20]]<-ifelse(substr(Seoul_Weather5$date,6,7)=="10"|
                         substr(Seoul_Weather5$date,6,7)=="11"|
                         substr(Seoul_Weather5$date,6,7)=="12",row(Seoul_Weather5),NA)
w_list_q[[20]]<-na.omit(w_list_q[[20]])
Seoul_Weather5_4<-Seoul_Weather5[w_list_q[[20]],]

##2015
w_list_q[[21]]<-ifelse(substr(Seoul_Weather6$date,6,7)=="01"|
                         substr(Seoul_Weather6$date,6,7)=="02"|
                         substr(Seoul_Weather6$date,6,7)=="03",row(Seoul_Weather6),NA)
w_list_q[[21]]<-na.omit(w_list_q[[21]])
Seoul_Weather6_1<-Seoul_Weather6[w_list_q[[21]],]

w_list_q[[22]]<-ifelse(substr(Seoul_Weather6$date,6,7)=="04"|
                         substr(Seoul_Weather6$date,6,7)=="05"|
                         substr(Seoul_Weather6$date,6,7)=="06",row(Seoul_Weather6),NA)
w_list_q[[22]]<-na.omit(w_list_q[[22]])
Seoul_Weather6_2<-Seoul_Weather6[w_list_q[[22]],]

w_list_q[[23]]<-ifelse(substr(Seoul_Weather6$date,6,7)=="07"|
                         substr(Seoul_Weather6$date,6,7)=="08"|
                         substr(Seoul_Weather6$date,6,7)=="09",row(Seoul_Weather6),NA)
w_list_q[[23]]<-na.omit(w_list_q[[23]])
Seoul_Weather6_3<-Seoul_Weather6[w_list_q[[23]],]

w_list_q[[24]]<-ifelse(substr(Seoul_Weather6$date,6,7)=="10"|
                         substr(Seoul_Weather6$date,6,7)=="11"|
                         substr(Seoul_Weather6$date,6,7)=="12",row(Seoul_Weather6),NA)
w_list_q[[24]]<-na.omit(w_list_q[[24]])
Seoul_Weather6_4<-Seoul_Weather6[w_list_q[[24]],]

##2016
w_list_q[[25]]<-ifelse(substr(Seoul_Weather7$date,6,7)=="01"|
                         substr(Seoul_Weather7$date,6,7)=="02"|
                         substr(Seoul_Weather7$date,6,7)=="03",row(Seoul_Weather7),NA)
w_list_q[[25]]<-na.omit(w_list_q[[25]])
Seoul_Weather7_1<-Seoul_Weather7[w_list_q[[25]],]

w_list_q[[26]]<-ifelse(substr(Seoul_Weather7$date,6,7)=="04"|
                         substr(Seoul_Weather7$date,6,7)=="05"|
                         substr(Seoul_Weather7$date,6,7)=="06",row(Seoul_Weather7),NA)
w_list_q[[26]]<-na.omit(w_list_q[[26]])
Seoul_Weather7_2<-Seoul_Weather7[w_list_q[[26]],]

w_list_q[[27]]<-ifelse(substr(Seoul_Weather7$date,6,7)=="07"|
                         substr(Seoul_Weather7$date,6,7)=="08"|
                         substr(Seoul_Weather7$date,6,7)=="09",row(Seoul_Weather7),NA)
w_list_q[[27]]<-na.omit(w_list_q[[27]])
Seoul_Weather7_3<-Seoul_Weather7[w_list_q[[27]],]

w_list_q[[28]]<-ifelse(substr(Seoul_Weather7$date,6,7)=="10"|
                         substr(Seoul_Weather7$date,6,7)=="11"|
                         substr(Seoul_Weather7$date,6,7)=="12",row(Seoul_Weather7),NA)
w_list_q[[28]]<-na.omit(w_list_q[[28]])
Seoul_Weather7_4<-Seoul_Weather7[w_list_q[[28]],]

##2017
w_list_q[[29]]<-ifelse(substr(Seoul_Weather8$date,6,7)=="01"|
                         substr(Seoul_Weather8$date,6,7)=="02"|
                         substr(Seoul_Weather8$date,6,7)=="03",row(Seoul_Weather8),NA)
w_list_q[[29]]<-na.omit(w_list_q[[29]])
Seoul_Weather8_1<-Seoul_Weather8[w_list_q[[29]],]

w_list_q[[30]]<-ifelse(substr(Seoul_Weather8$date,6,7)=="04"|
                         substr(Seoul_Weather8$date,6,7)=="05"|
                         substr(Seoul_Weather8$date,6,7)=="06",row(Seoul_Weather8),NA)
w_list_q[[30]]<-na.omit(w_list_q[[30]])
Seoul_Weather8_2<-Seoul_Weather8[w_list_q[[30]],]

w_list_q[[31]]<-ifelse(substr(Seoul_Weather8$date,6,7)=="07"|
                         substr(Seoul_Weather8$date,6,7)=="08"|
                         substr(Seoul_Weather8$date,6,7)=="09",row(Seoul_Weather8),NA)
w_list_q[[31]]<-na.omit(w_list_q[[31]])
Seoul_Weather8_3<-Seoul_Weather8[w_list_q[[31]],]

w_list_q[[32]]<-ifelse(substr(Seoul_Weather8$date,6,7)=="10"|
                         substr(Seoul_Weather8$date,6,7)=="11"|
                         substr(Seoul_Weather8$date,6,7)=="12",row(Seoul_Weather8),NA)
w_list_q[[32]]<-na.omit(w_list_q[[32]])
Seoul_Weather8_4<-Seoul_Weather8[w_list_q[[32]],]

##2018
w_list_q[[33]]<-ifelse(substr(Seoul_Weather9$date,6,7)=="01"|
                         substr(Seoul_Weather9$date,6,7)=="02"|
                         substr(Seoul_Weather9$date,6,7)=="03",row(Seoul_Weather9),NA)
w_list_q[[33]]<-na.omit(w_list_q[[33]])
Seoul_Weather9_1<-Seoul_Weather9[w_list_q[[33]],]

w_list_q[[34]]<-ifelse(substr(Seoul_Weather9$date,6,7)=="04"|
                         substr(Seoul_Weather9$date,6,7)=="05"|
                         substr(Seoul_Weather9$date,6,7)=="06",row(Seoul_Weather9),NA)
w_list_q[[34]]<-na.omit(w_list_q[[34]])
Seoul_Weather9_2<-Seoul_Weather9[w_list_q[[34]],]

w_list_q[[35]]<-ifelse(substr(Seoul_Weather9$date,6,7)=="07"|
                         substr(Seoul_Weather9$date,6,7)=="08"|
                         substr(Seoul_Weather9$date,6,7)=="09",row(Seoul_Weather9),NA)
w_list_q[[35]]<-na.omit(w_list_q[[35]])
Seoul_Weather9_3<-Seoul_Weather9[w_list_q[[35]],]

w_list_q[[36]]<-ifelse(substr(Seoul_Weather9$date,6,7)=="10"|
                         substr(Seoul_Weather9$date,6,7)=="11"|
                         substr(Seoul_Weather9$date,6,7)=="12",row(Seoul_Weather9),NA)
w_list_q[[36]]<-na.omit(w_list_q[[36]])
Seoul_Weather9_4<-Seoul_Weather9[w_list_q[[36]],]

# Save quaterly divided datas as csv files
write.csv(Seoul_Weather1_1, "Seoul_Weather10_1Q.csv", row.names = F)
write.csv(Seoul_Weather1_2, "Seoul_Weather10_2Q.csv", row.names = F)
write.csv(Seoul_Weather1_3, "Seoul_Weather10_3Q.csv", row.names = F)
write.csv(Seoul_Weather1_4, "Seoul_Weather10_4Q.csv", row.names = F)
write.csv(Seoul_Weather2_1, "Seoul_Weather11_1Q.csv", row.names = F)
write.csv(Seoul_Weather2_2, "Seoul_Weather11_2Q.csv", row.names = F)
write.csv(Seoul_Weather2_3, "Seoul_Weather11_3Q.csv", row.names = F)
write.csv(Seoul_Weather2_4, "Seoul_Weather11_4Q.csv", row.names = F)
write.csv(Seoul_Weather3_1, "Seoul_Weather12_1Q.csv", row.names = F)
write.csv(Seoul_Weather3_2, "Seoul_Weather12_2Q.csv", row.names = F)
write.csv(Seoul_Weather3_3, "Seoul_Weather12_3Q.csv", row.names = F)
write.csv(Seoul_Weather3_4, "Seoul_Weather12_4Q.csv", row.names = F)
write.csv(Seoul_Weather4_1, "Seoul_Weather13_1Q.csv", row.names = F)
write.csv(Seoul_Weather4_2, "Seoul_Weather13_2Q.csv", row.names = F)
write.csv(Seoul_Weather4_3, "Seoul_Weather13_3Q.csv", row.names = F)
write.csv(Seoul_Weather4_4, "Seoul_Weather13_4Q.csv", row.names = F)
write.csv(Seoul_Weather5_1, "Seoul_Weather14_1Q.csv", row.names = F)
write.csv(Seoul_Weather5_2, "Seoul_Weather14_2Q.csv", row.names = F)
write.csv(Seoul_Weather5_3, "Seoul_Weather14_3Q.csv", row.names = F)
write.csv(Seoul_Weather5_4, "Seoul_Weather14_4Q.csv", row.names = F)
write.csv(Seoul_Weather6_1, "Seoul_Weather15_1Q.csv", row.names = F)
write.csv(Seoul_Weather6_2, "Seoul_Weather15_2Q.csv", row.names = F)
write.csv(Seoul_Weather6_3, "Seoul_Weather15_3Q.csv", row.names = F)
write.csv(Seoul_Weather6_4, "Seoul_Weather15_4Q.csv", row.names = F)
write.csv(Seoul_Weather7_1, "Seoul_Weather16_1Q.csv", row.names = F)
write.csv(Seoul_Weather7_2, "Seoul_Weather16_2Q.csv", row.names = F)
write.csv(Seoul_Weather7_3, "Seoul_Weather16_3Q.csv", row.names = F)
write.csv(Seoul_Weather7_4, "Seoul_Weather16_4Q.csv", row.names = F)
write.csv(Seoul_Weather8_1, "Seoul_Weather17_1Q.csv", row.names = F)
write.csv(Seoul_Weather8_2, "Seoul_Weather17_2Q.csv", row.names = F)
write.csv(Seoul_Weather8_3, "Seoul_Weather17_3Q.csv", row.names = F)
write.csv(Seoul_Weather8_4, "Seoul_Weather17_4Q.csv", row.names = F)
write.csv(Seoul_Weather9_1, "Seoul_Weather18_1Q.csv", row.names = F)
write.csv(Seoul_Weather9_2, "Seoul_Weather18_2Q.csv", row.names = F)
write.csv(Seoul_Weather9_3, "Seoul_Weather18_3Q.csv", row.names = F)
write.csv(Seoul_Weather9_4, "Seoul_Weather18_4Q.csv", row.names = F)

#4. Calculate Means by quarter.
library(dplyr)

sw10_1 <- Seoul_Weather1_1 %>% 
  group_by(location) %>%
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw10_1) <- c("loc10_1", "avgtmp10_1", "rain10_1", "avghum10_1", "pm1010_1", "avgcloud10_1")

sw10_2 <- Seoul_Weather1_2 %>% 
  group_by(location) %>%
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw10_2) <- c("loc10_2", "avgtmp10_2", "rain10_2", "avghum10_2", "pm1010_2", "avgcloud10_2")

sw10_3 <- Seoul_Weather1_3 %>% 
  group_by(location) %>%
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw10_3) <- c("loc10_3", "avgtmp10_3", "rain10_3", "avghum10_3", "pm1010_3", "avgcloud10_3")

sw10_4 <- Seoul_Weather1_4 %>% 
  group_by(location) %>%
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw10_4) <- c("loc10_4", "avgtmp10_4", "rain10_4", "avghum10_4", "pm1010_4", "avgcloud10_4")

sw11_1 <- Seoul_Weather2_1 %>% 
  group_by(location) %>%
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw11_1) <- c("loc11_1", "avgtmp11_1", "rain11_1", "avghum11_1", "pm1011_1", "avgcloud11_1")

sw11_2 <- Seoul_Weather2_2 %>% 
  group_by(location) %>%
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw11_2) <- c("loc11_2", "avgtmp11_2", "rain11_2", "avghum11_2", "pm1011_2", "avgcloud11_2")

sw11_3 <- Seoul_Weather2_3 %>% 
  group_by(location) %>%
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw11_3) <- c("loc11_3", "avgtmp11_3", "rain11_3", "avghum11_3", "pm1011_3", "avgcloud11_3")

sw11_4 <- Seoul_Weather2_4 %>% 
  group_by(location) %>%
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw11_4) <- c("loc11_4", "avgtmp11_4", "rain11_4", "avghum11_4", "pm1011_4", "avgcloud11_4")

sw12_1 <- Seoul_Weather3_1 %>% 
  group_by(location) %>%
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw12_1) <- c("loc12_1", "avgtmp12_1", "rain12_1", "avghum12_1", "pm1012_1", "avgcloud12_1")

sw12_2 <- Seoul_Weather3_2 %>% 
  group_by(location) %>%
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw12_2) <- c("loc12_2", "avgtmp12_2", "rain12_2", "avghum12_2", "pm1012_2", "avgcloud12_2")

sw12_3 <- Seoul_Weather3_3 %>% 
  group_by(location) %>%
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw12_3) <- c("loc12_3", "avgtmp12_3", "rain12_3", "avghum12_3", "pm1012_3", "avgcloud12_3")

sw12_4 <- Seoul_Weather3_4 %>% 
  group_by(location) %>%
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw12_4) <- c("loc12_4", "avgtmp12_4", "rain12_4", "avghum12_4", "pm1012_4", "avgcloud12_4")

sw13_1 <- Seoul_Weather4_1 %>% 
  group_by(location) %>%
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw13_1) <- c("loc13_1", "avgtmp13_1", "rain13_1", "avghum13_1", "pm1013_1", "avgcloud13_1")

sw13_2 <- Seoul_Weather4_2 %>% 
  group_by(location) %>%
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw13_2) <- c("loc13_2", "avgtmp13_2", "rain13_2", "avghum13_2", "pm1013_2", "avgcloud13_2")

sw13_3 <- Seoul_Weather4_3 %>% 
  group_by(location) %>%
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw13_3) <- c("loc13_3", "avgtmp13_3", "rain13_3", "avghum13_3", "pm1013_3", "avgcloud13_3")

sw13_4 <- Seoul_Weather4_4 %>% 
  group_by(location) %>%
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw13_4) <- c("loc13_4", "avgtmp13_4", "rain13_4", "avghum13_4", "pm1013_4", "avgcloud13_4")

sw14_1 <- Seoul_Weather5_1 %>% 
  group_by(location) %>%
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw14_1) <- c("loc14_1", "avgtmp14_1", "rain14_1", "avghum14_1", "pm1014_1", "avgcloud14_1")

sw14_2 <- Seoul_Weather5_2 %>% 
  group_by(location) %>%
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw14_2) <- c("loc14_2", "avgtmp14_2", "rain14_2", "avghum14_2", "pm1014_2", "avgcloud14_2")

sw14_3 <- Seoul_Weather5_3 %>% 
  group_by(location) %>%
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw14_3) <- c("loc14_3", "avgtmp14_3", "rain14_3", "avghum14_3", "pm1014_3", "avgcloud14_3")

sw14_4 <- Seoul_Weather5_4 %>% 
  group_by(location) %>%
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw14_4) <- c("loc14_4", "avgtmp14_4", "rain14_4", "avghum14_4", "pm1014_4", "avgcloud14_4")

sw15_1 <- Seoul_Weather6_1 %>% 
  group_by(location) %>%
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw15_1) <- c("loc15_1", "avgtmp15_1", "rain15_1", "avghum15_1", "pm1015_1", "avgcloud15_1")

sw15_2 <- Seoul_Weather6_2 %>% 
  group_by(location) %>%
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw15_2) <- c("loc15_2", "avgtmp15_2", "rain15_2", "avghum15_2", "pm1015_2", "avgcloud15_2")

sw15_3 <- Seoul_Weather6_3 %>% 
  group_by(location) %>%
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw15_3) <- c("loc15_3", "avgtmp15_3", "rain15_3", "avghum15_3", "pm1015_3", "avgcloud15_3")

sw15_4 <- Seoul_Weather6_4 %>% 
  group_by(location) %>%
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw15_4) <- c("loc15_4", "avgtmp15_4", "rain15_4", "avghum15_4", "pm1015_4", "avgcloud15_4")

sw16_1 <- Seoul_Weather7_1 %>% 
  group_by(location) %>% 
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw16_1) <- c("loc16_1", "avgtmp16_1", "rain16_1", "avghum16_1", "pm1016_1", "avgcloud16_1")

sw16_2 <- Seoul_Weather7_2 %>% 
  group_by(location) %>% 
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw16_2) <- c("loc16_2", "avgtmp16_2", "rain16_2", "avghum16_2", "pm1016_2", "avgcloud16_2")

sw16_3 <- Seoul_Weather7_3 %>% 
  group_by(location) %>% 
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw16_3) <- c("loc16_3", "avgtmp16_3", "rain16_3", "avghum16_3", "pm1016_3", "avgcloud16_3")

sw16_4 <- Seoul_Weather7_4 %>% 
  group_by(location) %>% 
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw16_4) <- c("loc16_4", "avgtmp16_4", "rain16_4", "avghum16_4", "pm1016_4", "avgcloud16_4")

sw17_1 <- Seoul_Weather8_1 %>% 
  group_by(location) %>% 
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw17_1) <- c("loc17_1", "avgtmp17_1", "rain17_1", "avghum17_1", "pm1017_1", "avgcloud17_1")

sw17_2 <- Seoul_Weather8_2 %>% 
  group_by(location) %>% 
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw17_2) <- c("loc17_2", "avgtmp17_2", "rain17_2", "avghum17_2", "pm1017_2", "avgcloud17_2")

sw17_3 <- Seoul_Weather8_3 %>% 
  group_by(location) %>% 
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw17_3) <- c("loc17_3", "avgtmp17_3", "rain17_3", "avghum17_3", "pm1017_3", "avgcloud17_3")

sw17_4 <- Seoul_Weather8_4 %>% 
  group_by(location) %>% 
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw17_4) <- c("loc17_4", "avgtmp17_4", "rain17_4", "avghum17_4", "pm1017_4", "avgcloud17_4")

sw18_1 <- Seoul_Weather9_1 %>% 
  group_by(location) %>% 
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw18_1) <- c("loc18_1", "avgtmp18_1", "rain18_1", "avghum18_1", "pm1018_1", "avgcloud18_1")

sw18_2 <- Seoul_Weather9_2 %>% 
  group_by(location) %>% 
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw18_2) <- c("loc18_2", "avgtmp18_2", "rain18_2", "avghum18_2", "pm1018_2", "avgcloud18_2")

sw18_3 <- Seoul_Weather9_3 %>% 
  group_by(location) %>% 
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw18_3) <- c("loc18_3", "avgtmp18_3", "rain18_3", "avghum18_3", "pm1018_3", "avgcloud18_3")

sw18_4 <- Seoul_Weather9_4 %>% 
  group_by(location) %>% 
  summarise(tot_avg_temp <- mean(avg_temp),
            tot_rain_amt <- mean(rain_amt),
            tot_avg_humid <- mean(avg_humid),
            tot_PM10 <- mean(PM10),
            tot_avg_cloud_amt <- mean(avg_cloud_amt))

colnames(sw18_4) <- c("loc18_4", "avgtmp18_4", "rain18_4", "avghum18_4", "pm1018_4", "avgcloud18_4")

boxplot(sw10_1)
boxplot(sw10_2)
boxplot(sw10_3)
boxplot(sw10_4)
boxplot(sw11_1)
boxplot(sw11_2)
boxplot(sw11_3)
boxplot(sw11_4)
boxplot(sw12_1)
boxplot(sw12_2)
boxplot(sw12_3)
boxplot(sw12_4)
boxplot(sw13_1)
boxplot(sw13_2)
boxplot(sw13_3)
boxplot(sw13_4)
boxplot(sw14_1)
boxplot(sw14_2)
boxplot(sw14_3)
boxplot(sw14_4)
boxplot(sw15_1)
boxplot(sw15_2)
boxplot(sw15_3)
boxplot(sw15_4)
boxplot(sw16_1)
boxplot(sw16_2)
boxplot(sw16_3)
boxplot(sw16_4)
boxplot(sw17_1)
boxplot(sw17_2)
boxplot(sw17_3)
boxplot(sw17_4)
boxplot(sw18_1)
boxplot(sw18_2)
boxplot(sw18_3)
boxplot(sw18_4)

#5. Add up insolation data(by cbind)
## Data loading: Monthly insolation datas of each district.
sunlight <- read.csv("solar_centre_25_modify.csv", header=T, sep = ',')
library(dplyr)
sunlight <- arrange(sunlight, YEAR)
str(sunlight)
View(sunlight)
View(sw10_1)

sunlight <- sunlight%>%mutate(AVG_SUN_1Q = (JAN + FEB + MAR)/3 )  #Average insolations quaterly.
sunlight <- sunlight%>%mutate(AVG_SUN_2Q = (APR + MAY + JUN)/3 )
sunlight <- sunlight%>%mutate(AVG_SUN_3Q = (JUL + AUG + SEP)/3 )
sunlight <- sunlight%>%mutate(AVG_SUN_4Q = (OCT + NOV + DEC)/3 )

###Remove cloud amounts, add up quartely averaged insolations.
sw10_1 <- sw10_1[-6]%>%mutate(avgsunlight10_1 = filter(sunlight, YEAR==2010)$AVG_SUN_1Q)
sw10_2 <- sw10_2[-6]%>%mutate(avgsunlight10_2 = filter(sunlight, YEAR==2010)$AVG_SUN_2Q)
sw10_3 <- sw10_3[-6]%>%mutate(avgsunlight10_3 = filter(sunlight, YEAR==2010)$AVG_SUN_3Q)
sw10_4 <- sw10_4[-6]%>%mutate(avgsunlight10_4 = filter(sunlight, YEAR==2010)$AVG_SUN_4Q)

sw11_1 <- sw11_1[-6]%>%mutate(avgsunlight11_1 = filter(sunlight, YEAR==2011)$AVG_SUN_1Q)
sw11_2 <- sw11_2[-6]%>%mutate(avgsunlight11_2 = filter(sunlight, YEAR==2011)$AVG_SUN_2Q)
sw11_3 <- sw11_3[-6]%>%mutate(avgsunlight11_3 = filter(sunlight, YEAR==2011)$AVG_SUN_3Q)
sw11_4 <- sw11_4[-6]%>%mutate(avgsunlight11_4 = filter(sunlight, YEAR==2011)$AVG_SUN_4Q)

sw12_1 <- sw12_1[-6]%>%mutate(avgsunlight12_1 = filter(sunlight, YEAR==2012)$AVG_SUN_1Q)
sw12_2 <- sw12_2[-6]%>%mutate(avgsunlight12_2 = filter(sunlight, YEAR==2012)$AVG_SUN_2Q)
sw12_3 <- sw12_3[-6]%>%mutate(avgsunlight12_3 = filter(sunlight, YEAR==2012)$AVG_SUN_3Q)
sw12_4 <- sw12_4[-6]%>%mutate(avgsunlight12_4 = filter(sunlight, YEAR==2012)$AVG_SUN_4Q)

sw13_1 <- sw13_1[-6]%>%mutate(avgsunlight13_1 = filter(sunlight, YEAR==2013)$AVG_SUN_1Q)
sw13_2 <- sw13_2[-6]%>%mutate(avgsunlight13_2 = filter(sunlight, YEAR==2013)$AVG_SUN_2Q)
sw13_3 <- sw13_3[-6]%>%mutate(avgsunlight13_3 = filter(sunlight, YEAR==2013)$AVG_SUN_3Q)
sw13_4 <- sw13_4[-6]%>%mutate(avgsunlight13_4 = filter(sunlight, YEAR==2013)$AVG_SUN_4Q)

sw14_1 <- sw14_1[-6]%>%mutate(avgsunlight14_1 = filter(sunlight, YEAR==2014)$AVG_SUN_1Q)
sw14_2 <- sw14_2[-6]%>%mutate(avgsunlight14_2 = filter(sunlight, YEAR==2014)$AVG_SUN_2Q)
sw14_3 <- sw14_3[-6]%>%mutate(avgsunlight14_3 = filter(sunlight, YEAR==2014)$AVG_SUN_3Q)
sw14_4 <- sw14_4[-6]%>%mutate(avgsunlight14_4 = filter(sunlight, YEAR==2014)$AVG_SUN_4Q)

sw15_1 <- sw15_1[-6]%>%mutate(avgsunlight15_1 = filter(sunlight, YEAR==2015)$AVG_SUN_1Q)
sw15_2 <- sw15_2[-6]%>%mutate(avgsunlight15_2 = filter(sunlight, YEAR==2015)$AVG_SUN_2Q)
sw15_3 <- sw15_3[-6]%>%mutate(avgsunlight15_3 = filter(sunlight, YEAR==2015)$AVG_SUN_3Q)
sw15_4 <- sw15_4[-6]%>%mutate(avgsunlight15_4 = filter(sunlight, YEAR==2015)$AVG_SUN_4Q)

sw15_1 <- sw15_1[-6]%>%mutate(avgsunlight15_1 = filter(sunlight, YEAR==2015)$AVG_SUN_1Q)
sw15_2 <- sw15_2[-6]%>%mutate(avgsunlight15_2 = filter(sunlight, YEAR==2015)$AVG_SUN_2Q)
sw15_3 <- sw15_3[-6]%>%mutate(avgsunlight15_3 = filter(sunlight, YEAR==2015)$AVG_SUN_3Q)
sw15_4 <- sw15_4[-6]%>%mutate(avgsunlight15_4 = filter(sunlight, YEAR==2015)$AVG_SUN_4Q)

sw16_1 <- sw16_1[-6]%>%mutate(avgsunlight16_1 = filter(sunlight, YEAR==2016)$AVG_SUN_1Q)
sw16_2 <- sw16_2[-6]%>%mutate(avgsunlight16_2 = filter(sunlight, YEAR==2016)$AVG_SUN_2Q)
sw16_3 <- sw16_3[-6]%>%mutate(avgsunlight16_3 = filter(sunlight, YEAR==2016)$AVG_SUN_3Q)
sw16_4 <- sw16_4[-6]%>%mutate(avgsunlight16_4 = filter(sunlight, YEAR==2016)$AVG_SUN_4Q)

sw17_1 <- sw17_1[-6]%>%mutate(avgsunlight17_1 = filter(sunlight, YEAR==2017)$AVG_SUN_1Q)
sw17_2 <- sw17_2[-6]%>%mutate(avgsunlight17_2 = filter(sunlight, YEAR==2017)$AVG_SUN_2Q)
sw17_3 <- sw17_3[-6]%>%mutate(avgsunlight17_3 = filter(sunlight, YEAR==2017)$AVG_SUN_3Q)
sw17_4 <- sw17_4[-6]%>%mutate(avgsunlight17_4 = filter(sunlight, YEAR==2017)$AVG_SUN_4Q)

sw18_1 <- sw18_1[-6]%>%mutate(avgsunlight18_1 = filter(sunlight, YEAR==2018)$AVG_SUN_1Q)
sw18_2 <- sw18_2[-6]%>%mutate(avgsunlight18_2 = filter(sunlight, YEAR==2018)$AVG_SUN_2Q)
sw18_3 <- sw18_3[-6]%>%mutate(avgsunlight18_3 = filter(sunlight, YEAR==2018)$AVG_SUN_3Q)
sw18_4 <- sw18_4[-6]%>%mutate(avgsunlight18_4 = filter(sunlight, YEAR==2018)$AVG_SUN_4Q)

#6. Save all quartely averaged weather datas into a dataframe.
all_df <- data.frame()

all_df<-cbind(sw10_1,sw10_2[,2:6])
all_df<-cbind(all_df,sw10_3[,2:6])
all_df<-cbind(all_df,sw10_4[,2:6])
all_df<-cbind(all_df,sw11_1[,2:6])
all_df<-cbind(all_df,sw11_2[,2:6])
all_df<-cbind(all_df,sw11_3[,2:6])
all_df<-cbind(all_df,sw11_4[,2:6])
all_df<-cbind(all_df,sw12_1[,2:6])
all_df<-cbind(all_df,sw12_2[,2:6])
all_df<-cbind(all_df,sw12_3[,2:6])
all_df<-cbind(all_df,sw12_4[,2:6])
all_df<-cbind(all_df,sw13_1[,2:6])
all_df<-cbind(all_df,sw13_2[,2:6])
all_df<-cbind(all_df,sw13_3[,2:6])
all_df<-cbind(all_df,sw13_4[,2:6])
all_df<-cbind(all_df,sw14_1[,2:6])
all_df<-cbind(all_df,sw14_2[,2:6])
all_df<-cbind(all_df,sw14_3[,2:6])
all_df<-cbind(all_df,sw14_4[,2:6])
all_df<-cbind(all_df,sw15_1[,2:6])
all_df<-cbind(all_df,sw15_2[,2:6])
all_df<-cbind(all_df,sw15_3[,2:6])
all_df<-cbind(all_df,sw15_4[,2:6])
all_df<-cbind(all_df,sw16_1[,2:6])
all_df<-cbind(all_df,sw16_2[,2:6])
all_df<-cbind(all_df,sw16_3[,2:6])
all_df<-cbind(all_df,sw16_4[,2:6])
all_df<-cbind(all_df,sw17_1[,2:6])
all_df<-cbind(all_df,sw17_2[,2:6])
all_df<-cbind(all_df,sw17_3[,2:6])
all_df<-cbind(all_df,sw17_4[,2:6])
all_df<-cbind(all_df,sw18_1[,2:6])
all_df<-cbind(all_df,sw18_2[,2:6])
all_df<-cbind(all_df,sw18_3[,2:6])
all_df<-cbind(all_df,sw18_4[,2:6])