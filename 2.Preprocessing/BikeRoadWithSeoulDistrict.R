#### Kmeans results by bicycle coordinates, we finded lacking part so we modified cluster

library(dplyr)

setwd("C:\\Users\\gurtl\\Documents")
col_gu<-colors()[c(24,26,47,652,552,70,23,503,30,61,119,64,125,151,471)]#kmeans cluster color designate

##cluster 6:Sky blue, 5:red, 4:yellow ,3:light green ,2:blue ,1:black

############## Seocho-gu cluster value revision process ###############

sc <- read.csv("seocho.csv") #load the bike coordinate 
identify(sc$long, sc$lat) #When you click on the plot screen, you will see the line number with the x, y coordinates of the point in the picture.
View(sc) #Identify by view to see cluster values for points that fit x, y coordinates

########The process of changing cluster values in several ways#####
sc[c(145),'cluster'] = 1 #Replace the cluster value in the nth row with the value you want.#Use when I know the identify function
sc[sc[1] == 13979,'cluster'] = 3 #Replace with x value Cluster value

sc[(sc[3] >= 37.488) & (sc[5] == 1),'cluster'] = 4 #It is hard to change one by one, so it had been changed as ranged values

###########Output and csv file saving process####################

plot(sc$long,sc$lat,col = col_gu[sc$cluster])#Display image with cluster value 
#sc <- sc %>% filter(X != 13574)#Remove rows of coordinate points that are far apart
#seocho <- sc[c(order(sc$long)),]#Sort by long value and save
write.csv(sc,"seocho.csv",row.names = F)#Create csv file with saved values
Remainder

############ Seocho-gu complete, The remaining 24 gu Also modify cluster value with the same process ###########
#####################################################################################


############ gangnam-gu Modified process ###########

gn <- read.csv("gangnam.csv")

identify(gn$long, gn$lat)
gn[c(59),"cluster"] = 4
gn[(gn[2] >= 127.0383)&gn[3] >= 37.527,"cluster"] = 4

plot(gn$long,gn$lat,col = col_gu[gn$cluster])
#gangnam <- gn[c(order(gn$long)),]
#gn <- gn[,c(-1,-2,-3)]
#gn <- gn %>% filter(X != 871)
View(gn)

write.csv(gn,"gangnam.csv",row.names = F)

#########################################



############ gangbuk-gu Modified process ###########

gb <- read.csv("gangbuk.csv")

identify(gb$long, gb$lat)
gb[c(422,423,424,425,426,427,428,429,430,431,432,433,434),'cluster'] = 4
gb[gb[2] <= 127.0205,'cluster'] = 4

plot(gb$long,gb$lat,col = col_gu[gb$cluster])
View(gb)

write.csv(gb,"gangbuk.csv",row.names = F)


##########################################



########### gangdong-gu Modified process #############

gd <- read.csv("gangdong.csv")

identify(gd$long, gd$lat)
gd[c(206),'cluster'] = 4

plot(gd$long,gd$lat,col = col_gu[gd$cluster])
#View(gd)

write.csv(gd,"gangdong.csv",row.names = F)

##########################################



########## gangseo-gu Modified process ##############

gs <- read.csv("gangseo.csv")

identify(gs$long, gs$lat)
gs[c(112),'cluster'] = 6
gs[(gs[2] >= 126.8516)&(gs[3] >= 37.56857)&(gs[5] == 2),'cluster'] = 6

plot(gs$long,gs$lat,col = col_gu[gs$cluster])
View(gs)

write.csv(gs,"gangseo.csv",row.names = F)

##########################################



########### Dong Dae Moon-gu Modified process ##############

dd <- read.csv("ddm.csv")

plot(dd$long,dd$lat,col = col_gu[dd$cluster])

#write.csv(dd,"ddm.csv",row.names = F)

####### Not much to fix, short #########
###########################################



############ dongjak-gu Modified process #############
dj <- read.csv("dongjak.csv")

identify(dj$long, dj$lat)
dj[c(313),'cluster'] = 4

plot(dj$long,dj$lat,col = col_gu[dj$cluster])

dj[(dj[3] >= 37.50195)&(dj[5] == 3),"cluster"] = 4
#dongjak <- dj[c(order(dj$long)),]
View(dj)
dj <- dj[,c(-1)]

write.csv(dj,"dongjak.csv",row.names = F)

#############################################



############ Eunpyeong-gu Modified process ###############

ep <- read.csv("ep.csv")

identify(ep$long, ep$lat)
ep[c(74),] = 3
ep <- ep %>% filter(X != 21106)
ep <- ep[,c(-1)]

plot(ep$long,ep$lat,col = col_gu[ep$cluster])
#ep[ep[5] == 2,"cluster"] = 4

write.csv(ep,"ep.csv",row.names = F)

##############################################



########### geumcheon-gu Modified process #################

gc <- read.csv("geumcheon.csv")

plot(gc$long,gc$lat,col = col_gu[gc$cluster])

write.csv(gc,"geumcheon.csv",row.names = F)

##############################################



########### guro-gu Modified process #################

gr <- read.csv("guro.csv")

identify(gr$long, gr$lat)
gr[c(560),'cluster'] = 3

plot(gr$long,gr$lat,col = col_gu[gr$cluster])
gr[gr[5] == 2,"cluster"] = 4
#gr <- gr[c(order(gr$long)),]
#gr <- gr[,c(-1)]
#gr <- gr %>% filter(X != 7248)
View(gr)
gr <- gr[,c(-1)]

write.csv(gr,"guro.csv",row.names = F)

#############################################



############# gwanak-gu Modified process ##############

ga <- read.csv("gwanak.csv")

identify(ga$long, ga$lat)
ga[c(91),'cluster'] = 3
ga[(ga[2] >= 126.9601),"cluster"] = 5

plot(ga$long,ga$lat,col = col_gu[ga$cluster])
View(ga)

write.csv(ga,"gwanak.csv",row.names = F)

##############################################



########## gwangjin-gu Modified process ################

gj <- read.csv("gwangjin.csv")

plot(gj$long,gj$lat,col = col_gu[gj$cluster])

write.csv(gj,"gwangjin.csv",row.names = F)

#############################################



########### jongro-gu Modified process ################

jr <- read.csv("jongro.csv")

identify(jr$long, jr$lat)
jr[c(59),'cluster'] = 3

plot(jr$long,jr$lat,col = col_gu[jr$cluster])
View(jr)

write.csv(jr,"jongro.csv",row.names = F)

#############################################



########### jung-gu Modified process ##################

ju <- read.csv("jung.csv")

identify(ju$long, ju$lat)
plot(ju$long,ju$lat,col = col_gu[ju$cluster])
ju[(ju[2] >= 127.0049)&(ju[3] >= 37.5583),"cluster"] = 4
#jung <- ju[c(order(ju$long)),]
View(ju)
ju <- ju[,c(-1)]

write.csv(ju,"jung.csv",row.names = F)

################################################



########## jungnang-gu Modified process ##############

jn <- read.csv("jungnang.csv")

identify(jn$long, jn$lat)
jn[c(553),'cluster'] = 4
jn[(jn[2] >= 127.0796)&(jn[5] == 2),'cluster'] = 4

plot(jn$long,jn$lat,col = col_gu[jn$cluster])
View(jn)

write.csv(jn,"jungnang.csv",row.names = F)

###############################################



########## mapo-gu Modified process ############

mp <- read.csv("mapo.csv")

identify(mp$long, mp$lat)
mp[c(1132),'cluster'] = 2

plot(mp$long,mp$lat,col = col_gu[mp$cluster])
View(mp)

write.csv(mp,"mapo.csv",row.names = F)

#########################################



########### nowon-gu Modified process ################

nw <- read.csv("nowon.csv")

identify(nw$long, nw$lat)
nw[c(1132),'cluster'] = 4

plot(nw$long,nw$lat,col = col_gu[nw$cluster])

write.csv(nw,"nowon.csv",row.names = F)

##############################################



############# Seodaemun-gu Modified process ##############

sd <- read.csv("sdm.csv")

plot(sd$long,sd$lat,col = col_gu[sd$cluster])

write.csv(sd,"sdm.csv",row.names = F)

################################################



############### songbuk-gu Modified process ################

sb <- read.csv("songbuk.csv")

plot(sb$long,sb$lat,col = col_gu[sb$cluster])
#sb[sb[1] == 15429,"cluster"] = 1
#songbuk <- sb[c(order(sb$long)),]
#sb <- sb[,c(-1)]
#sb <- sb %>% filter(X != 15429)

#View(sb)
#write.csv(sb,"songbuk.csv",row.names = F)

##################################################



############# songdong-gu Modified process #################

sdg <- read.csv("songdong.csv")

identify(sdg$long, sdg$lat)

sdg[c(228,229),'cluster'] = 3
sdg[(sdg[3] >= 37.56225)&(sdg[2] >= 127.05072),"cluster"] = 1
plot(sdg$long,sdg$lat,col = col_gu[sdg$cluster])

#songdong <- sdg[c(order(sdg$long)),]

View(sdg)
sdg <- sdg[,c(-1)]

write.csv(sdg,"songdong.csv",row.names = F)

##################################################



############# songpa-gu Modified process ###################

sp <- read.csv("songpa.csv")

plot(sp$long,sp$lat,col = col_gu[sp$cluster])
sp <- sp[,c(-1)]

write.csv(sp,"songpa.csv",row.names = F)

##################################################



########### yangcheon-gu Modified process ####################

yc <- read.csv("yangcheon.csv")

plot(yc$long,yc$lat,col = col_gu[yc$cluster])

write.csv(yc,"yangcheon.csv",row.names = F)

###################################################



############# youngsan-gu Modified process ####################

ys <- read.csv("youngsan.csv")

identify(ys$long, ys$lat)
ys[c(120),'cluster'] = 2

plot(ys$long,ys$lat,col = col_gu[ys$cluster])
View(ys)

write.csv(ys,"youngsan.csv",row.names = F)

####################################################



############ dobong-gu Modified process #####################

db <- read.csv("dobong.csv")

identify(db$long, db$lat)
db[c(),'cluster'] = 2

plot(db$long,db$lat,col = col_gu[db$cluster])
#db[db[1] == 10605   ,"cluster"] = 3
#dobong <- db[c(order(db$long)),]
#View(dobong)

write.csv(db,"dobong.csv",row.names=F)

####################################################



############## Yeongdeungpo-gu Modified process ###################

ydp <- read.csv("yeongdp.csv")

identify(ydp$long, ydp$lat)
ydp[c(),'cluster'] = 4

ydp[(ydp[2] >= 126.9322) & (ydp[3] >= 37.52429) & (ydp[5] == 1),'cluster'] = 4
plot(ydp$long,ydp$lat,col = col_gu[ydp$cluster])

#ydp <- ydp[c(order(ydp$long)),]
#ydp <- ydp %>% filter(X != 19421)

View(ydp)
write.csv(ydp,"yeongdp.csv",row.names=F)
######################################################