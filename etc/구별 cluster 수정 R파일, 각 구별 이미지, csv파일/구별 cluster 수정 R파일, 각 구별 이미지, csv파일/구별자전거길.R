#### 자전거 좌표별로 kmeans한 결과 부족한 부분 발견 cluster를 수정하는 과정 


library(dplyr)

setwd("C:\\Users\\gurtl\\Documents")
col_gu<-colors()[c(24,26,47,652,552,70,23,503,30,61,119,64,125,151,471)]#kmeans cluster 색깔 지정

##cluster 6번:하늘색, 5번:빨강, 4번:노랑 ,3번:연두 ,2번:파랑 ,1번:검정


############## 서초구 cluster값 수정 과정 ###############

sc <- read.csv("seocho.csv")#자전거 좌표 읽음
identify(sc$long, sc$lat)#plot한 결과에 click하면 점의 x,y좌표가 있는 행 번호가 그림에 표시됨 
View(sc)#x,y 좌표에 맞는 점의 cluster 값을 확인 하기 위해 View로 파악

########여러가지 방법으로 cluster 값을 바꾸는 과정#####
sc[c(145),'cluster'] = 1#행 순서가 n번 째인 cluster값을 원하는 값으로 바꿈#identify 기능을 알았을 때 사용 
sc[sc[1] == 13979,'cluster'] = 3#일일이 X값으로 cluster 값을 바꿈

sc[(sc[3] >= 37.488) & (sc[5] == 1),'cluster'] = 4#하나씩 바꾸기엔 오래 걸려서 범위로 할 수있는건 범위로 바꿈

###########출력과 csv파일 저장 과정####################

plot(sc$long,sc$lat,col = col_gu[sc$cluster])#cluster 값으로 색깔을 주며 이미지 출력 
#sc <- sc %>% filter(X != 13574)#완전 동떨어진 좌표점들의 행 제거 
#seocho <- sc[c(order(sc$long)),]#long값으로 정렬한 다음 저장
write.csv(sc,"seocho.csv",row.names = F)#저장한 값으로 csv파일로 만듬


############ 서초구 완료 나머지 24개 구 또한 같은 과정으로 cluster값 수정 ###########
#####################################################################################


############ 강남구 수정 과정 ###########

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



############ 강북구 수정 과정 ###########

gb <- read.csv("gangbuk.csv")

identify(gb$long, gb$lat)
gb[c(422,423,424,425,426,427,428,429,430,431,432,433,434),'cluster'] = 4
gb[gb[2] <= 127.0205,'cluster'] = 4

plot(gb$long,gb$lat,col = col_gu[gb$cluster])
View(gb)

write.csv(gb,"gangbuk.csv",row.names = F)


##########################################



########### 강동구 수정 과정 #############

gd <- read.csv("gangdong.csv")

identify(gd$long, gd$lat)
gd[c(206),'cluster'] = 4

plot(gd$long,gd$lat,col = col_gu[gd$cluster])
#View(gd)

write.csv(gd,"gangdong.csv",row.names = F)

##########################################



########## 강서구 수정 과정 ##############

gs <- read.csv("gangseo.csv")

identify(gs$long, gs$lat)
gs[c(112),'cluster'] = 6
gs[(gs[2] >= 126.8516)&(gs[3] >= 37.56857)&(gs[5] == 2),'cluster'] = 6

plot(gs$long,gs$lat,col = col_gu[gs$cluster])
View(gs)

write.csv(gs,"gangseo.csv",row.names = F)

##########################################



########### 동대문 수정 과정 ##############

dd <- read.csv("ddm.csv")

plot(dd$long,dd$lat,col = col_gu[dd$cluster])

#write.csv(dd,"ddm.csv",row.names = F)

####### 고칠게 별로 없어서 짧음 #########
###########################################



############ 동작구 수정 과정 #############
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



############ 은평구 수정 과정 ###############

ep <- read.csv("ep.csv")

identify(ep$long, ep$lat)
ep[c(74),] = 3
ep <- ep %>% filter(X != 21106)
ep <- ep[,c(-1)]

plot(ep$long,ep$lat,col = col_gu[ep$cluster])
#ep[ep[5] == 2,"cluster"] = 4

write.csv(ep,"ep.csv",row.names = F)

##############################################



########### 금천구 수정 과정 #################

gc <- read.csv("geumcheon.csv")

plot(gc$long,gc$lat,col = col_gu[gc$cluster])

write.csv(gc,"geumcheon.csv",row.names = F)

##############################################



########### 구로구 수정 과정 #################

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



############# 관악구 수정 과정 ##############

ga <- read.csv("gwanak.csv")

identify(ga$long, ga$lat)
ga[c(91),'cluster'] = 3
ga[(ga[2] >= 126.9601),"cluster"] = 5

plot(ga$long,ga$lat,col = col_gu[ga$cluster])
View(ga)

write.csv(ga,"gwanak.csv",row.names = F)

##############################################



########## 광진구 수정 과정 ################

gj <- read.csv("gwangjin.csv")

plot(gj$long,gj$lat,col = col_gu[gj$cluster])

write.csv(gj,"gwangjin.csv",row.names = F)

#############################################



########### 종로구 수정 과정 ################

jr <- read.csv("jongro.csv")

identify(jr$long, jr$lat)
jr[c(59),'cluster'] = 3

plot(jr$long,jr$lat,col = col_gu[jr$cluster])
View(jr)

write.csv(jr,"jongro.csv",row.names = F)

#############################################



########### 중구 수정 과정 ##################

ju <- read.csv("jung.csv")

identify(ju$long, ju$lat)
plot(ju$long,ju$lat,col = col_gu[ju$cluster])
ju[(ju[2] >= 127.0049)&(ju[3] >= 37.5583),"cluster"] = 4
#jung <- ju[c(order(ju$long)),]
View(ju)
ju <- ju[,c(-1)]

write.csv(ju,"jung.csv",row.names = F)

################################################



########## 중랑구 수정 과정 ##############

jn <- read.csv("jungnang.csv")

identify(jn$long, jn$lat)
jn[c(553),'cluster'] = 4
jn[(jn[2] >= 127.0796)&(jn[5] == 2),'cluster'] = 4

plot(jn$long,jn$lat,col = col_gu[jn$cluster])
View(jn)

write.csv(jn,"jungnang.csv",row.names = F)

###############################################



########## 마포구 수정 과정 ############

mp <- read.csv("mapo.csv")

identify(mp$long, mp$lat)
mp[c(1132),'cluster'] = 2

plot(mp$long,mp$lat,col = col_gu[mp$cluster])
View(mp)

write.csv(mp,"mapo.csv",row.names = F)

#########################################



########### 노원구 수정 과정 ################

nw <- read.csv("nowon.csv")

identify(nw$long, nw$lat)
nw[c(1132),'cluster'] = 4

plot(nw$long,nw$lat,col = col_gu[nw$cluster])

write.csv(nw,"nowon.csv",row.names = F)

##############################################



############# 서대문구 수정 과정 ##############

sd <- read.csv("sdm.csv")

plot(sd$long,sd$lat,col = col_gu[sd$cluster])

write.csv(sd,"sdm.csv",row.names = F)

################################################



############### 성북구 수정 과정 ################

sb <- read.csv("songbuk.csv")

plot(sb$long,sb$lat,col = col_gu[sb$cluster])
#sb[sb[1] == 15429,"cluster"] = 1
#songbuk <- sb[c(order(sb$long)),]
#sb <- sb[,c(-1)]
#sb <- sb %>% filter(X != 15429)

#View(sb)
#write.csv(sb,"songbuk.csv",row.names = F)

##################################################



############# 성동구 수정 과정 #################

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



############# 송파구 수정 과정 ###################

sp <- read.csv("songpa.csv")

plot(sp$long,sp$lat,col = col_gu[sp$cluster])
sp <- sp[,c(-1)]

write.csv(sp,"songpa.csv",row.names = F)

##################################################



########### 양천구 수정 과정 ####################

yc <- read.csv("yangcheon.csv")

plot(yc$long,yc$lat,col = col_gu[yc$cluster])

write.csv(yc,"yangcheon.csv",row.names = F)

###################################################



############# 용산구 수정 과정 ####################

ys <- read.csv("youngsan.csv")

identify(ys$long, ys$lat)
ys[c(120),'cluster'] = 2

plot(ys$long,ys$lat,col = col_gu[ys$cluster])
View(ys)

write.csv(ys,"youngsan.csv",row.names = F)

####################################################



############ 도봉구 수정 과정 #####################

db <- read.csv("dobong.csv")

identify(db$long, db$lat)
db[c(),'cluster'] = 2

plot(db$long,db$lat,col = col_gu[db$cluster])
#db[db[1] == 10605   ,"cluster"] = 3
#dobong <- db[c(order(db$long)),]
#View(dobong)

write.csv(db,"dobong.csv",row.names=F)

####################################################



############## 영등포 수정 과정 ###################

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