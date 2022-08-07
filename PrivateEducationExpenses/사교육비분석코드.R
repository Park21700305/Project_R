#R_01분반_16조_
#21700305/박지성
#22100421/심다영

#1차 수정 데이터 불러오기.
file<-data.frame(read_xlsx("RRRRRR.xlsx",col_names=T, na='NA'))
#2015년도~2019년도 데이터 추출.
file4<-file %>% select(-c(2:31))
#평균,초등학교,중학교,일반고등학교 데이터 추출.    
file4<-file4 %>% select(-c(5,10,15,20,25))

#연도에 따라 사교육비의 변화를 그리기 위해 데이터 수정.
year_15<-c(file4[1,2:5])
year_16<-c(file4[1,6:9])
year_17<-c(file4[1,10:13])
year_18<-c(file4[1,14:17])
year_19<-c(file4[1,18:21])
class<-c("평균","초등","중등","고등")
#행 번호를 위한 작업.
names(year_15)<-c(1,2,3,4)
names(year_16)<-c(1,2,3,4)
names(year_17)<-c(1,2,3,4)
names(year_18)<-c(1,2,3,4)
names(year_19)<-c(1,2,3,4)
#학교급과 연도에 따른 사교육비 변수로 이루어진 표 구성.
b<-cbind(class,year_15,year_16,year_17,year_18,year_19)
b<-as.data.frame(b)
View(b)
#사교육비 변수를 위한 작업.
cost15<-b %>% select(year_15)
cost16<-b %>% select(year_16)
cost17<-b %>% select(year_17)
cost18<-b %>% select(year_18)
cost19<-b %>% select(year_19)
names(cost16)<-names(cost15)
names(cost17)<-names(cost15)
names(cost18)<-names(cost15)
names(cost19)<-names(cost15)
eee<-rbind(cost15,cost16,cost17,cost18,cost19)
eee<-rename(eee,cost=year_15)
#최종: 연도에 따른 초,중,고,평균 사교육비비교 그래프를 위한 프레임작업.
year<-as.numeric(rep(c("2015","2016","2017","2018","2019"),each=4))
grade<-rep(c("average","elementry","middle","high"),5)
for_line<-cbind(year,grade,eee)
for_line<-as.data.frame(for_line,stringAsFactors=T)
for_line$grade<-as.factor(for_line$grade)
for_line$cost<-as.numeric(for_line$cost)
#행 번호 순서 맞추기.
rownames(for_line)=c(1:20)
summary(for_line)
View(for_line)


#plot 한글깨짐 해결책.
par(family='Arial')
#학생 1인당 월평균 사교육비 line_graph
linegraph<-ggplot(for_line,aes(x=year,y=cost,linetype=grade,shape=grade))+
geom_line(aes(color=grade),size=1) + geom_point(aes(color=grade,shape=grade),
size=3) + ggtitle("학생 1인당 월평균 사교육비\n(단위:전국)") +
xlab("년도\n(2015~2019)") + ylab("(단위:만원)") +
theme(plot.title = element_text(hjust = 0.5, face = 'bold',
size = "15", color = "#330066" ), axis.title = element_text(face = 'bold',
color = "#330066",size = 13),axis.text.y=element_text(size = 12)) +
theme(legend.title = element_text(face = "bold",
size = 13, color = "darkblue")) + theme(legend.text = element_text(face = "bold", 
size = 11, color = "#330066")) + theme(legend.key = element_rect(color = "red",
fill = "white"), legend.key.size = unit(1,"cm")) +
theme(axis.text=element_text(face = 'bold'))
linegraph

for_col<-for_line
#평균,초,중,고 순으로 출력하기 위한 작업.
for_col$grade2 <- factor(for_col$grade, levels=c('average','elementry','middle','high'))
#연도를 요인형 변수로 변형. 
for_col$year<-as.factor(for_col$year)
#학생 1인당 월평균 사교육비 col_graph
colgraph<-ggplot(for_col,aes(x=year,y=cost,fill=year))+geom_col(colour="black")+
facet_grid(.~grade2)+ theme(axis.text=element_text(angle=45,face = 'bold'),
axis.text.y = element_text(size = 12) )+
theme(legend.text = element_text(face = 'bold',size = 11))
colgraph

#학교급별 1인당 월평균 사교육비 비교.
colgraph2<-ggplot(for_col,aes(x=year,y=cost,fill=grade2)) + 
geom_col(colour='black',position = 'dodge', width = 0.7) + 
ggtitle("학교급별 월평균 사교육비\n(단위:전국)") + ylab("(단위:만원)")+
theme(plot.title = element_text(hjust = 0.5, face = 'bold',size = "15", 
color = "#330066"),axis.text=element_text(face='bold'), axis.text.y=element_text(size = 12))+
theme(legend.text = element_text(face = 'bold',size = 11))
colgraph2

#1번 그래프 
bar_cost<-rbind(file[1,2],file[1,7],file[1,12],file[1,17],file[1,22],file[1,27],
file[1,32],file[1,37],file[1,42],file[1,47],file[1,52])
bar_year<-c("2009","2010","2011","2012","2013","2014","2015","2016","2017",
"2018","2019")
for_bar<-as.data.frame(cbind(bar_year,bar_cost))
for_bar<-rename(for_bar,bar_cost=V2)
for_bar$bar_cost<-as.numeric(for_bar$bar_cost)

bargraph<-ggplot(for_bar,aes(x=bar_year,y=bar_cost))+
geom_col(fill='Darkblue',colour="black",
width = 0.5)+ ggtitle("[전체학생 1인당 월평균 사교육비]")+
ylab("(단위:만원)")+xlab("2009~2019")+
theme(plot.title = element_text(hjust = 0.5,face = 'bold',
size ='15'),axis.text.x = element_text(size='13',face = 'bold',angle = 45),
axis.text.y = element_text(face = 'bold', size='14'))
bargraph

# 3번 그래프
# 패키지 다운로드
install.packages('tidyverse')
install.packages('stringi')
install.packages('devtools')
devtools::install_github('cardiomoon/kormaps2014',force=T)

library(tidyverse)
library(stringi)
library(kormaps2014)

install.packages('ggiraphExtra')
install.packages('maps')
install.packages('mapproj')

library(ggiraphExtra)
library(maps)
library(mapproj)

install.packages('readxl')
library(readxl)

# 한글 폰트 가져오기
install.packages("extrafont")
library(extrafont)
font_import()


# 데이터 불러오기
RR <- data.frame(read_xlsx("RRRRRR.xlsx",col_names=T,na='NA'))
row.names(RR) <- c('전체','서울','부산','대구','인천','광주','대전','울산','세종','경기',
                   '강원','충북','충남','전북','전남','경북','경남','제주')

# 2015-2019년도만 가져오기
RR15_19 = RR[,32:56]
View(RR15_19)

# kormap 데이터 프로세싱
str(changeCode(kormap1))
str(korpop1)
korpop1 <- rename(korpop1, pop=총인구_명, area=행정구역별_읍면동)
korpop1$pop <- as.numeric(korpop1$pop)


# 학급별로 지역별 총합 구하여 그래프 그릴 데이터프레임 만들기
total_ele <- (RR15_19[2]+RR15_19[7]+RR15_19[12]+RR15_19[17]+RR15_19[22])/5
total_mid <- (RR15_19[3]+RR15_19[8]+RR15_19[13]+RR15_19[18]+RR15_19[23])/5
total_high <- (RR15_19[5]+RR15_19[10]+RR15_19[15]+RR15_19[20]+RR15_19[25])/5
total_all <- (total_ele+total_mid+total_high)/3
area <- c('전체','서울','부산','대구','인천','광주','대전','울산','세종','경기',
          '강원','충북','충남','전북','전남','경북','경남','제주')
code <- c('','11','21','22','23','24','25','26','29','31','32','33','34',
          '35','36','37','38','39')

total <- data.frame(total_ele,total_mid,total_high,total_all,area,code)
total <- total[-1,]
names(total) <- c('Elementry', 'Midschool','Highschool','All_mean','area','code')


# 그래프 그리기
# 초등학교
ggplot(total, aes(map_id=code, fill=Elementry))+
  geom_map(map=kormap1,colour="black",size=0.1)+
  expand_limits(x=kormap1$long, y=kormap1$lat)+
  scale_fill_gradientn(colours = c("white","orange","red"))+
  ggtitle("2015-2019년 지역별 초등학교 월평균 사교육비")+
  theme(text=element_text(size=9,family='NanumGothic'))+
  coord_map()

# 중학교
ggplot(total, aes(map_id=code, fill=Midschool))+
  geom_map(map=kormap1,colour="black",size=0.1)+
  expand_limits(x=kormap1$long, y=kormap1$lat)+
  scale_fill_gradientn(colours = c("white","orange","red"))+
  ggtitle("2015-2019년 지역별 중학교 월평균 사교육비")+
  theme(text=element_text(size=9,family='NanumGothic'))+
  coord_map()

# 고등학교
ggplot(total, aes(map_id=code, fill=Highschool))+
  geom_map(map=kormap1,colour="black",size=0.1)+
  expand_limits(x=kormap1$long, y=kormap1$lat)+
  scale_fill_gradientn(colours = c("white","orange","red"))+
  ggtitle("2015-2019년 지역별 고등학교 월평균 사교육비")+
  theme(text=element_text(size=9,family='NanumGothic'))+
  coord_map()

# 전체 평균
ggplot(total, aes(map_id=code, fill=All_mean))+
  geom_map(map=kormap1,colour="black",size=0.1)+
  expand_limits(x=kormap1$long, y=kormap1$lat)+
  scale_fill_gradientn(colours = c("white","orange","red"))+
  ggtitle("2015-2019년 지역별 초중고 전체 월평균 사교육비")+
  theme(text=element_text(size=9,family='NanumGothic'))+
  coord_map()


# 1번 그래프
# 최근 5년 전체 사교육비 비교 초중고 학교 모든 전체를 다 합하여

# 전체 부분만 나타내기
mean_RR <- RR15_19 %>% filter(row.names(RR15_19)=='전체')
mean_RR <- mean_RR[grep('^mean',names(mean_RR))]
mean_RR

names(mean_RR) <- c('2015','2016','2017','2018','2019')
row.names(mean_RR) <- c('Total')
t_mean_RR <- data.frame(t(mean_RR))
View(t_mean_RR)


ggplot(data=t_mean_RR,aes(x=row.names(t_mean_RR),y=Total))+geom_bar(stat='identity',fill='darkblue')+
  xlab('Year')+ggtitle('2015-2019년도 5년간 전체 학생 1인당 월평균 사교육비')+
  theme(text=element_text(size=9,family='NanumGothic'))


