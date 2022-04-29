##01-1
install.packages("ggplot2")
library("ggplot2")
g1.1<-ggplot(data = mpg, aes(x=cty,y=hwy))+geom_point()
g1.1
##01-2
g1.2.1<-ggplot(data = midwest, aes(x=poptotal,y=popasian))+geom_point()
g1.2.1
g1.2.1<-ggplot(data = midwest, aes(x=poptotal,y=popasian))+geom_point()+
  xlim(0,500000)+ylim(0,10000)
g1.2.1
##01-3
attach(midwest)
cor(poptotal,popasian)
##02-1
install.packages("dplyr")
library(dplyr)
library("ggplot2")
g2.1<-as.data.frame(ggplot2::mpg)
g2.1<-g2.1 %>% filter(class=='suv') %>% group_by(manufacturer) %>% 
  summarise(mean_cty = mean(cty)) %>% arrange(desc(mean_cty)) %>% 
  head(5)
g2.11<-ggplot(data = g2.1,aes(x=manufacturer,y=mean_cty))+geom_bar(stat='identity')
g2.11
##02-2
mpg1<-ggplot(mpg,aes(x=class))+geom_bar()
mpg1
##03-1
economics1<-ggplot(economics,aes(x=date,y=psavert))+geom_line()
economics1
##03-2
mpg2<-mpg %>% filter(class %in% c('compact','subcompact','suv'))
mpg2<-ggplot(mpg2,aes(x=class,y=cty))+geom_boxplot()
mpg2

##Part2
##01
install.packages("foreign")
library(foreign)
welfare<-read.spss('Koweps_hpc10_2015_beta1.sav',to.data.frame = T)
welfare<-rename(welfare,gender=h10_g3,birth=h10_g4,marriage=h10_g10,
 religion=h10_g11,income=p1002_8aq1,code_job=h10_eco9,code_region=h10_reg7)
welfare
##02-1
sum(is.na(welfare$gender))
##02-2
welfare$gender[welfare$gender==1]<-'male'
welfare$gender[welfare$gender==2]<-'female'
welfare
##02-3
library(ggplot2)
as.factor(welfare$gender)
gdgraph<-ggplot(welfare,aes(x=welfare$gender))+geom_bar()
gdgraph
##02-4
welfare$income[welfare$income==0.0]<-NA
welfare$income[welfare$income==9999.0]<-NA
welfare
##02-5
sum(is.na(welfare$income))
##02-6
incomegp<-ggplot(welfare,aes(x=income))+geom_histogram(binwidth = 50,
  aes(y=..density..,fill=..count..))+xlim(0,1000)+geom_density(color='red')
incomegp
##02-7
icbygd<-welfare %>% filter(!is.na(income)) %>% group_by(gender) %>% 
  summarise(mean_income=mean(income))
icbygd
##02-8
icbygdgp<-ggplot(icbygd,aes(x=gender,y=mean_income))+
  geom_col()
icbygdgp
##02-9
install.packages("nortest")
library(nortest)
attach(welfare)
shapiro.test(income)
#정규성 검정 결과: 귀무가설(데이터가 정규분포를 따른다)기각.  
var.test(income~gender)
##남자와 여자의 월급의 분산은 유의 수준1%에서 동일하지 않다.
t.test(income~gender)
##남자와 여자의 월급평균은 유의수준1%에서 다르다고 할 수있지만 월급의 정규성
#검정에서 귀무가설이 기각 되었으므로 위 표본의 모집단은 정규분표를 따르지 않는다.  .
##02-10
GIgp<-welfare %>% filter(!is.na(income))
GIgp<-ggplot(GIgp,aes(x=income,fill=gender))+geom_histogram()
GIgp
##03-1
welfare2<-welfare %>% mutate(region=ifelse(code_region==1,'서울', 
  ifelse(code_region==2,'수도권(인천/경기)','지방권')))
welfare2
##03-2
#세 지역의 월급평균은 동일하다 vs  적어도 한개의 지역의 월급평균은 다르다.
as.factor(welfare2$region)
shapiro.test(welfare2$income)
#정규성 검정 결과: 귀무가설(데이터가 정규분포를 따른다)기각. 
install.packages('car')
library('car')
as.factor(welfare2$region)
leveneTest(welfare2$income~welfare2$region)
#등분산성 결과 귀무가설 채택(세 지역의 월급의 분산은 유의 수준5%에서 동일하다.) 
a1<-aov(welfare2$income~welfare2$region)
anova(a1)
#p-value=0.2586
#세 지역의 월급평균은 유의 수준 5%에서 동일하지만 정규성 검정에 의해
#데이터가 정규분포를 따르지 않으므로 분산분석의 결과는 무의미하다. 
