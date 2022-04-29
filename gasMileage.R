install.packages('dplyr')
library(dplyr)
library(ggplot2)

displ_under4<-mpg %>% filter(displ<=4)
displ_over5<-mpg %>% filter(displ>=5)
mean(displ_under4$hwy)
mean(displ_over5$hwy) 

cty_A<-mpg %>% filter(manufacturer=='audi') 
cty_T<-mpg %>% filter(manufacturer=='toyota')
mean(cty_A$cty)
mean(cty_T$cty)

mpg_hwy<-mpg %>% filter(manufacturer %in% c('chevrolet','ford','honda'))
mean(mpg_hwy$hwy)


new_mpg<-mpg %>% mutate(mean_ch=(cty+hwy)/2) %>% arrange(desc(mean_ch)) %>% 
  head(3)


mpg %>% group_by(class) %>% summarise(mean_cty=mean(cty))

mpg %>% group_by(class) %>% summarise(mean_cty=mean(cty)) %>% 
  arrange(desc(mean_cty)) 

mpg_cty<-mpg %>% group_by(class) %>% summarise(mean_cty=mean(cty)) %>% 
  arrange(desc(mean_cty)) 
attach(mpg_cty)
barplot(names.arg = class[1:7],mean_cty,main='mpg_cty',xlab = 'Class',
  ylab = 'Mean_cty')

mpg3<-mpg %>% group_by(manufacturer) %>% summarise(hwy3=mean(hwy)) %>% 
  arrange(desc(hwy3)) %>% head(3)

mpg_compact<-mpg %>%  filter(class=='compact') %>% group_by(manufacturer) %>%
  summarise(compact=sum(class=='compact')) %>% arrange(desc(compact))

fl_data <- data.frame(fl = c('c', 'd', 'e', 'p', 'r'),
  price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22), stringsAsFactors = T)

p_mpg<-merge(mpg,fl_data,by='fl')
#or p_mpg<-mpg %>% mutate(price_fl=ifelse(fl=='c',2.35,ifelse(fl=='d',2.38,
  #ifelse(fl=='e',2.11,ifelse(fl=='p',2.76,2.22)))))
View(p_mpg)


