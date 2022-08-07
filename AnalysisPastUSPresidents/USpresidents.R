# install.packages("tidyverse")
# install.packages("tidytext")
# install.packages("glue")
# install.packages("stringr")
# install.packages("SnowballC")
# install.packages("textdata")
# library(glue)
# library(tidyverse)
# library(tidytext)
# library(stringr)
# library(SnowballC)
# library(textdata)
# install.packages("multilinguer")
# library(multilinguer)
# install_jdk()
# install.packages(c('hash','tau','Sejong','RSQLite','devtools'),type = 'binary')
# install.packages('remotes')
# remotes::install_github('haven-jeon/KoNLP',upgrade = 'never',INSTALL_opts=c(--no-multiarch))
# library(stringr)
# library(hash)
# library(tau)
# library(RSQLite)
# library(rJava)
# library(memoise)
# library(dplyr)
# library(ggplot2)
# install.packages("wordcloud")
# library(wordcloud)
# library(RColorBrewer)
# install.packages('tm')
# library(tm)
# library(SnowballC)
# library(devtools)
# devtools::install_github("lchiffon/wordcloud2")
# library(wordcloud2)

#1-1_Bush.Sr
Bush<-readLines("./hw3_21700305/input/Bush_1989.txt",encoding = "UTF-8")
Bush<-Corpus(VectorSource(Bush))
Bush<-tm_map(Bush,stripWhitespace)
Bush<-tm_map(Bush,tolower)
Bush<-tm_map(Bush,removePunctuation)
Bush<-tm_map(Bush,removeNumbers)
Bush<-tm_map(Bush,removeWords,stopwords("english"))
Bush_table<-TermDocumentMatrix(Bush)
Bush_table<-as.matrix(Bush_table)
Bush_v<-sort(rowSums(Bush_table),decreasing = T)
Bush_df<-data.frame(word=names(Bush_v),freq=Bush_v)
Bush_df

wordcloud2(data = Bush_df,
           size = 0.5,
           minRotation = -pi/6,
           rotateRatio = 0.6,
           color = "black",
           figPath ="./hw3_21700305/Photos/Bush1.png")

#1-2_Bush2
Bush2<-readLines("./hw3_21700305/input/Bush_2001.txt",encoding = "UTF-8")
Bush2<-Corpus(VectorSource(Bush2))
Bush2<-tm_map(Bush2,stripWhitespace)
Bush2<-tm_map(Bush2,tolower)
Bush2<-tm_map(Bush2,removePunctuation)
Bush2<-tm_map(Bush2,removeNumbers)
Bush2<-tm_map(Bush2,removeWords,stopwords("english"))
Bush2_table<-TermDocumentMatrix(Bush2)
Bush2_table<-as.matrix(Bush2_table)
Bush2_v<-sort(rowSums(Bush2_table),decreasing = T)
Bush2_df<-data.frame(word=names(Bush2_v),freq=Bush2_v)
Bush2_df

wordcloud2(data = Bush2_df,
           size = 0.5,
           minRotation = -pi/6,
           rotateRatio = 0.6,
           color = "black",
           figPath ="./hw3_21700305/Photos/Bush2.png")

#1-3_Clinton
Clinton<-readLines("./hw3_21700305/input/Clinton_1993.txt",encoding = "UTF-8")
Clinton<-Corpus(VectorSource(Clinton))
Clinton<-tm_map(Clinton,stripWhitespace)
Clinton<-tm_map(Clinton,tolower)
Clinton<-tm_map(Clinton,removePunctuation)
Clinton<-tm_map(Clinton,removeNumbers)
Clinton<-tm_map(Clinton,removeWords,stopwords("english"))
Clinton_table<-TermDocumentMatrix(Clinton)
Clinton_table<-as.matrix(Clinton_table)
Clinton_v<-sort(rowSums(Clinton_table),decreasing = T)
Clinton_df<-data.frame(word=names(Clinton_v),freq=Clinton_v)
Clinton_df

wordcloud2(data = Clinton_df,
           size = 0.5,
           minRotation = -pi/6,
           rotateRatio = 0.6,
           color = "black",
           figPath ="./hw3_21700305/Photos/Clinton.png")

#1-4_Obama
Obama<-readLines("./hw3_21700305/input/Obama_2009.txt",encoding = "UTF-8")
Obama<-Corpus(VectorSource(Obama))
Obama<-tm_map(Obama,stripWhitespace)
Obama<-tm_map(Obama,tolower)
Obama<-tm_map(Obama,removePunctuation)
Obama<-tm_map(Obama,removeNumbers)
Obama<-tm_map(Obama,removeWords,stopwords("english"))
Obama_table<-TermDocumentMatrix(Obama)
Obama_table<-as.matrix(Obama_table)
Obama_v<-sort(rowSums(Obama_table),decreasing = T)
Obama_df<-data.frame(word=names(Obama_v),freq=Obama_v)
Obama_df

wordcloud2(data = Obama_df,
           size = 0.5,
           minRotation = -pi/6,
           rotateRatio = 0.6,
           color = "black",
           figPath ="./hw3_21700305/Photos/Obama.png")

#1-5_Trump
Trump<-readLines("./hw3_21700305/input/Trump_2017.txt",encoding = "UTF-8")
Trump<-Corpus(VectorSource(Trump))
Trump<-tm_map(Trump,stripWhitespace)
Trump<-tm_map(Trump,tolower)
Trump<-tm_map(Trump,removePunctuation)
Trump<-tm_map(Trump,removeNumbers)
Trump<-tm_map(Trump,removeWords,stopwords("english"))
Trump_table<-TermDocumentMatrix(Trump)
Trump_table<-as.matrix(Trump_table)
Trump_v<-sort(rowSums(Trump_table),decreasing = T)
Trump_df<-data.frame(word=names(Trump_v),freq=Trump_v)
Trump_df

wordcloud2(data = Trump_df,
           size = 0.5,
           minRotation = -pi/6,
           rotateRatio = 0.6,
           color = "black",
           figPath ="./hw3_21700305/Photos/Trump.png")
#2-1민주당
Demo_df<-data.frame()
Demo_df<-Clinton_df
Demo_df<-rbind(Demo_df,Obama_df)
Demo_df
wordcloud2(data = Demo_df,
           size = 0.5,
           minRotation = -pi/6,
           rotateRatio = 0.6,
           color = c("red","blue"),
           figPath ="./hw3_21700305/Photos/Dem.png")

#2-2공화당
Rep_df<-data.frame()
Rep_df<-Bush_df
Rep_df<-rbind(Rep_df,Bush2_df,Trump_df)
wordcloud2(data = Rep_df,
           size = 0.5,
           minRotation = -pi/6,
           rotateRatio = 0.6,
           color = c("red","blue"),
           figPath ="./hw3_21700305/Photos/Rep.png")


#3 5명의 대통령 함수
GetSentiment<-function(file)
{
  fileName<-glue("./hw3_21700305/input/",file,sep="")
  fileName<-trimws(fileName)
  fileText<-readLines(fileName)
  tokens<-tibble(text=fileText) %>% unnest_tokens(word,text)
  sentiment<-tokens %>% inner_join(get_sentiments("nrc")) %>% count(sentiment) %>% 
    spread(sentiment,n,fill = 0) %>% 
    mutate(year=as.numeric(str_match(file,"\\d{4}"))) %>% 
    mutate(president=str_match(file,"(.*)_")[2])
  return(sentiment)
}
files<-list.files("./hw3_21700305/input")
sentiments<-data.frame()
for (i in files){
  sentiments<-rbind(sentiments,GetSentiment(i))
}
sentiments<-sentiments %>% mutate(president=ifelse(president=='Bush'&year<2000,
    "Bush.Sr.",president))
#3-1
Graph_anger<-ggplot(sentiments,aes(x=president,y=anger))+geom_boxplot(aes(color=president))
Graph_anger
#3-2
Graph_anti<-ggplot(sentiments,aes(x=president,y=anticipation))+geom_boxplot(aes(color=president))
Graph_anti
#3-3
Graph_disg<-ggplot(sentiments,aes(x=president,y=disgust))+geom_boxplot(aes(color=president))
Graph_disg
#3-4
Graph_fear<-ggplot(sentiments,aes(x=president,y=fear))+geom_boxplot(aes(color=president))
Graph_fear
#3-5
Graph_joy<-ggplot(sentiments,aes(x=president,y=joy))+geom_boxplot(aes(color=president))
Graph_joy
#3-6
Graph_neg<-ggplot(sentiments,aes(x=president,y=negative))+geom_boxplot(aes(color=president))
Graph_neg
#3-7
Graph_pos<-ggplot(sentiments,aes(x=president,y=positive))+geom_boxplot(aes(color=president))
Graph_pos
#3-8
Graph_sad<-ggplot(sentiments,aes(x=president,y=sadness))+geom_boxplot(aes(color=president))
Graph_sad
#3-9
Graph_surp<-ggplot(sentiments,aes(x=president,y=surprise))+geom_boxplot(aes(color=president))
Graph_surp
#3-10
Graph_trust<-ggplot(sentiments,aes(x=president,y=trust))+geom_boxplot(aes(color=president))
Graph_trust

#4
GetSentiment2<-function(file)
{
  fileName2<-glue("./hw3_21700305/input/",file,sep="")
  fileName2<-trimws(fileName2)
  fileText2<-readLines(fileName2)
  tokens2<-fileText2 %>% tibble(text=fileText2) %>% unnest_tokens(word,text)
  sentiment2<-tokens2 %>% inner_join(get_sentiments("afinn")) 
   mean<-mean(sentiment2$value) 
   year<-as.numeric(str_match(file,"\\d{4}")) 
   president5<-str_match(file,"(.*)_")[2] 
   result<-cbind(mean,year,president5)
  return(result)
}

files<-list.files("./hw3_21700305/input")
sentiment2<-tibble()
for (i in files){
  sentiment2<-rbind(sentiment2,GetSentiment2(i))
}
sentiment2<-sentiment2 %>% mutate(president5=ifelse(president5=="Bush"&year<2000,
  "Bush.Sr.",president5))
sentiment2$mean<-as.numeric(sentiment2$mean)
sentiment2$president5<-as.factor(sentiment2$president5)
test<-sentiment2
sentiment2<-sentiment2 %>% group_by(president5) %>% summarise(avg=mean(mean))
#4-1
sentiment2    
Atest<-aov(mean~president5,data = test)
#4-2
anova(Atest)
#검정에 필요한 assumption이 모두 만족한다고 하였을때
# H0:다섯 명의 대통령 모두 감성점수 평균이 동일하다 vs H1: 적어도 한명의 대통령의 감성점수 평균이 다르다.
# 결론 p-value = 0.9134이므로 귀무가설(H0) 채택. 유의수준 5%에서 다섯 명의 대통령 감성점수는 동일하다고 할 수 있다.  

