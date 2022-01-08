setwd('d:/')
ks_original =  read.csv('covid_19_center.csv')
colnames(ks_original)
seq(2,ncol(ks_original))
ks =  ks_original[,seq(2,ncol(ks_original))]
ks$센터유형 = as.factor(ks$센터유형)
ks %>% str()
#막대그래프 그리기
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)

table(ks$센터유형)
P1_0 = ggplot(data=ks,mapping = aes(x=센터유형))
(P1_1 = P1_0 + geom_bar())

table(ks$센터명)
P2_0 = ggplot(data=ks,mapping = aes(x=센터명))
(P2_1 = P2_0 + geom_bar())

install.packages("stringr")
library(stringr)

customSplitArea = function(x){
  return(str_split(x," ")[[1]][2])
}
customSplitCity = function(x){
  return(str_split(x," ")[[1]][3])
}

customSplitArea(ks$센터명[13])

ks$area =  apply(ks['센터명'],1,customSplitArea)
ks$city = apply(ks['센터명'],1,customSplitCity)
ks %>% head()

seq(5,nrow(ks))

ks2 =  ks[seq(5,nrow(ks)),]
str(ks2)

ks2$area = as.factor(ks2$area)
ks2$city = as.factor(ks2$city)

#지역별 접종센터 현황
P3_0 = ggplot(data = ks2, mapping = aes(x = area))
(P3_1 = P3_0 + geom_bar())

#지역별 도시별 접종센터 현황 
install.packages("tidyr")
library(tidyr)
ks2_bar =  group_by(ks2,area,city) %>% summarise(count = n()) %>% 
  as.data.frame()

ggplot(data = ks2_bar, mapping = aes(x = area)) +
  geom_bar() 

temp02 = ks2_bar %>% filter(area == "경기도")


ggplot(data = temp02, mapping = aes(x = city)) +
  geom_bar() 



