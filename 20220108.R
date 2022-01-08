setwd("d:/")
getwd()
install.packages("ggplot2")
library(ggplot2)
ks <- read.csv("9.csv", colClasses = c(rep("factor", 6), rep("numeric", 3)))
head(ks)

P2_0 <- ggplot(data = ks,mapping = aes(x = 풍속))
P2_1 <- P2_0 + geom_histogram(breaks=seq(0,25,0.5))
P2_2 <- P2_1 + facet_wrap(~도시, ncol = 3)

install.packages("dplyr")
library(dplyr)
wind_MeanSD_city = summarise(group_by(ks, 도시),
                             Mean = mean(풍속), SD = sd(풍속))
wind_MeanSD_city

P2_3 =  P2_1 + facet_grid(도시~계절)
wind_MeanSD_cityseason = summarise(group_by(ks, 도시,계절),
                             Mean = mean(풍속), SD = sd(풍속))

str(ks)
# 연속형 데이터터
ks$월 <- factor(ks$월, levels = as.character(1:12))
#비 만 포함한 데이터프레임 작성
ks_rain = ks %>% filter(날씨=="비")

#1월 ~12월까지 비가 내일 날의 수
str(ks_rain$월)
ks_rain['num_month'] = as.integer(ks_rain$월)
ks_rain = ks_rain[order(ks_rain$num_month),]
ks_rain$월
rownames(ks_rain) = 1:nrow(ks_rain)
head(ks_rain,3)
tail(ks_rain,3)



p3_0 =  ggplot(data=ks_rain, mapping = aes(x=월))
P3_1 =  p3_0 + geom_line(aes(group=1),stat = 'count')
P3_2 =  P3_1 + geom_point(aes(group=1),stat = 'count')

#1월부터 12월까지 비가 내린 날의 수를 꺾은 선 그래프로 그리기（도시별）
(P3_3 <- P3_2 + facet_grid(. ~ 도시))

#1월부터 12월까지 평균강수량의 꺾은 선 그래프 그리기
P4_0 = ggplot(data=ks, mapping = aes(x=월,y=강수량))
(P4_1 = P4_0 + stat_summary(aes(group = 1), fun=mean, geom = 'line'))
(P4_2 = P4_1 + stat_summary(aes(group = 1), fun=mean, geom = 'point'))

#1월부터 12월까지 평균강수량의 꺾은 선 그래프 그리기（도시별）
(P4_3 = P4_2 + facet_grid(. ~도시))


#2년치의 기온 데이터로 조정  2014 2015
install.packages("tidyr")
library(tidyr)

ks_temp =  ks %>% filter(년 == "2014" | 년 == "2015") %>% 
  select(도시,월,일,년,계절,기온) %>% 
  spread(key=년,value = 기온, sep = "") %>% 
  rename(기온2014 = 년2014, 기온2015=년2015)

ks_temp %>% head()
#산점도 그리기
P5_0 = ggplot(data = ks_temp, mapping=aes(x=기온2014,y=기온2015))
(P5_1 = P5_0 + geom_point())

#산점도 그리기(도시별)
(P5_2 = P5_1 + facet_wrap(~도시))

#산점도 그리기（도시와 계절별）
(P5_3 = P5_1 + facet_grid(도시 ~ 계절))

P6_0 = ks %>% ggplot(mapping = aes(x=날씨))
(P6_1 = P6_0+geom_bar(aes(fill = 계절),position='stack'))
(P6_2 = P6_0+geom_bar(aes(fill = 계절),position='fill'))
(P6_3 = P6_0+geom_bar(aes(fill = 계절),position='dodge'))
(P6_4 = P6_0+geom_bar(aes(fill = 계절),position='identity'))

#막대 색상 변경(매핑 아님)
(P6_5 = P6_0 + geom_bar(fill='red'))

#선의 색과 선의 종류 매핑
(P6_6 = P6_0 + geom_line(aes(group = 도시,color=도시),stat="count") +
  geom_point(aes(group = 도시,color=도시),stat="count"))

#점의 색과 점의 종류 매핑
P7_0 = ggplot(data=ks_temp,mapping = aes(x=기온2014,y=기온2015))
(P7_1 = P7_0 + geom_point(aes(color = 계절, shape=계절)))

#축에 관한 설정
P8_0 <- ggplot(data = ks, mapping = aes(x = 날씨)) + 
  geom_bar(aes(fill = 계절))
P8_1 <- P8_0 + 
  scale_y_continuous(limits = c(0,6000), breaks = seq(0, 6000, 1000))
P8_2 <- P8_1 + labs(x = "날씨의 종류", y = "도수")
(P8_3 <- P8_2 + 
    theme(axis.text.x = element_text(size = 15), 
          axis.title.y = element_text(size = 20)))

#범례에 관한 설정
keys <- c("봄", "여름", "가을", "겨울")
mycolor <- c("plum", "tomato", "wheat", "lemonchiffon")
names(mycolor) <- keys
P8_4 <- P8_3 + scale_fill_manual(values = mycolor)
P8_5 <- P8_4 + theme(legend.position = "bottom")
P8_6 <- P8_5 + labs(fill = "사계절")
(P8_7 <- P8_6 + guides(fill = guide_legend(nrow = 1, byrow = TRUE)))

#집계 데이터로부터의 막대그래프
ks_bar = ks %>% group_by(계절,날씨) %>% summarise(도수 = n()) %>% 
  complete(계절,날씨, fill=list(도수=0)) %>% 
  as.data.frame() 

(P9_0 <- ggplot(ks_bar, aes(x = 날씨, y = 도수)) + 
    geom_bar(aes(fill = 계절), stat = "identity"))

#집계 데이터로부터의 꺾은 선 그래프
ks_line <- ks %>% 
  group_by(월, 도시) %>% 
  summarise(평균강수량 = mean(강수량)) %>% 
  as.data.frame()
P9_1 <- ggplot(ks_line, aes(x = 월, y = 평균강수량))
P9_2 <- P9_1 + geom_line(aes(group = 도시, color = 도시, 
                             linetype = 도시), stat = "identity")
(P9_3 <- P9_2 + geom_point(aes(group = 도시, color = 도시), 
                           stat = "identity"))

#문자정보를 부가한다（geom_text）
P10_1 <- ggplot(data = ks,mapping = aes(x = 날씨)) + 
  geom_bar()
(P10_2 <- P10_1 + geom_text(aes(label = ..count..), 
                            stat = "count", vjust = -0.5))

#분포의 모양을 알아본다（geom_density，geom_vline）
ks_mean_temp <- ks %>% 
  group_by(계절,도시) %>% 
  summarise(평균기온 = mean(기온)) %>% 
  as.data.frame()
P10_3 <- ggplot(data = ks, mapping = aes(x = 기온)) + 
  geom_density(aes(linetype = 계절, color = 계절))
P10_4 <- P10_3 + geom_vline(data = ks_mean_temp, 
                            aes(xintercept = 평균기온, color = 계절), 
                            linetype = "twodash") #linetype의 다른 종류는"solid","longdash","dotted","dotdash","dashed","blank"
(P10_5 <- P10_4 + facet_wrap( ~ 도시))

#데이터의 산포도를 상세하게 조사한다（geom_jitter）
P10_6 <- ggplot(data = ks,mapping = aes(x = 도시, y = 풍속)) + 
  geom_jitter(aes(color = 계절, group = 계절), 
              position = position_jitterdodge(dodge.width = 0.6), 
              alpha = 1 / 5) #position_jitterdodge(dodge.width = 0.6)로 점의 산포도 폭을 지정
(P10_7 <- P10_6 + stat_summary(aes(x = 도시, y = 풍속, group = 계절), 
     color = "white", 
     fun = median, geom = "point", 
     shape = 4, 
     position = position_dodge(width = 0.6))) #position_dodge(width = 0.6)로 dodge의 폭을 지정

