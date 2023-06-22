rm(list = ls())


options(max.print=100)

library(dplyr)
library(effsize)
library(tidyverse)
library(sf)
library(mapproj)
library(gmodels)
library(e1071)
library(ggridges)

# == 데이터 전처리 과정 ==

setwd("C:/Users/ksdre/Desktop/r")
#setwd("C:/dataset")

data_2021 <- read.csv("data_2021.csv", header = TRUE, fileEncoding = "euc-kr")
data_2019 <- read.csv("data_2019_2.csv", header = TRUE, fileEncoding = "euc-kr")
#data <- read.csv("data2.csv", header = TRUE, fileEncoding = "euc-kr")


# 원활한 전처리를 위해 크기 10만의 샘플 추출
sampleData21 <- sample_n(data_2021, 100000)
sampleData19 <- sample_n(data_2019, 100000)

#시도코드 49(제주도: 2019에 없음),50(알 수 없음) 삭제
sampleData19 = subset(sampleData19, !(시도코드 == 49 | 시도코드 == 50| 시도코드 == "50"| 시도코드 == "49"))
sampleData21 = subset(sampleData21, !(시도코드 == 49 | 시도코드 == 50| 시도코드 == "50"| 시도코드 == "49"))


# 필요한 변수만 저장
sample_2021 <- sampleData21[,c(1, 3, 4, 5, 13, 14, 15)]
sample_2019 <- sampleData19[,c(1, 3, 4, 5, 13, 14, 15)]


#2019 연령대 5~8 제거
sample_2019 <- sample_2019[!(sample_2019$연령대.코드.5세단위. %in% c("5", "6", "7", "8")),]

sample_2021 <- sample_2021[!(sample_2021$연령대.코드.5세단위. %in% c("5", "6", "7", "8")),]


#결측치 확인 및 제거
colSums(is.na(sample_2021))
sample_2021 = na.omit(sample_2021)

colSums(is.na(sample_2019))
sample_2019 = na.omit(sample_2019)


#num타입을 factor타입으로 바꿔주기
sample_2021[,1:4] <- lapply(sample_2021[,1:4], as.factor)
sample_2019[,1:4] <- lapply(sample_2019[,1:4], as.factor)



# 성별 label
sample_2019$성별코드 <- factor(sample_2019$성별코드, levels = c("1", "2"), labels = c("Male", "Female"))
sample_2021$성별코드 <- factor(sample_2021$성별코드, levels = c("1", "2"), labels = c("Male", "Female"))

# 지역 label

시도 <- data.frame(시도코드 = c("11", "26", "27", "28", "29", "30", "31", "36", "41", "42", "43", "44", "45", "46", "47", "48"),
                 시도이름 = c("서울특별시", "부산광역시", "대구광역시", "인천광역시", "광주광역시", "대전광역시", "울산광역시", "세종특별자치시", "경기도", "강원도", "충청북도", "충청남도", "전라북도", "전라남도", "경상북도", "경상남도"))
sample_2019$시도코드 <- factor(sample_2019$시도코드, levels = 시도$시도코드, labels = 시도$시도이름)
sample_2021$시도코드 <- factor(sample_2021$시도코드, levels = 시도$시도코드, labels = 시도$시도이름)



#IQR방식으로 이상치 제거
for (i in 5:7) {
  q1 <- quantile(sample_2019[[i]], 0.25)
  q3 <- quantile(sample_2019[[i]], 0.75)
  iqr <- q3 - q1
  
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  
  sample_2019 <- sample_2019[sample_2019[[i]] >= lower_bound & sample_2019[[i]] <= upper_bound, ]
}

for (i in 5:7) {
  q1 <- quantile(sample_2021[[i]], 0.25)
  q3 <- quantile(sample_2021[[i]], 0.75)
  iqr <- q3 - q1
  
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  
  sample_2021 <- sample_2021[sample_2021[[i]] >= lower_bound & sample_2021[[i]] <= upper_bound, ]
}




#분석을 위한 최종 크기 10000의 샘플 추출
sample_2021 <- sample_n(sample_2021, 10000)
sample_2019 <- sample_n(sample_2019, 10000)


#전처리 후 데이터 요약
str(sample_2019)
summary(sample_2019)
summary(sample_2021)



#박스플롯
par(mfrow = c(1, 2))
boxplot(sample_2019[,5:7], main = "2019 혈압, 혈당 수치",ylim = c(50,165))
boxplot(sample_2021[,5:7],main = "2021 혈압, 혈당 수치",ylim = c(50,165))


#모자이크 차트=============

#혈당, 혈압, 나이 데이터 추출

sex19 <- sample_2019[,c(3)]
age19 <- sample_2019[,c(4)]
Hpressure19 <- sample_2019[,c(5)]
Lpressure19 <- sample_2019[,c(6)]
sugar19 <- sample_2019[,c(7)]

sex21 <- sample_2021[,c(3)]
age21 <- sample_2021[,c(4)]
Hpressure21 <- sample_2021[,c(5)]
Lpressure21 <- sample_2021[,c(6)]
sugar21 <- sample_2021[,c(7)]

sugurTrue19 <- c()
sugurTrue21 <- c()
pressureTrue19 <- c()
pressureTrue21 <- c()

#혈당> 100이면 공복혈당장애 진단
#수축기혈압 >135, 이완기 혈압 >95이면 고혈압 진단
#정상, 비정상을 구분하는 전처리

for (i in 1:10000){
  if (sugar19[i] >= 100){
    sugurTrue19[i] <- "true"
  }else{
    sugurTrue19[i] <- "false"
  }
  
  if (sugar21[i] >= 100){
    sugurTrue21[i] <- "true"
  }else{
    sugurTrue21[i] <- "false"
  }
  
  if (Hpressure19[i] >= 135 | Lpressure19[i] >= 95){
    pressureTrue19[i] <- "true"
  }else{
    pressureTrue19[i] <- "false"
  }
  
  if (Hpressure21[i] >= 135 | Lpressure21[i] >= 95){
    pressureTrue21[i] <- "true"
  }else{
    pressureTrue21[i] <- "false"
  }
}

df19 <- data.frame(age19, sugurTrue19, pressureTrue19)
df21 <- data.frame(age21, sugurTrue21, pressureTrue21)

table(df19)
table(df21)


#모자이크 플롯
par(mfrow = c(1, 2))

mosaicplot(~age19+sugurTrue19, data = df19, color=TRUE,            
           main ="19년도 연령별 공복혈당장애 여부 비교", xlab = "나이(5세 단위)" , ylab = "당뇨병 여부")

mosaicplot(~age21+sugurTrue21, data = df21, color=TRUE,            
           main ="21년도 연령별 공복혈당장애 여부 비교", xlab = "나이(5세 단위)" , ylab = "당뇨병 여부")


mosaicplot(~age19+pressureTrue19, data = df19, color=TRUE,            
           main ="19년도 연령별 고혈압 여부 비교", xlab = "나이(5세 단위)" , ylab = "고혈압 여부")

mosaicplot(~age21+pressureTrue21, data = df21, color=TRUE,            
           main ="21년도  연령별 고혈압 여부 비교", xlab = "나이(5세 단위)" , ylab = "고혈압 여부")

# ===barplot===
par(mfrow = c(1, 2))
x <- data.frame(table(pressureTrue19))
y <- data.frame(table(pressureTrue21))
barplot(x$Freq, main = "고혈압 진단결과 분석", names = c("False", "True"),col = "blue")
barplot(y$Freq, col = adjustcolor("red", alpha.f = 0.5),add= TRUE)

x
y
legend("topright", legend = c("2019", "2021"), col = c("blue", "red"), 
       fill = c("blue", "red"), bty = "n")


x <- data.frame(table(sugurTrue19))
y <- data.frame(table(sugurTrue21))
barplot(x$Freq, main = "공복혈당장애 진단결과 분석", names = c("False", "True"),col = "blue")
barplot(y$Freq, main = "2021 혈당",  col = adjustcolor("red", alpha.f = 0.5), add = TRUE)
x
y


#===dotchart===


groups <- c("2019", "2021")
par(new = FALSE)
#연령별 혈당값 true수
x <- tapply(sugurTrue19, sample_2019$연령대.코드.5세단위., function(x) sum(x == "true"))


y <- tapply(sugurTrue21, sample_2021$연령대.코드.5세단위., function(x) sum(x == "true"))

par(mfrow = c(1, 1))
dotchart(x, main = "연령별 공복혈당장애 진단 여부 비교", xlab = "공복혈당장애를 진단받은 인원 수", ylab = "연령대", 
         xlim = c(0, 750), col = "blue",pch=1)
par(new = TRUE)
dotchart(y,col = "red", xlim = c(0, 750), pch=0)

legend("topright", legend = groups, pch = c(1, 0), col = c("blue", "red"))


par(new = FALSE)
#연령별 고혈압 true수
x <- tapply(pressureTrue19, sample_2019$연령대.코드.5세단위., function(x) sum(x == "true"))

y <- tapply(pressureTrue21, sample_2021$연령대.코드.5세단위., function(x) sum(x == "true"))


dotchart(x, main = "연령별 고혈압 진단 여부 비교", xlab = "고혈압을 진단받은 인원 수", ylab = "연령대", 
         xlim = c(0, 450), pch=1, col = "blue")
par(new = TRUE)
dotchart(y,col = "red", xlim = c(0, 450), pch=0)
legend("topright", legend = groups, pch = c(1, 0), col = c("blue", "red"))
par(new = FALSE)


#연령별 혈당값 평균값

x <- tapply(sugar19, age19, mean)
y <- tapply(sugar21, age21, mean)

dotchart(x, main = "연령별 평균 혈당값", xlab = "평균 혈당", ylab = "연령대", xlim = c(95, 103), pch=1, col="blue")
par(new = TRUE)
dotchart(y, col = "red", xlim = c(95, 103), pch=0)
legend("topleft", legend = groups, pch = c(1, 0), col = c("blue", "red"))


#연령별 혈압값 평균값

x <- tapply(Hpressure19, age19, mean)
y <- tapply(Hpressure21, age21, mean)

dotchart(x, main = "연령별 평균 수축기 혈압값", xlab = "평균 수축기 혈압", ylab = "연령대", 
         xlim = c(118, 133), pch=1, col="blue")
par(new = TRUE)
dotchart(y, col = "red", xlim = c(118, 133), pch=0)
legend("topleft", legend = groups, pch = c(1, 0), col = c("blue", "red"))

par(new = FALSE)


x <- tapply(Lpressure19, age19, mean)
y <- tapply(Lpressure21, age21, mean)
dotchart(x, main = "연령별 평균 이완기 혈압값", xlab = "평균 이완기 혈압", ylab = "연령대", 
         xlim = c(73.5, 77), pch=1, col="blue")
par(new = TRUE)
dotchart(y, col = "red", xlim = c(73.5, 77), pch=0)
legend("topright", legend = groups, pch = c(1, 0), col = c("blue", "red"))

par(new = FALSE)




# 히스토그램 비교
par(mfrow = c(1, 3))  # 1행 3열로 그래프 영역 나눔

# 수축 혈압 비교
hist(sample_2019$수축기.혈압, col = "blue", 
     main = "수축기 혈압 비교", xlab = "수축기 혈압")
hist(sample_2021$수축기.혈압, col = adjustcolor("red", alpha.f = 0.5), add = TRUE)

# 이완 혈압 비교
hist(sample_2019$이완기.혈압, col = "blue", 
     main = "이완기 혈압 비교", xlab = "이완기 혈압")
hist(sample_2021$이완기.혈압, col = adjustcolor("red", alpha.f = 0.5), add = TRUE)

# 혈당 비교
hist(sample_2019$식전혈당.공복혈당., col = "blue", 
     main = "혈당 비교", xlab = "식당")
hist(sample_2021$식전혈당.공복혈당., col = adjustcolor("red", alpha.f = 0.5), add = TRUE)

# 범례 추가
legend("topright", legend = c("2019", "2021"), col = c("blue", "red"), 
       fill = c("blue", "red"), bty = "n")



#밀도차트------------------------
#밀도 그래프
ggplot() +
  geom_density(data = sample_2019, aes(x = 수축기.혈압, color = "2019"), fill = "lightblue", alpha = 0.5) +
  geom_density(data = sample_2021, aes(x = 수축기.혈압, color = "2021"), fill = "lightgreen", alpha = 0.5) +
  geom_density(data = sample_2019, aes(x = 이완기.혈압, color = "2019"), fill = "lightblue", alpha = 0.5) +
  geom_density(data = sample_2021, aes(x = 이완기.혈압, color = "2021"), fill = "lightgreen", alpha = 0.5) +
  geom_density(data = sample_2019, aes(x = 식전혈당.공복혈당., color = "2019"), fill = "lightblue", alpha = 0.5) +
  geom_density(data = sample_2021, aes(x = 식전혈당.공복혈당., color = "2021"), fill = "lightgreen", alpha = 0.5) +
  geom_vline(data = sample_2019, aes(xintercept = mean(수축기.혈압), color = "2019 평균"), linetype = "dashed", size = 1.2) +
  geom_vline(data = sample_2021, aes(xintercept = mean(수축기.혈압), color = "2021 평균"), linetype = "dashed", size = 1.2) +
  geom_vline(data = sample_2019, aes(xintercept = mean(이완기.혈압), color = "2019 평균"), linetype = "dashed", size = 1.2) +
  geom_vline(data = sample_2021, aes(xintercept = mean(이완기.혈압), color = "2021 평균"), linetype = "dashed", size = 1.2) +
  geom_vline(data = sample_2019, aes(xintercept = mean(식전혈당.공복혈당.), color = "2019 평균"), linetype = "dashed", size = 1.2) +
  geom_vline(data = sample_2021, aes(xintercept = mean(식전혈당.공복혈당.), color = "2021 평균"), linetype = "dashed", size = 1.2) +
  geom_text(data = sample_2019, aes(x = mean(수축기.혈압), y = 0, label = "수축기.혈압"), vjust = -1, color = "red", size = 4) +
  geom_text(data = sample_2019, aes(x = mean(이완기.혈압), y = 0, label = "이완기.혈압"), vjust = -1, color = "red", size = 4) +
  geom_text(data = sample_2019, aes(x = mean(식전혈당.공복혈당.), y = 0, label = "식전혈당.공복혈당."), vjust = -1, color = "red", size = 4) +
  labs(x = "Health Metric", y = "Density", color = "Year") +
  scale_color_manual(values = c("2019" = "blue", "2021" = "green", "2019 평균" = "blue", "2021 평균" = "darkgreen")) +
  theme_minimal()


#지도그리기 전 혈당 차이 구하기

mean_2019 <- aggregate(식전혈당.공복혈당. ~ 시도코드, sample_2019, mean)
mean_2021 <- aggregate(식전혈당.공복혈당. ~ 시도코드, sample_2021, mean)


comparison_data <- merge(mean_2019, mean_2021, by = "시도코드", suffixes = c("_2019", "_2021"),sort = FALSE)

comparison_data$차이 <- comparison_data$식전혈당.공복혈당._2021 - comparison_data$식전혈당.공복혈당._2019

comparison_data$id = c(1:16)
comparison_data$id = factor(comparison_data$id)

map_diff = comparison_data[,c(5,4)]
new_row <- data.frame(id = factor(17), 차이 = 0)
map_diff = rbind(map_diff,new_row)
map_diff$id = as.character(map_diff$id)



ggplot(comparison_data, aes(x = 시도코드, y = 차이)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "시도코드", y = "혈당수치 차이", title = "2019년과 2021년 혈당수치 차이")

map_diff$차이[map_diff$차이 < 0] = 0


#지도 그리기
map_korea <- st_read('ctp_rvn.shp')

map_korea$CTPRVN_CD <- iconv(map_korea$CTPRVN_CD,
                             from='CP949',
                             to='UTF-8', 
                             sub=NA,
                             mark=TRUE,
                             toRaw=FALSE)
map_korea_shp <-  as(map_korea, 'Spatial')
map_korea_df <- fortify(map_korea_shp)

map_korea_df <- merge(map_korea_df, map_diff, by = "id",sort = FALSE)

str(map_korea_df)

map_korea_ggplot <- map_korea_df %>% 
  ggplot(aes(x=long, y=lat, group = group))

map_korea_ggplot+
  geom_polygon(fill='white', color='black')+
  coord_quickmap()

map_korea_ggplot+
  geom_polygon(aes(fill=차이), color='black', show.legend=TRUE)+
  scale_fill_gradient(low = "white", high = "red")+
  coord_quickmap()+
  theme_minimal()






# = 2021 수축기 혈압, 이완기 혈압, 식전혈당 변수를 저장 =
#수축기 혈압
highPressure21 <- sample_2021[,c(5)]
#이완기 혈압
lowPressure21 <- sample_2021[,c(6)]
#공복혈압
sugar21 <- sample_2021[,c(7)]

# = 2019 수축기 혈압, 이완기 혈압, 식전혈당 변수를 저장 =
highPressure19 <- sample_2019[,c(5)]
lowPressure19 <- sample_2019[,c(6)]
sugar19 <- sample_2019[,c(7)]

# 등분산성 테스트 - t-검정을 하기전 무조건 해야 한다고 함...
# 테스트 결과 p-value > 0.05이여야지 분산이 동일(분산 만족)이라고 판단
# 할때마다 달라지니 주의...

var.test(highPressure21, highPressure19)
var.test(lowPressure21, lowPressure19)
var.test(sugar21, sugar19)

# t-test
# p-value가 0.05 이하로 나와야 통계적으로 유의하다고 할 수 있다.
# 두 집단의 분산이 동일하다면 var.equal = TRUE를, 동일하지 않다면 var.equal = FALSE
# 세 변수 모두 통계적으로 유의했음

t.test(highPressure21, highPressure19, var.equal=TRUE)
t.test(lowPressure21, lowPressure19, var.equal=TRUE)
t.test(sugar21, sugar19, var.equal=FALSE)


# = anova =

year <- c(rep(2021, 10000), rep(2019, 10000))

#수축기 혈압 데이터 프레임
high<-c(highPressure21, highPressure19)
highPressure <- data.frame(year, high)

#이완기 혈압 데이터 프레임
low <-c(lowPressure21, lowPressure19)
lowPressure <- data.frame(year, low)

#식전혈당
sugar <- c(sugar21, sugar19)
bloodSugar <- data.frame(year, sugar)

# anova 수축기 혈압 
anova.d <-aov(high ~ year, highPressure)
anova.d$coefficients
summary(anova.d)

# anova 이완기 혈압 
anova.d <-aov(low ~ year, lowPressure)
anova.d$coefficients
summary(anova.d)

# anova 식전혈당
anova.d <-aov(sugar ~ year, bloodSugar)
anova.d$coefficients
summary(anova.d)


#효과분석. 
# d estimate: 0.2일 경우 작은효과크기
# d estimate: 0.5일 경우 중간 효과크기
# d estimate: 0.8일 경우 큰 효과크기
cohen.d(high ~  factor(year), data= highPressure)

cohen.d(low ~  factor(year), data=lowPressure)

cohen.d(sugar ~  factor(year), data=bloodSugar)


#히스토그램
hist(sample_2021[,5], breaks = "FD", 
     col = "lightblue", border = "white", 
     xlab = "Data", ylab = "Frequency", main = "수축기 혈압")

hist(sample_2021[,6], breaks = "FD", 
     col = "lightblue", border = "white", 
     xlab = "Data", ylab = "Frequency", main = "이완기 혈압")

hist(sample_2021[,7], breaks = "FD", 
     col = "lightblue", border = "white", 
     xlab = "Data", ylab = "Frequency", main = "공복혈당")


#밀도 플롯
plot(density(sample_2021[,5]), col = "blue", 
     lwd = 2, main = "Density Plot")

plot(density(sample_2021[,6]), col = "blue", 
     lwd = 2, main = "Density Plot")

plot(density(sample_2021[,7]), col = "blue", 
     lwd = 2, main = "Density Plot")




