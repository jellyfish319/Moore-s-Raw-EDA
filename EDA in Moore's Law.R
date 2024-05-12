getwd()

rm(list = ls())

setwd("Dataset/Computer Part Dataset")

library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(gridExtra)
library(DescTools)
library(knitr)

# 데이터 불러오기

chip_data <- read.csv("chip_dataset.csv")
gpu_prices <- read.csv("gpu_specs_prices.csv")
storage <- read.csv("SSD_HDD_benchmarks_v9.csv")
cpu_bench <- read.csv("CPU_benchmark_v4.csv")
cpu_core_bench <- read.csv("CPU_r23_v2.csv")
ram <- read.csv("RAM_Benchmarks_megalist.csv")

# 무어의 법칙 확인

summary(chip_data)
names(chip_data)
dim(chip_data)
str(chip_data)


for (i in c(2,3,4,8)) {
  print(sum(is.na(chip_data[,i])))
} # transistor 결측치 711개


# 결측치 제거
chip_data <- chip_data[,c(2,3,4,8)]
chip_data <- chip_data %>% filter(!is.na(Transistors..million.))
chip_data$Release.Date <- as.Date(chip_data$Release.Date)


# cpu와 gpu의 산점도

ggplot(chip_data, aes(x = Release.Date, y = Transistors..million., color = Type)) +
  geom_point(alpha = 0.3)


ggplot(chip_data, aes(x = Release.Date, y = log2(Transistors..million.) , color = Type)) +
  geom_point(alpha = 0.3) +
  stat_smooth(method = lm, level = 0) +
  scale_linetype_manual(values = c(1, 2))

# 특정 모델군의 성능 파악

i3_data <- filter(chip_data, str_detect(Product, "i3"))

ggplot(i3_data, aes(x = Release.Date, y = log2(Transistors..million.))) +
  geom_point(color = "navy", alpha = 0.7) +
  scale_x_date(limits = c(as.Date("2010-01-03"),as.Date("2017-01-13"))) +
  stat_smooth(method = lm,level = 0)

# cpu와 gpu의 트랜지스터 수의 연도별 평균 시각화

log_mean <- data.frame(cpu = rep(NA, time = 16), 
                   gpu = rep(NA, time = 16),
                   year = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016",
                                          "2017","2018","2019","2020","2021"))

mean <- data.frame(cpu = rep(NA, time = 16), 
                   gpu = rep(NA, time = 16),
                   year = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016",
                            "2017","2018","2019","2020","2021"))

for (i in 6:21) { 
  if ( i < 10) {
    df <- filter(chip_data, str_detect(Release.Date, paste0("200",as.character(i))))
    df1 <- filter(assign(paste0("chip_",i), df), str_detect(Type, "CPU"))
    assign(paste0("cpu_",i), df1)
    df2 <- filter(assign(paste0("chip_",i), df), str_detect(Type, "GPU"))
    assign(paste0("gpu_",i), df2)
  }
  else {
  df <- filter(chip_data, str_detect(Release.Date, paste0("20",as.character(i))))
  df1 <- filter(assign(paste0("chip_",i), df), str_detect(Type, "CPU"))
  assign(paste0("cpu_",i), df1)
  df2 <- filter(assign(paste0("chip_",i), df), str_detect(Type, "GPU"))
  assign(paste0("gpu_",i), df2)
  }
  
  df_name <- paste0("cpu_", i)
  df_col <- mean(log2(get(df_name)[,4]), na.rm = T)
  log_mean[i-5, 1] <- df_col
  
  df_name <- paste0("gpu_", i)
  df_col <- mean(log2(get(df_name)[,4]), na.rm = T)
  log_mean[i-5, 2] <- df_col
  
  df_name <- paste0("cpu_", i)
  df_col <- mean(get(df_name)[,4], na.rm = T)
  mean[i-5, 1] <- df_col
  
  df_name <- paste0("gpu_", i)
  df_col <- mean(get(df_name)[,4], na.rm = T)
  mean[i-5, 2] <- df_col
  
  rm(list = paste0("cpu_",i))
  rm(list = paste0("gpu_",i))
  rm(df_name)
  rm(df_col)
  rm(df1)
  rm(df2)
  rm(list = paste0("chip_",i))
  rm(list = paste0("df_",i))
}

mean <- mean %>% 
  pivot_longer(cols = c("cpu", "gpu"), names_to = "chipset", values_to = "Transistors..million.")

log_mean <- log_mean %>% 
  pivot_longer(cols = c("cpu", "gpu"), names_to = "chipset", values_to = "Transistors..million.")

ggplot(mean, aes(x = year, y = Transistors..million., fill = chipset)) +
  geom_bar(stat = "identity", position = "dodge") # 2019년에 performance 하락 발생

ggplot(log_mean, aes(x = year, y = Transistors..million., fill = chipset)) +
  geom_bar(stat = "identity", position = "dodge")

# CPU의 성능과 가격 간 관계

rm(list = ls())

cpu_price_bench <- read.csv("CPU_benchmark_v4.csv")

summary(cpu_price_bench)
names(cpu_price_bench)
dim(cpu_price_bench)
str(cpu_price_bench)

cpu_price_bench$testDate <- as.character(cpu_price_bench$testDate)

cpu_price_bench <- cpu_price_bench[,c(1,2,3,4,10,12)]

for (i in c(1:5)) {
  print(sum(is.na(cpu_price_bench[,i])))
} # price 결측치 1858개

# 결측치 제거
cpu_price_bench <- cpu_price_bench %>% filter(!is.na(price))

# 로그스케일 적용
ggplot(cpu_price_bench, aes(x = log2(cpuMark), y = log2(price), color = testDate)) + 
  geom_point()

# 로그스케일 cpuMark에만 적용
ggplot(cpu_price_bench, aes(x = log2(cpuMark), y = price, color = testDate)) + 
  geom_point()

# 로그스케일 미적용
ggplot(cpu_price_bench, aes(x = cpuMark, y = price, color = testDate)) + 
  geom_point()

# 2022년과 2015년, 2008년간 성능과 가격의 차이
compare <- filter(cpu_price_bench, cpu_price_bench$testDate == "2022"|cpu_price_bench$testDate == "2008"|cpu_price_bench$testDate == "2015")

ggplot(compare, aes(x = log2(price), y = log2(cpuMark), color = testDate)) +
  geom_point()

# 같은 가격인데도 시대별로 성능의 차이가 나는 이유

i3_2022 <- filter(cpu_price_bench, str_detect(cpu_price_bench$cpuName, "i3")&cpu_price_bench$testDate == "2022")
i7_2022 <- filter(cpu_price_bench, str_detect(cpu_price_bench$cpuName, "i7")&cpu_price_bench$testDate=="2022")
i7_2018 <- filter(cpu_price_bench, str_detect(cpu_price_bench$cpuName, "i7")&cpu_price_bench$testDate=="2018")

compare_1 <- rbind(i3_2022, i7_2022)
compare_2 <- rbind(i7_2018, i7_2022)
compare_3 <- rbind(i3_2022, i7_2018)

ggplot(compare_1, aes(x = price, y = cpuMark)) +
  geom_point() 

ggplot(compare_2, aes(x = price, y = cpuMark, color = testDate)) +
  geom_point()

ggplot(compare_3, aes(x = price, y = cpuMark, color = testDate)) +
  geom_point()

# 연도 별 cpuMark와 가격의 상자그림

ggplot(cpu_price_bench, aes(x = testDate, y = log(price))) +
  geom_violin() +
  geom_boxplot(width = 0.3, fill="yellow", outlier.colour=NA) +
  stat_summary(fun.y ="mean", geom ="point", shape=21, size=3, fill="white")

ggplot(cpu_price_bench, aes(x = testDate, y = log2(cpuMark))) +
  geom_violin() +
  geom_boxplot(width = 0.3, fill="yellow", outlier.colour=NA) +
  stat_summary(fun.y ="mean", geom ="point", shape=21, size=3, fill="white")

# 연도 별 가격의 중앙값과 평균

mean_price_year <- data.frame(mean = rep(NA, time = 16), year = c("2007","2008","2009","2010","2011","2012","2013","2014","2015","2016",
                "2017","2018","2019","2020","2021","2022"))

for (i in 7:22) {
  as.numeric(cpu_price_bench$testDate)
  k <- filter(cpu_price_bench, cpu_price_bench$testDate == i+2000)
  mean_price_year[i-6,1] <- mean(k$price)
  rm(k)
}

median_price_year <- data.frame(median = rep(NA, time = 16), year = c("2007","2008","2009","2010","2011","2012","2013","2014","2015","2016",
                "2017","2018","2019","2020","2021","2022"))

for (i in 7:22) {
  as.numeric(cpu_price_bench$cpuMark)
  k <- filter(cpu_price_bench, cpu_price_bench$testDate == i+2000)
  median_price_year[i-6,1] <- median(k$cpuMark)
  rm(k)
}

mean_price_year
median_price_year

as.character(cpu_price_bench$testDate)

price_year <- merge(mean_price_year, median_price_year)
price_year <- price_year %>% pivot_longer(cols = c("mean", "median"), names_to = "stats", values_to = "price")

ggplot(mean_price_year, aes(x = year, y = mean)) +
  geom_bar(stat = 'identity', fill = 'lightblue')

ggplot(price_year, aes(x = year, y = price, fill = stats)) +
  geom_bar(stat = "identity", position = 'dodge')

# 모델 간 연도별 가격의 평균

model_year <- cpu_price_bench

model_year$model <- ifelse(grepl("i3",cpu_price_bench$cpuName), 'i3',
                        ifelse(grepl("i5",cpu_price_bench$cpuName), 'i5',
                              ifelse(grepl("i7",cpu_price_bench$cpuName), 'i7',
                                      ifelse(grepl("i9",cpu_price_bench$cpuName), 'i9',NA))))

model_year <- na.omit(model_year) 

as.numeric(model_year$testDate)

a <- data.frame()
mean_price_year <- data.frame()
df <- data.frame(mean_of_price = NA, year = c("2008","2009","2010","2011","2012","2013","2014","2015","2016",
                                                            "2017","2018","2019","2020","2021","2022"), model = NA)
for (j in c(3,5,7,9)) {
  for (i in 1:15) {
  a <- filter(model_year, model_year$testDate == i + 2007&grepl(paste0("i",j), model_year$model))
  df[i,1] <- mean(a$price)
  df[,3] <- paste0("i",j)
  } 
  mean_price_year <- rbind(mean_price_year, df)
}

rm(df) ; rm(a)

ggplot(mean_price_year, aes(x = year, y = mean_of_price, fill = model)) + 
  geom_bar(stat = 'identity', position = 'dodge')

a <- data.frame()
mean_value_year <- data.frame()
df <- data.frame(mean_of_value = NA, year = c("2008","2009","2010","2011","2012","2013","2014","2015","2016",
                                                            "2017","2018","2019","2020","2021","2022"), model = NA)
for (j in c(3,5,7,9)) {
  for (i in 1:15) {
  a <- filter(model_year, model_year$testDate == i + 2007&grepl(paste0("i",j), model_year$model))
  df[i,1] <- mean(a$cpuValue)
  df[,3] <- paste0("i",j)
  } 
  mean_value_year <- rbind(mean_value_year, df)
}

rm(df) ; rm(a)

ggplot(mean_value_year, aes(x = year, y = mean_of_value, fill = model)) + 
  geom_bar(stat = 'identity', position = 'dodge')

# 가성비(cpuValue)

value <- cpu_price_bench[,c(1,4,5,6)]
ggplot(value, aes(x = testDate, y = log2(cpuValue))) +
  geom_point()


typeof(value$testDate)

value$testDate <- as.factor(value$testDate)

mean_value <- aggregate(cpuValue ~ testDate, value, mean)
mean_value$testDate <- as.numeric(levels(mean_value$testDate))
mean_value <- mean_value[-1,]

# 전체 CPU의 가성비의  선그래프
ggplot(mean_value, aes(x = testDate, y = cpuValue)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(min(mean_value$testDate), max(mean_value$testDate), by = 1))

idx <- grepl("i3",value$cpuName)
i3_value <- value[idx,]
idx <- grepl("Ryzen 3", value$cpuName)
Ryzen_3_value <- value[idx,]

i3_value$model <- "i3"
Ryzen_3_value$model <- "Ryzen 3"

i3_mean <- aggregate(cpuValue ~ testDate, i3_value, mean)
Ryzen_3_mean <- aggregate(cpuValue ~ testDate, Ryzen_3_value, mean)

i3_mean$testDate <- as.character(i3_mean$testDate)
i3_mean$testDate <- as.numeric(i3_mean$testDate)
Ryzen_3_mean$testDate <- as.character(Ryzen_3_mean$testDate)
Ryzen_3_mean$testDate <- as.numeric(Ryzen_3_mean$testDate)


idx <- grepl('AMD', value$cpuName)
AMD <- value[idx,]
idx <- grepl('Intel', value$cpuName)
Intel <- value[idx,]

Intel_mean <- aggregate(cpuValue ~ testDate, Intel, mean)
AMD_mean <- aggregate(cpuValue ~ testDate, AMD, mean)

# AMD의 Ryzen 3 모델과 Intel의 i3 모델의 연도별 가성비 비교
ggplot() +
  geom_line(i3_mean, mapping = aes(x = testDate, y = cpuValue), color = 'blue') +
  geom_line(Ryzen_3_mean, mapping = aes(x = testDate, y = cpuValue), color = 'red') +
  scale_x_continuous(breaks = seq(min(i3_mean$testDate), max(i3_mean$testDate), by = 1)) +
  geom_point(i3_mean, mapping = aes(x = testDate, y = cpuValue), color = 'blue') +
  geom_point(Ryzen_3_mean, mapping = aes(x = testDate, y = cpuValue), color = 'red') +
  labs(title = "i3 (Blue) VS Ryzen 3 (Red)")

Intel_mean$testDate <- as.character(Intel_mean$testDate)
Intel_mean$testDate <- as.numeric(Intel_mean$testDate)
AMD_mean$testDate <- as.character(AMD_mean$testDate)
AMD_mean$testDate <- as.numeric(AMD_mean$testDate)
Intel_mean <- Intel_mean[-1,]

# AMD와 Intel의 연도별 가성비 비교
ggplot() +
  geom_line(Intel_mean, mapping = aes(x = testDate, y = cpuValue), color = 'blue') +
  geom_line(AMD_mean, mapping = aes(x = testDate, y = cpuValue), color = 'red') +
  scale_x_continuous(breaks = seq(min(Intel_mean$testDate), max(Intel_mean$testDate), by = 1)) +
  geom_point(Intel_mean, mapping = aes(x = testDate, y = cpuValue), color = 'blue') +
  geom_point(AMD_mean, mapping = aes(x = testDate, y = cpuValue), color = 'red') +
  geom_line(mean_value, mapping = aes(x = testDate, y = cpuValue), color = 'black') +
  geom_point(mean_value,mapping = aes(x = testDate, y = cpuValue), color = 'black') +
  labs(title = "Intel (Blue) VS AMD (Red) VS Total (Black)")


# CPU 성능과 전력 간 관계

rm(list = ls())

cpu_power_bench <- read.csv("CPU_benchmark_v4.csv")
cpu_power_bench <- cpu_power_bench[,c(1,3,7,8,10,12)]

sum(is.na(cpu_power_bench$TDP)) # TDP의 결측치 685개
cpu_power_bench <- cpu_power_bench %>% filter(!is.na(TDP))

cpu_power_bench$testDate <- as.character(cpu_power_bench$testDate)

levels(factor(cpu_power_bench$category)) # 12가지의 유형들이 있음을 확인 가능
summary(factor(cpu_power_bench$category))

cpu_power_bench <- subset(cpu_power_bench, category %in% c("Server", "Desktop", "Laptop"))

ggplot(cpu_power_bench, aes(x = log2(cpuMark), y = TDP, color = category)) +
  geom_point()

ggplot(cpu_power_bench, aes(x = log2(cpuMark), y = TDP, color = testDate)) +
  geom_point()

# 모델별 전력

model_year <- cpu_power_bench

model_year$model <- ifelse(grepl("i3",cpu_power_bench$cpuName), 'i3',
                           ifelse(grepl("i5",cpu_power_bench$cpuName), 'i5',
                                  ifelse(grepl("i7",cpu_power_bench$cpuName), 'i7',
                                         ifelse(grepl("i9",cpu_power_bench$cpuName), 'i9',NA))))

model_year <- na.omit(model_year)
model_year$TDP <- as.character(model_year$TDP)

ggplot(model_year, aes(x = log2(cpuMark), y = as.numeric(model_year$TDP), color = model)) +
  geom_point() +
  ylab('TDP')


# TDP의 모델 별 도수분포표

model_year$TDP <- as.numeric(model_year$TDP)
model_year <- model_year[order(model_year$TDP),] # TDP의 크기에 따라 재정렬
model_year$TDP <- factor(model_year[order(model_year$TDP),'TDP'],
                         levels = unique(model_year$TDP), ordered = TRUE) # TDP를 factor형으로 변환하여 levels 부여

ls = list()

for (j in c(1:4)) {
  ls[[j]] = ggplot(model_year[model_year$model == paste0("i",2*j+1),], aes(x = TDP)) +
    geom_histogram(stat = 'count', color="black", fill="gray") +
    labs(title = paste0("i",2*j+1)) +
    scale_x_discrete(limits=c(levels(model_year[model_year$model == paste0("i",2*j+1),])))
} # 히스토그램의 scale을 levels의 오름차순으로 정렬하여 그림

do.call(grid.arrange,ls)

# 연도 별 TDP의 median

model_year$testDate <- as.numeric(model_year$testDate)

a <- data.frame()
median_power_year <- data.frame()
df <- data.frame(median_of_power = NA, year = c("2008","2009","2010","2011","2012","2013","2014","2015","2016",
                                              "2017","2018","2019","2020","2021","2022"), model = NA)

for (j in c(3,5,7,9)) {
  for (i in 1:15) {
    a <- filter(model_year, model_year$testDate == i + 2007 & grepl(paste0("i",j), model_year$model))
    df[i,1] <- median(as.numeric(a$TDP))
    df[,3] <- paste0("i",j)
  } 
  median_power_year <- rbind(median_power_year, df)
}

ggplot(median_power_year, aes(x = year, y = median_of_power, fill = model)) + 
  geom_bar(stat = 'identity', position = 'dodge')

rm(df) ; rm(a)

# 타입별 전력

cpu_power_bench$TDP <- as.numeric(cpu_power_bench$TDP)
cpu_power_bench <- cpu_power_bench[order(cpu_power_bench$TDP),] # TDP의 크기에 따라 재정렬
cpu_power_bench$TDP <- factor(cpu_power_bench[order(cpu_power_bench$TDP),'TDP'],
                              levels = unique(cpu_power_bench$TDP), ordered = TRUE) # TDP를 factor형으로 변환하여 levels 부여

type.list <- split(cpu_power_bench, cpu_power_bench$category)

ls = list()

for (i in c(1:3)) {
  ls[[i]] = ggplot(as.data.frame(type.list[[i]]), aes(x = TDP)) +
    geom_histogram(stat = 'count') +
    labs(title = names(type.list[i]))
}

do.call(grid.arrange,ls)

# 타입별 연도간 전력의 median

model_year$TDP <- as.numeric(model_year$TDP)
cpu_power_bench$TDP <- as.numeric(cpu_power_bench$TDP)

type_median <- aggregate(TDP ~ category + testDate, cpu_power_bench, median)

type_median <- type_median[-1,]

ggplot(type_median, aes(x = testDate, y = TDP, fill = category)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  xlab('year') +
  ylab('median of TDP')

# 저장장치에서의 무어의 법칙

rm(list = ls())

storage <- read.csv("SSD_HDD_benchmarks_v9.csv")

summary(storage)
dim(storage)
names(storage)
str(storage)

storage <- storage[,c(1:4,9)] # 이름, 타입, 용량, 속도, 날짜 추출

for (i in c(3,4,5)) {
  print(sum(is.na(storage[,i])))
} # 결측치 없음음


# 날짜 별 용량과 속도의 산점도를 타입별로 구분하여 그림
storage$testDate <- as.character(storage$testDate)
storage$type <- factor(storage$type)

p1 <-ggplot(storage, mapping = aes(x = testDate,y = diskCapacity, color = type)) +
  geom_point()

p2 <- ggplot(storage, mapping = aes(x = testDate,y = log2(diskCapacity), color = type)) +
  geom_point()

p3 <- ggplot(storage, aes(x = testDate, y = diskMark, color = type)) +
  geom_point()

p4 <- ggplot(storage, aes(x = testDate, y = log2(diskMark), color = type)) +
  geom_point()

ls = list(p1,p2,p3,p4)
do.call(grid.arrange,ls)

# 용량과 속도의 타입별 평균 & 연 평균
type_storage.ls <- split(storage, storage$type)
date_storage.ls <- split(storage, storage$testDate)

type_storage <- data.frame(setNames(lapply(c(1,2),
                                           function(i) sapply(type_storage.ls[[i]][c('diskCapacity', 'diskMark')], mean)),
                                    names(type_storage.ls)))
date_storage <- data.frame(setNames(lapply(1:length(date_storage.ls),
                                           function(i) sapply(date_storage.ls[[i]][c('diskCapacity', 'diskMark')], mean)),
                                    names(date_storage.ls)), check.names = FALSE)


# 타입별 속도와 용량의 연 평균

mean_storage <- aggregate(cbind(diskCapacity, diskMark) ~ type + testDate, storage, mean)
even <- mean_storage[as.numeric(mean_storage$testDate)%%2 == 0,]

p1 <- ggplot(mean_storage,aes(x = testDate, y = diskCapacity, fill = type)) +
  geom_bar(position = "dodge", stat = "identity")

p2 <- ggplot(mean_storage,aes(x = testDate, y = log2(diskCapacity), fill = type)) +
  geom_bar(position = "dodge", stat = "identity")


p3 <- ggplot(mean_storage,aes(x = testDate, y = diskMark, fill = type)) +
  geom_bar(position = "dodge", stat = "identity")

p4 <- ggplot(mean_storage,aes(x = testDate, y = log2(diskMark), fill = type)) +
  geom_bar(position = "dodge", stat = "identity")

ls = list(p1,p2,p3,p4)
do.call(grid.arrange, ls)

# 2년 간격으로 그래프를 확인

as.numeric(even$testDate)

even_ssd <- even[even$type == "SSD",]
even_hdd <- even[even$type == "HDD",]

p1 <- ggplot() +
  geom_line(even_hdd, mapping = aes(x = as.numeric(testDate), y = log2(diskMark)), color = "blue") +
  geom_line(even_ssd, mapping = aes(x = as.numeric(testDate), y = log2(diskMark)), color = 'red') +
  geom_point(even_hdd, mapping = aes(x = as.numeric(testDate), y = log2(diskMark)),color = "blue") +
  geom_point(even_ssd, mapping = aes(x = as.numeric(testDate), y = log2(diskMark)), color = 'red') +
  xlab("testDate") +
  labs(title = "diskMark (Red = SSD, Blue = HDD)")

p2 <- ggplot() +
  geom_line(even_hdd, mapping = aes(x = as.numeric(testDate), y = log2(diskCapacity)), color = "blue") +
  geom_line(even_ssd, mapping = aes(x = as.numeric(testDate), y = log2(diskCapacity)), color = 'red') +
  geom_point(even_hdd, mapping = aes(x = as.numeric(testDate), y = log2(diskCapacity)),color = "blue") +
  geom_point(even_ssd, mapping = aes(x = as.numeric(testDate), y = log2(diskCapacity)), color = 'red') +
  xlab("testDate") +
  labs(title = "diskCapacity (Red = SSD, Blue = HDD)")

ls = list(p1,p2)
do.call(grid.arrange, ls)

# RAM의 히스토그램
rm(list = ls())

ram <- read.csv("RAM_Benchmarks_megalist.csv")

summary(ram)
dim(ram)
names(ram)
str(ram)

ram <- ram[,-6]

for (i in c(1:5)) {
  print(sum(is.na(ram[,i])))
} # 결측치 없음


ggplot(ram, aes(x = latency)) +
  geom_histogram(stat = 'count') +
  xlim(0,200)

ggplot(ram, aes(x = write)) +
  geom_histogram(bins = 250)

ggplot(ram, aes(x = readUncached)) +
  geom_histogram(bins = 250)

ram$gen <- factor(ram$gen)

# 세대 별 write, latency, readUncached의 평균

kable(aggregate(cbind(write, latency, readUncached) ~ gen, ram, mean))












