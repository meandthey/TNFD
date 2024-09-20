library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)
library(tidyverse)
library(stringr)

################### [MDIS Data] ###################

## 서비스업
rawData_Se <- readxl::read_excel("../data/MDIS/서비스업.xlsx", sheet = "데이터", col_names = T, skip = 0)
rawData_Se <- rawData_Se %>%
  rename(시도코드 = 행정구역코드,
         대분류코드 = 산업대분류코드,
         중분류코드 = 산업중분류코드,
         소분류코드 = 산업소분류코드,
         세분류코드 = 산업세분류코드,
         세세분류코드 = 산업세세분류코드,
         종사자수 = 종사자수_합계수
  ) %>%
  mutate(종사자수 = as.numeric(종사자수),
         매출액 = as.numeric(매출액)) %>%
  select(시도코드, 대분류코드, 중분류코드, 소분류코드, 세분류코드, 세세분류코드,
         종사자수, 매출액)
  
## 운수업
rawData_Tr <- readxl::read_excel("../data/MDIS/운수업.xlsx", sheet = "데이터", col_names = T, skip = 0) 
rawData_Tr <- rawData_Tr %>%
  rename(시도코드 = 행정구역시도코드,
         대분류코드 = 산업대분류코드,
         중분류코드 = 산업중분류코드,
         소분류코드 = 산업소분류코드,
         세분류코드 = 산업세분류코드,
         세세분류코드 = 산업세세분류코드,
         종사자수 = 종사자수_조사_총소계,
         매출액 = 매출총액
  ) %>%
  mutate(종사자수 = as.numeric(종사자수),
         매출액 = as.numeric(매출액)) %>%
  select(시도코드, 대분류코드, 중분류코드, 소분류코드, 세분류코드, 세세분류코드,
         종사자수, 매출액)

## 광업_제조업
rawData_MiMa <- readxl::read_excel("../data/MDIS/광업_제조업.xlsx", sheet = "데이터", col_names = T, skip = 0) 
rawData_MiMa <- rawData_MiMa %>%
  rename(시도코드 = 행정구역시도코드,
         대분류코드 = 산업대분류코드,
         중분류코드 = 주사업_산업중분류코드,
         소분류코드 = 주사업_산업소분류코드,
         세분류코드 = 주사업_산업세분류코드,
         세세분류코드 = 주사업_산업세세분류코드,
         종사자수 = 종사자수_조사_총소계
  ) %>%
  mutate(종사자수 = as.numeric(종사자수)) %>%
  select(시도코드, 대분류코드, 중분류코드, 소분류코드, 세분류코드, 세세분류코드,
         종사자수, 자산총금액)

## Code Mapping ##
CodeMap <- readxl::read_excel("../data/Mapping/산업분류 Mapping.xlsx", sheet = "데이터", col_names = T, skip = 1) %>%
  mutate(소분류코드 = str_sub( 소분류코드, 3),
         세분류코드 = str_sub( 세분류코드, 4),
         세세분류코드 = str_sub( 세세분류코드, 5))




## Merge ##
totalData <- rawData_Se %>% bind_rows(rawData_Tr, rawData_MiMa) %>%
  mutate(중분류코드 = as.character(중분류코드),
         소분류코드 = as.character(소분류코드),
         세분류코드 = as.character(세분류코드),
         세세분류코드 = as.character(세세분류코드)) %>%
  left_join(CodeMap, by = c('대분류코드', '중분류코드', '소분류코드', '세분류코드', '세세분류코드'))


totalData_GG <- totalData %>%
  filter(시도코드 == 31) %>%
  mutate(Full코드 = paste0(대분류코드, 중분류코드, 소분류코드, 세분류코드, 세세분류코드))

totalData_GG_Num <- totalData_GG %>%
  count(대분류코드, 중분류코드, 소분류코드, 세분류코드, 세세분류코드,
        대분류명, 중분류명, 소분류명, 세분류명, 세세분류명)


ggplot(data = totalData_GG, aes(x = 중분류코드)) +
  geom_bar(stat = 'count', width = 0.7) +
  facet_wrap(~대분류코드)

ggplot(data = totalData_GG, aes(x = Full코드)) +
  geom_bar(stat = 'count', width = 0.7)

################### [상장법인 Data] ###################

rawData_KOS <- readxl::read_excel("../data/상장법인/상장법인목록.xlsx", sheet = "데이터", col_names = T, skip = 0) %>%
  rename(`상장기업 업종` = 업종)

TNFD_Map <- readxl::read_excel("../data/Mapping/TNFD Mapping.xlsx", sheet = "상장기업toTNFD", col_names = T, skip = 0)

TNFD_Def <- readxl::read_excel("../data/Mapping/TNFD Mapping.xlsx", sheet = "TNFD정의", col_names = T, skip = 0)

KOS_data <- rawData_KOS %>%
  left_join(TNFD_Map, by = c("상장기업 업종")) %>%
  left_join(TNFD_Def, by = c("TNFD코드"))


#### Test ################### [상장법인 Data] ###################

rawData_KOS <- readxl::read_excel("../data/상장법인/상장법인목록_test.xlsx", sheet = "데이터", col_names = T, skip = 0) %>%
  rename(`상장기업 업종` = 업종)

TNFD_Map <- readxl::read_excel("../data/Mapping/TNFD Mapping.xlsx", sheet = "상장기업toTNFD", col_names = T, skip = 0)

TNFD_Def <- readxl::read_excel("../data/Mapping/TNFD Mapping.xlsx", sheet = "TNFD정의", col_names = T, skip = 0)

KOS_data <- rawData_KOS %>%
  left_join(TNFD_Map, by = c("상장기업 업종")) %>%
  left_join(TNFD_Def, by = c("TNFD코드"))



#### Test ################### [상장법인 Data] ###################
read.csv("../data/데이터드림/2021년업종별사업체현황.csv")
read_csv("../data/데이터드림/2021년업종별사업체현황.csv")

read_delim("../data/데이터드림/2021년업종별사업체현황.csv", locale = locale(encoding = "EUC-KR"), delim = ",", col_names = F)

data_SSN <- readxl::read_excel("../data/상장법인/연구원님들/상장기업 검색_송새눈.xlsx", sheet = "상장기업 리스트", col_names = T, skip = 0)

data_SSN
















