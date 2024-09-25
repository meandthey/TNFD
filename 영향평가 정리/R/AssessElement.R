library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)
library(tidyverse)
library(stringr)


################### [평가항목 정리] ###################

rawData <- readxl::read_excel("../data/영향평가 정리.xlsx", sheet = "데이터", col_names = T, skip = 0)

rawData_eco <- rawData %>%
  filter(구분 == 'impact')

#  'impact'

targetData_eco <- rawData_eco %>%
  count(항목) %>%
  mutate(유형 = "")

targetData_eco_elementList <- targetData_eco$항목
for ( i in 1:length(targetData_eco_elementList)) {
  
  each_element <- rawData_eco %>%
    filter(항목 == targetData_eco_elementList[i]) %>% 
    pull(유형)
  
  whoHas_each_element <- paste(each_element, collapse=", ")
  
  targetData_eco <- targetData_eco %>%
    mutate(유형 = case_when(
      
      항목 == targetData_eco_elementList[i] ~ whoHas_each_element,
      TRUE ~ 유형
      
    ))
  
}


write.xlsx(targetData_eco, "../output/targetData_impact.xlsx", fileEncoding = "EUC-KR")
test <- rawData_eco %>%
  filter(항목 == "Air Filtration") 







################### [상장법인 Data] ###################
mergedData_KOS <- readxl::read_excel("../data/상장법인/연구원님들/경기도 산업 리스트 통합.xlsx", sheet = "Sheet1", col_names = T, skip = 0) %>%
  mutate(번호 = row.names(.)) %>%
  rename(`상장기업 업종` = 업종) 


mergedData_KOS <- mergedData_KOS %>%
  mutate(회사명 = gsub("_0", "", 회사명)) %>%
  filter(!grepl("_", 회사명)) %>%
  mutate(매출액 = as.numeric(매출액)) %>%
  filter(!is.na(매출액)) %>%
  filter(매출액 != 0) %>%
  arrange(desc(매출액)) %>%
  mutate(번호 = as.numeric(row.names(.)))



# mergedData_KOS: Number of rows: 701
max_sales <- max(mergedData_KOS$매출액)
min_sales <- min(mergedData_KOS$매출액)
avg_sales <- mean(mergedData_KOS$매출액)
median_sales <- mergedData_KOS[ceiling(c(700 * 0.5)), "매출액" ] %>% pull()
first75_sales <- mergedData_KOS[ceiling(c(700 * 0.75)), "매출액" ] %>% pull()


## Graph ##
graphData <- mergedData_KOS %>%
  select(회사명, 매출액, 번호)


ggplot(data = graphData, aes(x = 번호, y = 매출액)) +
  geom_point(size = 1) +
  geom_line() +
  geom_hline(yintercept = avg_sales, linetype = 'dashed', colour = 'black', size = 1) +
  geom_hline(yintercept = median_sales, linetype = 'dashed', colour = 'blue', size = 1) +
  geom_hline(yintercept = first75_sales, linetype = 'dashed', colour = 'red', size = 1) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        text = element_text(size = 40))


######## 상위 10%는 빼고.

mergedData_KOS_wo10p <- mergedData_KOS %>%
  filter(번호 > 71) %>%
  mutate(번호 = as.numeric(row.names(.)))

# mergedData_KOS: Number of rows: 701
max_sales_wo10p <- max(mergedData_KOS_wo10p$매출액)
min_sales_wo10p <- min(mergedData_KOS_wo10p$매출액)
avg_sales_wo10p <- mean(mergedData_KOS_wo10p$매출액)
median_sales_wo10p <- mergedData_KOS_wo10p[ceiling(c(nrow(mergedData_KOS_wo10p) * 0.5)), "매출액" ] %>% pull()

ggplot(data = mergedData_KOS_wo10p, aes(x = 번호, y = 매출액)) +
  geom_point(size = 2) +
  geom_line() +
  geom_hline(yintercept = avg_sales_wo10p, linetype = 'dashed', colour = 'black', size = 2) +
  geom_hline(yintercept = median_sales_wo10p, linetype = 'dashed', colour = 'blue', size = 2) +
  #geom_hline(yintercept = first75_sales, linetype = 'dashed', colour = 'red', size = 1) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        text = element_text(size = 40),
        axis.ticks.length = unit(.8, "cm")) +
  expand_limits(x = 0, y = 0)

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

rawData_Se %>%
  count(대분류코드)

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



totalData_GG %>%
  arrange(desc(매출액)) %>%
  filter(매출액 >= c(median_sales_wo10p/10^(6)))








################################################################################################################################################


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


################### [데이터드림림 Data] ###################
GGdream_Data <- read_delim("../data/데이터드림/2021년업종별사업체현황.csv", locale = locale(encoding = "EUC-KR"), delim = ",", col_names = T)
GGdream_Data <- GGdream_Data %>%
  mutate(소재지지번주소 = gsub("번지", "", 소재지지번주소 )) %>%
  mutate(번호 = row.names(.))
  # mutate(소재지지번주소 = case_when(
  #   
  #   grepl("번지", 소재지지번주소) ~ gsub("번지", "", 소재지지번주소 ),
  #   TRUE ~ 소재지지번주소
  #   
  # ))


GGdream_address <- str_split(GGdream_Data$소재지지번주소, " ", simplify = T)




################### [상장법인 Data] ###################

## SSN(486), JWH(445), WSY(537)
data_SSN <- readxl::read_excel("../data/상장법인/연구원님들/상장기업 검색_송새눈_240921.xlsx", sheet = "상장기업 리스트", col_names = T, skip = 0)
data_JWH <- readxl::read_excel("../data/상장법인/연구원님들/상장기업 검색_조원호_수정2.xlsx", sheet = "상장기업 리스트", col_names = T, skip = 0) %>%
  mutate(직원총수 = as.numeric(직원총수))
data_WSY <- readxl::read_excel("../data/상장법인/연구원님들/상장기업 검색_원수연.xlsx", sheet = "상장기업 리스트", col_names = T, skip = 0) %>%
  rename(매출액 = `매출액(백만원)`) %>%
  mutate(직원총수 = as.numeric(직원총수))


mergedData_KOS <- data_SSN %>%
  bind_rows(data_JWH, data_WSY) %>%
  mutate(회사명 = gsub("_0", "", 회사명)) %>%
  filter(!grepl("_", 회사명)) %>%
  mutate(번호 = row.names(.))
  

mergedData_KOS_address <- str_split(mergedData_KOS$지번주소, " ", simplify = T)

matchNumber_All <- c()
matchNumberMatrix <- matrix(nrow = nrow(mergedData_KOS_address), ncol = 10)
for ( i in 1:nrow(mergedData_KOS_address)) {
  
  KOS_each <- mergedData_KOS_address[i,]
  KOS_each <- KOS_each[KOS_each != ""]
  
  for ( j in 1:nrow(GGdream_address)) {
    
    All_each <- GGdream_address[j,]
    All_each <- All_each[All_each != ""]
    
    result_each <- sum(KOS_each %in% All_each)
    
    matchNumber_All[j] <- result_each

  }
  
  matchNumber <- which(matchNumber_All == max(matchNumber_All))
  
  if (length(matchNumber) >= 10 ) {
    
    matchNumberMatrix[i,1] <- 0
    
  } else {
    
    matchNumberMatrix[i,c(1:length(matchNumber))] <- matchNumber
    
  }
  
}

matchNumberTibble <- as.tibble(matchNumberMatrix) %>%
  mutate(번호 = row.names(.))

matchNumberMatrix_onlyOne <- matchNumberTibble %>%
  filter(is.na(V2),
         V1 != 0) %>%
  select(V1, 번호)

final_sampleData <- mergedData_KOS %>%
  left_join(matchNumberMatrix_onlyOne, by = c("번호")) %>%
  left_join(GGdream_Data %>% rename(V1 = 번호) %>% mutate(V1 = as.numeric(V1)), by = c("V1"))

  


mergedData_KOS 
GGdream_Data




matchNumberMatrix

matchNumber[matchNumber == max(matchNumber)]




matchNumber <- c()
for ( i in 1:nrow(GGdream_address)) {
  
  All_each <- GGdream_address[i,]
  All_each <- All_each[All_each != ""]
  
  result_each <- sum(KOS_1 %in% All_each)
  
  matchNumber[i] <- result_each
  #matchNumber[i, "AA"] <- result_each
  
  
}



##### 비교교 #####
KOS_1 <- mergedData_KOS_address[1,]
KOS_1 <- KOS_1[KOS_1 != ""]
#All_1[All_1 == ""] <- NA


All_1 <- GGdream_address[1,] 
All_1 <- All_1[All_1 != ""]
#KOS_1[KOS_1 == ""] <- NA

KOS_1 %in% All_1


All_1 %>%
  mutate(wtdAdrs = )

KOS_1 <- mergedData_KOS_address[1,]


for ( i in 1:nrow())












A <- "A B CC CAE FE"
B <- "A BC CD"
C <- "AA BB CCC CDE"
ABC <- t(data.frame( A, B, C))


AA <- c("사과", "배", "감")
BB <- c("오렌지", "사과", "포도", "배")
AA %in% BB



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



################### [상장법인 Data] ###################


data_SSN <- readxl::read_excel("../data/상장법인/연구원님들/상장기업 검색_송새눈.xlsx", sheet = "상장기업 리스트", col_names = T, skip = 0)

data_SSN



read.csv("../data/데이터드림/2021년업종별사업체현황.csv")
read_csv("../data/데이터드림/2021년업종별사업체현황.csv")

<- read_delim("../data/데이터드림/2021년업종별사업체현황.csv", locale = locale(encoding = "EUC-KR"), delim = ",", col_names = F)














