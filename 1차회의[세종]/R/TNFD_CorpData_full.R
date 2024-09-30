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


################### [데이터드림 Data] ###################
GGdream_Data <- read_delim("../data/데이터드림/2021년업종별사업체현황.csv", locale = locale(encoding = "EUC-KR"), delim = ",", col_names = T)
GGdream_Data <- GGdream_Data %>%
  mutate(소재지지번주소 = gsub("번지", "", 소재지지번주소 )) %>%
  mutate(번호 = row.names(.))



GGdream_address <- str_split(GGdream_Data$소재지지번주소, " ", simplify = T)




################### [상장법인 Data] ###################
mergedData_KOS <- readxl::read_excel("../data/상장법인/연구원님들/경기도 산업 리스트 통합.xlsx", sheet = "Sheet1", col_names = T, skip = 0) %>%
  mutate(번호 = row.names(.)) %>%
  rename(`상장기업 업종` = 업종)





################### [Mapping: 상장업종toTNFDsector] ###################
TNFD_mapping <- readxl::read_excel("../data/Mapping/TNFDMapping_total.xlsx", sheet = "상장업종toTNFD", col_names = T, skip = 0)
TNFD_mapping_big <- readxl::read_excel("../data/Mapping/TNFDMapping_total.xlsx", sheet = "TNFD대분류정의", col_names = T, skip = 0)
TNFD_mapping_med <- readxl::read_excel("../data/Mapping/TNFDMapping_total.xlsx", sheet = "TNFD중분류정의", col_names = T, skip = 0)

mergedData_KOS_with_TNFD <- mergedData_KOS %>%
  left_join(TNFD_mapping, by = c("상장기업 업종")) %>%
  left_join(TNFD_mapping_big, by = c("TNFD_대분류코드")) %>%
  left_join(TNFD_mapping_med, by = c("TNFD_중분류코드")) 


mergedData_KOS_with_TNFD_address <- str_split(mergedData_KOS_with_TNFD$지번주소, " ", simplify = T)



################### [Mapping: 데이터드림to상장기업업] ###################

matchNumber_All <- c()
matchNumberMatrix <- matrix(nrow = nrow(mergedData_KOS_with_TNFD_address), ncol = 10)
for ( i in 1:nrow(mergedData_KOS_with_TNFD_address)) {
  
  KOS_each <- mergedData_KOS_with_TNFD_address[i,]
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

final_sampleData <- mergedData_KOS_with_TNFD %>%
  left_join(matchNumberMatrix_onlyOne, by = c("번호")) %>%
  left_join(GGdream_Data %>% rename(V1 = 번호) %>% mutate(V1 = as.numeric(V1)), by = c("V1"))






###########################################################################################################################################################################

## SSN(486), JWH(445), WSY(537)
data_SSN <- readxl::read_excel("../data/상장법인/연구원님들/상장기업 검색_송새눈_240921.xlsx", sheet = "상장기업 리스트", col_names = T, skip = 0)
#   mutate(매출액 = as.numeric(str_squish(매출액)),
#          자산총계 = as.numeric(str_squish(자산총계)))
# 
# gsub("\\s", "", data_SSN$매출액)
# gsub("^#", "", data_SSN$매출액)
# str_squish(data_SSN$매출액)
# 
# as.numeric(gsub("\r\n", "", data_SSN$매출액))
# gsub(" ", "", data_SSN$매출액)


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














