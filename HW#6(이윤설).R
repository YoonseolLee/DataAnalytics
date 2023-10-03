###############################
#과목명: 데이터 애널리틱스
#과제명: 6-텍스트마이닝
#이름: 이윤설
#학번: 2015113412
#학과: 경영학부
###############################

library(tm)
library(topicmodels)

#csv 불러오기
setwd("C:\ba")
amazon <- read.csv("amazoncoffee.csv", stringsAsFactors = FALSE)

#title 확인
amazon$Title[1:3]

#multibyte 문제를 방지하기 위해 인코딩 변환 
#amazon의 리뷰를 utf-8로 변환
#iconv() : 문자 벡터 인코딩 변환 함수
ecodot <- iconv(enc2utf8(amazon$Review), sub="bytes")

## 1. 데이터 전처리
## 영어 text로 1차 전처리
ecodot[28]

ecodot <- gsub("[\U4E00-\U9FFF\U3000-\U303F]","",ecodot)
ecodot <- iconv(ecodot, from = "latin1", to="ASCII", sub="")
ecodot[28]

corpus <- VCorpus(VectorSource(ecodot))

inspect(corpus[[28]])

# TDM
#현재의 언어상태 확인
Sys.setlocale()

Sys.setlocale(category= "LC_ALL", locale="us")

tdm <- TermDocumentMatrix(corpus, control=list(
               removePunctuation = T,
               stopwords= 'SMART',
               tolower = T,
               removeeNumbers = T,
               wordLengths =c(3,7),
               stemming = F,
               stripWhitespace= T,
               weighting = weightTfIdf))
# 만든 tdm의 차원 확인
dim(tdm)

## 3. LSA

# lsa 라이브러리 불러오기
library(lsa)

# tdm의 list 형태 -> matrix -> textmatrix
amazon_TM <- as.textmatrix(as.matrix(tdm))

#amazon_TM을 10개의 차원으로 LSA 만들기
amazon_lsa <- lsa(amazon_TM, dim=7)

# Terms x New LSA Space (U K)
dim(amazon_lsa$tk)

# Documents X New LSA space (V K)
dim(amazon_lsa$dk)

# Singular values (Sigma K)
length(amazon_lsa$sk)

library(lsa)
data(lsa_dk)

# 이미지 확인
plot(lsa_dk)
