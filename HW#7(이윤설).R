########################
#과목명:데이터애널리틱스
#과제명:7-토픽모델링
#이름: 이윤설
#학번: 2015113412
#학과: 경영학부
########################

# 라이브러리 로딩
library(topicmodels)
library(tidytext)
library(tidyr)
library(ggplot2)
library(dplyr)

# DTM이 내장된 amazonDTM.Rdata 불러오기
setwd("C:\ba")
## load( file = "경로\\파일명.RData" )
load(file="C:\\ba\\amazonDTM.RData")

# amazonDTM을 이용하여 LDA 만들기
amz_lda <- LDA(AssociatedPress,
              k=5,
              method="Gibbs",
              control=list(seed=1234))
amz_lda

# 베타 탐색

# tidy(): LDA 모형 결과 확인
# 구축된 모형으로부터 beta(토픽 별 단어 확률분포)도출
# 도출 기준: beta / gamma(문서 별 토픽 확률분포)
amz_topics <- tidy(amz_lda, matrix="beta")
amz_topics

# 토픽 별 베타 정렬
amz_top_terms <- amz_topics %>% 
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

amz_top_terms

#ap_top_terms를 사용하여 ggplot 그리기
amz_top_terms %>%
  mutate(term=reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE)+
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

geom_col(show.legend = TRUE) # 범례 



