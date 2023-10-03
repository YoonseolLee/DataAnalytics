library(tidyverse)
library(rvest)

# 크롤링할 html 코드
# product_name="AmazonFresh-Donut-Ground-Coffee-Medium"
# ASIN="B072K54HNS"
scrap_amazon <- function(product_name,ASIN,page_num) {
  url_reviews <- paste0("https://www.amazon.com/",product_name,"/product-reviews/",
                        ASIN,"/pagenumber=",page_num,".html")
  doc<-read_html(url_reviews)

  #data-hook 또는class로 불러오기
  ##Review Date
  doc %>% 
    html_nodes('[data-hook="review-date"]') %>% 
    html_text()->Data
  
  ##Review Title
  doc %>% 
    html_nodes('[class="a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold"]') %>% 
    html_text()->Title
  
  ##Review Text
  doc %>% 
    html_nodes('[data-hook="review-body"]') %>% 
    html_text()->Review
  
  ##Number of Stars in Review
  doc %>% 
    html_nodes('[data-hook="review-star-rating"]') %>% 
    html_text()->Rating
  
  ## Verification
  doc %>% 
    html_nodes('[data-hook="avp-badge"]') %>% 
    html_text()->Verification
  
  #Return a tibble : dataframe 생성
  tibble(Data, Title, Review, Rating,Verification, Page=page_num) %>% 
    return()
}

## page 별 크롤링 시작
# product_name="AmazonFresh-Donut-Ground-Coffee-Medium"
# ASIN="B072K54HNS"
# 스크랩 시작
review_all <- vector("list", length=5)
for(i in 1:5) {
  review_all[[i]] <- scrap_amazon(product_name="AmazonFresh-Donut-Ground-Coffee-Medium",
                                  ASIN = "B072K54HNS", page_num=1)}
# review_all 내용을 rbind를 이용하여 한줄씩 리뷰를 저장
amazon <- do.call(rbind,review_all)

#####텍스트 전처리#####

#Rating에서 ".out of 5 stars" 지우기(gsub) 
#첫번째 값을 두번째 값으로 변경, 세번째 값에서 가져옴
#점수만 확인
amazon$Rating<-gsub(" out of 5 stars", "", amazon$Rating)
#Data에서 국가 및 날짜 나누기
# " on"과 "on "을 기준으로 문장을 1,2로 나누기
#country와 date 생성
amazon$Country<-strsplit(amazon$Data," on")[[1]][1]
amazon$Date<-strsplit(amazon$Data,"on ")[[1]][2]  
amazon$Country<-gsub("Reviewed in ", "", amazon$Country)

#Data 속성 지우기
amazon <- amazon[,-1]

#pattern을 이용하여 newline을 의미하는 "in"을 지우고,
#두칸 white space(맨 앞에 있는 공백) 없애기
amazon$Title <- gsub(pattern="\\n","",amazon$Title)
amazon$Review <- gsub(pattern="\\n","",amazon$Review)
amazon$Title <- gsub(pattern="  ", "", amazon$Title)
amazon$Review <- gsub(pattern="  ","",amazon$Review)

#write.csv
#csv 저장하기
write.csv(amazon,file="amazoncoffee.csv", row.names=T)

