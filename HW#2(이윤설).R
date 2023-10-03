####################################
# 과목명: 데이터애널리틱스
# 과제명: 2 - K군집분석
# 이름: 이윤설
# 학번: 2015113412
# 학과: 경영학부
####################################

# setwd 경로 설정
setwd("C:\\Users\\Yoon\\Desktop\\23-1학기\\데이터애널리틱스")

# teens 데이터프레임 생성성
teens <- read.csv("snsdata.csv")

# teens의 8~32열의 자료를 이용하여 interests 데이터프레임 생성
interests <- teens [8:32]
## scale을 적용하여 interests_z 생성
interests_z <- as.data.frame(lapply(interests, scale))

# kmeans를 이용하여, teen_clusters 생성(6군집)
teen_clusters <- kmeans(interests_z, 6)

## set.seed는 2023으로 지정
set.seed(2023)

# 본래 데이터 프레임에 군집ID 적용하여 cluster 생성
teens$cluster <- teen_clusters$cluster
## cluster 생성 후 3행부터 6행 까지 데이터 추출
teens[3:6, c("cluster", "friends", "music", "shopping")]

# 해석
teen_clusters$centers

# 1번 클러스터: softball, volleyball, sports에 높은 점수 -> Athletes 
# 2번 클러스터: cheerleading, cute, sexy, hot, dance, band, marching, mall에 높은 점수 -> Princesses
# 3번: swimming, baseball 에 높은 점수 -> Swimmer and baseballers
# 4번: 다 음수이고, 무관심한 학생들 -> basket cases
# 5번: kissed, sex, sports, music, rock에 높은 점수 -> criminals
# 6번: jesus, bible, god, church에 높은 점수 -> devoted
