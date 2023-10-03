#################################
#과목명:데이터애널리틱스
#과제명:4-자기조직화지도
#이름:이윤설
#학번:2015113412
#학과:경영학부
#################################

# iris 데이터 불러오기
data(iris)
data <- iris

# 변수 설정
data_train <- data[, c(1,2,3,4)]

# 데이터 스케일링
data_train_matrix <- as.matrix(scale(data_train))

# somgrid 생성
som_grid <- somgrid(xdim=10, ydim=10, topo="hexagonal")

# somgrid를 이용하여 som 생성
set.seed(31)
som_model <- som(data_train_matrix,
                 grid=som_grid,
                 rlen=50,
                 alpha=c(0.005,0.001),
                 keep.data=TRUE)

# hclust와 cutree 함수를 이용하여 somgrid 생성
som_cluster <- cutree(hclust(dist(getCodes(som_model))), 3)

# 팔레트 불러오기
pretty_palette <- c("#21C2FF", '#FFAE21', '#00FF32',
                    '#E12727', '#A27FDB', '#B34507', '#FF31FF')

# plot 생성 + 경계 구분 선 추가
plot(som_model, type="codes", bgcol = pretty_palette[som_cluster],
     main = "2015113412_이윤설")
add.cluster.boundaries(som_model, som_cluster)













