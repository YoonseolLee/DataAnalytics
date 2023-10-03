###################################
# 과목명: 데이터애널리틱스
# 과제명: 3 - 계층적군집분석
# 이름: 이윤설
# 학번: 2015113412
# 학과: 경영학부
###################################

# mtcars를 mt.df로 불러오기
mt.df <- data.frame(mtcars)
mt.df

# 유클리디안 거리 계산
d <- dist(mt.df, method = "euclidean")
d

# scale 함수를 이용한 mt.df.norm 생성(정규화)
mt.df.norm <- sapply(mt.df, scale)
mt.df.norm

# mt.df.norm에 대한 유클리디안 거리 행렬 d.norm 출력
d.norm <- dist(mt.df.norm, method = "euclidean") 
d.norm

# d.norm에 대한 덴드로그램 출력
hc1<-hclust(d.norm, method="single")

hc2<-hclust(d.norm, method="complete")

hc3<-hclust(d.norm, method="average")

hc4<-hclust(d.norm, method="median")

# k=3일 때를 plot에 표시
plot(hc1, hang=-1, ann=FALSE)
plot(hc2, hang=-1, ann=FALSE)
plot(hc3, hang=-1, ann=FALSE)
plot(hc4, hang=-1, ann=FALSE)

plot(hc2, hang=-1, ann=FALSE)

rect.hclust(hc2, k=3)

# 덴드로그램을 절단하여 군집을 mt.df.norm 자료의 행 이름에 반영
membership <- cutree(hc2, k=3)
membership
row.names(mt.df.norm) <- paste("(",membership, ")", row.names(mt.df),
                                     sep="")
mt.df.norm

# Heatmap 작성 및 결과 분석 진행
heatmap(as.matrix(mt.df.norm), Colv=NA, hclustfun=hclust, 
        col=rev(paste("grey",1:99,sep="")))

# 연비(mpg)는 멤버쉽 2에서 비교적 높게 나타나므로, 
# 해당 군집에 속한 차량들의 연비가 높은 편입니다. 

# 엔진의 기통수(cyl)은 멤버쉽 3에서 높게 나타나므로,
# 해당 군집에 속한 차량들이 부드러운 주행감을 제공할 수 있습니다.

# 배기량(disp)과 마력(hp)은 멤버쉽 3에서 높게 나타나므로,
# 해당 군집에 속한 차량들이 높은 가속성과 최고 속도를 제공할 수 있습니다.

# 뒤차축비(drat)은 멤버쉽 2에서 높게 나타나고, 
# 무게(wt)는 멤버쉽 3에서 높게 나타나므로
# 멤버쉽2 군집에 해당하는 차량들은 주행 중에 차량의 뒷바퀴에서 더 많은 힘이 발생하고,
# 멤버쉽3 군집에 해당하는 차량들은 비교적 무겁습니다.

#1/4mile 도달시간(qsec)과 V/S(vs)은 멤버쉽 2에서 높게 나타나므로,
# 해당 군집에 속한 차량들이 빠른 가속성과 좋은 주행 안정성을 제공하는 차량일 수 있습니다.

# 변속기어(am)과 전진기어 갯수(gear)는 멤버쉽 1,2에서 높게 나타나므로,
# 해당 군집에 속한 차량들이 변속 비율의 범위가 넓어지므로 
# 차량의 가속성이 우수할 수 있습니다.

# 기화기 갯수(carb)는 멤버쉽 1에서 높게 나타나므로,
# 이는 차량의 공기와 연료의 혼합이 균일해지도록 돕기 때문에, 
# 해당 군집에 속한 차량들의 연소가 더욱 깨끗하고 효율적으로 이루어질 수 있습니다.