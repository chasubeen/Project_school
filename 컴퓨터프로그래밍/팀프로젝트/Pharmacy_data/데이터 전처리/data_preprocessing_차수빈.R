### 데이터 파일 읽기
getwd() # 현재 작업 경로 확인하기
setwd("C:/waterbean/Ewha/comp/project") # 작업 경로 잡아주기
df1 <- read.csv("seoul_pharmacy.csv")
View(df1) # 스프레드시트 형태로 데이터를 보여줌
class(df1) # 읽어온 데이터 파일은 data.frame 형태임


### 주소에서 구만 선택하여 저장하기
addr_li<- strsplit(df1$주소,split = " ")

# 문자열 슬라이싱 결과 list 형태로 저장됨 -> 각각의 벡터 전체에서 두 번째 내부 원소가 구이다.

search <- function(x){ # 두 번째 요소 찾기
  x[2]
}
# sapply를 이용하여 두 번째 요소값만을 벡터형으로 가져오기
df1$행정구 <- sapply(addr_li,search)
View(df1)

### 필요힌 정보만을 가져오기
# 가져와야 할 컬럼의 수 > 없앨 컬럼의 수
# 기존의 data frame에서 약국ID, 우편번호1/2, 병원경도/위도, 작업일시 컬럼 삭제
df2 <- df1[,-c(1,21:25)]
View(df2)

### 보기 좋은 형태로 컬럼 순서 변경하기
df3 <- df2[,c(2,1,3,20,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19,11)]
View(df3)

### 컬럼명을 보기 좋게 변경하기
new_colname <- c("약국명","주소","대표전화","행정구","진료시작(월)","진료마감(월)","진료시작(화)","진료마감(화)","진료시작(수)","진료마감(수)","진료시작(목)","진료마감(목)","진료시작(금)","진료마감(금)","진료시작(토)","진료마감(토)","진료시작(일)","진료마감(일)","진료시작(공휴일)","진료마감(공휴일)")
colnames(df3) <- new_colname
View(df3)

### 지역 선택하기
# 생각해보니 굳이 나눌 필요가...있나...?
# 나중에 알고리즘 짤 때 사용자가 제공한 정보에 따라서 맞춰서
# 제공해주면 되는 거 아닐까

gu <- c("동대문구","동작구","마포구","서대문구","서초구")
df4 <- df3[which(df3$행정구%in%gu),]
View(df4)

### 공휴일 운영 여부
df4$공휴일 <- NA # 공휴일 컬럼을 NA로 설정
View(df4)
df4[is.na(df4$`진료시작(공휴일)`),"공휴일"] <- FALSE
df4[is.na(df4$`진료마감(공휴일)`),"공휴일"] <- FALSE
df4[!is.na(df4$`진료시작(공휴일)`),"공휴일"] <- TRUE
df4[!is.na(df4$`진료마감(공휴일)`),"공휴일"] <- TRUE
View(df4)

### 야간운영정보
# 야간운영 정보 여부만을 저장하는 data frame을 생성하기
night <- data.frame(matrix(nrow = nrow(df4),ncol = 8)) # 빈 df 생성
day_night <- c("야간진료(월)","야간진료(화)","야간진료(수)","야간진료(목)","야간진료(금)","야간진료(토)","야간진료(일)","야간진료(공휴일)")
colnames(night) <- day_night

# 각 요일의 각 약국(행)에 대해 야간운영 정보 확인
opened <- function(x){
  ifelse((700 <= x) && (x <= 2030),FALSE,TRUE)
}
night$`야간진료(월)` <- sapply(df4$`진료마감(월)`,opened)
night$`야간진료(화)` <- sapply(df4$`진료마감(화)`,opened)
night$`야간진료(수)` <- sapply(df4$`진료마감(수)`,opened)
night$`야간진료(목)` <- sapply(df4$`진료마감(목)`,opened)
night$`야간진료(금)` <- sapply(df4$`진료마감(금)`,opened)
night$`야간진료(토)` <- sapply(df4$`진료마감(토)`,opened)
night$`야간진료(일)` <- sapply(df4$`진료마감(일)`,opened)
night$`야간진료(공휴일)` <- sapply(df4$`진료마감(공휴일)`,opened)

# 원래의 dataframe과 열 방향으로 합치기
df4 <- cbind(df4,night)
View(df4)

# csv 파일로 내보내기
write.csv(df4, "C:/waterbean/Ewha/comp/project/seoul_pharmacy_3.csv",
          row.names = FALSE)


















































