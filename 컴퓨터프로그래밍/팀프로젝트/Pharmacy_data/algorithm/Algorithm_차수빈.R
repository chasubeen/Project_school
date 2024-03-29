### 전처리된 데이터 파일 가져오기
df1 <- read.csv("seoul_pharmacy_final.csv")
# 글꼴 깨짐 문제 발생(컬럼명에서 괄호가 사라진다....ㅇㅁㅇ...)
# 컬럼명 재설정
new_colname <- c("약국명","주소","대표전화","행정구","시작(월)","마감(월)","시작(화)","마감(화)","시작(수)","마감(수)","시작(목)","마감(목)","시작(금)","마감(금)","시작(토)","마감(토)","시작(일)","마감(일)","시작(공휴일)","마감(공휴일)","일요일운영","공휴일운영","야간운영(월)","야간운영(화)","야간운영(수)","야간운영(목)","야간운영(금)","야간운영(토)","야간운영(일)","야간운영(공휴일)")
colnames(df1) <- new_colname
View(df1)

### 입력
# 1. 지역: 사용자에게 '직접' 입력받는다.
place <- readline(prompt = "검색하고자 하는 지역을 행정구 단위로 입력해 주세요 : ")
cat("사용자가 입력한 지역은",place,"입니다.")

# 2. 현재 시간
curtime <- as.character(strsplit(date()," ")[[1]][4]) 
# 시간 부분만을 가져와서 문자열로 형변환
curtime <- as.numeric(substr(gsub(":","",curtime),1,4))
# gsub(a,b,data): data 내에 포함된 a 문자를 b 문자로 변경
# 시각에서 초를 제외한 시간, 분 추출 -> numeric으로 변경
# 530 <= (현재 시각) <= 2929가 되도록 형식 맞춰주기 
curtime <- ifelse((curtime<530),curtime + 2400,curtime)

# 3. 현재 요일
curday <- strsplit(weekdays(Sys.Date()),"")[[1]][1] 
# 한글자씩 나누고 제일 앞의 글자 가져오기("요일" 날려버리기)

# 4. 제공받을 정보 
# 옵션 선택하기
option = readline(prompt = "어떤 정보를 얻고 싶으신가요?\n1) 현재 운영중인 약국 2) 요일별 운영정보 3) 인근 약국 >> ")


### 출력
# 사용자가 선택한 옵션에 따라 적절한 정보를 제공한다.

switch(option,
       "1" = {
         
       },
       "2" = {
         
       },
       "3" = {
         
       }
       )

























# df[,c()] -> c() 해줄 때 컬럼명 직접 써주기
# 컬럼명을 가져올 때 인덱스로 가져오는 방법과 이름을 직접 명시하는 방법은 동시에 사용할 수는 없다.


### 출력
# 영업시간 구하기
working <- function(day,time){ 
  start = paste0("시작(",day,")")
  end = paste0("마감(",day,")")
}






















































