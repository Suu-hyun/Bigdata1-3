''' 25 - 05 - 10 강의 자료'''

'''
데이터 파악 함수
1. head()
2. tail()
3. View() -> 잘 안 씀(환경에서 클릭,V대문자)
4. dim() : 차원 출력
5. str() : 데이터 속성 출력
6. summary() : 요약통계량 출력(문자열x)

데이터 변수명 수정 함수 : rename(데이터명, 새 변수명 = 기존 변수명)
'''

# --------------------------------------------------------------------------

### 파생변수 생성 - 기존의 데이터를 활용해서 새로운 데이터를 생성성
df <- data.frame(var1 = c(4,3,8), # obs는 행의 갯수, variables은 변수의 갯수
                 var2 = c(2,6,1))
df

df$var_sum <- df$var1 + df$var2 # 변수 접속자 $(변수 df에 접속속)
df

df$var_mean <- (df$var1 +df$var2) / 2 # 평균균
df

# mpg 데이터에 통합 연비 변수 생성(평균 연비)
mpg <- as.data.frame(ggplot2 :: mpg) # ggplot2 안에 있는 데이터인 mpg불러오기
mpg

mpg$total <- (mpg$cty + mpg$hwy)/2 # 통합합 연비 변수 생성, 수치형 데이터만 가능
head(mpg) # 전체적으로 간단하게 볼 수 있는 것(상위 몇개 이상만)
mpg$total # total만 보고 싶을 경우
mean(mpg$total) # total연비의 평균을 보고 싶을 때

# cty는 도시연, hwy는 고속도로 연비

### 조건문을 활용해 파생변수 생성(조건문과 반복문이 코딩의 꽃)
summary(mpg$total) # mpg의 total(평균)을 통계요약으로 볼 때
hist(mpg$total) # 히스토그램으로 확인

## 기준 20으로 잡고 합격 판정 변수 생성 - ifelse(조건, 조건에 맞을 때 부여, 조건에 맞지 않을 때)
mpg$test <- ifelse(mpg$total >= 20, 'pass', 'fail') # 평균 연비로 pass, fail로 구분
head(mpg, 20) # 상위 20개 출력

# 빈도표로 합격 판정 자동차 수 확인
table(mpg$test) # table은 빈도표를 만드는 함수수

# 막대그래프에 빈도 표현
library(ggplot2) # ggplot2를 활성화
qplot(mpg$test) # qplot은 빈도 그래프(막대)

### 중첩 조건문 활용 - 연비 등급 생성
'''
등급을 total 변수 기준
A : 30 이상
B : 20~29
C : 20 미만
'''

mpg$grade <- ifelse(mpg$total >= 30, 'A', # 30 이상일 때 A
                    ifelse(mpg$total >= 20, 'B', 'C')) # 20이상일때 B, 그 외에는 C
head(mpg, 20) # head를 통해 상위 20개를 미리 보기
table(mpg$grade) # a,b,c의 빈도를 간단히 확인
qplot(mpg$grade) # 막대 그래프로 빈도 확인

'''
ggplot2 패키지에는 미국 동북중부 437개 지역의 인구통계 정보를 담은
midwest라는 데이터가 포합되어 있다. 이 데이터를 사용해 데이터 분석

Q1. ggplot2의 midwest 데이터를 데이터 프레임 형태로 불러와서 특성 파악

Q2. poptotal(전체인구)를 total로, popasian(아시아인구)을 asian으로 변경

Q3. total, asian 변수를 사용해서 전체 인구대비 아시아 인구 백분율 파생변수를
    만들고, 히스토그램을 만들어 도시들이 어떻게 분포하는지 확인
    
Q4. 아시아 인구 백분율 전체 평균을 구하고, 평균을 초과하면 "large", 그 외에는
    "small"을 부여하는 파생변수를 생성하세요.
'''
# 1.
midwest <- as.data.frame(ggplot2 :: midwest) # ggplot2에 있는 midwest 데이터 가져오기
str(midwest) # 데이터 특성 파악

# 2.
library(dplyr) # dplyr 활성화(rename함수 사용을 위해서)
midwest <- rename(midwest, total = poptotal) # poptotal울 total로 변수명 변경
midwest <- rename(midwest, asian = popasian)

# 3.
midwest$ratio <- midwest$asian/midwest$total*100 # 아시안/전체,백분율을 위해 *100
hist(midwest$ratio) # 백분율로 표현된 막대 그래프프

# 4.
mean(midwest$ratio) # 0.4872462
midwest$group <- ifelse(midwest$ratio > 0.4872462, 'large', 'small') # 평균보다 크면 large, 작으면 small
table(midwest$group) # large와 small의 빈도 수를 확인인
qplot(midwest$group) # 막대 그래프로 표현현(large, small)

# -------------------------------------------------------------------------------
''' 데이터 가공 - 원하는 형태로 데이터 다듬는다'''
'''
데이터 전처리 - dplyr 패키지 함수들

1. filter() : 행 추출 ex) mpg데이터에서 audi만 뽑을 때
2. select() : 열 추출 
3. arrange() : 정렬
4. mutate() : 변수 추가(파생변수)
5. summarise() : 통계치 산출
6. group_by() : 집단별로 나누기
7. left_join() : 데이터 합치기(열 기준)
8. bind_rows() : 데이터 합치기(행 기준) 
'''

### 조건에 맞는 데이터만 추출
# dplyr 패키지 로드 & 데이터 준비
install.packages('dplyr') # 파이프 기호 사용을 위해해
library(dplyr)
exam <- read.csv('csv_exam.csv') # files안에 exam 데이터를 불러올 때
exam

# 데이터에서 1반인 경우만 추출해서 출력 - filter를 사용
exam %>% filter(class == 1) # dplyr 사용할 때 파이프 기호를 주로 사용
''' 파이프 기호(%>%) 단축키 : ctrl + shift + M '''

# 1반이 아닌 경우
exam %>% filter(class != 1) # != 는 다르다 라는 연산자

## 초과, 미만, 이상, 이하 조건
# 수학 점수가 50점을 초과한 경우
exam %>% filter(math> 50)

## 여러 조건을 충족하는 행 추출

# 1반에서 수학점수가 50점 이상인 학생
exam %>% filter(class == 1 & math >= 50) # class가 1인데, 1에서 math가 50점이상

## 여러 조건 중 하나 이상 충족하는 행 추출

# 수학 점수가 90점 이상이거나 영어점수가 90점 이상인 학생
exam %>% filter(math >= 90 | english >= 90) # | : 버티컬 바 (or)

# 1, 3, 5 반만 추출
exam %>% filter(class == 1 | class == 3 | class == 5)

## 목록에 해당하면 추출 - %in% 기호

# 1,3,5반만 추출
exam %>% filter(class %in% c(1,3,5)) # class에서 1,3,5반만 추출

## 추출한 행으로 데이터 생성
class1 <- exam %>% filter(class == 1) # class 1 만 추출 후 변수 지정
mean(class1$english) # 1반 학생들의 영어평균 점수

'''
R에서 사용되는 기호들

- 논리 연산자

<  : 작다
<= : 작거나 같다
== : 같다
!= : 같지 않다
$  : 그리고
%in% : 매칭 확인 (포함 연산자)

- 산술 연산자
+, - : 더하기, 빼기
* : 곱하기
/ : 나누기
**, ^ : 제곱
%/% : 나눗셈의 몫
%% : 나눗셈의 나머지
'''

''' Q. 자동차 제조 회사에 따라 도시 연비가 다른지 알아보려 한다.
  "audi"와 "toyota" 중 어느 manufacturer(제조사)의 cty(도시연비)가
  평균적으로 높은지 알아보세요. '''

audi <- mpg %>% filter(manufacturer == 'audi') # 제조사 audi만(행)
toyota <- mpg %>% filter(manufacturer == 'toyota') # 제조사 toyota만

mean(audi$cty) # 아우디의 도시연비 평균
mean(toyota$cty) # 토요타의 도시연비 평균

### 필요한 변수만 추출 - select()

exam %>%  select(math) # 수학 점수(열)만 뽑을 때

## 여러 변수 추출
exam %>% select(class, math, english) # 반과 수학, 영어 점수

## 변수 제외
exam %>% select(-math) # 수학만 제외

### 전처리 패키지 dplyr 함수 조합

# 1반 학생들의 영어 점수만 추출
exam %>% filter(class == 1) %>% select(english)

# 가독성 있게 줄 바꿈
exam %>% 
  filter(class == 1) %>% 
  select(math, english)

exam %>%
  select(id, math) %>% 
  head()

### 순서대로 정렬 - arrange()

## 오름차순 정렬(기본값)
exam %>%arrange(math)

## 내림차순 정렬 - desc라는 함수를 사용
exam %>% arrange(desc(math))

## 정렬 기준 변수 여러개 지정
exam %>% arrange(class, math) # class를 정렬한 후 math정렬
exam

### 파생변수 추가 - mutate() -> 원본에는 영향 x(영향 주려면 변수 지정)
exam %>%
  mutate(total = math + english + science) %>% # total이라는 파생변수
  head

## 여러 파생변수 한 번에 추가
exam %>% 
  mutate(total = math + english + science,
         mean = (math + english + science)/3) %>% 
  head

## mutate에 ifelse 적용하기
exam %>% 
  mutate(test = ifelse(science > 60, 'pass', 'fail')) %>% 
  head

## 추가한 변수를 dplyr 코드에 바로 적용
exam %>% 
  mutate(total = math + english +science) %>% 
  arrange(desc(total)) %>% 
  head(10)

### 집단별로 요약 - group_by(), summarise()

# 반별 수학점수 평균
exam %>% 
  group_by(class) %>% # 반별로 나눠서서
  summarise(mean_math = mean(math)) # 수학 평균을 통계 요약

# summarise(data, 출력에 사용할 변수명 = 활용 함수명(변수명))

## 여러 요약 통계량 한 번에 산출
exam %>% 
  group_by(class) %>% 
  summarise(mean_math = mean(math), # 평균
            sum_math = sum(math), # 합계
            median_math = median(math), # 중앙값
            n = n()) # 학생 수(행의 개수를 세는데 어떤 변수를 넣어도 똑같아서 괄호 안에 비워도됨 )

''' 자주 사용하는 요약통계량 함수
1. mean() : 평균
2. sd() : 표준편차
3. sum() : 합계
4. median() : 중앙값
5. min(), max() : 최소, 최댓값
6. n() : 빈도 (행의 개수)

summarise() 함수는 데이터 요약을 계산할 때 사용, dplyr 패키지
핵심 함수 중 하나, 데이터 프레임을 요역해 단일 값 또는 각
그룹에 대한 통계 요약 제공
group_by()와 함께 쓰지 않으면 전체 데이터에 대해 요약
group_by()를 쓰면 그룹별 요약 통계 계산
새로운 요약 변수 생성
'''

## 각 집단별로 다시 집단 나누기 - 제조사별 자동차의 구동방식별 평균 도시 연비

mpg %>% 
  group_by(manufacturer, drv) %>% # 제조사로 나눠주고 구동박식으로 나눠줌
  summarise(mean_cty = mean(cty), # 도시 연비 평균
            car_count = n()) %>% # 자동차 갯수
  head(15)


''' Q. 회사별로 - group_by
"suv" 자동차의 - filter
도시 및 고속도로 통합 연비 - mutate
평균을 구해 - summarise
        내림차순으로 정렬하고, - arrange
연비 1~5위까지 출력  - head'''

mpg %>% 
  group_by(manufacturer) %>% # 제조사별로 나눠주고
  filter(class == 'suv') %>% # class가 suv인 것만 추출
  mutate(tot = (cty+hwy)/2) %>%  # 통합 연비 파생변수 생성
  summarise(mean_tot = mean(tot)) %>% # 통합 연비의 평균에 대한 
  arrange(desc(mean_tot)) %>% # 내림차순으로 정렬렬
  head(5)

### 데이터 합치기

## 가로로 합치기
# 중간고사
test1 <- data.frame(id = c(1,2,3,4,5),
                    midterm = c(60,80,70,90,85))
# 기말고사
test2 <- data.frame(id = c(1,2,3,4,5),
                    final = c(70,83,65,95,80))
test1
test2

total_test <- left_join(test1, test2, by='id') # id 기준으로 합치기, 왼쪽으로 join
total_test

## 다른 데이터 활용해서 변수 추가 - 매칭

exam

# 반별 담임교사 명단 생성
name <- data.frame(class = c(1,2,3,4,5),
                   teacher = c('KIM','LEE','PARK','CHOI','JUNG'))
name
exam

# class 기준 합치기
exam_new <- left_join(exam, name, by='class')
exam_new

## 세로로 합치기

# 면접자 1~5번
group_a <- data.frame(id = c(1,2,3,4,5),
                      test = c(60,80,70,90,85))
group_b <- data.frame(id = c(6,7,8,9,10),
                      test = c(70,83,98,95,87))
group_a
group_b

group_all <- bind_rows(group_a, group_b) # 두 데이터프레임의 레코드를 병합하여 출력해주는 함수(행단위로)
group_all

# ---------------------------------------------------------------------------
''' 데이터 정제 - 빠진 데이터, 이상한 데이터 제거 '''

''' 결측차 정제 - 빠진 데이터를 찾는 것

- 결측치(Missing value) : 누락된 값, 비어있는 값
  -> 함수 적용 불가, 분석 결과 왜곡
  -> 제거 후 분석 실시
'''

### 결측치 찾기
''' 결측치 표기 - 대문자 NA'''

df <- data.frame(s = c('M','F', NA, 'M','F'), # s는 성별
                 score = c(5,4,3,4,NA)) # 만족도 조사 점수
df

# 결측치 확인 - is.na()
is.na(df) # 결측이냐 아니냐(TRUE, FALSE)
is.na(mpg)

# 결측치 빈도 출력 - table() 와 함께 적용 -> 보기 좋음
table(is.na(df))

# 변수별로 결측치 확인
table(is.na(df$s))
table(is.na(df$score))

mean(df$score) # 결측치 있어서 결과가 나오지 않음


### 결측치 제거

# 결측치가 있는 행 제거
df %>% filter(is.na(score)) # score 예시 NA인 데이터만 출력

df %>% filter(!is.na(score)) # 결측치 제거

# 제거 후 분석
df_nomiss <- df %>% filter(!is.na(score))
df_nomiss

mean(df_nomiss$score) # 결측치가 제거되닌 결과값이 나옴

# 여러 변수 동시에 결측치 없는 데이터 추출
df_nomiss <- df %>% filter(!is.na(score) & !is.na(s)) # 결측치가 아닌 거만 뽑음
df_nomiss

# 결측치가 하나라도 있으면 제거
df_nomiss2 <- na.omit(df) # 모든 변수에 결측치 없는 데이터 추출
df_nomiss2
''' 분석에 필요한 데이터까지 손실 될 가능성 유의'''

# 함수의 결측치 제외 기능 활용 - na.rm = T 옵션 - rm은 romove의 줄임말
mean(df$score, na.rm = T) # 결측치 제외하고 평균 산출
sum(df$score, na.rm = T)


# summarise 안에서 na.rm = T 활용
exam[c(3,8,15),'math'] <- NA # exam에 3,8,15 수학 점수를 결측치로 지정
exam

exam %>%  summarise(mean_math = mean(math, na.rm = T)) # na.rm을 사용해서 결측치 제거

''' 결측치 대체하기

- 결측치가 많다면 모두 제외하면 데이터 손실 큼

- 결측치 대체법
1. 대표값(평균, 최빈값 등) 으로 일괄 대체
2. 통계분석 기법 적용, 예측값 추정해서 대체

'''

### 평균값으로 결측치 대체
mean(exam$math, na.rm = T) # 결측치를 제외한 평균을 구함

exam$math <- ifelse(is.na(exam$math), 55, exam$math) # math가 NA면 평균인 55로 결측치를 대체함
table(is.na(exam$math)) # 결측치가 있는지 확인인

exam

mean(exam$math)

''' 이상한 데이터 - 이상치 정제

- 이상치(Outlier) : 정상범주에서 크게 벗어난 값
  -> 이상치 포함시 분석 결과 왜곡
  -> 결측 처리 후 제외하고 분석
  
  
- 이상치 종류
1. 존재할 수 없는 값 ex) 성별 변수에 3 / 결측 처리
2. 극단적인 값 ex) 몸무게 변수에 250 / 정상범위 기준 정해서 결측 처리

'''
### 이상치 제거 - 1. 존재할 수 없는 값

outlier <- data.frame(s = c(1,2,1,3,2,1),
                      score = c(5,4,3,4,2,6))
oulier

### 이상치 확인
table(outlier$s) # 1,2인데 3이 존재
table(outlier$score) # 5까지 인데 6이 있음


## 결측 처리 - s, score
outlier$s <- ifelse(outlier$s == 3, NA, outlier$s) # 3일 경우 NA
outlier

outlier$score <- ifelse(outlier$score > 5, NA, outlier$score) # 5보다 크면 NA
outlier

### 제외하고 분석
outlier %>% 
  filter(!is.na(s) & !is.na(score)) %>% # 결측치가 아닌 경우추출
  group_by(s) %>% 
  summarise(mean_score = mean(score))

### 이상치 제거 - 2. 극단적인 값
'''
정상 범위 기준을 정해서 벗어나면 결측 처리

판단기준
1. 논리적 판단 ex) 성인 몸무게 40~150 벗어나면 극단치로 간주
2. 통계적 판단 ex) 상하위 0.3% 극단치 또는 상자 그림 1.5IQR 벗어나면 극단치
'''

## 상자 그림으로 극단치 기준 정해서 제거
mpg <- as.data.frame(ggplot2::mpg)
boxplot(mpg$hwy) # boxplot이 상자그림부르는 함수, 휴대폰 화면 녹화 자료로 상자 그림 확인

## 상자 그림 통계치 출력
boxplot(mpg$hwy)$stats
'''
      [,1]
[1,]   12 -> 최솟값 -> 중앙값 - 1.5IQR 보다 큰 데이터 중 가장 작은 값
[2,]   18 -> 1사분위수
[3,]   24 -> 2사분위수
[4,]   27 -> 3사분위수
[5,]   37 -> 최댓값 -> 중앙값 + 1.5IQR 보다 작은 데이터 중 가장 큰 값

1.5IQR: 1.5*(3사분위수 - 1사분위수)

'''

## 결측 처리 - 12 ~ 37 벗어나면 이상치로 간주하고 NA 할당
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy) # 12미만 37초과면 NA할당
table(is.na(mpg$hwy)) # NA빈도확인

## 결측치 제외하고 분석
mpg %>% 
  group_by(drv) %>% # drv으로 그룹을 나눔
  summarise(mean_hwy = mean(hwy, na.rm=T)) # 이상치를 결측치로 만들고 제외시킨 값이 더 정확함

# 수치형 데이터일 경우 가능, 문자형은 불가하다.

# -------------------------------------------------------------------------

''' 그래프

- 2차원 그래프, 3차원 그래프
- 지도 그래프
- 네트워크 그래프
- 모션 차트
- 인터랙티브 그래프


- ggplot2의 레이어 구조 이해

1단계 : 배경 설정(축)
2단계 : 그래프 추가(잠, 막대, 선 ...)
3단계 : 설정 추가 (축 범위, 색깔, 표식)
'''

### 산점도(Scatter plot) : 데이터를 x축과 y축에 점으로 표현한 그래프
''' 
나이와 소득처럼 연속된 값으로 된 두 변수의 관계를 표현할 때 사용
'''

library(ggplot2) # ggplot2 패키지 장착

# 1. 배경 설정 - x축 displ (배기량), y축 hwy(고속도로연비) 지정해 배경 생성
ggplot(data=mpg, aes(x=displ, y=hwy))

# 2. 그래프 - 산점도
ggplot(data=mpg, aes(x=displ, y=hwy)) + geom_point() # 산점도(point)그래프 추가(우하향->반비례)

# 3. 축범위 조정하는 설정 추가
ggplot(data=mpg, aes(x=displ, y=hwy)) +
  geom_point() +
  xlim(3,6) # x축에 3,6만큼 범위 조정

'''
qplot vs ggplot

qplot : 전처리 단계에 데이터 확인용, 문법 간단, 기능 단수
ggplot : 최종 보고용, 색 크기 폰트 세부 조작 가능
'''

### 막대그래프 (Bar chart) : 데이터의 크기를 막대의 길이로 표현한 그래프
'''
집단 간 차이를 표현할 때 많이 쓰임

성별 소득 차이 등..
'''

## 막대 그래프 1 - 평균 막대 그래프
# 각 집단의 평균값을 막대의 길이로 표현한 그래프

df_mpg <- mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy, na.rm = T))
df_mpg

# 그래프 생성
ggplot(data = df_mpg, aes(x=drv, y=mean_hwy)) + geom_col() # 막대그래프는 col

# 크기 순으로 정렬
ggplot(data=df_mpg, aes(x=reorder(drv, -mean_hwy), y = mean_hwy)) + geom_col()
# reorder는 다시 주문한다는 함수, -mean_hwy가 내림차순으로 나오게끔 재주문한다.
## 막대 그래프 2 - 빈도 막대 그래프
''' 값의 개수(빈도)로 막대의 길이를 표현한 그래프'''

# x축에 범주 변수, y 축에 빈도
ggplot(data=mpg, aes(x=drv)) + geom_bar() # 빈도 그래프이므로 y축 지정이 필요없다.

# x축에 연속 변수, y 축에 빈도
ggplot(data=mpg, aes(x=hwy)) + geom_bar()


'''
평균 막대 그래프 : 데이터를 요약한 평균표를 먼저 만든 후 평균표를 이용해
                    그래프 생성 - geom_col()
빈도 막대 그래프 : 별도로 표를 만들지 않고 원자료를 이용해서 바로
                      그래프 생성 - geom_bar()
'''

### 선 그래프(Line chart) : 데이터를 선으로 표현한 그래프
'''
시계열 그래프(Time Series chart) : 일정한 시간 간격을 두고 나열된 시계열
                                    데이터를 선으로 표현한 그래프.
                                    -> 환율, 주가지수 등 경제 지표가
                                    시간에 따라 어떻게 변하는지 표현할 때
                                    사용
'''

## 시계열 그래프 생성
ggplot(data=economics, aes(x=date, y=unemploy)) + geom_line() # 등락을 해왔지만 전체적으로 실업자 수 가 증가함

### 상자 그림 (Box plot) : 데이터 분포(퍼져있는 형태)를 직사각형 상자 모양으로 표현한 그래프
'''
분포를 알 수 있기 때문에 평균만 볼 때보다 데이터의 특성을 좀 더 자세히 알 수 있음'''

ggplot(data = mpg, aes(x=drv, y=hwy)) + geom_boxplot() # 분석은 휴대폰 화면 녹화 확인

# --------------------------------------------------------------------------

''' 인터랙티브 그래프 - 움직이는 그래프

사용패키지 : plotly
'''

### 패키지 설치 & 로드
install.packages('plotly')
library(plotly)

# 1. ggplot 으로 그래프 만들기
p <- ggplot(data=mpg, aes(x=displ, y=hwy, col=drv)) + geom_point()

# 2. 인터랙티브 그래프
ggplotly(p)


# day2끝