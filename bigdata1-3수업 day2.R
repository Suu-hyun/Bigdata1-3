''' 25 - 05 - 10 강의 자료'''

'''
데이터 파악 함수
1. head()
2. tail()
3. View()
4. dim() : 차원 출력
5. str() : 데이터 속성 출력
6. summary() : 요약통계량 출력

데이터 변수명 수정 함수 : rename(데이터명, 새 변수명 = 기존 변수명)
'''

# --------------------------------------------------------------------------

### 파생변수 생성
df <- data.frame(var1 = c(4,3,8),
                 var2 = c(2,6,1))
df

df$var_sum <- df$var1 + df$var2
df

df$var_mean <- (df$var1 +df$var2) / 2
df

# mpg 데이터에 통합 연비 변수 생성
mpg <- as.data.frame(ggplot2 :: mpg)
mpg

mpg$total <- (mpg$cty + mpg$hwy)/2 # 통탑 연비 변수 생성
head(mpg)
mpg$total
mean(mpg$total)

### 조건문을 활용해 파생변수 생성
summary(mpg$total)
hist(mpg$total)

## 기준 20으로 잡고 합격 판정 변수 생성
mpg$test <- ifelse(mpg$total >= 20, 'pass', 'fail')
head(mpg, 20)

# 빈도표로 합격 판정 자동차 수 확인
table(mpg$test)

# 막대그래프에 빈도 표현
library(ggplot2)
qplot(mpg$test)

### 중첩 조건문 활용 - 연비 등급 생성
'''
등급을 total 변수 기준
A : 30 이상
B : 20~29
C : 20 미만
'''

mpg$grade <- ifelse(mpg$total >= 30, 'A',
                    ifelse(mpg$total >= 20, 'B', 'C'))
head(mpg, 20)
table(mpg$grade)
qplot(mpg$grade)

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
midwest <- as.data.frame(ggplot2 :: midwest)
str(midwest)

# 2.
library(dplyr)
midwest <- rename(midwest, total = poptotal)
midwest <- rename(midwest, asian = popasian)

# 3.
midwest$ratio <- midwest$asian/midwest$total*100
hist(midwest$ratio)

# 4.
mean(midwest$ratio) # 0.4872462
midwest$group <- ifelse(midwest$ratio > 0.4872462, 'large', 'small')
table(midwest$group)
qplot(midwest$group)

# -------------------------------------------------------------------------------
''' 데이터 가공 - 원하는 형태로 데이터 다듬는다'''
'''
데이터 전처리 - dplyr 패키지 함수들

1. filter() : 행 추출
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
install.packages('dplyr')
library(dplyr)
exam <- read.csv('csv_exam.csv')
exam

# 데이터에서 1반인 경우만 추출해서 출력
exam %>% filter(class == 1)
''' 파이프 기호(%>%) 단축키 : ctrl + shift + M '''

# 1반이 아닌 경우
exam %>% filter(class != 1)

## 초과, 미만, 이상, 이하 조건
# 수학 점수가 50점을 초과한 경우
exam %>% filter(math> 50)

## 여러 조건을 충족하는 행 추출

# 1반에서 수학점수가 50점 이상인 학생
exam %>% filter(class == 1 & math >= 50)

## 여러 조건 중 하나 이상 충족하는 행 추출

# 수학 점수가 90점 이상이거나 영어점수가 90점 이상인 학생
exam %>% filter(math >= 90 | english >= 90) # | : 버티컬 바 (or)

# 1, 3, 5 반만 추출
exam %>% filter(class == 1 | class == 3 | class == 5)

## 목록에 해당하면 추출 - %in% 기호

# 1,3,5반만 추출
exam %>%filter(class %in% c(1,3,5))

## 추출한 행으로 데이터 생성
class1 <- exam %>% filter(class == 1)
mean(class1$english)

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

audi <- mpg %>% filter(manufacturer == 'audi')
toyota <- mpg %>% filter(manufacturer == 'toyota')

mean(audi$cty)
mean(toyota$cty)

### 필요한 변수만 추출 - select()

exam %>%  select(math)

## 여러 변수 추출
exam %>% select(class, math, english)

## 변수 제외
exam %>% select(-math)

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

## 오름차순 정렬
exam %>%arrange(math)

## 내림차순 정렬
exam %>% arrange(desc(math))

## 정렬 기준 변수 여러개 지정
exam %>% arrange(class, math)
exam

### 파생변수 추가 - mutate()
exam %>%
  mutate(total = math + english + science) %>% 
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
  group_by(class) %>% 
  summarise(mean_math = mean(math))

## 여러 요약 통계량 한 번에 산출
exam %>% 
  group_by(class) %>% 
  summarise(mean_math = mean(math), # 평균
            sum_math = sum(math), # 합계
            median_math = median(math), # 중앙값
            n = n()) # 학생 수(행의 수)

''' 자주 사용하는 요약통계량 함수
1. mean() : 평균
2. sd() : 표준편차
3. sum() : 합계
4. median() : 중앙값
5. min(), max() : 최소, 최댓값
6. n() : 빈도 (행의 개수)
'''

## 각 집단별로 다시 집단 나누기 - 제조사별 자동차의 구동방식별 평균 도시 연비

mpg %>% 
  group_by(manufacturer, drv) %>% 
  summarise(mean_cty = mean(cty),
            car_count = n()) %>% 
  head(15)


''' Q. 회사별로 - group_by
"suv" 자동차의 - filter
도시 및 고속도로 통합 연비 - mutate
평균을 구해 - summarise
        내림차순으로 정렬하고, - arrange
연비 1~5위까지 출력  - head'''

mpg %>% 
  group_by(manufacturer) %>% 
  filter(class == 'suv') %>% 
  mutate(tot = (cty+hwy)/2) %>% 
  summarise(mean_tot = mean(tot)) %>% 
  arrange(desc(mean_tot)) %>% 
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

total_test <- left_join(test1, test2, by='id') # id 기준으로 합치기
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

group_all <- bind_rows(group_a, group_b)
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

df <- data.frame(s = c('M','F', NA, 'M','F'),
                 score = c(5,4,3,4,NA))
df

# 결측치 확인 - is.na()
is.na(df)
is.na(mpg)

# 결측치 빈도 출력 - table() 와 함께 적용
table(is.na(df))

# 변수별로 결측치 확인
table(is.na(df$s))
table(is.na(df$score))

mean(df$score)


### 결측치 제거

# 결측치가 있는 행 제거
df %>% filter(is.na(score)) # score 예시 NA인 데이터만 출력

df %>% filter(!is.na(score)) # 결측치 제거

# 제거 후 분석
df_nomiss <- df %>% filter(!is.na(score))
df_nomiss

mean(df_nomiss$score)

# 여러 변수 동시에 결측치 없는 데이터 추출
df_nomiss <- df %>% filter(!is.na(score) & !is.na(s))
df_nomiss

# 결측치가 하나라도 있으면 제거
df_nomiss2 <- na.omit(df) # 모든 변수에 결측치 없는 데이터 추출
df_nomiss2
''' 분석에 필요한 데이터까지 손실 될 가능성 유의'''

# 함수의 결측치 제외 기능 활용 - na.rm = T 옵션
mean(df$score, na.rm = T) # 결측치 제외하고 평균 산출
sum(df$score, na.rm = T)


# summarise 안에서 na.rm = T 활용
exam[c(3,8,15),'math'] <- NA
exam

exam %>%  summarise(mean_math = mean(math, na.rm = T))

''' 결측치 대체하기

- 결측치가 많다면 모두 제외하면 데이터 손실 큼

- 결측치 대체법
1. 대표값(평균, 최빈값 등) 으로 일괄 대체
2. 통계분석 기법 적용, 예측값 추정해서 대체

'''

### 평균값으로 결측치 대체
mean(exam$math, na.rm = T)

exam$math <- ifelse(is.na(exam$math), 55, exam$math)
table(is.na(exam$math))

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
table(outlier$s)
table(outlier$score)


## 결측 처리 - s, score
outlier$s <- ifelse(outlier$s == 3, NA, outlier$s)
outlier

outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
outlier

### 제외하고 분석
outlier %>% 
  filter(!is.na(s) & !is.na(score)) %>% 
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
boxplot(mpg$hwy)

## 상자 그림 통계치 출력
boxplot(mpg$hwy)$stats





















































