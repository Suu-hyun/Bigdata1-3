''' 25-05-03 강의 자료 '''

### 큰 제목
##, # 소제목
''' 주석 : 코드 상에서 설명이나 실행결과에
          영향을 미치지 않는 글 '''

### 벡터(Vector)

a <- c(1,2,4,5,6)

## 숫자형 벡터
ex_vector1 <- c(-1,0,1)
ex_vector1

b <- c(1, 5, 67, 2, 5, 432, 56, 1, 5, 15, 54, 56465, 56, 4, 654, 654)
length(b)
str(b)
mode(b)

## 문자형 벡터
ex_vector2 <- c('hello', 'hi~!')
ex_vector2
exex_vector3 <- c('1', '2', '3')
ex_vector3

str(ex_vector3)

## 논리형 벡터
ex_vector4 <- c(TRUE, FALSE, TRUE, FALSE)
ex_vector4
str(ex_vector4)

### 범주형 자료

ex_vector5 <- c(2,1,3,2,1)
ex_vector5

cate_vector5 <- factor(ex_vector5,
                       labels=c('apple','banana', 'cherry'))
cate_vector5

### 행렬 - matrix()
x <- c(1,2,3,4,5,6)
matrix(x, nrow = 2, ncol = 3)
matrix(x, nrow = 3, ncol = 2)
matrix(x, nrow = 3, ncol = 2, byrow = T)

### 배열 - array()
y <- c(1,2,3,4,5,6)
array(y, dim=c(2,2,3))

### 리스트 - list()
'''리스트는 다중형 데이터다.'''
list1 <- list(c(1,2,3),"Hello",14)
list1

### 데이터 프레임 - data.frame()
''' 데이터 프레임의 각 열 제일 위에는 변수명이 있어야 한다.'''
ID <- c(1,2,3,4,5,6,7,8,9,10)
SEX <- c('F','M','F','M','M','F','F','F','M','F')
AGE <- c(50,40,28,50,27,23,56,47,20,38)
AREA <- c('서울','경기','제주','서울','서울','서울','경기','서울','인천','경기')

dataframe_ex <- data.frame(ID,SEX,AGE,AREA)
dataframe_ex

# 데이터 파악 - 변수 속성 확인
str(dataframe_ex)

# ------------------------------------------------------------------------------
''' 변수 (variable) : 변하는 수
 - 다양한 값을 지니고 있는 하나의 속성
 - 변수는 데이터 분석의 대상
'''

a <- 1
b <- 2
c <- 3
d <- 3.5

## 변수로 연산
a + b
a + b + c
5*d
a * d

## 여러 값으로 구성된 변수 생성
var1 <- c(1,2,5,7,8)
var1
var2 <- c(1:100)
var2

## seq() 함수
var3 <- seq(1,5) # 1~5 까지 연속값으로 var3 생성
var3

var4 <- seq(1,100, by = 2) # 1~100까지 2 간격 연속값으로 var4 생성
var4

var5 <- seq(0,200, by = 2)
var5

# 연속값 변수로 연산
var1 + var3

## 문자로 된 변수 생성
str1 <- 'a'
str1

str2 <- 'text'
str2

str3 <- 'Hello world !' # 따옴표 안의 공백도 문자열로 인정
str3

# 연속 문자 변수 생성
str4 <- c('a','b','c')
str4

str5 <- c('Hello!','world','is','good!')
str5

str1 + 2 # 문자로 된 변수로는 연산할 수 없다.

# ------------------------------------------------------------------------------

''' 함수(Fuction) : 값을 넣으면 특정한 기능을 수행해서 처음과 다른 값을 출력하는 수 '''

## 숫자를 다루는 함수
x <- c(1,2,3)
x

mean(x) # 평균함수
max(x) # 최댓값
min(x) # 최솟값

## 문자를 다루는 함수
str5

paste(str5, clooapse = '*') # 별표를 구분자로 str4의 단어들 하나로 합치기

## 함수의 결과로 새로운 변수로 지정
x_mean <- mean(x)
x_mean

str5_paste <- paste(str5, collapse = ' ')
str5_paste

# -----------------------------------------------------------------
'''패키지(package)
- 함수나 변수가 여러 개 들어있는 꾸러미
- 하나의 패키지 안에 다양한 함수가 존재
- 함수를 사용하려면 패키지 설치를 먼저 선행

  패키지 설치 -> 패키지 로드 -> 패키지 함수 사용
'''

### ggplot2 패키지 설치, 로드
install.packages('ggplot2') # 패키지 설치
library(ggplot2) # 패키지 로드

# ggplot2 함수
x <- c('a','a','b','c')
x

# 빈도 그래프 출력
qplot(x)

# ggplot2 의 mpg 데이터로 그래프 생성
''' 여러 패키지들은 패키지 안의 함수를 사용해볼 수 있게 예제 데이터를 포함하는 경우가 있다. '''

qplot(data=mpg, x=hwy)
qplot(data=mpg, x=cty)

# x축 drv(구동방식), y축 hwy(고속도로 연비)
qplot(data=mpg, x=drv, y=hwy, geom='line') # 선 그래프 형태
qplot(data=mpg, x=drv, y=hwy, geom='boxplot') # 상자 그림 형태
qplot(data=mpg, x=drv, y=hwy, geom='boxplot', colour = drv) # 색 구분

# -------------------------------------------------------------------------------
''' 데이터 프레임(DataFrame)
  - 열은 속성
  - 행은 한 사람의 정보
  - 데이터가 크다 = 행 또는 열이 많다.
'''

### 시험 성적 데이터 생성
english <- c(90,80,60,70)
english

math <- c(50,60,100,20)

df_midterm <- data.frame(english,math)
df_midterm

# 반을 추가하고 싶은 경우
class <- c(1,1,2,2)
df_midterm <- data.frame(english, math, class)
df_midterm

# 이 학생들의 수학점수 평균
mean(df_midterm$math) # df_midterm의 math로 평균 산출

''' Q1. 제품 가격 판매량
        사과  1800  24
        딸기  1500  38
        수박  3000  13  -> 해당 표를 데이터 프레임으로 만들어서 출력
    Q2. 위에서 만든 데이터 프레일을 이용해서 과일 가격 평균, 판매량 평균을 구하시오.
'''

# Q1.
sales <- data.frame(fruit = c('사과','딸기', '수박'),
                    price = c(1800,1500,3000),
                    volume = c(24,38,13))
# Q2.
sales
mean(sales$price)
mean(sales$volume)

### 외부 데이터 활용 - 축적된 시험 성적 데이터 불러오기
install.packages('readxl') # 엑셀 로드 패키지
library(readxl)

df_exam <- read_excel('excel_exam.xlsx') # 엑셀 파일 불러와서 def_exam 에 해당
df_exam






























































































