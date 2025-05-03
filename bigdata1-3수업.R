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
