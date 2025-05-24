''' 25 - 05 - 24 강의 자료'''
''' 텍스트 마이닝(Text Mining)
- 문자로 된 데이터에서 가치 있는 정보를 얻어내는 분석 기법
- SNS나 웹 사이트에 올라온 글을 분석해서 사람들이 어떤 이야기를
  나누고 있는지 파악할 때 활용
- 형택소 분석 : 문장을 구성하는 어절들이 어떤 품사로 되어 있는지
                분석

- 절차
1. 형태소 분석
2. 명사, 동사 형용사 등 의미를 지닌 품사 단어를 추출
3. 빈도표 만들기
4. 시각화 (워드 클라우드, 막대, 등)

- JAVA 설치 필요
'''


### 패키지 설피 및 로드
install.packages('rJava')
install.packages('memoise')
install.packages('KoNLP') # o만 소문자

install.packages('devtools')
devtools::install_github('haven-jeon/KoNLP')
library('KoNLP')
library('dplyr')
useNIADic()

extractNoun('대한민국의 영토는 한반도와 그 부속도서로 한다')

# 다음 주에 패키지 부분에서 업데이트 사항이 발생함.
# 다음 주 수업에 완벽히 정리하겠음

# DAY3 끝

# DAY4에 해결완료
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-11")

install.packages("rJava")

library(KoNLP)

useNIADic()

extractNoun('대한민국의 영토는 한반도와 그 부속도서로 한다')

### 노래 가사 텍스트 마이닝
txt <- readLines('rose.txt')
head(txt)

# 1. 특수문자 제거
install.packages('stringr')
library(stringr)

txt <- str_replace_all(txt, '\\W', ' ')

# 2. 명사 추출
nouns <- extractNoun(txt)

# 3. 추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount <- table(unlist(nouns))

# 4. 데이터 프레임 형태로 변환 -  ? 전처리함수, 시각화
df_word <- as.data.frame(wordcount, stringsAsFactors = F)

# 5. 변수명 수정
library(dplyr)
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)

# 6. 두 글자 이상 단어만 추출
df_word <- filter(df_word, word != 1)
str(df_word$word)
df_word <- filter(df_word, nchar(word) >= 2)
str(df_word)

# 7. top 20 추출
top20 <- df_word %>% 
  arrange(desc(freq))
head(20)
top20

### 워드 클라우드 - 단어 구름 시각화
install.packages('wordcloud')
library(wordcloud)
library(RColorBrewer)

# 단어 색상 목록 만들기
pal <- brewer.pal(8, 'Dark2') # Dark2 색상 목록에서 8개 색상 추출

# 워드 클라우드 생성 (시각화)
set.seed(1234) # 난수 고정
wordcloud(words = df_word$word, # 단어
          freq = df_word$freq, # 빈도
          min.freq = 2, # 최소 단어 빈도
          max.words = 200, # 표현할 단어 수
          random.order = F, # 고빈도 단어를 중앙에 배치
          rot.per = .1,  # 회전 단어 비율
          scale = c(4,0.3), # 단어 크기 범위
          colors = pal) # 색깔 목록

### 국정원 트윗 텍스트 마이닝
'''
국정원 계정 트윗 데이터
- 국정원 대선 개입 사실이 밝혀져 논란이 됐던 2013년 6월, 독립 언론
  뉴스타파가 인터넷을 통해 공개
- 국정원 계정으로 작성된 3,744개 트윗
'''

# 데이터 로드
twitter <- read.csv('twitter.csv',
                    header = T,
                    fileEncoding = 'utf-8') # Euc-kr, cp949 ....

# 변수명 수정
twitter <- rename(twitter,
                  no = 번호,
                  id = 계정이름,
                  date = 작성일,
                  tw = 내용)

# 특수문자 제거
library(dplyr)
library(stringr)
twitter$tw <- str_replace_all(twitter$tw, '\\W', ' ')

## 단어 빈도표 만들기
# 1. 트윗에서 명사 추출
nouns <- extractNoun(twitter$tw)

# 2. 추출한 명사 list 를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount <- table(unlist(nouns))

# 3. 데이터 프레임으로 변환]
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word

# 4. 변수명 수정
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)

## 두 글자 이상으로 된 단어 추출, 빈도 상위 20개 추출
# 두 글자 이상 단어만 추출
df_word <- filter(df_word, nchar(word) >= 2)

# 상위 20개 추출
top20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)
top20

## 단어 빈도 막대 그래프
library(ggplot2)

order <- arrange(top20, freq)$word # 빈도 순서 변수 생성

ggplot(data = top20, aes(x=word, y=freq, fill=word)) +
  ylim(0, 2500) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order) + # 빈도 순서 변수 기준 막대 정렬
  geom_text(aes(label = freq), hjust = -0.3) # 빈도 표시

## 워드 클라우드
pal <- brewer.pal(8, 'Dark2') # 색상 목록 생성
set.seed(1234) # 난수 고정

wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 10,
          max.freq = 200,
          random.order = F,
          rot.per = .1,
          scale = c(6,0.2),
          colors = pal)

# ------------------------------------------------------------------
''' 통계 분석 기법을 활용한 가설 검정 '''
'''

- 기술 통계
 -> 데이터를 요약해 설명하는 통계 기법
 -> ex) 사람들이 받는 월급을 집계해 전체 월급 평균 구하기
 
- 추론 통계
  -> 숫자를 요약하는 것을 넘어 어떤 값이 발생할 확률을 계산하는 통계 기법
  -> ex) 수집된 데이터에서 성별에 따라 월급 차이가 있는 것으로 나타났을 때,
          이런 차이가 우연히 발생할 확률을 계산

  
  1. 이런 차이가 우연히 나타날 확률이 작다.
    -> 성별에 따른 월급 차이가 통계적으로 유의하다고 결론
    
  2. 이런 차이가 우연히 나타날 확률이 크다.
    -> 성별에 따른 월급 차이가 통계적으로 유의하지 않다고 결론
  
  - 데이터를 이용해서 신뢰할 수 있는 결론을 내리려면 유의확률을 계산하는
  통계적 가설 검정 절차를 걸쳐야함.
  
- 통계적 가설 검정 : 유의확률을 이용해 가설을 검정하는 방법
- 유의 확률 : 실제로는 집단 간에 차이가 없는데 우연히 차이가 있는 데이터가
                추출될 확률
      1. 분석 결과 유의확률이 크게 나타난다면
       - 집단간 차이가 통계적으로 유의하지 않다고 해석
       -실제로 차이가 없더라도 우연에 의해 이 정도의 차이가 관찰될 가능성이 크다.
       
      2. 분석 결과 유의확률이 작게 나타난다면
       - 집단 간 차이가 통계적으로 유의하다고 해석
       - 실제로 차이가 없는데 우연히 이 정도의 차이가 관찰될 가능성이 작다,
         우연이라고 보기 힘들다.
'''

### t 검정 - 두 집단의 평균 비교
''' 두 집단의 평균에 통게적으로 유의한 차이가 있는지 알아볼 때 사용하는 통계 분석 기법 '''
    
mpg <- as.data.frame(ggplot2 :: mpg)

## compact 자동차와 suv 자동차의 도시 연비 t 검정
library(dplyr)
mpg_diff <- mpg %>% 
  select(class, cty) %>% 
  filter(class %in% c('compact','suv'))
head(mpg_diff)
table(mpg_diff$class)

# t-test
t.test(data=mpg_diff, cty~class, var.equal = T)

'''

p-value < 2.2e-16
-> 유의확률이 2.2e-16으로 0.05보다 작다. 그러므로 compact 차와 suv 차의
    도시연비 차이가 통계적으로 유의하다.
  
sample estimates:
mean in group compact     mean in group suv 
             20.12766              13.50000
-> compact 차가 suv 차보다 연비가 평균적으로 7 높은 편이다.

-> compact 차와 suv 차의 도비연비 차이는 통계적으로 유의하다.
'''

## 일반 휘발유와 고급 휘발유의 도시 연비 차이 ? -> t 검정
mpg_diff2 <- mpg %>% 
  select(fl, cty) %>% 
  filter(fl %in% c('p','r'))

table(mpg_diff2$f1)

# t-test
t.test(data=mpg_diff2, cty~fl, var.equal=T)

### 상관분석 - 두 변수의 관계성 분석
'''
상관분석 : 두 연속 변수가 서로 관련이 있는지 검정하는 통게 분석 기법

상관계수
  - 두 변수가 얼마나 관련되어 있는지, 관련성의 정도를 나타내는 값
  - 0~1 사이의 값을 지니고 1에 가까울수록 관련성이 크다
  - 상관계수가 양수면 정비례, 음수면 반비례
'''

## 실업자 수와 개인 소비 지출의 상관관계

# 데이터 로드
economics <- as.data.frame(ggplot2 :: economics)
economics

# 상관분석
cor.test(economics$unemploy, economics$pce)

''' 
p-value < 2.2e-16
-> 유의확률이 2.2e-16 으로 0.05보다 작다. 해당 상관은 우연일 확률이 작다.

sample estimates:
      cor
0.6145176
-> 개인소비지출이 증가하면 실업자수도 증가한다. 이러한 상관은 유의확률이 0.05보다
작기 때문에 통계적으로 유의하다.
'''


### 상관행렬 히트맵
'''
상관행렬
- 여러 변수 간 상관계수를 행렬로 나타낸 표
- 어떤 변수끼리 관련이 크고 적은지 파악할 수 있음
'''

head(mtcars)

# 상관행렬 만들기
car_cor <- cor(mtcars) # 생성
round(car_cor, 2) # 소수점 셋째 자리에서 반올림해서 출력


# 상관행렬 히트맵 : 값의 크기를 색깔로 표현한 그래프
install.packages('corrplot')
library(corrplot)

corrplot(car_cor)

# 1. 원 대신에 상관계수로 표시
corrplot(car_cor, method='number')

# 2. 다양한 옵션(파라미터) 지정
col <- colorRampPalette(c('#BB4444','#EE9988','#FFFFFF','#77AADD','#4477AA'))

corrplot(car_cor,
         method = 'color', # 색깔로 표현
         col = col(200), # 색상 200개 설정
         type = 'lower', # 왼쪽 아해 행렬만 표시
         order = 'hclust', # 유사한 상관계수끼리 군집화
         addCoef.col = 'black', # 상관계수 색깔
         tl.col = 'black', # 변수명 색깔
         tl.srt = 45, #  변수명
         diag = F) # 대각 행렬 제외

# ----------------------------------------------------------------------------
''' 나이브 베이즈 분류

기계 학습분야에서 나이브 베이즈는 특성들 사이의 독립을 가정하는
베이즈 정리를 적용한 확률 분류기의 일종 '''

''' tm 패키지 : 텍스트 마이닝 패키지 '''
install.packages('tm')
library(tm)

### 데이터 로드
sms_raw <- read.csv('sms_spam.csv',
                    stringsAsFactors = F)
str(sms_raw)
table(sms_raw$type)

sms_corpus <- VCorpus(VectorSource(sms_raw$text))
''' tm 패키지의 기능 : 텍스트 문서를 corpus 변환하는 함수
copus : 언어 모음을 만들어 놓은 것 '''

sms_corpus

inspect(sms_corpus[1:2])
as.character(sms_corpus[[1]]) # 1 위치 값을 문자형을 바꿔서 지정

lapply(sms_corpus[1:2], as.character)
# lapply : 벡터, 리스트, 표현식, df 등에 함수를 적용하고 그 결과를 리스트로 반환해주는 함수

sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
# 반응형 함수라고 하며 데이터 정제를 할 때 자주 쓰이는 함수다.
# 문서에 함수를 적용해서 변환된 결과를 반환. 즉, 텍스트를 변환해주는 함수다.
# content_tranformer(tolower) : 소문자 변환 함수

as.character(sms_corpus_clean[[1]]) # 소문자 확인
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers) # 숫자 제거
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords()) # 불용어 제거
# 불용어 : 의미 없게 쓰이는 단어 ex) 을, 를, 으로 ...
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation) # 특수문자 제거
stopwords()

install.packages('SnowballC')
''' SnowballC 패키지 : 어휘 비교를 지원하기 위해 단어를 공통 어근으로 축소하는
                      Porter의 단어 형태소 분석 알고리즘을 구현한 패키지 '''

library(SnowballC)
wordStem(c('learn','learned','learning','learns'))
# 벡터에서 주어진 각 단어의 어근을 찾는 함수
# 단어가 기본 구성요소로 줄어들어 단어 비교가 쉽다.
# ** 어근만으로 변환을 해야 나이브 베이즈 형태를 구축할 수 있다.

sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
# Porter의 스태밍 알고리즘을 사용해서 텍스트 문서의 어근 단어를 추출

sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)
# 텍스트 문서에서 여분의 공백을 제거

sms_dtm <- DocumentTermMatrix(sms_corpus_clean) # 문서-단어 행렬
# sms_dtm 객체는 문서별로 단어 빈도를 나타냄

sms_dtm2 <- DocumentTermMatrix(sms_corpus_clean, control = list(
  tolower=TRUE, removeNumbers=TRUE, stopwords=TRUE, removePunctuation=TRUE, stemming=TRUE
))

sms_train_labels <- sms_raw[1:4179, ]$type
sms_test_labels <- sms_raw[4180:5559, ]$type

sms_dtm_train <- sms_dtm[1:4179, ]
sms_dtm_test <- sms_dtm[4180:5559, ]

''' R 에서는 비율을 구하는 방법이 여러가지 존재하는데 그중에서 matrix 테이블을 한번에
proportion 테이블로 변환시키는 작업을 많이 하게된다.
prop.table()을 이용해서 proportion/비율/퍼센트 를 구할 수 있다. '''

prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

# install.packages('wordcloud')
library(wordcloud)
wordcloud(sms_corpus_clean,
          min.freq = 50, # 50미만의 빈도를 가진 단어는 출력 X
          random.order = FALSE) # 단어를 무작위 배치

spam <- subset(sms_raw, type == 'spam') # subset : 추출함수 -> 스팸메일만
ham <- subset(sms_raw, type == 'ham')
wordcloud(spam$text, max.words = 40, scale=c(3,0,5))
wordcloud(ham$text, max.words = 40, scale=c(3,0,5))


sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
# 문서-단어 또는 단어-문서 매트릭스에서 자주 사용되는 용어를 찾아준다. 하한값 = 5
str(sms_freq_words)

sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words] # 자주 등장하는 단어만 선택
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

convert_count <- function(x) {x <- ifelse(x>0, 'yes','no')} # 단어 출현 여부

# 각 단어가 등장했는 여부(yes/no)만을 갖는 범주형 데이터 프레임이 생성
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_count)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_count)

install.packages('e1071') # Naive Bayes 등 여러 머신러닝 알고리즘을 제공
library(e1071)

# 분류기 모델 학습
sms_classifier <- naiveBayes(sms_train, sms_train_labels)
sms_classifier

# 테스트 데이터로 예측 수행
sms_test_pred <- predict(sms_classifier, sms_test)
sms_test_pred


# 평가를 위한 교차표(cross table) 생성
install.packages('gmodels') # 교차표 생성 패키지
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE,
           dmm = c('predicted', 'actual'))
'''
prop.chisq : 카이제곱 기여도 비율 표시 안함
prop.t : 각 젤의 전체 비율 표시 안함
dmm : 행과 열에 이름 부여 (행 : 예측, 열 : 실제값)
'''

sms_classifier2 <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
# laplace : 라플라스 스무딩 기법 적용
'''
확률을 계산할 때 등장하지 않은 단어(0빈도)에 대해 확률이 0이 되지 않도록
조정하는 기법
-> 특히 테스트 데이터 학습 시 등장하지 않은 단어가 포함될 경우에 중요함.
'''

sms_test_pred2 <- predict(sms_classifier2, sms_test)

CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.r = FALSE,
           dmm = c('predicted', 'actual'))
'''
결과 해석

나이브 베이즈 분류 결과,

스팸 메일을 스팸 메일로 분류하는 확률은 0.912이고,
스팸 처리가 안된 메일을 스팸 처리가 안된 메일로 분류하는 확률은 0.98 정도 된다.
전체적으로 1346:34의 비율이 되므로 괜찮은 분류 결과를 내는 모델이라 볼 수 있다.
'''

# -----------------------------------------------------------------------------
''' 지도 시각화 '''

''' 단계 구분도
- 지역별 통계치를 색깔의 차이로 표현한 지도
- 인구나 소득 같은 특성이 지역별로 얼마나 다른지 쉽게 이해 가능
'''

install.packages('ggiraphExtra')
library(ggiraphExtra)

### 미국 주별 범죄 데이터 준비
str(USArrests) # 미국 주별 범죄 데이터 (4개의 범죄 종류, 50개주 데이터)

library(tibble)

head(USArrests)

# 1. 행 이름을 state 변수로 바꿔서 데이터 프레임 생성
crime <- rownames_to_column(USArrests, var='state')
crime

# 지도 데이터와 동일하게 맞추기 위해 state 값을 소문자로 수정
crime$state <- tolower(crime$state)
str(crime)

# 2. 미국 주 지도 데이터
library(ggplot2)
states_map <- map_data('state')
str(states_map)


# 3. 단계 구분도 생성
ggChoropleth(data=crime, # 지도에 표시할 데이터
             aes(fill = Murder, # 색깔로 표현할 변수
                             map_id = state), # 지역 기준 변수
             map = states_map) # 지도 데이터

# 4. 인터렉티브 적용
ggChoropleth(data=crime, # 지도에 표시할 데이터
             aes(fill = Murder, # 색깔로 표현할 변수
                 map_id = state), # 지역 기준 변수
             map = states_map, # 지도 데이터
             interactive = T) # 인터렉티브 적용

### 대한민국 지도 시각화
''' 긱 허브에 올라와 있는 kormaps2014 패키지 사용 '''
devtools::install_github('cardiomoon/kormaps2014')

library(stringi)
library(kormaps2014)

# 2014년 한국 행정지도(시도별)
str(changeCode(kormap1))
'''
kormap2 : 2014년 한국 행정지도(시군구별)
kormap3 : 2014년 한국 행정지도(읍면동별)
'''

# 2014 년 한국 인구 자료
str(changeCode(korpop1))
# changeCode : 한글 인코딩 문제 해결, 특정한 코드 값을 변환하여 지도의 텍스트 정보가 제대로 출력되게 해주는 함수

# 변수명 변경
korpop1 <- rename(korpop1, pop = 총인구_명, name = 행정구역별_읍면동)

# 2014년 인국 시각화
library(ggplot2)
library(maps)
library(mapproj)
library(ggiraphExtra)
ggChoropleth(data = korpop1, aes(fill=pop, map_id = code, tooltip = name),
             map=kormap1,
             interactive = T)

# 시도별 결핵 환자 수
ggChoropleth(data=tbc, aes(fill=NewPts, map_id = code, tooltip=name),
             map=kormap1,
             interactive = T)

# day 4 끝