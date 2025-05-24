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