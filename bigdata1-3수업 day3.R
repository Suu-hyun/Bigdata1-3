''' 25 - 05 - 17 강의 자료 '''

'''
데이터 가공 함수
1. 조건에 맞는 데이터만 추출
exam %>% filter(english >= 80)

# 여러 조건
exam %>% filter(class == 1 $ math >= 80)

# 여러 조건 중 하나 이상 충족
exam %>%filter(math >= 90 | english >= 90)

2. 필요한 변수만 추출
exam %>% select(class, math, english)

3. 정렬
exam %>% arrange(math) # 오름차순
exam %>% arrange(desc(math)) # 내림차순

4. 파생 변수
exam %>% mutate(total = math + english + science)

5. 집단별로 요약하기
mpg %>%
  group_by(manufacturer, drv) %>% 
  summarise(mean_cty = mean(sty))
'''  

# ---------------------------------------------------------------------------  

'''
한국복지패널데이터 분석
- 한국보건사회연구원 발간
- 가구의 경제활동을 연구해 정책 지원에 반영할 목적
- 2006 ~ 2015년까지 전국에서 가구를 선정해 매년 추적 조사
- 경제활동, 생활시태, 복지욕구 등 수천 개 변수에 대한 정보로 구성
'''

### 패키지
install.packages('foreign')
library(foreign) # spss 파일 로드
library(dplyr) # 전처리
library(ggplot2) # 시각화
library(readxl) # 엑셀 파일 로드

### 데이터 준비
raw_welfare <- read.spss(file = 'Koweps_hpc10_2015_beta1.sav',
                         to.data.frame = T)

### 복사본 생성
welfare <- raw_welfare

### 데이터 검토
head(welfare)
str(welfare)
summary(welfare)
'''
대규모 데이터 같은 경우는 변수가 많고 변수명들이 코드로 되어있어서
전체 데이터 구조를 한눈에 파악하기 어려움

변수명을 쉬운 단어로 바꾼 후 분석에 사용할 변수들 각각 파악해야함.
'''

welfare <- rename(welfare,
                  s = h10_g3, # 성별
                  birth = h10_g4, # 태어난 연도
                  marriage = h10_g10, # 혼인 상태
                  religion = h10_g11, # 종교
                  income = p1002_8aq1, # 월급
                  code_job = h10_eco9, # 직종 코드
                  code_region = h10_reg7) # 지역 코드
'''
데이터 분석 절차
1단계 - 변수 검토 및 전처리
2단계 - 변수 간 관계 분석(요약표 만들기), 시각화
'''


''' 성별에 따른 월급 차이 - 성별에 따라 월급이 다를까 ?
1. 변수 검토 및 전처리
성별
월급

2. 변수 간 관계 분석
성별 월급 평균표
그래프로 시각화

'''
### 성별 변수 검토
class(welfare$s) # numeric
table(welfare$s) # 이상치 x

# 결측치 확인
table(is.na(welfare$s)) # 결측치 없음(False)

# 성별 항목에 이름 부여
welfare$s <- ifelse(welfare$s == 1, 'male', 'female')
table(welfare$s)
qplot(welfare$s)

### 월급 변수 컴토 및 전처리
class(welfare$income) # numeric
summary(welfare$income)
qplot(welfare$income)

# 이상치 확인
summary(welfare$income)

# 결측치
table(is.na(welfare$income))

### 성별에 따른 월급 차이 분석
# 1. 성별 월급 평균표 생성
s_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(s) %>% 
  summarise(mean_income = mean(income))

s_income

# 2. 그래프 만들기
ggplot(data = s_income, aes(x=s, y=mean_income)) + geom_col()

''' 나이와 월급의 관계 - 몇 살 때 월급을 가장 많이 받을까 ? '''

### 변수 검토
class(welfare$birth)
summary(welfare$birth)

qplot(welfare$birth)

## 결측치 확인
table(is.na(welfare$birth))

### 파생변수 - 나이
welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)

### 나이와 월급의 관계 분석
# 1. 나이에 따른 월급 평균표 생성
age_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))
age_income

# 2. 시각화
ggplot(data = age_income, aes(x=age, y=mean_income)) +geom_line() # 60대부터 급격한 월급 감소량이 보임

''' 연령대에 따른 월급 차이 - 어떤 연령대의 월급이 가장 많을까? '''
### 파생변수 - 연령대
welfare <- welfare %>% 
  mutate(ageg = ifelse(age < 30, 'young',
                       ifelse(age<=59, 'middle', 'old')))
table(welfare$ageg)

### 연령대에 따른 월급 차이 분석
# 1. 연령대에 따른 월급 평균표
ageg_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income = mean(income))
ageg_income

# 2. 시각화 
ggplot(data=ageg_income, aes(x=ageg, y=mean_income)) + geom_col() +
  scale_x_discrete(limits = c('young','middle','old')) # 막대 정렬

''' 연령대 및 성별 월급 차이 - 성별 월급 차이는 연령대별로 다를까? '''

### 연령대 및 성별 월급 차이 분석
# 1. 연령대 및 성별 월급 평균표
s_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg, s) %>% 
  summarise(mean_income = mean(income))
s_income

# 2. 시각화
ggplot(data=s_income, aes(x=ageg, y=mean_income, fill = s)) +
  geom_col() +
  scale_x_discrete(limits = c('young','middle','old'))

# 3. 성별 막대 분리
ggplot(data=s_income, aes(x=ageg, y=mean_income, fill = s)) +
  geom_col(position = 'dodge') +
  scale_x_discrete(limits = c('young','middle','old'))

### 나이 및 성별 월급 차이 분석
# 1. 나이 및 성별 월급 평균
s_age <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, s) %>% 
  summarise(mean_income = mean(income))
head(s_age)

# 선그래프 색구분 옵션 col=변수명
ggplot(data=s_income, aes(x=age, y=mean_income, col=s)) +
  geom_line()

''' 직업별 월급 차이 - 어떤 직업이 월급을 가장 많이 받을까 ? '''

### 직업 변수 검토 및 전처리
class(welfare$code_job) # numeric
table(welfare$code_job)

# 1. 직업 분류 코드 목록 불러오기 (엑셀)
library(readxl)
list_job = read_excel('Koweps_Codebook.xlsx',
                      col_names = T,
                      sheet = 2)
head(list_job)

# 2. welfare 에 직업명 결합
welfare <- left_join(welfare, list_job, by='code_job')

welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job,job) %>% 
  head(10)

### 직업별 월급 차이 분석
job_income <- welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))
job_income

# 1. 상위 10개 추출
top10 <- job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)
top10

# 2. 그래프 생성
ggplot(data=top10, aes(x=reorder(job, mean_income), y=mean_income, fill = job)) +
  geom_col() +
  coord_flip()

# 3. 하위 10개 추출
bottom10 <- job_income %>% 
  arrange(mean_income) %>% 
  head(10)
bottom10

# 4. 그래프 생성
ggplot(data=bottom10, aes(x=reorder(job, -mean_income), # - 를 붙인 이유는 내림차순으로 정렬 
                          y=mean_income,
                          fill=job)) +
  geom_col() +
  coord_flip() +
  ylim(0, 850)

''' 종교 유무에 따른 이혼율 - 종교가 있으면 이혼을 덜 할까 ? '''
### 종교 변수 검토 및 전처리
class(welfare$religion) # numeric
table(welfare$religion)

# 종교 유무 이름 부여
welfare$religion <- ifelse(welfare$religion == 1, 'YES', 'NO')
table(welfare$religion)

### 혼인 상태 변수 검토 및 전처리
class(welfare$marriage)
table(welfare$marriage)

# 이혼 여부 변수 생성
welfare$group_marriage <- ifelse(welfare$marriage == 1, 'marriage',
                                 ifelse(welfare$marriage == 3, 'divorce',  NA))
table(welfare$group_marriage)
table(is.na(welfare$group_marriage))
qplot(welfare$group_marriage)

### 종교 유무에 따른 이혼율 분석
religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n),
         pct = round(n/tot_group*100, 1)) # round() : 반올림 함수
religion_marriage

# 1. 이혼율 표 생성 - 이혼만 추출
divorce <- religion_marriage %>% 
  filter(group_marriage == 'divorce') %>% 
  select(religion, pct)
divorce

# 2. 그래프 생성
ggplot(data=divorce, aes(x=religion, y=pct)) + geom_col()

''' 지역별 연령대 비율 - 노년층이 많은 지역은 어디일까? '''

### 지역 변수 검토 및 전처리
class(welfare$code_region) # numeric
table(welfare$code_region)

# 지역 코드 목록 만들기
list_region = data.frame(code_region = c(1:7),
                         region = c('서울',
                                    '수도권(인천/경기)',
                                    '부산/경남/울산',
                                    '대구/경북',
                                    '대전/충남',
                                    '강원/충북',
                                    '광주/전남/전북/제주도'))
list_region

# welfare 에 지역명 변수 추가
welfare <- left_join(welfare, list_region, by='code_region')
welfare %>% 
  select(code_region, region) %>% 
  head(20)

### 지역별 연령대 비율 분석
region_ageg <- welfare %>% 
  group_by(region, ageg) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group = sum(n),
         pct = round(n/tot_group*100, 2))
head(region_ageg)

### 그래프 생성
ggplot(data=region_ageg, aes(x=region, y=pct, fill = ageg)) +
  geom_col() +
  coord_flip()

### 막대 정렬 : 노년층 비율 높은 순
list_order_old <- region_ageg %>% 
  filter(ageg == 'old') %>% 
  arrange(pct)
list_order_old

# 지역명 순서 변수 만들기
order <- list_order_old$region
order

ggplot(data=region_ageg, aes(x=region, y=pct, fill=ageg)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = order)

# ----------------------------------------------------------------------------
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