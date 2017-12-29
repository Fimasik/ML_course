# подключаем пакеты

library(caret)
library(dplyr)
library(imputeMissings)
library(Hmisc)
library(CHAID)
library(randomForest)
library(ranger)
library(memisc)
library(ggplot2)
library(doParallel)
library(pROC)
library(rcompanion)
library(car)

# создаем рабочее пространство с файлами выборки

setwd("C:/Trees")

# загружаем данные

OTPset <- 
  read.csv2("Credit_OTP.csv", stringsAsFactors = F)

OTPset_test <- 
  read.csv2("Credit_OTP_new.csv", stringsAsFactors = F)

# смотрим количество уникальных значений у AGREEMENT_RK 
length(unique(OTPset$AGREEMENT_RK))
# удаляем переменную AGREEMENT_RK
OTPset$AGREEMENT_RK = NULL

# смотрим типы переменных
str(OTPset)

# пишем функцию для проставления метки "Не указано"
nonInformation <- 
  function(variable) {
    variable <- ifelse(variable == "" | variable == "Пропуск", "Не указано", 
                       variable)
    return(variable)
  }

# пишем функцию предобработки для приведения переменных к нужному формату, 
# удаления переменной с одним уникальным значением,
# создания переменной на соответствие адресов
preProcessData <- 
  function(data) {
    data <- 
      data %>%
      dplyr::mutate(
        TARGET = factor(TARGET, levels = c(0,1), labels = c("noResponse", "Response")),
        SOCSTATUS_WORK_FL = as.factor(SOCSTATUS_WORK_FL),
        SOCSTATUS_PENS_FL = as.factor(SOCSTATUS_PENS_FL),
        GENDER = as.factor(GENDER),
        MARITAL_STATUS = as.factor(MARITAL_STATUS),
        FAMILY_INCOME = as.numeric(as.factor(FAMILY_INCOME)),
        GEN_INDUSTRY = ifelse(GEN_INDUSTRY == "" & SOCSTATUS_PENS_FL == 1,
                              "Пенсия", GEN_INDUSTRY),
        GEN_INDUSTRY = nonInformation(GEN_INDUSTRY),
        GEN_TITLE = ifelse(GEN_TITLE == "" & SOCSTATUS_PENS_FL == 1,
                           "Пенсия", GEN_TITLE),
        GEN_TITLE = nonInformation(GEN_TITLE),
        ORG_TP_STATE = ifelse(ORG_TP_STATE == "" & SOCSTATUS_PENS_FL == 1,
                              "Пенсия", ORG_TP_STATE),
        ORG_TP_STATE = nonInformation(ORG_TP_STATE),
        ORG_TP_FCAPITAL = ifelse(ORG_TP_FCAPITAL == "" & SOCSTATUS_PENS_FL == 1,
                                 "Пенсия", ORG_TP_FCAPITAL),
        ORG_TP_FCAPITAL = nonInformation(ORG_TP_FCAPITAL),
        JOB_DIR = ifelse(JOB_DIR == "" & SOCSTATUS_PENS_FL == 1,
                         "Пенсия", JOB_DIR),
        JOB_DIR = nonInformation(JOB_DIR),
        TP_PROVINCE = nonInformation(TP_PROVINCE),
        REGION_NM = nonInformation(REGION_NM),
        REG_FACT_FL = as.factor(REG_FACT_FL),
        FACT_POST_FL = as.factor(FACT_POST_FL),
        REG_POST_FL = as.factor(REG_POST_FL),
        REG_FACT_POST_FL = as.factor(REG_FACT_POST_FL),
        FL_PRESENCE_FL = as.factor(FL_PRESENCE_FL),
        AUTO_RUS_FL = as.factor(AUTO_RUS_FL),
        HS_PRESENCE_FL = as.factor(HS_PRESENCE_FL),
        COT_PRESENCE_FL = as.factor(COT_PRESENCE_FL),
        GAR_PRESENCE_FL = as.factor(GAR_PRESENCE_FL),
        LAND_PRESENCE_FL = as.factor(LAND_PRESENCE_FL),
        DL_DOCUMENT_FL = NULL,
        GPF_DOCUMENT_FL = as.factor(GPF_DOCUMENT_FL),
        FACT_PHONE_FL = as.factor(FACT_PHONE_FL),
        REG_PHONE_FL = as.factor(REG_PHONE_FL),
        GEN_PHONE_FL = as.factor(GEN_PHONE_FL),
        PREVIOUS_CARD_NUM_UTILIZED =
          ifelse(is.na(PREVIOUS_CARD_NUM_UTILIZED), 0, PREVIOUS_CARD_NUM_UTILIZED),
        ### факт совпадения фактического области проживания и торговой точки
        FACT_TP_FL = as.factor(ifelse(FACT_ADDRESS_PROVINCE == TP_PROVINCE, 1, 0))
      )
    return(data)
  }

# пишем аналогичную функцию предобработки 
# для OTPset_test
preProcessDataTest <- 
  function(data) {
    data <- 
      data %>%
      dplyr::mutate(
        TARGET = factor(TARGET, levels = c(0,1), labels = c("noResponse", "Response")),
        SOCSTATUS_WORK_FL = as.factor(SOCSTATUS_WORK_FL),
        SOCSTATUS_PENS_FL = as.factor(SOCSTATUS_PENS_FL),
        GENDER = as.factor(GENDER),
        MARITAL_STATUS = as.factor(MARITAL_STATUS),
        FAMILY_INCOME = as.numeric(as.factor(FAMILY_INCOME)),
        GEN_INDUSTRY = ifelse(GEN_INDUSTRY == "Пропуск" & SOCSTATUS_PENS_FL == 1,
                              "Пенсия", GEN_INDUSTRY),
        GEN_INDUSTRY = ifelse(GEN_INDUSTRY == "Пропуск" & SOCSTATUS_PENS_FL == 1,
                              "Пенсия", GEN_INDUSTRY),
        GEN_INDUSTRY = nonInformation(GEN_INDUSTRY),
        GEN_TITLE = ifelse(GEN_TITLE == "Пропуск" & SOCSTATUS_PENS_FL == 1,
                           "Пенсия", GEN_TITLE),
        GEN_TITLE = nonInformation(GEN_TITLE),
        ORG_TP_STATE = ifelse(ORG_TP_STATE == "Пропуск" & SOCSTATUS_PENS_FL == 1,
                              "Пенсия", ORG_TP_STATE),
        ORG_TP_STATE = nonInformation(ORG_TP_STATE),
        ORG_TP_FCAPITAL = ifelse(ORG_TP_FCAPITAL == "Пропуск" & SOCSTATUS_PENS_FL == 1,
                                 "Пенсия", ORG_TP_FCAPITAL),
        ORG_TP_FCAPITAL = nonInformation(ORG_TP_FCAPITAL),
        JOB_DIR = ifelse(JOB_DIR == "Пропуск" & SOCSTATUS_PENS_FL == 1,
                         "Пенсия", JOB_DIR),
        JOB_DIR = nonInformation(JOB_DIR),
        TP_PROVINCE = nonInformation(TP_PROVINCE),
        REGION_NM = nonInformation(REGION_NM),
        REG_FACT_FL = as.factor(REG_FACT_FL),
        FACT_POST_FL = as.factor(FACT_POST_FL),
        REG_POST_FL = as.factor(REG_POST_FL),
        REG_FACT_POST_FL = as.factor(REG_FACT_POST_FL),
        FL_PRESENCE_FL = as.factor(FL_PRESENCE_FL),
        AUTO_RUS_FL = as.factor(AUTO_RUS_FL),
        HS_PRESENCE_FL = as.factor(HS_PRESENCE_FL),
        COT_PRESENCE_FL = as.factor(COT_PRESENCE_FL),
        GAR_PRESENCE_FL = as.factor(GAR_PRESENCE_FL),
        LAND_PRESENCE_FL = as.factor(LAND_PRESENCE_FL),
        DL_DOCUMENT_FL = NULL,
        GPF_DOCUMENT_FL = as.factor(GPF_DOCUMENT_FL),
        FACT_PHONE_FL = as.factor(FACT_PHONE_FL),
        REG_PHONE_FL = as.factor(REG_PHONE_FL),
        GEN_PHONE_FL = as.factor(GEN_PHONE_FL),
        PREVIOUS_CARD_NUM_UTILIZED =
          ifelse(is.na(PREVIOUS_CARD_NUM_UTILIZED), 0, PREVIOUS_CARD_NUM_UTILIZED),
        # факт совпадения фактического области проживания и торговой точки
        FACT_TP_FL = as.factor(ifelse(FACT_ADDRESS_PROVINCE == TP_PROVINCE, 1, 0))
      )
    return(data)
  }


# применяем функции предобработки
# к набору данных
OTPset <- preProcessData(OTPset)
# смотрим типы переменных
str(OTPset)
# выводим подробную информацию
# о переменных
Hmisc::describe(OTPset)

# разбиваем на обучающую и тестовую выборки

set.seed(45151)
index = createDataPartition(OTPset$TARGET, p = 0.7, list = F)
training <- OTPset[index,]
testing <- OTPset[-index,]

# анализируем редкие категории

freqTable <- 
  function(variable) {
    FreqTable <- data.frame(table(variable))
    FreqTable <- 
      FreqTable %>% 
      mutate(Share = Freq/sum(Freq)) %>%
      arrange(desc(Share)) %>%
      mutate(Share2 = cumsum(Share))
    return(FreqTable)
  }

FreqGEN_INDUSTRY <- freqTable(training$GEN_INDUSTRY)

table(training$GEN_TITLE)
FreqGEN_TITLE <- freqTable(training$GEN_TITLE) # оценка частоты и доли
training %>% group_by(GEN_TITLE) %>%
  summarise(PERSONAL_INCOME = mean(PERSONAL_INCOME)) # оценка по личному доходу

table(training$ORG_TP_STATE)

table(training$REG_ADDRESS_PROVINCE)
FreqREG_ADDRESS_PROVINCE <- 
  freqTable(training$REG_ADDRESS_PROVINCE) # оценка частоты и доли
FreqREG_ADDRESS_PROVINCE$variable

table(training$FACT_ADDRESS_PROVINCE)
table(training$POSTAL_ADDRESS_PROVINCE)

table(training$TP_PROVINCE)
TP_PROVINCE <- 
  freqTable(training$TP_PROVINCE) # оценка частоты и доли
TP_PROVINCE$variable

table(training$JOB_DIR)
JOB_DIR <- 
  freqTable(training$JOB_DIR) 

table(training$REGION_NM)

# "редкие" регионы
replaceRareRegion <- 
  function(region) {
    region = ifelse(region == "Москва" | region == "Хакасия" |
                      region == "Ямало-Ненецкий АО" | region == "Магаданская область" |
                      region == "Калмыкия" | region == "Дагестан" |
                      region == "Агинский Бурятский АО" | 
                      region == "Усть-Ордынский Бурятский АО" |
                      region == "Эвенкийский АО" | region == "Коми-Пермяцкий АО" |
                      region == "Коми-Пермяцкий АО" | region == "Чечня", 
                    "Другие регионы",
                    region)
    region = as.factor(region)
    return(region)
  }
# пишем функцию по укрупнению редких категорий
replaceRareClass <- 
  function(data) {
    data <- 
      data %>%
      dplyr::mutate(
        EDUCATION = 
          as.factor(
            ifelse(EDUCATION == "Ученая степень" | 
                             EDUCATION == "Два и более высших образования" |
                             EDUCATION == "Высшее",
                           "Высшее или несколько высших", EDUCATION)
          ),
        GEN_INDUSTRY = 
          as.factor(
            ifelse(GEN_INDUSTRY == "Юридические услуги/нотариальные услуги" | 
                     GEN_INDUSTRY == "Страхование" |
                     GEN_INDUSTRY == "Туризм" |
                     GEN_INDUSTRY == "Недвижимость" |
                     GEN_INDUSTRY == "Управляющая компания" |
                     GEN_INDUSTRY == "Логистика" |
                     GEN_INDUSTRY == "Подбор персонала" |
                     GEN_INDUSTRY == "Маркетинг" |
                     GEN_INDUSTRY == "Не указано", "Другие сферы", GEN_INDUSTRY)
          ),
        GEN_TITLE = as.factor(
          ifelse(GEN_TITLE == "Партнер" | GEN_TITLE == "Не указано" |
                   GEN_TITLE == "Военнослужащий по контракту", 
                 "Другое", GEN_TITLE)
        ),
        ORG_TP_STATE = as.factor(
          ifelse(ORG_TP_STATE == "Частная ком. с инос. капиталом" |
                   ORG_TP_STATE == "Не указано",
                 "Частная компания", ORG_TP_STATE)
        ),
        ORG_TP_FCAPITAL = as.factor(
          ifelse(ORG_TP_FCAPITAL == "Не указано", "Без участия", ORG_TP_FCAPITAL)
        ),
        JOB_DIR = 
          as.factor(ifelse(JOB_DIR == "Кадровая служба и секретариат" |
                             JOB_DIR == "Пр-техн. обесп. и телеком." |
                             JOB_DIR == "Юридическая служба" |
                             JOB_DIR == "Реклама и маркетинг" |
                             JOB_DIR == "Не указано",
                           "Другие направления", JOB_DIR)),
        REGION_NM =
          as.factor(ifelse(REGION_NM == "Не указано", "ЮЖНЫЙ", REGION_NM)),
        REG_ADDRESS_PROVINCE = replaceRareRegion(REG_ADDRESS_PROVINCE),
        FACT_ADDRESS_PROVINCE = replaceRareRegion(FACT_ADDRESS_PROVINCE),
        POSTAL_ADDRESS_PROVINCE = replaceRareRegion(POSTAL_ADDRESS_PROVINCE),
        TP_PROVINCE = as.factor(
          ifelse(TP_PROVINCE == "Сахалинская область" | TP_PROVINCE == "Еврейская АО" |
                   TP_PROVINCE == "Магаданская область" | 
                   TP_PROVINCE == "Москва" |
                   TP_PROVINCE == "Кабардино-Балкария" |
                   TP_PROVINCE == "1", "Другие регионы", TP_PROVINCE)
        )
      )
    return(data)
  }

# применяем функцию, укрупняющую редкие
# категории, к обучающей выборке
training <- replaceRareClass(training)

# создаем функцию, обрабатывающую выбросы

replaceOutlier <- 
  function(data) {
    data <- 
      data %>% mutate(
        FACT_LIVING_TERM = 
          ifelse(FACT_LIVING_TERM < 0, abs(FACT_LIVING_TERM), FACT_LIVING_TERM),
        FACT_LIVING_TERM = 
          ifelse(FACT_LIVING_TERM > quantile(FACT_LIVING_TERM, 0.999), NA, 
                 FACT_LIVING_TERM),
        WORK_TIME = 
          ifelse(WORK_TIME > quantile(WORK_TIME, 0.999, na.rm = T), NA, WORK_TIME)
      )
    return(data)
  }

# применяем функцию, обрабатывающую выбросы,
# к обучающей выборке
training <- replaceOutlier(training)

# смотрим количество пропусков по каждой переменной
# в обучающей выборке
sapply(training, function(x) sum(is.na(x)))

# импутируем пропуски в количественных 
# переменных медианами
training[sapply(training, is.numeric)] <- lapply(training[sapply(training,  
                                                     is.numeric)], function(x) 
                                                       ifelse(is.na(x), 
                                                              median(x, na.rm = TRUE), x))
# смотрим количество пропусков по каждой переменной
# в обучающей выборке
sapply(training, function(x) sum(is.na(x)))

# смотрим типы переменных в обучающей выборке
str(training)

# выводим гистограмму распределения
# для переменной CREDIT
plotNormalHistogram(training$CREDIT)

# выводим график квантиль-квантиль
# для переменной CREDIT
qqnorm(training$CREDIT,
       ylab="Sample Quantiles")
qqline(training$CREDIT, 
       col="red")

# выполняем логарифмическое преобразование,
# используем константу 0.01, чтобы не брать
# логарифм нуля
var_log = log(training$CREDIT+0.01)

# выводим гистограмму распределения
# для преобразованной переменной CREDIT
plotNormalHistogram(var_log)

# выводим график квантиль-квантиль
# для преобразованной переменной CREDIT
qqnorm(var_log,
       ylab="Sample Quantiles")
qqline(var_log, 
       col="red")

# выполняем преобразование корней третьей степени,
# используем модуль, чтобы не вычислять корни
# отрицательных чисел, и затем учитываем знак числа
var_cube = sign(training$CREDIT) * abs(training$CREDIT)^(1/3)

# выводим гистограмму распределения
# для преобразованной переменной CREDIT
plotNormalHistogram(var_cube)

# выводим график квантиль-квантиль
# для преобразованной переменной CREDIT
qqnorm(var_cube,
       ylab="Sample Quantiles")
qqline(var_cube, 
       col="red")

# вычисляем лямбду преобразования Бокса-Кокса
powerTransform(training$CREDIT)

# выполняем преобразование с помощью
# вычисленной лямбда
trans_var <-bcPower(training$CREDIT, -0.04163767)

# выводим гистограмму распределения
# для преобразованной переменной CREDIT
plotNormalHistogram(trans_var)

# выводим график квантиль-квантиль
# для преобразованной переменной CREDIT
qqnorm(trans_var,
       ylab="Sample Quantiles")
qqline(trans_var, 
       col="red")

# пишем функцию, создающую новые переменные
newFeaturesData <- 
  function(data) {
    data <- 
      data %>% mutate(
        PERSONAL_INCOME_INT = cut(PERSONAL_INCOME, 
                                  c(0,5000,10000,20000,50000,max(PERSONAL_INCOME))),
        FAMILY_INCOME_V2 = factor(FAMILY_INCOME, labels = levels(PERSONAL_INCOME_INT)),
        PERSONAL_FAMILY_INCOME = as.factor(
          ifelse(PERSONAL_INCOME_INT == FAMILY_INCOME_V2, 1, 0)
        ),
        PERSONAL_INCOME_INT = NULL, FAMILY_INCOME_V2 = NULL,
        PERSONAL_INCOME_LOG = log(PERSONAL_INCOME+0.01),
        CHILD_DEPENDANTS = CHILD_TOTAL/(CHILD_TOTAL+DEPENDANTS),
        CHILD_DEPENDANTS = ifelse(is.nan(CHILD_DEPENDANTS), 0, CHILD_DEPENDANTS),
        AUTO_FOR_FL = as.factor(ifelse(AUTO_RUS_FL == 0 & OWN_AUTO > 0, 1, 0)),
        CREDIT2=log(CREDIT),
        WORK_TIME2=log(WORK_TIME+0.01),
        FACT_LIVING_TERM=log(FACT_LIVING_TERM+0.01),
        PAYMENT = CREDIT/TERM,
        PTI = PAYMENT/PERSONAL_INCOME,
        LOAN_AVG_DLQ_AMT=log(LOAN_AVG_DLQ_AMT+0.01),
        FST_SHARE = FST_PAYMENT/(FST_PAYMENT+CREDIT),
        LOAN_SHARE_CLOSED = LOAN_NUM_CLOSED/LOAN_NUM_TOTAL,
        AVR_NUM_PAYM = LOAN_NUM_PAYM/LOAN_NUM_TOTAL,
        TERM_ON_AVR_NUM = TERM/AVR_NUM_PAYM,
        LOAN_SHARE_DLQ = LOAN_DLQ_NUM/LOAN_NUM_PAYM,
        DLQ_TIME = LOAN_MAX_DLQ/LOAN_DLQ_NUM,
        PERSONAL_INCOME = NULL,
        DLQ_TIME = ifelse(is.nan(DLQ_TIME), 0, DLQ_TIME),
        LOAN_MAX_DLQ_RANGE = abs((LOAN_MAX_DLQ_AMT - LOAN_AVG_DLQ_AMT)/LOAN_AVG_DLQ_AMT),
        LOAN_MAX_DLQ_RANGE = ifelse(is.nan(LOAN_MAX_DLQ_RANGE), 0, LOAN_MAX_DLQ_RANGE)
      )
    return(data)
  }

# применяем функцию, создающую новые переменные,
# к обучающей выборке
training <- newFeaturesData(training)

# применяем функцию, укрупняющую редкие
# категории, к обучающей выборке
testing <- replaceRareClass(testing)

# применяем функцию, обрабатывающую выбросы,
# к тестовой выборке
testing <- replaceOutlier(testing)

# смотрим количество пропусков по каждой переменной
# в тестовой выборке
sapply(testing, function(x) sum(is.na(x)))

# импутируем пропуски в количественных 
# переменных медианами
testing[sapply(testing, is.numeric)] <- lapply(testing[sapply(testing,  
                                                                 is.numeric)], function(x) 
                                                                   ifelse(is.na(x), 
                                                                          median(x, na.rm = TRUE), x))

# смотрим количество пропусков по каждой переменной
# в тестовой выборке
sapply(testing, function(x) sum(is.na(x)))

# применяем функцию, создающую новые переменные,
# к тестовой выборке
testing <- newFeaturesData(testing)

# смотрим типы переменных в тестовой выборке
str(testing)


# преобразовываем весь обучающий набор и итоговый тестовый набор

# перевыгружаем данные для лучшей воспроизводимости
OTPset <- read.csv2("Credit_OTP.csv", stringsAsFactors = F)
OTPset_test <- read.csv2("Credit_OTP_new.csv", stringsAsFactors = F)

# удаляем переменную AGREEMENT_RK
OTPset$AGREEMENT_RK = NULL

# применяем функции предобработки
OTPset <- preProcessData(OTPset)

# применяем функцию, укрупняющую редкие
# категории
OTPset <- replaceRareClass(OTPset)

# применяем функцию, обрабатывающую выбросы
OTPset <- replaceOutlier(OTPset)

# смотрим количество пропусков по каждой переменной
sapply(OTPset, function(x) sum(is.na(x)))

# импутируем пропуски в количественных 
# переменных медианами
OTPset[sapply(OTPset, is.numeric)] <- lapply(OTPset[sapply(OTPset,  
                                                              is.numeric)], function(x) 
                                                                ifelse(is.na(x), 
                                                                       median(x, na.rm = TRUE), x))
# смотрим количество пропусков по каждой переменной
sapply(OTPset, function(x) sum(is.na(x)))

# применяем функцию, создающую новые переменные
OTPset <- newFeaturesData(OTPset)

# удаляем переменную AGREEMENT_RK
OTPset_test$AGREEMENT_RK = NULL

# применяем функции предобработки
OTPset_test <- preProcessDataTest(OTPset_test)

# применяем функцию, укрупняющую редкие
# категории
OTPset_test <- replaceRareClass(OTPset_test)

# применяем функцию, обрабатывающую выбросы
OTPset_test <- replaceOutlier(OTPset_test)

# смотрим количество пропусков по каждой переменной
sapply(OTPset_test, function(x) sum(is.na(x)))

# импутируем пропуски в количественных 
# переменных медианами
OTPset_test[sapply(OTPset_test, is.numeric)] <- lapply(OTPset_test[sapply(OTPset_test,  
                                                           is.numeric)], function(x) 
                                                             ifelse(is.na(x), 
                                                                    median(x, na.rm = TRUE), x))
# смотрим количество пропусков по каждой переменной
sapply(OTPset_test, function(x) sum(is.na(x)))

# применяем функцию, создающую новые переменные
OTPset_test <- newFeaturesData(OTPset_test)


# загружаем библиотеку h2o, 
# перед загрузкой библиотеки h2o убедитесь, что библиотека h2o установлена 
# (сначала установите Java SE Development Kit 8, обратите внимание, 
# 9-я версия H2O не поддерживается, а затем после установки Java 
# установите пакет h2o с помощью команды
# install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R"))) и
# затем загрузите библиотеку)

library(h2o)
h2o.init(nthreads=-1, max_mem_size = "8G")

# смотрим датафреймы перед преобразованием
# во фреймы H2O
str(OTPset)
str(OTPset_test)

# выполняем преобразование во фреймы h2o
train <- as.h2o(OTPset)
valid <- as.h2o(OTPset_test)

# строим модель логистической регрессии
lr1 <- h2o.glm(family= "binomial", training_frame = train, validation_frame = valid, 
               x=c(2:65), y=1, seed = 1000000)

summary(lr1)

# строим модель логистической регрессии 
# с перебором lambda - силы штрафа 
lr2 <- h2o.glm(family= "binomial", training_frame = train, validation_frame = valid, 
               x=c(2:65), y=1, seed = 1000000, lambda_search = TRUE)

summary(lr2)

# строим модель логистической регрессии с перебором парных взаимодействий
lr3 <- h2o.glm(family= "binomial", training_frame = train, validation_frame = valid, 
               x=c(2:65), y=1, seed = 1000000, lambda_search = TRUE, 
               interactions = c('GENDER', 'SOCSTATUS_PENS_FL', 'FAMILY_INCOME'))

summary(lr3)

# выполняем решетчатый поиск с перебором alpha и lambda,
# alpha задает тип регуляризации: значение 1 соответствует 
# l1-регуляризации (лассо), значение 0 соответствует 
# l2-регуляризации (гребневой регрессии), 
# промежуточное значение соответствует 
# комбинации штрафов l1 и l2 (эластичной сети)
# lambda задает силу штрафа
hyper_parameters <- list(alpha = c(0, 0.2, 0.4, 0.6, 1))
glm_grid <- h2o.grid(algorithm = "glm", grid_id = "gridresults", hyper_params = hyper_parameters, 
                     training_frame = train, validation_frame = valid, x = c(2:65), y = "TARGET",
                     lambda_search = TRUE, family = "binomial")

# выводим результаты решетчатого поиска
summary(glm_grid)

# сортируем по AUC
sortedGrid <- h2o.getGrid("gridresults", sort_by = "auc", decreasing = TRUE)

# выводим результаты решетчатого поиска,
# отсортировав по убыванию AUC
sortedGrid

