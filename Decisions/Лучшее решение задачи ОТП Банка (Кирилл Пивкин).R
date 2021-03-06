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
OTPset <- read.csv2("Credit_OTP.csv", stringsAsFactors = F)
OTPset_test <- read.csv2("Credit_OTP_new.csv", stringsAsFactors = F)

# смотрим количество уникальных значений у AGREEMENT_RK 
length(unique(OTPset$AGREEMENT_RK))

# удаляем переменную AGREEMENT_RK
OTPset$AGREEMENT_RK <- NULL

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
        REGION_NM = ifelse(REGION_NM == "ПОВОЛЖСКИЙ", "ПРИВОЛЖСКИЙ", REGION_NM),
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

# пишем функцию предобработки для тестового набора
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
        REGION_NM = ifelse(REGION_NM == "ПОВОЛЖСКИЙ", "ПРИВОЛЖСКИЙ", REGION_NM),
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

# выводим подробную информацию
# о переменных
Hmisc::describe(OTPset)

# разбиваем на обучающую и тестовую выборки
set.seed(45151)
index = createDataPartition(OTPset$TARGET, p = 0.7, list = F)
training <- OTPset[index,]
testing <- OTPset[-index,]

# пишем функцию для анализа редких категорий
freqTable <- 
  function(variable) {
    FreqTable <- data.frame(table(variable))
    FreqTable <- 
      FreqTable %>% 
      mutate(Share = Freq/sum(Freq)) %>%
      arrange(desc(Share)) %>%
      mutate(Cumshare = cumsum(Share))
    return(FreqTable)
  }

# выводим обычные и накопленные доли 
# категорий переменной GEN_INDUSTRY, с помощью 
# options(scipen=999) отключаем 
# экспоненциальное представление чисел
options(scipen=999)
FreqGEN_INDUSTRY <- freqTable(training$GEN_INDUSTRY)
FreqGEN_INDUSTRY

# выводим частоты категорий переменной GEN_TITLE
table(training$GEN_TITLE)

# выводим обычные и накопленные доли 
# категорий переменной GEN_TITLE
FreqGEN_TITLE <- freqTable(training$GEN_TITLE)
FreqGEN_TITLE

# смотрим средний личный доход в каждой
# категории переменной GEN_TITLE
training %>% group_by(GEN_TITLE) %>%
  summarise(PERSONAL_INCOME = mean(PERSONAL_INCOME))

# выводим частоты категорий переменной ORG_TP_STATE
table(training$ORG_TP_STATE)

# выводим частоты категорий переменной REG_ADDRESS_PROVINCE
table(training$REG_ADDRESS_PROVINCE)

# выводим обычные и накопленные доли 
# категорий переменной REG_ADDRESS_PROVINCE
FreqREG_ADDRESS_PROVINCE <- freqTable(training$REG_ADDRESS_PROVINCE)
FreqREG_ADDRESS_PROVINCE

# выводим частоты категорий переменной FACT_ADDRESS_PROVINCE
table(training$FACT_ADDRESS_PROVINCE)

# выводим частоты категорий переменной POSTAL_ADDRESS_PROVINCE
table(training$POSTAL_ADDRESS_PROVINCE)

# выводим частоты категорий переменной TP_PROVINCE
table(training$TP_PROVINCE)

# выводим обычные и накопленные доли 
# категорий переменной TP_PROVINCE
FreqTP_PROVINCE <- freqTable(training$TP_PROVINCE)
FreqTP_PROVINCE

# выводим частоты категорий переменной JOB_DIR
table(training$JOB_DIR)

# выводим обычные и накопленные доли 
# категорий переменной JOB_DIR
FreqJOB_DIR <- freqTable(training$JOB_DIR) 

# выводим частоты категорий переменной REGION_NM
table(training$REGION_NM)

# пишем функцию по укрупнению "редких" регионов
replaceRareRegion <- 
  function(region) {
    region = ifelse(region == "Москва" | region == "Хакасия" |
                      region == "Ямало-Ненецкий АО" | region == "Магаданская область" |
                      region == "Калмыкия" | region == "Дагестан" |
                      region == "Агинский Бурятский АО" | 
                      region == "Усть-Ордынский Бурятский АО" |
                      region == "Эвенкийский АО" |
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

# пишем функцию, обрабатывающую выбросы
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

# выводим гистограмму распределения для переменной CREDIT 
# с помощью функции plotNormalHistogram пакета rcompanion
plotNormalHistogram(training$CREDIT)

# выводим график квантиль-квантиль для переменной CREDIT
# с помощью функций qqnorm и qqline
qqnorm(training$CREDIT,
       ylab="Sample Quantiles")
qqline(training$CREDIT, 
       col="red")

# выполняем логарифмическое преобразование переменной
# CREDIT, используем константу 0.01, чтобы не брать
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
# с помощью функции powerTransform пакета car
powerTransform(training$CREDIT)

# выполняем преобразование с помощью вычисленной лямбда,
# используя функцию bcPower пакета car
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

# смотрим типы переменных
str(training)

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

# смотрим типы переменных
str(testing)

# настраиваем параметры решетчатого поиска
set.seed(15111)
ctrl = trainControl(method = "repeatedcv", number=5, repeats=1, classProbs=T, 
                    summaryFunction = twoClassSummary)
gridSet <- expand.grid(
  .mtry = c(20, 24, 28, 32),
  .min.node.size =c(100, 110),
  .splitrule = "gini"
)

modelWeights <- ifelse(training$TARGET == "noResponse",
                       (1/prop.table(table(training$TARGET))[1]) * 0.5,
                       (1/prop.table(table(training$TARGET))[2]) * 0.5)

modelInfo <- list(label = "Random Forest",
                  library = c("e1071", "ranger", "dplyr"),
                  check = function(pkg) {
                    requireNamespace("ranger")
                    current <- packageDescription("ranger")$Version
                    expected <- "0.8.0"
                    if(compareVersion(current, expected) < 0)
                      stop("This modeling workflow requires ranger version ",
                           expected, "or greater.", call. = FALSE)
                  },
                  loop = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = c("mtry", "splitrule", "min.node.size"),
                                          class = c("numeric", "character", "numeric"),
                                          label = c("#Randomly Selected Predictors", 
                                                    "Splitting Rule", 
                                                    "Minimal Node Size")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      srule <-
                        if (is.factor(y))
                          "gini"
                      else
                        "variance"
                      out <- expand.grid(mtry = 
                                           caret::var_seq(p = ncol(x),
                                                          classification = is.factor(y),
                                                          len = len),
                                         min.node.size = ifelse( is.factor(y), 1, 5), 
                                         splitrule = c(srule, "extratrees"))
                    } else {
                      srules <- if (is.factor(y))
                        c("gini", "extratrees")
                      else
                        c("variance", "extratrees", "maxstat")
                      out <-
                        data.frame(
                          min.node.size= sample(1:(min(20,nrow(x))), size = len, replace = TRUE), 
                          mtry = sample(1:ncol(x), size = len, replace = TRUE),
                          splitrule = sample(srules, size = len, replace = TRUE)
                        )
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if((!is.data.frame(x))||dplyr::is.tbl(x)) x <- as.data.frame(x)
                    x$.outcome <- y
                    if(!is.null(wts)) {
                      out <- ranger::ranger(dependent.variable.name = ".outcome", 
                                            data = x, 
                                            mtry = param$mtry, 
                                            min.node.size = param$min.node.size,
                                            splitrule = as.character(param$splitrule),
                                            write.forest = TRUE,
                                            probability = classProbs, 
                                            case.weights = wts, 
                                            ...)
                    } else {
                      out <- ranger::ranger(dependent.variable.name = ".outcome", 
                                            data = x, 
                                            mtry = param$mtry, 
                                            min.node.size = param$min.node.size,
                                            splitrule = as.character(param$splitrule),
                                            write.forest = TRUE,
                                            probability = classProbs, 
                                            ...)
                    }
                    ## in case the resampling method is "oob"
                    if(!last) out$y <- y
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if((!is.data.frame(newdata))||dplyr::is.tbl(newdata)) newdata <- as.data.frame(newdata)
                    out <- predict(modelFit, newdata)$predictions
                    if(!is.null(modelFit$obsLevels) & modelFit$treetype == "Probability estimation") {
                      out <- colnames(out)[apply(out, 1, which.max)]
                    }
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    predict(modelFit, newdata)$predictions
                  },
                  predictors = function(x, ...) {
                    var_index <- sort(unique(unlist(lapply(x$forest$split.varIDs, function(x) x))))
                    var_index <-var_index[var_index > 0]
                    x$forest$independent.variable.names[var_index]
                  },
                  varImp = function(object, ...){
                    if(length(object$variable.importance) == 0)
                      stop("No importance values available")
                    imps <- ranger:::importance(object)
                    out <- data.frame(Overall = as.vector(imps))
                    rownames(out) <- names(imps)
                    out
                  },
                  levels = function(x) {
                    if(x$treetype == "Probability estimation") {
                      out <- colnames(x$predictions)
                    } else {
                      if(x$treetype == "Classification") {
                        out <- levels(x$predictions)
                      } else out <- NULL
                    }
                    out
                  },
                  oob = function(x) {
                    postResample(x$predictions, x$y)
                  },
                  tags = c("Random Forest", "Ensemble Model", "Bagging",
                           "Implicit Feature Selection", "Accepts Case Weights"),
                  sort = function(x) x[order(x[,1]),])


# запускаем решетчатый поиск, перебираем 
# параметры mtry и min.node.size пакета
# ranger (то есть количество случайно
# обираемых предикторов для разбиения и
# минимальное количество наблюдений в 
# терминальном узле для случайного леса)
ranger_gridsearch <- train(TARGET ~ ., data = training, num.trees=800,
                           method = modelInfo, importance = "impurity",
                           weights = modelWeights,
                           metric = "ROC", 
                           trControl = ctrl, tuneGrid = gridSet)

# печатаем результаты решетчатого поиска
print(ranger_gridsearch)


# преобразовываем весь обучающий набор и итоговый тестовый набор
# перевыгружаем данные
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
# во фреймы H2O, обратите внимание, h2o
# не умеет обрабатывать упорядоченные факторы
# (ordered factors)
str(OTPset)
str(OTPset_test)

# выполняем преобразование во фреймы h2o
train <- as.h2o(OTPset)
valid <- as.h2o(OTPset_test)

# взглянем на обучающий фрейм h2o
str(train)

# строим модель логистической регрессии
glm1 <- h2o.glm(family= "binomial", training_frame = train, validation_frame = valid, 
               x=c(2:65), y=1, seed = 1000000)

# смотрим модель
summary(glm1)

# строим модель логистической регрессии 
# с перебором lambda - силы штрафа 
glm2 <- h2o.glm(family= "binomial", training_frame = train, validation_frame = valid, 
               x=c(2:65), y=1, seed = 1000000, lambda_search = TRUE)

# смотрим модель
summary(glm2)

# выполняем решетчатый поиск с перебором alpha и lambda,
# alpha задает тип регуляризации: значение 1 соответствует 
# l1-регуляризации (лассо), значение 0 соответствует 
# l2-регуляризации (гребневой регрессии), 
# промежуточное значение соответствует 
# комбинации штрафов l1 и l2 (эластичной сети),
# lambda задает силу штрафа
hyper_parameters <- list(alpha = c(0, 0.2, 0.4, 0.6, 1))
glm_grid <- h2o.grid(algorithm = "glm", grid_id = "glm_grid", 
                     hyper_params = hyper_parameters, 
                     training_frame = train, validation_frame = valid, x = c(2:65), y = "TARGET",
                     lambda_search=TRUE, family = "binomial", seed = 1000000)

# выводим результаты решетчатого поиска
summary(glm_grid)

# сортируем по AUC
sorted_glm_grid <- h2o.getGrid("glm_grid", sort_by = "auc", decreasing = TRUE)

# выводим результаты решетчатого поиска,
# отсортировав по убыванию AUC
sorted_glm_grid


# снова выполняем решетчатый поиск, но теперь дополнительно
# зададим список предикторов для рассмотрения парных
# взаимодействий
glm_grid2 <- h2o.grid(algorithm = "glm", grid_id = "glm_grid2", 
                     hyper_params = hyper_parameters, 
                     training_frame = train, validation_frame = valid, x = c(2:65), y = "TARGET",
                     lambda_search=TRUE, family = "binomial",
                     interactions=c("GENDER", "EDUCATION", "REGION_NM", "REG_ADDRESS_PROVINCE"),
                     seed = 1000000)

# выводим результаты решетчатого поиска
summary(glm_grid2)

# сортируем по AUC
sorted_glm_grid2 <- h2o.getGrid("glm_grid2", sort_by = "auc", decreasing = TRUE)

# выводим результаты решетчатого поиска,
# отсортировав по убыванию AUC
sorted_glm_grid2

# записываем идентификатор наилучшей модели
best_model_id <- sorted_glm_grid2@model_ids[[1]]

# извлекаем наилучшую модель
best_model <- h2o.getModel(best_model_id)

# смотрим наилучшую модель
best_model

# строим модель градиентного бустинга, перечислены 
# значения параметров по умолчанию: learn_rate -
# темп обучения, ntrees - количество деревьев (итераций),
# max_depth - глубина, min_rows - количество
# наблюдений в терминальном узле, sample_rate -
# процент отобранных строк для построения дерева,
# col_sample_rate - процент случайно отбираемых столбцов 
# для каждого разбиения узла, col_sample_rate_per_tree -
# процент случайно отбираемых столбцов для каждого дерева,
# col_sample_rate_per_tree - относительное изменение 
# отбора столбцов для каждого уровня дерева
gbm1 <- h2o.gbm(learn_rate=0.1, ntrees = 50, max_depth = 5, min_rows = 10,
                sample_rate = 1, col_sample_rate = 1,
                col_sample_rate_change_per_level = 1, 
                col_sample_rate_per_tree = 1,
                training_frame = train, validation_frame = valid, 
                x=c(2:65), y=1, seed = 1000000)

summary(gbm1)


# попробуем уменьшить глубину
gbm2 <- h2o.gbm(learn_rate=0.1, ntrees = 50, max_depth = 2, min_rows = 10,
                sample_rate = 1, col_sample_rate = 1,
                col_sample_rate_change_per_level = 1, 
                col_sample_rate_per_tree = 1,
                training_frame = train, validation_frame = valid, 
                x=c(2:65), y=1, seed = 1000000)

summary(gbm2)


# теперь попробуем увеличить минимальное 
# количество наблюдений в листе
gbm3 <- h2o.gbm(learn_rate=0.1, ntrees = 50, max_depth = 2, min_rows = 115,
                sample_rate = 1, col_sample_rate = 1,
                col_sample_rate_change_per_level = 1, 
                col_sample_rate_per_tree = 1,
                training_frame = train, validation_frame = valid, 
                x=c(2:65), y=1, seed = 1000000)

summary(gbm3)


# теперь попробуем уменьшить процент отбираемых 
# столбцов (вносим рандомизацию)
gbm4 <- h2o.gbm(learn_rate=0.1, ntrees = 50, max_depth = 2, min_rows = 115,
                sample_rate = 1, col_sample_rate = 0.14,
                col_sample_rate_change_per_level = 1, 
                col_sample_rate_per_tree = 1,
                training_frame = train, validation_frame = valid, 
                x=c(2:65), y=1, seed = 1000000)

summary(gbm4)


# теперь попробуем уменьшить процент отбираемых 
# столбцов для каждого дерева (вносим рандомизацию)
gbm5 <- h2o.gbm(learn_rate=0.1, ntrees = 50, max_depth = 2, min_rows = 115,
                sample_rate = 1, col_sample_rate = 0.14,
                col_sample_rate_change_per_level = 1, 
                col_sample_rate_per_tree = 0.25,
                training_frame = train, validation_frame = valid, 
                x=c(2:65), y=1, seed = 1000000)

summary(gbm5)

# возвращаемся к базовым параметрам, увеличиваем learn_rate
gbm6 <- h2o.gbm(learn_rate=0.2, ntrees = 50, max_depth = 2, min_rows = 115,
                sample_rate = 1, col_sample_rate = 0.14,
                col_sample_rate_change_per_level = 1, 
                col_sample_rate_per_tree = 0.25,
                training_frame = train, validation_frame = valid, 
                x=c(2:65), y=1, seed = 1000000)

summary(gbm6)


# поскольку мы немного увеличили learn_rate, можно 
# немного снизить количество итераций
gbm7 <- h2o.gbm(learn_rate=0.2, ntrees = 45, max_depth = 2, min_rows = 115,
                sample_rate = 1, col_sample_rate = 0.14,
                col_sample_rate_change_per_level = 1, 
                col_sample_rate_per_tree = 0.25,
                training_frame = train, validation_frame = valid, 
                x=c(2:65), y=1, seed = 1000000)

summary(gbm7)


# глубину пропускаем, увеличиваем минимальное 
# количество наблюдений в листе
gbm8 <- h2o.gbm(learn_rate=0.2, ntrees = 45, max_depth = 2, min_rows = 125,
                sample_rate = 1, col_sample_rate = 0.14,
                col_sample_rate_change_per_level = 1, 
                col_sample_rate_per_tree = 0.25,
                training_frame = train, validation_frame = valid, 
                x=c(2:65), y=1, seed = 1000000)

summary(gbm8)

