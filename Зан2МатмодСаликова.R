#Саликова Жанат создайте модель множественной линейной регрессии ночных потоков паров воды 
#за период 2013 года по данным измерений методом турбулентной пульсации
rm(list=ls())
library("tidyverse")#загружаем пакеты

library("nycflights13") #загружаем пакеты

library("tidyr") #загружаем пакеты

library("stringr") #загружаем пакеты

library("dplyr") #загружаем пакеты

library("tibble") #загружаем пакеты

library("readr") #загружаем пакеты
library(rnoaa)#загружаем пакеты
library(lubridate)#загружаем пакеты


#читаем данные из файла eddypro, пропускаем первую строку и заменяем текстовые 'NA', 
#пустые и сгенерированные пороговые значения на NA, игнорируем строки с "[" 

data = read_csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"), comment=c("[")) 

data = data[-1,]# удаляю еще одну строку
data = data[year(data$date) == 2013, c(1:ncol(data))]#данные отбираем за 2013 год


data=data[data$daytime == FALSE,] # отбираем данные ночных потоков

glimpse(data)# смотрим что получилось

data = select(data, -(roll)) # удаляем пустой и ненужный столбец

data = data %>% mutate_if(is.character, factor) #делаем преобразование строковых значений в факторные

names(data) = names(data) %>% str_replace_all("[!]","_emph_") %>% #Заменим специальные символы в названии стобцов на допустимые для переменных имена
  
  #заменяем конфликтующие знаки колонок
  
  str_replace_all("[?]","_quest_") %>%  
  
  str_replace_all("[*]","_star_") %>%  
  
  str_replace_all("[+]","_plus_") %>% 
  
  str_replace_all("[-]","_minus_") %>% 
  
  str_replace_all("[@]","_at_") %>% 
  
  str_replace_all("[$]","_dollar_") %>%
  
  str_replace_all("[#]","_hash_") %>% 
  
  str_replace_all("[/]","_div_") %>% 
  
  str_replace_all("[%]","_perc_") %>% 
  
  str_replace_all("[&]","_amp_") %>% 
  
  str_replace_all("[\\^]","_power_") %>% 
  
  str_replace_all("[()]","_") 

glimpse(data)# смотрим результат 

data_numeric = data[,sapply(data,is.numeric) ] #выберем все переменные типа numeric

data_non_numeric = data[,!sapply(data,is.numeric) ]#все остальные переменные

cor_td = cor(drop_na(data_numeric)) %>% as.data.frame %>% select(h2o_flux) #создадим матрицу для 
#корелляционного анализа и преобразовываем ее в таблиу с нужным нам столбцом потоки паров воды

vars = row.names(cor_td)[cor_td$h2o_flux^2 > .1] %>% na.exclude #выберем имена переменных (строк) 
#с коэффициентом детерминации больше 0.1

formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep=""));formula #собераем переменные 
#из вектора в формулу

#Создадим обучающую и тестирующую непересекающиеся выборки с помошью базового функционала для 
#обучения и тестирования моделей

row_numbers = 1:length(data_numeric$h2o_flux)

teach = sample(row_numbers, floor(length(data_numeric$h2o_flux)*.7))

test = row_numbers[-teach]

teaching_tbl = data_numeric[teach,]#Обучающая выборка

testing_tbl = data_numeric[test,]#Тестирующая выборка

#МОДЕЛЬ 1

model = lm(formula, data = data);model #создаем модель линейной регрессии

formula = h2o_flux ~ (rand_err_Tau + LE + qc_LE + rand_err_LE + h2o_flux + 
                        qc_h2o_flux + rand_err_h2o_flux + h2o_time_lag + sonic_temperature + 
                        air_temperature + air_density + air_molar_volume + es + RH + 
                        VPD + u_star_ + TKE + un_LE + un_h2o_flux + u_var + v_var + 
                        w_var + w_div_h2o_cov + flowrate)

coef(model)#коэффициенты

resid(model)#остатки

confint(model)#доверительный интервал

summary(model)#P-значения по модели

anova(model)#дисперсионный анализ

plot(model)#графическое представление модели
# МОДЕЛЬ 2

formula2 = h2o_flux ~ (rand_err_Tau + LE + qc_LE + rand_err_LE +  
                         rand_err_h2o_flux + h2o_time_lag + sonic_temperature + 
                         air_temperature + air_density + air_molar_volume + es + RH + 
                         VPD + u_star_ + TKE + un_LE + un_h2o_flux +w_div_h2o_cov)

model2 = lm(formula2, data = data);model2 #создаем модель линейной регрессии

anova(model2)#дисперсионный анализ

summary(model2)#P-значения по модели

anova(model2)#дисперсионный анализ

plot(model2)#графическое представление модели

# МОДЕЛЬ 3

formula3 = h2o_flux ~ (LE + air_density + air_molar_volume + es + RH + 
                         VPD + un_LE + un_h2o_flux +w_div_h2o_cov)

model3 = lm(formula3, data = data);model3 #создаем модель линейной регрессии

anova(model3)#дисперсионный анализ

summary(model3)#P-значения по модели

anova(model3)#дисперсионный анализ

summary(model3)

plot(model3)#графическое представление модели

# МОДЕЛЬ 4

formula4 = h2o_flux ~ (LE + RH + VPD + un_LE + un_h2o_flux +w_div_h2o_cov)

model4 = lm(formula4, data = data);model4 #создаем модель линейной регрессии

model4 = lm(formula4, data = data)

anova(model4)#дисперсионный анализ

summary(model4)#P-значения по модели

plot(model4)#графическое представление модели
