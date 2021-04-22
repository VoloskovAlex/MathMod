# Задание 2 Д-Х 124 Волосков А.Е.
# создайте модель множественной линейной регрессии 
# дневных потоков паров воды за осенний период 2013 года 
# по данным измерений методом турбулентной пульсации

# установка рабочей директории
setwd("D:/R_voloskov/MathMod2/MathMod")

# подключаем необходимые библиотеки
library(tidyverse)
library(readr)
library(stringr)
library(dplyr)

library(ggplot2)
getwd()
eddypro = read.csv("eddypro.csv", skip = 1, na = c("", "NA", "-9999", "-9999.0"), comment = c("["))

# Обработка данных
# удаляем не нужные перую строку и пустой столбец
eddypro = eddypro[-1, ]
eddypro = select(eddypro, -(roll))

# столбцы "chr" превращаем в факторы
eddypro = eddypro %>% mutate_if(is.character, factor)

# заменим специальные символы в названии стобцов на имена
names(eddypro) = names(eddypro) %>%
  str_replace_all("[!]", "_exclam_") %>%
  str_replace_all("[?]", "_quest_") %>%
  str_replace_all("[*]", "_star_") %>%
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_slah_") %>%
  str_replace_all("[%]", "_percent_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]", "_")

# для проверки надо столбцы таблицы превратить в вектора
glimpse(eddypro)

# удалим строки с NA
eddypro = drop_na(eddypro)
# Отфильтруем по заданию данные только за осенний период. С начала сентября (243 день) по конец ноября(334 день)
eddypro = filter(eddypro, DOY >= 243 & DOY < 334)
# по заданию нужны данные за дневное время
eddypro = filter(eddypro, daytime == TRUE)
# получим таблицу, состоящую только из чисел, с не будем работать до конца
eddypro_numeric = eddypro[, sapply(eddypro, is.numeric)]
# gолучим таблицу, содержащую остальные колонки
eddypro_non_numeric = eddypro[, !sapply(eddypro, is.numeric)]

# Множественная регрессия
# содзаем обучающую (teach) и тестовую (test) непересекающиеся выборки
row_numbers = 1:length(eddypro_numeric$h2o_flux)
teach = sample(row_numbers, floor(length(eddypro_numeric$h2o_flux)*0.7))
test = row_numbers[-teach]
# обычающая выборка
teaching_tb1 = eddypro_numeric[teach,]
# тестовая выборка
testing_tb1 = eddypro_numeric[test,]

# создаем модель 1, добавляем в нее все переменные с помощью "(,)" и используем обучающую выборку
mod1 = lm(h2o_flux~ (.), data = teaching_tb1)
# получим информацию о моделе и коэффициенты
summary(mod1)
# проанализируем переменные по значимости
anova(mod1)
# построим графики
plot(mod1)

# cоздадим модель 2 добавив в неё значимые переменные из результатов функции anova()(со значимостью до 0.01, соответственно ***,** и * пометки)
mod2 = lm(h2o_flux~ DOY + Tau + qc_Tau + rand_err_Tau + H + qc_H +  rand_err_H  + LE + qc_LE + rand_err_h2o_flux + rand_err_co2_flux + H_strg 
          + h2o_molar_density + h2o_mole_fraction  + h2o_mixing_ratio + co2_molar_density + co2_mole_fraction + co2_mixing_ratio + h2o_time_lag 
          + sonic_temperature  + air_temperature + air_pressure + air_density + air_heat_capacity  + air_molar_volume + water_vapor_density  + e + es + RH +  Tdew 
          + u_unrot + v_unrot + w_unrot + u_rot + v_rot + w_rot + wind_dir + yaw + pitch + TKE + L + bowen_ratio + x_peak + x_offset  + x_offset + x_10 + x_30 + x_50 +x_70 
          + un_Tau + H_scf + un_LE + un_co2_flux + un_h2o_flux + h2o_spikes + h2o.1, data = teaching_tb1)
# получим информацио о моделе и коэффициенты
summary(mod2)
# проанализируем переменные по значимости
anova(mod2)
# сравним с предыдущей моделью
anova(mod2, mod1)
# построим графики
plot(mod2)

# cоздадим модель 3, повторяем отбрасывание 
mod3 = lm(h2o_flux~DOY + Tau + qc_Tau +  qc_H +  rand_err_H  + qc_LE + rand_err_h2o_flux + co2_flux + H_strg + h2o_molar_density 
          + co2_molar_density + co2_time_lag+ h2o_mixing_ratio + co2_molar_density + air_pressure + u_unrot + v_unrot + w_unrot + v_rot + yaw 
          + bowen_ratio + TKE + x_peak + un_h2o_flux, data = teaching_tb1)
# получим информацио о моделе и коэффициенты
summary(mod3)
# проанализируем переменные по значимости
anova(mod3)
# сравним с предыдущей моделью
anova(mod3, mod2)
# построим графики
plot(mod3)

# проведем корреляционный анализ переменных
# выбераем из таблицы только участвующие в линейной модели переменные
cor_teaching_tb1 = select(teaching_tb1, h2o_flux, DOY, Tau, qc_Tau, qc_H, rand_err_H, qc_LE, rand_err_h2o_flux, h2o_flux, H_strg, co2_molar_density, 
                          h2o_molar_density, h2o_time_lag, air_pressure, u_unrot, v_unrot, w_unrot, v_rot, yaw, bowen_ratio,  x_peak, un_h2o_flux)
# получаем таблицу коэффициентов корреляций
# подправляем модель 3, убираем из нее одну из двух коррелирующих между собой переменных (начиная от коэф. >= 0.7)
cor_td = cor(cor_teaching_tb1) %>% as.data.frame

# построим графики по полученной модели и проверем ее
# построим точки h2o_flux от h2o_flux на значениях teaching выборки, наложим предсказанные значения по модели 3 на teaching выборке сверху в виде линии
# в идеале линия должна  пройти через все точки, а так как график h2o_flux от самой себя, то он должен идти под углом 45 градусов
qplot(h2o_flux, data = teaching_tb1) + geom_line(aes(y = predict(mod3, teaching_tb1)))
# повторим на тестовой выборке
qplot(h2o_flux, data = testing_tb1) + geom_line(aes(y = predict(mod3, testing_tb1)))
# nак как модель зависит от множества переменных, мы можем вывести много графиков зависимостей h2o_flux от учитываемых в моделе параметров,
# в идеале предсказанная линия должна пройти через все точки, или как можно ближе к ним на тестовой выборке
qplot(DOY, h2o_flux, data = testing_tb1) + geom_line(aes(y = predict(mod3, testing_tb1)))
qplot(Tau, h2o_flux, data = testing_tb1) + geom_line(aes(y = predict(mod3, testing_tb1)))
qplot(h2o_flux, h2o_flux, data = testing_tb1) + geom_line(aes(y = predict(mod3, testing_tb1)))
