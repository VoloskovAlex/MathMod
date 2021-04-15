#Задание 1
#Для региона 79 рассчитайте урожайность пшеницы в 2013 году, взяв для рассчета средние суммы активных температур за текущий год, с 14 ближайших метеостанций но убирая из рассчета активных температур дни с температурой выше 30 градусов
#Еврейский автономный область
#Столица Биробиджан -  48.7928, 132.924
rm(list=ls())
library(tidyverse)
library(rnoaa)
library(lubridate)

station_data = ghcnd_stations()
write.csv(station_data, file = "station_data.csv")

#загрузка списка всех метеостанций
station_data = read.csv("station_data.csv")

#получим список метеостанций ближайших к столице, создав таблицу с именем региона и координатами его столицы
Birobijan = data.frame(id = "Birobijan", latitude =  48.7928, longitude = 132.924)

#найдем метеостанции, соответствующие критериям
Birobijan_around = meteo_nearby_stations(lat_lon_df = Birobijan, station_data = station_data, limit = 14, var = c("PRCP", "TAVG"), year_min = 2013)
Birobijan_around

#первым элементом таблицы будет идентификатор метеостанции Биробиджана
Birobijan_id = Birobijan_around[["Birobijan"]][["id"]][1]
summary(Birobijan_id)
Birobijan_id

#чтобы получить таблицу всех метеостанций вокруг столицы нужно выбрать целиком первый объект из списка
Birobijan_table = Birobijan_around[[1]]
summary(Birobijan_table)

#в таблице Birobijan_table оказалось 14 объектов, ранжированных по расстоянию от столицы
#для получения всех данных с 1 метеостанции, зная ее идентификатор, используем команду meteo_tidy_ghcnd
all_Birobijan_data = meteo_tidy_ghcnd(stationid = Birobijan_id)
all_Birobijan_data

#создадим промежуточный объект, куда будем скачивать данные с конкретной метеостанции
all_i = data.frame()
#созадим объект, куда будем скачаем все данные всех метеостанций
all_Birobijan_meteodata = data.frame()

#цикл для всех метеостанций
for(i in 1:14)
{print(i)
  #выберем нужные свойсва
  all_i = meteo_tidy_ghcnd(stationid = Birobijan_table$id[i])
  all_i = all_i[,c("id", "data", "tmax", "tmin")]
  #с помощью команды rbind соединяем данные, полученные на предыдущих и даннои этапах цикла
  all_Birobijan_meteodata = rbind(all_Birobijan_meteodata, all_i)
}

#добавим колонку со средней температурой
all_Birobijan_meteodata = mutate(all_Birobijan_meteodata, tavg = (all_Birobijan_meteodata$tmax + all_Birobijan_meteodata$tmin)/2)

#запишем результат
write.csv(all_Birobijan_meteodata, "all_Birobijan_meteodata.csv")

#считываем данные из файла all_Birobijan_meteodata.csv
all_Birobijan_meteodata = read.csv("all_Birobijan_meteodata.csv")
#посмотрим на данные
str(all_Birobijan_meteodata)
#добавим год, месяц, день
all_Birobijan_meteodata = mutate(all_Birobijan_meteodata, year = year(date), month = month(date), day = day(date))
#проверим результат
str(all_Birobijan_meteodata)

#отфильтруем данные за 2013 год
years_Birobijan_meteodata = filter(all_Birobijan_meteodata, year %in% c(2012:2014))
#проверим результат
str(years_Birobijan_meteodata)
summary(years_Birobijan_meteodata)

#средняя сумма активных температур за месяц
#изучаем формулу и видим, что нужно расчитать сумму активных температур больше 5 градусов и, по условию задания, меньше 30 градусов по месячно, остальное в формуле - это константы

#разделим температуру на 10, чтобы привести в нормальный вид
years_Birobijan_meteodata[, "tavg"] = years_Birobijan_meteodata$tavg/10
summary(years_Birobijan_meteodata)

#превратим в нули все NA и где tavg<5 и tavg>30
years_Birobijan_meteodata[is.na(years_Birobijan_meteodata$tavg), "tavg"] = 0
years_Birobijan_meteodata[years_Birobijan_meteodata$tavg<5, "tavg"] = 0
years_Birobijan_meteodata[years_Birobijan_meteodata$tavg>30, "tavg"] = 0

#проверяем, что температура получилась или 0, или больше 5 и меньше 30 градусов
summary(years_Birobijan_meteodata)

#расчитаем суммарную температуру за месяц за 2013 год для всех станций, группируем по метеостанциям, годам и месяцам с функцией group_by
alldays = group_by(years_Birobijan_meteodata, id, year, month)
#функция summarize применяет некоторые действия к отдельным группам, полученным с помощью функции group_by, просуммируем температуру по этим группам с помощью функции sum
sumT_alldays_Birobijan = summarize(alldays, tsum = sum(tavg))
#максимальная суммарная температура за месяц ???
summary(sumT_alldays_Birobijan)

#сгруппируем данные по месяцами
groups_Birobijan_months = group_by(sumT_alldays_Birobijan, month)
groups_Birobijan_months
#найдем для всех метеостанций среднее по месяцам за год
sumT_months = summarize(groups_Birobijan_months, St = mean(tsum))
sumT_months

#подготовка к расчету по формуле урожая
#ввод констант
afi = c(0.000, 0.000, 0.000, 32.11, 26.31, 25.64, 23.20, 18.73, 16.30, 13.83, 0.000, 0.000)
bfi = c(0.000, 0.000, 0.000, 11.30, 9.26, 9.03, 8.16, 6.59, 5.73, 4.87, 0.000, 0.000)
di = c(0.000, 0.000, 0.000, 0.33, 1.00, 1.00, 1.00, 0.32, 0.000, 0.000, 0.000, 0.000)
y = 1.0
kf = 300
Qj = 1600
Lj = 2.2
Ej = 25

#расчитаем Fi по месяцам
sumT_months = mutate(sumT_months, Fi = afi+bfi*y*St)
#расчитаем Yi
sumT_months = mutate(sumT_months, Yi = ((Fi*di)*kf)/(Qj*Lj*(100 - Ej)))

#расчитаем урожай как сумму по месяцам 
Yield = sum(sumT_months$Yi); Yield


