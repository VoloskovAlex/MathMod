#Задание 2
#Для региона 79 рассчитайте урожайность пшеницы в 2013 году взяв для рассчета средние суммы активных температур за текущий год, с 14 ближайших метеостанций
#Еврейский автономный округ
# Столица город Биробиджан - 48.7928, 132.924
rm(list=ls())

library(tideverse)
library(rnoaa)
station_data = ghcnd_stations()
write.csv(station_data, file="station_data.csv")

# Загрузка списока всех метеостанций
station_data=read.csv("station_data.csv")

# Получили список метеостанций ближайших к столице, создав таблицу с именем региона и координатами его столицы
Birobijana = data.frame(id="Birobijana", latitude=48.7928, longitude=132.924)

# Найдем метеостанции, соответствующие критериям
Birobijana_around = meteo_nearby_stations(lat_lon_df = Birobijana, station_data = station_data, limit = 14, var=c("PRCP","TAVG"), year_max = 2021, year_min=2013)
Birobijana_around

# Первым элементом таблицы будет идентификатор метеостанции Биробиджана
Birobijana_id = Birobijana_around[["Birobijana"]][["is"]][1]
summary(Birobijana_id)
Birobijana_id

# Получаем таблицу всех метеостанций вокруг Биробиджана, нужно выбрать целиком первый объект из списка
Birobijana_table = Birobijana_around[[1]]
summary(Birobijana_table)

# в таблице Birobijana_table оказалось 14 объектов, ранжированных по расстоянию от Биробиджана

# Для получения всех данных с 1 метеостанции, зная ее идентификатор, используйем команду all_Birobijana_data
all_Birobijana_data = meteo_tidy_ghcnd(stationid = Birobijana_id)

#Создадим промежуточный объект, куда будем скачивать данные с конкретной метеостанции
all_i = data_frame()
#Создадим объект, куда скачаем все данные всех метеостанций
all_EvreyskayAO_meteodata = data.frame()

#Цикл для всех метеостанций
for(i in 1:14)
{print(i) 
  #выберем нужные свойства
  all_i = meteo_tidy_ghcnd(stationid = Birobijana_table$id[i])
  all_i = all_i[c("id", "date", "tmax", "tmin")]
  #с помощью команды rbind соединяем данные, полученные на предыдущих и данном этапах цикла
  all_EvreyskayAO_meteodata = rbind(all_EvreyskayAO_meteodata, all_i)}

# добавим колонку со средней температурой
all_EvreyskayAO_meteodata = mutate(all_EvreyskayAO_meteodata, tavg = (all_EvreyskayAO_meteodata$tmax + all_EvreyskayAO_meteodata$tmin)/2)

# Записываем полученные результаты
write.csv(all_EvreyskayAO_meteodata, "all_EvreyskayAO_meteodata.csv")

# Cчитываем данные из файла all_EvreyskayAO_meteodata.csv
all_EvreyskayAO_meteodata = read.csv("all_EvreyskayAO_meteodata.csv")
str(all_EvreyskayAO_meteodata)

#отфильтруем данные за 2013-2021 года
years_EvreyskayAO_meteodata = filter(all_EvreyskayAO_meteodata, year %in% c(2013:2021))

str(years_EvreyskayAO_meteodata)
summary(years_EvreyskayAO_meteodata)

#Средняя (по годам и метеостанциям) сумма активных температур за месяц

# Изучаем формулу и видим, что нужно расчитать сумму температур больше 5 град. по месячно, остальное в формуле - константы
# Разделим температуру на 10, чтобы привести в нормальный вид

years_EvreyskayAO_meteodata["tavg"] = years_EvreyskayAO_meteodata$tavg/10
summary(years_EvreyskayAO_meteodata)
# Превратим в нули все NA и где tavg<5 и tavg>30
years_EvreyskayAO_meteodata[is.na(years_EvreyskayAO_meteodata$tavg), "tavg"] = 0
years_EvreyskayAO_meteodata[years_EvreyskayAO_meteodata$tavg<5, years_EvreyskayAO_meteodata$tavg>30, "tavg"] =0

#проверка
summary(years_EvreyskayAO_meteodata)

# Расчитаем суммарную температуру за месяц текущего года для всех станций группируем по метеостанциям, годам и месяцам
alldays = group_by(years_EvreyskayAO_meteodata, id, year, month)

#функция summarize применяет некоторые действия к отдельным группам, полученным с помощью функции group_by
#просуммирую температуру по этим группам с помощью sum
sumT_alldays_EvreyskayAO = summarize(alldays, tsum = sum(tavg))
summary(sumT_alldays_EvreyskayAO)

# Сгруппируем данные по месяцам 
group_EvreyskayAO_month = group_by(sumT_alldays_EvreyskayAO, month)
group_EvreyskayAO_month

# найдем для всех метеостанций и всех лет среднее по месяцам
sumT_months = summarize(group_EvreyskayAO_month, St=mean(tsum))
sumT_months

#Расчет по формуле Урожая
# Ввод констант
afi= c(0.00, 0.00, 0.00, 32.11, 26.31, 25.64, 23.20, 18.73, 16.30, 13.83, 0.00, 0.00)
bfi= c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03, 8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
di= c(0.00, 0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
y= 1.0
kf=300
Qi = 1600
Lj = 2.2
Ej=25
# Рассчитаем Fi по месяцам
sumT_months = mutate(sumT_months, Fi= afi+bfi*y*St)
#Рассчитаем Yi
sumT_months = mutate(sumT_months, Yi = ((Fi*di)*kf)/(Qj*Lj*(100-Ej)))
#Расчитываем урожай как сумму по месяцам
Yield = sum(sumT_months$Yi)
Yield

