#ЗАДАНИЕ 1: для региона 79 - Еврейская АО рассчитайте урожайность пшеницы в 2013 году, 
#взяв для рассчета средние суммы активных температур за текущий год, 
#с 14 ближайших метеостанций но убирая из рассчета активных температур дни с температурой выше 30 градусов
# Столица Биробиджан latitude = 48.7928, longitude = 132.924
#Установка рабочей директории
rm(list=ls())

setwd("D:/R_voloskov/MathMod")
getwd()
#Выбираем пакеты
library (tidyverse)
library(rnoaa)
library(lubridate)

# устанавливаем список метеостанций
station_data = ghcnd_stations()
write.csv(station_data,file = "station_data.csv")
station_data=read.csv("station_data.csv")

#После получения списка всех станций, получаем список станций ближайших 
# к столице нашего региона,
#создав таблицу с именем региона и координатами его столицы
Birobijan = data.frame(id="Birobijan", latitude = 48.7928, longitude = 132.924)
Birobijan_around = meteo_nearby_stations(lat_lon_df = Birobijan, 
                                   station_data = station_data,
                                   limit = 14,
                                   var=c("TAVG"),
                                   year_min = 2012, year_max = 2014)

#Birobijan_around это список единственным элементом которого является таблица, содержащая идентификаторы метеостанций отсортированных по их 
# удалленности от столицы, первым элементом таблицы будет идентификатор метеостанции Биробиджана, получим его
Birobijan_id=Birobijan_around[["Birobijan"]][["id"]][1]
summary(Birobijan_id)

# для получения таблицы со всеми метеостанциями вокруг столицы 
# необходимо выбрать целиком первый объект из списка
Birobijan_table=Birobijan_around[[1]]
summary(Birobijan_table)

# в таблице Birobijan_table оказалось 14 объектов, ранжированных по расстоянию от столицы

#сформируем список необходимых станций
Birobijan_stations=Birobijan_table
str(Birobijan_stations)

# список содержит 14 метеостанций расположенных вблизи Биробиджана выведем индетификаторы отфильрованных метеостанций 
Birobijan_stations$id

# скачаем погодые данных для наших метеостанций
# чтобы получить все данные с 1 метеостанции используем команду meteo_tidy_ghcnd
all_Birobijan_data=meteo_tidy_ghcnd(stationid = Birobijan_id)
summary(all_Birobijan_data)

# создать цикл, в котором бы скачивались  нужные данные для всех метеостанций 
# cоздадим объект, куда скачаем все данные всех метеостанций
all_Birobijan_meteodata = data.frame()

# создаем цикл для наших 14 метеостанций
stations_names=Birobijan_stations$id
stations_names=stations_names[1:14]

for (sname in stations_names)
{ one_meteo=meteo_tidy_ghcnd( stationid = sname,
                              date_min = "2013-01-01",
                              date_max = "2013-12-31")
station_vars=names(one_meteo)
if (!("tavg" %in% station_vars)){
  if(!("tmax"%in% station_vars)){
    next()
  }
  one_meteo=one_meteo %>% mutate(tavg=(tmax+tmin)/2)}
one_meteo=one_meteo %>% select(id,date,tavg)
one_meteo = one_meteo %>% mutate(tavg=tavg/10)
all_Birobijan_meteodata=rbind(all_Birobijan_meteodata, one_meteo)}

# записываем полученные результаты 
write.csv(all_Birobijan_meteodata,"all_Birobijan_meteodata.csv")
# считываем данные 
all_Birobijan_meteodata=read.csv("all_Birobijan_meteodata.csv")

str(all_Birobijan_meteodata)

# добавим год, месяц, день
all_Birobijan_meteodata=all_Birobijan_meteodata %>% mutate(year=year(date), 
                                               month=month(date), 
                                               day=day(date))
# превратим NA в 0 и где tavg<5 и tavg>30 
all_Birobijan_meteodata[is.na(all_Birobijan_meteodata$tavg),"tavg"] = 0
all_Birobijan_meteodata[all_Birobijan_meteodata$tavg<5, "tavg"] = 0
all_Birobijan_meteodata[all_Birobijan_meteodata$tavg>30, "tavg"] = 0
summary(all_Birobijan_meteodata)

# сгруппируем метеостанции по id, месяцам и проссумируем темперетатуру
# по этим группам, затем сгурппируем данные по месяцам и найдем среднее по месяцам для всех метеостанций
group_meteodata =all_Birobijan_meteodata %>% group_by(id,year,month) 
sumT_group_meteodata = group_meteodata %>% summarise(tsum=sum(tavg))
groups_month=sumT_group_meteodata%>%group_by(month)
sumT_month=groups_month%>%summarise(St=mean(tsum))

# Подготовка к расчету по формуле Урожая ##
# Ввод констант
afi = c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000) 
bfi = c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000) 
di = c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000) 
y = 1.0 
Kf = 300
Qj = 1600
Lj = 2.2 
Ej = 25 

# Рассчитаем Fi по месяцаv
sumT_month =sumT_month %>% mutate(Fi = afi+bfi*y*St)

#Рассчитаем Yi
sumT_month = sumT_month %>% mutate( Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))
##  Расчитываем урожай 
Yield = (sum(sumT_month$Yi)) 
Yield
#Результат 16,9 ц/га