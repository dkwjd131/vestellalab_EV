install.packages("RMySQL")
install.packages("plotrix")
install.packages("data.table",repos="http://R-Forge.R-project.org")
install.packages("mongolite")
install.packages("pool")
install.packages("ggplot2")
library(RMySQL)
library(jsonlite)
library(dplyr)
library(stringr)
library(plotrix)
library(data.table)
library(mongolite)
library(DBI)
library(ggplot2)

# Mysql OBD 데이터 베이스 연동
con <- dbConnect(MySQL(), user = "root", password="qwe123", host="49.254.250.186", client.flag=CLIENT_MULTI_RESULTS)

sql <- "use obd"
obd_tmp <- dbGetQuery(con, sql)
sql <- "select * from `bf_obd_test` where created_on like '2019-08-06%'"
obd_tmp <- dbGetQuery(con, sql)
bf_obd_tmp <- dbGetQuery(con, sql)


## maria db 연동 

con <- dbConnect(MySQL(), user = "root", password="irex01!234!", host="183.111.236.46",client.flag=CLIENT_MULTI_RESULTS )


sql <- "use kisti"
kisti_tmp <- dbGetQuery(con, sql)
sql <- "show tables"
kisti_tmp <- dbGetQuery(con, sql)
sql <- "select count(*) from sensorParser"
kisti_tmp <- dbGetQuery(con, sql)
sql <- "select * from sensorParser order by sensorParser.timestamp limit 1433776" # 시간 오름차순으로 정렬 후 최대 1433776개 행
kisti_tmp <- dbGetQuery(con, sql)


#날짜가 0000년 00월 00일로 데이터있는 쓰레기값과, 행이 1개 뿐인 2017년 6월 1일 데이터 제거
kisti_tmp <- kisti_tmp[-(1:41),]


# 문자형 날짜 칼럼을 날짜형 데이터로 변환
kisti_tmp$timestamp <- as.POSIXct(kisti_tmp$timestamp,format="%Y-%m-%d %H:%M:%S")


kisti_server <-kisti_tmp[kisti_tmp$gateway_id=="SERVER",]
kisti_PLG <-kisti_tmp[kisti_tmp$gateway_id=="PLNGateway",]


kisti_server_1999 <-kisti_server[kisti_server$node_id=="IK1999",]
kisti_server_1013 <-kisti_server[kisti_server$node_id=="IK1013",]

kisti_server <- unique(kisti_server)
kisti_PLG <- unique(kisti_PLG)

boxplot(kisti_server$so2_percent,data=kisti_server,ylab="이산황가스 퍼센트") # 이산화황 box plot
boxplot(kisti_server$temp_value,data=kisti_server,ylab="온도") # 온도 box plot



colSums(is.na(kisti_server)) # 컬럼별 NA 값 갯수
colSums(is.na(kisti_PLG)) # 컬럼별 NA 값 갯수




# 날짜 - kisti_server$temp_value 그래프
plot(kisti_server$temp_value ~ kisti_server$timestamp)

# 날짜 - kisti_server$temp_value 그래프
plot(kisti_server_1999$temp_value ~ kisti_server_1999$timestamp)

# 날짜 - kisti_server$temp_value 그래프
plot(kisti_PLG$temp_value ~ kisti_PLG$timestamp)



twoord.plot(d0806$created_on, d0806$pid.000_stateofchargebms, d0806$created_on, d0806$pid.000_batterydcvoltage,
            xlab = "created on", ylab = "state of charge bms", rylab = "battery dc voltage",lcol=4)


############################################## 이중축 그래프 날짜 - SOC(BMS) - battery dc voltage 그래프
##############################################
obd_tmp$data <- gsub("\"null\"","null",obd_tmp$data)
obd_tmp$data <- gsub("null","\"null\"",obd_tmp$data) # null 값이면 문자로 null로 입력

obd_tmp$data <- str_sub(obd_tmp$data,2)
obd_tmp$data <- paste("{\"created_on\": \"",obd_tmp$created_on,"\", ",obd_tmp$data,sep="") #created_on 값도 json 형식으로 추가

obd_tmp$data <- gsub("\\s","",obd_tmp$data) # 공백 제거
obd_tmp$data <- tolower(obd_tmp$data) # 소문자

var <- obd_tmp[1,2]
js <- fromJSON(var)
data_0806 <- data.frame(js)


for(num in 2:nrow(obd_tmp)){
  var <- obd_tmp[num,2]
  js <- fromJSON(var)
  data_tmp <- data.frame(js)
  
  for(num1 in 1:ncol(data_tmp)){
    data_tmp[,num1] <- as.factor(data_tmp[,num1])
  }
  
  data_0806<-bind_rows(data_0806,data_tmp)
}


data_0806_tmp <- data_0806[,c("pid.000_batterycurrent",
                              "pid.000_batterypower",
                              "pid.000_cumulativechargecurrent", 
                              "pid.000_cumulativeenergycharged",
                              "pid.000_cumulativedischargecurrent",
                              "pid.000_cumulativeenergydischarged",
                              "pid.000_stateofchargebms",
                              "ev_speed_kmh")]


data_0806_tmp$pid.000_batterycurrent <- gsub("a","",data_0806_tmp$pid.000_batterycurrent)
data_0806_tmp$pid.000_batterypower <- gsub("kw","",data_0806_tmp$pid.000_batterypower)
data_0806_tmp$pid.000_cumulativechargecurrent <- gsub("ah","",data_0806_tmp$pid.000_cumulativechargecurrent)
data_0806_tmp$pid.000_cumulativeenergycharged <- gsub("kwh","",data_0806_tmp$pid.000_cumulativeenergycharged)
data_0806_tmp$pid.000_cumulativedischargecurrent <- gsub("ah","",data_0806_tmp$pid.000_cumulativedischargecurrent)
data_0806_tmp$pid.000_cumulativeenergydischarged <- gsub("kwh","",data_0806_tmp$pid.000_cumulativeenergydischarged)
data_0806_tmp$pid.000_stateofchargebms <- gsub("%","",data_0806_tmp$pid.000_stateofchargebms)

for(num in 1:ncol(data_0806_tmp)){
  data_0806_tmp[,num] <- as.factor(data_0806_tmp[,num])
}

for(num in 1:ncol(data_0806_tmp)){
  data_0806_tmp[,num] <- as.character(data_0806_tmp[,num])
}

for(num in 1:ncol(data_0806_tmp)){
  data_0806_tmp[,num] <- as.numeric(data_0806_tmp[,num])
}

ggcorr(data_0806_tmp, label = TRUE)
##################################################################################################################
obd_tmp <- obd_tmp[12417:28861,] # after 19/02/20 10:40:42

obd_tmp$data <- gsub("\"null\"","null",obd_tmp$data)
obd_tmp$data <- gsub("null","\"null\"",obd_tmp$data) # null 값이면 문자로 null로 입력

obd_tmp$data <- str_sub(obd_tmp$data,2)
obd_tmp$data <- paste("{\"created_on\": \"",obd_tmp$created_on,"\", ",obd_tmp$data,sep="") #created_on 값도 json 형식으로 추가

obd_tmp$data <- gsub("\\s","",obd_tmp$data) # 공백 제거
obd_tmp$data <- tolower(obd_tmp$data) # 소문자

var <- obd_tmp[1,2]
js <- fromJSON(var)
data <- data.frame(js)

for(num in 1:162){
  data[,num] <- as.character(data[,num])
  data[1,num] <- gsub(":","",data[1,num])
  data[,num] <- as.factor(data[,num])
}

#obd_tmp <- obd_tmp[!(obd_tmp$created_on=="2019-02-20 11:41:03"),]


obd_tmp[obd_tmp$created_on=="2019-07-25 16:22:11","data"] =
  "{\"created_on\": \"2019-07-25 16:22:11\", \"pid\": {\"Speed\": \"0.0\", \"Accuracy\": \"0.0\", \"Latitude\": \"0.0\", \"Longitude\": \"0.0\", \"003_vmcu d\":
\"0.0\", \"003_vmcu n\": \"0.0\", \"003_vmcu p\": \"1.0\", \"003_vmcu r\": \"0.0\", \"Before Parsing\": \"\\u0000\\u0005null\\r?\\r\\r\", \"000_hv_charging\":
\"0.0\", \"000_bms ignition\": \"1.0\", \"000_battery power\": \"1.1559kW\", \"000_bms main relay\": \"1.0\", \"000_operating time\": \"1346.829722222222hours\",
\"003_vmcu brakes on\": \"0.0\", \"000_battery current\": \"3.0A\", \"000_cell voltage 01\": \"3.64V\", \"000_cell voltage 02\": \"0.48V\", \"000_cell voltage
03\": \"0.76V\", \"000_cell voltage 04\": \"0.76V\", \"000_cell voltage 05\": \"1.44V\", \"000_cell voltage 06\": \"3.26V\", \"000_cell voltage 07\": \"5.1V\",
\"000_cell voltage 08\": \"3.02V\", \"000_cell voltage 09\": \"0.3V\", \"000_cell voltage 10\": \"1.32V\", \"000_cell voltage 11\": \"0.26V\", \"000_cell voltage
12\": \"0.22V\", \"000_cell voltage 13\": \"0.24V\", \"000_cell voltage 14\": \"0.26V\", \"000_cell voltage 15\": \"0.24V\", \"000_cell voltage 16\": \"0.22V\",
\"000_cell voltage 17\": \"0.26V\", \"000_cell voltage 18\": \"0.0V\", \"000_cell voltage 19\": \"0.24V\", \"000_cell voltage 20\": \"4.06V\", \"000_cell voltage
21\": \"0.06V\", \"000_cell voltage 22\": \"4.06V\", \"000_cell voltage 23\": \"0.98V\", \"000_cell voltage 24\": \"0.0V\", \"000_cell voltage 25\": \"0.0V\",
\"000_cell voltage 26\": \"2.92V\", \"000_cell voltage 27\": \"0.0V\", \"000_cell voltage 28\": \"0.02V\", \"000_cell voltage 29\": \"4.82V\", \"000_cell voltage
30\": \"0.4V\", \"000_cell voltage 31\": \"0.0V\", \"000_cell voltage 32\": \"0.02V\", \"000_cell voltage 33\": \"3.64V\", \"000_cell voltage 34\": \"0.48V\",
\"000_cell voltage 35\": \"0.76V\", \"000_cell voltage 36\": \"0.76V\", \"000_cell voltage 37\": \"1.44V\", \"000_cell voltage 38\": \"3.26V\", \"000_cell
voltage 39\": \"5.1V\", \"000_cell voltage 40\": \"3.02V\", \"000_cell voltage 41\": \"0.3V\", \"000_cell voltage 42\": \"1.32V\", \"000_cell voltage 43\": 
\"0.26V\", \"000_cell voltage 44\": \"0.22V\", \"000_cell voltage 45\": \"0.24V\", \"000_cell voltage 46\": \"0.26V\", \"000_cell voltage 47\": \"0.24V\",
\"000_cell voltage 48\": \"0.22V\", \"000_cell voltage 49\": \"0.26V\", \"000_cell voltage 50\": \"0.0V\", \"000_cell voltage 51\": \"0.24V\", \"000_cell voltage
52\": \"4.06V\", \"000_cell voltage 53\": \"0.06V\", \"000_cell voltage 54\": \"4.06V\", \"000_cell voltage 55\": \"0.98V\", \"000_cell voltage 56\": \"0.0V\",
\"000_cell voltage 57\": \"0.0V\", \"000_cell voltage 58\": \"2.92V\", \"000_cell voltage 59\": \"0.0V\", \"000_cell voltage 60\": \"0.02V\", \"000_cell voltage
61\": \"4.82V\", \"000_cell voltage 62\": \"0.4V\", \"000_cell voltage 63\": \"0.0V\", \"000_cell voltage 64\": \"0.02V\", \"000_cell voltage 65\": \"3.64V\",
\"000_cell voltage 66\": \"0.48V\", \"000_cell voltage 67\": \"0.76V\", \"000_cell voltage 68\": \"0.76V\", \"000_cell voltage 69\": \"1.44V\", \"000_cell
voltage 70\": \"3.26V\", \"000_cell voltage 71\": \"5.1V\", \"000_cell voltage 72\": \"3.02V\", \"000_cell voltage 73\": \"0.3V\", \"000_cell voltage 74\": 
\"1.32V\", \"000_cell voltage 75\": \"0.26V\", \"000_cell voltage 76\": \"0.22V\", \"000_cell voltage 77\": \"0.24V\", \"000_cell voltage 78\": \"0.26V\",
\"000_cell voltage 79\": \"0.24V\", \"000_cell voltage 80\": \"0.22V\", \"000_cell voltage 81\": \"0.26V\", \"000_cell voltage 82\": \"0.0V\", \"000_cell voltage
83\": \"0.24V\", \"000_cell voltage 84\": \"4.06V\", \"000_cell voltage 85\": \"0.06V\", \"000_cell voltage 86\": \"4.06V\", \"000_cell voltage 87\": \"0.98V\",
\"000_cell voltage 88\": \"0.0V\", \"000_cell voltage 89\": \"0.0V\", \"000_cell voltage 90\": \"2.92V\", \"000_cell voltage 91\": \"0.0V\", \"000_cell voltage
92\": \"0.02V\", \"000_cell voltage 93\": \"4.82V\", \"000_cell voltage 94\": \"0.4V\", \"000_cell voltage 95\": \"0.0V\", \"000_cell voltage 96\": \"0.02V\",
\"000_state of health\": \"100.0%\", \"003_vmcu brake lamp\": \"0.0\", \"000_rapid charge port\": \"0.0\", \"000_airbag h/wire duty\": \"80.0\", 
\"000_battery dc voltage\": \"385.3V\", \"000_battery fan status\": \"0.0\", \"000_normal charge port\": \"0.0\", \"003_vmcu brake related\": \"90.0\", 
\"000_drive motor speed 1\": \"212.0rpm\", \"000_drive motor speed 2\": \"212.0rpm\", \"000_state of charge bms\": \"88.0%\", \"000_battery fan feedback\": 
\"0.0Hz\", \"000_isolation resistance\": \"1000.0kOhm\", \"000_maximum cell voltage\": \"4.0V\", \"000_minimum cell voltage\": \"4.0V\", 
\"000_minimum deterioration\": \"100.0%\", \"000_available charge power\": \"66.79kW\", \"003_vmcu accel pedal depth\": \"0.0%\", 
\"000_battery max temperature\": \"9.0C\", \"000_battery min temperature\": \"9.0C\", \"000_state of charge display\": \"93.0%\", 
\"003_vmcu real vehicle speed\": \"0.0mph\", \"000_maximum cell voltage no.\": \"7.0\", \"000_minimum cell voltage no.\": \"10.0\", 
\"003_vmcu accel pedal related\": \"0.0\", \"000_auxillary battery voltage\": \"14.2V\", \"000_available discharge power\": \"98.0kW\", 
\"000_battery inlet temperature\": \"10.0C\", \"000_cumulative charge current\": \"12837.4Ah\", \"000_cumulative energy charged\": \"4765.9kWh\", 
\"004_calc average cell voltage\": \"4.013541666666667V\", \"000_inverter capacitor voltage\": \"385.0V\", \"003_vmcu motor actual speed rpm\": \"6326.0rpm\",
\"000_battery heater 1 temperature\": \"0.0C\", \"000_battery heater 2 temperature\": \"0.0C\", \"000_cumulative discharge current\": \"12811.0Ah\", 
\"000_cumulative energy discharged\": \"4634.2kWh\", \"000_battery module 01 temperature\": \"9.0C\", \"000_battery module 02 temperature\": \"9.0C\", 
\"000_battery module 03 temperature\": \"9.0C\", \"000_battery module 04 temperature\": \"9.0C\", \"000_battery module 05 temperature\": \"9.0C\", 
\"000_battery module 06 temperature\": \"9.0C\", \"000_battery module 07 temperature\": \"9.0C\", \"000_battery module 08 temperature\": \"9.0C\", 
\"000_battery module 09 temperature\": \"9.0C\", \"000_battery module 10 temperature\": \"9.0C\", \"000_battery module 11 temperature\": \"9.0C\", 
\"000_battery module 12 temperature\": \"9.0C\", \"000_battery cell voltage deviation\": \"0.0V\", \"000_maximum deterioration cell no.\": \"6.0\", 
\"000_minimum deterioration cell no.\": \"10.0\", \"004_calc estimated time 45kw 80% charge\": \"0.0minutes\", \"004_calc average battery module temperature\":
\"9.0C\"}, \"MacAddress\": \"00:1D:A5:00:2E:72\"}"

obd_tmp[obd_tmp$created_on=="2019-07-25 16:22:36","data"] =
  "{\"created_on\": \"2019-07-25 16:22:36\", \"pid\": {\"Speed\": \"0.0\", \"Accuracy\": \"0.0\", \"Latitude\": \"0.0\", \"Longitude\": \"0.0\", \"003_vmcu d\":
\"0.0\", \"003_vmcu n\": \"0.0\", \"003_vmcu p\": \"1.0\", \"003_vmcu r\": \"0.0\", \"Before Parsing\": \"null\\r?\\r\\r\", \"000_hv_charging\": \"0.0\",
\"000_bms ignition\": \"1.0\", \"000_battery power\": \"1.1559kW\", \"000_bms main relay\": \"1.0\", \"000_operating time\": \"1346.829722222222hours\",
\"003_vmcu brakes on\": \"0.0\", \"000_battery current\": \"3.0A\", \"000_cell voltage 01\": \"3.64V\", \"000_cell voltage 02\": \"0.48V\", 
\"000_cell voltage 03\": \"0.76V\", \"000_cell voltage 04\": \"0.76V\", \"000_cell voltage 05\": \"1.44V\", \"000_cell voltage 06\": \"3.26V\", 
\"000_cell voltage 07\": \"5.1V\", \"000_cell voltage 08\": \"3.02V\", \"000_cell voltage 09\": \"0.3V\", \"000_cell voltage 10\": \"1.32V\", 
\"000_cell voltage 11\": \"0.26V\", \"000_cell voltage 12\": \"0.22V\", \"000_cell voltage 13\": \"0.24V\", \"000_cell voltage 14\": \"0.26V\", 
\"000_cell voltage 15\": \"0.24V\", \"000_cell voltage 16\": \"0.22V\", \"000_cell voltage 17\": \"0.26V\", \"000_cell voltage 18\": \"0.0V\", 
\"000_cell voltage 19\": \"0.24V\", \"000_cell voltage 20\": \"4.06V\", \"000_cell voltage 21\": \"0.06V\", \"000_cell voltage 22\": \"4.06V\", 
\"000_cell voltage 23\":\"0.98V\", \"000_cell voltage 24\": \"0.0V\", \"000_cell voltage 25\": \"0.0V\", \"000_cell voltage 26\": \"2.92V\", 
\"000_cell voltage 27\": \"0.0V\",\"000_cell voltage 28\": \"0.02V\", \"000_cell voltage 29\": \"4.82V\", \"000_cell voltage 30\": \"0.4V\", 
\"000_cell voltage 31\": \"0.0V\", \"000_cell voltage 32\": \"0.02V\", \"000_cell voltage 33\": \"3.64V\", \"000_cell voltage 34\": \"0.48V\", 
\"000_cell voltage 35\": \"0.76V\", \"000_cell voltage 36\": \"0.76V\", \"000_cell voltage 37\": \"1.44V\", \"000_cell voltage 38\": \"3.26V\", 
\"000_cell voltage 39\": \"5.1V\", \"000_cell voltage 40\": \"3.02V\", \"000_cell voltage 41\": \"0.3V\", \"000_cell voltage 42\": \"1.32V\", 
\"000_cell voltage 43\": \"0.26V\", \"000_cell voltage 44\": \"0.22V\", \"000_cell voltage 45\": \"0.24V\", \"000_cell voltage 46\": \"0.26V\", 
\"000_cell voltage 47\": \"0.24V\", \"000_cell voltage 48\": \"0.22V\", \"000_cell voltage 49\": \"0.26V\", \"000_cell voltage 50\": \"0.0V\", 
\"000_cell voltage 51\": \"0.24V\", \"000_cell voltage 52\": \"4.06V\", \"000_cell voltage 53\": \"0.06V\", \"000_cell voltage 54\": \"4.06V\", 
\"000_cell voltage 55\": \"0.98V\", \"000_cell voltage 56\": \"0.0V\", \"000_cell voltage 57\": \"0.0V\", \"000_cell voltage 58\": \"2.92V\", 
\"000_cell voltage 59\": \"0.0V\", \"000_cell voltage 60\": \"0.02V\", \"000_cell voltage 61\": \"4.82V\", \"000_cell voltage 62\": \"0.4V\", 
\"000_cell voltage 63\": \"0.0V\", \"000_cell voltage 64\": \"0.02V\", \"000_cell voltage 65\": \"3.64V\", \"000_cell voltage 66\": \"0.48V\", 
\"000_cell voltage 67\": \"0.76V\", \"000_cell voltage 68\": \"0.76V\", \"000_cell voltage 69\": \"1.44V\", \"000_cell voltage 70\": \"3.26V\", 
\"000_cell voltage 71\": \"5.1V\", \"000_cell voltage 72\": \"3.02V\", \"000_cell voltage 73\": \"0.3V\", \"000_cell voltage 74\": \"1.32V\", 
\"000_cell voltage 75\": \"0.26V\", \"000_cell voltage 76\": \"0.22V\", \"000_cell voltage 77\": \"0.24V\", \"000_cell voltage 78\": \"0.26V\", 
\"000_cell voltage 79\": \"0.24V\", \"000_cell voltage 80\": \"0.22V\", \"000_cell voltage 81\": \"0.26V\", \"000_cell voltage 82\": \"0.0V\", 
\"000_cell voltage 83\": \"0.24V\", \"000_cell voltage 84\": \"4.06V\", \"000_cell voltage 85\": \"0.06V\", \"000_cell voltage 86\": \"4.06V\", 
\"000_cell voltage 87\": \"0.98V\", \"000_cell voltage 88\": \"0.0V\", \"000_cell voltage 89\": \"0.0V\", \"000_cell voltage 90\": \"2.92V\", 
\"000_cell voltage 91\": \"0.0V\", \"000_cell voltage 92\": \"0.02V\", \"000_cell voltage 93\": \"4.82V\", \"000_cell voltage 94\": \"0.4V\", 
\"000_cell voltage 95\": \"0.0V\", \"000_cell voltage 96\": \"0.02V\", \"000_state of health\": \"100.0%\", \"003_vmcu brake lamp\": \"0.0\", 
\"000_rapid charge port\": \"0.0\", \"000_airbag h/wire duty\": \"80.0\", \"000_battery dc voltage\": \"385.3V\", \"000_battery fan status\": \"0.0\", 
\"000_normal charge port\": \"0.0\", \"003_vmcu brake related\": \"90.0\", \"000_drive motor speed 1\": \"212.0rpm\", \"000_drive motor speed 2\": \"212.0rpm\",
\"000_state of charge bms\": \"88.0%\", \"000_battery fan feedback\": \"0.0Hz\", \"000_isolation resistance\": \"1000.0kOhm\", \"000_maximum cell voltage\": 
\"4.0V\", \"000_minimum cell voltage\": \"4.0V\", \"000_minimum deterioration\": \"100.0%\", \"000_available charge power\": \"66.79kW\", 
\"003_vmcu accel pedal depth\": \"0.0%\", \"000_battery max temperature\": \"9.0C\", \"000_battery min temperature\": \"9.0C\", \"000_state of charge display\":
\"93.0%\", \"003_vmcu real vehicle speed\": \"0.0mph\", \"000_maximum cell voltage no.\": \"7.0\", \"000_minimum cell voltage no.\": \"10.0\", 
\"003_vmcu accel pedal related\": \"0.0\", \"000_auxillary battery voltage\": \"14.2V\", \"000_available discharge power\": \"98.0kW\", 
\"000_battery inlet temperature\": \"10.0C\", \"000_cumulative charge current\": \"12837.4Ah\", \"000_cumulative energy charged\": \"4765.9kWh\", 
\"004_calc average cell voltage\": \"4.013541666666667V\", \"000_inverter capacitor voltage\": \"385.0V\", \"003_vmcu motor actual speed rpm\": \"6326.0rpm\",
\"000_battery heater 1 temperature\": \"0.0C\", \"000_battery heater 2 temperature\": \"0.0C\", \"000_cumulative discharge current\": \"12811.0Ah\", 
\"000_cumulative energy discharged\": \"4634.2kWh\", \"000_battery module 01 temperature\": \"9.0C\", \"000_battery module 02 temperature\": \"9.0C\", 
\"000_battery module 03 temperature\": \"9.0C\", \"000_battery module 04 temperature\": \"9.0C\", \"000_battery module 05 temperature\": \"9.0C\", 
\"000_battery module 06 temperature\": \"9.0C\", \"000_battery module 07 temperature\": \"9.0C\", \"000_battery module 08 temperature\": \"9.0C\", 
\"000_battery module 09 temperature\": \"9.0C\", \"000_battery module 10 temperature\": \"9.0C\", \"000_battery module 11 temperature\": \"9.0C\", 
\"000_battery module 12 temperature\": \"9.0C\", \"000_battery cell voltage deviation\": \"0.0V\", \"000_maximum deterioration cell no.\": \"6.0\", 
\"000_minimum deterioration cell no.\": \"10.0\", \"004_calc estimated time 45kw 80% charge\": \"0.0minutes\", \"004_calc average battery module temperature\":
\"9.0C\"}, \"MacAddress\": \"00:1D:A5:00:2E:72\"}"

obd_tmp[obd_tmp$created_on=="2019-07-25 16:22:11","data"] = "{\"created_on\": \"2019-07-25 16:22:11\", \"pid\": {\"Speed\": \"0.0\", \"Accuracy\": \"0.0\",
\"Latitude\": \"0.0\", \"Longitude\": \"0.0\", \"003_vmcu d\":\"0.0\", \"003_vmcu n\": \"0.0\", \"003_vmcu p\": \"1.0\", \"003_vmcu r\": \"0.0\",
\"Before Parsing\": \"\\u0000\\u0005null\\r?\\r\\r\", \"000_hv_charging\":\"0.0\", \"000_bms ignition\": \"1.0\", \"000_battery power\": \"1.1559kW\",
\"000_bms main relay\": \"1.0\", \"000_operating time\": \"1346.829722222222hours\",\"003_vmcu brakes on\": \"0.0\", \"000_battery current\": \"3.0A\",
\"000_cell voltage 01\": \"3.64V\", \"000_cell voltage 02\": \"0.48V\", \"000_cell voltage 03\": \"0.76V\", \"000_cell voltage 04\": \"0.76V\", 
\"000_cell voltage 05\": \"1.44V\", \"000_cell voltage 06\": \"3.26V\", \"000_cell voltage 07\": \"5.1V\",\"000_cell voltage 08\": \"3.02V\",
\"000_cell voltage 09\": \"0.3V\", \"000_cell voltage 10\": \"1.32V\", \"000_cell voltage 11\": \"0.26V\", \"000_cell voltage 12\": \"0.22V\",
\"000_cell voltage 13\": \"0.24V\", \"000_cell voltage 14\": \"0.26V\", \"000_cell voltage 15\": \"0.24V\", \"000_cell voltage 16\": \"0.22V\",
\"000_cell voltage 17\": \"0.26V\", \"000_cell voltage 18\": \"0.0V\", \"000_cell voltage 19\": \"0.24V\", \"000_cell voltage 20\": \"4.06V\",
\"000_cell voltage 21\": \"0.06V\", \"000_cell voltage 22\": \"4.06V\", \"000_cell voltage 23\": \"0.98V\", \"000_cell voltage 24\": \"0.0V\",
\"000_cell voltage 25\": \"0.0V\", \"000_cell voltage 26\": \"2.92V\", \"000_cell voltage 27\": \"0.0V\", \"000_cell voltage 28\": \"0.02V\",
\"000_cell voltage 29\": \"4.82V\", \"000_cell voltage 30\": \"0.4V\", \"000_cell voltage 31\": \"0.0V\", \"000_cell voltage 32\": \"0.02V\",
\"000_cell voltage 33\": \"3.64V\", \"000_cell voltage 34\": \"0.48V\", \"000_cell voltage 35\": \"0.76V\", \"000_cell voltage 36\": \"0.76V\",
\"000_cell voltage 37\": \"1.44V\", \"000_cell voltage 38\": \"3.26V\", \"000_cell voltage 39\": \"5.1V\", \"000_cell voltage 40\": \"3.02V\",
\"000_cell voltage 41\": \"0.3V\", \"000_cell voltage 42\": \"1.32V\", \"000_cell voltage 43\": \"0.26V\", \"000_cell voltage 44\": \"0.22V\",
\"000_cell voltage 45\": \"0.24V\", \"000_cell voltage 46\": \"0.26V\", \"000_cell voltage 47\": \"0.24V\",\"000_cell voltage 48\": \"0.22V\",
\"000_cell voltage 49\": \"0.26V\", \"000_cell voltage 50\": \"0.0V\", \"000_cell voltage 51\": \"0.24V\", \"000_cell voltage 52\": \"4.06V\",
\"000_cell voltage 53\": \"0.06V\", \"000_cell voltage 54\": \"4.06V\", \"000_cell voltage 55\": \"0.98V\", \"000_cell voltage 56\": \"0.0V\",
\"000_cell voltage 57\": \"0.0V\", \"000_cell voltage 58\": \"2.92V\", \"000_cell voltage 59\": \"0.0V\", \"000_cell voltage 60\": \"0.02V\",
\"000_cell voltage 61\": \"4.82V\", \"000_cell voltage 62\": \"0.4V\", \"000_cell voltage 63\": \"0.0V\", \"000_cell voltage 64\": \"0.02V\",
\"000_cell voltage 65\": \"3.64V\", \"000_cell voltage 66\": \"0.48V\", \"000_cell voltage 67\": \"0.76V\", \"000_cell voltage 68\": \"0.76V\",
\"000_cell voltage 69\": \"1.44V\", \"000_cell voltage 70\": \"3.26V\", \"000_cell voltage 71\": \"5.1V\", \"000_cell voltage 72\": \"3.02V\",
\"000_cell voltage 73\": \"0.3V\", \"000_cell voltage 74\":  \"1.32V\", \"000_cell voltage 75\": \"0.26V\", \"000_cell voltage 76\": \"0.22V\",
\"000_cell voltage 77\": \"0.24V\", \"000_cell voltage 78\": \"0.26V\", \"000_cell voltage 79\": \"0.24V\", \"000_cell voltage 80\": \"0.22V\",
\"000_cell voltage 81\": \"0.26V\", \"000_cell voltage 82\": \"0.0V\", \"000_cell voltage 83\": \"0.24V\", \"000_cell voltage 84\": \"4.06V\",
\"000_cell voltage 85\": \"0.06V\", \"000_cell voltage 86\": \"4.06V\", \"000_cell voltage 87\": \"0.98V\",\"000_cell voltage 88\": \"0.0V\",
\"000_cell voltage 89\": \"0.0V\", \"000_cell voltage 90\": \"2.92V\", \"000_cell voltage 91\": \"0.0V\", \"000_cell voltage 92\": \"0.02V\",
\"000_cell voltage 93\": \"4.82V\", \"000_cell voltage 94\": \"0.4V\", \"000_cell voltage 95\": \"0.0V\", \"000_cell voltage 96\": \"0.02V\",
\"000_state of health\": \"100.0%\", \"003_vmcu brake lamp\": \"0.0\", \"000_rapid charge port\": \"0.0\", \"000_airbag h/wire duty\": \"80.0\",
\"000_battery dc voltage\": \"385.3V\", \"000_battery fan status\": \"0.0\", \"000_normal charge port\": \"0.0\", \"003_vmcu brake related\": \"90.0\",
\"000_drive motor speed 1\": \"212.0rpm\", \"000_drive motor speed 2\": \"212.0rpm\", \"000_state of charge bms\": \"88.0%\", \"000_battery fan feedback\":
\"0.0Hz\", \"000_isolation resistance\": \"1000.0kOhm\", \"000_maximum cell voltage\": \"4.0V\", \"000_minimum cell voltage\": \"4.0V\", 
\"000_minimum deterioration\": \"100.0%\", \"000_available charge power\": \"66.79kW\", \"003_vmcu accel pedal depth\": \"0.0%\", 
\"000_battery max temperature\": \"9.0C\", \"000_battery min temperature\": \"9.0C\", \"000_state of charge display\": \"93.0%\", 
\"003_vmcu real vehicle speed\": \"0.0mph\", \"000_maximum cell voltage no.\": \"7.0\", \"000_minimum cell voltage no.\": \"10.0\",
\"003_vmcu accel pedal related\": \"0.0\", \"000_auxillary battery voltage\": \"14.2V\", \"000_available discharge power\": \"98.0kW\",
\"000_battery inlet temperature\": \"10.0C\", \"000_cumulative charge current\": \"12837.4Ah\", \"000_cumulative energy charged\": \"4765.9kWh\",
\"004_calc average cell voltage\": \"4.013541666666667V\", \"000_inverter capacitor voltage\": \"385.0V\", \"003_vmcu motor actual speed rpm\": 
\"6326.0rpm\",\"000_battery heater 1 temperature\": \"0.0C\", \"000_battery heater 2 temperature\": \"0.0C\", \"000_cumulative discharge current\": 
\"12811.0Ah\", \"000_cumulative energy discharged\": \"4634.2kWh\", \"000_battery module 01 temperature\": \"9.0C\", \"000_battery module 02 temperature\": 
\"9.0C\", \"000_battery module 03 temperature\": \"9.0C\", \"000_battery module 04 temperature\": \"9.0C\", \"000_battery module 05 temperature\": \"9.0C\",
\"000_battery module 06 temperature\": \"9.0C\", \"000_battery module 07 temperature\": \"9.0C\", \"000_battery module 08 temperature\": \"9.0C\", 
\"000_battery module 09 temperature\": \"9.0C\", \"000_battery module 10 temperature\": \"9.0C\", \"000_battery module 11 temperature\": \"9.0C\", 
\"000_battery module 12 temperature\": \"9.0C\", \"000_battery cell voltage deviation\": \"0.0V\", \"000_maximum deterioration cell no.\": \"6.0\", 
\"000_minimum deterioration cell no.\": \"10.0\", \"004_calc estimated time 45kw 80% charge\": \"0.0minutes\", \"004_calc average battery module temperature\":
\"9.0C\"}, \"MacAddress\": \"00:1D:A5:00:2E:72\"}"


for(num in 2:nrow(obd_tmp)){
  var <- obd_tmp[num,2]
  js <- fromJSON(var)
  data_tmp <- data.frame(js)
  
  for(num1 in 1:ncol(data_tmp)){
    data_tmp[,num1] <- as.factor(data_tmp[,num1])
  }
  
  data<-bind_rows(data,data_tmp)
}

#str(data, list.len=ncol(data)) 칼럼 전부 확인하기 

data <- data[is.na(data$pid),]
data <- subset(data, select= -pid)

data_tmp <- data[!is.na(data$pid.a),]

data[,"not_na"] = 0
#data[,"not_na"] = data$not_na + 1




#vmcu_d의 칼럼들에서 null이 아닌 칼럼이 몇 개 인지 확인
data[!is.na(data$pid.003_vmcu.d),"not_na"] =  data[!is.na(data$pid.003_vmcu.d),"not_na"] + 1
data[!is.na(data$pid.VMCU_Mode._55.vmcu.d),"not_na"] =  data[!is.na(data$pid.VMCU_Mode._55.vmcu.d),"not_na"] + 1
data[!is.na(data$pid.VMCU_Mode.vmcu.d),"not_na"] =  data[!is.na(data$pid.VMCU_Mode.vmcu.d),"not_na"] + 1
data[!is.na(data$pid.vmcu.d),"not_na"] =  data[!is.na(data$pid.vmcu.d),"not_na"] + 1
data[!is.na(data$pid.003_vmcu.gear.stick.d),"not_na"] =  data[!is.na(data$pid.003_vmcu.gear.stick.d),"not_na"] + 1
#vmcu_d의 칼럼들에서 null이 아닌 칼럼의 값을 vmcu_d 컬럼에 대입. 전부 null값이 행이 157개
data[!is.na(data$pid.003_vmcu.d),"vmcu_d"] = data[!is.na(data$pid.003_vmcu.d),"pid.003_vmcu.d"]
data[!is.na(data$pid.VMCU_Mode._55.vmcu.d),"vmcu_d"] = data[!is.na(data$pid.VMCU_Mode._55.vmcu.d),"pid.VMCU_Mode._55.vmcu.d"]
data[!is.na(data$pid.VMCU_Mode.vmcu.d),"vmcu_d"] = data[!is.na(data$pid.VMCU_Mode.vmcu.d),"pid.VMCU_Mode.vmcu.d"]
data[!is.na(data$pid.vmcu.d),"vmcu_d"] = data[!is.na(data$pid.vmcu.d),"pid.vmcu.d"]
data[!is.na(data$pid.003_vmcu.gear.stick.d),"vmcu_d"] = data[!is.na(data$pid.003_vmcu.gear.stick.d),"pid.003_vmcu.gear.stick.d"]





#vmcu_n의 칼럼들에서 null이 아닌 칼럼이 몇 개 인지 확인
data[!is.na(data$pid.003_vmcu.n),"not_na"] =  data[!is.na(data$pid.003_vmcu.n),"not_na"] + 1
data[!is.na(data$pid.VMCU_Mode._55.vmcu.n),"not_na"] =  data[!is.na(data$pid.VMCU_Mode._55.vmcu.n),"not_na"] + 1
data[!is.na(data$pid.VMCU_Mode.vmcu.n),"not_na"] =  data[!is.na(data$pid.VMCU_Mode.vmcu.n),"not_na"] + 1
data[!is.na(data$pid.vmcu.n),"not_na"] =  data[!is.na(data$pid.vmcu.n),"not_na"] + 1
data[!is.na(data$pid.003_vmcu.gear.stick.n),"not_na"] =  data[!is.na(data$pid.003_vmcu.gear.stick.n),"not_na"] + 1
#vmcu_n의 칼럼들에서 null이 아닌 칼럼의 값을 vmcu_n 컬럼에 대입. 전부 null값이 행이 157개
data[!is.na(data$pid.003_vmcu.n),"vmcu_n"] = data[!is.na(data$pid.003_vmcu.n),"pid.003_vmcu.n"]
data[!is.na(data$pid.VMCU_Mode._55.vmcu.n),"vmcu_n"] = data[!is.na(data$pid.VMCU_Mode._55.vmcu.n),"pid.VMCU_Mode._55.vmcu.n"]
data[!is.na(data$pid.VMCU_Mode.vmcu.n),"vmcu_n"] = data[!is.na(data$pid.VMCU_Mode.vmcu.n),"pid.VMCU_Mode.vmcu.n"]
data[!is.na(data$pid.vmcu.n),"vmcu_n"] = data[!is.na(data$pid.vmcu.n),"pid.vmcu.n"]
data[!is.na(data$pid.003_vmcu.gear.stick.n),"vmcu_n"] = data[!is.na(data$pid.003_vmcu.gear.stick.n),"pid.003_vmcu.gear.stick.n"]




#vmcu_p의 칼럼들에서 null이 아닌 칼럼이 몇 개 인지 확인
data[!is.na(data$pid.003_vmcu.p),"not_na"] =  data[!is.na(data$pid.003_vmcu.p),"not_na"] + 1
data[!is.na(data$pid.VMCU_Mode._55.vmcu.p),"not_na"] =  data[!is.na(data$pid.VMCU_Mode._55.vmcu.p),"not_na"] + 1
data[!is.na(data$pid.VMCU_Mode.vmcu.p),"not_na"] =  data[!is.na(data$pid.VMCU_Mode.vmcu.p),"not_na"] + 1
data[!is.na(data$pid.vmcu.p),"not_na"] =  data[!is.na(data$pid.vmcu.p),"not_na"] + 1
data[!is.na(data$pid.003_vmcu.gear.stick.p),"not_na"] =  data[!is.na(data$pid.003_vmcu.gear.stick.p),"not_na"] + 1
#vmcu_p의 칼럼들에서 null이 아닌 칼럼의 값을 vmcu_p 컬럼에 대입. 전부 null값이 행이 157개
data[!is.na(data$pid.003_vmcu.p),"vmcu_p"] = data[!is.na(data$pid.003_vmcu.p),"pid.003_vmcu.p"]
data[!is.na(data$pid.VMCU_Mode._55.vmcu.p),"vmcu_p"] = data[!is.na(data$pid.VMCU_Mode._55.vmcu.p),"pid.VMCU_Mode._55.vmcu.p"]
data[!is.na(data$pid.VMCU_Mode.vmcu.p),"vmcu_p"] = data[!is.na(data$pid.VMCU_Mode.vmcu.p),"pid.VMCU_Mode.vmcu.p"]
data[!is.na(data$pid.vmcu.p),"vmcu_p"] = data[!is.na(data$pid.vmcu.p),"pid.vmcu.p"]
data[!is.na(data$pid.003_vmcu.gear.stick.p),"vmcu_p"] = data[!is.na(data$pid.003_vmcu.gear.stick.p),"pid.003_vmcu.gear.stick.p"]





#vmcu_r의 칼럼들에서 null이 아닌 칼럼이 몇 개 인지 확인
data[!is.na(data$pid.003_vmcu.r),"not_na"] =  data[!is.na(data$pid.003_vmcu.r),"not_na"] + 1
data[!is.na(data$pid.VMCU_Mode._55.vmcu.r),"not_na"] =  data[!is.na(data$pid.VMCU_Mode._55.vmcu.r),"not_na"] + 1
data[!is.na(data$pid.VMCU_Mode.vmcu.r),"not_na"] =  data[!is.na(data$pid.VMCU_Mode.vmcu.r),"not_na"] + 1
data[!is.na(data$pid.vmcu.r),"not_na"] =  data[!is.na(data$pid.vmcu.r),"not_na"] + 1
data[!is.na(data$pid.003_vmcu.gear.stick.r),"not_na"] =  data[!is.na(data$pid.003_vmcu.gear.stick.r),"not_na"] + 1
#vmcu_r의 칼럼들에서 null이 아닌 칼럼의 값을 vmcu_r 컬럼에 대입. 전부 null값이 행이 157개
data[!is.na(data$pid.003_vmcu.r),"vmcu_r"] = data[!is.na(data$pid.003_vmcu.r),"pid.003_vmcu.r"]
data[!is.na(data$pid.VMCU_Mode._55.vmcu.r),"vmcu_r"] = data[!is.na(data$pid.VMCU_Mode._55.vmcu.r),"pid.VMCU_Mode._55.vmcu.r"]
data[!is.na(data$pid.VMCU_Mode.vmcu.r),"vmcu_r"] = data[!is.na(data$pid.VMCU_Mode.vmcu.r),"pid.VMCU_Mode.vmcu.r"]
data[!is.na(data$pid.vmcu.r),"vmcu_r"] = data[!is.na(data$pid.vmcu.r),"pid.vmcu.r"]
data[!is.na(data$pid.003_vmcu.gear.stick.r),"vmcu_r"] = data[!is.na(data$pid.003_vmcu.gear.stick.r),"pid.003_vmcu.gear.stick.r"]







#hv_charging의 칼럼들에서 null이 아닌 칼럼이 몇 개 인지 확인
data[!is.na(data$pid.000_hv_charging),"not_na"] =  data[!is.na(data$pid.000_hv_charging),"not_na"] + 1
data[!is.na(data$pid.000_HV_Charging.),"not_na"] =  data[!is.na(data$pid.000_HV_Charging.),"not_na"] + 1
data[!is.na(data$pid.000_hv_charging.),"not_na"] =  data[!is.na(data$pid.000_hv_charging.),"not_na"] + 1
data[!is.na(data$pid.000_hv.charging.),"not_na"] =  data[!is.na(data$pid.000_hv.charging.),"not_na"] + 1
data[!is.na(data$pid.000_hv.charging),"not_na"] =  data[!is.na(data$pid.000_hv.charging),"not_na"] + 1
#hv_charging의 칼럼들에서 null이 아닌 칼럼의 값을 hv_charging 컬럼에 대입. 전부 null값이 행이 47개
data[!is.na(data$pid.000_hv_charging),"hv_charging"] = data[!is.na(data$pid.000_hv_charging),"pid.000_hv_charging"]
data[!is.na(data$pid.000_HV_Charging.),"hv_charging"] = data[!is.na(data$pid.000_HV_Charging.),"pid.000_HV_Charging."]
data[!is.na(data$pid.000_hv_charging.),"hv_charging"] = data[!is.na(data$pid.000_hv_charging.),"pid.000_hv_charging."]
data[!is.na(data$pid.000_hv.charging.),"hv_charging"] = data[!is.na(data$pid.000_hv.charging.),"pid.000_hv.charging."]
data[!is.na(data$pid.000_hv.charging),"hv_charging"] = data[!is.na(data$pid.000_hv.charging),"pid.000_hv.charging"]





#bms_ignition의 칼럼들에서 null이 아닌 칼럼이 몇 개 인지 확인
data[!is.na(data$pid.000_bms.ignition),"not_na"] =  data[!is.na(data$pid.000_bms.ignition),"not_na"] + 1
data[!is.na(data$pid.000_BMS.Ignition),"not_na"] =  data[!is.na(data$pid.000_BMS.Ignition),"not_na"] + 1
data[!is.na(data$pid.000_bms_ignition),"not_na"] =  data[!is.na(data$pid.000_bms_ignition),"not_na"] + 1
#bms_ignition의 칼럼들에서 null이 아닌 칼럼의 값을 bms_ignition 컬럼에 대입. 전부 null값이 행이 47개
data[!is.na(data$pid.000_bms.ignition),"bms_ignition"] = data[!is.na(data$pid.000_bms.ignition),"pid.000_bms.ignition"]
data[!is.na(data$pid.000_BMS.Ignition),"bms_ignition"] = data[!is.na(data$pid.000_BMS.Ignition),"pid.000_BMS.Ignition"]
data[!is.na(data$pid.000_bms_ignition),"bms_ignition"] = data[!is.na(data$pid.000_bms_ignition),"pid.000_bms_ignition"]





data$pid.000_battery.power. <- as.character(data$pid.000_battery.power.)
data$pid.000_battery.power <- as.character(data$pid.000_battery.power)
data$pid.000_Battery.Power <- as.character(data$pid.000_Battery.Power)
data$pid.000_battery_power <- as.character(data$pid.000_battery_power)

#battery_power 의 칼럼들에서 null이 아닌 칼럼이 몇 개 인지 확인
data[!is.na(data$pid.000_battery.power.),"not_na"] =  data[!is.na(data$pid.000_battery.power.),"not_na"] + 1
data[!is.na(data$pid.000_battery.power),"not_na"] =  data[!is.na(data$pid.000_battery.power),"not_na"] + 1
data[!is.na(data$pid.000_Battery.Power),"not_na"] =  data[!is.na(data$pid.000_Battery.Power),"not_na"] + 1
data[!is.na(data$pid.000_battery_power),"not_na"] =  data[!is.na(data$pid.000_battery_power),"not_na"] + 1
#battery_power 의 칼럼들에서 null이 아닌 칼럼의 값을 battery_power 컬럼에 대입. 전부 null값이 행이 47개
data[!is.na(data$pid.000_battery.power.),"battery_power"] = data[!is.na(data$pid.000_battery.power.),"pid.000_battery.power."]
data[!is.na(data$pid.000_battery.power),"battery_power"] = data[!is.na(data$pid.000_battery.power),"pid.000_battery.power"]
data[!is.na(data$pid.000_Battery.Power),"battery_power"] = data[!is.na(data$pid.000_Battery.Power),"pid.000_Battery.Power"]
data[!is.na(data$pid.000_battery_power),"battery_power"] = data[!is.na(data$pid.000_battery_power),"pid.000_battery_power"]






#bms_main_relay 의 칼럼들에서 null이 아닌 칼럼이 몇 개 인지 확인
data[!is.na(data$pid.000_bms.main.relay),"not_na"] =  data[!is.na(data$pid.000_bms.main.relay),"not_na"] + 1
data[!is.na(data$pid.000_BMS.Main.Relay.),"not_na"] =  data[!is.na(data$pid.000_BMS.Main.Relay.),"not_na"] + 1
data[!is.na(data$pid.000_bms_main_relay.),"not_na"] =  data[!is.na(data$pid.000_bms_main_relay.),"not_na"] + 1
data[!is.na(data$pid.000_bms.main.relay.),"not_na"] =  data[!is.na(data$pid.000_bms.main.relay.),"not_na"] + 1
#bms_main_relay 의 칼럼들에서 null이 아닌 칼럼의 값을 bms_main_relay 컬럼에 대입. 전부 null값이 행이 47개
data[!is.na(data$pid.000_bms.main.relay),"bms_main_relay"] = data[!is.na(data$pid.000_bms.main.relay),"pid.000_bms.main.relay"]
data[!is.na(data$pid.000_BMS.Main.Relay.),"bms_main_relay"] = data[!is.na(data$pid.000_BMS.Main.Relay.),"pid.000_BMS.Main.Relay."]
data[!is.na(data$pid.000_bms_main_relay.),"bms_main_relay"] = data[!is.na(data$pid.000_bms_main_relay.),"pid.000_bms_main_relay."]
data[!is.na(data$pid.000_bms.main.relay.),"bms_main_relay"] = data[!is.na(data$pid.000_bms.main.relay.),"pid.000_bms.main.relay."]







#cumulative_energy_discharged 의 칼럼들에서 null이 아닌 칼럼이 몇 개 인지 확인
data[!is.na(data$pid.000_cumulative.energy.discharged),"not_na"] =  data[!is.na(data$pid.000_cumulative.energy.discharged),"not_na"] + 1
data[!is.na(data$pid.000_Cumulative.Energy.Discharged.),"not_na"] =  data[!is.na(data$pid.000_Cumulative.Energy.Discharged.),"not_na"] + 1
data[!is.na(data$pid.000_cumulative_energy_discharged),"not_na"] =  data[!is.na(data$pid.000_cumulative_energy_discharged),"not_na"] + 1
#cumulative_energy_discharged 의 칼럼들에서 null이 아닌 칼럼의 값을 cumulative_energy_discharged 컬럼에 대입. 전부 null값이 행이 47개
data[!is.na(data$pid.000_cumulative.energy.discharged),"cumulative_energy_discharged"] = data[!is.na(data$pid.000_cumulative.energy.discharged),"pid.000_cumulative.energy.discharged"]
data[!is.na(data$pid.000_Cumulative.Energy.Discharged.),"cumulative_energy_discharged"] = data[!is.na(data$pid.000_Cumulative.Energy.Discharged.),"pid.000_Cumulative.Energy.Discharged."]
data[!is.na(data$pid.000_cumulative_energy_discharged),"cumulative_energy_discharged"] = data[!is.na(data$pid.000_cumulative_energy_discharged),"pid.000_cumulative_energy_discharged"]








#n_ev_speed_kmh 의 칼럼들에서 null이 아닌 칼럼이 몇 개 인지 확인
data[!is.na(data$EVSpeed_kmh),"not_na"] =  data[!is.na(data$EVSpeed_kmh),"not_na"] + 1
data[!is.na(data$ev_speed_kmh),"not_na"] =  data[!is.na(data$ev_speed_kmh),"not_na"] + 1
#n_ev_speed_kmh 의 칼럼들에서 null이 아닌 칼럼의 값을 n_ev_speed_kmh 컬럼에 대입. 전부 null값이 행이 14180개
data[!is.na(data$EVSpeed_kmh),"n_ev_speed_kmh"] = data[!is.na(data$EVSpeed_kmh),"EVSpeed_kmh"]
data[!is.na(data$ev_speed_kmh),"n_ev_speed_kmh"] = data[!is.na(data$ev_speed_kmh),"ev_speed_kmh"]






#operating_time 의 칼럼들에서 null이 아닌 칼럼이 몇 개 인지 확인
data[!is.na(data$pid.000_operating.time),"not_na"] =  data[!is.na(data$pid.000_operating.time),"not_na"] + 1
data[!is.na(data$pid.000_Operating.Time.),"not_na"] =  data[!is.na(data$pid.000_Operating.Time.),"not_na"] + 1
data[!is.na(data$pid.000_operating_time),"not_na"] =  data[!is.na(data$pid.000_operating_time),"not_na"] + 1
#operating_time 의 칼럼들에서 null이 아닌 칼럼의 값을 operating_time 컬럼에 대입. 전부 null값이 행이 48개
data[!is.na(data$pid.000_operating.time),"operating_time"] = data[!is.na(data$pid.000_operating.time),"pid.000_operating.time"]
data[!is.na(data$pid.000_Operating.Time.),"operating_time"] = data[!is.na(data$pid.000_Operating.Time.),"pid.000_Operating.Time."]
data[!is.na(data$pid.000_operating_time),"operating_time"] = data[!is.na(data$pid.000_operating_time),"pid.000_operating_time"]








#battery_current 의 칼럼들에서 null이 아닌 칼럼이 몇 개 인지 확인
data[!is.na(data$pid.000_battery.current),"not_na"] =  data[!is.na(data$pid.000_battery.current),"not_na"] + 1
data[!is.na(data$pid.000_Battery.Current),"not_na"] =  data[!is.na(data$pid.000_Battery.Current),"not_na"] + 1
data[!is.na(data$pid.000_battery_current),"not_na"] =  data[!is.na(data$pid.000_battery_current),"not_na"] + 1
#battery_current 의 칼럼들에서 null이 아닌 칼럼의 값을 battery_current 컬럼에 대입. 전부 null값이 행이 48개
data[!is.na(data$pid.000_battery.current),"battery_current"] = data[!is.na(data$pid.000_battery.current),"pid.000_battery.current"]
data[!is.na(data$pid.000_Battery.Current),"battery_current"] = data[!is.na(data$pid.000_Battery.Current),"pid.000_Battery.Current"]
data[!is.na(data$pid.000_battery_current),"battery_current"] = data[!is.na(data$pid.000_battery_current),"pid.000_battery_current"]






names(data)[names(data) == "pid.000_state.of.health"] <- c("state_of_health")





#data<-subset(data,select=-battery_power) 
data[,"not_na"] = 0
data_tmp <- data[data$not_na >=2,]
data_tmp <- data[is.na(data$operating_time),]




#190806 데이터만으로 분석

data$cumulative_energy_discharged <- gsub("kwh","",data$cumulative_energy_discharged)
data$cumulative_energy_discharged <- gsub("kWh","",data$cumulative_energy_discharged)
data$cumulative_energy_discharged <- as.factor(data$cumulative_energy_discharged)
data$cumulative_energy_discharged <- as.character(data$cumulative_energy_discharged)
data$cumulative_energy_discharged <- as.numeric(data$cumulative_energy_discharged)
data$n_ev_speed_kmh <- as.factor(data$n_ev_speed_kmh)
data$n_ev_speed_kmh <- as.numeric(data$n_ev_speed_kmh)

data$battery_power <- gsub("kW","",data$battery_power)
data$battery_power <- as.factor(data$battery_power)
data$battery_power <- as.character(data$battery_power)
data$battery_power <- as.numeric(data$battery_power)

data$operating_time <- gsub("hours","",data$operating_time)
data$operating_time <- as.factor(data$operating_time)
data$operating_time <- as.character(data$operating_time)
data$operating_time <- as.numeric(data$operating_time)

data$state_of_health <- gsub("%","",data$state_of_health)
data$state_of_health <- as.factor(data$state_of_health)
data$state_of_health <- as.character(data$state_of_health)
data$state_of_health <- as.numeric(data$state_of_health)


# 0806 데이터로만 분석 
data_0806 <- data[grep("2019-08-06",data$created_on),]

data_0806 <- data_0806[,c("n_ev_speed_kmh","cumulative_energy_discharged")]


install.packages("GGally")
library(GGally)


ggcorr(data_0806, label = TRUE)




# 0806 데이터 practice voltage 제거
data_0806 <- subset(data_0806,select=-practicevolt)



# 0806 데이터 단위 제거 및 데이터 타입 변환

# batterypower
batterypower_0806 <- data_0806[is.na(data_0806$pid.000_batterypower),] # battery power 데이터가 NA인 데이터는 없음
rm(batterypower_0806)

data_0806$pid.000_batterypower <- gsub("kw","",data_0806$pid.000_batterypower)
data_0806$pid.000_batterypower <- as.factor(data_0806$pid.000_batterypower)
data_0806$pid.000_batterypower <- as.character(data_0806$pid.000_batterypower)
data_0806$pid.000_batterypower <- as.numeric(data_0806$pid.000_batterypower)

# operatingtime
test_0806 <- data_0806[is.na(data_0806$pid.000_operatingtime),] # operatingtime 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_operatingtime <- gsub("hours","",data_0806$pid.000_operatingtime)
data_0806$pid.000_operatingtime <- as.factor(data_0806$pid.000_operatingtime)
data_0806$pid.000_operatingtime <- as.character(data_0806$pid.000_operatingtime)
data_0806$pid.000_operatingtime <- as.numeric(data_0806$pid.000_operatingtime)

#batterycurrent
test_0806 <- data_0806[is.na(data_0806$pid.000_batterycurrent),] # batterycurrent 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_batterycurrent <- gsub("a","",data_0806$pid.000_batterycurrent)
data_0806$pid.000_batterycurrent <- as.factor(data_0806$pid.000_batterycurrent)
data_0806$pid.000_batterycurrent <- as.character(data_0806$pid.000_batterycurrent)
data_0806$pid.000_batterycurrent <- as.numeric(data_0806$pid.000_batterycurrent)

#cellvoltage
for(cnt in 13:108){  # cellvoltage 데이터가 NA인 데이터는 없음
  if(nrow(data_0806[is.na(data_0806[,cnt]),])!=0){
    print(cnt)
  }
}

for(cnt in 13:108){  
  data_0806[,cnt] <- gsub("v","",data_0806[,cnt])
  data_0806[,cnt] <- as.factor(data_0806[,cnt])
  data_0806[,cnt] <- as.character(data_0806[,cnt])
  data_0806[,cnt] <- as.numeric(data_0806[,cnt])
}

#stateofhealth
test_0806 <- data_0806[is.na(data_0806$pid.000_stateofhealth),] # stateofhealth 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_stateofhealth <- gsub("%","",data_0806$pid.000_stateofhealth)
data_0806$pid.000_stateofhealth <- as.factor(data_0806$pid.000_stateofhealth)
data_0806$pid.000_stateofhealth <- as.character(data_0806$pid.000_stateofhealth)
data_0806$pid.000_stateofhealth <- as.numeric(data_0806$pid.000_stateofhealth)

#batterydcvoltage
test_0806 <- data_0806[is.na(data_0806$pid.000_batterydcvoltage),] # batterydcvoltage 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_batterydcvoltage <- gsub("v","",data_0806$pid.000_batterydcvoltage)
data_0806$pid.000_batterydcvoltage <- as.factor(data_0806$pid.000_batterydcvoltage)
data_0806$pid.000_batterydcvoltage <- as.character(data_0806$pid.000_batterydcvoltage)
data_0806$pid.000_batterydcvoltage <- as.numeric(data_0806$pid.000_batterydcvoltage)

#drivemotorspeed1
test_0806 <- data_0806[is.na(data_0806$pid.000_drivemotorspeed1),] # drivemotorspeed1 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_drivemotorspeed1 <- gsub("rpm","",data_0806$pid.000_drivemotorspeed1)
data_0806$pid.000_drivemotorspeed1 <- as.factor(data_0806$pid.000_drivemotorspeed1)
data_0806$pid.000_drivemotorspeed1 <- as.character(data_0806$pid.000_drivemotorspeed1)
data_0806$pid.000_drivemotorspeed1 <- as.numeric(data_0806$pid.000_drivemotorspeed1)

#drivemotorspeed2
test_0806 <- data_0806[is.na(data_0806$pid.000_drivemotorspeed2),] # drivemotorspeed2 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_drivemotorspeed2 <- gsub("rpm","",data_0806$pid.000_drivemotorspeed2)
data_0806$pid.000_drivemotorspeed2 <- as.factor(data_0806$pid.000_drivemotorspeed2)
data_0806$pid.000_drivemotorspeed2 <- as.character(data_0806$pid.000_drivemotorspeed2)
data_0806$pid.000_drivemotorspeed2 <- as.numeric(data_0806$pid.000_drivemotorspeed2)

#stateofchargebms
test_0806 <- data_0806[is.na(data_0806$pid.000_stateofchargebms),] # stateofchargebms 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_stateofchargebms <- gsub("%","",data_0806$pid.000_stateofchargebms)
data_0806$pid.000_stateofchargebms <- as.factor(data_0806$pid.000_stateofchargebms)
data_0806$pid.000_stateofchargebms <- as.character(data_0806$pid.000_stateofchargebms)
data_0806$pid.000_stateofchargebms <- as.numeric(data_0806$pid.000_stateofchargebms)

#batteryfanfeedback
test_0806 <- data_0806[is.na(data_0806$pid.000_batteryfanfeedback),] # batteryfanfeedback 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_batteryfanfeedback <- gsub("hz","",data_0806$pid.000_batteryfanfeedback)
data_0806$pid.000_batteryfanfeedback <- as.factor(data_0806$pid.000_batteryfanfeedback)
data_0806$pid.000_batteryfanfeedback <- as.character(data_0806$pid.000_batteryfanfeedback)
data_0806$pid.000_batteryfanfeedback <- as.numeric(data_0806$pid.000_batteryfanfeedback)

#isolationresistance
test_0806 <- data_0806[is.na(data_0806$pid.000_isolationresistance),] # isolationresistance 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_isolationresistance <- gsub("kohm","",data_0806$pid.000_isolationresistance)
data_0806$pid.000_isolationresistance <- as.factor(data_0806$pid.000_isolationresistance)
data_0806$pid.000_isolationresistance <- as.character(data_0806$pid.000_isolationresistance)
data_0806$pid.000_isolationresistance <- as.numeric(data_0806$pid.000_isolationresistance)

#maximumcellvoltage
test_0806 <- data_0806[is.na(data_0806$pid.000_maximumcellvoltage),] # maximumcellvoltage 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_maximumcellvoltage <- gsub("v","",data_0806$pid.000_maximumcellvoltage)
data_0806$pid.000_maximumcellvoltage <- as.factor(data_0806$pid.000_maximumcellvoltage)
data_0806$pid.000_maximumcellvoltage <- as.character(data_0806$pid.000_maximumcellvoltage)
data_0806$pid.000_maximumcellvoltage <- as.numeric(data_0806$pid.000_maximumcellvoltage)

#minimumcellvoltage
test_0806 <- data_0806[is.na(data_0806$pid.000_minimumcellvoltage),] # minimumcellvoltage 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_minimumcellvoltage <- gsub("v","",data_0806$pid.000_minimumcellvoltage)
data_0806$pid.000_minimumcellvoltage <- as.factor(data_0806$pid.000_minimumcellvoltage)
data_0806$pid.000_minimumcellvoltage <- as.character(data_0806$pid.000_minimumcellvoltage)
data_0806$pid.000_minimumcellvoltage <- as.numeric(data_0806$pid.000_minimumcellvoltage)

#minimumdeterioration
test_0806 <- data_0806[is.na(data_0806$pid.000_minimumdeterioration),] # minimumdeterioration 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_minimumdeterioration <- gsub("%","",data_0806$pid.000_minimumdeterioration)
data_0806$pid.000_minimumdeterioration <- as.factor(data_0806$pid.000_minimumdeterioration)
data_0806$pid.000_minimumdeterioration <- as.character(data_0806$pid.000_minimumdeterioration)
data_0806$pid.000_minimumdeterioration <- as.numeric(data_0806$pid.000_minimumdeterioration)

#availablechargepower
test_0806 <- data_0806[is.na(data_0806$pid.000_availablechargepower),] # availablechargepower 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_availablechargepower <- gsub("kw","",data_0806$pid.000_availablechargepower)
data_0806$pid.000_availablechargepower <- as.factor(data_0806$pid.000_availablechargepower)
data_0806$pid.000_availablechargepower <- as.character(data_0806$pid.000_availablechargepower)
data_0806$pid.000_availablechargepower <- as.numeric(data_0806$pid.000_availablechargepower)

#vmcuaccelpedaldepth
test_0806 <- data_0806[is.na(data_0806$pid.003_vmcuaccelpedaldepth),] # vmcuaccelpedaldepth 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.003_vmcuaccelpedaldepth <- gsub("%","",data_0806$pid.003_vmcuaccelpedaldepth)
data_0806$pid.003_vmcuaccelpedaldepth <- as.factor(data_0806$pid.003_vmcuaccelpedaldepth)
data_0806$pid.003_vmcuaccelpedaldepth <- as.character(data_0806$pid.003_vmcuaccelpedaldepth)
data_0806$pid.003_vmcuaccelpedaldepth <- as.numeric(data_0806$pid.003_vmcuaccelpedaldepth)

#batterymaxtemperature 
test_0806 <- data_0806[is.na(data_0806$pid.000_batterymaxtemperature),] # batterymaxtemperature 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_batterymaxtemperature <- gsub("c","",data_0806$pid.000_batterymaxtemperature)
data_0806$pid.000_batterymaxtemperature <- as.factor(data_0806$pid.000_batterymaxtemperature)
data_0806$pid.000_batterymaxtemperature <- as.character(data_0806$pid.000_batterymaxtemperature)
data_0806$pid.000_batterymaxtemperature <- as.numeric(data_0806$pid.000_batterymaxtemperature)

#batterymintemperature 
test_0806 <- data_0806[is.na(data_0806$pid.000_batterymintemperature),] # batterymintemperature 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_batterymintemperature <- gsub("c","",data_0806$pid.000_batterymintemperature)
data_0806$pid.000_batterymintemperature <- as.factor(data_0806$pid.000_batterymintemperature)
data_0806$pid.000_batterymintemperature <- as.character(data_0806$pid.000_batterymintemperature)
data_0806$pid.000_batterymintemperature <- as.numeric(data_0806$pid.000_batterymintemperature)

#stateofchargedisplay
test_0806 <- data_0806[is.na(data_0806$pid.000_stateofchargedisplay),] # stateofchargedisplay 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_stateofchargedisplay <- gsub("%","",data_0806$pid.000_stateofchargedisplay)
data_0806$pid.000_stateofchargedisplay <- as.factor(data_0806$pid.000_stateofchargedisplay)
data_0806$pid.000_stateofchargedisplay <- as.character(data_0806$pid.000_stateofchargedisplay)
data_0806$pid.000_stateofchargedisplay <- as.numeric(data_0806$pid.000_stateofchargedisplay)

#vmcurealvehiclespeed
test_0806 <- data_0806[is.na(data_0806$pid.003_vmcurealvehiclespeed),] # vmcurealvehiclespeed 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.003_vmcurealvehiclespeed <- gsub("mph","",data_0806$pid.003_vmcurealvehiclespeed)
data_0806$pid.003_vmcurealvehiclespeed <- as.factor(data_0806$pid.003_vmcurealvehiclespeed)
data_0806$pid.003_vmcurealvehiclespeed <- as.character(data_0806$pid.003_vmcurealvehiclespeed)
data_0806$pid.003_vmcurealvehiclespeed <- as.numeric(data_0806$pid.003_vmcurealvehiclespeed)

#auxillarybatteryvoltage
test_0806 <- data_0806[is.na(data_0806$pid.000_auxillarybatteryvoltage),] # auxillarybatteryvoltage 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_auxillarybatteryvoltage <- gsub("v","",data_0806$pid.000_auxillarybatteryvoltage)
data_0806$pid.000_auxillarybatteryvoltage <- as.factor(data_0806$pid.000_auxillarybatteryvoltage)
data_0806$pid.000_auxillarybatteryvoltage <- as.character(data_0806$pid.000_auxillarybatteryvoltage)
data_0806$pid.000_auxillarybatteryvoltage <- as.numeric(data_0806$pid.000_auxillarybatteryvoltage)

#availabledischargepower
test_0806 <- data_0806[is.na(data_0806$pid.000_availabledischargepower),] # availabledischargepower 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_availabledischargepower <- gsub("kw","",data_0806$pid.000_availabledischargepower)
data_0806$pid.000_availabledischargepower <- as.factor(data_0806$pid.000_availabledischargepower)
data_0806$pid.000_availabledischargepower <- as.character(data_0806$pid.000_availabledischargepower)
data_0806$pid.000_availabledischargepower <- as.numeric(data_0806$pid.000_availabledischargepower)

#batteryinlettemperature
test_0806 <- data_0806[is.na(data_0806$pid.000_batteryinlettemperature),] # batteryinlettemperature 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_batteryinlettemperature <- gsub("c","",data_0806$pid.000_batteryinlettemperature)
data_0806$pid.000_batteryinlettemperature <- as.factor(data_0806$pid.000_batteryinlettemperature)
data_0806$pid.000_batteryinlettemperature <- as.character(data_0806$pid.000_batteryinlettemperature)
data_0806$pid.000_batteryinlettemperature <- as.numeric(data_0806$pid.000_batteryinlettemperature)

#cumulativechargecurrent
test_0806 <- data_0806[is.na(data_0806$pid.000_cumulativechargecurrent),] # cumulativechargecurrent 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_cumulativechargecurrent <- gsub("ah","",data_0806$pid.000_cumulativechargecurrent)
data_0806$pid.000_cumulativechargecurrent <- as.factor(data_0806$pid.000_cumulativechargecurrent)
data_0806$pid.000_cumulativechargecurrent <- as.character(data_0806$pid.000_cumulativechargecurrent)
data_0806$pid.000_cumulativechargecurrent <- as.numeric(data_0806$pid.000_cumulativechargecurrent)

#cumulativeenergycharged
test_0806 <- data_0806[is.na(data_0806$pid.000_cumulativeenergycharged),] # cumulativeenergycharged 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_cumulativeenergycharged <- gsub("kwh","",data_0806$pid.000_cumulativeenergycharged)
data_0806$pid.000_cumulativeenergycharged <- as.factor(data_0806$pid.000_cumulativeenergycharged)
data_0806$pid.000_cumulativeenergycharged <- as.character(data_0806$pid.000_cumulativeenergycharged)
data_0806$pid.000_cumulativeenergycharged <- as.numeric(data_0806$pid.000_cumulativeenergycharged)

#calcaveragecellvoltage
test_0806 <- data_0806[is.na(data_0806$pid.004_calcaveragecellvoltage),] # calcaveragecellvoltage 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.004_calcaveragecellvoltage <- gsub("v","",data_0806$pid.004_calcaveragecellvoltage)
data_0806$pid.004_calcaveragecellvoltage <- as.factor(data_0806$pid.004_calcaveragecellvoltage)
data_0806$pid.004_calcaveragecellvoltage <- as.character(data_0806$pid.004_calcaveragecellvoltage)
data_0806$pid.004_calcaveragecellvoltage <- as.numeric(data_0806$pid.004_calcaveragecellvoltage)

#invertercapacitorvoltage
test_0806 <- data_0806[is.na(data_0806$pid.000_invertercapacitorvoltage),] # invertercapacitorvoltage 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_invertercapacitorvoltage <- gsub("v","",data_0806$pid.000_invertercapacitorvoltage)
data_0806$pid.000_invertercapacitorvoltage <- as.factor(data_0806$pid.000_invertercapacitorvoltage)
data_0806$pid.000_invertercapacitorvoltage <- as.character(data_0806$pid.000_invertercapacitorvoltage)
data_0806$pid.000_invertercapacitorvoltage <- as.numeric(data_0806$pid.000_invertercapacitorvoltage)

#vmcumotoractualspeedrpm
test_0806 <- data_0806[is.na(data_0806$pid.003_vmcumotoractualspeedrpm),] # vmcumotoractualspeedrpm 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.003_vmcumotoractualspeedrpm <- gsub("rpm","",data_0806$pid.003_vmcumotoractualspeedrpm)
data_0806$pid.003_vmcumotoractualspeedrpm <- as.factor(data_0806$pid.003_vmcumotoractualspeedrpm)
data_0806$pid.003_vmcumotoractualspeedrpm <- as.character(data_0806$pid.003_vmcumotoractualspeedrpm)
data_0806$pid.003_vmcumotoractualspeedrpm <- as.numeric(data_0806$pid.003_vmcumotoractualspeedrpm)

#batteryheater1temperature
test_0806 <- data_0806[is.na(data_0806$pid.000_batteryheater1temperature),] # batteryheater1temperature 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_batteryheater1temperature <- gsub("c","",data_0806$pid.000_batteryheater1temperature)
data_0806$pid.000_batteryheater1temperature <- as.factor(data_0806$pid.000_batteryheater1temperature)
data_0806$pid.000_batteryheater1temperature <- as.character(data_0806$pid.000_batteryheater1temperature)
data_0806$pid.000_batteryheater1temperature <- as.numeric(data_0806$pid.000_batteryheater1temperature)

#batteryheater2temperature
test_0806 <- data_0806[is.na(data_0806$pid.000_batteryheater2temperature),] # batteryheater2temperature 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_batteryheater2temperature <- gsub("c","",data_0806$pid.000_batteryheater2temperature)
data_0806$pid.000_batteryheater2temperature <- as.factor(data_0806$pid.000_batteryheater2temperature)
data_0806$pid.000_batteryheater2temperature <- as.character(data_0806$pid.000_batteryheater2temperature)
data_0806$pid.000_batteryheater2temperature <- as.numeric(data_0806$pid.000_batteryheater2temperature)

#cumulativedischargecurrent
test_0806 <- data_0806[is.na(data_0806$pid.000_cumulativedischargecurrent),] # cumulativedischargecurrent 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_cumulativedischargecurrent <- gsub("ah","",data_0806$pid.000_cumulativedischargecurrent)
data_0806$pid.000_cumulativedischargecurrent <- as.factor(data_0806$pid.000_cumulativedischargecurrent)
data_0806$pid.000_cumulativedischargecurrent <- as.character(data_0806$pid.000_cumulativedischargecurrent)
data_0806$pid.000_cumulativedischargecurrent <- as.numeric(data_0806$pid.000_cumulativedischargecurrent)

#cumulativeenergydischarged
test_0806 <- data_0806[is.na(data_0806$pid.000_cumulativeenergydischarged),] # cumulativeenergydischarged 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_cumulativeenergydischarged <- gsub("kwh","",data_0806$pid.000_cumulativeenergydischarged)
data_0806$pid.000_cumulativeenergydischarged <- as.factor(data_0806$pid.000_cumulativeenergydischarged)
data_0806$pid.000_cumulativeenergydischarged <- as.character(data_0806$pid.000_cumulativeenergydischarged)
data_0806$pid.000_cumulativeenergydischarged <- as.numeric(data_0806$pid.000_cumulativeenergydischarged)

# batterymodule01temperature ~ batterymodule12temperature
for(cnt in 146:157){  # batterymodule01temperature ~ batterymodule12temperature 데이터가 NA인 데이터는 없음
  if(nrow(data_0806[is.na(data_0806[,cnt]),])==0){
    print(cnt)
  }
}

for(cnt in 146:157){  
  data_0806[,cnt] <- gsub("c","",data_0806[,cnt])
  data_0806[,cnt] <- as.factor(data_0806[,cnt])
  data_0806[,cnt] <- as.character(data_0806[,cnt])
  data_0806[,cnt] <- as.numeric(data_0806[,cnt])
}

#batterycellvoltagedeviation
test_0806 <- data_0806[is.na(data_0806$pid.000_batterycellvoltagedeviation),] # batterycellvoltagedeviation 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.000_batterycellvoltagedeviation <- gsub("v","",data_0806$pid.000_batterycellvoltagedeviation)
data_0806$pid.000_batterycellvoltagedeviation <- as.factor(data_0806$pid.000_batterycellvoltagedeviation)
data_0806$pid.000_batterycellvoltagedeviation <- as.character(data_0806$pid.000_batterycellvoltagedeviation)
data_0806$pid.000_batterycellvoltagedeviation <- as.numeric(data_0806$pid.000_batterycellvoltagedeviation)

#calcaveragebatterymoduletemperature
test_0806 <- data_0806[is.na(data_0806$pid.004_calcaveragebatterymoduletemperature),] # calcaveragebatterymoduletemperature 데이터가 NA인 데이터는 없음
rm(test_0806)

data_0806$pid.004_calcaveragebatterymoduletemperature <- gsub("c","",data_0806$pid.004_calcaveragebatterymoduletemperature)
data_0806$pid.004_calcaveragebatterymoduletemperature <- as.factor(data_0806$pid.004_calcaveragebatterymoduletemperature)
data_0806$pid.004_calcaveragebatterymoduletemperature <- as.character(data_0806$pid.004_calcaveragebatterymoduletemperature)
data_0806$pid.004_calcaveragebatterymoduletemperature <- as.numeric(data_0806$pid.004_calcaveragebatterymoduletemperature)

#vmcuambienttemp
test_0806 <- data_0806[is.na(data_0806$pid.003_vmcuambienttemp),] # vmcuambienttemp 데이터가 NA인 데이터는 1111개 
rm(test_0806)

data_0806$pid.003_vmcuambienttemp <- gsub("c","",data_0806$pid.003_vmcuambienttemp)
data_0806$pid.003_vmcuambienttemp <- as.factor(data_0806$pid.003_vmcuambienttemp)
data_0806$pid.003_vmcuambienttemp <- as.character(data_0806$pid.003_vmcuambienttemp)
data_0806$pid.003_vmcuambienttemp <- as.numeric(data_0806$pid.003_vmcuambienttemp)

#$ speed                                      : chr  "0.0" "0.0" "0.0" "0.0" ... -> gps 관련 위치 데이터로 추정

#$ removeunitspeed                            : chr  "75.81490000000001" "72.38560000000001" "66.10660000000001" "60.1657" ...
#$ evspeed_kmh                                : chr  NA NA NA NA ...
#$ ev_speed_kmh                               : chr  NA NA NA NA ...

data_0806$speed <- as.factor(data_0806$speed)
data_0806$removeunitspeed <- as.factor(data_0806$removeunitspeed)
data_0806$evspeed_kmh <- as.factor(data_0806$evspeed_kmh)
data_0806$ev_speed_kmh <- as.factor(data_0806$ev_speed_kmh)

data_0806$sp <- 0
# data_0806[!is.na(data_0806$speed),"sp"] <-data_0806[!is.na(data_0806$speed),"sp"] +1 
# -> speed와 removeunitspeed에 동시에 값이 존재하는 데이터 발견 -> speed는 제외하고 아래 3개의 컬럼 합침

data_0806[!is.na(data_0806$removeunitspeed),"sp"] <-data_0806[!is.na(data_0806$removeunitspeed),"sp"] +1 
data_0806[!is.na(data_0806$evspeed_kmh),"sp"] <-data_0806[!is.na(data_0806$evspeed_kmh),"sp"] +1
data_0806[!is.na(data_0806$ev_speed_kmh),"sp"] <-data_0806[!is.na(data_0806$ev_speed_kmh),"sp"] +1
ss <- data_0806[data_0806$sp != 1,] # 3개의 컬럼의 NA를 비교한 결과, 동시에 값이 존재하는 데이터 없음
rm(ss)

# removeunitspeed, evspeed_kmh, ev_speed_kmh 3개의 컬럼을 speed_ev_kmh로 합침
a <- data_0806[!is.na(data_0806$removeunitspeed),]
b <- data_0806[!is.na(data_0806$evspeed_kmh),]
c <- data_0806[!is.na(data_0806$ev_speed_kmh),]

a$speed_ev_kmh <- a$removeunitspeed
b$speed_ev_kmh <- b$evspeed_kmh
c$speed_ev_kmh <- c$ev_speed_kmh

data_0806 <- bind_rows(a,b,c)
rm(a,b,c)
data_0806 <- subset(data_0806,select=-sp)

data_0806$speed_ev_kmh <- as.factor(data_0806$speed_ev_kmh)
data_0806$speed_ev_kmh <- as.character(data_0806$speed_ev_kmh)
data_0806$speed_ev_kmh <- as.numeric(data_0806$speed_ev_kmh)

d0806 <- subset(data_0806,select=-removeunitspeed)
d0806 <- subset(d0806,select=-evspeed_kmh)
d0806 <- subset(d0806,select=-ev_speed_kmh)



# 0806 데이터 그래프화

# created on 컬럼 날짜 형식을 변환
d0806$created_on <- as.POSIXct(d0806$created_on,format="%Y-%m-%d%H:%M:%S")
# 날짜 - SOC(BMS) 그래프
plot(d0806$pid.000_stateofchargebms ~ d0806$created_on)
# 날짜 -  battery dc voltage 그래프
plot(d0806$pid.000_batterydcvoltage ~ d0806$created_on)
# 날짜 -  battery power 그래프
plot(d0806$pid.000_batterypower ~ d0806$created_on)
# 날짜 -  operatingtime 그래프
plot(d0806$pid.000_operatingtime ~ d0806$created_on)
# 날짜 -  battery current 그래프
plot(d0806$pid.000_batterycurrent ~ d0806$created_on)
# 날짜 -  state of health 그래프
plot(d0806$pid.000_stateofhealth ~ d0806$created_on)
# 날짜 -  drive motor speed1 그래프
plot(d0806$pid.000_drivemotorspeed1 ~ d0806$created_on)
# 날짜 -  drive motor speed2 그래프
plot(d0806$pid.000_drivemotorspeed2 ~ d0806$created_on)
# 날짜 -  battery fan feedback 그래프
plot(d0806$pid.000_batteryfanfeedback ~ d0806$created_on)
# 날짜 -  calcaveragebatterymoduletemperature 그래프
plot(d0806$pid.004_calcaveragebatterymoduletemperature ~ d0806$created_on)
# 날짜 -  vmcuambienttemp 그래프
plot(d0806$pid.003_vmcuambienttemp ~ d0806$created_on)
# 날짜 -  speed_ev_kmh 그래프
plot(d0806$speed_ev_kmh ~ d0806$created_on)
# 날짜 -  auxillarybatteryvoltage 그래프
plot(d0806$pid.000_auxillarybatteryvoltage ~ d0806$created_on)
# 날짜 -  minimumcellvoltage 그래프
plot(d0806$pid.000_minimumcellvoltage ~ d0806$created_on)
# 날짜 -  vmcuaccelpedaldepth 그래프
plot(d0806$pid.003_vmcuaccelpedaldepth ~ d0806$created_on)
# 날짜 -  invertercapacitorvoltage 그래프
plot(d0806$pid.000_invertercapacitorvoltage ~ d0806$created_on)



# 이중축 그래프 날짜 - SOC(BMS) - battery dc voltage 그래프
twoord.plot(d0806$created_on, d0806$pid.000_stateofchargebms, d0806$created_on, d0806$pid.000_batterydcvoltage,
            xlab = "created on", ylab = "state of charge bms", rylab = "battery dc voltage",lcol=4)

# 이중축 그래프 날짜 - battery average temp - batteryfanfeedbacke 그래프
twoord.plot(d0806$created_on, d0806$pid.004_calcaveragebatterymoduletemperature, d0806$created_on, d0806$pid.000_batteryfanfeedback,
            xlab = "created on", ylab = "battery avg temp", rylab = "battery fan feedback",lcol=4)


# 이중축 그래프 날짜 - SOC(BMS) - vmcuambienttemp 그래프
twoord.plot(d0806$created_on, d0806$pid.000_stateofchargebms, d0806$created_on, d0806$pid.003_vmcuambienttemp,
            xlab = "created on", ylab = "state of charge bms", rylab = "ambienttemp",lcol=4)

# 이중축 그래프 날짜 - SOC(BMS) - SOC(Display) 그래프
twoord.plot(d0806$created_on, d0806$pid.000_stateofchargebms, d0806$created_on, d0806$pid.000_stateofchargedisplay,
            xlab = "created on", ylab = "SOC(BMS)", rylab = "SOC(Display)",lcol=4)

# 이중축 그래프 날짜 - SOC(BMS) - battery power 그래프
twoord.plot(d0806$created_on, d0806$pid.000_stateofchargebms, d0806$created_on, d0806$pid.000_batterypower,
            xlab = "created on", ylab = "state of charge bms", rylab = "battery power",lcol=4)

# 이중축 그래프 날짜 - speed - batterydcvoltage 그래프
twoord.plot(d0806$created_on, d0806$speed_ev_kmh, d0806$created_on, d0806$pid.000_batterydcvoltage,
            xlab = "created on", ylab = "speed", rylab = "batterydcvoltage",lcol=4)

# 이중축 그래프 날짜 - avgtemp- batterydcvoltage 그래프
twoord.plot(d0806$created_on, d0806$pid.004_calcaveragebatterymoduletemperature, d0806$created_on, d0806$pid.000_batterydcvoltage,
            xlab = "created on", ylab = "temp", rylab = "batterydcvoltage",lcol=4)

# 이중축 그래프 날짜 - avgtemp- cumulativeenergydischarged 그래프
twoord.plot(d0806$created_on, d0806$pid.004_calcaveragebatterymoduletemperature, d0806$created_on, d0806$pid.000_cumulativeenergydischarged,
            xlab = "created on", ylab = "temp", rylab = "cumulativeenergydischarged",lcol=4)

# 이중축 그래프 날짜 - avgtemp- cumulativedischargecurrent 그래프
twoord.plot(d0806$created_on, d0806$pid.004_calcaveragebatterymoduletemperature, d0806$created_on, d0806$pid.000_cumulativedischargecurrent,
            xlab = "created on", ylab = "temp", rylab = "cumulativedischargecurrent",lcol=4)

# 이중축 그래프 날짜 - avgtemp- vmcuambienttemp 그래프
twoord.plot(d0806$created_on, d0806$pid.004_calcaveragebatterymoduletemperature, d0806$created_on, d0806$pid.003_vmcuambienttemp,
            xlab = "created on", ylab = "temp", rylab = "vmcuambienttemp",lcol=4)

# 이중축 그래프 날짜 - speed- soc(bms) 그래프
twoord.plot(d0806$created_on, d0806$speed_ev_kmh, d0806$created_on, d0806$pid.000_stateofchargebms,
            xlab = "created on", ylab = "speed", rylab = "soc(bms)",lcol=4)


# 이중축 그래프 날짜 - speed- soc(bms) 그래프
twoord.plot(d0806$created_on, d0806$pid.000_cumulativeenergydischarged, d0806$created_on, d0806$pid.000_stateofchargebms,
            xlab = "created on", ylab = "cumulativeenergydischarged", rylab = "soc(bms)",lcol=4)

# 이중축 그래프 날짜 - speed- soc(bms) 그래프
twoord.plot(d0806$created_on, d0806$pid.000_batteryfanfeedback, d0806$created_on, d0806$pid.000_stateofchargebms,
            xlab = "created on", ylab = "batteryfanfeedback", rylab = "soc(bms)",lcol=4)

# 이중축 그래프 날짜 - speed- soc(bms) 그래프
twoord.plot(d0806$created_on, d0806$pid.004_calcaveragebatterymoduletemperature, d0806$created_on, d0806$pid.000_stateofchargebms,
            xlab = "created on", ylab = "calcaveragebatterymoduletemperature", rylab = "soc(bms)",lcol=4)

# 이중축 그래프 날짜 - batteryavgtemp - batterymintemperature 그래프
twoord.plot(d0806$created_on, d0806$pid.004_calcaveragebatterymoduletemperature, d0806$created_on, d0806$pid.000_batterymintemperature,
            xlab = "created on", ylab = "calcaveragebatterymoduletemperature", rylab = "batterymintemperature",lcol=4)

# 이중축 그래프 날짜 - batteryavgtemp - batterypower 그래프
twoord.plot(d0806$created_on, d0806$pid.004_calcaveragebatterymoduletemperature, d0806$created_on, d0806$pid.000_batterypower,
            xlab = "created on", ylab = "calcaveragebatterymoduletemperature", rylab = "pid.000_batterypower",lcol=4)

# 이중축 그래프 날짜 - batterydcvoltage - ambient 그래프
twoord.plot(d0806$created_on, d0806$pid.000_batterydcvoltage, d0806$created_on, d0806$pid.003_vmcuambienttemp,
            xlab = "created on", ylab = "batterydcvoltage", rylab = "vmcuambienttemp",lcol=4)

# 이중축 그래프 날짜 - batteryavgtemp - speed_ev_kmh 그래프
twoord.plot(d0806$created_on, d0806$pid.004_calcaveragebatterymoduletemperature, d0806$created_on, d0806$speed_ev_kmh,
            xlab = "created on", ylab = "calcaveragebatterymoduletemperature", rylab = "speed_ev_kmh",lcol=4)

# 이중축 그래프 날짜 - operating - batterydcvoltage 그래프
twoord.plot(d0806$created_on, d0806$pid.000_operatingtime, d0806$created_on, d0806$pid.000_batterydcvoltage,
            xlab = "created on", ylab = "operatingtime", rylab = "batterydcvoltage",lcol=4)

# 이중축 그래프 날짜 - dcvoltage - maxcellvoltage 그래프
twoord.plot(d0806$created_on, d0806$pid.000_batterydcvoltage, d0806$created_on, d0806$pid.000_maximumcellvoltage,
            xlab = "created on", ylab = "batterydcvoltage", rylab = "maximumcellvoltage",lcol=4)

# 이중축 그래프 날짜 - dcvoltage - mincellvoltage 그래프
twoord.plot(d0806$created_on, d0806$pid.000_batterydcvoltage, d0806$created_on, d0806$pid.000_minimumcellvoltage,
            xlab = "created on", ylab = "batterydcvoltage", rylab = "minimumcellvoltage",lcol=4)


# 이중축 그래프 날짜 - dcvoltage - auxillarybatteryvoltage 그래프
twoord.plot(d0806$created_on, d0806$pid.000_batterydcvoltage, d0806$created_on, d0806$pid.000_auxillarybatteryvoltage,
            xlab = "created on", ylab = "batterydcvoltage", rylab = "auxillarybatteryvoltage",lcol=4)


# 이중축 그래프 날짜 - batteryfanfeedback - auxillarybatteryvoltage 그래프
twoord.plot(d0806$created_on, d0806$pid.000_batteryfanfeedback, d0806$created_on, d0806$pid.000_auxillarybatteryvoltage,
            xlab = "created on", ylab = "batteryfanfeedback", rylab = "auxillarybatteryvoltage",lcol=4)


# 이중축 그래프 날짜 - ambienttemp - auxillarybatteryvoltage 그래프
twoord.plot(d0806$created_on, d0806$pid.003_vmcuambienttemp, d0806$created_on, d0806$pid.000_auxillarybatteryvoltage,
            xlab = "created on", ylab = "vmcuambienttemp", rylab = "auxillarybatteryvoltage",lcol=4)


# 이중축 그래프 날짜 - cumulativeenergydischarged - auxillarybatteryvoltage 그래프
twoord.plot(d0806$created_on, d0806$pid.000_cumulativeenergydischarged, d0806$created_on, d0806$pid.000_auxillarybatteryvoltage,
            xlab = "created on", ylab = "cumulativeenergydischarged", rylab = "auxillarybatteryvoltage",lcol=4)


# 이중축 그래프 날짜 - batterymaxtemperature - batterymintemperatur 그래프
twoord.plot(d0806$created_on, d0806$pid.000_batterymaxtemperature, d0806$created_on, d0806$pid.000_batterymintemperature,
            xlab = "created on", ylab = "batterymaxtemperature", rylab = "batterymintemperatur",lcol=4)


# 이중축 그래프 날짜 - availablechargepower - availabledischargepower 그래프
twoord.plot(d0806$created_on, d0806$pid.000_availablechargepower, d0806$created_on, d0806$pid.000_availabledischargepower,
            xlab = "created on", ylab = "batterymaxtemperature", rylab = "batterymintemperatur",lcol=4)


# 이중축 그래프 날짜 - vmcubrakerelated - batterycurrent 그래프
twoord.plot(d0806$created_on, d0806$pid.003_vmcubrakerelated, d0806$created_on, d0806$pid.000_batterycurrent,
            xlab = "created on", ylab = "vmcubrakerelated", rylab = "batterycurrent",lcol=4)



# 이중축 그래프 날짜 - vmcubrakerelated - batterypower 그래프
twoord.plot(d0806$created_on, d0806$pid.003_vmcubrakerelated, d0806$created_on, d0806$pid.000_batterypower,
            xlab = "created on", ylab = "vmcubrakerelated", rylab = "batterypower",lcol=4)


# 이중축 그래프 날짜 - vmcubrakerelated - batterycurrent 그래프
twoord.plot(d0806$created_on, d0806$pid.003_vmcuaccelpedaldepth, d0806$created_on, d0806$pid.000_batterycurrent,
            xlab = "created on", ylab = "vmcuaccelpedaldepth", rylab = "batterycurrent",lcol=4)


# 이중축 그래프 날짜 - vmcubrakerelated - batterypower 그래프
twoord.plot(d0806$created_on, d0806$pid.003_vmcuaccelpedaldepth, d0806$created_on, d0806$pid.000_batterypower,
            xlab = "created on", ylab = "vmcuaccelpedaldepth", rylab = "batterypower",lcol=4)


# 이중축 그래프 날짜 - drivemotorspeed1 - drivemotorspeed2 그래프
twoord.plot(d0806$created_on, d0806$pid.000_drivemotorspeed1, d0806$created_on, d0806$pid.000_drivemotorspeed2,
            xlab = "created on", ylab = "drivemotorspeed1", rylab = "drivemotorspeed2",lcol=4)

# 이중축 그래프 날짜 - drivemotorspeed1 - vmcumotoractualspeedrpm 그래프
twoord.plot(d0806$created_on, d0806$pid.000_drivemotorspeed1, d0806$created_on, d0806$pid.003_vmcumotoractualspeedrpm,
            xlab = "created on", ylab = "drivemotorspeed1", rylab = "vmcumotoractualspeedrpm",lcol=4)



# 이중축 그래프 [1-1] 날짜 - speed_ev_kmh - pid.000_batterypower 그래프
twoord.plot(d0806_table_1_1$created_on, d0806_table_1_1$speed_ev_kmh, d0806_table_1_1$created_on, d0806_table_1_1$pid.000_batterypower,
            xlab = "created on", ylab = "speed_ev_kmh", rylab = "pid.000_batterypower",lcol=4)



# 이중축 그래프 [1-2] 날짜 - speed_ev_kmh - pid.000_batterypower 그래프
twoord.plot(d0806_table_1_2$created_on, d0806_table_1_2$speed_ev_kmh, d0806_table_1_2$created_on, d0806_table_1_2$pid.000_batterypower,
            xlab = "created on", ylab = "speed_ev_kmh", rylab = "pid.000_batterypower",lcol=4)




# 이중축 그래프 [1-3] 날짜 - speed_ev_kmh - pid.000_batterypower 그래프
twoord.plot(d0806_table_1_3$created_on, d0806_table_1_3$speed_ev_kmh, d0806_table_1_3$created_on, d0806_table_1_3$pid.000_batterypower,
            xlab = "created on", ylab = "speed_ev_kmh", rylab = "pid.000_batterypower",lcol=4)



# 이중축 그래프 [1-4] 날짜 - speed_ev_kmh - pid.000_batterypower 그래프
twoord.plot(d0806_table_1_4$created_on, d0806_table_1_4$speed_ev_kmh, d0806_table_1_4$created_on, d0806_table_1_4$pid.000_batterypower,
            xlab = "created on", ylab = "speed_ev_kmh", rylab = "pid.000_batterypower",lcol=4)




# 이중축 그래프 [1-5] 날짜 - speed_ev_kmh - pid.000_batterypower 그래프
twoord.plot(d0806_table_1_5$created_on, d0806_table_1_5$speed_ev_kmh, d0806_table_1_5$created_on, d0806_table_1_5$pid.000_batterypower,
            xlab = "created on", ylab = "speed_ev_kmh", rylab = "pid.000_batterypower",lcol=4)



# 이중축 그래프 [1-6] 날짜 - speed_ev_kmh - pid.000_batterypower 그래프
twoord.plot(d0806_table_1_6$created_on, d0806_table_1_6$speed_ev_kmh, d0806_table_1_6$created_on, d0806_table_1_6$pid.000_batterypower,
            xlab = "created on", ylab = "speed_ev_kmh", rylab = "pid.000_batterypower",lcol=4)




# 이중축 그래프 [1-7] 날짜 - speed_ev_kmh - pid.000_batterypower 그래프
twoord.plot(d0806_table_1_7$created_on, d0806_table_1_7$speed_ev_kmh, d0806_table_1_7$created_on, d0806_table_1_7$pid.000_batterypower,
            xlab = "created on", ylab = "speed_ev_kmh", rylab = "pid.000_batterypower",lcol=4)




# 이중축 그래프 [4-1] 날짜 - speed_ev_kmh - pid.000_batterypower 그래프
twoord.plot(d0806_table_4_1$created_on, d0806_table_4_1$speed_ev_kmh, d0806_table_4_1$created_on, d0806_table_4_1$pid.000_batterypower,
            xlab = "created on", ylab = "speed_ev_kmh", rylab = "pid.000_batterypower",lcol=4)




# 이중축 그래프 [4-2] 날짜 - speed_ev_kmh - pid.000_batterypower 그래프
twoord.plot(d0806_table_4_2$created_on, d0806_table_4_2$speed_ev_kmh, d0806_table_4_2$created_on, d0806_table_4_2$pid.000_batterypower,
            xlab = "created on", ylab = "speed_ev_kmh", rylab = "pid.000_batterypower",lcol=4)

# 0,1 등의 값을 갖는 데이터를 숫자 타입으로 변환
d0806$pid.003_vmcud <- as.factor(d0806$pid.003_vmcud)
d0806$pid.003_vmcun <- as.factor(d0806$pid.003_vmcun)
d0806$pid.003_vmcup <- as.factor(d0806$pid.003_vmcup)
d0806$pid.003_vmcur <- as.factor(d0806$pid.003_vmcur)
d0806$pid.003_vmcubrakeson <- as.factor(d0806$pid.003_vmcubrakeson)
d0806$pid.003_vmcubrakelamp <- as.factor(d0806$pid.003_vmcubrakelamp)
d0806$pid.000_airbagh.wireduty <- as.factor(d0806$pid.000_airbagh.wireduty)
d0806$pid.000_batteryfanstatus <- as.factor(d0806$pid.000_batteryfanstatus)
d0806$pid.003_vmcubrakerelated <- as.factor(d0806$pid.003_vmcubrakerelated)
d0806$pid.000_maximumcellvoltageno. <- as.factor(d0806$pid.000_maximumcellvoltageno.)
d0806$pid.000_minimumcellvoltageno. <- as.factor(d0806$pid.000_minimumcellvoltageno.)
d0806$pid.003_vmcuaccelpedalrelated <- as.factor(d0806$pid.003_vmcuaccelpedalrelated)


# factor 데이터를 character 데이터로 변환
d0806$pid.003_vmcubrakerelated <- as.character(d0806$pid.003_vmcubrakerelated)
d0806_table_1_6$pid.003_vmcubrakeson <- as.character(d0806_table_1_6$pid.003_vmcubrakeson)
d0806_table_1_4$pid.003_vmcubrakeson <- as.character(d0806_table_1_4$pid.003_vmcubrakeson)
d0806_table_1_7$pid.003_vmcubrakeson <- as.character(d0806_table_1_7$pid.003_vmcubrakeson)



# character 데이터를 numeric 데이터로 변환
d0806$pid.003_vmcubrakerelated <- as.numeric(d0806$pid.003_vmcubrakerelated)
d0806_table_1_6$pid.003_vmcubrakeson <- as.numeric(d0806_table_1_6$pid.003_vmcubrakeson)
d0806_table_1_4$pid.003_vmcubrakeson <- as.numeric(d0806_table_1_4$pid.003_vmcubrakeson)
d0806_table_1_7$pid.003_vmcubrakeson <- as.numeric(d0806_table_1_7$pid.003_vmcubrakeson)


# 회귀분석

# "pid.000_batterypower",
#,
#"speed_ev_kmh",
#"pid.004_calcaveragebatterymoduletemperature"


data_0806_tmp2 <- d0806_table_4_2[,c("pid.000_batterypower","pid.000_stateofchargebms")]
#data_0806_tmp2 <- data_0806_tmp2[data_0806_tmp2$pid.003_vmcuambienttemp > 0,]
##data_0806_tmp2$created_on = as.numeric(data_0806_tmp2$created_on)/(24*60*60)
data_0806_tmp2 <- na.omit(data_0806_tmp2)
par(mfrow=c(1,1))
plot(data_0806_tmp2)
m2 <-lm(pid.000_batterypower~cumulativeenergydischarged_inter,data = data_0806_tmp2)
summary(m2)
par(mfrow=c(2,2))
plot(m2)
abline(m2,col="blue")

#write.csv(d0806,"C:/Temp/data_0806.csv")






# split 기준으로 구간 나누기 
# 0806 데이터 측정 시간 단위가 2~4초 간격인 데이터만 추출
d0806_table$rownum <- seq.int(nrow(d0806_table)) # 행 번호를 칼럼으로 추가
d0806_table_tmp <- d0806_table[d0806_table$created_on >= "2019-08-06 11:06:45",] 






# 급출발 (0~5.0km/h 이하 속도에서, 초당 10km/h 이상 가속 운행한 경우) 
d0806_table_start_1 <- d0806_table_tmp[d0806_table_tmp$speed_inter >= 10,] 
d0806_table_start_2 <- d0806_table_tmp[d0806_table_tmp$rownum %in% (d0806_table_start_1$rownum-1),]
d0806_table_start <- bind_rows(d0806_table_start_1,d0806_table_start_2)
d0806_table_start <- unique(d0806_table_start)
d0806_table_start <- arrange(d0806_table_start, rownum)
d0806_table_start$check <- 0
d0806_table_start[(d0806_table_start$speed_ev_kmh - d0806_table_start$speed_inter)<= 5, "check"] = 1
d0806_table_start[d0806_table_start$speed_inter < 10, "check"] = 0
d0806_table_start <- as.data.table(d0806_table_start)
d0806_table_start<-d0806_table_start[,check2 := check - shift(check,-1L)]
d0806_table_start[d0806_table_start$check2 == -1, "check"] = 1
d0806_table_start <- d0806_table_start[d0806_table_start$check ==1,]
d0806_table_start <- d0806_table_start[-9,]
rm(d0806_table_start_1,d0806_table_start_2)
plot(d0806_table_start$speed_ev_kmh ~ d0806_table_start$created_on)








# 급가속 (6.0km/h 이상 속도에서, 초당 8km/h 이상 가속 운행한 경우) 
d0806_table_incrs_1 <- d0806_table_tmp[d0806_table_tmp$speed_inter >= 8,] 
d0806_table_incrs_2 <- d0806_table_tmp[d0806_table_tmp$rownum %in% (d0806_table_incrs_1$rownum-1),]
d0806_table_incrs <- bind_rows(d0806_table_incrs_1,d0806_table_incrs_2)
d0806_table_incrs <- unique(d0806_table_incrs)
d0806_table_incrs <- arrange(d0806_table_incrs, rownum)
d0806_table_incrs$check <- 0
d0806_table_incrs[(d0806_table_incrs$speed_ev_kmh - d0806_table_incrs$speed_inter)>=6, "check"] = 1
d0806_table_incrs[d0806_table_incrs$speed_inter < 8, "check"] = 0
d0806_table_incrs <- as.data.table(d0806_table_incrs)
d0806_table_incrs<-d0806_table_incrs[,check2 := check - shift(check,-1L)]
d0806_table_incrs[d0806_table_incrs$check2 == -1, "check"] = 1
d0806_table_incrs <- d0806_table_incrs[d0806_table_incrs$check ==1,]
#d0806_table_incrs[,c(1,177)] <- d0806_table_incrs[,c(177,1)]
d0806_table_incrs <- d0806_table_incrs[-46,]
rm(d0806_table_incrs_1,d0806_table_incrs_2)
plot(d0806_table_incrs$speed_ev_kmh ~ d0806_table_incrs$created_on)









# 급감속 (6.0km/h 이상 속도에서, 초당 14km/h 이상 감속 운행한 경우) 
d0806_table_decrs_1 <- d0806_table_tmp[d0806_table_tmp$speed_inter <= -14,] 
d0806_table_decrs_2 <- d0806_table_tmp[d0806_table_tmp$rownum %in% (d0806_table_decrs_1$rownum-1),]
d0806_table_decrs <- bind_rows(d0806_table_decrs_1,d0806_table_decrs_2)
d0806_table_decrs <- unique(d0806_table_decrs)
d0806_table_decrs <- arrange(d0806_table_decrs, rownum)
d0806_table_decrs$check <- 0
d0806_table_decrs[(d0806_table_decrs$speed_ev_kmh)>=6, "check"] = 1
d0806_table_decrs[d0806_table_decrs$speed_inter >= -14, "check"] = 0
d0806_table_decrs <- as.data.table(d0806_table_decrs)
d0806_table_decrs<-d0806_table_decrs[,check2 := check - shift(check,-1L)]
d0806_table_decrs[d0806_table_decrs$check2 == -1, "check"] = 1
d0806_table_decrs <- d0806_table_decrs[d0806_table_decrs$check ==1,]
#d0806_table_decrs[,c(1,177)] <- d0806_table_decrs[,c(177,1)]
d0806_table_decrs <- d0806_table_decrs[-5,]
rm(d0806_table_decrs_1,d0806_table_decrs_2)
plot(d0806_table_decrs$speed_ev_kmh ~ d0806_table_decrs$created_on)










# 급정지 (초당 14km/h 이상 감속하여, 속도가 0~5.0km/h 이하가 된 경우) 
d0806_table_stop_1 <- d0806_table_tmp[d0806_table_tmp$speed_inter <= -14,] 
d0806_table_stop_2 <- d0806_table_tmp[d0806_table_tmp$rownum %in% (d0806_table_stop_1$rownum-1),]
d0806_table_stop <- bind_rows(d0806_table_stop_1,d0806_table_stop_2)
d0806_table_stop <- unique(d0806_table_stop)
d0806_table_stop <- arrange(d0806_table_stop, rownum)
d0806_table_stop$check <- 0
d0806_table_stop[d0806_table_stop$speed_inter<=-14 & d0806_table_stop$speed_ev_kmh<=5 , "check"] = 1
d0806_table_stop <- as.data.table(d0806_table_stop)
d0806_table_stop<-d0806_table_stop[,check2 := check - shift(check,-1L)]
d0806_table_stop[d0806_table_stop$check2 == -1, "check"] = 1
d0806_table_stop <- d0806_table_stop[d0806_table_stop$check ==1,]
d0806_table_stop[,c(1,177)] <- d0806_table_stop[,c(177,1)]
d0806_table_stop <- d0806_table_stop[-5,]
rm(d0806_table_stop_1,d0806_table_stop_2)
plot(d0806_table_stop$speed_ev_kmh ~ d0806_table_stop$created_on)










# 정속 (40 ~110 km/h 사이를 주행하며 가속도 5km/h 이하를 20초 이상 유지) 
d0806_table_fix_1 <- d0806_table_tmp[d0806_table_tmp$speed_ev_kmh <= 110 & d0806_table_tmp$speed_ev_kmh >= 40,] 
d0806_table_stop_2 <- d0806_table_tmp[d0806_table_tmp$rownum %in% (d0806_table_stop_1$rownum-1),]
d0806_table_stop <- bind_rows(d0806_table_stop_1,d0806_table_stop_2)
d0806_table_stop <- unique(d0806_table_stop)
d0806_table_stop <- arrange(d0806_table_stop, rownum)
d0806_table_stop$check <- 0
d0806_table_stop[d0806_table_stop$speed_inter<=-14 & d0806_table_stop$speed_ev_kmh<=5 , "check"] = 1
d0806_table_stop <- as.data.table(d0806_table_stop)
d0806_table_stop<-d0806_table_stop[,check2 := check - shift(check,-1L)]
d0806_table_stop[d0806_table_stop$check2 == -1, "check"] = 1
d0806_table_stop <- d0806_table_stop[d0806_table_stop$check ==1,]
d0806_table_stop[,c(1,177)] <- d0806_table_stop[,c(177,1)]
d0806_table_stop <- d0806_table_stop[-5,]
rm(d0806_table_stop_1,d0806_table_stop_2)
plot(d0806_table_stop$speed_ev_kmh ~ d0806_table_stop$created_on)



#8/6 속도 및 pid.000_cumulativeenergydischarged 데이터 차이 컬럼 추가
d0806_table <- as.data.table(d0806)
d0806_table<-d0806_table[,speed_inter := speed_ev_kmh - shift(speed_ev_kmh,1L)]
d0806_table<-d0806_table[,cumulativeenergydischarged_inter := pid.000_cumulativeenergydischarged - shift(pid.000_cumulativeenergydischarged,1L)]




#8/6 1-1, 1-4, 1-6 구간 SOC 차이 컬럼 추가
d0806_table_1_1<-d0806_table_1_1[,soc_inter := pid.000_stateofchargebms - shift(pid.000_stateofchargebms,1L)]
d0806_table_1_4<-d0806_table_1_4[,soc_inter := pid.000_stateofchargebms - shift(pid.000_stateofchargebms,1L)]
d0806_table_1_6<-d0806_table_1_6[,soc_inter := pid.000_stateofchargebms - shift(pid.000_stateofchargebms,1L)]




# 날짜 -  speed_ev_kmh 차이 그래프
plot(d0806_table$speed_inter ~ d0806_table$created_on)

# 날짜 -  speed_ev_kmh 그래프
plot(d0806_table$speed_ev_kmh ~ d0806_table$created_on)

# 날짜 -  cumulativeenergydischarged 차이 그래프
plot(d0806_table$cumulativeenergydischarged_inter ~ d0806_table$created_on)

# 날짜 -  pid.000_cumulativeenergydischarged 그래프
plot(d0806_table$pid.000_cumulativeenergydischarged ~ d0806_table$created_on)



#  구간1  10:43:12 ~ 10:57:39 (1~106행) 
d0806_table_1 <- d0806_table[1:106,]
plot(d0806_table_1$speed_inter ~ d0806_table_1$created_on)
plot(d0806_table_1$speed_ev_kmh ~ d0806_table_1$created_on)



#  구간1_1 10:43:38 ~ 10:45:44 일정하게 속도 증가
d0806_table_1_1 <- d0806_table_1[5:23,]
plot(d0806_table_1_1$speed_inter ~ d0806_table_1_1$created_on)
plot(d0806_table_1_1$speed_ev_kmh ~ d0806_table_1_1$created_on)


#  구간1-2 10:45:44 ~ 10:46:41 속도 감속
d0806_table_1_2 <- d0806_table_1[23:31,]
plot(d0806_table_1_2$speed_inter ~ d0806_table_1_2$created_on)
plot(d0806_table_1_2$speed_ev_kmh ~ d0806_table_1_2$created_on)



#  구간1-3 10:48:56 ~ 10:49:17 급출발
d0806_table_1_3 <- d0806_table_1[44:47,]
plot(d0806_table_1_3$speed_inter ~ d0806_table_1_3$created_on)
plot(d0806_table_1_3$speed_ev_kmh ~ d0806_table_1_3$created_on)



#  구간1-4 10:49:24 ~ 10:49:45 급정지
d0806_table_1_4 <- d0806_table_1[48:51,]
plot(d0806_table_1_4$speed_inter ~ d0806_table_1_4$created_on)
plot(d0806_table_1_4$speed_ev_kmh ~ d0806_table_1_4$created_on)



#  구간1-5 10:50:40 ~ 10:51:02 급출발
d0806_table_1_5 <- d0806_table_1[59:62,]
plot(d0806_table_1_5$speed_inter ~ d0806_table_1_5$created_on)
plot(d0806_table_1_5$speed_ev_kmh ~ d0806_table_1_5$created_on)




#  구간1-6 10:54:26 ~ 10:54:46 급정지
d0806_table_1_6 <- d0806_table_1[89:92,]
plot(d0806_table_1_6$speed_inter ~ d0806_table_1_6$created_on)
plot(d0806_table_1_6$speed_ev_kmh ~ d0806_table_1_6$created_on)




#  구간1-7 10:49:31 ~ 10:50:40 속도가 0인 구간
d0806_table_1_7 <- d0806_table_1[49:59,]
plot(d0806_table_1_7$speed_inter ~ d0806_table_1_7$created_on)
plot(d0806_table_1_7$speed_ev_kmh ~ d0806_table_1_7$created_on)




#  구간2  11:06:45 ~ 11:20:25 (107 ~ 367행) 
d0806_table_2 <- d0806_table[107:367,]
plot(d0806_table_2$speed_inter ~ d0806_table_2$created_on)
plot(d0806_table_2$speed_ev_kmh ~ d0806_table_2$created_on)




#  구간3  11:20:27 ~ 11:22:26 (368 ~ 406행) 
d0806_table_3 <- d0806_table[368:406,]
plot(d0806_table_3$speed_inter ~ d0806_table_3$created_on)
plot(d0806_table_3$speed_ev_kmh ~ d0806_table_3$created_on)




#  구간3-1  11:21:02 ~ 11:21:58 (12 ~ 30행) 
d0806_table_3_1 <- d0806_table_3[12:30,]
plot(d0806_table_3_1$speed_inter ~ d0806_table_3_1$created_on)
plot(d0806_table_3_1$speed_ev_kmh ~ d0806_table_3_1$created_on)




#  구간4  11:24:10 ~ 11:27:00(407 ~ 601행) 
d0806_table_4 <- d0806_table_4[1:56,]
plot(d0806_table_4$speed_inter ~ d0806_table_4$created_on)
plot(d0806_table_4$speed_ev_kmh ~ d0806_table_4$created_on)





#  구간4-1  11:24:10 ~ 11:24:25(1 ~ 6행) 
d0806_table_4_1 <- d0806_table_4[1:6,]
plot(d0806_table_4_1$speed_inter ~ d0806_table_4_1$created_on)
plot(d0806_table_4_1$speed_ev_kmh ~ d0806_table_4_1$created_on)




#  구간4-2  11:24:38 ~ 11:24:56(10 ~ 16행) 
d0806_table_4_2 <- d0806_table_4[10:16,]
plot(d0806_table_4_2$speed_inter ~ d0806_table_4_2$created_on)
plot(d0806_table_4_2$speed_ev_kmh ~ d0806_table_4_2$created_on)



#  구간5 11:27:00 ~ 11:29:39 (56 ~ 107행) 
d0806_table_5 <- d0806_table_4[56:107,]
plot(d0806_table_5$speed_inter ~ d0806_table_5$created_on)
plot(d0806_table_5$speed_ev_kmh ~ d0806_table_5$created_on)




#  구간6 11:29:39 ~ 11:34:29 (107 ~ 195행) 
d0806_table_6 <- d0806_table_4[107:195,]
plot(d0806_table_6$speed_inter ~ d0806_table_6$created_on)
plot(d0806_table_6$speed_ev_kmh ~ d0806_table_6$created_on)




# 9월 19일 토크프로 데이터 
d0919_1 <- read.csv("C:\\Users\\VEStellaLab\\Desktop\\ev\\data\\trackLog_2019_9_19_14_03_44.csv", header = T)
d0919_3 <- read.csv("C:\\Users\\VEStellaLab\\Desktop\\ev\\data\\trackLog_2019_9_19_14_23_31.csv", header = T)

d0919_1 <- d0919_1[-14,] # column 이름이 데이터 중간에 삽입되어 있어서 제거 
d0919_3 <- d0919_3[-23,] # column 이름이 데이터 중간에 삽입되어 있어서 제거


#factor 타입 데이터를 character로 변환 후 numeric으로 변환
d0919_1$X._ABRP_Battery.Current.A. <- as.character(d0919_1$X._ABRP_Battery.Current.A.)
d0919_1$X._ABRP_Battery.Power.kW. <- as.character(d0919_1$X._ABRP_Battery.Power.kW.)
d0919_1$X._ABRP_Cumulative.Energy.Discharged.kWh. <- as.character(d0919_1$X._ABRP_Cumulative.Energy.Discharged.kWh.)
d0919_1$X._ABRP_State.of.Charge.BMS... <- as.character(d0919_1$X._ABRP_State.of.Charge.BMS...)
d0919_1$X._ABRP_Battery.DC.Voltage.V. <- as.character(d0919_1$X._ABRP_Battery.DC.Voltage.V.)
d0919_1$X._ABRP_Cumulative.Energy.Charged.kWh. <- as.character(d0919_1$X._ABRP_Cumulative.Energy.Charged.kWh.)
d0919_1$X._ABRP_State.of.Charge.Display... <- as.character(d0919_1$X._ABRP_State.of.Charge.Display...)
d0919_1$X._ABRP_State.of.Health... <- as.character(d0919_1$X._ABRP_State.of.Health...)
d0919_1$X._ABRP_VMCU.Real.Vehicle.Speed.km.h. <- as.character(d0919_1$X._ABRP_VMCU.Real.Vehicle.Speed.km.h.)
d0919_1$X000_Auxillary.Battery.Voltage.V. <- as.character(d0919_1$X000_Auxillary.Battery.Voltage.V.)
d0919_1$X000_Available.Charge.Power.kW. <- as.character(d0919_1$X000_Available.Charge.Power.kW.)
d0919_1$X000_Available.Discharge.Power.kW. <- as.character(d0919_1$X000_Available.Discharge.Power.kW.)
d0919_1$X000_Battery.Current.A. <- as.character(d0919_1$X000_Battery.Current.A.)
d0919_1$X000_Battery.DC.Voltage.V. <- as.character(d0919_1$X000_Battery.DC.Voltage.V.)
d0919_1$X000_Battery.Fan.Feedback.Hz. <- as.character(d0919_1$X000_Battery.Fan.Feedback.Hz.)
d0919_1$X000_Battery.Max.Temperature.째C. <- as.character(d0919_1$X000_Battery.Max.Temperature.째C.)
d0919_1$X000_Battery.Power.kW. <- as.character(d0919_1$X000_Battery.Power.kW.)
d0919_1$X000_Cumulative.Charge.Current.Ah. <- as.character(d0919_1$X000_Cumulative.Charge.Current.Ah.)
d0919_1$X000_Cumulative.Discharge.Current.Ah. <- as.character(d0919_1$X000_Cumulative.Discharge.Current.Ah.)
d0919_1$X000_Cumulative.Energy.Charged.kWh. <- as.character(d0919_1$X000_Cumulative.Energy.Charged.kWh.)
d0919_1$X000_Cumulative.Energy.Discharged.kWh. <- as.character(d0919_1$X000_Cumulative.Energy.Discharged.kWh.)
d0919_1$X000_Drive.Motor.Speed.1.rpm. <- as.character(d0919_1$X000_Drive.Motor.Speed.1.rpm.)
d0919_1$X000_Drive.Motor.Speed.2.rpm. <- as.character(d0919_1$X000_Drive.Motor.Speed.2.rpm.)
d0919_1$X000_Inverter.Capacitor.Voltage.V. <- as.character(d0919_1$X000_Inverter.Capacitor.Voltage.V.)
d0919_1$X003_VMCU.Real.Vehicle.Speed.km.h. <- as.character(d0919_1$X003_VMCU.Real.Vehicle.Speed.km.h. )
d0919_1$X000_State.of.Charge.BMS... <- as.character(d0919_1$X000_State.of.Charge.BMS... ) 
d0919_1$X000_State.of.Charge.Display... <- as.character(d0919_1$X000_State.of.Charge.Display... )
d0919_1$X003_VMCU.Motor.Actual.Speed.RPM.rpm. <- as.character(d0919_1$X003_VMCU.Motor.Actual.Speed.RPM.rpm. )
d0919_1$X004_CALC.Average.Battery.Module.Temperature.째C. <- as.character(d0919_1$X004_CALC.Average.Battery.Module.Temperature.째C. )
d0919_1$X004_CALC.Average.Cell.Voltage.V.  <- as.character(d0919_1$X004_CALC.Average.Cell.Voltage.V. )


d0919_1$X._ABRP_Battery.Current.A. <- as.numeric(d0919_1$X._ABRP_Battery.Current.A.)
d0919_1$X._ABRP_Battery.Power.kW. <- as.numeric(d0919_1$X._ABRP_Battery.Power.kW.)
d0919_1$X._ABRP_Cumulative.Energy.Discharged.kWh. <- as.numeric(d0919_1$X._ABRP_Cumulative.Energy.Discharged.kWh.)
d0919_1$X._ABRP_State.of.Charge.BMS... <- as.numeric(d0919_1$X._ABRP_State.of.Charge.BMS...)
d0919_1$X._ABRP_Battery.DC.Voltage.V. <- as.numeric(d0919_1$X._ABRP_Battery.DC.Voltage.V.)
d0919_1$X._ABRP_Cumulative.Energy.Charged.kWh. <- as.numeric(d0919_1$X._ABRP_Cumulative.Energy.Charged.kWh.)
d0919_1$X._ABRP_State.of.Charge.Display... <- as.numeric(d0919_1$X._ABRP_State.of.Charge.Display...)
d0919_1$X._ABRP_State.of.Health... <- as.numeric(d0919_1$X._ABRP_State.of.Health...)
d0919_1$X._ABRP_VMCU.Real.Vehicle.Speed.km.h. <- as.numeric(d0919_1$X._ABRP_VMCU.Real.Vehicle.Speed.km.h.)
d0919_1$X000_Auxillary.Battery.Voltage.V. <- as.numeric(d0919_1$X000_Auxillary.Battery.Voltage.V.)
d0919_1$X000_Available.Charge.Power.kW. <- as.numeric(d0919_1$X000_Available.Charge.Power.kW.)
d0919_1$X000_Available.Discharge.Power.kW. <- as.numeric(d0919_1$X000_Available.Discharge.Power.kW.)
d0919_1$X000_Battery.Current.A. <- as.numeric(d0919_1$X000_Battery.Current.A.)
d0919_1$X000_Battery.DC.Voltage.V. <- as.numeric(d0919_1$X000_Battery.DC.Voltage.V.)
d0919_1$X000_Battery.Fan.Feedback.Hz. <- as.numeric(d0919_1$X000_Battery.Fan.Feedback.Hz.)
d0919_1$X000_Battery.Max.Temperature.째C. <- as.numeric(d0919_1$X000_Battery.Max.Temperature.째C.)
d0919_1$X000_Battery.Power.kW. <- as.numeric(d0919_1$X000_Battery.Power.kW.)
d0919_1$X000_Cumulative.Charge.Current.Ah. <- as.numeric(d0919_1$X000_Cumulative.Charge.Current.Ah.)
d0919_1$X000_Cumulative.Discharge.Current.Ah. <- as.numeric(d0919_1$X000_Cumulative.Discharge.Current.Ah.)
d0919_1$X000_Cumulative.Energy.Charged.kWh. <- as.numeric(d0919_1$X000_Cumulative.Energy.Charged.kWh.)
d0919_1$X000_Cumulative.Energy.Discharged.kWh. <- as.numeric(d0919_1$X000_Cumulative.Energy.Discharged.kWh.)
d0919_1$X000_Drive.Motor.Speed.1.rpm. <- as.numeric(d0919_1$X000_Drive.Motor.Speed.1.rpm.)
d0919_1$X000_Drive.Motor.Speed.2.rpm. <- as.numeric(d0919_1$X000_Drive.Motor.Speed.2.rpm.)
d0919_1$X000_Inverter.Capacitor.Voltage.V. <- as.numeric(d0919_1$X000_Inverter.Capacitor.Voltage.V.)
d0919_1$X003_VMCU.Real.Vehicle.Speed.km.h. <- as.numeric(d0919_1$X003_VMCU.Real.Vehicle.Speed.km.h. )
d0919_1$X000_State.of.Charge.BMS... <- as.numeric(d0919_1$X000_State.of.Charge.BMS... ) 
d0919_1$X000_State.of.Charge.Display... <- as.numeric(d0919_1$X000_State.of.Charge.Display... )
d0919_1$X003_VMCU.Motor.Actual.Speed.RPM.rpm. <- as.numeric(d0919_1$X003_VMCU.Motor.Actual.Speed.RPM.rpm. )
d0919_1$X004_CALC.Average.Battery.Module.Temperature.째C. <- as.numeric(d0919_1$X004_CALC.Average.Battery.Module.Temperature.째C. )
d0919_1$X004_CALC.Average.Cell.Voltage.V.  <- as.numeric(d0919_1$X004_CALC.Average.Cell.Voltage.V. )





#factor 타입 데이터를 character로 변환 후 numeric으로 변환
d0919_3$X._ABRP_Battery.Current.A. <- as.character(d0919_3$X._ABRP_Battery.Current.A.)
d0919_3$X._ABRP_Battery.Power.kW. <- as.character(d0919_3$X._ABRP_Battery.Power.kW.)
d0919_3$X._ABRP_Cumulative.Energy.Discharged.kWh. <- as.character(d0919_3$X._ABRP_Cumulative.Energy.Discharged.kWh.)
d0919_3$X._ABRP_State.of.Charge.BMS... <- as.character(d0919_3$X._ABRP_State.of.Charge.BMS...)
d0919_3$X._ABRP_Battery.DC.Voltage.V. <- as.character(d0919_3$X._ABRP_Battery.DC.Voltage.V.)
d0919_3$X._ABRP_Cumulative.Energy.Charged.kWh. <- as.character(d0919_3$X._ABRP_Cumulative.Energy.Charged.kWh.)
d0919_3$X._ABRP_State.of.Charge.Display... <- as.character(d0919_3$X._ABRP_State.of.Charge.Display...)
d0919_3$X._ABRP_State.of.Health... <- as.character(d0919_3$X._ABRP_State.of.Health...)
d0919_3$X._ABRP_VMCU.Real.Vehicle.Speed.km.h. <- as.character(d0919_3$X._ABRP_VMCU.Real.Vehicle.Speed.km.h.)
d0919_3$X000_Auxillary.Battery.Voltage.V. <- as.character(d0919_3$X000_Auxillary.Battery.Voltage.V.)
d0919_3$X000_Available.Charge.Power.kW. <- as.character(d0919_3$X000_Available.Charge.Power.kW.)
d0919_3$X000_Available.Discharge.Power.kW. <- as.character(d0919_3$X000_Available.Discharge.Power.kW.)
d0919_3$X000_Battery.Current.A. <- as.character(d0919_3$X000_Battery.Current.A.)
d0919_3$X000_Battery.DC.Voltage.V. <- as.character(d0919_3$X000_Battery.DC.Voltage.V.)
d0919_3$X000_Battery.Fan.Feedback.Hz. <- as.character(d0919_3$X000_Battery.Fan.Feedback.Hz.)
d0919_3$X000_Battery.Max.Temperature.째C. <- as.character(d0919_3$X000_Battery.Max.Temperature.째C.)
d0919_3$X000_Battery.Power.kW. <- as.character(d0919_3$X000_Battery.Power.kW.)
d0919_3$X000_Cumulative.Charge.Current.Ah. <- as.character(d0919_3$X000_Cumulative.Charge.Current.Ah.)
d0919_3$X000_Cumulative.Discharge.Current.Ah. <- as.character(d0919_3$X000_Cumulative.Discharge.Current.Ah.)
d0919_3$X000_Cumulative.Energy.Charged.kWh. <- as.character(d0919_3$X000_Cumulative.Energy.Charged.kWh.)
d0919_3$X000_Cumulative.Energy.Discharged.kWh. <- as.character(d0919_3$X000_Cumulative.Energy.Discharged.kWh.)
d0919_3$X000_Drive.Motor.Speed.1.rpm. <- as.character(d0919_3$X000_Drive.Motor.Speed.1.rpm.)
d0919_3$X000_Drive.Motor.Speed.2.rpm. <- as.character(d0919_3$X000_Drive.Motor.Speed.2.rpm.)
d0919_3$X000_Inverter.Capacitor.Voltage.V. <- as.character(d0919_3$X000_Inverter.Capacitor.Voltage.V.)
d0919_3$X003_VMCU.Real.Vehicle.Speed.km.h. <- as.character(d0919_3$X003_VMCU.Real.Vehicle.Speed.km.h. )
d0919_3$X000_State.of.Charge.BMS... <- as.character(d0919_3$X000_State.of.Charge.BMS... ) 
d0919_3$X000_State.of.Charge.Display... <- as.character(d0919_3$X000_State.of.Charge.Display... )
d0919_3$X003_VMCU.Motor.Actual.Speed.RPM.rpm. <- as.character(d0919_3$X003_VMCU.Motor.Actual.Speed.RPM.rpm. )
d0919_3$X004_CALC.Average.Battery.Module.Temperature.째C. <- as.character(d0919_3$X004_CALC.Average.Battery.Module.Temperature.째C. )
d0919_3$X004_CALC.Average.Cell.Voltage.V.  <- as.character(d0919_3$X004_CALC.Average.Cell.Voltage.V. )


d0919_3$X._ABRP_Battery.Current.A. <- as.numeric(d0919_3$X._ABRP_Battery.Current.A.)
d0919_3$X._ABRP_Battery.Power.kW. <- as.numeric(d0919_3$X._ABRP_Battery.Power.kW.)
d0919_3$X._ABRP_Cumulative.Energy.Discharged.kWh. <- as.numeric(d0919_3$X._ABRP_Cumulative.Energy.Discharged.kWh.)
d0919_3$X._ABRP_State.of.Charge.BMS... <- as.numeric(d0919_3$X._ABRP_State.of.Charge.BMS...)
d0919_3$X._ABRP_Battery.DC.Voltage.V. <- as.numeric(d0919_3$X._ABRP_Battery.DC.Voltage.V.)
d0919_3$X._ABRP_Cumulative.Energy.Charged.kWh. <- as.numeric(d0919_3$X._ABRP_Cumulative.Energy.Charged.kWh.)
d0919_3$X._ABRP_State.of.Charge.Display... <- as.numeric(d0919_3$X._ABRP_State.of.Charge.Display...)
d0919_3$X._ABRP_State.of.Health... <- as.numeric(d0919_3$X._ABRP_State.of.Health...)
d0919_3$X._ABRP_VMCU.Real.Vehicle.Speed.km.h. <- as.numeric(d0919_3$X._ABRP_VMCU.Real.Vehicle.Speed.km.h.)
d0919_3$X000_Auxillary.Battery.Voltage.V. <- as.numeric(d0919_3$X000_Auxillary.Battery.Voltage.V.)
d0919_3$X000_Available.Charge.Power.kW. <- as.numeric(d0919_3$X000_Available.Charge.Power.kW.)
d0919_3$X000_Available.Discharge.Power.kW. <- as.numeric(d0919_3$X000_Available.Discharge.Power.kW.)
d0919_3$X000_Battery.Current.A. <- as.numeric(d0919_3$X000_Battery.Current.A.)
d0919_3$X000_Battery.DC.Voltage.V. <- as.numeric(d0919_3$X000_Battery.DC.Voltage.V.)
d0919_3$X000_Battery.Fan.Feedback.Hz. <- as.numeric(d0919_3$X000_Battery.Fan.Feedback.Hz.)
d0919_3$X000_Battery.Max.Temperature.째C. <- as.numeric(d0919_3$X000_Battery.Max.Temperature.째C.)
d0919_3$X000_Battery.Power.kW. <- as.numeric(d0919_3$X000_Battery.Power.kW.)
d0919_3$X000_Cumulative.Charge.Current.Ah. <- as.numeric(d0919_3$X000_Cumulative.Charge.Current.Ah.)
d0919_3$X000_Cumulative.Discharge.Current.Ah. <- as.numeric(d0919_3$X000_Cumulative.Discharge.Current.Ah.)
d0919_3$X000_Cumulative.Energy.Charged.kWh. <- as.numeric(d0919_3$X000_Cumulative.Energy.Charged.kWh.)
d0919_3$X000_Cumulative.Energy.Discharged.kWh. <- as.numeric(d0919_3$X000_Cumulative.Energy.Discharged.kWh.)
d0919_3$X000_Drive.Motor.Speed.1.rpm. <- as.numeric(d0919_3$X000_Drive.Motor.Speed.1.rpm.)
d0919_3$X000_Drive.Motor.Speed.2.rpm. <- as.numeric(d0919_3$X000_Drive.Motor.Speed.2.rpm.)
d0919_3$X000_Inverter.Capacitor.Voltage.V. <- as.numeric(d0919_3$X000_Inverter.Capacitor.Voltage.V.)
d0919_3$X003_VMCU.Real.Vehicle.Speed.km.h. <- as.numeric(d0919_3$X003_VMCU.Real.Vehicle.Speed.km.h. )
d0919_3$X000_State.of.Charge.BMS... <- as.numeric(d0919_3$X000_State.of.Charge.BMS... ) 
d0919_3$X000_State.of.Charge.Display... <- as.numeric(d0919_3$X000_State.of.Charge.Display... )
d0919_3$X003_VMCU.Motor.Actual.Speed.RPM.rpm. <- as.numeric(d0919_3$X003_VMCU.Motor.Actual.Speed.RPM.rpm. )
d0919_3$X004_CALC.Average.Battery.Module.Temperature.째C. <- as.numeric(d0919_3$X004_CALC.Average.Battery.Module.Temperature.째C. )
d0919_3$X004_CALC.Average.Cell.Voltage.V.  <- as.numeric(d0919_3$X004_CALC.Average.Cell.Voltage.V. )




# 날짜 데이터 형식으로 변환
# 이상문자 제거
d0919_1$Device.Time <- gsub("쌩","",d0919_1$Device.Time)
d0919_1$Device.Time <- gsub("攼㹣","",d0919_1$Device.Time)
d0919_3$Device.Time <- gsub("썡","",d0919_3$Device.Time)
d0919_3$Device.Time <- gsub("攼㹣","",d0919_3$Device.Time)

# 날짜 형식으로 변환
d0919_1$Device.Time <- as.POSIXct(d0919_1$Device.Time,format="%d-%m-%Y %H:%M:%S")
d0919_3$Device.Time <- as.POSIXct(d0919_3$Device.Time,format="%d-%m-%Y %H:%M:%S")


# 2개 테이블 합침
d0919 <- bind_rows(d0919_1,d0919_3)
rm(d0919_1,d0919_3)

#날짜 타입 데이터를 숫자 데이터로 변환
#d0919_1_tmp <- d0919_1
#d0919_1_tmp$Device.Time = as.numeric(d0919_1_tmp$Device.Time)/(24*60*60)



# 0919 data 플롯 그리기
par(mfrow=c(1,1))

# 시간 - X._ABRP_Battery.Current.A. 그래프
plot(d0919$X._ABRP_Battery.Current.A. ~ d0919$Device.Time)

# 시간 - X000_Battery.Current.A. 그래프
plot(d0919$X000_Battery.Current.A. ~ d0919$Device.Time)

# 시간 - X000_State.of.Charge.BMS... 그래프
plot(d0919$X000_State.of.Charge.BMS... ~ d0919$Device.Time)

# 시간 - X003_VMCU.Real.Vehicle.Speed.km.h. 그래프
plot(d0919$X003_VMCU.Real.Vehicle.Speed.km.h. ~ d0919$Device.Time)

# 시간 - X000_Battery.Power.kW. 그래프
plot(d0919$X000_Battery.Power.kW. ~ d0919$Device.Time)






# Mongo DB Connetion

con <- mongo(collection = "ev_obd", url = "mongodb://ev:Ev123@49.254.250.186:27017/ev")
df <- con$find(query ='{"created_on":/^2019-11-11/}')
#db.user.find({"name":/^Si/});
d0927_tmp1 <- df$pid
d0927_tmp2 <- subset(df, select = -pid)
d0927 <- bind_cols(d0927_tmp1,d0927_tmp2)
rm(d0927_tmp1,d0927_tmp2)

df_temp <- df[df$created_on == "2019-11-11 14:11:05",]
# 회귀분석
# "pid.000_batterypower"
#"speed_ev_kmh",
#"pid.004_calcaveragebatterymoduletemperature"

data_0927_tmp2 <- df$pid[,c("created_on","000_state of charge bms")]
#data_0806_tmp2 <- data_0806_tmp2[data_0806_tmp2$pid.003_vmcuambienttemp > 0,]
##data_0806_tmp2$created_on = as.numeric(data_0806_tmp2$created_on)/(24*60*60)
data_0806_tmp2 <- na.omit(data_0806_tmp2)
par(mfrow=c(1,1))
plot(data_0806_tmp2)
m2 <-lm(pid.000_batterypower~cumulativeenergydischarged_inter,data = data_0806_tmp2)
summary(m2)
par(mfrow=c(2,2))
plot(m2)
abline(m2,col="blue")