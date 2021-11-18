
#============================================================
#  Title    : Mongodb ev data 분석
#  Subtitle : 191105 Data collection
#  location : 112km 역삼공영주차장 <-> 강원도 횡성 휴게소 
#  Author   : MtoV Jiwon Lee, Dokyung Yoon
#  Date     : 20191105
#  Updates  : 20191105
#============================================================

#install packages and calling library
install.packages("data.table",repos="http://R-Forge.R-project.org")
install.packages("plotrix")
install.packages("mongolite")
install.packages("data.table")
install.packages("dplyr")
install.packages("ggplot2")
library(jsonlite)
library(stats)
library(base)
library(dplyr)
library(stringr)
library(plotrix)
library(data.table)
library(mongolite)
library(ggplot2)


con <- mongo(collection = "ev_obd", url = "mongodb://ev:Ev123@49.254.250.186:27017/ev")

#test_20191107
df_20191107 <- con$find(query ='{"created_on": {"$gt":"2019-11-07 15:07:00", "$lt":"2019-11-07 23:59:59"}}')
d0927_tmp1 <- df_20191107$pid
d0927_tmp2 <- subset(df_20191107, select = -pid)
df_test_20191107 <- bind_cols(d0927_tmp1,d0927_tmp2)
rm(d0927_tmp1,d0927_tmp2)
#write.csv(df_test_20191107, "df_test_20191107.csv")
#head(df_test_20191107, 1)

#test_20191111
df_20191111 <- con$find(query ='{"created_on": {"$gt":"2019-11-11 10:00:00", "$lt":"2019-11-11 23:59:59"}}')
d1111_tmp1 <- df_20191111$pid
d1111_tmp2 <- subset(df_20191111, select = -pid)
df_test_20191111 <- bind_cols(d1111_tmp1,d1111_tmp2)
rm(d1111_tmp1,d1111_tmp2)
#write.csv(df_test_20191111, "df_test_20191111.csv")
#head(df_test_20191111, 1)




#test_20191115
df_20191115 <- con$find(query ='{"created_on": {"$gt":"2019-11-15 10:19:06", "$lt":"2019-11-15 23:59:59"}}')
d1115_tmp1 <- df_20191115$pid
d1115_tmp2 <- subset(df_20191115, select = -pid)
df_test_20191115 <- bind_cols(d1115_tmp1,d1115_tmp2)
rm(d1115_tmp1,d1115_tmp2)
#write.csv(df_test_20191115, "df_test_20191115")
#head(df_test_20191115, 1)




# bind rows
total_test <- bind_rows(df_test_20191107, df_test_20191111)



# 20191107 데이터 속도와 날짜 값 데이터 형 변환
df_test_20191107$ev_speed_kmh <- as.numeric(df_test_20191107$ev_speed_kmh)
df_test_20191107$created_on <- as.POSIXct(df_test_20191107$created_on)

# 20191107 데이터 날짜 -  drive motor speed2 그래프
plot(df_test_20191107$ev_speed_kmh ~ df_test_20191107$created_on)




# 20191111 데이터 속도와 날짜 값 데이터 형 변환
df_test_20191111$ev_speed_kmh <- as.numeric(df_test_20191111$ev_speed_kmh)
df_test_20191111$created_on <- as.POSIXct(df_test_20191111$created_on)

# 20191111 데이터 날짜 -  drive motor speed2 그래프
plot(df_test_20191111$ev_speed_kmh ~ df_test_20191111$created_on)




# 20191115 데이터 속도와 날짜 값 데이터 형 변환
df_test_20191115$ev_speed_kmh <- as.numeric(df_test_20191115$ev_speed_kmh)
df_test_20191115$created_on <- as.POSIXct(df_test_20191115$created_on)

# 20191115 데이터 날짜 -  drive motor speed2 그래프
plot(df_test_20191115$ev_speed_kmh ~ df_test_20191115$created_on)





# 데이터 테이블 타입으로 변환
df_test_20191107<- as.data.table(df_test_20191107)
df_test_20191111<- as.data.table(df_test_20191111)
df_test_20191115<- as.data.table(df_test_20191115)



# 수집되는 시간의 차 구하기
df_test_20191107<-df_test_20191107[,created_on_inter := created_on - shift(created_on,1L)]
df_test_20191111<-df_test_20191111[,created_on_inter := created_on - shift(created_on,1L)]
df_test_20191115<-df_test_20191115[,created_on_inter := created_on - shift(created_on,1L)]



# 11월 7, 11, 15일 데이터 시간 - 시간차 그래프
plot(df_test_20191107$created_on_inter ~ df_test_20191107$created_on)
plot(df_test_20191111$created_on_inter ~ df_test_20191111$created_on)
plot(df_test_20191115$created_on_inter ~ df_test_20191115$created_on)




# 11월 7, 11, 15일 시간 간격이 10초 이상인 데이터 추출
tmp_1107<-df_test_20191107[df_test_20191107$created_on_inter >= 10,]
tmp_1111<-df_test_20191111[df_test_20191111$created_on_inter >= 10,]
tmp_1115<-df_test_20191115[df_test_20191115$created_on_inter >= 10,]






# 11월 7일 시간 간격이 10초 이상인 데이터의 바로 전 데이터 추출
k <- (tmp_1107$created_on - tmp_1107$created_on_inter)

for(i in 1:4){
  ttt <- df_test_20191107[df_test_20191107$created_on == k[i],]
  tmp_1107 <- rbind(ttt, tmp_1107)  
  
}

#정렬
tmp_1107 <- tmp_1107[c(order(tmp_1107$created_on)),]






# 11월 15일 시간 간격이 10초 이상인 데이터의 바로 전 데이터 추출
k <- (tmp_1115$created_on - tmp_1115$created_on_inter)

for(i in 1:2){
  ttt <- df_test_20191115[df_test_20191115$created_on == k[i],]
  tmp_1115 <- rbind(ttt, tmp_1115)  
  
}

#정렬
tmp_1115 <- tmp_1115[c(order(tmp_1115$created_on)),]





# ev_speed 칼럼의 속도값을 현재행과 이전행의 평균을 구한 ev_spd_avg 칼럼 추가
df_test_20191115<-df_test_20191115[,ev_spd_avg := (ev_speed_kmh + shift(ev_speed_kmh,1L))/2]




# ev_spd_avg 칼럼의 초단위 값을 시간단위 값으로 변환
df_test_20191115$created_on_inter <- df_test_20191115$created_on_inter/3600 


# ev_distance
df_test_20191115$ev_distance <- df_test_20191115$created_on_inter * df_test_20191115$ev_spd_avg


# 총 누적 ev_distance 구하기
# 평균으로 구한 누적 거리 70.39907
df_test_20191115[is.na(df_test_20191115$ev_distance),"ev_distance"] = 0
cumul_ev_distance <- sum(df_test_20191115$ev_distance)

# 이전 행으로 구한 누적거리 70.44504
df_test_20191115<-df_test_20191115[,ev_distance_2 := (created_on_inter * shift(ev_speed_kmh,1L))]
df_test_20191115[is.na(df_test_20191115$ev_distance_2),"ev_distance_2"] = 0

# 이후 행으로 구한 누적거리 70.3531
df_test_20191115[is.na(df_test_20191115$created_on_inter),"created_on_inter"] = 0
df_test_20191115$ev_distance_3 <-df_test_20191115$ev_speed_kmh * df_test_20191115$created_on_inter




#=============================================================================================
# 11/15일 데이터 급가속, 급감속, 급정지, 급출발 컬럼 추가

#속도차이 컬럼 추가
df_test_20191115<-df_test_20191115[,ev_spd_inter := ev_speed_kmh - shift(ev_speed_kmh,1L)]

df_test_20191115[is.na(df_test_20191115$ev_spd_inter),"ev_spd_inter"] = 0




# 급출발 (0~5.0km/h 이하 속도에서, 초당 10km/h 이상 가속 운행한 경우)
df_test_20191115$check1 = 0
df_test_20191115[df_test_20191115$ev_spd_inter >= 10,"check1"] = 1 
df_test_20191115[(df_test_20191115$check1 == 1)&((df_test_20191115$ev_speed_kmh - df_test_20191115$ev_spd_inter)>5),"check1"] = 0 



#df_test_20191115[df_test_20191115$check1 == 1,"check1"] = 1 
#df_test_20191115<-df_test_20191115[,check2 := (check1 - shift(check1,1L))]
#d0806_table_start_2 <- d0806_table_tmp[d0806_table_tmp$rownum %in% (d0806_table_start_1$rownum-1),]
#d0806_table_start <- bind_rows(d0806_table_start_1,d0806_table_start_2)
#d0806_table_start <- unique(d0806_table_start)
#d0806_table_start <- arrange(d0806_table_start, rownum)
#d0806_table_start$check <- 0
#d0806_table_start[(d0806_table_start$speed_ev_kmh - d0806_table_start$speed_inter)<= 5, "check"] = 1
#d0806_table_start[d0806_table_start$speed_inter < 10, "check"] = 0
#d0806_table_start <- as.data.table(d0806_table_start)
#d0806_table_start<-d0806_table_start[,check2 := check - shift(check,-1L)]
#d0806_table_start[d0806_table_start$check2 == -1, "check"] = 1
#d0806_table_start <- d0806_table_start[d0806_table_start$check ==1,]
#d0806_table_start <- d0806_table_start[-9,]
#rm(d0806_table_start_1,d0806_table_start_2)
#plot(d0806_table_start$speed_ev_kmh ~ d0806_table_start$created_on)








# 급가속 (6.0km/h 이상 속도에서, 초당 8km/h 이상 가속 운행한 경우) 
df_test_20191115$check2 = 0
df_test_20191115[df_test_20191115$ev_spd_inter >= 8,"check2"] = 1
df_test_20191115[(df_test_20191115$check2 == 1)&((df_test_20191115$ev_speed_kmh - df_test_20191115$ev_spd_inter)<6),"check2"] = 0



#d0806_table_incrs_1 <- d0806_table_tmp[d0806_table_tmp$speed_inter >= 8,] 
#d0806_table_incrs_2 <- d0806_table_tmp[d0806_table_tmp$rownum %in% (d0806_table_incrs_1$rownum-1),]
#d0806_table_incrs <- bind_rows(d0806_table_incrs_1,d0806_table_incrs_2)
#d0806_table_incrs <- unique(d0806_table_incrs)
#d0806_table_incrs <- arrange(d0806_table_incrs, rownum)
#d0806_table_incrs$check <- 0
#d0806_table_incrs[(d0806_table_incrs$speed_ev_kmh - d0806_table_incrs$speed_inter)>=6, "check"] = 1
#d0806_table_incrs[d0806_table_incrs$speed_inter < 8, "check"] = 0
#d0806_table_incrs <- as.data.table(d0806_table_incrs)
#d0806_table_incrs<-d0806_table_incrs[,check2 := check - shift(check,-1L)]
#d0806_table_incrs[d0806_table_incrs$check2 == -1, "check"] = 1
#d0806_table_incrs <- d0806_table_incrs[d0806_table_incrs$check ==1,]
#d0806_table_incrs[,c(1,177)] <- d0806_table_incrs[,c(177,1)]
#d0806_table_incrs <- d0806_table_incrs[-46,]
#rm(d0806_table_incrs_1,d0806_table_incrs_2)
#plot(d0806_table_incrs$speed_ev_kmh ~ d0806_table_incrs$created_on)



 





# 급감속 (6.0km/h 이상 속도에서, 초당 14km/h 이상 감속 운행한 경우) 
df_test_20191115$check3 = 0
df_test_20191115[df_test_20191115$ev_spd_inter <= -14,"check3"] = 1
df_test_20191115[(df_test_20191115$check3 == 1)&(df_test_20191115$ev_speed_kmh<6),"check3"] = 0



#d0806_table_decrs_1 <- d0806_table_tmp[d0806_table_tmp$speed_inter <= -14,] 
#d0806_table_decrs_2 <- d0806_table_tmp[d0806_table_tmp$rownum %in% (d0806_table_decrs_1$rownum-1),]
#d0806_table_decrs <- bind_rows(d0806_table_decrs_1,d0806_table_decrs_2)
#d0806_table_decrs <- unique(d0806_table_decrs)
#d0806_table_decrs <- arrange(d0806_table_decrs, rownum)
#d0806_table_decrs$check <- 0
#d0806_table_decrs[(d0806_table_decrs$speed_ev_kmh)>=6, "check"] = 1
#d0806_table_decrs[d0806_table_decrs$speed_inter >= -14, "check"] = 0
#d0806_table_decrs <- as.data.table(d0806_table_decrs)
#d0806_table_decrs<-d0806_table_decrs[,check2 := check - shift(check,-1L)]
#d0806_table_decrs[d0806_table_decrs$check2 == -1, "check"] = 1
#d0806_table_decrs <- d0806_table_decrs[d0806_table_decrs$check ==1,]
#d0806_table_decrs[,c(1,177)] <- d0806_table_decrs[,c(177,1)]
#d0806_table_decrs <- d0806_table_decrs[-5,]
#rm(d0806_table_decrs_1,d0806_table_decrs_2)
#plot(d0806_table_decrs$speed_ev_kmh ~ d0806_table_decrs$created_on)










# 급정지 (초당 14km/h 이상 감속하여, 속도가 0~5.0km/h 이하가 된 경우) 
df_test_20191115$check4 = 0
df_test_20191115[df_test_20191115$ev_spd_inter <= -14,"check4"] = 1
df_test_20191115[(df_test_20191115$check4 == 1)&(df_test_20191115$ev_speed_kmh>=6),"check4"] = 0



#d0806_table_stop_1 <- d0806_table_tmp[d0806_table_tmp$speed_inter <= -14,] 
#d0806_table_stop_2 <- d0806_table_tmp[d0806_table_tmp$rownum %in% (d0806_table_stop_1$rownum-1),]
#d0806_table_stop <- bind_rows(d0806_table_stop_1,d0806_table_stop_2)
#d0806_table_stop <- unique(d0806_table_stop)
#d0806_table_stop <- arrange(d0806_table_stop, rownum)
#d0806_table_stop$check <- 0
#d0806_table_stop[d0806_table_stop$speed_inter<=-14 & d0806_table_stop$speed_ev_kmh<=5 , "check"] = 1
#d0806_table_stop <- as.data.table(d0806_table_stop)
#d0806_table_stop<-d0806_table_stop[,check2 := check - shift(check,-1L)]
#d0806_table_stop[d0806_table_stop$check2 == -1, "check"] = 1
#d0806_table_stop <- d0806_table_stop[d0806_table_stop$check ==1,]
#d0806_table_stop[,c(1,177)] <- d0806_table_stop[,c(177,1)]
#d0806_table_stop <- d0806_table_stop[-5,]
#rm(d0806_table_stop_1,d0806_table_stop_2)
#plot(d0806_table_stop$speed_ev_kmh ~ d0806_table_stop$created_on)










# 정속 (40 ~110 km/h 사이를 주행하며 가속도 5km/h 이하를 20초 이상 유지) 

# 행 번호를 칼럼으로 추가
df_test_20191115$rownum <- seq.int(nrow(df_test_20191115))

# 정속구간 카운팅 칼럼 check5 초기화
df_test_20191115$check5 = 0



k = 0      # 기준 속도 값 저장
t = 0      # 구간의 duration 저장. 20초 이상 유지해야 함.
start = 0  # 정속 구간의 시작 시간 행 번호
finish = 0 # 정속 구간의 종료 시간 행 번호
up = 0     # 기준 속도 값보다 큰 속도값 중 최대 속도 값에서 기준 속도값을 뺀 값 
down = 0   # 기준 속도 값보다 작은 속도값 중 최저 속도 값을 기준 속도값에서 뺀 값
p = 0      # p = up + down. p는 가속도 이며 값이 5를 넘지 않는 구간 

for(i in 1:4884){ # 테이블의 모든 행을 탐색
  

   k = df_test_20191115[i,68]     # 기준 속도 값 저장. 68번째 칼럼은 ev_speed_kmh
   j = i                          # 기준 속도 값의 다음 행 부터 탐색 시작하기 위한 변수
   start = i                      # 기준 속도 값의 행 번호 저장. 구간의 시작 행 번호 저장. 
   
      while((p <= 5)&(j < 4884)){    # 가속도가 5이하이고 행 끝까지 탐색하지 않았다면 반복
          
          j = j + 1                   # 다음행을 탐색하기 위함
          
          if( k <= df_test_20191115[j,68]){            # 탐색 행의 속도가 기준 속도값 보다 크거나 같다면
                if(up < (df_test_20191115[j,68] - k)){ # up 보다 차이값이 크다면
                  up = df_test_20191115[j,68] - k      # up값 업데이트
                }
          }
          else{
            if(down < (k - df_test_20191115[j,68])){ # down 보다 차이값이 크다면
               down = k - df_test_20191115[j,68]     # down값 업데이트
            }
          }
        
          p = up + down

      }
      
      finish = j
   
      if(p>5){          # 가속도가 5를 넘는다면
         finish = j -1  # 구간의 마지막 행 번호 저장
      }
      
      t = df_test_20191115[finish,60] - df_test_20191115[start,60] # 구간의 duration 저장
      
      if(finish == 4884){
        i == 4884  # 구간의 마지막 행이 데이터의 마지막 행이라면 더 이상 탐색할 필요가 없음으로 for 문 탈츨
      }
      
      if(t>=20){  # duration이 20초 이상이라면 정속구간이다.
        
      }
}



#d0806_table_fix_1 <- d0806_table_tmp[d0806_table_tmp$speed_ev_kmh <= 110 & d0806_table_tmp$speed_ev_kmh >= 40,] 
#d0806_table_stop_2 <- d0806_table_tmp[d0806_table_tmp$rownum %in% (d0806_table_stop_1$rownum-1),]
#d0806_table_stop <- bind_rows(d0806_table_stop_1,d0806_table_stop_2)
#d0806_table_stop <- unique(d0806_table_stop)
#d0806_table_stop <- arrange(d0806_table_stop, rownum)
#d0806_table_stop$check <- 0
#d0806_table_stop[d0806_table_stop$speed_inter<=-14 & d0806_table_stop$speed_ev_kmh<=5 , "check"] = 1
#d0806_table_stop <- as.data.table(d0806_table_stop)
#d0806_table_stop<-d0806_table_stop[,check2 := check - shift(check,-1L)]
#d0806_table_stop[d0806_table_stop$check2 == -1, "check"] = 1
#d0806_table_stop <- d0806_table_stop[d0806_table_stop$check ==1,]
#d0806_table_stop[,c(1,177)] <- d0806_table_stop[,c(177,1)]
#d0806_table_stop <- d0806_table_stop[-5,]
#rm(d0806_table_stop_1,d0806_table_stop_2)
#plot(d0806_table_stop$speed_ev_kmh ~ d0806_table_stop$created_on)




#=============================================================================================


#total_test2 <- total_test %>% filter(ev_speed_kmh != '0.0')

install.packages("dplyr")
library(dplyr)
install.packages("stringr")
library(stringr)
# 불필요 컬럼 제거
new_df <- total_test2 %>% select(-c(MacAddress, gps_speed, accuracy, latitude, longitude, before_parsing,created_on ,MacAddress))

clean_df <- new_df %>% 
  mutate_all(~gsub("%", "", .)) %>%
  mutate_all(~gsub("ah", "", .)) %>%
  mutate_all(~gsub("V", "", .)) %>%
  mutate_all(~gsub("minutes", "", .)) %>%
  mutate_all(~gsub("kW", "", .)) %>%
  mutate_all(~gsub("C", "", .)) %>%
  mutate_all(~gsub("hours", "", .)) %>%
  mutate_all(~gsub("rpm", "", .)) %>%
  mutate_all(~gsub("h", "", .)) %>%
  mutate_all(~gsub("Ah", "", .)) %>%
  mutate_all(~gsub("Hz", "", .)) %>%
  mutate_all(~gsub("A", "", .)) %>%
  mutate_all(~gsub("mph", "", .)) %>%
  mutate_all(~gsub("mp", "", .))
  
clean_df_num <- clean_df %>% 
                  mutate_if(is.character, as.numeric)


install.packages("corrplot")
library(corrplot)
## corrplot 0.84 loaded
M <- cor(clean_df_num)
corrplot(M, method = "circle")

install.packages('plyr')
library(plyr)
detach("package:plyr", unload=TRUE)
detach(package:plyr)

clean_df_num$'000_state of charge bms' <- as.factor(clean_df_num$'000_state of charge bms')

# distance만 누적
temp1 <- clean_df_num %>%
           group_by(`000_state of charge bms`) %>% 
           summarize(cumulative_distance = sum(gps_distance))
  
temp2 <- clean_df_num %>% 
           select(-gps_distance) %>%
           group_by(`000_state of charge bms`) %>% 
           dplyr::summarise_all(funs(mean))

# distance, 속도 두개 누적
temp1 <- clean_df_num %>%
  group_by(`000_state of charge bms`) %>% 
  summarize(cumulative_distance = sum(gps_distance))

temp2 <- clean_df_num %>%
  group_by(`000_state of charge bms`) %>% 
  summarize(cumulative_speed = sum(ev_speed_kmh))

temp3 <- clean_df_num %>% 
  select(-gps_distance, ) %>%
  group_by(`000_state of charge bms`) %>% 
  dplyr::summarise_all(funs(mean))

final_tb <- left_join(temp1, temp2, by = c("000_state of charge bms" = "000_state of charge bms") )

final_tb2 <- left_join(temp3, final_tb, by = c("000_state of charge bms" = "000_state of charge bms") )

real_final_tb <- final_tb2 %>%
                    rename(
                      stateOfChargeBMS = '000_state of charge bms', 
                      A = '003_vmcu accel pedal depth', 
                      B = '003_vmcu accel pedal related', 
                      C = '003_vmcu brake lamp', 
                      D = '003_vmcu brake related', 
                      E = '003_vmcu brakes on', 
                      F = '003_vmcu d', 
                      G = '003_vmcu n', 
                      H = '003_vmcu p', 
                      I = '003_vmcu r', 
                      J = '003_vmcu real vehicle speed', 
                      K = '003_vmcu motor actual speed rpm', 
                      L = '000_auxillary battery voltage', 
                      M = '000_available charge power', 
                      N = '000_available discharge power', 
                      O = '000_battery current', 
                      P = '000_battery dc voltage', 
                      Q = '000_battery fan feedback', 
                      R = '000_battery fan status', 
                      S = '000_battery inlet temperature', 
                      T = '000_battery max temperature', 
                      U = '000_bms ignition', 
                      V = '000_bms main relay', 
                      W = '000_cumulative charge current', 
                      X = '000_cumulative discharge current', 
                      Y = '000_cumulative energy charged', 
                      Z = '000_cumulative energy discharged', 
                      A1 = '000_drive motor speed 1', 
                      B1 = '000_drive motor speed 2', 
                      C1 = '000_inverter capacitor voltage', 
                      D1 = '000_maximum cell voltage', 
                      E1 = '000_maximum cell voltage no_', 
                      F1 = '000_minimum cell voltage', 
                      G1 = '000_minimum cell voltage no_', 
                      H1 = '000_operating time', 
                      I1 = '000_airbag h/wire duty', 
                      J1 = '000_battery cell voltage deviation', 
                      K1 = '000_minimum deterioration', 
                      L1 = '000_minimum deterioration cell no_', 
                      M1 = '000_state of charge display', 
                      N1 = '004_calc estimated time 45kw 80% charge', 
                      O1 = '000_battery power', 
                      P1 = '004_calc average cell voltage', 
                      Q1 = '000_battery min temperature',
                      R1 = '000_maximum deterioration cell no_',
                      S1 ='003_vmcu ambient temp'
                    )
real_final_tb2 <- real_final_tb %>% 
                    select(-stateOfChargeBMS)

install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(real_final_tb2, histogram=TRUE, pch=19)

library(corrplot)
## corrplot 0.84 loaded
M <- cor(real_final_tb2)
corrplot(M, method = "circle")



install.packages("DAAG")
library(DAAG)
cv.lm(data = real_final_tb2, form.lm = formula(cumulative_distance ~ .), m=5, dots = 
        FALSE, seed=29, plotit=TRUE, printit=TRUE)


set.seed(123) 
trainingRowIndex <- sample(1:nrow(real_final_tb2), 0.8*nrow(real_final_tb2))
trainingData <- real_final_tb2[trainingRowIndex, ]  # training data
testData  <- real_final_tb2[-trainingRowIndex, ]   # test data

# modTrain <- lm(cumulative_distance ~. -G -I -N -Q -R -U -V -R1 -K1 -L1 , data=trainingData)
modTrain <- lm(cumulative_distance ~ A+B+C+D+F+B1+S1+ev_speed_kmh+cumulative_speed, data=trainingData)
predict <- predict(modTrain, testData)  
summary(modTrain)

act_pred <- data.frame(cbind(actuals=testData$cumulative_distance, predicteds=predict)) 
cor(act_pred)

head(act_pred, n=10)

mape <- mean(abs((act_pred$predicteds - act_pred$actuals))/act_pred$actuals)
print(mape)

