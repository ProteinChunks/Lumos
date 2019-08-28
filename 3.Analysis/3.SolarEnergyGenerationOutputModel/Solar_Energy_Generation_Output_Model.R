# Solar energy generation output model

# References : https://photovoltaic-software.com/principle-ressources/how-calculate-solar-energy-power-pv-systems

# The global formula of electricity generated in output of a photovoltaic system
# E = A * r * H * PR
# E = Energy (kWh) 
# A = Total solar panel Area (m2) 
# r = solar panel yield or efficiency(%) 
# H = Annual average solar radiation on tilted panels (shadings not included)
# PR = Performance ratio, coefficient for losses (range between 0.5 and 0.9, default value = 0.75)

### Solar energy generate in each Seoul district
bike_half_compl<-read.csv("bike_half_compl.csv")                # Coordinates of bikeroads in Seoul.
bike_half_list<-split(bike_half_compl,bike_half_compl$지번주소) # Split them by the names of districts.
str(bike_half_list)

df<-read.csv("solar_3year_predict.csv")                         # Expected insolation of Seoul for the next 3 years.
bike_m2_dj<-read.csv("bikeroad_length_width(district).csv")     # Area of bikeroads in each district.





##### 1. Gangnam district 
## Total expected energy generate(per year) : 31209911kwh
kw_gn = ((((bike_m2_dj[1,2]*bike_m2_dj[1,3]) / (0.27*0.27)) * 2/5) *
           (0.0625 * 0.1675 * (mean(df$Point.Forecast)*365) * 0.78)) +
  ((((bike_m2_dj[1,2]*bike_m2_dj[1,3]) / (0.27*0.27))  * 3/5) *
     (0.0625 * 0.1675 * (mean(df$Point.Forecast)*365) * 0.81))

## Total expected energy generate(per month)
# 2/5 of solar panels are shaded by buliding shadow, 3/5 are not shaded. This is reflected on a for sentence just below.
kw_gn_list <- list()
for(i in 1:36){
  kw_gn_list[[i]] <- ((((bike_m2_dj[1,2]*bike_m2_dj[1,3]) / (0.27*0.27)) * 2/5) * (0.0625 * 0.1675 * (mean(df$Point.Forecast[i])*30) * 0.78)) +
    ((((bike_m2_dj[1,2]*bike_m2_dj[1,3]) / (0.27*0.27))  * 3/5) * (0.0625 * 0.1675 * (mean(df$Point.Forecast[i])*30) * 0.81))
}

kw_gn_list[[36]]
kw_gn_list_cbind <- cbind(kw_gn_list[[1]], kw_gn_list[[2]]) # Bind monthly energy generate from 1st month to 36th month.

for(i in 3:36){
  kw_gn_list_cbind <- cbind(kw_gn_list_cbind, kw_gn_list[[i]])
}

## Monthly expected energy generate(36 months)
kw_gn_list_cbind <- cbind("강남구", kw_gn_list_cbind) 
kw_gn_list_cbind  



##### 2. Gangdong district 
## Total expected energy generate(per year) : 19378164kwh
kw_gd = ((((bike_m2_dj[2,2]*bike_m2_dj[2,3]) / (0.27*0.27)) * 1/4) *
           (0.0625 * 0.1675 * (mean(df$Point.Forecast.1)*365) * 0.78)) +
  ((((bike_m2_dj[2,2]*bike_m2_dj[2,3]) / (0.27*0.27))  * 3/4) * 
     (0.0625 * 0.1675 * (mean(df$Point.Forecast.1)*365) * 0.81))

## Total expected energy generate(per month)
# 1/4 of solar panels are shaded by buliding shadow, 3/4 are not shaded. This is reflected on a for sentence just below.
kw_gd_list <- list()
for(i in 1:36){
  kw_gd_list[[i]] <- ((((bike_m2_dj[2,2]*bike_m2_dj[2,3]) / (0.27*0.27)) * 1/4) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.1[i])*30) * 0.78)) +
    ((((bike_m2_dj[2,2]*bike_m2_dj[2,3]) / (0.27*0.27))  * 3/4) * (0.0625 * 0.1675 * (mean(df$Point.Forecast.1[i])*30) * 0.81))
}

kw_gd_list[[36]]
kw_gd_list_cbind <- cbind(kw_gd_list[[1]], kw_gd_list[[2]])# Bind monthly energy generate from 1st month to 36th month.

for(i in 3:36){
  kw_gd_list_cbind <- cbind(kw_gd_list_cbind, kw_gd_list[[i]])
}

## Monthly expected energy generate(36 months)
kw_gd_list_cbind <- cbind("강동구", kw_gd_list_cbind)
kw_gd_list_cbind



##### 3. Gangbuk district 
## Total expected energy generate(per year) : 3335034kwh
kw_gb = ((((bike_m2_dj[3,2]*bike_m2_dj[3,3]) / (0.27*0.27)) * 1/4) *
           (0.0625 * 0.1675 * (mean(df$Point.Forecast.2)*365) * 0.78)) +
  ((((bike_m2_dj[3,2]*bike_m2_dj[3,3]) / (0.27*0.27))  * 3/4) *
     (0.0625 * 0.1675 * (mean(df$Point.Forecast.2)*365) * 0.81))

## Total expected energy generate(per month)
# 1/4 of solar panels are shaded by buliding shadow, 3/4 are not shaded. This is reflected on a for sentence just below.
kw_gb_list <- list()
for(i in 1:36){
  kw_gb_list[[i]] <- ((((bike_m2_dj[3,2]*bike_m2_dj[3,3]) / (0.27*0.27)) * 1/4) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.2[i])*30) * 0.78)) +
    ((((bike_m2_dj[3,2]*bike_m2_dj[3,3]) / (0.27*0.27))  * 3/4) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.2[i])*30) * 0.81))
}

kw_gb_list[[36]]
kw_gb_list_cbind <- cbind(kw_gb_list[[1]], kw_gb_list[[2]])# Bind monthly energy generate from 1st month to 36th month.

for(i in 3:36){
  kw_gb_list_cbind <- cbind(kw_gb_list_cbind, kw_gb_list[[i]])
}

## Monthly expected energy generate(36 months)
kw_gb_list_cbind <- cbind("강북구", kw_gb_list_cbind)
kw_gb_list_cbind



##### 4. Gangseo district 
## Total expected energy generate(per year) : 11177992kwh
kw_gs = ((((bike_m2_dj[4,2]*bike_m2_dj[4,3]) / (0.27*0.27)) * 1/4) *
           (0.0625 * 0.1675 * (mean(df$Point.Forecast.3)*365) * 0.78)) +
  ((((bike_m2_dj[4,2]*bike_m2_dj[4,3]) / (0.27*0.27))  * 3/4) *
     (0.0625 * 0.1675 * (mean(df$Point.Forecast.3)*365) * 0.81))

## Total expected energy generate(per month)
# 1/4 of solar panels are shaded by buliding shadow, 3/4 are not shaded. This is reflected on a for sentence just below.
kw_gs_list <- list()
for(i in 1:36){
  kw_gs_list[[i]] <- ((((bike_m2_dj[4,2]*bike_m2_dj[4,3]) / (0.27*0.27)) * 1/4) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.3[i])*30) * 0.78)) +
    ((((bike_m2_dj[4,2]*bike_m2_dj[4,3]) / (0.27*0.27))  * 3/4) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.3[i])*30) * 0.81))
}

kw_gs_list[[36]]
kw_gs_list_cbind <- cbind(kw_gs_list[[1]], kw_gs_list[[2]])# Bind monthly energy generate from 1st month to 36th month. 

for(i in 3:36){
  kw_gs_list_cbind <- cbind(kw_gs_list_cbind, kw_gs_list[[i]])
}

## Monthly expected energy generate(36 months)
kw_gs_list_cbind <- cbind("강서구", kw_gs_list_cbind)
kw_gs_list_cbind



##### 5. Gwanak district 
## Total expected energy generate(per year) : 10169155kwh
kw_gw = ((((bike_m2_dj[5,2]*bike_m2_dj[5,3]) / (0.27*0.27)) * 1/4) *
           (0.0625 * 0.1675 * (mean(df$Point.Forecast.4)*365) * 0.78)) +
  ((((bike_m2_dj[5,2]*bike_m2_dj[5,3]) / (0.27*0.27))  * 3/4) *
     (0.0625 * 0.1675 * (mean(df$Point.Forecast.4)*365) * 0.81))

## Total expected energy generate(per month)
# 1/4 of solar panels are shaded by buliding shadow, 3/4 are not shaded. This is reflected on a for sentence just below.
kw_gw_list <- list()
for(i in 1:36){
  kw_gw_list[[i]] <- ((((bike_m2_dj[5,2]*bike_m2_dj[5,3]) / (0.27*0.27)) * 1/4) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.4[i])*30) * 0.78)) +
    ((((bike_m2_dj[5,2]*bike_m2_dj[5,3]) / (0.27*0.27))  * 3/4) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.4[i])*30) * 0.81))
}

kw_gw_list[[36]]
kw_gw_list_cbind <- cbind(kw_gw_list[[1]], kw_gw_list[[2]])# Bind monthly energy generate from 1st month to 36th month. 

for(i in 3:36){
  kw_gw_list_cbind <- cbind(kw_gw_list_cbind, kw_gw_list[[i]])
}

## Monthly expected energy generate(36 months)
kw_gw_list_cbind <- cbind("관악구", kw_gw_list_cbind)
kw_gw_list_cbind



##### 6. Gwangjin district
## Total expected energy generate(per year) : 32929825kwh
kw_gj = ((((bike_m2_dj[6,2]*bike_m2_dj[6,3]) / (0.27*0.27)) * 1/11) *
           (0.0625 * 0.1675 * (mean(df$Point.Forecast.5)*365) * 0.78)) +
  ((((bike_m2_dj[6,2]*bike_m2_dj[6,3]) / (0.27*0.27))  * 10/11) *
     (0.0625 * 0.1675 * (mean(df$Point.Forecast.5)*365) * 0.81))

## Total expected energy generate(per month)
# 1/11 of solar panels are shaded by buliding shadow, 10/11 are not shaded. This is reflected on a for sentence just below.
kw_gj_list <- list()
for(i in 1:36){
  kw_gj_list[[i]] <- ((((bike_m2_dj[6,2]*bike_m2_dj[6,3]) / (0.27*0.27)) * 1/11) * (0.0625 * 0.1675 * (mean(df$Point.Forecast.5[i])*30) * 0.78)) +
    ((((bike_m2_dj[6,2]*bike_m2_dj[6,3]) / (0.27*0.27))  * 10/11) * (0.0625 * 0.1675 * (mean(df$Point.Forecast.5[i])*30) * 0.81))
}

kw_gj_list[[36]]
kw_gj_list_cbind <- cbind(kw_gj_list[[1]], kw_gj_list[[2]])# Bind monthly energy generate from 1st month to 36th month. 

for(i in 3:36){
  kw_gj_list_cbind <- cbind(kw_gj_list_cbind, kw_gj_list[[i]])
}

## Monthly expected energy generate(36 months)
kw_gj_list_cbind <- cbind("광진구", kw_gj_list_cbind)
kw_gj_list_cbind



##### 7. Guro district
## Total expected energy generate(per year) : 11339849kwh
kw_gr = ((((bike_m2_dj[7,2]*bike_m2_dj[7,3]) / (0.27*0.27)) * 1/4) *
           (0.0625 * 0.1675 * (mean(df$Point.Forecast.6)*365) * 0.78)) +
  ((((bike_m2_dj[7,2]*bike_m2_dj[7,3]) / (0.27*0.27))  * 3/4) *
     (0.0625 * 0.1675 * (mean(df$Point.Forecast.6)*365) * 0.81))

## Total expected energy generate(per month)
# 1/4 of solar panels are shaded by buliding shadow, 3/4 are not shaded. This is reflected on a for sentence just below.
kw_gr_list <- list()
for(i in 1:36){
  kw_gr_list[[i]] <- ((((bike_m2_dj[7,2]*bike_m2_dj[7,3]) / (0.27*0.27)) * 1/4) * (0.0625 * 0.1675 * (mean(df$Point.Forecast.6[i])*30) * 0.78)) +
    ((((bike_m2_dj[7,2]*bike_m2_dj[7,3]) / (0.27*0.27))  * 3/4) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.6[i])*30) * 0.81))
}

kw_gr_list[[36]]
kw_gr_list_cbind <- cbind(kw_gr_list[[1]], kw_gr_list[[2]])# Bind monthly energy generate from 1st month to 36th month.

for(i in 3:36){
  kw_gr_list_cbind <- cbind(kw_gr_list_cbind, kw_gr_list[[i]])
}

## Monthly expected energy generate(36 months)
kw_gr_list_cbind <- cbind("구로구", kw_gr_list_cbind)
kw_gr_list_cbind



##### 8. Geumcheon district 
## Total expected energy generate(per year) : 8324530kwh
kw_gc = ((((bike_m2_dj[8,2]*bike_m2_dj[8,3]) / (0.27*0.27)) * 1/9) *
           (0.0625 * 0.1675 * (mean(df$Point.Forecast.7)*365) * 0.78)) +
  ((((bike_m2_dj[8,2]*bike_m2_dj[8,3]) / (0.27*0.27))  * 8/9) *
     (0.0625 * 0.1675 * (mean(df$Point.Forecast.7)*365) * 0.81))

## Total expected energy generate(per month)
# 1/9 of solar panels are shaded by buliding shadow, 8/9 are not shaded. This is reflected on a for sentence just below.
kw_gc_list <- list()
for(i in 1:36){
  kw_gc_list[[i]] <- ((((bike_m2_dj[8,2]*bike_m2_dj[8,3]) / (0.27*0.27)) * 1/9) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.7[i])*30) * 0.78)) +
    ((((bike_m2_dj[8,2]*bike_m2_dj[8,3]) / (0.27*0.27))  * 8/9) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.7[i])*30) * 0.81))
}

kw_gc_list[[36]]
kw_gc_list_cbind <- cbind(kw_gc_list[[1]], kw_gc_list[[2]])# Bind monthly energy generate from 1st month to 36th month.

for(i in 3:36){
  kw_gc_list_cbind <- cbind(kw_gc_list_cbind, kw_gc_list[[i]])
}

## Monthly expected energy generate(36 months)
kw_gc_list_cbind <- cbind("금천구", kw_gc_list_cbind)
kw_gc_list_cbind



##### 9. Nowon district
## Total expected energy generate(per year) : 17584943kwh
kw_nw = ((((bike_m2_dj[9,2]*bike_m2_dj[9,3]) / (0.27*0.27)) * 1) *
           (0.0625 * 0.1675 * (mean(df$Point.Forecast.8)*365) * 0.78)) +
  ((((bike_m2_dj[9,2]*bike_m2_dj[9,3]) / (0.27*0.27)) * 0) *
     (0.0625 * 0.1675 * (mean(df$Point.Forecast.8)*365) * 0.81))

## Total expected energy generate(per month)
# Whole solar panels are shaded by buliding shadow. This is reflected on a for sentence just below.
kw_nw_list <- list()
for(i in 1:36){
  kw_nw_list[[i]] <- ((((bike_m2_dj[9,2]*bike_m2_dj[9,3]) / (0.27*0.27))) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.8[i])*30) * 0.78))
}

kw_nw_list[[36]]
kw_nw_list_cbind <- cbind(kw_nw_list[[1]], kw_nw_list[[2]])# Bind monthly energy generate from 1st month to 36th month. 

for(i in 3:36){
  kw_nw_list_cbind <- cbind(kw_nw_list_cbind, kw_nw_list[[i]])
}

## Monthly expected energy generate(36 months)
kw_nw_list_cbind <- cbind("노원구", kw_nw_list_cbind)
kw_nw_list_cbind



##### 10. Dobong district 
## Total expected energy generate(per year) : 7918935kwh
kw_db = ((((bike_m2_dj[10,2]*bike_m2_dj[10,3]) / (0.27*0.27)) * 5/6) *
           (0.0625 * 0.1675 * (mean(df$Point.Forecast.9)*365) * 0.78)) +
  ((((bike_m2_dj[10,2]*bike_m2_dj[10,3]) / (0.27*0.27))  * 1/6) *
     (0.0625 * 0.1675 * (mean(df$Point.Forecast.9)*365) * 0.81))

## Total expected energy generate(per month)
# 5/6 of solar panels are shaded by buliding shadow, 1/6 are not shaded. This is reflected on a for sentence just below.
kw_db_list <- list()
for(i in 1:36){
  kw_db_list[[i]] <- ((((bike_m2_dj[10,2]*bike_m2_dj[10,3]) / (0.27*0.27)) * 5/6) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.9[i])*30) * 0.78)) +
    ((((bike_m2_dj[10,2]*bike_m2_dj[10,3]) / (0.27*0.27))  * 1/6) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.9[i])*30) * 0.81))
}

kw_db_list[[36]]
kw_db_list_cbind <- cbind(kw_db_list[[1]], kw_db_list[[2]])# Bind monthly energy generate from 1st month to 36th month. 

for(i in 3:36){
  kw_db_list_cbind <- cbind(kw_db_list_cbind, kw_db_list[[i]])
}

## Monthly expected energy generate(36 months)
kw_db_list_cbind <- cbind("도봉구", kw_db_list_cbind)
kw_db_list_cbind



##### 11. Dongdaemun district 
## Total expected energy generate(per year) : 17355912kwh
kw_dd = ((((bike_m2_dj[11,2]*bike_m2_dj[11,3]) / (0.27*0.27)) * 3/5) *
           (0.0625 * 0.1675 * (mean(df$Point.Forecast.10)*365) * 0.78)) +
  ((((bike_m2_dj[11,2]*bike_m2_dj[11,3]) / (0.27*0.27))  * 2/5) *
     (0.0625 * 0.1675 * (mean(df$Point.Forecast.10)*365) * 0.81))

## Total expected energy generate(per month)
# 3/5 of solar panels are shaded by buliding shadow, 2/5 are not shaded. This is reflected on a for sentence just below.
kw_dd_list <- list()
for(i in 1:36){
  kw_dd_list[[i]] <- ((((bike_m2_dj[11,2]*bike_m2_dj[11,3]) / (0.27*0.27)) * 3/5) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.10[i])*30) * 0.78)) +
    ((((bike_m2_dj[11,2]*bike_m2_dj[11,3]) / (0.27*0.27))  * 2/5) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.10[i])*30) * 0.81))
}

kw_dd_list[[36]]
kw_dd_list_cbind <- cbind(kw_dd_list[[1]], kw_dd_list[[2]])# Bind monthly energy generate from 1st month to 36th month. 

for(i in 3:36){
  kw_dd_list_cbind <- cbind(kw_dd_list_cbind, kw_dd_list[[i]])
}

## Monthly expected energy generate(36 months)
kw_dd_list_cbind <- cbind("동대문구", kw_dd_list_cbind)
kw_dd_list_cbind



##### 12. Dongjak district 
## Total expected energy generate(per year) : 7673429kwh
kw_dj = ((((bike_m2_dj[12,2]*bike_m2_dj[12,3]) / (0.27*0.27)) * 4/7) *
           (0.0625 * 0.1675 * (mean(df$Point.Forecast.11)*365) * 0.78)) +
  ((((bike_m2_dj[12,2]*bike_m2_dj[12,3]) / (0.27*0.27))  * 3/7) *
     (0.0625 * 0.1675 * (mean(df$Point.Forecast.11)*365) * 0.81))

## Total expected energy generate(per month)
# 4/7 of solar panels are shaded by buliding shadow, 3/7 are not shaded. This is reflected on a for sentence just below.
kw_dj_list <- list()
for(i in 1:36){
  kw_dj_list[[i]] <- ((((bike_m2_dj[12,2]*bike_m2_dj[12,3]) / (0.27*0.27)) * 4/7) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.11[i])*30) * 0.78)) +
    ((((bike_m2_dj[12,2]*bike_m2_dj[12,3]) / (0.27*0.27))  * 3/7) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.11[i])*30) * 0.81))
}


kw_dj_list[[36]]
kw_dj_list_cbind <- cbind(kw_dj_list[[1]], kw_dj_list[[2]])# Bind monthly energy generate from 1st month to 36th month.

for(i in 3:36){
  kw_dj_list_cbind <- cbind(kw_dj_list_cbind, kw_dj_list[[i]])
}

## Monthly expected energy generate(36 months)
kw_dj_list_cbind <- cbind("동작구", kw_dj_list_cbind)
kw_dj_list_cbind



##### 13. Mapo district
## Total expected energy generate(per year) : 15281754kwh
kw_mp = ((((bike_m2_dj[13,2]*bike_m2_dj[13,3]) / (0.27*0.27)) * 15/16) *
           (0.0625 * 0.1675 * (mean(df$Point.Forecast.12)*365) * 0.78)) +
  ((((bike_m2_dj[13,2]*bike_m2_dj[13,3]) / (0.27*0.27))  * 1/16) *
     (0.0625 * 0.1675 * (mean(df$Point.Forecast.12)*365) * 0.81))

## Total expected energy generate(per month)
# 15/16 of solar panels are shaded by buliding shadow, 1/16 are not shaded. This is reflected on a for sentence just below.
kw_mp_list <- list()
for(i in 1:36){
  kw_mp_list[[i]] <- ((((bike_m2_dj[13,2]*bike_m2_dj[13,3]) / (0.27*0.27)) * 15/16) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.12[i])*30) * 0.78)) +
    ((((bike_m2_dj[13,2]*bike_m2_dj[13,3]) / (0.27*0.27))  * 1/16) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.12[i])*30) * 0.81))
}

kw_mp_list[[36]]
kw_mp_list_cbind <- cbind(kw_mp_list[[1]], kw_mp_list[[2]])# Bind monthly energy generate from 1st month to 36th month.

for(i in 3:36){
  kw_mp_list_cbind <- cbind(kw_mp_list_cbind, kw_mp_list[[i]])
}

## Monthly expected energy generate(36 months)
kw_mp_list_cbind <- cbind("마포구", kw_mp_list_cbind)
kw_mp_list_cbind



##### 14. Seodaemun district 
## Total expected energy generate(per year) : 42026730kwh
kw_sd = ((((bike_m2_dj[14,2]*bike_m2_dj[14,3]) / (0.27*0.27)) * 5/6) *
           (0.0625 * 0.1675 * (mean(df$Point.Forecast.13)*365) * 0.78)) +
  ((((bike_m2_dj[14,2]*bike_m2_dj[14,3]) / (0.27*0.27))  * 1/6) *
     (0.0625 * 0.1675 * (mean(df$Point.Forecast.13)*365) * 0.81))

## Total expected energy generate(per month)
# 5/6 of solar panels are shaded by buliding shadow, 1/6 are not shaded. This is reflected on a for sentence just below.
kw_sd_list <- list()
for(i in 1:36){
  kw_sd_list[[i]] <- ((((bike_m2_dj[14,2]*bike_m2_dj[14,3]) / (0.27*0.27)) * 5/6) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.13[i])*30) * 0.78)) +
    ((((bike_m2_dj[14,2]*bike_m2_dj[14,3]) / (0.27*0.27))  * 1/6) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.13[i])*30) * 0.81))
}

kw_sd_list[[36]]
kw_sd_list_cbind <- cbind(kw_sd_list[[1]], kw_sd_list[[2]])# Bind monthly energy generate from 1st month to 36th month. 

for(i in 3:36){
  kw_sd_list_cbind <- cbind(kw_sd_list_cbind, kw_sd_list[[i]])
}

## Monthly expected energy generate(36 months)
kw_sd_list_cbind <- cbind("서대문구", kw_sd_list_cbind)
kw_sd_list_cbind



##### 15. Seocho district
## Total expected energy generate(per year) : 16571946kwh
kw_sc = ((((bike_m2_dj[15,2]*bike_m2_dj[15,3]) / (0.27*0.27)) * 7/8) *
           (0.0625 * 0.1675 * (mean(df$Point.Forecast.14)*365) * 0.78)) +
  ((((bike_m2_dj[15,2]*bike_m2_dj[15,3]) / (0.27*0.27))  * 1/8) *
     (0.0625 * 0.1675 * (mean(df$Point.Forecast.14)*365) * 0.81))

## Total expected energy generate(per month)
# 7/8 of solar panels are shaded by buliding shadow, 1/8 are not shaded. This is reflected on a for sentence just below.
kw_sc_list <- list()
for(i in 1:36){
  kw_sc_list[[i]] <- ((((bike_m2_dj[15,2]*bike_m2_dj[15,3]) / (0.27*0.27)) * 7/8) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.14[i])*30) * 0.78)) +
    ((((bike_m2_dj[15,2]*bike_m2_dj[15,3]) / (0.27*0.27))  * 1/8) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.14[i])*30) * 0.81))
}

kw_sc_list[[36]]
kw_sc_list_cbind <- cbind(kw_sc_list[[1]], kw_sc_list[[2]])# Bind monthly energy generate from 1st month to 36th month.

for(i in 3:36){
  kw_sc_list_cbind <- cbind(kw_sc_list_cbind, kw_sc_list[[i]])
}

# Monthly expected energy generate(36 months)
kw_sc_list_cbind <- cbind("서초구", kw_sc_list_cbind)
kw_sc_list_cbind



##### 16. Seongdong district 
## Total expected energy generate(per year) : 24277589kwh
kw_seongdong = ((((bike_m2_dj[16,2]*bike_m2_dj[16,3]) / (0.27*0.27)) * 8/9) *
           (0.0625 * 0.1675 * (mean(df$Point.Forecast.15)*365) * 0.78)) +
  ((((bike_m2_dj[16,2]*bike_m2_dj[16,3]) / (0.27*0.27))  * 1/9) *
     (0.0625 * 0.1675 * (mean(df$Point.Forecast.15)*365) * 0.81))

## Total expected energy generate(per month)
# 8/9 of solar panels are shaded by buliding shadow, 1/9 are not shaded. This is reflected on a for sentence just below.
kw_seongdong_list <- list()
for(i in 1:36){
  kw_seongdong_list[[i]] <- ((((bike_m2_dj[16,2]*bike_m2_dj[16,3]) / (0.27*0.27)) * 8/9) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.15[i])*30) * 0.78)) +
    ((((bike_m2_dj[16,2]*bike_m2_dj[16,3]) / (0.27*0.27))  * 1/9) *(0.0625 * 0.1675 * (mean(df$Point.Forecast.15[i])*30) * 0.81))
}

kw_seongdong_list[[36]]
kw_seongdong_list_cbind <- cbind(kw_seongdong_list[[1]], kw_seongdong_list[[2]])# Bind monthly energy generate from 1st month to 36th month. 

for(i in 3:36){
  kw_seongdong_list_cbind <- cbind(kw_seongdong_list_cbind, kw_seongdong_list[[i]])
}

## Monthly expected energy generate(36 months)
kw_seongdong_list_cbind <- cbind("성동구", kw_seongdong_list_cbind)
kw_seongdong_list_cbind



##### 17. Seongbuk district
## Total expected energy generate(per year) : 3308915kwh
kw_sb = ((((bike_m2_dj[17, 2]*bike_m2_dj[17, 3]) / (0.27*0.27)) * 3/4) * 
           (0.0625 * 0.1675 * (mean(df$Point.Forecast.16)*365) * 0.78)) + 
  ((((bike_m2_dj[17,2]*bike_m2_dj[17,3]) / (0.27*0.27))  * 1/4) * 
     (0.0625 * 0.1675 * (mean(df$Point.Forecast.16)*365) * 0.81))

## Total expected energy generate(per month)
# 3/4 of solar panels are shaded by buliding shadow, 1/4 are not shaded. This is reflected on a for sentence just below.
kw_sb_list <- list()
for(i in 1:36){
  kw_sb_list[[i]] <- ((((bike_m2_dj[17,2]*bike_m2_dj[17,3]) / (0.27*0.27)) * 3/4) * (0.0625 * 0.1675 * (mean(df$Point.Forecast.16[i])*30) * 0.78)) + 
    ((((bike_m2_dj[17,2]*bike_m2_dj[17,3]) / (0.27*0.27))  * 1/4) * (0.0625 * 0.1675 * (mean(df$Point.Forecast.16[i])*30) * 0.81))
}

kw_sb_list[[36]]
kw_sb_list_cbind <- cbind(kw_sb_list[[1]], kw_sb_list[[2]])# Bind monthly energy generate from 1st month to 36th month.

for(i in 3:36){
  kw_sb_list_cbind <- cbind(kw_sb_list_cbind, kw_sb_list[[i]])
}

## Monthly expected energy generate(36 months)
kw_sb_list_cbind <- cbind("성북구", kw_sb_list_cbind)
kw_sb_list_cbind



##### 18. Songpa district
## Total expected energy generate(per year) : 24528557kwh
kw_songpa = ((((bike_m2_dj[18,2]*bike_m2_dj[18,3]) / (0.27*0.27)) * 5/8) * 
               (0.0625 * 0.1675 * (mean(df$Point.Forecast.17)*365) * 0.78)) + 
  ((((bike_m2_dj[18,2]*bike_m2_dj[18,3]) / (0.27*0.27))  * 3/8) * 
     (0.0625 * 0.1675 * (mean(df$Point.Forecast.17)*365) * 0.81))   

## Total expected energy generate(per month)
# 5/8 of solar panels are shaded by buliding shadow, 3/8 are not shaded. This is reflected on a for sentence just below.
kw_songpa_list <- list()
for(i in 1:36){
  kw_songpa_list[[i]] <- ((((bike_m2_dj[18,2]*bike_m2_dj[18,3]) / (0.27*0.27)) * 5/8) * (0.0625 * 0.1675 * (mean(df$Point.Forecast.17[i])*30) * 0.78)) +
    ((((bike_m2_dj[18,2]*bike_m2_dj[18,3]) / (0.27*0.27))  * 3/8) * (0.0625 * 0.1675 * (mean(df$Point.Forecast.17[i])*30) * 0.81))   
}

kw_songpa_list[[36]]
kw_songpa_list_cbind <- cbind(kw_songpa_list[[1]], kw_songpa_list[[2]]) 

for(i in 3:36){
  kw_songpa_list_cbind <- cbind(kw_songpa_list_cbind,kw_songpa_list[[i]])
}

## Monthly expected energy generate(36 months)
kw_songpa_list_cbind <- cbind("송파구", kw_songpa_list_cbind)
kw_songpa_list_cbind



##### 19. Yangcheon district
## Total expected energy generate(per year) : 19230085kwh
kw_yangcheon = ((((bike_m2_dj[19,2]*bike_m2_dj[19,3]) / (0.27*0.27)) * 4/5) * 
                  (0.0625 * 0.1675 * (mean(df$Point.Forecast.18)*365) * 0.78)) +
  ((((bike_m2_dj[19,2]*bike_m2_dj[19,3]) / (0.27*0.27))  * 1/5) * 
     (0.0625 * 0.1675 * (mean(df$Point.Forecast.18)*365) * 0.81))   

## Total expected energy generate(per month)
# 4/5 of solar panels are shaded by buliding shadow, 1/5 are not shaded. This is reflected on a for sentence just below.
kw_yangcheon_list <- list()
for(i in 1:36){
  kw_yangcheon_list[[i]] <- ((((bike_m2_dj[19,2]*bike_m2_dj[19,3]) / (0.27*0.27)) * 4/5) * (0.0625 * 0.1675 * (mean(df$Point.Forecast.18[i])*30) * 0.78)) +
    ((((bike_m2_dj[19,2]*bike_m2_dj[19,3]) / (0.27*0.27))  * 1/5) * (0.0625 * 0.1675 * (mean(df$Point.Forecast.18[i])*30) * 0.81))   
}

kw_yangcheon_list[[36]]
kw_yangcheon_list_cbind <- cbind(kw_yangcheon_list[[1]], kw_yangcheon_list[[2]]) 

for(i in 3:36){
  kw_yangcheon_list_cbind <- cbind(kw_yangcheon_list_cbind, kw_yangcheon_list[[i]])
}

## Monthly expected energy generate(36 months)
kw_yangcheon_list_cbind <- cbind("양천구", kw_yangcheon_list_cbind)
kw_yangcheon_list_cbind



##### 20. Yeongdeungpo district
## Total expected energy generate(per year) : kwh     # 영등포구 : 18140623 kWh/y???????????
kw_yeongdeungpo = ((((bike_m2_dj[20,2]*bike_m2_dj[20,3]) / (0.27*0.27)) * 4/5) * 
                     (0.0625 * 0.1675 * (mean(df$Point.Forecast.19)*365) * 0.78)) +
  ((((bike_m2_dj[20,2]*bike_m2_dj[20,3]) / (0.27*0.27))  * 1/5) * 
     (0.0625 * 0.1675 * (mean(df$Point.Forecast.19)*365) * 0.81))   

## Total expected energy generate(per month)
# 4/5 of solar panels are shaded by buliding shadow, 1/5 are not shaded. This is reflected on a for sentence just below.
kw_yeongdeungpo_list <- list()
for(i in 1:36){
  kw_yeongdeungpo_list[[i]] <- ((((bike_m2_dj[20,2]*bike_m2_dj[20,3]) / (0.27*0.27)) * 4/5) * (0.0625 * 0.1675 * (mean(df$Point.Forecast.19[i])*30) * 0.78)) +
    ((((bike_m2_dj[20,2]*bike_m2_dj[20,3]) / (0.27*0.27))  * 1/5) * (0.0625 * 0.1675 * (mean(df$Point.Forecast.19[i])*30) * 0.81))    
}

kw_yeongdeungpo_list[[36]]
kw_yeongdeungpo_list_cbind <- cbind(kw_yeongdeungpo_list[[1]], kw_yeongdeungpo_list[[2]]) 

for(i in 3:36){
  kw_yeongdeungpo_list_cbind <- cbind(kw_yeongdeungpo_list_cbind, kw_yeongdeungpo_list[[i]])
}

## Monthly expected energy generate(36 months)
kw_yeongdeungpo_list_cbind <- cbind("영등포구", kw_yeongdeungpo_list_cbind)
kw_yeongdeungpo_list_cbind



##### 21. Yongsan district
## Total expected energy generate(per year) : 9066428kwh
kw_yongsan = ((((bike_m2_dj[21,2]*bike_m2_dj[21,3]) / (0.27*0.27)) * 4/5) * 
                (0.0625 * 0.1675 * (mean(df$Point.Forecast.20)*365) * 0.78)) + 
  ((((bike_m2_dj[21,2]*bike_m2_dj[21,3]) / (0.27*0.27))  * 1/5) * 
     (0.0625 * 0.1675 * (mean(df$Point.Forecast.20)*365) * 0.81))   

## Total expected energy generate(per month)
# 4/5 of solar panels are shaded by buliding shadow, 1/5 are not shaded. This is reflected on a for sentence just below.
kw_yongsan_list <- list()
for(i in 1:36){
  kw_yongsan_list[[i]] <- ((((bike_m2_dj[21,2]*bike_m2_dj[21,3]) / (0.27*0.27)) * 4/5) * (0.0625 * 0.1675 * (mean(df$Point.Forecast.20[i])*30) * 0.78)) +
    ((((bike_m2_dj[21,2]*bike_m2_dj[21,3]) / (0.27*0.27))  * 1/5) * (0.0625 * 0.1675 * (mean(df$Point.Forecast.20[i])*30) * 0.81))    
}

kw_yongsan_list[[36]]
kw_yongsan_list_cbind <- cbind(kw_yongsan_list[[1]], kw_yongsan_list[[2]]) 

for(i in 3:36){
  kw_yongsan_list_cbind <- cbind(kw_yongsan_list_cbind, kw_yongsan_list[[i]])
}

## Monthly expected energy generate(36 months)
kw_yongsan_list_cbind <- cbind("용산구", kw_yongsan_list_cbind)
kw_yongsan_list_cbind



##### 22. Eunpyeong district
## Total expected energy generate(per year) : 15852810kWh
kw_eunpyeong = ((((bike_m2_dj[22,2]*bike_m2_dj[22,3]) / (0.27*0.27)) * 4/5) * 
                  (0.0625 * 0.1675 * (mean(df$Point.Forecast.21)*365) * 0.78)) +
  ((((bike_m2_dj[22,2]*bike_m2_dj[22,3]) / (0.27*0.27))  * 1/5) * 
     (0.0625 * 0.1675 * (mean(df$Point.Forecast.21)*365) * 0.81))   

## Total expected energy generate(per month)
# 4/5 of solar panels are shaded by buliding shadow, 1/5 are not shaded. This is reflected on a for sentence just below.
kw_eunpyeong_list <- list()
for(i in 1:36){
  kw_eunpyeong_list[[i]] <- ((((bike_m2_dj[22,2]*bike_m2_dj[22,3]) / (0.27*0.27)) * 4/5) * (0.0625 * 0.1675 * (mean(df$Point.Forecast.21[i])*30) * 0.78)) +
    ((((bike_m2_dj[22,2]*bike_m2_dj[22,3]) / (0.27*0.27))  * 1/5) * (0.0625 * 0.1675 * (mean(df$Point.Forecast.21[i])*30) * 0.81))    
}

kw_eunpyeong_list[[36]]
kw_eunpyeong_list_cbind <- cbind(kw_eunpyeong_list[[1]], kw_eunpyeong_list[[2]]) 

for(i in 3:36){
  kw_eunpyeong_list_cbind <- cbind(kw_eunpyeong_list_cbind, kw_eunpyeong_list[[i]])
}

## Monthly expected energy generate(36 months)
kw_eunpyeong_list_cbind <- cbind("은평구", kw_eunpyeong_list_cbind)
kw_eunpyeong_list_cbind



##### 23. Jongro district
## Total expected energy generate(per year) : 2064306kwh
kw_jongro = ((((bike_m2_dj[23,2]*bike_m2_dj[23,3]) / (0.27*0.27)) * 4/5) * 
               (0.0625 * 0.1675 * (mean(df$Point.Forecast.22)*365) * 0.78)) +
  ((((bike_m2_dj[23,2]*bike_m2_dj[23,3]) / (0.27*0.27))  * 1/5) * 
     (0.0625 * 0.1675 * (mean(df$Point.Forecast.22)*365) * 0.81))

## Total expected energy generate(per month)
# 4/5 of solar panels are shaded by buliding shadow, 1/5 are not shaded. This is reflected on a for sentence just below.
kw_jongro_list <- list()
for(i in 1:36){
  kw_jongro_list[[i]] <- ((((bike_m2_dj[23,2]*bike_m2_dj[23,3]) / (0.27*0.27)) * 4/5) * (0.0625 * 0.1675 * (mean(df$Point.Forecast.22[i])*30) * 0.78)) +
    ((((bike_m2_dj[23,2]*bike_m2_dj[23,3]) / (0.27*0.27))  * 1/5) * (0.0625 * 0.1675 * (mean(df$Point.Forecast.22[i])*30) * 0.81))    
}

kw_jongro_list[[36]]
kw_jongro_list_cbind <- cbind(kw_jongro_list[[1]], kw_jongro_list[[2]]) 

for(i in 3:36){
  kw_jongro_list_cbind <- cbind(kw_jongro_list_cbind, kw_jongro_list[[i]])
}

## Monthly expected energy generate(36 months)
kw_jongro_list_cbind <- cbind("종로구", kw_jongro_list_cbind)
kw_jongro_list_cbind



##### 24. Jung district 
## Total expected energy generate(per year) : 529825.4kwh
kw_junggu = ((((bike_m2_dj[24,2]*bike_m2_dj[24,3]) / (0.27*0.27)) * 1/2) * 
              (0.0625 * 0.1675 * (mean(df$Point.Forecast.23)*365) * 0.78)) +
  ((((bike_m2_dj[24,2]*bike_m2_dj[24,3]) / (0.27*0.27))  * 1/2) * 
     (0.0625 * 0.1675 * (mean(df$Point.Forecast.23)*365) * 0.81))

## Total expected energy generate(per month)
# 1/2 of solar panels are shaded by buliding shadow, 1/2 are not shaded. This is reflected on a for sentence just below.
kw_junggu_list <- list()
for(i in 1:36){
  kw_junggu_list[[i]] <- ((((bike_m2_dj[24,2]*bike_m2_dj[24,3]) / (0.27*0.27)) * 1/2) * (0.0625 * 0.1675 * (mean(df$Point.Forecast.23[i])*30) * 0.78)) +
    ((((bike_m2_dj[24,2]*bike_m2_dj[24,3]) / (0.27*0.27))  * 1/2) * (0.0625 * 0.1675 * (mean(df$Point.Forecast.23[i])*30) * 0.81))    
}

kw_junggu_list[[36]]
kw_junggu_list_cbind <- cbind(kw_junggu_list[[1]], kw_junggu_list[[2]]) 

for(i in 3:36){
  kw_junggu_list_cbind <- cbind(kw_junggu_list_cbind, kw_junggu_list[[i]])
}

## Monthly expected energy generate(36 months)
kw_junggu_list_cbind <- cbind("중구", kw_junggu_list_cbind)
kw_junggu_list_cbind



##### 25. Jungnang district
## Total expected energy generate(per year) : 4910902kwh
kw_jungnang = ((((bike_m2_dj[25,2]*bike_m2_dj[25,3]) / (0.27*0.27)) * 2/3) * 
                 (0.0625 * 0.1675 * (mean(df$Point.Forecast.24)*365) * 0.78)) +
  ((((bike_m2_dj[25,2]*bike_m2_dj[25,3]) / (0.27*0.27))  * 1/3) * 
     (0.0625 * 0.1675 * (mean(df$Point.Forecast.24)*365) * 0.81))

## Total expected energy generate(per month)
# 2/3 of solar panels are shaded by buliding shadow, 1/3 are not shaded. This is reflected on a for sentence just below.
kw_jungnang_list <- list()
for(i in 1:36){
  kw_jungnang_list[[i]] <- ((((bike_m2_dj[25,2]*bike_m2_dj[25,3]) / (0.27*0.27)) * 2/3) * (0.0625 * 0.1675 * (mean(df$Point.Forecast.24[i])*30) * 0.78)) +
    ((((bike_m2_dj[25,2]*bike_m2_dj[25,3]) / (0.27*0.27))  * 1/3) * (0.0625 * 0.1675 * (mean(df$Point.Forecast.24[i])*30) * 0.81))    
}

kw_jungnang_list[[36]]
kw_jungnang_list_cbind <- cbind(kw_jungnang_list[[1]], kw_jungnang_list[[2]]) 

for(i in 3:36){
  kw_jungnang_list_cbind <- cbind(kw_jungnang_list_cbind, kw_jungnang_list[[i]])
}

## Monthly expected energy generate(36 months)
kw_jungnang_list_cbind <- cbind("중랑구", kw_jungnang_list_cbind)
kw_jungnang_list_cbind




############ Make a csv file: Integrated monthly expected energy generate for 36 months of 25 districts in Seoul city.
kw_gu_total <- data.frame()
kw_gu_total <- rbind(kw_gu_total, kw_gn_list_cbind)
kw_gu_total <- rbind(kw_gu_total, kw_gd_list_cbind)
kw_gu_total <- rbind(kw_gu_total, kw_gb_list_cbind)
kw_gu_total <- rbind(kw_gu_total, kw_gs_list_cbind)
kw_gu_total <- rbind(kw_gu_total, kw_gw_list_cbind)
kw_gu_total <- rbind(kw_gu_total, kw_gj_list_cbind)
kw_gu_total <- rbind(kw_gu_total, kw_gr_list_cbind)
kw_gu_total <- rbind(kw_gu_total, kw_gc_list_cbind)
kw_gu_total <- rbind(kw_gu_total, kw_nw_list_cbind)
kw_gu_total <- rbind(kw_gu_total, kw_db_list_cbind)
kw_gu_total <- rbind(kw_gu_total, kw_dd_list_cbind)
kw_gu_total <- rbind(kw_gu_total, kw_dj_list_cbind)
kw_gu_total <- rbind(kw_gu_total, kw_mp_list_cbind)
kw_gu_total <- rbind(kw_gu_total, kw_sd_list_cbind)
kw_gu_total <- rbind(kw_gu_total, kw_sc_list_cbind)
kw_gu_total <- rbind(kw_gu_total, kw_seongdong_list_cbind)
kw_gu_total <- rbind(kw_gu_total, kw_sb_list_cbind)
kw_gu_total <- rbind(kw_gu_total, kw_songpa_list_cbind)
kw_gu_total <- rbind(kw_gu_total, kw_yangcheon_list_cbind)
kw_gu_total <- rbind(kw_gu_total, kw_yeongdeungpo_list_cbind)
kw_gu_total <- rbind(kw_gu_total, kw_yongsan_list_cbind)
kw_gu_total <- rbind(kw_gu_total, kw_eunpyeong_list_cbind)
kw_gu_total <- rbind(kw_gu_total, kw_jongro_list_cbind)
kw_gu_total <- rbind(kw_gu_total, kw_junggu_list_cbind)
kw_gu_total <- rbind(kw_gu_total, kw_jungnang_list_cbind)

View(kw_gu_total)

for(i in 2:37){
  j=i-1
  colnames(kw_gu_total)[i] <- paste0("gen_",j)
}

kw_gu_total<-as.data.frame(kw_gu_total)
class(kw_gu_total)
colnames(kw_gu_total)[1] <- "loc"


#columns informations: 
##  loc: Names of 25 districts of Seoul city.
##  gen_0, gen_1, ... gen_36: Monthly expected energy generate of each district(from 1st month ~ 36th month).
write.csv(kw_gu_total, "kw_gu_total.csv")