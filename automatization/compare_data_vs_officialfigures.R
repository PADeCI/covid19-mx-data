##************************************************************************
## Script Name: 
## Purpose:         
## 
##
## Created:                               
## Authors: Mariana Fernández 
##          
## GitHub: marianafdz465  
##
##
##************************************************************************


#--------------------------------------------#
####             Libraries                ####  
#--------------------------------------------#
library("XLConnect")



#--------------------------------------------#
####             Read xlsx                ####  
#--------------------------------------------#
wb <- loadWorkbook("data-validation/bitacora_historica_datos_abiertos.xlsx")
#ssa <- fread("data/state/covid_ssa_state_2020-07-17.csv",header=TRUE)


#--------------------------------------------#
####        Append new data              ####  
#--------------------------------------------#

date <- Sys.Date()
#date <- as.Date("2020-10-17")
date <- format(date, format="%Y-%m-%d")


#Confirmados, Negativos y Sospechosos
resultado <- table(ssa$CLASIFICACION_FINAL)
resultado <- as.vector(resultado)
confirmados <- resultado[1] + resultado[2] + resultado[3]
negativos <- resultado[7]
sospechosos <- resultado[4] +resultado[5] + resultado[6]


#Hospitalizados
indicator_hosp <- table(ssa_covid$TIPO_PACIENTE) # 2 = hospitalizado
indicator_hosp <- as.vector(indicator_hosp)
hospitalizados <- indicator_hosp[2]


#Porcentaje hospitalizados
perc_hosp <- (hospitalizados / confirmados) * 100
table(ssa_covid$hosp_ind) # check if %hosp = hosp_ind/covid

#UCI
indicator_uci <- table(ssa_covid$UCI)# 1 = sí
indicator_uci <- as.vector(indicator_uci)
uci <- indicator_uci[1]

#Check UCI
table(ssa_covid$icu_ind)

#Intubado
indicator_intu <- table(ssa_covid$INTUBADO) # 1 = sí
indicator_intu <- as.vector(indicator_intu)
intubados <- indicator_intu[1]

#Check intubados
table(ssa_covid$vent_ind)

#Dead
indicator_deaths <- table(ssa_covid$dead_ind) # Has to be the same as the official report


indicator_deaths <- as.vector(indicator_deaths)
deaths <- indicator_deaths[2]

#Pruebas en base
tests <- sum(tests_data$new_cases)



#--------------------------------------------#
####   Test to check the consistency  ####  
#--------------------------------------------#

# Just to double check we got the right numbers in everything

#Confirmados
ifelse(sum(dx_data$new_cases) == confirmados,"COINCIDE", "NO COINCIDE") 

#Hospitalizados
ifelse(sum(hosp_data$new_cases) == hospitalizados, "COINCIDE", "NO COINCIDE")

#UCI
ifelse(sum(icu_data$new_cases) == uci, "COINCIDE", "NO COINCIDE")


#Intubados
ifelse(sum(vent_data$new_cases) == intubados, "COINCIDE", "NO COINCIDE" )


#Muertes
ifelse(sum(deaths_data$new_cases) == deaths,"COINCIDE", "NO COINCIDE" )

#Tests
ifelse(sum(tests_data$new_cases) == tests,"COINCIDE", "NO COINCIDE" )



#--------------------------------------------#
####Create Data Frame and append new data ####  
#--------------------------------------------#
data_fr <- data.frame(date,
                      confirmados = sum(dx_data$new_cases), 
                      negativos, 
                      sospechosos,
                      deaths = sum(deaths_data$new_cases),
                      perc_hosp,
                      hospitalizados = sum(hosp_data$new_cases), 
                      uci = sum(icu_data$new_cases), 
                      intubados = sum(vent_data$new_cases), 
                      tests = sum(tests_data$new_cases))
appendWorksheet(wb, data_fr, sheet = 1)
saveWorkbook(wb)





#--------------------------------------------#
####   Fill the notebook  ####  
#--------------------------------------------#
table(ssa_covid$date_dx)
table(ssa_covid$date_sx)
table(ssa_covid$date_dead)


# Count number of observations
dim(sx_data)
dim(dx_data)
dim(hosp_data)
dim(icu_data)
dim(vent_data)
dim(deaths_data)
dim(tests_data)

# Count number of observations
dim(sx_data_nal)
dim(dx_data_nal)
dim(hosp_data_nal)
dim(icu_data_nal)
dim(vent_data_nal)
dim(deaths_data_nal)
dim(tests_data_nal)

dim(df_covid_ssa_ZMVM)








