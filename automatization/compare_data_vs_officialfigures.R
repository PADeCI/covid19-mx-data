###########################################################
#            COVID-19 Mexico, Open Data from SSA          #
#           Data                                          #
#                                                         #
# Authors:                                                #
#         PaDeCI                                          #
#         marianafdz465                                   #
#                                                         # 
# May   2020                                              #
###########################################################



#--------------------------------------------#
####             Libraries                ####  
#--------------------------------------------#
library("XLConnect")



#--------------------------------------------#
####             Read xlsx                ####  
#--------------------------------------------#
wb <- loadWorkbook("/Users/marianafernandez/Documents/COSMO/covid19-mx-data/data-raw/base3.xlsx")


#--------------------------------------------#
####        Append new data              ####  
#--------------------------------------------#

date <- Sys.Date()
date <- format(date, format="%Y-%m-%d")


#Confirmados, Negativos y Sospechosos
resultado <- table(ssa$RESULTADO)
resultado <- as.vector(resultado)
confirmados <- resultado[1]
negativos <- resultado[2]
sospechosos <- resultado[3]

                         
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
####   Fill the notebook  ####  
#--------------------------------------------#



                         
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
data_fr <- data.frame(date, confirmados, negativos, sospechosos, deaths,perc_hosp,hospitalizados, uci, intubados, tests)
appendWorksheet(wb, data_fr, sheet = 1)
saveWorkbook(wb)










