  #*******************************************************************# 
  # This script loads, formats, and tabulates data from Mexico        #
  #                                                                   #
  # Depends on:                                                       #
  #   Author: Manuel Cardona                                          #
  #   E-mail: mcardona@poverty-action.org                             #
  #   Date: Apr 30, 2020                                              # 
  #   Last edit: May 02, 2020                                         #
  #*******************************************************************# 
  
  # *****************************************************************************
  #### 01_Load libraries and working space ####
  # *****************************************************************************
  rm(list = ls()) # to clean the workspace
  
  
  library(tidyverse)
  library(lubridate)
  library(ggplot2)
  library(dplyr)
  library(knitr)
  library(magrittr)
  library(formattable)
  library(htmltools)
  library(webshot)
  library(extrafont)
  library(kableExtra)
  
  font_import(paths = "C:/Users/Manuel Cardona Arias/Downloads/Open_Sans/",prompt = F)
  
  
  # *****************************************************************************
  #### 02_Arrange_data: DX Confirmed Cases per 100,000 habs ####
  # *****************************************************************************
    
    load("~/GitHub/covid19-mx-nacional/data/state/df_covid_ssa_state.Rdata")
    state <- df_covid_ssa_state
    state <- state[c(4,5,8,9,10,12,13)]
    
    state_dX <- subset(state, var_resultado == "Confirmados")
    states <- state_dX$entidad
    state_dX$entidad[state_dX$entidad == "México"] <- "Nacional"
    
    #Confirmed cases rate (Confirmed cases/ Population * 100,000)
    state_dX$DX_rate <- (state_dX$cum_cases/state_dX$population)*100000
    
    state_dX <-state_dX %>%
      group_by(entidad)%>%
      mutate(last_case = max(time_cases))
    
    state_dX$end_label <- case_when(state_dX$time_cases == state_dX$last_case ~ state_dX$entidad) 
     
    state_dX_copy <- state_dX[complete.cases(state_dX), ]
    state_dX_copy <- state_dX_copy[c(2,6,8)]
    state_dX_copy$DX_rate<-round(state_dX_copy$DX_rate, digits = 2)
    
    state_dX_copy <- state_dX_copy[order(-state_dX_copy$cum_cases),]
    state_dX_copy<-rename(state_dX_copy, Entidad = entidad)
    state_dX_copy<-rename(state_dX_copy, "Número de casos confirmados acumulados (NCCA)" = cum_cases)
    state_dX_copy<-rename(state_dX_copy, "Tasa de incidencia acumulada (TIA)" = DX_rate)
    
    state_dX_copy$`Tasa de incidencia acumulada (TIA)`<-round(state_dX_copy$`Tasa de incidencia acumulada (TIA)`, digits = 0)
    
    state_dX_copy$`Número de casos confirmados acumulados (NCCA)`[state_dX_copy$Entidad=="Nacional"]<-NA_character_
    state_dX_copy<-state_dX_copy[complete.cases(state_dX_copy),]
    
    state_dX_copy$rank_dx<-seq(1, length(state_dX_copy$`Número de casos confirmados acumulados (NCCA)`))
    
    state_dX_copy <- state_dX_copy[order(-state_dX_copy$`Tasa de incidencia acumulada (TIA)`),]
    state_dX_copy$rank_tasa<-seq(1, length(state_dX_copy$`Número de casos confirmados acumulados (NCCA)`))
  
    state_dX_copy <- state_dX_copy[order(state_dX_copy$rank_dx),]
    
    state_dX_copy<-rename(state_dX_copy, "Posición (NCCA)" = rank_dx)
    state_dX_copy<-rename(state_dX_copy, "Posición (TIA)" = rank_tasa) 
    
    state_dX_copy<-state_dX_copy[c(1,2,4,5,3)]
    
  
# *****************************************************************************************
#### 03_Tabulate_data: DX Confirmed Cases and Rate of confirmed cases per 100,000 habs ####
# *****************************************************************************************
formattable(state_dX_copy,
            align = c("c", "r", "c", "c", "l"),
            list(
  area(col = "Número de casos confirmados acumulados (NCCA)") ~ normalize_bar("#9bbcd4", 0.2),
  area(col = "Tasa de incidencia acumulada (TIA)") ~ normalize_bar("#c994c7", 0.2)
))
  
# ********************************************************************************************************************
#### 04_Tabulate_data: DX Confirmed Cases and Rate of confirmed cases per 100,000 habs, for a selection of states ####
# ********************************************************************************************************************
state_dX_copy<-subset(state_dX_copy, state_dX_copy$Entidad=="Ciudad de México" | state_dX_copy$Entidad=="Aguascalientes" | state_dX_copy$Entidad=="Zacatecas" | state_dX_copy$Entidad=="Jalisco" | state_dX_copy$Entidad=="Puebla" | state_dX_copy$Entidad=="Guanajuato",
                      select = c(`Entidad`, `Número de casos confirmados acumulados (NCCA)`, `Posición (NCCA)`, `Posición (TIA)`, `Tasa de incidencia acumulada (TIA)`))

  
  formattable(state_dX_copy[1:5],
              align = c("c", "r", "c", "c", "l"),
              list(
                area(col = "Número de casos confirmados acumulados (NCCA)") ~ normalize_bar("#9bbcd4", 0.2),
                area(col = "Tasa de incidencia acumulada (TIA)") ~ normalize_bar("#c994c7", 0.2)
              ))
  
  
  # *****************************************************************************
  #### 05_Arrange_data: Tests rate and positivity rate ####
  # *****************************************************************************
  
  load("~/GitHub/covid19-mx-nacional/data/state/df_covid_ssa_state.Rdata")
  state <- df_covid_ssa_state
  state <- state[c(4,5,8,9,10,12,13)]
  
  state_DX <- subset(state, var_resultado == "Confirmados")
  state_DX$entidad[state_DX$entidad == "México"] <- "Nacional"
  
  state_test <- subset(state, var_resultado == "Pruebas")
  state_test$entidad[state_test$entidad == "México"] <- "Nacional"
  
  state_F <- merge(state_DX, state_test, by=c("entidad", "date"))
  remove(state_DX)
  remove(state_test)
  state_F <- state_F[c(1,2,5,6,7,11,12)]
  
  state_F<-rename(state_F, population = population.x)
  state_F<-rename(state_F, "Número de casos confirmados (NCC)" = cum_cases.x)
  state_F<-rename(state_F, "Número de pruebas (NP)" = cum_cases.y)
  
  state_F <- state_F %>%
    group_by(entidad) %>%
    mutate(last_case = max(time_cases.x))
  
  state_F$end_label <- case_when(state_F$time_cases.x == state_F$last_case ~ state_F$entidad) 
  state_F <- state_F[complete.cases(state_F), ]
  

  #Tests rate (Tests/ Population * 100,000)
  state_F$test_rate <- (state_F$`Número de pruebas (NP)`/state_F$population)*100000
  
  #Positivity rate (Confirmed cases/Number of Tests * 100)
  state_F$positivity_rate <- ((state_F$`Número de casos confirmados (NCC)`/state_F$`Número de pruebas (NP)`)*100)
  
  state_F <- state_F[c(1, 10, 11)]
  
  state_F$test_rate<-round(state_F$test_rate, digits = 2)
  state_F$positivity_rate<-round(state_F$positivity_rate, digits = 2)
  
  #Create rankings 
  state_F <- state_F[order(-state_F$test_rate),]
  state_F<-rename(state_F, Entidad = entidad)
  state_F<-rename(state_F, "Tasa de pruebas acumulada (TPA)" = test_rate)
  state_F<-rename(state_F, "Tasa de positividad acumulada (TPosA)" = positivity_rate)
  
  state_F$`Tasa de pruebas acumulada (TPA)`<-round(state_F$`Tasa de pruebas acumulada (TPA)`, digits = 0)
  state_F$`Tasa de positividad acumulada (TPosA)`<-round(state_F$`Tasa de positividad acumulada (TPosA)`, digits = 0)
  
  state_F$`Tasa de pruebas acumulada (TPA)`[state_F$Entidad=="Nacional"]<-NA_character_
  state_F<-state_F[complete.cases(state_F),]
  
  state_F$rank_test<-seq(1, length(state_F$`Tasa de pruebas acumulada (TPA)`))
  
  state_F <- state_F[order(-state_F$`Tasa de positividad acumulada (TPosA)`),]
  state_F$rank_positivity<-seq(1, length(state_F$`Tasa de positividad acumulada (TPosA)`))
  
  state_F <- state_F[order(state_F$rank_test),]
  
  state_F<-rename(state_F, "Posición (TPA)" = rank_test)
  state_F<-rename(state_F, "Posición (TPosA)" = rank_positivity) 
  
  state_F<-state_F[c(1,2,4,5,3)]
  
  
  # *****************************************************************************************
  #### 06_Tabulate_data: Rate of tests and rate of positivity ####
  # *****************************************************************************************
  formattable(state_F,
              align = c("c", "r", "c", "c", "l"),
              list(
                area(col = "Tasa de pruebas acumulada (TPA)") ~ normalize_bar("#9bbcd4", 0.2),
                area(col = "Tasa de positividad acumulada (TPosA)") ~ normalize_bar("#c994c7", 0.2)
              ))
  
  # ********************************************************************************************************************
  #### 04_Tabulate_data: DX Confirmed Cases and Rate of confirmed cases per 100,000 habs, for a selection of states ####
  # ********************************************************************************************************************
  state_F<-subset(state_F, state_F$Entidad=="Ciudad de México" | state_F$Entidad=="Aguascalientes" | state_F$Entidad=="Zacatecas" | state_F$Entidad=="Jalisco" | state_F$Entidad=="Puebla" | state_F$Entidad=="Guanajuato",
                        select = c(`Entidad`, `Tasa de pruebas acumulada (TPA)`, `Posición (TPA)`, `Posición (TPosA)`, `Tasa de positividad acumulada (TPosA)`))
  
  
  formattable(state_F[1:5],
              align = c("c", "r", "c", "c", "l"),
              list(
                area(col = "Tasa de pruebas acumulada (TPA)") ~ normalize_bar("#9bbcd4", 0.2),
                area(col = "Tasa de positividad acumulada (TPosA)") ~ normalize_bar("#c994c7", 0.2)
              ))
  
  