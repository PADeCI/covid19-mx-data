###########################################################
#            COVID-19 Mexico, Open Data from SSA          #
#                   Data Wrangle by State                 #
#                                                         #
# Authors:                                                #
#         Yadira Peralta                                  #
#         Regina Medina                                   # 
# April 2020                                              #
###########################################################

# This script loads raw data from the SSA, creates
# and saves data sets in the format we need.

#--------------------------------------------#
####             Libraries                ####  
#--------------------------------------------#

library(tidyverse)

#--------------------------------------------#
####            Load data                 #### 
#--------------------------------------------#

ssa <- read.csv("data-raw/200525COVID19MEXICO.csv")
load("data-raw/df_pop_state.Rdata")   # population 

#--------------------------------------------#
####     Population in 2020 by state      ####  
#--------------------------------------------#

df_pop_state_2020 <- df_pop_state %>%
  ungroup() %>%
  mutate(state = gsub("Yucatán", "Yucatan", state)) %>%
  mutate(state = gsub("National", "Mexico", state)) %>%
  group_by(state) %>%
  filter(year == 2020) %>%
  select(state, population)


#--------------------------------------------#
####      Working with SSA data           ####   
#--------------------------------------------#

# Check that database confirms numbers in the public report
table(ssa$RESULTADO)
# Oficial report: 71105 COVID cases, 29509 suspects
# In data Value 1: 71105; Value 3: 29509

# Keep important variables only from ssa data
ssa_data <- ssa %>%
  select(ENTIDAD_RES, MUNICIPIO_RES, FECHA_INGRESO, FECHA_SINTOMAS, 
         FECHA_DEF, EDAD, SEXO, TIPO_PACIENTE, INTUBADO, UCI, RESULTADO)

# Naming the values of the ENTIDAD_RES variable in english and spanish
ssa_data <- ssa_data %>%
  mutate(state = case_when(ENTIDAD_RES==1 ~ "Aguascalientes",
                           ENTIDAD_RES==2 ~ "Baja California",
                           ENTIDAD_RES==3 ~ "Baja California Sur",
                           ENTIDAD_RES==4 ~ "Campeche",
                           ENTIDAD_RES==5 ~ "Coahuila",
                           ENTIDAD_RES==6 ~ "Colima",
                           ENTIDAD_RES==7 ~ "Chiapas",
                           ENTIDAD_RES==8 ~ "Chihuahua",
                           ENTIDAD_RES==9 ~ "Mexico City",
                           ENTIDAD_RES==10 ~ "Durango",
                           ENTIDAD_RES==11 ~ "Guanajuato",
                           ENTIDAD_RES==12 ~ "Guerrero",
                           ENTIDAD_RES==13 ~ "Hidalgo",
                           ENTIDAD_RES==14 ~ "Jalisco",
                           ENTIDAD_RES==15 ~ "State of Mexico",
                           ENTIDAD_RES==16 ~ "Michoacan",
                           ENTIDAD_RES==17 ~ "Morelos",
                           ENTIDAD_RES==18 ~ "Nayarit",
                           ENTIDAD_RES==19 ~ "Nuevo Leon",
                           ENTIDAD_RES==20 ~ "Oaxaca",
                           ENTIDAD_RES==21 ~ "Puebla",
                           ENTIDAD_RES==22 ~ "Queretaro",
                           ENTIDAD_RES==23 ~ "Quintana Roo",
                           ENTIDAD_RES==24 ~ "San Luis Potosi",
                           ENTIDAD_RES==25 ~ "Sinaloa",
                           ENTIDAD_RES==26 ~ "Sonora",
                           ENTIDAD_RES==27 ~ "Tabasco",
                           ENTIDAD_RES==28 ~ "Tamaulipas",
                           ENTIDAD_RES==29 ~ "Tlaxcala",
                           ENTIDAD_RES==30 ~ "Veracruz",
                           ENTIDAD_RES==31 ~ "Yucatan",
                           ENTIDAD_RES==32 ~ "Zacatecas")) %>%
  mutate(entidad = case_when(ENTIDAD_RES==1 ~ "Aguascalientes",
                             ENTIDAD_RES==2 ~ "Baja California",
                             ENTIDAD_RES==3 ~ "Baja California Sur",
                             ENTIDAD_RES==4 ~ "Campeche",
                             ENTIDAD_RES==5 ~ "Coahuila",
                             ENTIDAD_RES==6 ~ "Colima",
                             ENTIDAD_RES==7 ~ "Chiapas",
                             ENTIDAD_RES==8 ~ "Chihuahua",
                             ENTIDAD_RES==9 ~ "Ciudad de México",
                             ENTIDAD_RES==10 ~ "Durango",
                             ENTIDAD_RES==11 ~ "Guanajuato",
                             ENTIDAD_RES==12 ~ "Guerrero",
                             ENTIDAD_RES==13 ~ "Hidalgo",
                             ENTIDAD_RES==14 ~ "Jalisco",
                             ENTIDAD_RES==15 ~ "Estado de México",
                             ENTIDAD_RES==16 ~ "Michoacán",
                             ENTIDAD_RES==17 ~ "Morelos",
                             ENTIDAD_RES==18 ~ "Nayarit",
                             ENTIDAD_RES==19 ~ "Nuevo León",
                             ENTIDAD_RES==20 ~ "Oaxaca",
                             ENTIDAD_RES==21 ~ "Puebla",
                             ENTIDAD_RES==22 ~ "Querétaro",
                             ENTIDAD_RES==23 ~ "Quintana Roo",
                             ENTIDAD_RES==24 ~ "San Luis Potosí",
                             ENTIDAD_RES==25 ~ "Sinaloa",
                             ENTIDAD_RES==26 ~ "Sonora",
                             ENTIDAD_RES==27 ~ "Tabasco",
                             ENTIDAD_RES==28 ~ "Tamaulipas",
                             ENTIDAD_RES==29 ~ "Tlaxcala",
                             ENTIDAD_RES==30 ~ "Veracruz",
                             ENTIDAD_RES==31 ~ "Yucatán",
                             ENTIDAD_RES==32 ~ "Zacatecas"))
# Check that cases are well-named (check with catalog as well)
table(ssa_data$ENTIDAD_RES)
table(ssa_data$state)
table(ssa_data$entidad)
 
# Create a variable = 1 if COVID-19 is positive
ssa_data$covid <- ifelse(ssa_data$RESULTADO == 1, 1, 0)
# There must be 71105 observations = 1
table(ssa_data$covid)

# Subset the data for COVID-19 cases only
ssa_covid <- subset(ssa_data, covid==1)

# Format date variables as.Date() and create date_dx and date_sx variables
table(ssa_covid$FECHA_INGRESO)
table(ssa_covid$FECHA_SINTOMAS)
table(ssa_covid$FECHA_DEF)
ssa_covid$date_dx <- as.Date(ssa_covid$FECHA_INGRESO, format = "%Y-%m-%d")
ssa_covid$date_sx <- as.Date(ssa_covid$FECHA_SINTOMAS, format = "%Y-%m-%d")
ssa_covid$date_dead <- as.Date(ssa_covid$FECHA_DEF, format = "%Y-%m-%d")
table(ssa_covid$date_dx)
table(ssa_covid$date_sx)
table(ssa_covid$date_dead)
str(ssa_covid)

# Create variable hosp_ind = 1 if patient was hospitalized
# Create variable icu_ind = 1 if patient required ICU
# Create variable vent_ind = 1 if patient was in ventilator
# Create variable dead_ind = 1 if patient died
ssa_covid <- ssa_covid %>%
  mutate(hosp_ind = ifelse(TIPO_PACIENTE == 2, 1, 0)) %>%
  mutate(icu_ind = ifelse(UCI == 1, 1, 0)) %>%
  mutate(vent_ind = ifelse(INTUBADO == 1, 1, 0)) %>%
  mutate(dead_ind = ifelse(is.na(date_dead), 0, 1))

# Check that all indicator variables are ok
table(ssa_covid$TIPO_PACIENTE) # 2 = hospitalizado
table(ssa_covid$hosp_ind) # check if %hosp = hosp_ind/covid
table(ssa_covid$UCI) # 1 = sí
table(ssa_covid$icu_ind)
table(ssa_covid$INTUBADO) # 1 = sí
table(ssa_covid$vent_ind)
table(ssa_covid$dead_ind) # Has to be the same as the official report

# Create a variable = 1 to count the number of test by state later
# and the date_dx variable in the complete data set from SSA
ssa_data$test_ind <- 1
table(ssa_data$FECHA_INGRESO)
ssa_data$date_dx <- as.Date(ssa_data$FECHA_INGRESO, format = "%Y-%m-%d")
table(ssa_data$date_dx)

# NOTA: Hay un registro con fecha 1969-12-31
#       Esto debe ser un error.
#       En las bases que usan ssa_data lo voy a omitir

# ACTUALIZACIÓN: El registro con fecha 1969-12-31 fue corregido 


#------------------------------------------------#
#### Data sets with output variables by state ####   
#------------------------------------------------#

# All data sets will have the following 3 variables:
# new_cases: number of new cases with var_outcome on that date
# cum_cases: cummulative number of cases of var_outcome up to that date
# time_cases: time (number of days where 0 = day of first var_outcome)

# Symptomatic observations grouped by (country, state, county) 
# and date_sx
sx_data <- ssa_covid %>% 
  mutate(country = "Mexico", 
         county = state,
         pais = "México",
         municipio = entidad) %>%
  group_by(country, state, county, pais, entidad, municipio, date_sx) %>%
  summarise(new_cases = sum(covid)) %>%
  complete(date_sx = seq.Date(min(date_sx), max(Sys.Date()), by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_sx)-1)),
         var_outcome = "Symptoms",
         var_resultado = "Síntomas") 

# Confirmed observations grouped by (country, state, county) 
# and date_dx
dx_data <- ssa_covid %>% 
  mutate(country = "Mexico", 
         county = state,
         pais = "México",
         municipio = entidad) %>%
  group_by(country, state, county, pais, entidad, municipio, date_dx) %>%
  summarise(new_cases = sum(covid)) %>%
  complete(date_dx = seq.Date(min(date_dx), max(Sys.Date()), by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_dx)-1)),
         var_outcome = "Confirmed",
         var_resultado = "Confirmados") 

# Hospitalized observations grouped by (country, state, county) 
# and date_dx (this date will be taken as the hospitalization date)
hosp_data <- ssa_covid %>% 
  mutate(country = "Mexico", 
         county = state,
         pais = "México",
         municipio = entidad) %>%
  group_by(country, state, county, pais, entidad, municipio, date_dx) %>%
  summarise(new_cases = sum(hosp_ind)) %>%
  complete(date_dx = seq.Date(min(date_dx), max(Sys.Date()), by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_dx)-1)),
         var_outcome = "Hospitalized",
         var_resultado = "Hospitalizados") 

# ICU observations grouped by (country, state, county) 
# and date_dx (this date will be taken as the ICU date)
icu_data <- ssa_covid %>% 
  mutate(country = "Mexico", 
         county = state,
         pais = "México",
         municipio = entidad) %>%
  group_by(country, state, county, pais, entidad, municipio, date_dx) %>%
  summarise(new_cases = sum(icu_ind)) %>%
  complete(date_dx = seq.Date(min(date_dx), max(Sys.Date()), by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_dx)-1)),
         var_outcome = "ICU",
         var_resultado = "UCI")

# Observations with ventilator grouped by (country, state, county) 
# and date_dx (this date will be taken as the ventilator date)
vent_data <- ssa_covid %>% 
  mutate(country = "Mexico", 
         county = state,
         pais = "México",
         municipio = entidad) %>%
  group_by(country, state, county, pais, entidad, municipio, date_dx) %>%
  summarise(new_cases = sum(vent_ind)) %>%
  complete(date_dx = seq.Date(min(date_dx), max(Sys.Date()), by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_dx)-1)),
         var_outcome = "Ventilator",
         var_resultado = "Intubado")

# Deaths grouped by (country, state, county) and date_dead
deaths_data <- ssa_covid %>% 
  mutate(country = "Mexico", 
         county = state,
         pais = "México",
         municipio = entidad) %>%
  filter(!is.na(date_dead)) %>%
  group_by(country, state, county, pais, entidad, municipio, date_dead) %>%
  summarise(new_cases = sum(dead_ind)) %>%
  complete(date_dead = seq.Date(min(date_dead), max(Sys.Date()), by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_dead)-1)),
         var_outcome = "Deaths",
         var_resultado = "Muertes")

# Number of tests grouped by (country, state, county) and date_dx
tests_data <- ssa_data %>% 
  # filter(date_dx != "1969-12-31") %>%  # Esta es la línea que agregué para omitir la observación con esta fecha
  mutate(country = "Mexico", 
         county = state,
         pais = "México",
         municipio = entidad) %>%
  group_by(country, state, county, pais, entidad, municipio, date_dx) %>%
  summarise(new_cases = sum(test_ind)) %>%
  complete(date_dx = seq.Date(min(date_dx), max(Sys.Date()), by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_dx)-1)),
         var_outcome = "Tests",
         var_resultado = "Pruebas")

# Just to double check we got the right numbers in everything
sum(sx_data$new_cases)
sum(dx_data$new_cases)
sum(hosp_data$new_cases)
sum(icu_data$new_cases)
sum(vent_data$new_cases)
sum(deaths_data$new_cases)
sum(tests_data$new_cases)

# Count number of observations
dim(sx_data)
dim(dx_data)
dim(hosp_data)
dim(icu_data)
dim(vent_data)
dim(deaths_data)
dim(tests_data)

#------------------------------------------------#
####    National data for output variables    ####   
#------------------------------------------------#

# Symptomatic observations 
sx_data_nal <- ssa_covid %>% 
  mutate(country = "Mexico", 
         state = "Mexico",
         county = "Mexico",
         pais = "México",
         entidad = "México",
         municipio = "México") %>%
  group_by(country, state, county, pais, entidad, municipio, date_sx) %>%
  summarise(new_cases = sum(covid)) %>%
  complete(date_sx = seq.Date(min(date_sx), max(Sys.Date()), by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_sx)-1)),
         var_outcome = "Symptoms",
         var_resultado = "Síntomas") 

# Confirmed observations
dx_data_nal <- ssa_covid %>% 
  mutate(country = "Mexico", 
         state = "Mexico",
         county = "Mexico",
         pais = "México",
         entidad = "México",
         municipio = "México") %>%
  group_by(country, state, county, pais, entidad, municipio, date_dx) %>%
  summarise(new_cases = sum(covid)) %>%
  complete(date_dx = seq.Date(min(date_dx), max(Sys.Date()), by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_dx)-1)),
         var_outcome = "Confirmed",
         var_resultado = "Confirmados") 

# Hospitalized observations 
hosp_data_nal <- ssa_covid %>% 
  mutate(country = "Mexico", 
         state = "Mexico",
         county = "Mexico",
         pais = "México",
         entidad = "México",
         municipio = "México") %>%
  group_by(country, state, county, pais, entidad, municipio, date_dx) %>%
  summarise(new_cases = sum(hosp_ind)) %>%
  complete(date_dx = seq.Date(min(date_dx), max(Sys.Date()), by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_dx)-1)),
         var_outcome = "Hospitalized",
         var_resultado = "Hospitalizados") 

# ICU observations 
icu_data_nal <- ssa_covid %>% 
  mutate(country = "Mexico", 
         state = "Mexico",
         county = "Mexico",
         pais = "México",
         entidad = "México",
         municipio = "México") %>%
  group_by(country, state, county, pais, entidad, municipio, date_dx) %>%
  summarise(new_cases = sum(icu_ind)) %>%
  complete(date_dx = seq.Date(min(date_dx), max(Sys.Date()), by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_dx)-1)),
         var_outcome = "ICU",
         var_resultado = "UCI")

# Observations with ventilator 
vent_data_nal <- ssa_covid %>% 
  mutate(country = "Mexico", 
         state = "Mexico",
         county = "Mexico",
         pais = "México",
         entidad = "México",
         municipio = "México") %>%
  group_by(country, state, county, pais, entidad, municipio, date_dx) %>%
  summarise(new_cases = sum(vent_ind)) %>%
  complete(date_dx = seq.Date(min(date_dx), max(Sys.Date()), by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_dx)-1)),
         var_outcome = "Ventilator",
         var_resultado = "Intubado")

# Deaths 
deaths_data_nal <- ssa_covid %>% 
  mutate(country = "Mexico", 
         state = "Mexico",
         county = "Mexico",
         pais = "México",
         entidad = "México",
         municipio = "México") %>%
  filter(!is.na(date_dead)) %>%
  group_by(country, state, county, pais, entidad, municipio, date_dead) %>%
  summarise(new_cases = sum(dead_ind)) %>%
  complete(date_dead = seq.Date(min(date_dead), max(Sys.Date()), by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_dead)-1)),
         var_outcome = "Deaths",
         var_resultado = "Muertes")

# Number of tests 
tests_data_nal <- ssa_data %>% 
  # filter(date_dx != "1969-12-31") %>%  # Esta es la línea que agregué para omitir la observación con esta fecha
  mutate(country = "Mexico", 
         state = "Mexico",
         county = "Mexico",
         pais = "México",
         entidad = "México",
         municipio = "México") %>%
  group_by(country, state, county, pais, entidad, municipio, date_dx) %>%
  summarise(new_cases = sum(test_ind)) %>%
  complete(date_dx = seq.Date(min(date_dx), max(Sys.Date()), by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_dx)-1)),
         var_outcome = "Tests",
         var_resultado = "Pruebas")

# Just to double check we got the right numbers in everything
sum(sx_data_nal$new_cases)
sum(dx_data_nal$new_cases)
sum(hosp_data_nal$new_cases)
sum(icu_data_nal$new_cases)
sum(vent_data_nal$new_cases)
sum(deaths_data_nal$new_cases)
sum(tests_data_nal$new_cases)

# Count number of observations
dim(sx_data_nal)
dim(dx_data_nal)
dim(hosp_data_nal)
dim(icu_data_nal)
dim(vent_data_nal)
dim(deaths_data_nal)
dim(tests_data_nal)


#------------------------------------------------#
####      Append data and add population      ####   
#------------------------------------------------#

# Symptomatic observations 
sx_full <- sx_data %>%
  bind_rows(sx_data_nal) %>%
  rename(date = date_sx) %>%
  left_join(df_pop_state_2020, 
            by = c("state" = "state")) %>%
  select(country, state, county, pais, entidad, municipio, 
         var_outcome, var_resultado, population, date,
         new_cases, cum_cases, time_cases)

# Confirmed observations 
dx_full <- dx_data %>%
  bind_rows(dx_data_nal) %>%
  rename(date = date_dx) %>%
  left_join(df_pop_state_2020, 
            by = c("state" = "state")) %>%
  select(country, state, county, pais, entidad, municipio, 
         var_outcome, var_resultado, population, date,
         new_cases, cum_cases, time_cases)

# Hospitalized observations 
hosp_full <- hosp_data %>%
  bind_rows(hosp_data_nal) %>%
  rename(date = date_dx) %>%
  left_join(df_pop_state_2020, 
            by = c("state" = "state")) %>%
  select(country, state, county, pais, entidad, municipio, 
         var_outcome, var_resultado, population, date,
         new_cases, cum_cases, time_cases)

# ICU observations 
icu_full <- icu_data %>%
  bind_rows(icu_data_nal) %>%
  rename(date = date_dx) %>%
  left_join(df_pop_state_2020, 
            by = c("state" = "state")) %>%
  select(country, state, county, pais, entidad, municipio, 
         var_outcome, var_resultado, population, date,
         new_cases, cum_cases, time_cases)

# Observations with ventilator 
vent_full <- vent_data %>%
  bind_rows(vent_data_nal) %>%
  rename(date = date_dx) %>%
  left_join(df_pop_state_2020, 
            by = c("state" = "state")) %>%
  select(country, state, county, pais, entidad, municipio, 
         var_outcome, var_resultado, population, date,
         new_cases, cum_cases, time_cases)

# Deaths 
deaths_full <- deaths_data %>%
  bind_rows(deaths_data_nal) %>%
  rename(date = date_dead) %>%
  left_join(df_pop_state_2020, 
            by = c("state" = "state")) %>%
  select(country, state, county, pais, entidad, municipio, 
         var_outcome, var_resultado, population, date,
         new_cases, cum_cases, time_cases)

# Number of tests 
tests_full <- tests_data %>%
  bind_rows(tests_data_nal) %>%
  rename(date = date_dx) %>%
  left_join(df_pop_state_2020, 
            by = c("state" = "state")) %>%
  select(country, state, county, pais, entidad, municipio, 
         var_outcome, var_resultado, population, date,
         new_cases, cum_cases, time_cases)


#--------------------------------------------#
####             Final data set           ####  
#--------------------------------------------#

# Append all 7 complete data sets
df_covid_ssa_state <- sx_full %>%
  bind_rows(dx_full) %>%
  bind_rows(hosp_full) %>%
  bind_rows(icu_full) %>%
  bind_rows(vent_full) %>%
  bind_rows(deaths_full) %>%
  bind_rows(tests_full) 

# Add date stamp to data set
#Antención aquí
df_covid_ssa_state$time_stamp <- Sys.Date()
#df_covid_ssa_state$time_stamp <- "2020-05-16"


#--------------------------------------------#
####               Save data              ####  
#--------------------------------------------#

# Save complete and most updated data file 
save(df_covid_ssa_state,
     file = "data/state/df_covid_ssa_state.Rdata")

# Save file in csv format
write.csv(df_covid_ssa_state, paste0("data/state/covid_ssa_state_",Sys.Date(),".csv"),
          row.names = FALSE)

# write.csv(df_covid_ssa_state, "data/state/covid_ssa_state_2020-05-16.csv",
#            row.names = FALSE)

# Another option to save the file (just in case accents are not shown)
#write.table(df_covid_ssa_state, paste0("data/state/covid_ssa_state_",Sys.Date(),".csv"),
#  row.names = FALSE, sep = ",")










