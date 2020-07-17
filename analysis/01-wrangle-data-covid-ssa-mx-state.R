###########################################################
#            COVID-19 Mexico, Open Data from SSA          #
#           Data Wrangle by State and adding ZMVM         #
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

ssa <- read.csv("data-raw/200716COVID19MEXICO.csv")
load("data-raw/df_pop_state.Rdata")   # population for states
# Data for ZMVM
load("data-raw/df_pop_ZMVM.Rdata")   
load("data-raw/df_pop_county.Rdata") 

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
table(ssa_data$covid)

# Subset the data for COVID-19 cases only
ssa_covid <- subset(ssa_data, covid==1)

# Format date variables as.Date() and create date_dx and date_sx variables
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
ssa_data$date_dx <- as.Date(ssa_data$FECHA_INGRESO, format = "%Y-%m-%d")
table(ssa_data$date_dx)


#------------------------------------------------#
#### Data sets with output variables by state ####   
#------------------------------------------------#

# All data sets will have the following 3 variables:
# new_cases: number of new cases with var_outcome on that date
# cum_cases: cummulative number of cases of var_outcome up to that date
# time_cases: time (number of days where 0 = day of first var_outcome)

# Date until which we create the sequence
max_date <- Sys.Date()
#max_date <- as.Date("2020-06-27")

# Symptomatic observations grouped by (country, state, county) 
# and date_sx
sx_data <- ssa_covid %>% 
  mutate(country = "Mexico", 
         county = state,
         pais = "México",
         municipio = entidad) %>%
  group_by(country, state, county, pais, entidad, municipio, date_sx) %>%
  summarise(new_cases = sum(covid)) %>%
  complete(date_sx = seq.Date(min(date_sx), max_date, by="day"), # Create a sequence of dates
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
  complete(date_dx = seq.Date(min(date_dx), max_date, by="day"), # Create a sequence of dates
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
  complete(date_dx = seq.Date(min(date_dx), max_date, by="day"), # Create a sequence of dates
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
  complete(date_dx = seq.Date(min(date_dx), max_date, by="day"), # Create a sequence of dates
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
  complete(date_dx = seq.Date(min(date_dx), max_date, by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_dx)-1)),
         var_outcome = "Ventilator",
         var_resultado = "Intubados")

# Deaths grouped by (country, state, county) and date_dead
deaths_data <- ssa_covid %>% 
  mutate(country = "Mexico", 
         county = state,
         pais = "México",
         municipio = entidad) %>%
  filter(!is.na(date_dead)) %>%
  group_by(country, state, county, pais, entidad, municipio, date_dead) %>%
  summarise(new_cases = sum(dead_ind)) %>%
  complete(date_dead = seq.Date(min(date_dead), max_date, by="day"), # Create a sequence of dates
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
  complete(date_dx = seq.Date(min(date_dx), max_date, by="day"), # Create a sequence of dates
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
         entidad = "Nacional",
         municipio = "Nacional") %>%
  group_by(country, state, county, pais, entidad, municipio, date_sx) %>%
  summarise(new_cases = sum(covid)) %>%
  complete(date_sx = seq.Date(min(date_sx), max_date, by="day"), # Create a sequence of dates
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
         entidad = "Nacional",
         municipio = "Nacional") %>%
  group_by(country, state, county, pais, entidad, municipio, date_dx) %>%
  summarise(new_cases = sum(covid)) %>%
  complete(date_dx = seq.Date(min(date_dx), max_date, by="day"), # Create a sequence of dates
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
         entidad = "Nacional",
         municipio = "Nacional") %>%
  group_by(country, state, county, pais, entidad, municipio, date_dx) %>%
  summarise(new_cases = sum(hosp_ind)) %>%
  complete(date_dx = seq.Date(min(date_dx), max_date, by="day"), # Create a sequence of dates
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
         entidad = "Nacional",
         municipio = "Nacional") %>%
  group_by(country, state, county, pais, entidad, municipio, date_dx) %>%
  summarise(new_cases = sum(icu_ind)) %>%
  complete(date_dx = seq.Date(min(date_dx), max_date, by="day"), # Create a sequence of dates
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
         entidad = "Nacional",
         municipio = "Nacional") %>%
  group_by(country, state, county, pais, entidad, municipio, date_dx) %>%
  summarise(new_cases = sum(vent_ind)) %>%
  complete(date_dx = seq.Date(min(date_dx), max_date, by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_dx)-1)),
         var_outcome = "Ventilator",
         var_resultado = "Intubados")

# Deaths 
deaths_data_nal <- ssa_covid %>% 
  mutate(country = "Mexico", 
         state = "Mexico",
         county = "Mexico",
         pais = "México",
         entidad = "Nacional",
         municipio = "Nacional") %>%
  filter(!is.na(date_dead)) %>%
  group_by(country, state, county, pais, entidad, municipio, date_dead) %>%
  summarise(new_cases = sum(dead_ind)) %>%
  complete(date_dead = seq.Date(min(date_dead), max_date, by="day"), # Create a sequence of dates
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
  complete(date_dx = seq.Date(min(date_dx), max_date, by="day"), # Create a sequence of dates
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
####    Final data set for the states     ####  
#--------------------------------------------#

# Append all 7 complete data sets
df_covid_ssa_state <- sx_full %>%
  bind_rows(dx_full) %>%
  bind_rows(hosp_full) %>%
  bind_rows(icu_full) %>%
  bind_rows(vent_full) %>%
  bind_rows(deaths_full) %>%
  bind_rows(tests_full) 


#------------------------------------------------------------------#
#------------------------------------------------------------------#

#----------------------------------------------#
#### Paste counties and population for ZMVM ####   
#----------------------------------------------#

df_pop_county <- df_pop_county %>%
  mutate(county_id = as.numeric(county_id))

ssa_covid <- ssa_covid %>% 
  mutate(county_id = formatC(MUNICIPIO_RES, width = 3, flag = "0"), 
         state_id = formatC(ENTIDAD_RES, width = 2, flag = "0"), 
         county_id = paste0(state_id, county_id), 
         county_id= as.numeric(county_id)) %>%
  left_join(df_pop_county, by = c("county_id" = "county_id")) %>%
  rename(entidad = "entidad.x")

ssa_data <- ssa_data %>% 
  mutate(county_id = formatC(MUNICIPIO_RES, width = 3, flag = "0"), 
         state_id = formatC(ENTIDAD_RES, width = 2, flag = "0"), 
         county_id = paste0(state_id, county_id), 
         county_id= as.numeric(county_id)) %>%
  left_join(df_pop_county, by = c("county_id" = "county_id")) %>%
  rename(entidad = "entidad.x")


#--------------------------------------------#
####       Create the ZMVM category       ####   
#--------------------------------------------#

# Acronyms:
# Zona Metropolitana del Valle de México (ZMVM)
# Mexico City Metropolitan Area (MCMA)

# Vector of ZMVM  county ids
v_zmvm_id <- c("9002", "9003", "9004", "9005", "9006",
               "9007", "9008", "9009", "9010", "9011",
               "9012", "9013", "9014", "9015", "9016",
               "9017", "13069",
               "15002", "15009", "15010", "15011",
               "15013", "15015", "15016", "15017", 
               "15020", "15022", "15023", "15024", 
               "15025", "15028", "15029", "15030", 
               "15031", "15033", "15034", "15035", 
               "15036", "15037", "15038", "15039", 
               "15044", "15046", "15050", "15053",
               "15057", "15058", "15059", "15060",
               "15061", "15065", "15068", "15069",
               "15070", "15075", "15081", "15083",
               "15084", "15089", "15091", "15092",
               "15093", "15094", "15095", "15096",
               "15099", "15100", "15103", "15104",
               "15108", "15109", "15112", "15120",
               "15121", "15122", "15125")

# Create dummy for each county in ZMVM and get a subset of the
# data for ZMVM only
ssa_covid_ZMVM <- ssa_covid %>%
  mutate(zmvm = ifelse(county_id %in% v_zmvm_id, 1, 0)) %>%
  filter(zmvm == 1) %>%
  mutate(entidad = case_when(zmvm == 1 ~ "ZMVM"), 
         state = case_when(zmvm == 1 ~ "MCMA"), 
         county_name_esp = case_when(zmvm == 1 ~ "ZMVM"), 
         county_name_eng = case_when(zmvm == 1 ~ "MCMA"))

ssa_data_ZMVM <- ssa_data %>%
  mutate(zmvm = ifelse(county_id %in% v_zmvm_id, 1, 0)) %>%
  filter(zmvm == 1) %>%
  mutate(entidad = case_when(zmvm == 1 ~ "ZMVM"), 
         state = case_when(zmvm == 1 ~ "MCMA"), 
         county_name_esp = case_when(zmvm == 1 ~ "ZMVM"), 
         county_name_eng = case_when(zmvm == 1 ~ "MCMA"))

# Compare population 
sum(unique(ssa_covid_ZMVM$population))
sum(unique(ssa_data_ZMVM$population))


#------------------------------------------------#
####           Add ZMVM population            ####   
#------------------------------------------------#
df_pop_ZMVM <- df_pop_ZMVM %>%
  select(entidad, population) 

ssa_covid_ZMVM <- ssa_covid_ZMVM %>%
  left_join(df_pop_ZMVM, by = "entidad") %>%
  rename(population = "population.y")

ssa_data_ZMVM <- ssa_data_ZMVM %>%
  left_join(df_pop_ZMVM, by = "entidad") %>%
  rename(population = "population.y")


#------------------------------------------------#
####      ZMVM data for output variables      ####   
#------------------------------------------------#

# Symptomatic observations 
sx_data_ZMVM <- ssa_covid_ZMVM %>% 
  mutate(country = "Mexico", 
         state = "MCMA",
         county = "MCMA",
         pais = "México",
         entidad = "ZMVM",
         municipio = "ZMVM") %>%
  group_by(country, state, county, pais, entidad, municipio, population, date_sx) %>%
  summarise(new_cases = sum(covid)) %>%
  complete(date_sx = seq.Date(min(date_sx), max_date, by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_sx)-1)),
         var_outcome = "Symptoms",
         var_resultado = "Síntomas") 

# Confirmed observations
dx_data_ZMVM <- ssa_covid_ZMVM %>% 
  mutate(country = "Mexico", 
         state = "MCMA",
         county = "MCMA",
         pais = "México",
         entidad = "ZMVM",
         municipio = "ZMVM") %>%
  group_by(country, state, county, pais, entidad, municipio, population, date_dx) %>%
  summarise(new_cases = sum(covid)) %>%
  complete(date_dx = seq.Date(min(date_dx), max_date, by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_dx)-1)),
         var_outcome = "Confirmed",
         var_resultado = "Confirmados") 

# Hospitalized observations 
hosp_data_ZMVM <- ssa_covid_ZMVM %>% 
  mutate(country = "Mexico", 
         state = "MCMA",
         county = "MCMA",
         pais = "México",
         entidad = "ZMVM",
         municipio = "ZMVM") %>%
  group_by(country, state, county, pais, entidad, municipio, population, date_dx) %>%
  summarise(new_cases = sum(hosp_ind)) %>%
  complete(date_dx = seq.Date(min(date_dx), max_date, by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_dx)-1)),
         var_outcome = "Hospitalized",
         var_resultado = "Hospitalizados") 

# ICU observations 
icu_data_ZMVM <- ssa_covid_ZMVM %>% 
  mutate(country = "Mexico", 
         state = "MCMA",
         county = "MCMA",
         pais = "México",
         entidad = "ZMVM",
         municipio = "ZMVM") %>%
  group_by(country, state, county, pais, entidad, municipio, population, date_dx) %>%
  summarise(new_cases = sum(icu_ind)) %>%
  complete(date_dx = seq.Date(min(date_dx), max_date, by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_dx)-1)),
         var_outcome = "ICU",
         var_resultado = "UCI")

# Observations with ventilator 
vent_data_ZMVM <- ssa_covid_ZMVM %>% 
  mutate(country = "Mexico", 
         state = "MCMA",
         county = "MCMA",
         pais = "México",
         entidad = "ZMVM",
         municipio = "ZMVM") %>%
  group_by(country, state, county, pais, entidad, municipio, population, date_dx) %>%
  summarise(new_cases = sum(vent_ind)) %>%
  complete(date_dx = seq.Date(min(date_dx), max_date, by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_dx)-1)),
         var_outcome = "Ventilator",
         var_resultado = "Intubados")

# Deaths 
deaths_data_ZMVM <- ssa_covid_ZMVM %>% 
  mutate(country = "Mexico", 
         state = "MCMA",
         county = "MCMA",
         pais = "México",
         entidad = "ZMVM",
         municipio = "ZMVM") %>%
  filter(!is.na(date_dead)) %>%
  group_by(country, state, county, pais, entidad, municipio, population, date_dead) %>%
  summarise(new_cases = sum(dead_ind)) %>%
  complete(date_dead = seq.Date(min(date_dead), max_date, by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_dead)-1)),
         var_outcome = "Deaths",
         var_resultado = "Muertes")

# Number of tests 
tests_data_ZMVM <- ssa_data_ZMVM %>% 
  mutate(country = "Mexico", 
         state = "MCMA",
         county = "MCMA",
         pais = "México",
         entidad = "ZMVM",
         municipio = "ZMVM") %>%
  group_by(country, state, county, pais, entidad, municipio, population, date_dx) %>%
  summarise(new_cases = sum(test_ind)) %>%
  complete(date_dx = seq.Date(min(date_dx), max_date, by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_dx)-1)),
         var_outcome = "Tests",
         var_resultado = "Pruebas")


#------------------------------------------------#
####  Clean and arrange data to be appended   ####   
#------------------------------------------------#

# Symptomatic observations 
sx_ZMVM <- sx_data_ZMVM %>%
  rename(date = date_sx) %>%
  select(country, state, county, pais, entidad, municipio, 
         var_outcome, var_resultado, population, 
         date, new_cases, cum_cases, time_cases)

# Confirmed observations 
dx_ZMVM <- dx_data_ZMVM %>%
  rename(date = date_dx) %>%
  select(country, state, county, pais, entidad, municipio, 
         var_outcome, var_resultado, population, 
         date, new_cases, cum_cases, time_cases)

# Hospitalized observations 
hosp_ZMVM <- hosp_data_ZMVM %>%
  rename(date = date_dx) %>%
  select(country, state, county, pais, entidad, municipio, 
         var_outcome, var_resultado, population, 
         date, new_cases, cum_cases, time_cases)

# ICU observations 
icu_ZMVM <- icu_data_ZMVM %>%
  rename(date = date_dx) %>%
  select(country, state, county, pais, entidad, municipio, 
         var_outcome, var_resultado, population, 
         date, new_cases, cum_cases, time_cases)

# Observations with ventilator 
vent_ZMVM <- vent_data_ZMVM %>%
  rename(date = date_dx) %>%
  select(country, state, county, pais, entidad, municipio, 
         var_outcome, var_resultado, population, 
         date, new_cases, cum_cases, time_cases)

# Deaths 
deaths_ZMVM <- deaths_data_ZMVM %>%
  rename(date = date_dead) %>%
  select(country, state, county, pais, entidad, municipio, 
         var_outcome, var_resultado, population, 
         date, new_cases, cum_cases, time_cases)

# Number of tests 
tests_ZMVM <- tests_data_ZMVM %>%
  rename(date = date_dx) %>%
  select(country, state, county, pais, entidad, municipio, 
         var_outcome, var_resultado, population, 
         date, new_cases, cum_cases, time_cases)


#--------------------------------------------#
####     Final data set for ZMVM          ####  
#--------------------------------------------#

# Append all 7 complete data sets
df_covid_ssa_ZMVM <- sx_ZMVM %>%
  bind_rows(dx_ZMVM) %>%
  bind_rows(hosp_ZMVM) %>%
  bind_rows(icu_ZMVM) %>%
  bind_rows(vent_ZMVM) %>%
  bind_rows(deaths_ZMVM) %>%
  bind_rows(tests_ZMVM) 


#------------------------------------------------------------------#
#------------------------------------------------------------------#
dim(df_covid_ssa_ZMVM)

#--------------------------------------------#
####  Final data set for states and ZMVM  ####  
#--------------------------------------------#

df_covid_ssa_state <- df_covid_ssa_state %>%
  bind_rows(df_covid_ssa_ZMVM)

# Add date stamp to data set
df_covid_ssa_state$time_stamp <- Sys.Date()
#df_covid_ssa_state$time_stamp <- "2020-06-27"


#--------------------------------------------#
####               Save data              ####  
#--------------------------------------------#

# Save complete and most updated data file 
save(df_covid_ssa_state,
     file = "data/state/df_covid_ssa_state.Rdata")

# Save file in csv format
write.csv(df_covid_ssa_state, paste0("data/state/covid_ssa_state_",Sys.Date(),".csv"),
         row.names = FALSE)

# write.csv(df_covid_ssa_state, "data/state/covid_ssa_state_2020-06-27.csv",
#       row.names = FALSE)

# Another option to save the file (just in case accents are not shown)
#write.table(df_covid_ssa_state, paste0("data/state/covid_ssa_state_",Sys.Date(),".csv"),
#  row.names = FALSE, sep = ",")










