###########################################################
#            COVID-19 Mexico, Open Data from SSA          #
#                 Data Wrangle by County                  #
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
library(data.table)

#--------------------------------------------#
####            Load data                 #### 
#--------------------------------------------#

#ssa <- read.csv("data-raw/200713COVID19MEXICO.csv")
ssa <- fread("data-raw/210315COVID19MEXICO.csv",header=TRUE)
load("data-raw/df_pop_county.Rdata")   # population and names

#--------------------------------------------#
####      Working with SSA data           ####   
#--------------------------------------------#

# Check that database confirms numbers in the public report
#(Sumar de clasificacion final; 1,2 y 3 )
#Negativos = 7
table(ssa$CLASIFICACION_FINAL)

# Keep important variables only from ssa data
ssa_data <- ssa %>%
  select(ENTIDAD_RES, MUNICIPIO_RES, FECHA_INGRESO, FECHA_SINTOMAS, 
         FECHA_DEF, EDAD, SEXO, TIPO_PACIENTE, INTUBADO, UCI, CLASIFICACION_FINAL)

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
ssa_data$covid <- ifelse(ssa_data$CLASIFICACION_FINAL %in% c(1,2,3), 1, 0)
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



#--------------------------------------------#
####  Paste counties and population info  ####   
#--------------------------------------------#
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


#------------------------------------------------------------------#
#------------------------------------------------------------------#

####  Work for ZMVM  ####
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
         state = case_when(zmvm == 1 ~ "MCMA"))

ssa_data_ZMVM <- ssa_data %>%
  mutate(zmvm = ifelse(county_id %in% v_zmvm_id, 1, 0)) %>%
  filter(zmvm == 1) %>%
  mutate(entidad = case_when(zmvm == 1 ~ "ZMVM"), 
         state = case_when(zmvm == 1 ~ "MCMA"))

# Compare population 
sum(unique(ssa_covid_ZMVM$population))
sum(unique(ssa_data_ZMVM$population))

#------------------------------------------------#
####    Add ZMVM state to the whole data      ####   
#------------------------------------------------#

# Remove variables that were added in the process and are no longer needed
ssa_covid_ZMVM <- ssa_covid_ZMVM[-28]
ssa_data_ZMVM <- ssa_data_ZMVM[-23]

# Bind rows for ZMVM
ssa_covid <- ssa_covid %>%
  bind_rows(ssa_covid_ZMVM)
ssa_data <- ssa_data %>%
  bind_rows(ssa_data_ZMVM)

#------------------------------------------------------------------#
#------------------------------------------------------------------#


#-------------------------------------------------#
#### Data sets with output variables by county ####   
#-------------------------------------------------#

# All data sets will have the following 3 variables:
# new_cases: number of new cases with var_outcome on that date
# cum_cases: cummulative number of cases of var_outcome up to that date
# time_cases: time (number of days where 0 = day of first var_outcome)

# Date until which we create the sequence
max_date <- Sys.Date()
# max_date <- as.Date("2021-03-15")

# Symptomatic observations grouped by (country, state, county) 
# and date_sx
sx_data <- ssa_covid %>% 
  mutate(country = "Mexico", 
         county = county_name_eng,
         pais = "México",
         municipio = county_name_esp) %>%
  group_by(country, state, county, pais, entidad, municipio, county_id, date_sx) %>%
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
         county = county_name_eng,
         pais = "México",
         municipio = county_name_esp) %>%
  group_by(country, state, county, pais, entidad, municipio, county_id, date_dx) %>%
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
         county = county_name_eng,
         pais = "México",
         municipio = county_name_esp) %>%
  group_by(country, state, county, pais, entidad, municipio, county_id, date_dx) %>%
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
         county = county_name_eng,
         pais = "México",
         municipio = county_name_esp) %>%
  group_by(country, state, county, pais, entidad, municipio, county_id, date_dx) %>%
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
         county = county_name_eng,
         pais = "México",
         municipio = county_name_esp) %>%
  group_by(country, state, county, pais, entidad, municipio, county_id, date_dx) %>%
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
         county = county_name_eng,
         pais = "México",
         municipio = county_name_esp) %>%
  filter(!is.na(date_dead)) %>%
  group_by(country, state, county, pais, entidad, municipio, county_id, date_dead) %>%
  summarise(new_cases = sum(dead_ind)) %>%
  complete(date_dead = seq.Date(min(date_dead), max_date, by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_dead)-1)),
         var_outcome = "Deaths",
         var_resultado = "Muertes")

# Number of tests grouped by (country, state, county) and date_dx
tests_data <- ssa_data %>% 
  mutate(country = "Mexico", 
         county = county_name_eng,
         pais = "México",
         municipio = county_name_esp) %>%
  group_by(country, state, county, pais, entidad, municipio, county_id, date_dx) %>%
  summarise(new_cases = sum(test_ind)) %>%
  complete(date_dx = seq.Date(min(date_dx), max_date, by="day"), # Create a sequence of dates
           fill = list(new_cases = 0)) %>% # Fill the dates without cases with zero
  mutate(cum_cases = cumsum(new_cases), 
         time_cases = c(0:(length(date_dx)-1)),
         var_outcome = "Tests",
         var_resultado = "Pruebas")

# Count number of observations
dim(sx_data)
dim(dx_data)
dim(hosp_data)
dim(icu_data)
dim(vent_data)
dim(deaths_data)
dim(tests_data)


#--------------------------------------------#
####  Arrange variables in all data sets  ####   
#--------------------------------------------#
# Symptomatic observations 
sx_full <- sx_data %>%
  rename(date = date_sx) %>%
  left_join(df_pop_county, 
            by = c("county_id" = "county_id")) %>%
  rename(entidad = "entidad.x") %>%
  select(country, state, county, pais, entidad, municipio, 
         county_id, population, var_outcome, var_resultado,  
         date, new_cases, cum_cases, time_cases)

# Confirmed observations 
dx_full <- dx_data %>%
  rename(date = date_dx) %>%
  left_join(df_pop_county, 
            by = c("county_id" = "county_id")) %>%
  rename(entidad = "entidad.x") %>%
  select(country, state, county, pais, entidad, municipio, 
         county_id, population, var_outcome, var_resultado,  
         date, new_cases, cum_cases, time_cases)

# Hospitalized observations 
hosp_full <- hosp_data %>%
  rename(date = date_dx) %>%
  left_join(df_pop_county, 
    by = c("county_id" = "county_id")) %>%
  rename(entidad = "entidad.x") %>%
  select(country, state, county, pais, entidad, municipio, 
         county_id, population, var_outcome, var_resultado,  
         date, new_cases, cum_cases, time_cases)

# ICU observations 
icu_full <- icu_data %>%
  rename(date = date_dx) %>%
  left_join(df_pop_county, 
    by = c("county_id" = "county_id")) %>%
  rename(entidad = "entidad.x") %>%
  select(country, state, county, pais, entidad, municipio, 
         county_id, population, var_outcome, var_resultado,  
         date, new_cases, cum_cases, time_cases)

# Observations with ventilator 
vent_full <- vent_data %>%
  rename(date = date_dx) %>%
  left_join(df_pop_county, 
    by = c("county_id" = "county_id")) %>%
  rename(entidad = "entidad.x") %>%
  select(country, state, county, pais, entidad, municipio, 
         county_id, population, var_outcome, var_resultado,  
         date, new_cases, cum_cases, time_cases)

# Deaths 
deaths_full <- deaths_data %>%
  rename(date = date_dead) %>%
  left_join(df_pop_county, 
    by = c("county_id" = "county_id")) %>%
  rename(entidad = "entidad.x") %>%
  select(country, state, county, pais, entidad, municipio, 
         county_id, population, var_outcome, var_resultado,  
         date, new_cases, cum_cases, time_cases)

# Number of tests 
tests_full <- tests_data %>%
  rename(date = date_dx) %>%
  left_join(df_pop_county, 
    by = c("county_id" = "county_id")) %>%
  rename(entidad = "entidad.x") %>%
  select(country, state, county, pais, entidad, municipio, 
         county_id, population, var_outcome, var_resultado,  
         date, new_cases, cum_cases, time_cases)


#--------------------------------------------#
####           Final data set             ####  
#--------------------------------------------#

# Append all 7 complete data sets
df_covid_ssa_county <- sx_full %>%
  bind_rows(dx_full) %>%
  bind_rows(hosp_full) %>%
  bind_rows(icu_full) %>%
  bind_rows(vent_full) %>%
  bind_rows(deaths_full) %>%
  bind_rows(tests_full) 

# Add date stamp to data set
df_covid_ssa_county$time_stamp <- Sys.Date()
# df_covid_ssa_county$time_stamp <- "2021-03-15"

#--------------------------------------------#
####               Save data              ####  
#--------------------------------------------#

# Save complete and most updated data file 
save(df_covid_ssa_county,
     file = "data/county/df_covid_ssa_county.Rdata")

# Save file in csv format
write.csv(df_covid_ssa_county, paste0("data/county/covid_ssa_county_",Sys.Date(),".csv"),
          row.names = FALSE)
# write.csv(df_covid_ssa_county, "data/county/covid_ssa_county_2020-03-15.csv",
#            row.names = FALSE)











