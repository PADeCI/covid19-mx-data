#*****************************************************************************# 
# This script loads, and formats data from Mexico City                        #
#                                                                             #
# Depends on:                                                                 #
# Author: Andrea Luviano                                                      # 
# E-mail: andrea.luviano@cide.edu                                             #
#*****************************************************************************# 
#******************************************************************************

#### BED OCUPATION - CDMX ####

# *****************************************************************************
#******************************************************************************

rm(list = ls()) # to clean the workspace

# *****************************************************************************
#### 01_Load_packages ####
# *****************************************************************************

library(dplyr)
library(readxl)

# *****************************************************************************
#### 02_Load_data ####
# *****************************************************************************

load("data/projections/05_projections_mex_Mexico City_2020-05-03.RData")

# *****************************************************************************
#### 01_Current_Hospital_COVID-19_beds ####
# *****************************************************************************

df_cdmx_beds_hosp <- read_excel("data-raw/comportamiento_hospitalario_cdmx.xlsx", 
                           sheet = "COVID 19 Ciudad de México") %>% 
  mutate(date = as.Date(Fecha), 
         Camas = "Camas de pacientes con COVID-19") %>% 
  select(date, Camas, hosp = `Camas_Hospital_CDMX`) 

# *****************************************************************************
#### 01_Proyected_hosp_beds ####
# *****************************************************************************

cdmx_date   <- min(df_cdmx_beds_hosp$date) %>% 
  as.Date()
min_date    <- max(df_cdmx_beds_hosp$date)
max_date    <- max(df_out_mex_total$dates)
bed_total   <- 2552
p_ocupation <- 0.897
bed_used <- bed_total*p_ocupation
bed_free <- bed_total - bed_used

## May
df_cdmx_beds_hosp_may <- df_cdmx_beds_hosp %>% 
  mutate(Distanciamiento = "Distanciamiento social hasta el 31 de mayo") 

df_cdmx_beds_hosp_may$hosp <- as.numeric(gsub(",", "", df_cdmx_beds_hosp_may$hosp))
  
df_cdmx_sccosmo_beds_hosp_may <- df_out_mex_total %>% 
  filter(Outcome == "All Hospitalization prevalence", 
         Intervention == "Intervención estimada: Reducción de 44% hasta fin de mayo") %>% 
  mutate(date = as.Date(dates), 
         Camas = "Camas proyectadas con distanciamieto social hasta el 31 de mayo", 
         Distanciamiento = "Distanciamiento social hasta el 31 de mayo") %>% 
  filter(date > "2020-04-28") %>% 
  select(date, Camas, hosp = value, Distanciamiento)

df_cdmx_sccosmo_beds_hosp_may[1,3] <- 0

df_cdmx_beds_used_may <- seq.Date(min(cdmx_date), max(max_date), by="day") %>% 
  as.data.frame() %>% 
  rename(date = ".") %>% 
  mutate(`Camas` = "Camas usadas", 
         hosp = bed_total*p_ocupation, 
         Distanciamiento = "Distanciamiento social hasta el 31 de mayo")

df_cdmx_beds_sccosmo_used_covid_19_may <- bind_rows(df_cdmx_beds_used_may , df_cdmx_beds_hosp_may, 
                                               df_cdmx_sccosmo_beds_hosp_may)

## June
df_cdmx_beds_hosp_jun <- df_cdmx_beds_hosp %>% 
  mutate(Distanciamiento = "Distanciamiento social hasta el 30 de junio") 

df_cdmx_beds_hosp_jun$hosp <- as.numeric(gsub(",", "", df_cdmx_beds_hosp_jun$hosp))

df_cdmx_sccosmo_beds_hosp_jun <- df_out_mex_total %>% 
  filter(Outcome == "All Hospitalization prevalence", 
         Intervention == "Intervención estimada: Reducción de 44% hasta fin de junio") %>% 
  mutate(date = as.Date(dates), 
         Camas = "Camas proyectadas con distanciamiento social hasta el 30 de junio", 
         Distanciamiento = "Distanciamiento social hasta el 30 de junio") %>% 
  filter(date > "2020-04-28") %>%
  select(date, Camas, hosp = value, Distanciamiento)

df_cdmx_sccosmo_beds_hosp_jun[1,3] <- 0

df_cdmx_beds_used_jun <- seq.Date(min(cdmx_date), max(max_date), by="day") %>% 
  as.data.frame() %>% 
  rename(date = ".") %>% 
  mutate(`Camas` = "Camas usadas", 
         hosp = bed_total*p_ocupation, 
         Distanciamiento = "Distanciamiento social hasta el 30 de junio")

df_cdmx_beds_sccosmo_used_covid_19_jun <- 
  bind_rows(df_cdmx_beds_used_jun , df_cdmx_beds_hosp_jun, 
            df_cdmx_sccosmo_beds_hosp_jun)

df_cdmx_beds_sccosmo_used_covid_19 <- 
  bind_rows(df_cdmx_beds_sccosmo_used_covid_19_may,
            df_cdmx_beds_sccosmo_used_covid_19_jun) %>% 
  arrange(desc(Distanciamiento))

df_cdmx_beds_sccosmo_used_covid_19$Distanciamiento <- 
  ordered(df_cdmx_beds_sccosmo_used_covid_19$Distanciamiento,
          as.character(unique(df_cdmx_beds_sccosmo_used_covid_19$Distanciamiento)))
  
save(df_cdmx_beds_sccosmo_used_covid_19, 
     file = "data/df_cdmx_beds_sccosmo_used_covid_19")

# *****************************************************************************
#### 03_ICU_beds ####
# *****************************************************************************

# *****************************************************************************
#### 01_Current_ICU_COVID-19_beds ####
# *****************************************************************************

df_cdmx_icu <- read_excel("data-raw/comportamiento_hospitalario_cdmx.xlsx", 
                          sheet = "COVID 19 Ciudad de México") %>% 
  select(date = Fecha, hosp = `Camas_Intubados_CDMX`)

# *****************************************************************************
#### 01_Proyected_ICU_beds ####
# *****************************************************************************

df_cdmx_beds_hosp_may <- df_cdmx_beds_hosp %>% 
  mutate(Distanciamiento = "Distanciamiento social hasta el 31 de mayo") 

df_cdmx_beds_hosp_may$hosp <- as.numeric(gsub(",", "", df_cdmx_beds_hosp_may$hosp))

df_cdmx_sccosmo_beds_hosp_may <- df_out_mex_total %>% 
  filter(Outcome == "All Hospitalization prevalence", 
         Intervention == "Intervención estimada: Reducción de 44% hasta fin de mayo") %>% 
  mutate(date = as.Date(dates), 
         Camas = "Camas proyectadas con distanciamieto social hasta el 31 de mayo", 
         Distanciamiento = "Distanciamiento social hasta el 31 de mayo") %>% 
  filter(date > "2020-04-28") %>% 
  select(date, Camas, hosp = value, Distanciamiento)

df_cdmx_sccosmo_beds_hosp_may[1,3] <- 0

df_cdmx_beds_used_may <- seq.Date(min(cdmx_date), max(max_date), by="day") %>% 
  as.data.frame() %>% 
  rename(date = ".") %>% 
  mutate(`Camas` = "Camas usadas", 
         hosp = bed_total*p_ocupation, 
         Distanciamiento = "Distanciamiento social hasta el 31 de mayo")

df_cdmx_beds_sccosmo_used_covid_19_may <- bind_rows(df_cdmx_beds_used_may , df_cdmx_beds_hosp_may, 
                                                    df_cdmx_sccosmo_beds_hosp_may)

## June
df_cdmx_beds_hosp_jun <- df_cdmx_beds_hosp %>% 
  mutate(Distanciamiento = "Distanciamiento social hasta el 30 de junio") 

df_cdmx_beds_hosp_jun$hosp <- as.numeric(gsub(",", "", df_cdmx_beds_hosp_jun$hosp))

df_cdmx_sccosmo_beds_hosp_jun <- df_out_mex_total %>% 
  filter(Outcome == "All Hospitalization prevalence", 
         Intervention == "Intervención estimada: Reducción de 44% hasta fin de junio") %>% 
  mutate(date = as.Date(dates), 
         Camas = "Camas proyectadas con distanciamiento social hasta el 30 de junio", 
         Distanciamiento = "Distanciamiento social hasta el 30 de junio") %>% 
  filter(date > "2020-04-28") %>%
  select(date, Camas, hosp = value, Distanciamiento)

df_cdmx_sccosmo_beds_hosp_jun[1,3] <- 0

df_cdmx_beds_used_jun <- seq.Date(min(cdmx_date), max(max_date), by="day") %>% 
  as.data.frame() %>% 
  rename(date = ".") %>% 
  mutate(`Camas` = "Camas usadas", 
         hosp = bed_total*p_ocupation, 
         Distanciamiento = "Distanciamiento social hasta el 30 de junio")

df_cdmx_beds_sccosmo_used_covid_19_jun <- 
  bind_rows(df_cdmx_beds_used_jun , df_cdmx_beds_hosp_jun, 
            df_cdmx_sccosmo_beds_hosp_jun)

df_cdmx_beds_sccosmo_used_covid_19 <- 
  bind_rows(df_cdmx_beds_sccosmo_used_covid_19_may,
            df_cdmx_beds_sccosmo_used_covid_19_jun) %>% 
  arrange(desc(Distanciamiento))

df_cdmx_beds_sccosmo_used_covid_19$Distanciamiento <- 
  ordered(df_cdmx_beds_sccosmo_used_covid_19$Distanciamiento,
          as.character(unique(df_cdmx_beds_sccosmo_used_covid_19$Distanciamiento)))

save(df_cdmx_beds_sccosmo_used_covid_19, 
     file = "data/df_cdmx_beds_sccosmo_used_covid_19")
