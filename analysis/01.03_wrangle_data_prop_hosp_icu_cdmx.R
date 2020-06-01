#*****************************************************************************# 
# This script loads, and formats data from Mexico City                        #
#                                                                             #
# Depends on:                                                                 #
# Author: Andrea Luviano                                                      # 
# E-mail: andrea.luviano@cide.edu                                             #
#*****************************************************************************# 

library(readxl)
library(dplyr)
library(tidyr)

v_names_age_groups <- c("0-4 años", "5-14 años", "15-24 años", "25-44 años", 
                        "45-54 años", "55-64 años", "65-69 años", "70+ años")

df_national <- read.csv("data-raw/200502COVID19MEXICO.csv") %>% 
  select(state = ENTIDAD_RES, age = EDAD, conf = RESULTADO, 
         date_dx = FECHA_INGRESO, hosp = TIPO_PACIENTE, vent = INTUBADO, 
         icu = UCI)

df_cdmx_conf <- df_national %>% 
  filter(state == 9, 
         conf == 1)

nrow(df_cdmx_conf)

df_cdmx_conf_age_group <- df_cdmx_conf %>% 
  mutate(age_groups = cut(age, c(-1, 5, 15, 25, 45, 55, 65, 69, 114))) %>% 
  group_by(age_groups) %>% 
  summarise(idx = n())

levels(df_cdmx_conf_age_group$age_groups) = v_names_age_groups

sum(df_cdmx_conf_age_group$idx)

df_cdmx_conf_hosp_age_group <- df_cdmx_conf %>% 
  filter(hosp == 2) %>% 
  mutate(age_groups = cut(age, c(-1, 5, 15, 25, 45, 55, 65, 69, 114))) %>% 
  group_by(age_groups) %>% 
  summarise(hosp = n())
  
levels(df_cdmx_conf_hosp_age_group$age_groups) = v_names_age_groups

sum(df_cdmx_conf_hosp_age_group$hosp)
  
p_hosp <- sum(df_cdmx_conf_hosp_age_group$hosp)/sum(df_cdmx_conf_age_group$idx)
p_hosp

df_cdmx_conf_hosp_icu_age_group <- df_cdmx_conf %>% 
  filter(hosp == 2) %>% 
  mutate(real_uci = case_when((icu == 1 | vent == 1 ~ 1)), 
         age_groups = cut(age, c(-1, 5, 15, 25, 45, 55, 65, 69, 114))) %>% 
  filter(real_uci == 1) %>% 
  group_by(age_groups) %>% 
  summarise(icu = n()) %>% 
  complete(age_groups, fill = list(icu = 0))

levels(df_cdmx_conf_hosp_icu_age_group$age_groups) = v_names_age_groups

p_icu <- sum(df_cdmx_conf_hosp_icu_age_group$icu)/sum(df_cdmx_conf_age_group$idx)
p_icu 

df_cdmx_p_hosp_icu 

(sum(df_cdmx_conf_hosp_age_group$hosp) - sum(df_cdmx_conf_hosp_icu_age_group$icu)) / sum(df_cdmx_conf_age_group$idx)

#**********

df_cdmx_hosp_time_serie <- df_cdmx_conf %>% 
  filter(hosp == 2) %>% 
  group_by(date_dx) %>% 
  summarise(hosp = n()) %>% 
  mutate(date = as.Date(date_dx)) %>% 
  complete(date = seq.Date(min(date), max(date), by="day"), 
           fill = list(hosp= 0)) %>% 
  mutate(cum_hosp = cumsum(hosp)) %>% 
  select(date, cum_hosp)
  

