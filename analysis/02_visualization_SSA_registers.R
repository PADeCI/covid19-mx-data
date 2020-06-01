###########################################################
#            COVID-19 Mexico, Open Data from SSA          #
#             Data Regiter Pattern Visualizations         #
#                                                         #
# Authors: Regina Isabel Medina                           #
#                                                         # 
# May 2020                                                #
###########################################################

# This script generates visualizations that illustrate how 
# new data is reported by the SSA. 

#--------------------------------------------#
####          00.  Libraries              ####  
#--------------------------------------------#
library(tidyverse)
library(readr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(scales)
library(dampack)

#---------------------------------------------
####        01.  Cleaning data            ####
#---------------------------------------------
#### 01.1 Adding time stamp for all df generated before 2020-05-08 ####
# First date registered: 2020-04-24
# Last date without time stamp: 2020-05-07

# Set starting point to loop 
date_to_modify <- as.Date("2020-05-07")

# The loop repeats itself 16 times (number of days without time stamps)
for (i in 1:16){
      # Set a blank data frame to start joining all we need
      if (i == 1) {
        df_previous_stamped <- as.data.frame(x = NA, col.names = names(df_date))
      }

      # Import data set
      df_date <- read.csv(paste0("data/state/covid_ssa_state_", date_to_modify, ".csv"), 
                  encoding = "UTF-8") 
      # Add time stamp
      df_date_stamped <- df_date %>%
            mutate(time_stamp = date_to_modify)
      
      # Bind into a single df
      df_previous_stamped <- df_previous_stamped %>%
            bind_rows(df_date_stamped)
      
      # Change date that will be modified in the next execution of the loop
      date_to_modify = date_to_modify - 1
}  

# Remove NAs that were coerced to the df
df_previous_stamped <- df_previous_stamped %>%
  filter(var_outcome != "NA") %>%
  select(-"NA") %>%
  mutate(time_stamp = as.Date(time_stamp))

#### 01.2 Set parameters  ####
# Set must recent date to include in analysis 
last_date <- as.Date("2020-05-24")

# Set number of days which will be analysed 
number_days <- 17  # In this case, all df stamped

#### 01.3 Construct data framework with the days that already had time stamp ####
# Set the starting point for the loop 
date <- last_date 

# Loop that builds the accumulated data frame
for (i in 1:number_days) {
      # Set a blank data frame to start joining all we need
      if (i == 1) {
      df_full <- as.data.frame(x = NA, col.names = names(df_date))
      }
      # Import data set  
      df_date <- read.csv(paste0("data/state/covid_ssa_state_", date, ".csv"), 
                  encoding = "UTF-8")
      
      # Bind into a single df
      df_full <- df_full%>%
        bind_rows(df_date)
      
      # Substract one day, these process will be repeated as many times as dates set
      date <- date - 1 
}

# Remove NAs that were coerced to the df
df_dates_stamped <- df_full %>%
    filter(var_outcome != "NA") %>%
    select(-"NA") %>%
    mutate(time_stamp = as.Date(time_stamp))

#### 01.4 Construct data framework with all dates stamped
df_all_dates_stamped <- df_dates_stamped %>%
    bind_rows(df_previous_stamped) %>%
    mutate(difference = new_cases - lag(new_cases))

#--------------------------------------------#
####    Subset of interest  (national)    ####
#--------------------------------------------#
# Set parameters
day_interest <- as.Date("2020-04-24")
states_interest <- "Mexico" # Set to national data 
var_outcome_interest <- "Confirmed"

# Filter according to parameters
df_date_interest <- df_all_dates_stamped %>%
    filter(date == day_interest, 
            state == states_interest,
            var_outcome == var_outcome_interest)  %>%
    mutate(time_stamp = as.Date(time_stamp))
#View(df_date_interest)  

#--------------------------------------------#
####  Single date difference in registers ####
#--------------------------------------------#
# Clean and subset df for the last register (only for those generated after may 8th)
df_last_date <-  read.csv(paste0("data/state/covid_ssa_state_", last_date, ".csv"), 
    encoding = "UTF-8") # This will be used in the new_cases' dates distribution graph
df_before_last_date  <- read.csv(paste0("data/state/covid_ssa_state_", (last_date-1), ".csv"), 
    encoding = "UTF-8")

df_last_register <- df_last_date %>%
    left_join(df_before_last_date, by = c("country", "state", "county", 
      "pais", "entidad", "municipio", "var_outcome", "var_resultado", 
      "population", "date")) %>%
    rename(new_cases_last_day = "new_cases.x", 
           new_cases_day_before = "new_cases.y", 
           cum_cases_last_day = "cum_cases.x", 
           cum_cases_day_before = "cum_cases.y", 
           time_stamp_last_day = "time_stamp.x", 
           time_stamp_day_before = "time_stamp.y") %>%
    mutate(time_stamp_last_day = as.Date(time_stamp_last_day), 
           time_stamp_day_before = as.Date(time_stamp_day_before), 
           date = as.Date(date)) %>%
    complete(new_cases_day_before, fill = list(new_cases_day_before = 0, cum_cases_day_before = 0)) %>%
    filter(state == states_interest,
      var_outcome == var_outcome_interest, 
      date >= "2020-04-01")  %>%
    mutate(new_registers = new_cases_last_day - new_cases_day_before) %>%
    mutate(lab_ypos = cumsum(new_registers)-0.5*1) # Para posición correcta de etiquetas

df_last_register_2weeks <- df_last_register %>%
    filter(date >= "2020-05-8")

df_last_register_1week <- df_last_register %>%
    filter(date >= "2020-05-15") %>%
    arrange(desc(new_registers)) 
  
# Clean and subset df for the last register (only for those generated before may 8th)
date_interest <- as.Date("2020-04-25")

df_single_date_interest <- df_all_dates_stamped %>%
    filter(time_stamp == date_interest)

df_single_previous_date_interest <- df_all_dates_stamped %>%
    filter(time_stamp == (date_interest-1))

df_single <- df_single_date_interest %>%
    left_join(df_single_previous_date_interest, by = c("country", "state", "county", 
      "pais", "entidad", "municipio", "var_outcome", "var_resultado", 
      "population", "date")) %>%
    rename(new_cases_last_day = "new_cases.x", 
      new_cases_day_before = "new_cases.y", 
      cum_cases_last_day = "cum_cases.x", 
      cum_cases_day_before = "cum_cases.y", 
      time_stamp_last_day = "time_stamp.x", 
      time_stamp_day_before = "time_stamp.y") %>%
    mutate(time_stamp_last_day = as.Date(time_stamp_last_day), 
      time_stamp_day_before = as.Date(time_stamp_day_before), 
      date = as.Date(date)) %>%
    complete(new_cases_day_before, fill = list(new_cases_day_before = 0, cum_cases_day_before = 0)) %>%
    filter(state == states_interest,
      var_outcome == var_outcome_interest, 
      date >= "2020-04-01")  %>%
    mutate(new_registers = new_cases_last_day - new_cases_day_before) %>%
    arrange(desc(date)) %>%
    mutate(lab_ypos = cumsum(new_registers)-0.5*new_registers) # For labels position


df_single_1week <- df_single %>%
  filter(date >= date_interest-7)

df_single_2weeks <- df_single %>%
  filter(date >= date_interest-14)

#--------------------------------------------#
####      Visualizations (national)       ####
#--------------------------------------------#
# Color palette 
    # Red:  "#D81920"
    # Blue: "#444DA3"
    # Light blue: "#01A6CC"
    # Green: "#03A650"

# Evolution of registers for a single date of interest ####
ggplot(df_date_interest, aes(x = time_stamp, y = new_cases)) +
  geom_col(fill = "#01A6CC") +
  scale_x_date(date_labels = "%d %b", #Change the format of the date in the x axis
    date_breaks = "1 day") +
  geom_text(aes(x = time_stamp, y=new_cases, label=comma(new_cases)), 
    vjust = -0.5, size = 3) +
  labs(title = "Número de casos confirmados de Covid-19 a nivel nacional para el 24 de abril \nsegún diferentes fechas de reporte",
    x = "Fecha de reporte",
    y = "Número de casos confirmados el 24 de abril",
    caption =  paste(" Fuente: Elaboración propia el", format(Sys.Date(), "%d/%m/%Y"), "a las",  
      format(Sys.time(), "%H:%M"), "horas",
      "con información de las bases de datos",
      "públicas de la Dirección General de Epidemiología de la Secretaría de Salud \n del 24 de abril al 24 de mayo"), 
      ".") +
  coord_cartesian(xlim = c(as.Date("2020-04-24")), as.Date("2020-05-24"), expand = F, 
    default = F, clip = "on") +
  theme_minimal() + #This is the basic theme to be used
  scale_y_continuous(labels = comma) +
  coord_cartesian(xlim = c(as.Date("2020-04-25"), as.Date("2020-05-23"))) +
  theme(plot.title = element_text(face = "bold",
    size = 16,
    family = ""),
    plot.subtitle = element_text(size = 12,
      face = "plain", 
      family = ""),
    plot.caption = element_text(hjust = 0, 
      face = "plain", 
      family = "",
      size = 8,
      colour = "#777777"),
    panel.background = element_rect(fill = "white", 
      colour = "white", 
      size = 0.15, 
      linetype = "solid"),
    axis.title.x = (element_text(size = 12,
      family = "")),
    axis.title.y = (element_text(size =12,
      family = "")),
    element_line(linetype = "dotted",
      colour = "gray99",
      size = .1),
    axis.text.x = element_text(angle = 90,
      hjust = 1,
      size = 8, 
      family = ""),
    axis.text.y = element_text(size = 8,
      family = ""),
    legend.text = element_text(size = 12,
      family = ""))+
  ggsave(paste0("figs/registers_",
    states_interest, "_", 
    var_outcome_interest, "_", 
    format(day_interest, "%F"), ".jpeg"), 
    width = 12, height = 6)+
  ggsave(paste0("figs/registers_",
    states_interest, "_",
    var_outcome_interest, "_", 
    format(day_interest, "%F"), ".pdf"), 
    width = 14, height = 6)

# Diffence in number of new cases for a single date in multiple reports ####
#ggplot(df_date_interest, aes(x = date, y = difference)) +
#   geom_col()

# Distribution of new cases' dates on a single report multiple bars ####
ggplot(df_last_register, aes(x = date, y = new_registers)) +
  geom_col(fill = "#444DA3") +
  scale_x_date(
    date_labels = "%d %b", #Change the format of the date in the x axis
    date_breaks = "1 day") +
  geom_text(aes(x = date, y=new_registers, label=comma(new_registers)), 
    vjust = -0.5, size = 3) +
  labs(title = "Distribución de fechas de confirmación de los nuevos casos de Covid-19 \nreportados el 24 de mayo a nivel nacional",
    x = "Fecha de confirmación",
    y = "Número de casos reportados el 24 de mayo",
    caption =  paste(" Fuente: Elaboración propia el", format(Sys.Date(), "%d/%m/%Y"), "a las",  
      format(Sys.time(), "%H:%M"), "horas",
      "con información de la base de datos",
      "pública de la Dirección General de Epidemiología de la Secretaría de Salud del 24 de mayo de 2020"), 
    ".") +
  coord_cartesian(xlim = c(as.Date("2020-04-03"), as.Date("2020-05-22"))) +
  theme_minimal() + #This is the basic theme to be used
  theme(plot.title = element_text(face = "bold",
    size = 16,
    family = ""),
    plot.subtitle = element_text(size = 12,
      face = "plain", 
      family = ""),
    plot.caption = element_text(hjust = 0, 
      face = "plain", 
      family = "",
      size = 8,
      colour = "#777777"),
    panel.background = element_rect(fill = "white", 
      colour = "white", 
      size = 0.15, 
      linetype = "solid"),
    axis.title.x = (element_text(size = 12,
      family = "")),
    axis.title.y = (element_text(size =12,
      family = "")),
    element_line(linetype = "dotted",
      colour = "gray99",
      size = .1),
    axis.text.x = element_text(angle = 90,
      hjust = 1,
      size = 8, 
      family = ""),
    axis.text.y = element_text(size = 8,
      family = ""),
    legend.text = element_text(size = 12,
      family = "")) +
  ggsave(paste0("figs/registers_dist_",
    states_interest, "_", 
    var_outcome_interest, "_", 
    format(last_date, "%F"), ".jpeg"), 
    width = 10, height = 6)+
  ggsave(paste0("figs/registers_dist_",
    states_interest, "_",
    var_outcome_interest, "_", 
    format(last_date, "%F"), ".pdf"), 
    width = 14, height = 6)

# Distribution in a single report single stacked bar ####
# Failed version
#ggplot(df_last_register_1week, aes(x = time_stamp_last_day, y = new_registers, fill = as.factor(date), color = as.factor(date))) +
#  geom_col() +
#  #theme(legend.position = "none") +
#  geom_label(aes(label = new_registers), position = "identity", col = "black", 
#    show.legend = F) 

# Functioning version
ggplot(df_single_1week, aes(x = time_stamp_last_day, y = new_registers, fill = as.factor(date), 
  color = as.factor(date))) +
  geom_col(show.legend = T) +
  geom_text(aes(y = lab_ypos, label = new_registers), col = "black", show.legend = F) +
  labs(title = "Distribución de nuevos casos confirmados 
    \nreportados el 25 de abril durante la semana 
    \nprevia al reporte", 
      x = "Fecha de reporte: 25 de abril",
      y = "Nuevos casos reportados", 
      fill = "Fecha de confirmación",
      col = "",
      caption =  paste(" Fuente: Elaboración propia el", format(Sys.Date(), "%d/%m/%Y"), 
        "a las",  format(Sys.time(), "%H:%M"), "horas",
        "con \n información de la base de datos",
        "pública de la Dirección General \n de Epidemiología de la Secretaría de Salud del 25 de abril de 2020"), ".") +
  theme_minimal() +
  scale_fill_brewer(palette = "Spectral") +
  scale_color_brewer(palette = "Spectral") +
  guides(colour = F) + # Hide color label (we only need the fill label)
  theme(plot.title = element_text(face = "bold",
    lineheight = .4,
    size = 16,
    family = ""),
    plot.subtitle = element_text(size = 12,
      face = "plain", 
      family = ""),
    plot.caption = element_text(hjust = 0, 
      face = "plain", 
      family = "",
      size = 8,
      colour = "#777777"),
    panel.background = element_rect(fill = "white", 
      colour = "white", 
      size = 0.15, 
      linetype = "solid"),
    axis.title.x = (element_text(size = 12,
      family = "")),
    axis.title.y = (element_text(size =12,
      family = "")),
    element_line(linetype = "dotted",
      colour = "gray99",
      size = .1),
    axis.text.x = element_text(angle = 45,
      hjust = 1,
      size = 0, 
      family = ""),
    axis.text.y = element_text(size = 8,
      family = ""),
    legend.text = element_text(size = 12,
      family = "")) +
  ggsave(paste0("figs/lastweek_dist_", 
    states_interest, "_", 
    var_outcome_interest, "_", 
    format(date_interest, "%F"), ".jpeg"), 
    width = 6, height = 10) +
  ggsave(paste0("figs/lastweek_dist_", 
    states_interest, "_", 
    var_outcome_interest, "_", 
    format(date_interest, "%F"), ".pdf"), 
    width = 6, height = 10)


#--------------------------------------------#
#  Subset of interest  (Mexico City)     ####
#--------------------------------------------#
# Set parameters
day_interest <- as.Date("2020-04-24")
states_interest <- "Mexico City" # Set to national data 
var_outcome_interest <- "Confirmed"

# Filter according to parameters
df_date_interest <- df_all_dates_stamped %>%
  filter(date == day_interest, 
    state == states_interest,
    var_outcome == var_outcome_interest)  %>%
  mutate(time_stamp = as.Date(time_stamp))
View(df_date_interest)  


#--------------------------------------------#
#      Visualizations (Mexico City)       ####
#--------------------------------------------#
# Color palette 
# Red:  "#D81920"
# Blue: "#444DA3"
# Light blue: "#01A6CC"
# Green: "#03A650"

# Evolution of registers for a single date of interest ####
ggplot(df_date_interest, aes(x = time_stamp, y = new_cases)) +
  geom_col(fill = "#01A6CC") +
  scale_x_date(
    date_labels = "%d %b", #Change the format of the date in the x axis
    date_breaks = "2 days") +
  geom_text(aes(x = time_stamp, y=new_cases, label=new_cases), 
    position = position_dodge(width=2), vjust = -0.5, size = 3) +
  labs(title = "Número de casos de Covid-19 confirmados en Ciudad de México para el 24 de abril \n según diferentes fechas de reporte",
    x = "Fecha de reporte",
    y = "Casos confirmados",
    caption =  paste(" Fuente: Elaboración propia el", format(Sys.Date(), "%d/%m/%Y"), "a las",  
      format(Sys.time(), "%H:%M"), "horas",
      "con información de la base de datos ",
      "pública de la Dirección General de Epidemiología de la Secretaría de Salud"), 
    ".") +
  theme_minimal() + #This is the basic theme to be used
  ggsave(paste0("figs/registers_",
    states_interest, "_", 
    var_outcome_interest, "_", 
    format(day_interest, "%F"), ".jpeg"), 
    width = 10, height = 6)+
  ggsave(paste0("figs/registers_",
    states_interest, "_",
    var_outcome_interest, "_", 
    format(day_interest, "%F"), ".pdf"), 
    width = 10, height = 6)


# Disctribution of new cases' dates on a single report ####
ggplot(df_last_register, aes(x = date, y = new_registers)) +
  geom_col(fill = "#444DA3") +
  scale_x_date(
    date_labels = "%d %b", #Change the format of the date in the x axis
    date_breaks = "3 days") +
  geom_text(aes(x = date, y=new_registers, label=new_registers), 
    position = position_dodge(width=2), vjust = -0.5, size = 3) +
  labs(title = "Distribución de fechas de diagnóstico de los nuevos casos de Covid-19 reportados el 22 de mayo en Ciudad de México",
    x = "Fecha de diagnóstico",
    y = "Número de casos confirmados",
    caption =  paste(" Fuente: Elaboración propia el", format(Sys.Date(), "%d/%m/%Y"), "a las",  
      format(Sys.time(), "%H:%M"), "horas",
      "con información de la base de datos ",
      "pública de la Dirección General de Epidemiología de la Secretaría de Salud"), 
    ".") +
  theme_minimal() + #This is the basic theme to be used
  ggsave(paste0("figs/registers_dist_",
    states_interest, "_", 
    var_outcome_interest, "_", 
    format(last_date, "%F"), ".jpeg"), 
    width = 10, height = 6)+
  ggsave(paste0("figs/registers_dist_",
    states_interest, "_",
    var_outcome_interest, "_", 
    format(last_date, "%F"), ".pdf"), 
    width = 10, height = 6)


#--------------------------------------------#
#                   DRAFTS                ####
#--------------------------------------------#


#ggplot(df_date_interest, aes(x = date, y = new_cases, color = time_cases)) + 
#  geom_col()

## Generate graphs ##
ggplot(df_dates_analyzed, aes(x = date, y = new_cases, color = time_stamp)) +
  geom_line() 
#  geom_line(data = t1_mod) +
#  geom_line(data = t2_mod) +
#  geom_line(data = t3_mod) +
#  geom_line(data = t4_mod) +
#  geom_line(data = t5_mod) +
#  labs(title = "Registro de  nuevos por día para Ciudad de México", 
#    x = "Nuevos casos diagnosticados", 
#    y = "Fecha del diagnóstico del caso") +
#  ggsave(paste0("figs/sccosmo_cdmx_registers",".jpeg"), 
#    width = 10, height = 6)+
#  ggsave(paste0("figs/sccosmo_cdmx_registers", ".pdf"), 
#    width = 10, height = 6)


#-----------------------------------------####