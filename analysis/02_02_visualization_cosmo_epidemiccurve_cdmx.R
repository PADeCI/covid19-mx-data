#*******************************************************************# 
# This script loads, formats, and plots data from Mexico City       #
#                                                                   #
# Depends on:                                                       #
#   Author: Andrea Luviano                                          # 
#   E-mail: andrea.luviano@cide.edu                                 #
#   Date: Apr 29, 2020                                              #
#   Last edit: Manuel Cardona                                       #
#   E-mail: mcardona@poverty-action.org                             #
#   Date: May 02, 2020                                              #  
#*******************************************************************# 

#******************************************************************************
#******************************************************************************

#### EPIDEMIC CURVE OF COVID-19 ####

# *****************************************************************************
#******************************************************************************

rm(list = ls()) # to clean the workspace

# *****************************************************************************
#### 01_Load_packages ####
# *****************************************************************************

library(dplyr)
library(ggplot2)
library(grid)
library(scales)
library(dampack)
library(extrafont)
library(gridExtra)
library(purrr)
library(magick)

# *****************************************************************************
#### 01_Load_data ####
# *****************************************************************************
setwd("~/GitHub/covid19-mx-nacional")
load("CDMX_projections_april30/projections/05_projections_mex_Mexico City_2020-05-03_new.RData")

# *****************************************************************************
#### 02_Arrange_data####
# *****************************************************************************

#Data frame with all the social distancing factors

df_epidemic_curve <- subset(df_out_mex_total, county=="Mexico City" & Outcome=="Prevalent diagnosed cases")
df_epidemic_curve <- df_epidemic_curve[c(7, 3, 6)]

df_epidemic_curve_incident <- subset(df_out_mex_total, county=="Mexico City" & Outcome=="Incident diagnosed cases")
df_epidemic_curve_incident <- df_epidemic_curve_incident[c(7, 3, 6)]


# *****************************************************************************
#### 03_PLOTS ####
# *****************************************************************************

#Import font. You may need to download the relevant fonts from:
#https://fonts.google.com/specimen/Open+Sans?query=open+sans

font_import(paths = "C:/Users/Manuel Cardona Arias/Downloads/Open_Sans/",prompt = F) 

# *****************************************************************************
#### 03.01_Epidemic_curve: Active Cases ####
# *****************************************************************************

date_start <- as.Date(min(df_epidemic_curve$date)) %>% #Create a date object for the first registered date
  format(format = "%d/%m/%Y")

date_end <- as.Date(max(df_epidemic_curve$date)) %>% #Create a date object for the last registered date
  format(format = "%d/%m/%Y")

#Labels for the four different interventions
sin <- grobTree(textGrob("No social distancing", x=0.425,  y=0.98, hjust=0.5,
                         gp=gpar(col="#D81920", fontsize=8, fontface="plain", family = "Open Sans", vjust = 50))) 


abril <- grobTree(textGrob("Social distancing until\n April 30", x=0.64, y=0.89, hjust=0.5,
                           gp=gpar(col="#444DA3", fontsize=8, fontface="plain", family = "Open Sans")))


mayo <- grobTree(textGrob("Social distancing until\n May 31", x=0.76, y=0.50, hjust=0.5,
                          gp=gpar(col="#01A6CC", fontsize=8, fontface="plain", family = "Open Sans")))


junio <- grobTree(textGrob("Social distancing until\n June 30", x=0.89, y=0.23, hjust=0.5,
                           gp=gpar(col="#03A650", fontsize=8, fontface="plain", family = "Open Sans")))

interv1 <- grobTree(textGrob("Social distancing: ", x=0.99, y=0.98, hjust=1,
                             gp=gpar(col="black", fontsize=8, fontface="bold", family = "Open Sans")))
interv2 <- grobTree(textGrob("estimated redution of social interaction of ", x=0.97, y=0.95, hjust=1,
                             gp=gpar(col="black", fontsize=8, fontface="plain", family = "Open Sans")))
interv3 <- grobTree(textGrob("44%", x=0.995, y=0.95, hjust=1,
                             gp=gpar(col="black", fontsize=8, fontface="bold", family = "Open Sans")))

#Label for the date in which the social distancing started
dist <- grobTree(textGrob("Date when social distancing\nstrategies begun", x=0.217, y=0.75, hjust=1,
                          gp=gpar(col="#556363", fontsize=8, fontface="plain", family = "Open Sans")))


#Plot:

ggplot(
  data = df_epidemic_curve, #Specifies dataframe to be used
  aes(x = dates, #X axis variable
      y = value, #Y axis variable
      color = Intervention)) + #Specify the different lines that need to be created
  annotation_custom(sin) + #Add all the relevant annotations created above
  annotation_custom(abril)+
  annotation_custom(mayo)+
  annotation_custom(junio)+
  annotation_custom(interv1)+
  annotation_custom(interv2)+
  annotation_custom(interv3)+
  annotation_custom(dist)+
  geom_segment(aes(x = as.Date("2020-08-08"),
                   y = 17000,
                   xend = as.Date("2020-07-27"),
                   yend = 10000), 
               colour='#03A650', 
               size=.5,
               arrow = arrow(length = unit(0.12, "cm")))+
  geom_line( #Geom line needs to be specified so all data points are connected
    size = 0.7, #Size of line
    show.legend = FALSE) + #Avoids legend table to be shown
  geom_vline( #Plots vertical lines
    xintercept = as.numeric(as.Date("2020-03-23")), #Vertical line indicating the begining of social distancing
    show.legend = TRUE, #Avoids legend to be shown on legends' table
    size = 0.5, #Size of line
    colour = "#556363", #RGB color code of line
    linetype = "dashed")+ #Line type
  theme( #Style options: these features are standard across all graphs (some exceptions)
    plot.title = element_text(face = "bold", family = "Open Sans", size = 15), #Specifies all the possible features for the title (this list is not exhaustive)
    plot.subtitle = element_text(face = "plain", family = "Open Sans", size = 11), #Specifies all the possible features for the subtitle (this list is not exhaustive)
    plot.caption = element_text(hjust = 0, size = 7, colour = "#777777"), #Specifies all the possible features for the caption (this list is not exhaustive)
    panel.background = element_rect(fill = "white", #Specifies the features for the panel background
                                    colour = "white", #This refers to the outter lines
                                    size = 0.15, #Size of lines
                                    linetype = "solid"), #Line type
    panel.grid.major = element_line(size = 0.15, #Specifies the features for the major grid
                                    linetype = 'dotted',
                                    colour = "gray"),
    text = element_text(family = "Open Sans"), #Specifies font family to be used
    axis.text.x = element_text(angle = 45, hjust = 1, #Features for the x axis labels
                               family = "Open Sans",
                               size = 7),
    axis.text.y = element_text(family = "Open Sans", #Features for the y axis labels
                               size = 8),
    legend.position = "bottom") + #Position for the legends table
  guides( 
    col = guide_legend(nrow = 2)) + 
  scale_x_date( #Features for the units of x axis
    date_labels = "%d %b", #Change the format of the date in the x axis
    date_breaks = "10 days") + #Include 6 breaks in the x axis
  scale_y_continuous( #Features for the units of y axis
    breaks = number_ticks(12), #Include 12 breaks in the Y axis
    labels = comma) +
  scale_colour_manual( #Colours for the different lines
    name = "Intervención", 
    values = c("#D81920", "#444DA3","#01A6CC", "#03A650")) + #RGB color codes
  labs( #Specify text to be shown in the graph elements
    title = "Projected COVID-19 epidemic curves by different dates of\nsocial distancing strategies termination in Mexico City",
    subtitle = paste("Projected prevalent diagnosed cases from 30/04/2020", "to", date_end),
    x = "",
    y = "Prevalent diagnosed cases ", 
    caption = paste(" Source: Made by @PADeCI1 on", format(Sys.Date(), "%d/%m/%Y"), "at",  
                    format(Sys.time(), "%H:%M"), "hours, ",
                    "using projections of the SC-COSMO model and public data from the Secretaría de Salud ",
                    "Federal. Prevalent diagnosed cases are\n projected under",
                    "four diferent social distancig scenarios: no social distancing",
                    "; reduction of social interaction of 44% until:",
                    "April 30, May 31, and June 30."))+
  ggsave(paste0("figs/sccosmo_cdmx_epidemic_curve_",
                format(Sys.Date(), "%F"), "_", format(Sys.time(), "%H"), ".jpeg"), 
         width = 10, height = 6)


# *****************************************************************************
#### 03.02_Epidemic_curve: Incident  Cases ####
# *****************************************************************************

date_start <- as.Date(min(df_epidemic_curve_incident$date)) %>% 
  format(format = "%d/%m/%Y")

date_end <- as.Date(max(df_epidemic_curve_incident$date)) %>% 
  format(format = "%d/%m/%Y")

#Labels for the four different interventions
sin <- grobTree(textGrob("No NPI", x=0.42,  y=0.98, hjust=0.5,
                         gp=gpar(col="#D81920", fontsize=8, fontface="plain", family = "Open Sans", vjust = 50))) 


mayo <- grobTree(textGrob("NPI until\n May 31", x=0.63, y=0.875, hjust=0.5,
                          gp=gpar(col="#444DA3", fontsize=8, fontface="plain", family = "Open Sans")))


junio <- grobTree(textGrob("NPI until\n June 30", x=0.755, y=0.49, hjust=0.5,
                           gp=gpar(col="#01A6CC", fontsize=8, fontface="plain", family = "Open Sans")))

agosto <- grobTree(textGrob("NPI until\n August 31", x=0.875, y=0.22, hjust=0.5,
                           gp=gpar(col="#03A650", fontsize=8, fontface="plain", family = "Open Sans")))




interv1 <- grobTree(textGrob("NPI effect: ", x=0.885, y=0.98, hjust=1,
                             gp=gpar(col="black", fontsize=8, fontface="bold", family = "Open Sans")))
interv2 <- grobTree(textGrob("estimated reduction ", x=0.99, y=0.98, hjust=1,
                             gp=gpar(col="black", fontsize=8, fontface="plain", family = "Open Sans")))
interv3 <- grobTree(textGrob("on effective contacts by 44% ", x=0.995, y=0.95, hjust=1,
                             gp=gpar(col="black", fontsize=8, fontface="plain", family = "Open Sans")))

#Label for the date in which the social distancing started
dist <- grobTree(textGrob("Date of NPI initiation", x=0.217, y=0.75, hjust=1,
                          gp=gpar(col="#556363", fontsize=8, fontface="plain", family = "Open Sans")))


#Different y axis scales
df_epidemic_curve_incident %>%
  filter(Intervention == "Sin intervención" | Intervention == "Intervención estimada: Reducción de 44% hasta el 31 mayo" | Intervention == "Intervención estimada: Reducción de 44% hasta el 30 de junio" | Intervention == "Intervención estimada: Reducción de 44% hasta el 31 de agosto") %>%
  ggplot(aes(x = dates, 
      y = value, 
      color = Intervention)) +
  annotation_custom(sin) +
  annotation_custom(mayo)+
  annotation_custom(junio)+
  annotation_custom(agosto)+
  annotation_custom(interv1)+
  annotation_custom(interv2)+
  annotation_custom(interv3)+
  annotation_custom(dist)+
  geom_segment(aes(x = as.Date("2020-08-07"),
                   y = 6000,
                   xend = as.Date("2020-07-20"),
                   yend = 4000), 
               colour='#03A650', 
               size=.5,
               arrow = arrow(length = unit(0.12, "cm")))+
  geom_line(
    size = 0.7,
    show.legend = FALSE) +
  geom_vline(
    xintercept = as.numeric(as.Date("2020-03-23")), #Vertical line indicating the begining of social distancing
    show.legend = TRUE,
    size = 0.5,
    colour = "#556363",
    linetype = "dashed")+
  theme(
    plot.title = element_text(face = "bold", family = "Open Sans", size = 15),
    plot.subtitle = element_text(face = "plain", family = "Open Sans", size = 11),
    plot.caption = element_text(hjust = 0, size = 7, colour = "#777777"),
    panel.background = element_rect(fill = "white", 
                                    colour = "white", 
                                    size = 0.15, 
                                    linetype = "solid"),
    panel.grid.major = element_line(size = 0.15, 
                                    linetype = 'dotted',
                                    colour = "gray"),
    text = element_text(family = "Open Sans"),
    axis.text.x = element_text(angle = 45, hjust = 1,
                               family = "Open Sans",
                               size = 7),
    axis.text.y = element_text(family = "Open Sans",
                               size = 8),
    legend.position = "bottom") +
  guides(
    col = guide_legend(nrow = 2)) + 
  scale_x_date(
    date_labels = "%d %b", #Change the format of the date in the x axis
    date_breaks = "10 days") + #Include 6 breaks in the x axis
  scale_y_continuous(
    breaks = number_ticks(12),
    labels = comma) +
  scale_colour_manual(
    name = "Intervención", 
    values = c("#D81920", "#444DA3","#01A6CC", "#03A650")) +
  labs(
    title = "Projected COVID-19 epidemic curves by different dates of\nlifting of non-pharmaceutical interventions (NPI) in Mexico City",
    subtitle = paste("Projected daily prevalent diagnosed cases from 30/04/2020", "to", date_end),
    x = "",
    y = "Daily incident diagnosed cases", 
    caption = paste(" Source: Made by @PADeCI1 on", format(Sys.Date(), "%d/%m/%Y"), "at",  
                    format(Sys.time(), "%H:%M"), "hours, ",
                    "using projections of the SC-COSMO model (sc-cosmo.org) and public data from the Secretaría de Salud ",
                    "Federal.\n Prevalent diagnosed cases are projected under",
                    "four diferent social distancig scenarios: no social distancing",
                    "; reduction of social interaction of 44% until:",
                    "May 31, June 30, and August 31."))+
  ggsave(paste0("figs/sccosmo_cdmx_epidemic_curve_incident_",
                format(Sys.Date(), "%F"), "_", format(Sys.time(), "%H"), ".jpeg"), 
         width = 10, height = 6)


# *****************************************************************************
#### 04 GIF ####
# *****************************************************************************

  # List those Plots, Read them in, and then make animation
  list.files(path = "C:/Users/Manuel Cardona Arias/Documents/GitHub/covid19-mx-nacional/figs/Epidemic curve by  pieces CDMX/", pattern = "*.jpeg", full.names = T) %>% 
    map(magick::image_read) %>% # reads each path file
    magick::image_join() %>% # joins image
    magick::image_animate(fps=1) %>% # animates, can opt for number of loops
    magick::image_write("C:/Users/Manuel Cardona Arias/Documents/GitHub/covid19-mx-nacional/figs/Epidemic curve by  pieces CDMX/Epidemic_curve_CDMX.gif") # write to current dir
