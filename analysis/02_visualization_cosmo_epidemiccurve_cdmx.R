#*******************************************************************# 
# This script loads, formats, and plots data from Mexico City       #
#                                                                   #
# Depends on:                                                       #
#   Author: Andrea Luviano                                          # 
#   E-mail: andrea.luviano@cide.edu                                 #
#   Date: Apr 29, 2020                                              #
#   Last edit: Manuel Cardona                                       #
#   E-mail: mcardona@poverty-action.org                             #
#   Date: Apr 30, 2020                                              #  
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

# *****************************************************************************
#### 01_Load_data ####
# *****************************************************************************
setwd("~/GitHub/covid19-mx-nacional")
load("data/projections/05_projections_mex_Mexico City_2020-04-30.RData")

# *****************************************************************************
#### 01_Load_data ####
# *****************************************************************************

#Data frame with all the social distancing factors

df_epidemic_curve <- df_out_mex_total %>% 
  filter(county == "Mexico City", 
         Outcome == "Diagnosed infections") %>%  
  select(Intervention, dates, value)

# *****************************************************************************
#### 02_PLOTS ####
# *****************************************************************************

font_import(paths = "C:/Users/Manuel Cardona Arias/Downloads/Open_Sans/",prompt = F)

# *****************************************************************************
#### 02.01_Epidemic_curve ####
# *****************************************************************************

date_start <- as.Date(min(df_epidemic_curve$date)) %>% 
  format(format = "%d/%m/%Y")

date_end <- as.Date(max(df_epidemic_curve$date)) %>% 
  format(format = "%d/%m/%Y")

#Labels for the four different interventions
sin <- grobTree(textGrob("Sin distanciamiento", x=0.15,  y=1.03, hjust=0.5,
                         gp=gpar(col="#D81920", fontsize=10, fontface="plain", family = "Open Sans", vjust = 50))) 
sinline <- grobTree(textGrob("-", x=0.06,  y=1.03, hjust=0,
                         gp=gpar(col="#D81920", fontsize=17, fontface="plain", family = "Open Sans", vjust = 50))) 

abril <- grobTree(textGrob("Distanciamiento social al\n 30 de abril", x=0.30, y=1.03, hjust=0,
                           gp=gpar(col="#444DA3", fontsize=10, fontface="plain", family = "Open Sans")))
abrilline <- grobTree(textGrob("-", x=0.27,  y=1.03, hjust=0,
                             gp=gpar(col="#444DA3", fontsize=17, fontface="plain", family = "Open Sans", vjust = 50))) 


mayo <- grobTree(textGrob("Distanciamiento social al\n 31 de mayo", x=0.55, y=1.03, hjust=0,
                          gp=gpar(col="#01A6CC", fontsize=10, fontface="plain", family = "Open Sans")))
mayoline <- grobTree(textGrob("-", x=0.52,  y=1.03, hjust=0,
                               gp=gpar(col="#01A6CC", fontsize=17, fontface="plain", family = "Open Sans", vjust = 50))) 

junio <- grobTree(textGrob("Distanciamiento social al\n 30 de junio", x=0.80, y=1.03, hjust=0,
                           gp=gpar(col="#03A650", fontsize=10, fontface="plain", family = "Open Sans")))
junioline <- grobTree(textGrob("-", x=0.77,  y=1.03, hjust=0,
                              gp=gpar(col="#03A650", fontsize=17, fontface="plain", family = "Open Sans", vjust = 50))) 

interv1 <- grobTree(textGrob("Distanciamiento social: ", x=0.6, y=1.08, hjust=0,
                            gp=gpar(col="black", fontsize=11, fontface="bold", family = "Open Sans")))
interv2 <- grobTree(textGrob("reducción estimada del contacto social en ", x=0.73, y=1.08, hjust=0,
                            gp=gpar(col="black", fontsize=11, fontface="plain", family = "Open Sans")))
interv3 <- grobTree(textGrob("44%", x=0.947, y=1.08, hjust=0,
                             gp=gpar(col="black", fontsize=11, fontface="bold", family = "Open Sans")))

#Label for the date in which the social distancing started
dist <- grobTree(textGrob("Fecha de implementación del\ndistanciamiento social", x=0.217, y=0.75, hjust=1,
                          gp=gpar(col="#a8a8a8", fontsize=10, fontface="plain", family = "Open Sans")))


#Different y axis scales
ggplot(
    data = df_epidemic_curve, 
    aes(x = dates, 
    y = value, 
    color = Intervention)) +
  annotation_custom(sin) +
  annotation_custom(sinline)+
  annotation_custom(abril)+
  annotation_custom(abrilline)+
  annotation_custom(mayo)+
  annotation_custom(mayoline)+
  annotation_custom(junio)+
  annotation_custom(junioline)+
  annotation_custom(interv1)+
  annotation_custom(interv2)+
  annotation_custom(interv3)+
  annotation_custom(dist)+
  coord_cartesian(clip = "off") +
  geom_line(
    size = 1,
    show.legend = FALSE) +
  geom_vline(
    xintercept = as.numeric(as.Date("2020-03-23")), #Vertical line indicating the begining of social distancing
    show.legend = TRUE,
    size = .5,
    colour = "#556363",
    linetype = "dashed")+
  theme(
    plot.title = element_text(face = "bold", family = "Open Sans", size = 16, vjust = 8),
    plot.subtitle = element_text(face = "plain", family = "Open Sans", size = 12, vjust = 10 ),
    plot.caption = element_text(hjust = 0, size = 10, colour = "#777777"),
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
                               size = 9),
    axis.text.y = element_text(family = "Open Sans",
                               size = 10),
    legend.position = "bottom",
    plot.margin=unit(c(1.5,1.5,0.5,1),"cm")) +
  guides(
    col = guide_legend(nrow = 2)) + 
  scale_x_date(
    date_labels = "%d %b", #Change the format of the date in the x axis
    date_breaks = "10 days") + #Include 6 breaks in the x axis
  scale_y_continuous(
    breaks = number_ticks(6),
    labels = comma,
    limits = c(0,90000)) +
  scale_colour_manual(
    name = "Intervención", 
    values = c("#D81920", "#444DA3","#01A6CC", "#03A650")) +
  labs(
    title = "Curva epidémica de COVID-19 en Ciudad de México",
    subtitle = paste("Casos proyectados desde el 30/04/2020", "hasta el", date_end),
    x = "",
    y = "Casos diagnosticados ", 
    caption = paste(" Fuente: Elaboración propia el", format(Sys.Date(), "%d/%m/%Y"), "a las",  
                    format(Sys.time(), "%H:%M"), "horas",
                    "con proyecciones del modelo SC-COSMO e información de la Secretaría de Secretaría de Salud Federal, del 30/04/2020.",
                    "Los casos diagnosticados\n son proyectados bajo cuatro escenarios distintos de distanciamiento social: ",
                    "sin distanciamiento social; con intervenciones de reducción estimada del contacto social en 44% hasta ", 
                    "el 31 de abril, hasta el\n 30 de mayo, y hasta el 30 de junio. Ajustes de la visualización en colaboración con Serendipia."))
+
  ggsave(paste0("figs/sccosmo_cdmx_epidemic_curve_",
              format(Sys.Date(), "%F"), "_", format(Sys.time(), "%H.%M"), ".jpeg"), 
       width = 14.88888889, height = 9.527777778, dpi = 320)