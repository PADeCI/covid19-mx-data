library(dplyr)
library(ggplot2)
library(grid)
library(scales)
library(tidyr)
library(dampack)

load("data/df_cdmx_beds_sccosmo_used_covid_19")

txtsize <- 12
bed_total   <- 10000
#p_ocupation <- 0.897

note_total_beds <- grobTree(textGrob(paste("Capacidad total regular=", comma(bed_total), "camas"), 
                                     x = 0.05,  y = 0.93, hjust = 0,
                                     gp = gpar(col = "black", 
                                               fontsize = (txtsize-2), 
                                               fontface = "plain")))

date_lim <- grobTree(textGrob("Fecha en la que se alcanza el límite de \ncapacidad total regular de camas", 
                              x=0.6, y=0.4, hjust=0.5,
                              gp=gpar(col="white",
                                      fontsize=10, 
                                      fontface="bold"),
                              rot = 90))
#May
ggplot(data = df_cdmx_beds_sccosmo_used_covid_19, 
       aes(fill = `Camas`, 
           x = date, 
           y = hosp)) +
  geom_area(position = "stack") +
  facet_wrap(~ Distanciamiento) + #scales = "free_y"
  geom_hline(yintercept = bed_total) +
  #coord_cartesian(ylim=c(2000, 2600)) +
  geom_vline(xintercept = as.numeric(as.Date("2020-05-12")), 
             linetype = "dashed", 
             color = "white")  +
  annotation_custom(note_total_beds) + 
  annotation_custom(date_lim)+
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        panel.background = element_rect(fill = "white", 
                                        colour = "gray", 
                                        size = 0.15, 
                                        linetype = "solid"),
        panel.grid.major = element_line(size = 0.15, 
                                        linetype = 'solid',
                                        colour = "gray"), 
        panel.grid.minor = element_line(size = 0.15, 
                                        linetype = 'solid',
                                        colour = "gray"), 
        axis.text.x = element_text(angle = 90, hjust = 1,),  
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow=2, byrow=TRUE)) + 
  scale_x_date(date_labels = "%d/%m", #Change the format of the date in the x axis
               breaks = number_ticks(20)) +
  scale_y_continuous(breaks = number_ticks(6), 
                     labels = comma) +
  scale_fill_manual(values=c ("#F15923", "#01A6CC", "#444DA3", "#D81920")) +
  labs(title = "Ocupación de camas de hospitalización en la Ciudad de México",
       subtitle = paste("Datos actualizados hasta el 01/05/20202"),
       x = "Fecha",
       y = "Número de camas", 
       caption = paste("Fuente: Elaboración propia el", format(Sys.Date(), "%d/%m/%Y"), "a las",  
                       format(Sys.time(), "%H:%M"), "horas",
                       "con información de la Secretaría de Salud Federal."))

ggsave(paste0("figs/cdmx_bed_capacity_hosp_",
              format(Sys.Date(), "%F"), ".pdf"), 
       width = 10, height = 6)

