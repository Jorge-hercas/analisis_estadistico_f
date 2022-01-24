
######################### Librerías #########################
library(dplyr)
library(spdplyr)
library(echarts4r)
#library(ggplot2)
library(leaflet)
library(reactable)
library(shiny)
library(shinyWidgets)

#############################################################



# Lectura y transformación de los datos

## Shapefile para cambiar vista del mapa
bounds <- sf::read_sf("shapefile/EEUU/Estados_Unidos_Estados.shp") 


## Datos proporcionados 
df <- vroom::vroom("Partrunner_DataAnalyst_Assignment.csv")


df <- df |> 
  mutate(total_industrias = rowSums(df[5:9]),
         proper_ap = (`Property A`/max(df$`Property A`))/3,
         proper_bp = (`Property B`/max(df$`Property B`))/3,
         proper_cp = (`Property C`/max(df$`Property C`))/3,
         proper_tot = (proper_ap+proper_bp+proper_cp)/2,
         ind_met = (total_industrias/max(total_industrias))/2,
         metrica_fin = proper_tot+ind_met
         
  ) |> 
  arrange(desc(metrica_fin))



# Tooltip y paleta de colores iniciales

paleta <- colorBin( palette="Reds", domain=df$metrica_fin, 
                    na.color="transparent", bins=6)

tooltip_texto <- paste0(
  "Ciudad: ", df$City, "<br/>",
  "Cantidad total de empresas: ", df$total_industrias, "<br/>",
  "Calif. de las características de mercado (normalizadas): ", round(df$proper_tot, digits = 2)*100,"%", "<br/>",
  "<strong>Calificación final: </strong>", round(df$metrica_fin, digits = 2)*100,"%", "<br/>") |> 
  lapply(htmltools::HTML)




