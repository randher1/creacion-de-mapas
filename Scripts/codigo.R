#Cargar librerias
require(pacman)

pacman::p_load(car,ggrepel,gdata, cartography, lubridate, raster, 
               rgdal, rgeos, sf, ggspatial, tidyverse, RColorBrewer,ggpubr,
               ggplot2,dplyr)
#limpiar espaciop de trabajo
rm(list = ls()) 

#Eliminamos la notacion científica
options(scipen = 999999)


#carga de archivos espaciales y csv 
censo<- read.csv("Data/db/Originales/censo.csv",sep = ",")
mapa <- read_sf("Data/shapefiles/Barrios/Barrios.shp")
mapa<-mapa[-c(185:210), ]

# cabiamos de nombre a las variables
names(censo)<-c("BARRIOS",
                "OCUPA_FAMILIA",
                "OCUPA_MOTOTAXISMO",
                "OCUPA_RECREATIVO")


# Unión de datos espaciales y csv
censo_w <- merge(censo,mapa,by.x='BARRIOS',by.y='NOMBRE',all.y=T) 



# Convertir censo_w a un objeto sf si aún no lo es
if (!inherits(censo_w, "sf")) {
  censo_w <- st_as_sf(censo_w)
}

# Asignar CRS si aún no lo tiene
if (is.na(st_crs(censo_w))) {
  st_crs(censo_w) <- 4326  # Reemplaza 4326 con el código EPSG adecuado si no es el correcto
}



# Generar centroides para todos los barrios
censo_w <- censo_w %>%
  mutate(centroid = map(geometry, st_centroid),
         coords = map(centroid, st_coordinates),
         coords_x = map_dbl(coords, 1),
         coords_y = map_dbl(coords, 2))

# Seleccionar el top de  barrios con los valores más altos en la columna 'OCUPA_MOTOTAXISMO'
top <- censo_w %>%
  slice_max(order_by = OCUPA_MOTOTAXISMO, n = 5) %>%
  arrange(desc(OCUPA_MOTOTAXISMO)) %>%
  mutate(rank = row_number(),
         label = paste0(rank, ": ", BARRIOS, " : ","(",OCUPA_MOTOTAXISMO,")"),
         BARRIOS = factor(BARRIOS, levels = rev(BARRIOS))) 

#valor del top
n_top<-length(top$rank)
n_top

# Crear el mapa
mapa <- ggplot(data = censo_w) +
  geom_sf(aes(geometry = geometry, fill = OCUPA_MOTOTAXISMO)) +
  annotation_north_arrow(location = 'tl', 
                         which_north = "true", 
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "bl", 
                   width_hint = 0.4) +
  scale_fill_gradient2(name = 'Mototaxismo',
                       low = "#1A9641",
                       mid = "#FFFFBF",
                       high = "#D7191C",
                       midpoint = median(censo_w$OCUPA_MOTOTAXISMO, 
                                         na.rm = TRUE),
                       na.value = 'white') +
  # Agregar etiquetas solo para los barrios del top 
  geom_text_repel(data = top,
                  mapping = aes(x = coords_x, y = coords_y, label = label),
                  size = 3, min.segment.length = 0, show.legend = FALSE) +
  labs(fill = NULL,x="Latitud",y="Longitud") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = 'bold'),
        legend.position = 'bottom',
        legend.key.width = unit(5, 'line'))

print(mapa)

# crear gráfico de barras del top
top_bar <-ggplot(top, aes(x = BARRIOS, y = OCUPA_MOTOTAXISMO)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(x="",y = "Ocupación en Mototaxismo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

print(top_bar)

# unir ambos graficos

arranged_maps <-ggarrange(mapa, top_bar, ncol = 2, common.legend = TRUE,
          legend = 'bottom')


# agregar titulo conjunto y pie de imagen
final_plot <- annotate_figure(arranged_maps,
                              top = text_grob(paste0('Top ',n_top,' de barrios en el sector mototaxismo\nCARTAGENA'), 
                                              face = "bold", 
                                              size = 14),
                              bottom = text_grob('Elaboración propia \nAutor: Randolf Herrera',
                                                 hjust = 1, x = 1, face = "italic", size = 10))
print(final_plot)


guardar_grafico <- function(grafico) {
  formatos <- c("ps", "svg", "pdf")  # Lista de formatos de archivo
  
  nombre_archivo_base <- deparse(substitute(grafico))  # Obtener el nombre del gráfico
  
  for (formato in formatos) {
    nombre_archivo <- paste0("Resultados/img/", nombre_archivo_base, ".", formato)  # Nombre del archivo con extensión correspondiente
    dispositivo <- switch(formato,
                          "ps" = postscript,
                          "svg" = svg,
                          "pdf" = pdf)
    
    # Abrir dispositivo de salida
    dispositivo(nombre_archivo, width = 11, height = 9)
    
    # Imprimir el gráfico
    print(grafico)  # Asegúrate de imprimir el gráfico
    
    # Cerrar dispositivo de salida
    dev.off()
  }
}

guardar_grafico(top_bar)
