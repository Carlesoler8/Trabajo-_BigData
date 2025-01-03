---
  title: "Agricultura en España"
description: |
  Vamos a analizar varios datos de la agricultura en España.
author:  
  - name: Joan Puigcerver Llorens (jopuig3@alumni.uv.es)
- name: Carles Soler Sanchis (@alumni.uv.es)

date: 2024-12-01                           #--
categories: [trabajo BigData]      #--
image: "./imagenes/imagen_01.png"
title-block-banner: true #- {true, false, "green","#AA0000"}
title-block-banner-color: "#FFB5C5"    #-"#FFFFFF"
toc-depth: 3
smooth-scroll: true
format:
  html:
  backgroundcolor: "#F1F3F4"
#embed-resources: true
link-external-newwindow: true
#css: ./assets/my_css_file.css   #- CUIDADO!!!!
code-tools: true
code-link: true
---
  
#Cargar paquetes necesarios
library(ggplot2)
library(plotly)
library(corrplot)
library(dplyr)
library(reshape2)
library(tidyverse)
library(cluster)
library(gt)
library(gtExtras)
library(gridExtra)
library(fmsb)
library(radarchart)
library(gganimate)
library(ggwordcloud)
library(png)
library(ggrepel)
library(dplyr)
library(readr)

# Cargamos todos los datos que utilizaremos a lo largo del trabajo:

dir.create("datos")


# Importar el archivo usando el comando import del paquete rio
datos <- rio::import("C:/Users/Usuario/Desktop/datos/1101.csv")
datos_prod <- rio::import("datos/agricultura_prod.csv")
datos_precios <- rio::import("datos/agricultura_precios.csv")

# Dividir los datos por sexo´
library(dplyr)
hombres <- datos %>% filter(Sexo == "Hombres")
mujeres <- datos %>% filter(Sexo == "Mujeres")
ambos_sexos <- datos %>% filter(Sexo == "Ambos sexos")

#También separamos por tipo de titulares

titulares <- datos %>% filter(`Clase de titular` == "Titulares")
titulares_jefes <- datos %>% filter(`Clase de titular` == "Titulares jefes de explotación")

#Ahora vamos a crear unas nuevas variables en porcentajes, que luego compararemos
datos$Total <- as.numeric(gsub("[^0-9.]", "", datos$Total))

# Paso 1: Agrupar los datos por Grupos de edad
datos_agrupados <- datos %>%
  group_by(`Grupos de edad`, Sexo) %>%        # Agrupar por Grupos de edad y Sexo
  summarise(Total_Grupo = sum(Total, na.rm = TRUE), .groups = "drop")  # Calcular total por grupo

# Paso 2: Calcular el total por sexo en cada grupo de edad
datos_agrupados <- datos_agrupados %>%
  group_by(`Grupos de edad`) %>%            
  mutate(Total_Sexo = sum(Total_Grupo, na.rm = TRUE),
         Porcentaje_Sexo = (Total_Grupo / Total_Sexo) * 100) %>%
  ungroup()


#Ahora mostramos los resultados con un gáfico
# Filtrar los datos para "Todas las edades" y solo Hombres/Mujeres
datos_filtrados <- datos %>%
  filter(Sexo %in% c("Hombres", "Mujeres") & `Grupos de edad` == "Todas las edades") %>%
  group_by(Sexo) %>%                                        # Agrupar solo por Sexo
  summarise(Total_Grupo = sum(Total, na.rm = TRUE),         # Sumar totales para Hombres y Mujeres
            .groups = "drop")                               # Eliminar agrupaciones

total_todas_edades <- sum(datos_filtrados$Total_Grupo, na.rm = TRUE)

# Recalcular los porcentajes
datos_filtrados <- datos_filtrados %>%
  mutate(Porcentaje_Sexo = (Total_Grupo / total_todas_edades) * 100)

# Crear el gráfico circular con los porcentajes corregidos
ggplot(datos_filtrados, aes(x = "", y = Porcentaje_Sexo, fill = Sexo)) +
  geom_bar(stat = "identity", width = 1, color = "white") +   # Crear barras apiladas
  coord_polar(theta = "y") +                                  # Convertir a gráfico circular
  labs(title = "Distribución por Sexo (Todas las edades)", y = NULL, x = NULL) +
  theme_void() +                                              # Eliminar ejes innecesarios
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +       # Colores personalizados
  geom_text(aes(label = paste0(round(Porcentaje_Sexo, 1), "%")),
            position = position_stack(vjust = 0.5), size = 5) # Añadir etiquetas porcentuales



#Ahora, con los datos iniciales vamos a comparar simplemente por edades para ver cuáles tienen más peso
# Filtrar los datos para excluir "Todas las edades"
datos_por_edad <- datos %>%
  filter(`Grupos de edad` != "Todas las edades") %>%   # Excluir el grupo "Todas las edades"
  group_by(`Grupos de edad`) %>%                       # Agrupar por Grupos de edad
  summarise(Total_Grupo = sum(Total, na.rm = TRUE),    # Sumar el total por grupo de edad
            .groups = "drop")                          # Eliminar agrupaciones

# Calcular el total general y los porcentajes
total_general <- sum(datos_por_edad$Total_Grupo, na.rm = TRUE)
datos_por_edad <- datos_por_edad %>%
  mutate(Porcentaje_Grupo = (Total_Grupo / total_general) * 100)  # Calcular el porcentaje

# Crear el gráfico circular con ggplot2
ggplot(datos_por_edad, aes(x = "", y = Porcentaje_Grupo, fill = `Grupos de edad`)) +
  geom_bar(stat = "identity", width = 1, color = "white") +       # Crear barras apiladas
  coord_polar(theta = "y") +                                      # Convertir a gráfico circular
  labs(title = "Distribución por Grupos de Edad (sin 'Todas las edades')",
       y = NULL, x = NULL) +
  theme_void() +                                                  # Eliminar ejes innecesarios
  scale_fill_brewer(palette = "Set3") +                           # Paleta de colores
  geom_text(aes(label = paste0(round(Porcentaje_Grupo, 1), "%")),
            position = position_stack(vjust = 0.5), size = 5)     # Añadir etiquetas porcentuales

#También haremos un gráfico de barras con el núemero de personas en cada grupo de edad
# Filtrar y agrupar los datos para obtener el total por grupo de edad
datos_por_edad <- datos %>%
  filter(`Grupos de edad` != "Todas las edades") %>%   # Excluir "Todas las edades" si es necesario
  group_by(`Grupos de edad`) %>%
  summarise(Total_Grupo = sum(Total, na.rm = TRUE), .groups = "drop")  # Calcular total por grupo


# Abreviar nombres largos
datos_por_edad <- datos_por_edad %>%
  mutate(`Grupos de edad` = case_when(
    `Grupos de edad` == "Menores de 25 años" ~ "Menores 25",
    `Grupos de edad` == "De 25 a 34 años" ~ "25-34",
    `Grupos de edad` == "De 35 a 44 años" ~ "35-44",
    `Grupos de edad` == "De 45 a 54 años" ~ "45-54",
    TRUE ~ `Grupos de edad`
  ))

# Crear gráfico
ggplot(datos_por_edad, aes(x = reorder(`Grupos de edad`, -Total_Grupo), y = Total_Grupo / 1000, fill = `Grupos de edad`)) +
  geom_bar(stat = "identity", color = "white") +         # Gráfico de barras
  labs(title = "Distribución por Grupos de Edad",
       x = "Grupos de Edad",
       y = "Total (en miles)") +
  theme_minimal() +                                      # Tema limpio
  scale_y_continuous(labels = scales::comma) +           # Formatear eje Y
  scale_fill_brewer(palette = "Set3", guide = "none") +  # Colores y sin leyenda
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +  # Rotación y tamaño
  geom_text(aes(label = paste0(round(Total_Grupo / 1000, 1), "k")),      # Etiquetas dentro de las barras
            vjust = -0.5, size = 5)





#Ahora haremos un gráfico para ver en qué proporción los titulares son solo titulares o también trabajan en sus tierras
# Agrupar por Clase de titular
proporciones_titulares <- datos %>%
  group_by(`Clase de titular`) %>%
  summarise(Total_Clase = sum(Total, na.rm = TRUE), .groups = "drop")  # Sumar totales por clase

# Calcular el total general
total_titulares <- sum(proporciones_titulares$Total_Clase, na.rm = TRUE)

# Añadir la proporción a cada categoría
proporciones_titulares <- proporciones_titulares %>%
  mutate(Proporción = (Total_Clase / total_titulares) * 100)



# Crear un gráfico circular para visualizar las proporciones
ggplot(proporciones_titulares, aes(x = "", y = Proporción, fill = `Clase de titular`)) +
  geom_bar(stat = "identity", width = 1, color = "white") +       # Crear barras apiladas
  coord_polar(theta = "y") +                                      # Convertir a gráfico circular
  labs(title = "Proporción de Titulares y Titulares jefes de explotación",
       y = NULL, x = NULL) +
  theme_void() +                                                  # Eliminar ejes innecesarios
  scale_fill_brewer(palette = "Set3") +                           # Paleta de colores
  geom_text(aes(label = paste0(round(Proporción, 1), "%")),
            position = position_stack(vjust = 0.5), size = 5)     # Añadir etiquetas porcentuales


#Finalmente con estos datos, compararemos la proporción de explotaciones con y sin tierras:
# Filtrar los datos para explotaciones con y sin tierras
explotaciones <- datos %>%
  group_by(`Tamaño de las explotaciones según SAU (Ha.)`) %>%                     # Agrupar por tipo de explotación
  summarise(Total_Explotaciones = sum(Total, na.rm = TRUE), # Sumar totales
            .groups = "drop")                              # Eliminar agrupaciones

# Calcular el total general y los porcentajes
total_general <- sum(explotaciones$Total_Explotaciones, na.rm = TRUE)
explotaciones <- explotaciones %>%
  mutate(Proporción = (Total_Explotaciones / total_general) * 100)


# Crear un gráfico de barras para comparar
colores <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(length(unique(explotaciones$`Tamaño de las explotaciones según SAU (Ha.)`)))

library(scales)

ggplot(explotaciones, aes(x = reorder(`Tamaño de las explotaciones según SAU (Ha.)`, -Total_Explotaciones),
                          y = Total_Explotaciones / 1000, fill = `Tamaño de las explotaciones según SAU (Ha.)`)) +
  geom_bar(stat = "identity", color = "white") +
  scale_fill_manual(values = colores) +  # Usar colores personalizados
  labs(title = "Comparación de Explotaciones con y sin Tierras",
       x = "Tipo de Explotación",
       y = "Total de Explotaciones (en miles)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +  
  theme(legend.position = "none",              
        axis.text.x = element_text(angle = 45, hjust = 1))  +                                
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotar las etiquetas del eje X


# Crear un gráfico circular para visualizar proporciones
library(RColorBrewer)

# Generar una paleta extendida basada en Set2
num_colores <- length(unique(explotaciones$`Tamaño de las explotaciones según SAU (Ha.)`))
colores <- colorRampPalette(brewer.pal(8, "Set2"))(num_colores)

ggplot(explotaciones, aes(x = "", y = Proporción, fill = `Tamaño de las explotaciones según SAU (Ha.)`)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(title = "Proporción de Explotaciones con y sin Tierras",
       x = NULL, y = NULL) +
  theme_void() +
  scale_fill_manual(values = colores) +  # Usar colores personalizados
  geom_text(aes(label = paste0(round(Proporción, 1), "%")),
            position = position_stack(vjust = 0.5), size = 5)



#Ahora vamos a trabajar con los segundos datos que hemos importado.
#Haremos primero un gáfico de barras con la producción por países el año 2023
# Filtrar datos para el año 2023
# Filtrar datos para el año 2023
data_filtered <- datos_prod %>%
  filter(!grepl(" ", geo))  # Eliminar filas donde 'Geo' tiene un espacio
data_2023 <- data_filtered %>%
  filter(TIME_PERIOD == max(TIME_PERIOD, na.rm = TRUE)) %>%                # Filtrar datos para 2023
  group_by(geo) %>%                       # Agrupar por países
  summarise(Total_Production = sum(OBS_VALUE, na.rm = TRUE)) %>% # Sumar producción
  arrange(Total_Production)               # Ordenar por producción

# Verificar los datos procesados
print(data_2023)

# Crear el gráfico de barras horizontal
ggplot(data_2023, aes(x = reorder(geo, Total_Production), y = Total_Production)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +                          # Invertir el gráfico para barras horizontales
  labs(title = "Producción Agrícola por País (2023)",
       x = "País",
       y = "Producción (Millones de Euros)") +
  theme_minimal()

#Para visualizarlo de otra manera vamos a hacerlo con un mapa
# Filtrar datos para un año específico, en este caso 2023
data_2023 <- datos_prod %>%
  filter(TIME_PERIOD == 2023) %>%
  group_by(geo) %>%
  summarise(Total_Production = sum(OBS_VALUE, na.rm = TRUE))  # Sumar producción por país

# Descargar geometrías de países de Europa
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filtrar países de la UE (asegúrate de tener los nombres correctos en "Geo")
eu_map <- world %>%
  filter(region_un == "Europe") %>%
  select(name, geometry)  # Seleccionar nombre del país y geometría

# Unir los datos de producción con el mapa
map_data <- eu_map %>%
  left_join(data_2023, by = c("name" = "geo"))  # Combinar usando nombres de países

# Crear el mapa de coropletas
ggplot(map_data) +
  geom_sf(aes(fill = Total_Production), color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey90",
                      name = "Producción\n(Millones de Euros)") +
  labs(title = "Producción Agrícola por País (2023)",
       subtitle = "Unión Europea",
       caption = "Datos en millones de euros") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),       # Aumentar tamaño del título
    plot.subtitle = element_text(size = 14),                  # Aumentar tamaño del subtítulo
    legend.title = element_text(size = 12),                   # Ajustar tamaño de la leyenda
    legend.text = element_text(size = 10)
  ) +
  coord_sf(expand = FALSE)                                    # Ajustar límites del mapa

#Como vemos, Francia fue el país con más producció agrícola en 2023 así que vamos a ver su evolución a lo largo de los años
# Filtrar los datos para Francia
data_france <- datos_prod %>%
  filter(geo == "France") %>%  # Filtrar solo para Francia
  group_by(TIME_PERIOD) %>%           # Agrupar por año
  summarise(Total_Production = sum(OBS_VALUE, na.rm = TRUE))  # Sumar la producción por año

# Crear el gráfico de barras
ggplot(data_france, aes(x = TIME_PERIOD, y = Total_Production)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Evolución de la Producción Agrícola en Francia",
       x = "Año",
       y = "Producción Agrícola (Millones de Euros)") +
  theme_minimal()


#Obviamente nos interesa más España así que veamos también su evolución a lo largo de los años
data_spain <- datos_prod %>%
  filter(geo == "Spain") %>%  # Filtrar solo para España
  group_by(TIME_PERIOD) %>%           # Agrupar por año
  summarise(Total_Production = sum(OBS_VALUE, na.rm = TRUE))  # Sumar la producción por año

# Crear el gráfico de barras
ggplot(data_spain, aes(x = TIME_PERIOD, y = Total_Production)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Evolución de la Producción Agrícola en Francia",
       x = "Año",
       y = "Producción Agrícola (Millones de Euros)") +
  theme_minimal()

#Y con un gráfico animado
# Filtrar datos para España
data_spain <- datos_prod %>%
  filter(geo == "Spain") %>%                                # Filtrar para España
  filter(!is.na(OBS_VALUE) & !is.infinite(OBS_VALUE)) %>%   # Eliminar valores no válidos
  group_by(TIME_PERIOD) %>%
  summarise(Total_Production = sum(OBS_VALUE, na.rm = TRUE))  # Sumar producción por año

# Verificar si los datos están limpios
print(head(data_spain))

# Crear el gráfico base con ggplot
p <- ggplot(data_spain, aes(x = TIME_PERIOD, y = Total_Production)) +
  geom_line(color = "blue", size = 1.2) +                   # Línea principal
  geom_point(size = 3, color = "red") +                    # Puntos en cada año
  labs(title = "Evolución de la Producción Agrícola en España",
       x = "Año",
       y = "Producción (Millones de Euros)",
       caption = "Fuente: Dataset proporcionado") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"))

# Añadir animación con gganimate
anim <- p +
  transition_reveal(TIME_PERIOD) +                      # Revelar línea a lo largo de los años
  labs(subtitle = "Año: {frame_along}")

# Generar y guardar la animación como GIF
animate(anim, width = 800, height = 600, fps = 10, duration = 10, renderer = gifski_renderer("produccion_espana_linea.gif"))


#Y ahora haremos un gráfico circular para ver la proporción de la producción agrícola de España respecto a la del resto de Europa
# Filtrar datos para el año 2023
data_2023 <- datos_prod %>%
  filter(TIME_PERIOD == 2023)

# Calcular la producción agrícola de España y del resto de Europa
summary_data <- data_2023 %>%
  mutate(Category = ifelse(geo == "Spain", "España", "Resto de Europa")) %>%
  group_by(Category) %>%
  summarise(Total_Production = sum(OBS_VALUE, na.rm = TRUE))

# Crear el gráfico circular
ggplot(summary_data, aes(x = "", y = Total_Production, fill = Category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("España" = "red", "Resto de Europa" = "blue")) +
  labs(title = "Proporción de la Producción Agrícola Española vs Resto de Europa (2023)",
       fill = "Categoría") +
  theme_void() +
  geom_text(aes(label = paste0(round(Total_Production / sum(Total_Production) * 100, 1), "%")),
            position = position_stack(vjust = 0.5), size = 5)

#La producción de España fue solo el 2.5% del total de Europa, pero veamos qué porcentaje fue la de los 3 países con mayor producción
# Filtrar datos para el año 2023 y excluir agrupaciones no deseadas
data_2023 <- datos_prod %>%
  filter(TIME_PERIOD == 2023) %>%
  filter(!grepl("European|Euro area", geo))  # Excluir filas con "European" o "Euro area"

# Calcular la producción total por país y seleccionar los 3 principales
top_countries <- data_2023 %>%
  group_by(geo) %>%
  summarise(Total_Production = sum(OBS_VALUE, na.rm = TRUE)) %>%
  arrange(desc(Total_Production)) %>%
  slice_head(n = 3)  # Seleccionar los 3 países con mayor producción

# Identificar los tres principales y el resto de Europa
summary_data <- data_2023 %>%
  group_by(geo) %>%
  summarise(Total_Production = sum(OBS_VALUE, na.rm = TRUE)) %>%
  mutate(Category = ifelse(geo %in% top_countries$geo, geo, "Resto de Europa")) %>%
  group_by(Category) %>%
  summarise(Total_Production = sum(Total_Production, na.rm = TRUE))

# Validar los datos finales antes del gráfico
print("Summary data for the chart:")
print(summary_data)

# Crear el gráfico circular
ggplot(summary_data, aes(x = "", y = Total_Production, fill = Category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set3", name = "Categoría") +
  labs(title = "Comparación de los 3 países con mayor producción agrícola vs Resto de Europa (2023)") +
  theme_void() +
  geom_text(aes(label = paste0(Category, ": ", round(Total_Production / sum(Total_Production) * 100, 1), "%")),
            position = position_stack(vjust = 0.5), size = 4)
#Como vemos, los tres países con mayor producción agraria suman casi el 45% de la producción total europea, veamos qué pasa si añadimos España que era el cuarto:
data_2023 <- datos_prod %>%
  filter(TIME_PERIOD == 2023) %>%
  filter(!grepl("European|Euro area", geo))  # Excluir filas con "European" o "Euro area"

# Calcular la producción total por país y seleccionar los 3 principales
top_countries <- data_2023 %>%
  group_by(geo) %>%
  summarise(Total_Production = sum(OBS_VALUE, na.rm = TRUE)) %>%
  arrange(desc(Total_Production)) %>%
  slice_head(n = 4)  # Seleccionar los 4 países con mayor producción

# Identificar los tres principales y el resto de Europa
summary_data <- data_2023 %>%
  group_by(geo) %>%
  summarise(Total_Production = sum(OBS_VALUE, na.rm = TRUE)) %>%
  mutate(Category = ifelse(geo %in% top_countries$geo, geo, "Resto de Europa")) %>%
  group_by(Category) %>%
  summarise(Total_Production = sum(Total_Production, na.rm = TRUE))


# Crear el gráfico circular
ggplot(summary_data, aes(x = "", y = Total_Production, fill = Category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set3", name = "Categoría") +
  labs(title = "Comparación de los 3 países con mayor producción agrícola vs Resto de Europa (2023)") +
  theme_void() +
  geom_text(aes(label = paste0(Category, ": ", round(Total_Production / sum(Total_Production) * 100, 1), "%")),
            position = position_stack(vjust = 0.5), size = 4)
#Comparativa por países
# Filtrar datos para eliminar países con más de una palabra en el nombre
data_filtered_cleaned <- data_filtered %>%
  filter(!grepl(" ", geo))  # Excluir países con más de una palabra en 'geo'

# Crear el gráfico de barras
ggplot(data_filtered_cleaned, aes(x = TIME_PERIOD, y = OBS_VALUE, fill = geo)) +
  geom_bar(stat = "identity") +
  labs(title = "Contribución de cada país a la producción total por año",
       x = "Año", y = "Producción (Millones de Euros)") +
  scale_y_continuous(labels = scales::comma) +  # Formatear el eje Y
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar etiquetas de años para mayor claridad
    plot.title = element_text(size = 16, face = "bold")
  )


#Finalmente, vamos a trabajar con los últimos datos para ver un poco la evolución de precios y algunos índices
#Vamos a arreglarlos un poco para no tener que estar haciéndolo en cada apartado:
# Filtrar el dataset eliminando países con nombres de más de una palabra
data_cleaned <- datos_precios %>%
  filter(!grepl(" ", geo))  # Excluir filas donde 'geo' contiene un espacio

#Ahora trabajaremos con el dataset data_cleaned
#Cogeremos el maíz como el principal cereal representante de la actividad agrícola, para medir la evolución de precios

# Filtrar para un producto específico
data_germany <- data_cleaned %>%
  filter(geo == "Germany")
data_filtered <- data_cleaned %>%
  filter(prod_veg == "Soft wheat - prices per 100 kg")

ggplot(data_germany, aes(x = TIME_PERIOD, y = OBS_VALUE, color = geo)) +
  geom_line() +
  labs(title = "Tendencia del Precio de Maíz en Alemania",
       x = "Año", y = "Precio por 100kgs (Euros/Moneda local)",
       color = "País") +
  theme_minimal()

#Veamos ahora los precios en España:
data_spain <- data_cleaned %>%
  filter(geo == "Spain")
data_filtered <- data_cleaned %>%
  filter(prod_veg == "Soft wheat - prices per 100 kg")

ggplot(data_spain, aes(x = TIME_PERIOD, y = OBS_VALUE, color = geo)) +
  geom_line() +
  labs(title = "Tendencia del Precio de Maíz en Alemania",
       x = "Año", y = "Precio por 100kgs (Euros/Moneda local)",
       color = "País") +
  theme_minimal()


#Ahora haremos la media de todos los países:
# Calcular la media global por año
global_mean <- data_cleaned %>%
  group_by(TIME_PERIOD) %>%
  summarise(Global_Mean = mean(OBS_VALUE, na.rm = TRUE))  # Media de producción global

# Filtrar los datos de Alemania
data_spain <- data_spain %>%
  filter(geo == "Spain") %>%
  select(TIME_PERIOD, OBS_VALUE) %>%
  rename(Spain = OBS_VALUE)  # Renombrar la columna para distinguirla

# Combinar las dos series
combined_data <- global_mean %>%
  left_join(data_spain, by = "TIME_PERIOD")

# Crear el gráfico
ggplot(combined_data, aes(x = TIME_PERIOD)) +
  geom_line(aes(y = Global_Mean, color = "Media Global"), size = 1) +
  geom_line(aes(y = Spain, color = "España"), size = 1) +
  labs(title = "Producción Agrícola: España vs Media Global",
       x = "Año",
       y = "Producción (Euros/Moneda local)",
       color = "Leyenda") +
  theme_minimal() +
  scale_color_manual(values = c("Media Global" = "blue", "España" = "red")) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#Veamos ahora qué ha pasado con el precio del aceite de oliva estos últimos años en España:
# Filtrar los datos para España y el producto "Aceite de oliva"
data_olive_oil <- data_cleaned %>%
  filter(geo == "Spain", prod_veg == "Extra virgin olive oil - prices per 100 litres")  # Ajustar "Olive oil" según los valores reales en 'prod_veg'

# Crear el gráfico de línea
ggplot(data_olive_oil, aes(x = TIME_PERIOD, y = OBS_VALUE)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(color = "green", size = 3) +
  labs(title = "Evolución del Precio del Aceite de Oliva en España (Por 100l)",
       x = "Año",
       y = "Precio (Euros/Moneda local)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#Vemos una importante tendencia alcista desde el año 2020
#Ahora vamos a ver la media de Europa
data_olive_oil <- data_cleaned %>%
  filter(prod_veg == "Extra virgin olive oil - prices per 100 litres")  # Asegúrate de que el nombre coincida exactamente con los valores de prod_veg

# Calcular la media de precios en Europa por año
europe_mean_olive <- data_olive_oil %>%
  group_by(TIME_PERIOD) %>%
  summarise(Average_Price = mean(OBS_VALUE, na.rm = TRUE))

# Crear el gráfico
ggplot(europe_mean_olive, aes(x = TIME_PERIOD, y = Average_Price)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(color = "green", size = 3) +
  labs(title = "Media de Precios del Aceite de Oliva en Europa",
       x = "Año",
       y = "Precio Medio (Euros/Moneda local)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#Y ahora vamos a poner en el mismo gráfico los datos de España también
# Filtrar los datos para "Olive oil" en España
data_olive_spain <- data_cleaned %>%
  filter(prod_veg == "Extra virgin olive oil - prices per 100 litres", geo == "Spain") %>%
  group_by(TIME_PERIOD) %>%
  summarise(Spain_Price = mean(OBS_VALUE, na.rm = TRUE))

# Calcular la media europea para "Olive oil"
data_olive_europe <- data_cleaned %>%
  filter(prod_veg == "Extra virgin olive oil - prices per 100 litres") %>%
  group_by(TIME_PERIOD) %>%
  summarise(Europe_Mean = mean(OBS_VALUE, na.rm = TRUE))

# Combinar los dos datasets
combined_data <- data_olive_europe %>%
  left_join(data_olive_spain, by = "TIME_PERIOD")

# Crear el gráfico con ambas líneas
ggplot(combined_data, aes(x = TIME_PERIOD)) +
  geom_line(aes(y = Europe_Mean, color = "Media Europea"), size = 1.2) +
  geom_line(aes(y = Spain_Price, color = "España"), size = 1.2) +
  labs(title = "Evolución de Precios del Aceite de Oliva: España vs Media Europea",
       x = "Año",
       y = "Precio (Euros/Moneda local)",
       color = "Leyenda") +
  scale_color_manual(values = c("Media Europea" = "blue", "España" = "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
