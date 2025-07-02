# ==============================================================================
# ANÁLISIS DE CAPACIDAD INSTALADA INDUSTRIAL - INDEC
# ==============================================================================
# Este script descarga, procesa y visualiza datos de utilización de capacidad 
# instalada en la industria argentina, creando gráficos slope para diferentes períodos

# ------------------------------------------------------------------------------
# 1. CARGA DE LIBRERÍAS NECESARIAS
# ------------------------------------------------------------------------------
pacman::p_load(tidyverse,  # Para manipulación de datos (dplyr, ggplot2, etc.)
               readxl,     # Para leer archivos Excel
               magrittr,   # Para operadores pipe adicionales (%<>%, %$%, etc.)
               janitor)    # Para limpieza de datos (clean_names, etc.)

# ------------------------------------------------------------------------------
# 2. DESCARGA DE DATOS DESDE INDEC
# ------------------------------------------------------------------------------
# Crear archivo temporal para almacenar el Excel descargado
temp <- tempfile()

# Descargar archivo de capacidad instalada desde el FTP del INDEC
# Modo "wb" (write binary) es necesario para archivos Excel
download.file("https://www.indec.gob.ar/ftp/cuadros/economia/sh_capacidad_03_20.xls", 
              temp, mode = "wb")

# Leer la hoja específica "UCI - NG y bloques" del archivo Excel
capacidad_instalada_orig <- read_excel(temp, sheet = "UCI - NG y bloques")

# ------------------------------------------------------------------------------
# 3. LIMPIEZA Y ESTRUCTURACIÓN INICIAL DE DATOS
# ------------------------------------------------------------------------------
capacidad_instalada <- capacidad_instalada_orig %>% 
  drop_na() %>%                    # Eliminar filas con valores faltantes
  set_colnames(.[1, ]) %>%         # Usar la primera fila como nombres de columnas
  slice(2:n())                     # Mantener desde la fila 2 hasta el final

# ------------------------------------------------------------------------------
# 4. ASIGNACIÓN DE AÑOS A LOS DATOS
# ------------------------------------------------------------------------------
# Los datos están organizados por meses consecutivos, necesitamos identificar el año
# Crear columna de año inicialmente vacía
capacidad_instalada$anio <- NA

# Asignar años según la posición de las filas (12 meses por año)
capacidad_instalada$anio[1:12] <- 2016      # Filas 1-12: año 2016
capacidad_instalada$anio[13:24] <- 2017     # Filas 13-24: año 2017
capacidad_instalada$anio[25:36] <- 2018     # Filas 25-36: año 2018
capacidad_instalada$anio[37:48] <- 2019     # Filas 37-48: año 2019
capacidad_instalada$anio[49:length(capacidad_instalada$anio)] <- 2020  # Resto: año 2020

# ------------------------------------------------------------------------------
# 5. TRANSFORMACIÓN A FORMATO LARGO (TIDY DATA)
# ------------------------------------------------------------------------------
capacidad_instalada <- capacidad_instalada %>% 
  # Seleccionar columnas relevantes y renombrar 'Período' como 'Año'
  select(anio, Año = Período, `Nivel general`:`Metalmecánica excluida industria automotriz`) %>% 
  
  # Convertir todas las columnas de sectores a numéricas
  mutate_at(vars(`Nivel general`:`Metalmecánica excluida industria automotriz`), 
            ~ as.numeric(as.character(.))) %>% 
  
  # Transformar de formato ancho a largo: cada sector en una fila
  gather(., key = "Sector", value = "Valor", 3:length(.)) %>% 
  
  # Crear identificador único: "Mes_Año" (ej: "Enero_2016")
  mutate(Año = paste0(str_remove(Año, "[*]"), "_", anio)) %>% 
  
  # Eliminar columna temporal de año
  select(-anio)

# ------------------------------------------------------------------------------
# 6. CARGA DE FUNCIONES PARA GRÁFICOS SLOPE
# ------------------------------------------------------------------------------
# Cargar funciones personalizadas para crear gráficos estilo Tufte desde GitHub
source("https://github.com/pablotis/capacidad_instalada_indec/raw/master/funciones_tufte_slope.R")  

# Crear directorio para guardar los gráficos
dir.create("Gráficos")

# ==============================================================================
# GENERACIÓN DE GRÁFICOS SLOPE
# ==============================================================================

# ------------------------------------------------------------------------------
# 7. GRÁFICO 1: COMPARACIÓN ENERO 2016 VS ENERO 2020 (4 años de diferencia)
# ------------------------------------------------------------------------------
# Filtrar solo los datos de Enero 2016 y Enero 2020 para comparar extremos
df <- capacidad_instalada %>% 
  filter(Año %in% c("Enero_2016", "Enero_2020")) %>% 
  arrange(Año)

# Preparar datos para el gráfico slope usando función personalizada
df <- tufte_sort(df, 
                 x="Año",           # Variable del eje X (tiempo)
                 y="Valor",         # Variable del eje Y (capacidad utilizada)
                 group="Sector",    # Variable de agrupación (sectores industriales)
                 method="tufte",    # Método de ordenamiento estilo Tufte
                 min.space=0.05)   # Espacio mínimo entre líneas

# Convertir a factor y redondear valores para mejor presentación
df <- transform(df, 
                x=factor(x, levels=c("Enero_2016", "Enero_2020"), 
                         labels=c("Enero_2016", "Enero_2020")), 
                y=round(y,1))

# Generar y guardar el gráfico
png("Gráficos/Capacidad Instalada Corte Enero_16_20.png", 
    width = 8, height = 9, units = 'in', res = 300)
plot_slopegraph(df) + 
  labs(title = "Utilización (%) de la capacidad instalada en la industria según
       bloques sectoriales.",
       subtitle = "Período Enero 2016 - 2020.",
       caption = "Fuente: Elaboración propia en base al INDEC.") + 
  tema  # Tema personalizado definido en las funciones cargadas
dev.off()

# ------------------------------------------------------------------------------
# 8. GRÁFICO 2: COMPARACIÓN ENERO 2019 VS ENERO 2020 (1 año de diferencia)
# ------------------------------------------------------------------------------
# Filtrar datos consecutivos para analizar cambio año a año
df <- capacidad_instalada %>% 
  filter(Año %in% c("Enero_2019", "Enero_2020")) %>% 
  arrange(Año)

# Aplicar misma preparación de datos
df <- tufte_sort(df, 
                 x="Año", 
                 y="Valor", 
                 group="Sector", 
                 method="tufte", 
                 min.space=0.05)

df <- transform(df, 
                x=factor(x, levels=c("Enero_2019", "Enero_2020"), 
                         labels=c("Enero_2019", "Enero_2020")), 
                y=round(y,1))

# Generar y guardar el gráfico
png("Gráficos/Capacidad Instalada Corte Enero_19_20.png", 
    width = 8, height = 9, units = 'in', res = 300)
plot_slopegraph(df) + 
  labs(title = "Utilización (%) de la capacidad instalada en la industria según
       bloques sectoriales.",
       subtitle = "Período Enero 2019 - 2020.",
       caption = "Fuente: Elaboración propia en base al INDEC.") + 
  tema
dev.off()

# ------------------------------------------------------------------------------
# 9. GRÁFICO 3: SERIE TEMPORAL ENERO 2016-2020 (evolución año a año)
# ------------------------------------------------------------------------------
# Incluir todos los eneros para mostrar tendencia completa
df <- capacidad_instalada %>% 
  filter(Año %in% c("Enero_2016", "Enero_2017", "Enero_2018", "Enero_2019", "Enero_2020")) %>% 
  arrange(Año)

# Preparar datos para gráfico multi-temporal
df <- tufte_sort(df, 
                 x="Año", 
                 y="Valor", 
                 group="Sector", 
                 method="tufte", 
                 min.space=0.05)

# Definir orden cronológico correcto para los 5 años
df <- transform(df, 
                x=factor(x, levels=c("Enero_2016", "Enero_2017", 
                                     "Enero_2018", "Enero_2019", 
                                     "Enero_2020"), 
                         labels=c("Enero_2016", "Enero_2017", 
                                  "Enero_2018", "Enero_2019",
                                  "Enero_2020")), 
                y=round(y,1))

# Generar y guardar el gráfico de serie temporal
png("Gráficos/Capacidad Instalada serie Enero_16_20.png", 
    width = 8, height = 9, units = 'in', res = 300)
plot_slopegraph(df) + 
  labs(title = "Utilización (%) de la capacidad instalada en la industria según
       bloques sectoriales.",
       subtitle = "Serie Enero de 2016 a 2020.",
       caption = "Fuente: Elaboración propia en base al INDEC.") + 
  tema
dev.off()

