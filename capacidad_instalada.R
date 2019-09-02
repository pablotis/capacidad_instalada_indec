pacman::p_load(tidyverse, readxl, magrittr, janitor)

temp <- tempfile()
download.file("https://www.indec.gob.ar/ftp/cuadros/economia/sh_capacidad_08_19.xls", temp, mode = "wb")

capacidad_instalada_orig <- read_excel(temp, sheet = "UCI - NG y bloques")

capacidad_instalada <- capacidad_instalada_orig %>% 
  drop_na() %>%
  set_colnames(., capacidad_instalada[1, ]) %>% 
  slice(., 2:n())

capacidad_instalada$anio <- NA
capacidad_instalada$anio[1:12] <- 2016
capacidad_instalada$anio[13:24] <- 2017
capacidad_instalada$anio[25:36] <- 2018
capacidad_instalada$anio[37:length(capacidad_instalada$anio)] <- 2019

capacidad_instalada <- capacidad_instalada %>% 
  select(anio, Año = Período, `Nivel general`:`Metalmecánica excluida industria automotriz`) %>% 
  mutate_at(vars(`Nivel general`:`Metalmecánica excluida industria automotriz`), ~ as.numeric(as.character(.))) %>% 
  gather(., key = "Sector", value = "Valor", 3:length(.)) %>% 
  mutate(Año = paste0(str_remove(Año, "[*]"), "_", anio)) %>% 
  select(-anio)

# Cargo función para estructurar la base
source("https://github.com/pablotis/capacidad_instalada_indec/raw/master/funciones_tufte_slope.R")  

dir.create("Gráficos")

### Corte Meses entre puntas ------------------------------------------

df <- capacidad_instalada %>% 
  filter(Año %in% c("Junio_2016", "Junio_2019")) %>% 
  arrange(Año)

## Preparo data    
df <- tufte_sort(df, 
                 x="Año", 
                 y="Valor", 
                 group="Sector", 
                 method="tufte", 
                 min.space=0.05)

df <- transform(df, 
                x=factor(x, levels=c("Junio_2016", "Junio_2019"), 
                         labels=c("Junio_2016", "Junio_2019")), 
                y=round(y,1))


png("Gráficos/Capacidad Instalada Corte Junio_16_19.png", width = 8, height = 9, units = 'in', res = 300)
plot_slopegraph(df) + 
  labs(title = "Utilización (%) de la capacidad instalada en la industria según
       bloques sectoriales.",
       subtitle = "Período Junio 2016 - 2019.",
       caption = "Fuente: Elaboración propia en base al INDEC.") + 
  tema
dev.off()


### Corte Meses contínuos ------------------------------------------
df <- capacidad_instalada %>% 
  filter(Año %in% c("Junio_2018", "Junio_2019")) %>% 
  arrange(Año)

df <- tufte_sort(df, 
                 x="Año", 
                 y="Valor", 
                 group="Sector", 
                 method="tufte", 
                 min.space=0.05)

df <- transform(df, 
                x=factor(x, levels=c("Junio_2018", "Junio_2019"), 
                         labels=c("Junio_2018", "Junio_2019")), 
                y=round(y,1))


png("Gráficos/Capacidad Instalada Corte Junio_18_19.png", width = 8, height = 9, units = 'in', res = 300)
plot_slopegraph(df) + 
  labs(title = "Utilización (%) de la capacidad instalada en la industria según
       bloques sectoriales.",
       subtitle = "Período Junio 2018 - 2019.",
       caption = "Fuente: Elaboración propia en base al INDEC.") + 
  tema
dev.off()


### Corte Meses contínuos año a año ------------------------------------------
df <- capacidad_instalada %>% 
  filter(Año %in% c("Junio_2016", "Junio_2017", "Junio_2018", "Junio_2019")) %>% 
  arrange(Año)

df <- tufte_sort(df, 
                 x="Año", 
                 y="Valor", 
                 group="Sector", 
                 method="tufte", 
                 min.space=0.05)

df <- transform(df, 
                x=factor(x, levels=c("Junio_2016", "Junio_2017", 
                                     "Junio_2018", "Junio_2019"), 
                         labels=c("Junio_2016", "Junio_2017", 
                                  "Junio_2018", "Junio_2019")), 
                y=round(y,1))


png("Gráficos/Capacidad Instalada serie Junio_16_19.png", width = 8, height = 9, units = 'in', res = 300)
plot_slopegraph(df) + 
  labs(title = "Utilización (%) de la capacidad instalada en la industria según
       bloques sectoriales.",
       subtitle = "Serie Junio de 2016 a 2019.",
       caption = "Fuente: Elaboración propia en base al INDEC.") + 
  tema
dev.off()
