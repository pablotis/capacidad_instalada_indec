
#Fuente: Adaptación del script original de James Keirstead, quien tradujo a R el estilo de gráficos de 
### Edward Tufte: http://charliepark.org/slopegraphs/

require(pacman)
p_load(dplyr, ggplot2, openxlsx, reshape2, gghighlight)

theme_set(theme_classic())

#########################################################################################
##################################### Corte Meses entre puntas
#########################################################################################

### Cargar base de datos en formato plano quedandonos con los meses/años en cuestión.
## Por hacer: automatizar descarga de base
## Por hacer: Armar tabla plana

#source_df <- read.csv("base.csv")

# Define functions. Primary Source: https://github.com/jkeirstead/r-slopegraph
# Adaptatio by pablotis -->
source("funciones_tufte_slope.r")

## Prepare data    
df <- tufte_sort(df, 
                 x="Año", 
                 y="Valor", 
                 group="Sector", 
                 method="tufte", 
                 min.space=0.05)

df <- transform(df, 
                x=factor(x, levels=c("Marzo_2016", "Marzo_2019"), 
                         labels=c("Marzo_2016", "Marzo_2019")), 
                y=round(y,1))

png("Capacidad Instalada Corte marzo_16_19.png", width = 8, height = 9, units = 'in', res = 300)
plot_slopegraph(df) + 
  labs(title = "Utilización (%) de la capacidad instalada en la industria según
       bloques sectoriales.",
       subtitle = "Período Marzo 2016 - 2019.",
       caption = "Fuente: Elaboración propia en base al INDEC.") + 
  tema
dev.off()


#########################################################################################
##################################### Corte Meses consecutivos
#########################################################################################

### Cargar base de datos en formato plano quedandonos con los meses/años en cuestión.
## Por hacer: automatizar descarga de base
## Por hacer: Armar tabla plana

#source_df <- read.csv("base.csv")

# Define functions. Primary Source: https://github.com/jkeirstead/r-slopegraph
# Adaptatio by pablotis -->
source("funciones_tufte_slope.r")

## Prepare data    
df <- tufte_sort(df, 
                 x="Año", 
                 y="Valor", 
                 group="Sector", 
                 method="tufte", 
                 min.space=0.05)

df <- transform(df, 
                x=factor(x, levels=c("Marzo_2018", "Marzo_2019"), 
                         labels=c("Marzo_2018", "Marzo_2019")), 
                y=round(y,1))

png("Capacidad Instalada Serie marzo_18_19.png", width = 8, height = 9, units = 'in', res = 300)
plot_slopegraph(df) + 
  labs(title = "Utilización (%) de la capacidad instalada en la industria según
       bloques sectoriales.",
       subtitle = "Período Serie Marzo 2018 - 2019.",
       caption = "Fuente: Elaboración propia en base al INDEC.") + 
  tema
dev.off()


#########################################################################################
##################################### Serie Meses consecutivos
#########################################################################################

### Cargar base de datos en formato plano quedandonos con los meses/años en cuestión.
## Por hacer: automatizar descarga de base
## Por hacer: Armar tabla plana

#source_df <- read.csv("base.csv")

# Define functions. Primary Source: https://github.com/jkeirstead/r-slopegraph
# Adaptatio by pablotis -->
source("funciones_tufte_slope.r")

## Prepare data    
df <- tufte_sort(df, 
                 x="Año", 
                 y="Valor", 
                 group="Sector", 
                 method="tufte", 
                 min.space=0.05)

df <- transform(df, 
                x=factor(x, levels=c("Marzo_2016", "Marzo_2017", "Marzo_2018", 
                                     "Marzo_2019"), 
                         labels=c("Marzo_2016", "Marzo_2017", "Marzo_2018", 
                                  "Marzo_2019")), 
                y=round(y,1))

png("Capacidad Instalada Serie marzo_16_19.png", width = 8, height = 9, units = 'in', res = 300)
plot_slopegraph(df) + 
  labs(title = "Utilización (%) de la capacidad instalada en la industria según
       bloques sectoriales.",
       subtitle = "Período Serie Marzo 2016 - 2019.",
       caption = "Fuente: Elaboración propia en base al INDEC.") + 
  tema
dev.off()
