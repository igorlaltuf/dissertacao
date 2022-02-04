# Mapa corredores BRT da cidade do Rio de Janeiro
library(readxl)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(ggspatial)
library(geobr)
options(scipen = 99999)

# carregar shapefiles
malha.rj <- read_municipality(code_muni = 3304557) %>% 
  janitor::clean_names()

brt <- read_sf('input/trajeto_BRT/Trajetos_BRT.shp') %>% 
  janitor::clean_names()

brt_oeste <- brt %>% dplyr::filter(nome %in% 'TransOeste')
brt_carioca <- brt %>% dplyr::filter(nome %in% 'TransCarioca')
brt_olimpica <- brt %>% dplyr::filter(nome %in% 'TransOlímpica')
brt_brasil <- brt %>% dplyr::filter(nome %in% 'TransBrasil')

# mapa
ggplot(malha.rj) + 
  geom_sf(size = 0.05, fill = NA) +
  geom_sf(data = brt_oeste, aes(col = 'Transoeste'), size = 0.75, show.legend = 'line')+ # Esse col dentro do AES define o nome da legenda
  geom_sf(data = brt_carioca, aes(col = 'Transcarioca'), size = 0.75, show.legend = 'line')+ # Para o show legend line funcionar, todos os modais devem ter este atributo.
  geom_sf(data = brt_olimpica, aes(col = 'Transolímpica'), size = 0.75, show.legend = 'line')+
  geom_sf(data = brt_brasil, aes(col = 'Transbrasil'), size = 0.75, show.legend = 'line')+
  scale_color_manual(values = c("#D34945","#9045D3","#45CFD3","#88D345")) +
  labs(col = "") + # Nome das legendas
  labs(fill = '') + #Muda o nome da legenda com o fill.
  guides(colour = guide_legend(title.position = "top"))+
  annotation_scale(location = 'br')+ #Adiciona escala
  coord_sf(crs = 4674) +
  annotation_north_arrow(location='tl', 
                         style = north_arrow_fancy_orienteering())+
  theme_minimal()+
  theme(legend.position = 'bottom', legend.direction = "horizontal") + theme_bw() + #daqui p baixo tira o grid do mapa
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = 'bottom',
        legend.direction = "horizontal")

ggsave(path = 'output/', filename = 'mapa_brt_rj.png', dpi = 300)
