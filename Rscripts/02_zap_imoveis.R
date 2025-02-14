# Lan�amentos imobili�rios no Rio de Janeiro (Zap Im�veis)

# 1 - arregar bibliotecas ------------------------------------------------------
library(readxl)
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(sf)
library(ggmap)
library(ggspatial)

# 2 - Importar dados -----------------------------------------------------------
dados <- read_excel('input/dados zap imoveis/PesquisaMercadoCompleta_3669-zapimoveis.xlsx',
                    skip = 2) %>%
  janitor::clean_names() %>%
  mutate(ano = year(data_lancamento),
         cep = str_replace_all(cep, "[[:punct:]]", "")) %>% 
  filter(cidade != 'NITEROI')

summary(dados)


# Lan�amentos imobili�rios por bairro
bairros_imob <- dados %>%
  group_by(zona_de_valor) %>%
  count() %>%
  arrange(desc(n))


# Lan�amentos imobili�rios residenciais por bairro
bairros_res <- dados %>%
  filter(tipologia %in% c('Res. Vertical',
                          'Res. Horizontal',
                          'Retrofit Residencial')) %>%
  group_by(zona_de_valor) %>%
  count() %>%
  arrange(desc(n))

# Lan�amentos imobili�rios por ano
anos <- dados %>%
  group_by(ano) %>%
  count()

plot(y = anos$n,x = anos$ano, type = 'l')


# novas unidades residenciais por bairro
unidades_bairro <- dados %>%
  filter(tipologia %in% c('Res. Vertical',
                          'Res. Horizontal',
                          'Retrofit Residencial')) %>%
  group_by(zona_de_valor) %>%
  summarise(unidades_bairro = sum(no_total_de_unidades, na.rm = T)) %>%
  arrange(desc(unidades_bairro))


# consultar os ceps dos im�veis residenciais
dados_espaciais <- dados %>%
  filter(tipologia %in% c('Res. Vertical',
                          'Res. Horizontal',
                          'Retrofit Residencial')) %>%
  group_by(cep) %>%
  summarise(total_unidades = sum(no_total_de_unidades)) %>%
  mutate(cep_digitos = str_count(cep),
         cep = case_when(cep_digitos == 8 ~ cep,
                         cep_digitos == 7 ~ str_c(cep, '0'))) # add 0 ao final dos ceps com 7 d�gitos

ceps_rj <- unique(dados_espaciais$cep)

# library(cepR)
# token <- ''

# obs: se todos os ceps n�o tiverem 8 d�gitos, n�o roda
# ceps_espacial <- busca_multi(token = token,
#                              lista_ceps = ceps_rj)


# write.csv2(ceps_espacial,'ceps_zapimoveis.csv',row.names = F)
ceps_espacial <- read_csv2('input/ceps_zapimoveis.csv') %>% 
  na.omit() %>% 
  mutate(cep = as.character(cep)) 
   
dados_espaciais <- dados_espaciais %>%
  left_join(ceps_espacial)

# separar dados faltantes para usar na API do Google
ceps_google <- dados_espaciais %>% 
  filter(cidade %in% NA)


# Os dados faltantes est�o apenas nos mapas, nas tabelas est�o presentes todos os dados.
# Ceps n�o encontrados (propor��o sobre o total): 9,8% do total de unidades
sum(ceps_google$total_unidades, na.rm = T)/
sum(dados_espaciais$total_unidades, na.rm = T)

# Ceps n�o encontrados (propor��o sobre o total): 7,6% do total de lan�amentos residenciais
52/680

# remover dados faltantes para fazer o mapa
dados_espaciais <- dados_espaciais %>% 
  na.omit() %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%   # Falta resolver os NAs manualmente!
  st_set_crs(4326) %>% 
  st_transform(crs = 4674)


# tentar achar os CEPS

shape_rio <- geobr::read_municipality(code_muni = 3304557)


# Mapa dos lan�amentos imobili�rios na cidade do Rio de Janeiro
brt <- read_sf('input/trajeto_BRT/Trajetos_BRT.shp') %>% 
  filter(Nome != 'TransBrasil')

ggplot() +
  geom_sf(data = shape_rio) +
  geom_sf(data = dados_espaciais, 
          aes(fill = total_unidades), 
          size = .6, 
          alpha= .5) +
  geom_sf(data = brt, size = .5) +
  annotation_north_arrow(location='tl', 
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = 'br')+
  theme_classic() +
  theme(legend.position = 'none') 
  

ggsave(filename = 'output/mapa_brt_zap.png', dpi = 600)

  
# Mapa dos lan�amentos imobili�rios ponderado pela quantidade de unidades
ggplot() +
  geom_sf(data = shape_rio) +
  geom_sf(data = dados_espaciais, 
          aes(size = total_unidades), 
          alpha= .1) +
  annotation_north_arrow(location='tl', 
                         style = north_arrow_fancy_orienteering())+
  theme_minimal()+
  theme(legend.position = 'bottom', legend.direction = "horizontal") + theme_bw() + #daqui p baixo tira o grid do mapa
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = 'bottom',
        legend.direction = "horizontal") +
  guides(size = guide_legend(title = "Quantidade de unidades\nresidenciais:"))



