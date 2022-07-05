# Lançamentos imobiliários no Rio de Janeiro (Zap Imóveis)

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


# Lançamentos imobiliários por bairro
bairros_imob <- dados %>%
  group_by(zona_de_valor) %>%
  count() %>%
  arrange(desc(n))


# Lançamentos imobiliários residenciais por bairro
bairros_res <- dados %>%
  filter(tipologia %in% c('Res. Vertical',
                          'Res. Horizontal',
                          'Retrofit Residencial')) %>%
  group_by(zona_de_valor) %>%
  count() %>%
  arrange(desc(n))

# Lançamentos imobiliários por ano
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


# consultar os ceps dos imóveis residenciais
dados_espaciais <- dados %>%
  filter(tipologia %in% c('Res. Vertical',
                          'Res. Horizontal',
                          'Retrofit Residencial')) %>%
  group_by(cep) %>%
  summarise(total_unidades = sum(no_total_de_unidades)) %>%
  mutate(cep_digitos = str_count(cep),
         cep = case_when(cep_digitos == 8 ~ cep,
                         cep_digitos == 7 ~ str_c(cep, '0'))) # add 0 ao final dos ceps com 7 dígitos

ceps_rj <- unique(dados_espaciais$cep)

# library(cepR)
# token <- ''

# obs: se todos os ceps não tiverem 8 dígitos, não roda
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


# Os dados faltantes estão apenas nos mapas, nas tabelas estão presentes todos os dados.
# Ceps não encontrados (proporção sobre o total): 9,8% do total de unidades
sum(ceps_google$total_unidades, na.rm = T)/
sum(dados_espaciais$total_unidades, na.rm = T)

# Ceps não encontrados (proporção sobre o total): 7,6% do total de lançamentos residenciais
52/680

# remover dados faltantes para fazer o mapa
dados_espaciais <- dados_espaciais %>% 
  na.omit() %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%   # Falta resolver os NAs manualmente!
  st_set_crs(4326) %>% 
  st_transform(crs = 4674)


# tentar achar os CEPS

shape_rio <- geobr::read_municipality(code_muni = 3304557)


# Mapa dos lançamentos imobiliários na cidade do Rio de Janeiro

ggplot() +
  geom_sf(data = shape_rio) +
  geom_sf(data = dados_espaciais, 
          aes(fill = total_unidades), 
          size = .4, 
          alpha= .3) +
  annotation_north_arrow(location='tl', 
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = 'br')+
  theme_classic() +
  theme(legend.position = 'none') 
  
  
  
# Mapa dos lançamentos imobiliários ponderado pela quantidade de unidades
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



