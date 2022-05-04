# ITBI no entorno da Transcarioca 

options(scipen = 9999)
source('Rscripts/01_itbi.R') # inclui as bibliotecas que serão usadas


# ---------------------------------------------------

# importar dados das estações
estacoes_BRT <- sf::st_read('input/estacoes-brt/estacoes_BRT.shp')

# 4 - análise da transcarioca (obras de 2011 a 2014) --------------------------------------------------

# ver bairros com estações da transcarioca com dados disponíveis de 2010 até 2014.
brt_carioca <- estacoes_BRT %>% 
  janitor::clean_names() %>% 
  dplyr::filter(flg_trans_c == 1)

bairros <- geobr::read_neighborhood() %>% 
  dplyr::filter(code_muni == '3304557')

st_crs(bairros)
st_crs(brt_carioca)

brt_carioca <- st_transform(brt_carioca, 4674)

bairros_brt_c <- st_intersection(bairros, brt_carioca)

estacoes_carioca <- bairros_brt_c %>% 
  group_by(name_neighborhood) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  mutate(name_neighborhood = tolower(name_neighborhood))

bairros_carioca <- itbi_m2_bairro %>% 
  mutate(bairro = tolower(bairro)) %>% 
  dplyr::filter(bairro %in% estacoes_carioca$name_neighborhood) %>% 
  select(codbairro) %>% 
  as_vector()


itbi_m2 <- itbi_m2_bairro %>% 
  dplyr::filter(codbairro %in% bairros_carioca,
                x2010 != 0) 

# dos 17 bairros cortados pela transcarioca, tenho dados para 13 bairros. Mesmo assim, engloba a maior parte das estações.
# Não existem dados de ITBI para o Galeão e para a Cidade Universitária. Dados escassos para Maré e Vaz Lobo não tem dados de ITBI para 2010 



# cálculo da variação anual do valor do m² para cada bairro que tem estações do BRT transcarioca
var_carioca <- itbi_m2 %>%
  mutate(var_2011 = round(x2011/x2010, 2),
         var_2012 = round(x2012/x2011, 2),
         var_2013 = round(x2013/x2012, 2),
         var_2014 = round(x2014/x2013, 2),
         var_2015 = round(x2015/x2014, 2),
         var_2016 = round(x2016/x2015, 2),
         var_2017 = round(x2017/x2016, 2),
         var_2018 = round(x2018/x2017, 2),
         var_2019 = round(x2019/x2018, 2),
         var_2020 = round(x2020/x2019, 2),
         var_2021 = round(x2021/x2020, 2)
  ) %>% 
  select(1, 2, 15:25)

# https://docs.opentripplanner.org/en/latest/Basic-Tutorial/
# https://itdpbrasil.org/tutorial-saiba-como-calcular-o-pnt/ Ver os dois últimos vídeos do PNT


# calcular o entorno das estações ----------------------------------------------


# install.packages("opentripplanner") 
# importante!!! Usar versão 1.5 do OTP e versão 8 do Java por questões de funções que não funcionam

path_data <- file.path('temp/', "OTP") # Make a folder to store the data 
dir.create(path_data)  

# Download do OTP (precisa ser a versão acima da 2.0 para ser compatível com o java)
# path_otp <- otp_dl_jar(path_data, version = '1.5.0', cache = F) 
otp_check_java(otp_version = 1.5)

# função para verificar se está funcionando?
opentripplanner:::otp_checks(otp = 'temp/OTP/otp-1.5.0-shaded.jar',
                             dir = 'temp',
                             router = 'default',
                             otp_version = 1.5)

options(java.parameters = "-Xmx8G") # R permite que o java use até 8gb de ram

# eu preciso dos arquivos do otp na extensão jar e o arquivo da cidade do rio de janeiro osm no formato pbf


# rodar apenas uma vez
# construir o grafo que representa a rede da malha viária do Rio e que permite o roteamento
# log <- otp_build_graph(otp = 'temp/OTP/otp-1.5.0-shaded.jar',
#                        memory = 8000, # mem ram para fazer a análise
#                        dir = 'temp', # diretório para salvar o graph
#                        router = 'default',
#                        otp_version = 1.5) # área que quero baixar (rio de Janeiro)

otp_setup(otp = 'temp/OTP/otp-1.5.0-shaded.jar',
          dir = 'temp',
          router = 'default',
          port = 8080,
          wait = F)

otp_rj <- otp_connect()

brt_carioca <- brt_carioca %>% 
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>% 
  select(objectid, nome, lat, lon) 

# criar o entorno das estações a partir do tempo de caminhada.
temp_min <- 15
temp_seg <- temp_min * 60

area_15min <- otp_isochrone(otpcon = otp_rj,
                            fromPlace = brt_carioca, # local de partida
                            cutoffSec = temp_seg,    # tempo de viagem em segundos
                            mode = 'WALK',
                            fromID = brt_carioca$nome
) 

# checar área em 15 min
mapview::mapview(area_15min)

# area até 45 min
temp_min <- 45
temp_seg <- temp_min * 60

area_45min <- otp_isochrone(otpcon = otp_rj,
                            fromPlace = brt_carioca, # local de partida
                            cutoffSec = temp_seg,    # tempo de viagem em segundos
                            mode = 'WALK',
                            fromID = brt_carioca$nome
)

# checar área em 45 min
mapview::mapview(area_45min)



# Separar ruas acessíveis até 15 min das estações ------------------------------------------------------------------
st_crs(area_15min)
st_crs(shape_ruas)

shape_ruas <- st_transform(shape_ruas, crs = 4326)
sf_use_s2(F)
df1 <- st_intersection(shape_ruas, area_15min) 

# plot(df1$geometry)
# mapview::mapview(df1)

# Separar bairros que tenho dados de 2010 até 2014
df1 <- left_join(df1, itbi_m2_rua, by = 'cl') %>% 
  na.omit() %>% 
  dplyr::filter(x2010 != 0,
                x2011 != 0,
                x2012 != 0,
                x2013 != 0,
                x2014 != 0
                ) %>%  
  select(cl, logradouro, codbairro, bairro, x2010,
         x2011, x2012, x2013, x2014) %>% 
  group_by(cl, logradouro, codbairro, bairro, x2010,
           x2011, x2012, x2013, x2014) %>% 
  summarise(geometry = st_union(geometry))

mapview::mapview(df1)

# remover bairros com menos de 5 ruas 
ruas_p_bairro_15 <- df1 %>% 
  group_by(bairro) %>% 
  count() %>% 
  st_drop_geometry() %>% 
  dplyr::filter(n >= 5)

# agrupar valores das ruas acima por bairro
df_15_min <- df1 %>% 
  group_by(bairro) %>% 
  summarise(across(starts_with("x"), ~ mean(., na.rm = T)),
            across(starts_with("x"), ~ round(., 0))) %>% 
  mutate(var2011 = (x2011 / x2010) -1,
         var2012 = (x2012 / x2011) -1,
         var2013 = (x2013 / x2012) -1,
         var2014 = (x2014 / x2013) -1,
         tx_med_anual_11_14 = (var2011 + var2012 + var2013 + var2014) / 4) %>% 
  dplyr::filter(bairro %in% ruas_p_bairro_15$bairro) %>% 
  group_by(bairro, x2010, x2011, x2012, x2013, x2014, tx_med_anual_11_14) %>% 
  summarise(across(starts_with("var"), ~ round(., 2))) %>% 
  mutate(tx_med_anual_11_14 = round(tx_med_anual_11_14, 2)) %>% 
  arrange(desc(tx_med_anual_11_14))

# ver shape dos 15 min
mapview::mapview(df_15_min)

# Separar ruas acessíveis até 45 min das estações ------------------------------------------------------
df2 <- st_intersection(shape_ruas, st_buffer(area_45min, 0))
# razão de usar o buffer https://gis.stackexchange.com/questions/163445/getting-topologyexception-input-geom-1-is-invalid-which-is-due-to-self-intersec

df2 <- left_join(df2, itbi_m2_rua, by = 'cl') %>% 
  na.omit() %>% 
  dplyr::filter(x2010 != 0,
                x2011 != 0,
                x2012 != 0,
                x2013 != 0,
                x2014 != 0
  ) %>%  
  select(cl, logradouro, codbairro, bairro, x2010,
         x2011, x2012, x2013, x2014) %>% 
  group_by(cl, logradouro, codbairro, bairro, x2010,
           x2011, x2012, x2013, x2014) %>% 
  summarise(geometry = st_union(geometry))

mapview::mapview(df2)

# remover bairros com menos de 5 ruas 
ruas_p_bairro_45 <- df2 %>% 
  group_by(bairro) %>% 
  count() %>% 
  st_drop_geometry() %>% 
  dplyr::filter(n >= 5)

# agrupar valores das ruas acima por bairro
df_45_min <- df2 %>% 
  group_by(bairro) %>% 
  summarise(across(starts_with("x"), ~ mean(., na.rm = T)),
            across(starts_with("x"), ~ round(., 0))) %>% 
  mutate(var2011 = (x2011 / x2010) -1,
         var2012 = (x2012 / x2011) -1,
         var2013 = (x2013 / x2012) -1,
         var2014 = (x2014 / x2013) -1,
         tx_med_anual_11_14 = (var2011 + var2012 + var2013 + var2014) / 4) %>% 
  dplyr::filter(bairro %in% ruas_p_bairro_45$bairro) %>% 
  group_by(bairro, x2010, x2011, x2012, x2013, x2014, tx_med_anual_11_14) %>% 
  summarise(across(starts_with("var"), ~ round(., 2))) %>% 
  mutate(tx_med_anual_11_14 = round(tx_med_anual_11_14, 2)) %>% 
  arrange(desc(tx_med_anual_11_14))

# ver shape dos 15 min
mapview::mapview(df_45_min)


# calcular variação anual em cada bairro
df_bairros <- itbi_m2_bairro %>% 
  dplyr::filter(x2010 != 0,
                x2011 != 0,
                x2012 != 0,
                x2013 != 0,
                x2014 != 0
  ) %>%  
  ungroup() %>% 
  select(bairro, x2010, x2011, x2012, x2013, x2014) %>% 
  group_by(bairro, x2010, x2011, x2012, x2013, x2014) %>% 
  mutate(var2011 = (x2011 / x2010) -1,
         var2012 = (x2012 / x2011) -1,
         var2013 = (x2013 / x2012) -1,
         var2014 = (x2014 / x2013) -1,
         tx_med_anual_11_14 = (var2011 + var2012 + var2013 + var2014) / 4) %>% 
         group_by(bairro, x2010, x2011, x2012, x2013, x2014, tx_med_anual_11_14) %>% 
         summarise(across(starts_with("var"), ~ round(., 2))) %>% 
         mutate(tx_med_anual_11_14 = round(tx_med_anual_11_14, 2)) %>% 
         arrange(desc(tx_med_anual_11_14))


# Tabela final comparando dist 15 min, dist 45min e média do bairro (da taxa média de variação anual de 2011 até 2014). 
df_transcarioca

df_45_min
df_15_min
df_bairros

# recorte das variáveis de crescimento anual médio
df_15_min_rec <- df_15_min %>% select(bairro, tx_med_anual_11_14) %>% st_drop_geometry() %>% rename(tx_15m = tx_med_anual_11_14)
df_45_min_rec <- df_45_min %>% select(bairro, tx_med_anual_11_14) %>% st_drop_geometry() %>% rename(tx_45m = tx_med_anual_11_14)
df_bairros_rec <- df_bairros %>% ungroup() %>% select(bairro, tx_med_anual_11_14) %>% rename(tx_bairro = tx_med_anual_11_14)


df_transcarioca <- left_join(df_15_min_rec, df_45_min_rec, by = 'bairro') %>% 
  left_join(df_bairros_rec)
  

# Obras da transcarioca:   março /2011 até junho / 2014

# Foram analisados 9 bairros dos 17 bairros cortados pela transcarioca (considerando disponibilidade dos dados e existencia de dados de ITBI de pelo menos 5 ruas por bairro)

# Taxa Média de Crescimento Anual de 2011 até 2014 = somar as taxas e dividir pelo número de anos
# em df_trancarioca eu calculei a taxa média de crescimento anual para as ruas que acessam o brt em 15 min,
# 45 min e o total de ruas do bairro com dados do ITBI. 

# resultado: a valorização da área de entorno das estações do BRT transcarioca referentes aos bairros de: 

# "Madureira"       "Olaria"          "Barra da Tijuca" "Penha Circular"  "Vila da Penha"   "Jacarepaguá"    
# "Tanque"          "Praça Seca"      "Taquara" 

# Não tiveram uma taxa de valorização média de 2011 até 2014 acima do entorno de 45 min do BRT.
# Em todos os casos (bairros), ela foi abaixo da valorização do metro ² do barro como um todo.
# indica que de forma geral não houve valorização.

# fonte do arquivo pbf do rio de janeiro https://download.openstreetmap.fr/extracts/south-america/brazil/southeast/

