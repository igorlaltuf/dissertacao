# ITBI no entorno da Transoeste

options(scipen = 9999)
source('Rscripts/01_itbi.R') # inclui as bibliotecas que serão usadas

# ---------------------------------------------------

# importar dados das estações
estacoes_BRT <- sf::st_read('input/estacoes-brt/estacoes_BRT.shp')

# 4 - análise da transoeste (obras de 2010 a 2012) --------------------------------------------------
# ver bairros com estações da transoeste com dados disponíveis de 2010 até 2012.
brt_oeste <- estacoes_BRT %>% 
  janitor::clean_names() %>% 
  dplyr::filter(flg_trans_o == 1)


bairros <- geobr::read_neighborhood() %>% 
  dplyr::filter(code_muni == '3304557')

st_crs(bairros)
st_crs(brt_oeste)

brt_oeste <- st_transform(brt_oeste, 4674)

bairros_brt_o <- st_intersection(bairros, brt_oeste)

estacoes_oeste <- bairros_brt_o %>% 
  group_by(name_neighborhood) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  mutate(name_neighborhood = tolower(name_neighborhood))

bairros_oeste <- itbi_m2_bairro %>% 
  mutate(bairro = tolower(bairro)) %>% 
  dplyr::filter(bairro %in% estacoes_oeste$name_neighborhood) %>% 
  select(codbairro) %>% 
  as_vector()


itbi_m2 <- itbi_m2_bairro %>% 
  dplyr::filter(codbairro %in% bairros_oeste,
                x2010 != 0) 




# Calcular área de entorno --------------------------------------------------------------------------------
# importante: Usar versão 1.5 do OTP e versão 8 do Java por questões de funções que não funcionam

path_data <- file.path('temp/', "OTP") # Make a folder to store the data
dir.create(path_data)

# Download do OTP (precisa ser a versão acima da 2.0 para ser compatível com o java)
# path_otp <- otp_dl_jar(path_data, version = '1.5.0', cache = F) 
# otp_check_java(otp_version = 1.5)

# função para verificar se está funcionando?
opentripplanner:::otp_checks(otp = 'temp/OTP/otp-1.5.0-shaded.jar',
                             dir = 'temp',
                             router = 'default',
                             otp_version = 1.5)

options(java.parameters = "-Xmx8G") # R permite que o java use até 8gb de ram

# eu preciso dos arquivos do otp na extensão jar e o arquivo da cidade do rio de janeiro osm no formato pbf

# rodar apenas uma vez
# construir o grafo que representa a rede da malha viária do Rio e que permite o roteamento
log <- otp_build_graph(otp = 'temp/OTP/otp-1.5.0-shaded.jar',
                       memory = 8000, # mem ram para fazer a análise
                       dir = 'temp', # diretório para salvar o graph
                       router = 'default',
                       otp_version = 1.5) # área que quero baixar (rio de Janeiro)

otp_setup(otp = 'temp/OTP/otp-1.5.0-shaded.jar',
          dir = 'temp',
          router = 'default',
          port = 8080,
          wait = F)


otp_rj <- otp_connect()


brt_oeste <- brt_oeste %>% 
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>% 
  select(objectid, nome, lat, lon)


# criar o entorno das estações a partir do tempo de caminhada.
temp_min <- 15
temp_seg <- temp_min * 60

area_15min <- otp_isochrone(otpcon = otp_rj,
                            fromPlace = brt_oeste, # local de partida
                            cutoffSec = temp_seg,    # tempo de viagem em segundos
                            mode = 'WALK',
                            fromID = brt_oeste$nome) 


# checar área em 15 min
mapview::mapview(area_15min)


# area até 45 min
temp_min <- 45
temp_seg <- temp_min * 60

area_45min <- otp_isochrone(otpcon = otp_rj,
                            fromPlace = brt_oeste, # local de partida
                            cutoffSec = temp_seg,    # tempo de viagem em segundos
                            mode = 'WALK',
                            fromID = brt_oeste$nome
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

# Separar bairros que tenho dados de 2010 até 2012
df1 <- left_join(df1, itbi_m2_rua, by = 'cl') %>% 
  na.omit() %>% 
  dplyr::filter(x2010 != 0,
                x2011 != 0,
                x2012 != 0
  ) %>%  
  select(cl, logradouro, codbairro, bairro, x2010, x2011, x2012) %>% 
  group_by(cl, logradouro, codbairro, bairro, x2010, x2011, x2012) %>% 
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
         tx_med_anual_11_12 = (var2011 + var2012) / 2) %>% 
  dplyr::filter(bairro %in% ruas_p_bairro_15$bairro) %>% 
  group_by(bairro, x2010, x2011, x2012, tx_med_anual_11_12) %>% 
  summarise(across(starts_with("var"), ~ round(., 2))) %>% 
  mutate(tx_med_anual_11_12 = round(tx_med_anual_11_12, 2)) %>% 
  arrange(desc(tx_med_anual_11_12))

# ver shape dos 15 min
mapview::mapview(df_15_min)



# Separar ruas acessíveis até 45 min das estações ------------------------------------------------------
df2 <- st_intersection(shape_ruas, st_buffer(area_45min, 0))

df2 <- left_join(df2, itbi_m2_rua, by = 'cl') %>% 
  na.omit() %>% 
  dplyr::filter(x2010 != 0,
                x2011 != 0,
                x2012 != 0
  ) %>%  
  select(cl, logradouro, codbairro, bairro, x2010, x2011, x2012) %>% 
  group_by(cl, logradouro, codbairro, bairro, x2010, x2011, x2012) %>% 
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
         tx_med_anual_11_12 = (var2011 + var2012) / 2) %>% 
  dplyr::filter(bairro %in% ruas_p_bairro_15$bairro) %>% 
  group_by(bairro, x2010, x2011, x2012, tx_med_anual_11_12) %>% 
  summarise(across(starts_with("var"), ~ round(., 2))) %>% 
  mutate(tx_med_anual_11_12 = round(tx_med_anual_11_12, 2)) %>% 
  arrange(desc(tx_med_anual_11_12))

# ver shape dos 15 min
mapview::mapview(df_45_min)


# calcular variação anual em cada bairro
df_bairros <- itbi_m2_bairro %>% 
  dplyr::filter(x2010 != 0,
                x2011 != 0,
                x2012 != 0
  ) %>%  
  ungroup() %>% 
  select(bairro, x2010, x2011, x2012) %>% 
  group_by(bairro, x2010, x2011, x2012) %>% 
  mutate(var2011 = (x2011 / x2010) -1,
         var2012 = (x2012 / x2011) -1,
         tx_med_anual_11_12 = (var2011 + var2012) / 2) %>% 
  group_by(bairro, x2010, x2011, x2012, tx_med_anual_11_12) %>% 
  summarise(across(starts_with("var"), ~ round(., 2))) %>% 
  mutate(tx_med_anual_11_12 = round(tx_med_anual_11_12, 2)) %>% 
  arrange(desc(tx_med_anual_11_12))

# Tabela final comparando dist 15 min, dist 45min e média do bairro (da taxa média de variação anual de 2011 até 2014). 
df_15_min
df_45_min
df_bairros

# recorte das variáveis de crescimento anual médio
df_15_min_rec <- df_15_min %>% select(bairro, tx_med_anual_11_12) %>% st_drop_geometry() %>% rename(tx_15m = tx_med_anual_11_12)
df_45_min_rec <- df_45_min %>% select(bairro, tx_med_anual_11_12) %>% st_drop_geometry() %>% rename(tx_45m = tx_med_anual_11_12)
df_bairros_rec <- df_bairros %>% ungroup() %>% select(bairro, tx_med_anual_11_12) %>% rename(tx_bairro = tx_med_anual_11_12)


df_transoeste <- left_join(df_15_min_rec, df_45_min_rec, by = 'bairro') %>% 
  left_join(df_bairros_rec)

# nos três bairros analisados a valorização foi menor no entorno do que no total do bairro.

# Tenho dados para 3 dos 8 bairros da transoeste

# 
# transoeste:
#   julho 2010 até julho 2012
# 
# igp-m até 2019 (pq 2020 ele aumentou mt)
# https://github.com/igorlaltuf/processo-republica/blob/main/rscripts/01_an%C3%A1lise_e_gr%C3%A1ficos.R
# 
