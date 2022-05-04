# ITBI no entorno da Transolímpica 

options(scipen = 9999)
source('Rscripts/01_itbi.R') # inclui as bibliotecas que serão usadas

# ---------------------------------------------------

# importar dados das estações
estacoes_BRT <- sf::st_read('input/estacoes-brt/estacoes_BRT.shp')

# 4 - análise da olímpica (obras de 2012 a 2016) --------------------------------------------------
# ver bairros com estações da transolimpica com dados disponíveis de 2012 até 2016.
brt_olimpica <- estacoes_BRT %>% 
  janitor::clean_names() %>% 
  dplyr::filter(flg_tran_1 == 1)

bairros <- geobr::read_neighborhood() %>% 
  dplyr::filter(code_muni == '3304557')

st_crs(bairros)
st_crs(brt_olimpica)

brt_olimpica <- st_transform(brt_olimpica, 4674)

bairros_brt_o <- st_intersection(bairros, brt_olimpica)

estacoes_olimpica <- bairros_brt_o %>% 
  group_by(name_neighborhood) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  mutate(name_neighborhood = tolower(name_neighborhood))

bairros_olimpica <- itbi_m2_bairro %>% 
  mutate(bairro = tolower(bairro)) %>% 
  dplyr::filter(bairro %in% estacoes_olimpica$name_neighborhood) %>% 
  select(codbairro) %>% 
  as_vector()


itbi_m2 <- itbi_m2_bairro %>% 
  dplyr::filter(codbairro %in% bairros_olimpica,
                x2011 != 0) 

 

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

brt_olimpica <- brt_olimpica %>% 
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>% 
  select(objectid, nome, lat, lon)

# criar o entorno das estações a partir do tempo de caminhada.
temp_min <- 15
temp_seg <- temp_min * 60

area_15min <- otp_isochrone(otpcon = otp_rj,
                            fromPlace = brt_olimpica, # local de partida
                            cutoffSec = temp_seg,    # tempo de viagem em segundos
                            mode = 'WALK',
                            fromID = brt_olimpica$nome) 


# checar área em 15 min
mapview::mapview(area_15min)

# area até 45 min
temp_min <- 45
temp_seg <- temp_min * 60

area_45min <- otp_isochrone(otpcon = otp_rj,
                            fromPlace = brt_olimpica, # local de partida
                            cutoffSec = temp_seg,    # tempo de viagem em segundos
                            mode = 'WALK',
                            fromID = brt_olimpica$nome
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

# Separar bairros que tenho dados de 2012 até 2016
df1 <- left_join(df1, itbi_m2_rua, by = 'cl') %>% 
  na.omit() %>% 
  dplyr::filter(x2011 != 0,
                x2012 != 0,
                x2013 != 0,
                x2014 != 0,
                x2015 != 0,
                x2016 != 0
  ) %>%  
  select(cl, logradouro, codbairro, bairro, x2011, x2012, x2013,
         x2014, x2015, x2016) %>% 
  group_by(cl, logradouro, codbairro, bairro, x2011, x2012, x2013,
           x2014, x2015, x2016) %>% 
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
  mutate(var2012 = (x2012 / x2011) -1,
         var2013 = (x2013 / x2012) -1,
         var2014 = (x2014 / x2013) -1,
         var2015 = (x2015 / x2014) -1,
         var2016 = (x2016 / x2015) -1,
         tx_med_anual_12_16 = (var2012 + var2013 + var2014 + var2015 + var2016) / 5) %>% 
  dplyr::filter(bairro %in% ruas_p_bairro_15$bairro) %>% 
  group_by(bairro, x2011, x2012, x2013, x2014, x2015, x2016, tx_med_anual_12_16) %>% 
  summarise(across(starts_with("var"), ~ round(., 2))) %>% 
  mutate(tx_med_anual_12_16 = round(tx_med_anual_12_16, 2)) %>% 
  arrange(desc(tx_med_anual_12_16))

# ver shape dos 15 min
mapview::mapview(df_15_min)



# Separar ruas acessíveis até 45 min das estações ------------------------------------------------------
df2 <- st_intersection(shape_ruas, st_buffer(area_45min, 0))

df2 <- left_join(df2, itbi_m2_rua, by = 'cl') %>% 
  na.omit() %>% 
  dplyr::filter(x2011 != 0,
                x2012 != 0,
                x2013 != 0,
                x2014 != 0,
                x2015 != 0,
                x2016 != 0
  ) %>%  
  select(cl, logradouro, codbairro, bairro, x2011, x2012, x2013,
         x2014, x2015, x2016) %>% 
  group_by(cl, logradouro, codbairro, bairro, x2011, x2012, x2013,
           x2014, x2015, x2016) %>% 
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
  mutate(var2012 = (x2012 / x2011) -1,
         var2013 = (x2013 / x2012) -1,
         var2014 = (x2014 / x2013) -1,
         var2015 = (x2015 / x2014) -1,
         var2016 = (x2016 / x2015) -1,
         tx_med_anual_12_16 = (var2012 + var2013 + var2014 + var2015 + var2016) / 5) %>% 
  dplyr::filter(bairro %in% ruas_p_bairro_15$bairro) %>% 
  group_by(bairro, x2011, x2012, x2013, x2014, x2015, x2016, tx_med_anual_12_16) %>% 
  summarise(across(starts_with("var"), ~ round(., 2))) %>% 
  mutate(tx_med_anual_12_16 = round(tx_med_anual_12_16, 2)) %>% 
  arrange(desc(tx_med_anual_12_16))

# ver shape dos 15 min
mapview::mapview(df_45_min)


# calcular variação anual em cada bairro
df_bairros <- itbi_m2_bairro %>% 
  dplyr::filter(x2011 != 0,
                x2012 != 0,
                x2013 != 0,
                x2014 != 0,
                x2015 != 0,
                x2016 != 0
  ) %>%  
  ungroup() %>% 
  select(bairro, x2011, x2012, x2013, x2014, x2015, x2016) %>% 
  group_by(bairro, x2011, x2012, x2013, x2014, x2015, x2016) %>% 
  mutate(var2012 = (x2012 / x2011) -1,
         var2013 = (x2013 / x2012) -1,
         var2014 = (x2014 / x2013) -1,
         var2015 = (x2015 / x2014) -1,
         var2016 = (x2016 / x2015) -1,
         tx_med_anual_12_16 = (var2012 + var2013 + var2014 + var2015 + var2016) / 5) %>% 
  group_by(bairro, x2011, x2012, x2013, x2014, x2015, x2016, tx_med_anual_12_16) %>% 
  summarise(across(starts_with("var"), ~ round(., 2))) %>% 
  mutate(tx_med_anual_12_16 = round(tx_med_anual_12_16, 2)) %>% 
  arrange(desc(tx_med_anual_12_16))



# Tabela final comparando dist 15 min, dist 45min e média do bairro (da taxa média de variação anual de 2011 até 2014). 
df_transolimpica
df_45_min
df_15_min
df_bairros

# recorte das variáveis de crescimento anual médio
df_15_min_rec <- df_15_min %>% select(bairro, tx_med_anual_12_16) %>% st_drop_geometry() %>% rename(tx_15m = tx_med_anual_12_16)
df_45_min_rec <- df_45_min %>% select(bairro, tx_med_anual_12_16) %>% st_drop_geometry() %>% rename(tx_45m = tx_med_anual_12_16)
df_bairros_rec <- df_bairros %>% ungroup() %>% select(bairro, tx_med_anual_12_16) %>% rename(tx_bairro = tx_med_anual_12_16)


df_transolimpica <- left_join(df_15_min_rec, df_45_min_rec, by = 'bairro') %>% 
  left_join(df_bairros_rec)



# Vila Militar não tem dados de ITBI, Deodoro não tem dado para todos os anos.
# Foram analisados 4 bairros dos 9 bairros cortados pela transolimpica (considerando disponibilidade dos dados e existencia de dados de ITBI de pelo menos 5 ruas por bairro)

# Nos quatro bairros analisados (Barra, Jacarepaguá, Recreio e Taquara) o entorno de 15 minutos foi maior do que 
# a valorização do bairro. No entanto, apenas na barra da tijuca o entorno de 15 minutos foi superior ao entorno de 45 min.
# indica que de forma geral não houve valorização.


# transolimpica:
# Obras de junho/2012 até julho 2016
# https://extra.globo.com/noticias/rio/eduardo-paes-lanca-transolimpica-promete-inaguracao-em-2015-372697.html 
# anuncio transolímpica foi em 2010. Náo tenho dados de 2009 do ITBI.

