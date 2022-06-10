# ITBI no entorno das estações do BRT Transoeste 

# Obras de julho de 2010 até julho de 2012.

options(scipen = 9999)
source('Rscripts/01_itbi.R') # inclui as bibliotecas que serão usadas

# 1 - importar dados das estações e filtrar dados do corredor ---------------------------------------------------
estacoes_BRT <- sf::st_read('input/estacoes-brt/estacoes_BRT.shp')

# ver bairros com estações da transoeste com dados disponíveis de 2010 até 2012.
brt_corredor <- estacoes_BRT %>% 
  janitor::clean_names() %>% 
  dplyr::filter(flg_trans_o == 1)

bairros <- geobr::read_neighborhood() %>% 
  dplyr::filter(code_muni == '3304557')

st_crs(bairros)
st_crs(brt_corredor)

brt_corredor <- st_transform(brt_corredor, 4674)

bairros_brt_c <- st_intersection(bairros, brt_corredor)

estacoes_corredor <- bairros_brt_c %>% 
  group_by(name_neighborhood) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  mutate(name_neighborhood = tolower(name_neighborhood))

bairros_corredor <- itbi_m2_bairro %>% 
  mutate(bairro = tolower(bairro)) %>% 
  dplyr::filter(bairro %in% estacoes_corredor$name_neighborhood) %>% 
  select(codbairro) %>% 
  as_vector()

itbi_m2 <- itbi_m2_bairro %>% 
  dplyr::filter(codbairro %in% bairros_corredor,
                x2010 != 0) 


# 2 - calcular a área do entorno das estações --------------------------------------------------------------------------

# 2.1 - calcular o entorno de 15 minutos --------------------------------------------------------------------

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

brt_corredor <- brt_corredor %>% 
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>% 
  select(objectid, nome, lat, lon) 


# criar o entorno das estações a partir do tempo de caminhada.
temp_min <- 15
temp_seg <- temp_min * 60

area_15min <- otp_isochrone(otpcon = otp_rj,
                            fromPlace = brt_corredor, # local de partida
                            cutoffSec = temp_seg,    # tempo de viagem em segundos
                            mode = 'WALK',
                            fromID = brt_corredor$nome
) 

# checar área em 15 min
mapview::mapview(area_15min)



# 2.2 - calcular o entorno de 45 minutos --------------------------------------------------------------------

# area até 45 min
temp_min <- 45
temp_seg <- temp_min * 60

area_45min <- otp_isochrone(otpcon = otp_rj,
                            fromPlace = brt_corredor, # local de partida
                            cutoffSec = temp_seg,    # tempo de viagem em segundos
                            mode = 'WALK',
                            fromID = brt_corredor$nome
)

# checar área em 45 min
mapview::mapview(area_45min)



# 3 - Calculos das ruas no entorno das estações ------------------------------------------------------------

# 3.1 Separar ruas acessíveis até 15 min das estações ------------------------------------------------------
st_crs(area_15min)
st_crs(shape_ruas)

shape_ruas <- st_transform(shape_ruas, crs = 4326)
sf_use_s2(F)
df1 <- st_intersection(shape_ruas, area_15min) 

# plot(df1$geometry)
# mapview::mapview(df1)

# Separar ruas do entorno de 15 minutos que tenho dados para os anos da obra
df1 <- left_join(df1, itbi_m2_rua, by = 'cl') %>% 
  na.omit() %>% 
  dplyr::filter(x2010 != 0,
                x2011 != 0,
                x2012 != 0
                ) %>%  
  select(cl, logradouro, fromPlace, codbairro, bairro, x2010, x2011, x2012) %>% 
  group_by(cl, logradouro, fromPlace, codbairro, bairro, x2010, x2011, x2012) %>% 
  summarise(geometry = st_union(geometry)) 

# 3.2 Calcular a taxa média de crescimento anual das ruas acessíveis em 15 minutos ------------------------------
df_1_estacao <- df1 %>% 
  group_by(fromPlace) %>% 
  summarise(across(starts_with("x"), ~ mean(., na.rm = T)),
            across(starts_with("x"), ~ round(., 2))) %>% 
  mutate(var2011 = round((x2011 / x2010) -1, 2),
         var2012 = round((x2012 / x2011) -1, 2),
         tx_med_anual = round((var2011 + var2012) / 2, 2))

mapview::mapview(df1)

# 3.3 Quantidade de ruas por estação do BRT em 15 minutos -------------------------------------------------------
ruas_estacao_15 <- df1 %>%                               
  group_by(fromPlace) %>% 
  count() %>% 
  st_drop_geometry()


# 3.4 - Separar ruas acessíveis até 45 min das estações ------------------------------------------------------

df2 <- st_intersection(shape_ruas, st_buffer(area_45min, 0))

df2 <- left_join(df2, itbi_m2_rua, by = 'cl') %>%                         
  na.omit() %>% 
  dplyr::filter(x2010 != 0,
                x2011 != 0,
                x2012 != 0
  ) %>%  
  select(cl, logradouro, fromPlace, codbairro, bairro, x2010, x2011, x2012) %>% 
  group_by(cl, logradouro, fromPlace, codbairro, bairro, x2010, x2011, x2012) %>% 
  summarise(geometry = st_union(geometry)) 


# 3.5 - Calcular a taxa média de crescimento anual das ruas acessíveis em 45 minutos ------------------------------

df_2_estacao <- df2 %>% 
  group_by(fromPlace) %>% 
  summarise(across(starts_with("x"), ~ mean(., na.rm = T)),
            across(starts_with("x"), ~ round(., 2))) %>% 
  mutate(var2011 = round((x2011 / x2010) -1, 2),
         var2012 = round((x2012 / x2011) -1, 2),
         tx_med_anual = round((var2011 + var2012) / 2, 2))

mapview::mapview(df2)

# 3.6 Quantidade de ruas por estação do BRT em 45 minutos -------------------------------------------------

ruas_estacao_45 <- df2 %>%                                                     
  group_by(fromPlace) %>% 
  count() %>% 
  st_drop_geometry()


# 3.7 Comparação da quantidade de ruas por estação  ------------------------------------------------------

comp_ruas_estacao <- left_join(ruas_estacao_15, ruas_estacao_45, by = 'fromPlace') %>% 
  rename('qtd_ruas_15_min' = n.x,
         'qtd_ruas_45_min' = n.y) %>% 
  arrange(desc(qtd_ruas_15_min))


# 4 Calcular variação por bairro --------------------------------------------------------------------------

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
         tx_med_anual = (var2011 + var2012) / 2) %>% 
         group_by(bairro, x2010, x2011, x2012, tx_med_anual) %>% 
         summarise(across(starts_with("var"), ~ round(., 2))) %>% 
         mutate(tx_med_anual = round(tx_med_anual, 2)) %>% 
         arrange(desc(tx_med_anual))

df_bairros <- df_bairros %>% ungroup() %>% select(bairro, tx_med_anual) %>% rename(tx_bairro = tx_med_anual) %>% 
  mutate(bairro = tolower(bairro))

# 5 Tabela final com o crescimento anual médio --------------------------------------------------------------------

# recorte das variáveis de crescimento anual médio
df_15_min <- df_1_estacao %>% select(fromPlace, tx_med_anual) %>% st_drop_geometry() %>% rename(tx_15m = tx_med_anual)
df_45_min <- df_2_estacao %>% select(fromPlace, tx_med_anual) %>% st_drop_geometry() %>% rename(tx_45m = tx_med_anual)

# Adicionar o valor por bairro
bairros_estac <- bairros_brt_c %>% 
  select(nome, name_neighborhood) 

df_brt <- left_join(df_15_min, df_45_min, by = c('fromPlace')) %>% 
  left_join(bairros_estac, by = c('fromPlace' = 'nome')) %>%
  mutate(name_neighborhood = tolower(name_neighborhood)) %>% 
  left_join(df_bairros, by = c('name_neighborhood' = 'bairro')) %>% 
  mutate(check_1 = ifelse(tx_15m > tx_45m, 1, 0)) %>% 
  mutate(check_2 = ifelse(tx_15m > tx_bairro, 1, 0)) 


# tabela final:
df_brt

ruas_entorno_4 <- comp_ruas_estacao %>% 
  dplyr::filter(qtd_ruas_15_min >= 4) %>% 
  select(fromPlace) %>% 
  as_vector()

df_brt_entorno_4 <- df_brt %>% 
  dplyr::filter(fromPlace %in% ruas_entorno_4)

write.csv2(comp_ruas_estacao, 'output/01_entorno_tabelas/ruas_entorno_oeste.csv',
           row.names = F, fileEncoding = 'UTF-8')

write.csv2(df_brt_entorno_4, 'output/01_entorno_tabelas/valoriz_oeste.csv',
           row.names = F, fileEncoding = 'UTF-8')




# check_1 compara as ruas no entorno de até 15 minutos com aquelas no entorno de 45 min.
# check_2 compara as ruas no entorno de até 15 minutos com a média do bairro.


# 6 Comentários --------------------------------------------------------------------------------------


sum(df_brt$check_1) 

sum(df_brt$check_2, na.rm = T) 



# 7 - Mapas --------------------------------------------------------------------------------------------
rj <- geobr::read_municipality(code_muni = 3304557)

# 7.1 - Mapa com ruas de entorno dos corredores

# mapa 1 - para cada corredor, plotar ruas do entorno de 15 ao lado das ruas com entorno de 45 min
# mapa 15 min
a <- ggplot() +
  geom_sf(data = rj) +
  geom_sf(data = df1)+  # Whether to order the factor result or not
  geom_sf(data = brt_corredor) +
  coord_sf(xlim = c(-43.7, -43.21), ylim = c(-22.85, -23.03)) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() 

a
# mapa 45 min
b <- ggplot() +
  geom_sf(data = rj) +
  geom_sf(data = df2)+  # Whether to order the factor result or not
  geom_sf(data = brt_corredor) +
  coord_sf(xlim = c(-43.7, -43.21), ylim = c(-22.85, -23.03)) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() 

b

(a)/
(b)

ggsave('output/01_entorno_mapas/transoeste_15_45.png', scale = 1.2, width = 9, height = 6, dpi = 600)


# 7.2 - Mapa do corredor BRT vs ruas que mais valorizaram no período

taxas_ruas <- itbi_m2_rua %>% 
  na.omit() %>% 
  dplyr::filter(x2010 != 0,
                x2011 != 0,
                x2012 != 0) %>%
  mutate(var2011 = (x2011 / x2010) -1,
         var2012 = (x2012 / x2011) -1,
         tx_med_anual = (var2011 + var2012) / 2) %>% 
  group_by(cl, logradouro, x2010, x2011, x2012, tx_med_anual) %>% 
  summarise(across(starts_with("var"), ~ round(., 2))) %>% 
  mutate(tx_med_anual = round(tx_med_anual, 2)) %>% 
  arrange(desc(tx_med_anual)) %>% 
  ungroup() %>% 
  select(cl, logradouro, tx_med_anual)
  
  

# ruas com dados para o período
taxas_ruas <- left_join(shape_ruas, taxas_ruas, by = 'cl') %>% 
  na.omit()

a <- ggplot() +
  geom_sf(data = rj) +
  geom_sf(data = taxas_ruas) +  # Whether to order the factor result or not
  geom_sf(data = brt_corredor) +
  coord_sf(xlim = c(-43.7, -43.21), ylim = c(-22.85, -23.03)) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() 



# recortar o 1/3 de ruas que mais valorizaram anualmente no período (acima do percentil 66%)
quant_66 <- quantile(taxas_ruas$tx_med_anual, probs = .66)

ruas_valorizadas <- taxas_ruas %>% 
  dplyr::filter(tx_med_anual > quant_66)


b <- ggplot() +
  geom_sf(data = rj) +
  geom_sf(data = ruas_valorizadas) +  # Whether to order the factor result or not
  geom_sf(data = brt_corredor) +
  coord_sf(xlim = c(-43.7, -43.21), ylim = c(-22.85, -23.03)) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() 

(a)/
(b)


ggsave('output/01_entorno_mapas/valorizacao_transoeste.png', scale = 1.2, width = 9, height = 6, dpi = 600)



# 7.3 Terminal Jardim Oceânico

brt_jd <- brt_corredor %>% 
  dplyr::filter(nome %in% 'Terminal Jd. Oceânico')

jd_15 <- df1 %>% 
  dplyr::filter(fromPlace %in% 'Terminal Jd. Oceânico')

jd_45 <- df2 %>% 
  dplyr::filter(fromPlace %in% 'Terminal Jd. Oceânico')

a <- ggplot() +
  geom_sf(data = rj) +
  geom_sf(data = jd_15) +  # Whether to order the factor result or not
  geom_sf(data = brt_jd) +
  coord_sf(xlim = c(-43.28, -43.35), ylim = c(-22.98, -23.02)) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() 
a

b <- ggplot() +
  geom_sf(data = rj) +
  geom_sf(data = jd_45) +  # Whether to order the factor result or not
  geom_sf(data = brt_jd) +
  coord_sf(xlim = c(-43.28, -43.35), ylim = c(-22.98, -23.02)) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() 
b

(a)/
(b)

ggsave('output/01_entorno_mapas/entorno_jd_oceanico.png', scale = 1.2, width = 9, height = 6, dpi = 600)



# 7.4 - Ruas que mais valorizaram entre 2010 e 2016 e todas as estações de BRT


# 7.2 - Mapa do corredor BRT vs ruas que mais valorizaram no período

taxas_ruas <- itbi_m2_rua %>% 
  na.omit() %>% 
  dplyr::filter(x2010 != 0,
                x2011 != 0,
                x2012 != 0,
                x2013 != 0,
                x2014 != 0,
                x2015 != 0,
                x2016 != 0) %>%
  mutate(var2011 = (x2011 / x2010) -1,
         var2012 = (x2012 / x2011) -1,
         var2013 = (x2013 / x2012) -1,
         var2014 = (x2014 / x2013) -1,
         var2015 = (x2015 / x2014) -1,
         var2016 = (x2016 / x2015) -1,
         tx_med_anual = (var2011 + var2012 + var2013 + var2014 + var2015 + var2016) / 6) %>% 
  group_by(cl, logradouro, x2010, x2011, x2012, x2013, x2014, x2015, x2016, tx_med_anual) %>% 
  summarise(across(starts_with("var"), ~ round(., 2))) %>% 
  mutate(tx_med_anual = round(tx_med_anual, 2)) %>% 
  arrange(desc(tx_med_anual)) %>% 
  ungroup() %>% 
  select(cl, logradouro, tx_med_anual)



# ruas com dados para o período
taxas_ruas <- left_join(shape_ruas, taxas_ruas, by = 'cl') %>% 
  na.omit()

# recortar o 1/3 de ruas que mais valorizaram anualmente no período (acima do percentil 66%)
quant_66 <- quantile(taxas_ruas$tx_med_anual, probs = .66)

ruas_valorizadas <- taxas_ruas %>% 
  dplyr::filter(tx_med_anual > quant_66)




var_cari <- read_csv2('output/01_entorno_tabelas/valoriz_carioca.csv')
var_oeste <- read_csv2('output/01_entorno_tabelas/valoriz_oeste.csv')
var_olimp <- read_csv2('output/01_entorno_tabelas/valoriz_olimpica.csv')

brt_operacao_var <- rbind(var_cari, var_oeste, var_olimp) %>% 
  mutate(fromPlace = tolower(fromPlace)) 

var_bairro <- brt_operacao_var %>% 
  dplyr::filter(check_1 == 1) %>% 
  group_by(name_neighborhood ) %>% 
  count()


  
brt_operacao <- estacoes_BRT %>% 
  dplyr::filter(Flg_TransB == 0) %>% 
  mutate(Nome = tolower(Nome)) %>%  
  dplyr::filter(Nome %in% brt_operacao_var$fromPlace)
  
brt_operacao_var <- brt_operacao_var %>% 
  dplyr::filter(check_1 == 1)
  
brt_operacao_var <- brt_operacao %>% 
  dplyr::filter(Nome %in% brt_operacao_var$fromPlace)


ggplot() +
  geom_sf(data = rj) +
  geom_sf(data = ruas_valorizadas, aes(geometry = geometry,
                                       col = 'Ruas mais valorizadas'),
          size = 0.3, show.legend = 'line') +
  geom_sf(data = brt_operacao, aes(geometry = geometry,
                                   col = 'Estação com menor\nvalorização'), 
          stat = "sf_coordinates", size = .8, show.legend = 'point') + 
  geom_sf(data = brt_operacao_var, aes(geometry = geometry,
                                       col = 'Estação com maior\nvalorização'), 
          stat = "sf_coordinates", size = .8, show.legend = 'point') +
  scale_color_manual(values = c("Estação com maior\nvalorização" = 'green', 
                                "Estação com menor\nvalorização" = 'orange',
                                "Ruas mais valorizadas" = 'black'),
                     name = '',
                     guide = guide_legend(override.aes = list(linetype=c("blank", "blank", "solid"),
                                                              shape=c(16, 16, NA)))) +
  coord_sf(xlim = c(-43.78, -43.15), ylim = c(-22.78, -23.07)) +
  labs(x = NULL, y = NULL) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5))




ggsave('output/01_entorno_mapas/valorizacao_2011-2016.png', scale = 1, width = 9, height = 6, dpi = 600)











# Demais links:
# razão de usar o buffer https://gis.stackexchange.com/questions/163445/getting-topologyexception-input-geom-1-is-invalid-which-is-due-to-self-intersec
# https://docs.opentripplanner.org/en/latest/Basic-Tutorial/
# https://itdpbrasil.org/tutorial-saiba-como-calcular-o-pnt/ Ver os dois últimos vídeos do PNT
# fonte do arquivo pbf do rio de janeiro https://download.openstreetmap.fr/extracts/south-america/brazil/southeast/
