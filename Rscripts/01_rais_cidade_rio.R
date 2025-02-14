# Dados da RAIS da cidade do Rio de Janeiro

# Carregar bibliotecas ----------------------------------------------
rm(list = ls())
source('Rscripts/00_carregar_bibliotecas.R')
source('Rscripts/00_global_functions.R')

# Credenciais -------------------------------------------------------
credencial <- Sys.getenv("CREDENCIAL_BASE_DOS_DADOS")
basedosdados::set_billing_id(credencial)

# I - Compara��o de v�nculos ativos entre 2019 e 2020 por bairro do Rio de Janeiro

# Importar dados ----------------------------------------------------
cod.bairros <- read_excel('Input/RAIS_vinculos_layout2018e2019.xls', sheet=4, skip=1)
colnames(cod.bairros) <- c('categoria','bairro','codigo')
cod.bairros$codigo <- as.integer(cod.bairros$codigo)

# 1 - V�nculos ativos por bairros em 2019 ---------------------
query <- "SELECT id_municipio, ano, bairros_rj, cnae_2, vinculo_ativo_3112 FROM `basedosdados.br_me_rais.microdados_vinculos`
          WHERE ano = 2019 AND id_municipio = '3304557' AND vinculo_ativo_3112 = 1"

bairros.rj <- read_sql(query)

vinc_ativos_2019 <- bairros.rj %>% 
                    select(3,5) %>% 
                    group_by(bairros_rj) %>% 
                    summarise(vinc_ativo_bairro = sum(vinculo_ativo_3112, na.rm = TRUE)) %>% 
                    mutate(bairros_rj = as.integer(bairros_rj)) %>% 
                    left_join(cod.bairros, by=c('bairros_rj'='codigo')) %>% 
                    rename(vinc_ativo_2019 = vinc_ativo_bairro)

# Vagas por bairro (verificar NAs)
125903/sum(vinc_ativos_2019$vinc_ativo_2019) # 5,71% dos valores est�o como NA
 
vinc_ativos_2019 <- vinc_ativos_2019 %>% 
                     drop_na() # remove linhas que contenham um campo com NA


# V�nculos ativos por bairros em 2020 ------------------------------------------
query <- "SELECT id_municipio, ano, bairros_rj, cnae_2, vinculo_ativo_3112 FROM `basedosdados.br_me_rais.microdados_vinculos`
          WHERE ano = 2020 AND id_municipio = '3304557' AND vinculo_ativo_3112 = 1"

bairros.rj <- read_sql(query)

vinc_ativos_2020 <- bairros.rj %>% 
                    select(3,5) %>% 
                    group_by(bairros_rj) %>% 
                    summarise(vinc_ativo_bairro = sum(vinculo_ativo_3112, na.rm = TRUE)) %>% 
                    mutate(bairros_rj = as.integer(bairros_rj)) %>% 
                    left_join(cod.bairros, by=c('bairros_rj'='codigo')) %>% 
                    rename(vinc_ativo_2020 = vinc_ativo_bairro)

# Vagas por bairro (verificar NAs)
143532/sum(vinc_ativos_2020$vinc_ativo_2020) # 6,9% dos valores est�o como NA

vinc_ativos_2020 <- vinc_ativos_2020 %>% 
  drop_na() # remove linhas que contenham um campo com NA


# Mapa Saldo de v�nculos ativos 2019 vs 2020  -----------------------------------------------------------
# Vinculos ativos n�o classificados por bairro pela RAIS - dados faltantes (NA)
# 2019: 125903 
# 2020: 143532 

vinc_atv_2019 <- sum(vinc_ativos_2019$vinc_ativo_2019)
vinc_atv_2020 <- sum(vinc_ativos_2020$vinc_ativo_2020)

dif_vinc_ativos <- left_join(vinc_ativos_2019,vinc_ativos_2020) %>% 
  mutate(saldo = vinc_ativo_2020 - vinc_ativo_2019) %>% 
  arrange(desc(saldo)) %>% 
  select(4,1,5,2,6)  

# Corre��o de nomes
dif_vinc_ativos["bairro"][dif_vinc_ativos["bairro"] == "Freguesia - Jacarepagua"] <- "Freguesia (Jacarepagua)"
dif_vinc_ativos["bairro"][dif_vinc_ativos["bairro"] == "Freguesia - Ilha do Governador"] <- "Freguesia (Ilha do Governador)"

# Mapa dos Saldos ---------------------------------------------------------------------------------------
shape_bairros_rj <- read_sf('input/bairros_rj_editado_igor/bairros_rj.shp') %>% 
  janitor::clean_names() %>% 
  mutate(bairro = rm_accent(nm_bairro))

# Verifica a diferen�a entre bairros do shape e da RAIS
diferec_shape_rais <- setdiff(dif_vinc_ativos$bairro, shape_bairros_rj$bairro)
diferec_shape_rais <- dif_vinc_ativos %>% 
  dplyr::filter(bairro %in% diferec_shape_rais)

# Dados faltantes RAIS vs Shape
faltantes_shape_2019 <- sum(diferec_shape_rais$vinc_ativo_2019, na.rm = T) 
faltantes_shape_2020 <- sum(diferec_shape_rais$vinc_ativo_2020, na.rm = T)

# Dados faltantes para cada ano (vinculos ativos n�o classificados por bairros):
# 2019:
x <- 125903 + faltantes_shape_2019

# 2020:
y <- 143532 + faltantes_shape_2020

x - y

# Ao todo, 2020 tem menos -17.326 v�nculos ativos que 2019 por motivo de n�o informa��o do bairro dos v�nculos

shape_bairros_rj <- shape_bairros_rj %>% 
  left_join(dif_vinc_ativos) %>% 
  na.omit() %>% 
  mutate(var_percent = round((saldo/vinc_ativo_2019)*100, 2), # Varia��o de 2019 para 2020
         prop_percent_2019 = round((vinc_ativo_2019/sum(vinc_ativo_2019)) * 100, 2), # propor��o de cada bairro em 2019
         prop_percent_2020 = round((vinc_ativo_2020/sum(vinc_ativo_2020)) * 100, 2),
         var_prop_2019_2020 = round(prop_percent_2020 - prop_percent_2019,2)) %>%   # propor��o de cada bairro em 2020
         arrange(desc(var_prop_2019_2020)) 


quant <- quantile(shape_bairros_rj$var_prop_2019_2020, probs = c(.1,.25,.50,.75,.9))

shape_bairros_rj <- shape_bairros_rj %>% 
  dplyr::mutate(class = case_when( 
   var_prop_2019_2020 <= quant[1] ~ "Muito Baixo",
   var_prop_2019_2020 <= quant[2] ~ "Baixo",
   var_prop_2019_2020 <= quant[3] ~ "M�dio Baixo",
   var_prop_2019_2020 <= quant[4] ~ "M�dio Alto",
   var_prop_2019_2020 <= quant[5] ~ "Alto",
   var_prop_2019_2020 > quant[5] ~ "Muito Alto"),
   class = factor(class,
                  levels = c("Muito Baixo", "Baixo", "M�dio Baixo", "M�dio Alto", "Alto", "Muito Alto")))

# Coment�rios ----------------------------------------------------------------
# var_prop_2019_2020 esta vari�vel mostra o quanto a participa��o de cada bairro sobre o total variou entre os dois anos
# Ela impede que bairros que variaram muitos por terem poucos v�nculos ativos se destaquem

# V�nculos ativos - total munic�pio do Rio
# 2019: 2.197.860 (valores incluindo os NAs)
# 2020: 2.074.215 (valores incluindo os NAs) 
# No total, houve uma queda de 123.645 v�nculos ativos ou 5,62% dos v�nculos formais ativos no munic�pio

# V�nculos ativos - An�lise interbairros

sum(shape_bairros_rj$vinc_ativo_2020) - sum(shape_bairros_rj$vinc_ativo_2019)

# Queda de 140.971 v�nculos ativos entre os dois anos.
# Entretanto, 2020 tem menos 17.326 v�nculos ativos que 2019 por motivo de n�o informa��o do bairro dos v�nculos.

# 

ggplot(shape_bairros_rj)+
  geom_sf(aes(fill = class), size = 0.05) +
  scale_fill_manual(values = brewer.pal(6,"RdYlGn"))+
  ggtitle('Classifica��o da varia��o da propor��o de v�nculos ativos\nde cada bairro em rela��o ao total do munic�pio entre\n2019 e 2020')+
  labs(fill='') + #Muda o nome da legenda com o fill.
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

ggsave('output/01_rais/bairros_RAIS_Rio.png', width = 9, height = 6, dpi = 300)

shape_bairros_rj_mapview <- shape_bairros_rj %>% 
  select("nm_bairro","geometry","vinc_ativo_2020","vinc_ativo_2019","saldo","var_percent",       
         "prop_percent_2019","prop_percent_2020","var_prop_2019_2020","class")


mapview::mapview(shape_bairros_rj_mapview, zcol = "class", label = 'nm_bairro', col.regions = brewer.pal(6, "RdYlGn")) %>% 
  mapview::mapshot(url = 'output/01_rais/bairros_RAIS_Rio.html')
  

# 2 - Evolu��o dos v�nculos ativos na cidade do Rio de Janeiro
query <- "SELECT id_municipio, ano, SUM(vinculo_ativo_3112) as vinculos_ativos FROM `basedosdados.br_me_rais.microdados_vinculos`
          WHERE id_municipio = '3304557'
          GROUP BY id_municipio, ano"
df <- read_sql(query)
df$vinculos_ativos <- as.numeric(df$vinculos_ativos)
df$ano <- as.numeric(df$ano)


# Gr�fico com a evolu��o anual de v�nculos ativos (em milh�es)
ggplot(df,aes(ano,vinculos_ativos/1000000))+
  ggtitle("Evolu��o dos v�nculos ativos de emprego formais \n na cidade do Rio de Janeiro")+
  geom_line(color='blue', linetype = 'dashed')+
  geom_point(color='black')+
  ylab('Vinculos formais ativos (em milh�es)')+
  scale_x_continuous(name = 'Ano', # dado do eixo x
                     limits = c(1985,2021),  # valor m�ximo e m�nimo do eixo
                     n.breaks = 10) +
  theme(panel.background = element_blank())+
  theme(panel.background = element_blank(),plot.title = element_text(hjust = 0.5))# centraliza o texto

ggsave('output/01_rais/vinc_ativo_rio.png', dpi = 300)
# 
# 
# 
# # 3 - Evolu��o dos empregos formais na cidade do Rio (todos os v�nculos)
# query <- "SELECT id_municipio,ano,SUM(numero_vinculos) as vinculos FROM `basedosdados.br_me_rais.microdados_vinculos`
#           WHERE id_municipio = '3304557'
#           GROUP BY id_municipio, ano"
# df <- read_sql(query)
# 
# df$vinculos <- as.numeric(df$vinculos)
# df$ano <- as.numeric(df$ano)
# 
# # Gr�fico com a evolu��o anual do n�mero de v�nculos (em milh�es)
# ggplot(df,aes(ano,vinculos/1000000))+
#   ggtitle("Evolu��o do n�mero de v�nculos de emprego formais \n na cidade do Rio de Janeiro")+
#   geom_line(color='blue', linetype = 'dashed')+
#   geom_point(color='black')+
#   ylab('Vinculos formais (em milh�es)')+
#   scale_x_continuous(name = 'Ano', # dado do eixo x
#                      limits = c(2010,2019),  # valor m�ximo e m�nimo do eixo
#                      n.breaks = 10) +
#   theme(panel.background = element_blank(),plot.title = element_text(hjust = 0.5))# centraliza o texto
# 
# 




##### Recorte de funcion�rios de empresas de �nibus na cidade do Rio de Janeiro
# 4 - Empregados empresas de �nibus municipal ou reg metropolitana
# cod cnae 49213 Transporte rodovi�rio coletivo de passageiros, com itiner�rio fixo, municipal e em regi�o metropolitana
query <- "SELECT id_municipio,ano,SUM(vinculo_ativo_3112) as vinculos_ativos FROM `basedosdados.br_me_rais.microdados_vinculos`
          WHERE id_municipio = '3304557' AND cnae_2 = '49213'
          GROUP BY id_municipio, ano"
df.onibus <- read_sql(query)
df.onibus$vinculos_ativos <- as.numeric(df.onibus$vinculos_ativos)
df.onibus$ano <- as.numeric(df.onibus$ano)

ggplot(df.onibus,aes(ano,vinculos_ativos))+
  geom_line(color='blue', linetype = 'dashed')+
  geom_point(color='black') +
  ylab('Vinculos formais ativos de empregados em \n empresas de �nibus municipal ou na RM')+
  scale_y_continuous(n.breaks = 6)+
  scale_x_continuous(name = 'Ano', # dado do eixo x
                     limits = c(2008,2021),  # valor m�ximo e m�nimo do eixo
                     n.breaks = 10) +
  theme(panel.background = element_blank())

ggsave('output/01_rais/01_cnae_49213_muni_rio_rais.png', dpi = 300)



# Vinculos formais ativos da ocupa��o cobrador de transportes coletivos (exceto trem) CBO: 511215
query <- "SELECT id_municipio,ano,cbo_2002,SUM(vinculo_ativo_3112) as vinculos_ativos FROM `basedosdados.br_me_rais.microdados_vinculos`
          WHERE id_municipio = '3304557' AND cbo_2002 = '511215'
          GROUP BY id_municipio, ano, cbo_2002"
df.trocador <- read_sql(query)

df.trocador$vinculos_ativos <- as.numeric(df.trocador$vinculos_ativos)
df.trocador$ano <- as.numeric(df.trocador$ano)

ggplot(df.trocador,aes(ano,vinculos_ativos))+
  geom_line(color='blue', linetype = 'dashed')+
  geom_point(color='black')+
  ylab('Vinculos formais ativos da ocupa��o \n cobrador de transportes coletivos (exceto trem)')+
  scale_x_continuous(name = 'Ano', # dado do eixo x
                     limits = c(2004,2020),  # valor m�ximo e m�nimo do eixo
                     n.breaks = 10) +
  theme(panel.background = element_blank())

ggsave('output/01_rais/01_cob_511215_muni_rio_rais.png', dpi = 300)





# V�nculos formais ativos da ocupa��o motorista de �nibus urbano CBO:782410
query <- "SELECT id_municipio,ano,cbo_2002,SUM(vinculo_ativo_3112) as vinculos_ativos FROM `basedosdados.br_me_rais.microdados_vinculos`
          WHERE id_municipio = '3304557' AND cbo_2002 = '782410'
          GROUP BY id_municipio, ano, cbo_2002"
df.motorista <- read_sql(query)

df.motorista$vinculos_ativos <- as.numeric(df.motorista$vinculos_ativos)
df.motorista$ano <- as.numeric(df.motorista$ano)

ggplot(df.motorista,aes(ano,vinculos_ativos))+
  geom_line(color='blue', linetype = 'dashed')+
  geom_point(color='black')+
  ylab('Vinculos formais ativos da ocupa��o \n motorista de �nibus urbano')+
  scale_x_continuous(name = 'Ano', # dado do eixo x
                     limits = c(2003,2020),  # valor m�ximo e m�nimo do eixo
                     n.breaks = 10) +
  theme(panel.background = element_blank())

ggsave('output/01_rais/01_mot_782410_muni_rio_rais.png', dpi = 300)



# usar esta query para ver os campos da tabela do banco de dados.
query <- 'SELECT *  FROM `basedosdados.br_me_rais.microdados_vinculos`
          LIMIT 5'


# 
# # Vinculos formais totais da ocupa��o cobrador de transportes coletivos (exceto trem) CBO: 511215
# query <- "SELECT id_municipio,ano,cbo_2002,SUM(numero_vinculos) as vinculos FROM `basedosdados.br_me_rais.microdados_vinculos`
#           WHERE id_municipio = 3304557 AND cbo_2002 = '511215'
#           GROUP BY id_municipio, ano, cbo_2002"
# df.trocador <- read_sql(query)
# 
# df.trocador$vinculos <- as.numeric(df.trocador$vinculos)
# df.trocador$ano <- as.numeric(df.trocador$ano)
# 
# ggplot(df.trocador,aes(ano,vinculos))+
#   geom_line(color='blue', linetype = 'dashed')+
#   geom_point(color='black')+
#   ylab('N�mero de vinculos da ocupa��o \n cobrador de transportes coletivos (exceto trem)')+
#   scale_x_continuous(name = 'Ano', # dado do eixo x
#                      limits = c(2003,2019),  # valor m�ximo e m�nimo do eixo
#                      n.breaks = 10) +
#   theme(panel.background = element_blank())