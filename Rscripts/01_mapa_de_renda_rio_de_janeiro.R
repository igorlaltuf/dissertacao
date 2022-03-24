# Renda por bairro da cidade do Rio
#Fonte: Censo de 2010 e dados do IPP e da SMT do Rio (dados do trajeto do BRT)
rm(list = ls())

# carregar bibliotecas
library(dplyr) 
library(sf)
library(ggplot2)
library(RColorBrewer)
library(ggspatial)
library(geobr) 
library(tmap)
library(tmaptools)
library(leaflet)
library(nngeo)

options(scipen=10000) 

sf_use_s2(FALSE) # usar sempre antes do st_intersection após a versão 1.0 do sf

# Transformar o shapefile do IBGE em um dataframe sem sf para relacionar o código do bairro com o código dos microdados.
micro.e.bairros <- read_sf('input/rj_setores_censitarios_limites/33SEE250GC_SIR.shp', options = "ENCODING=WINDOWS-1252") %>% 
  dplyr::filter(NM_MUNICIP == 'RIO DE JANEIRO') %>% 
  as.data.frame() %>% 
  select(-geometry) %>%  #Aqui eu precisei transformar o objeto SF em dataframe para depois selecionar a coluna geometry e retirar.
  select(2:5)


# teste com geobr#############
geobr::lookup_muni(name_muni = 'Rio de Janeiro')
shape.muni <- read_census_tract(year = 2010, code_tract = 3304557, simplified = F)
# simplified = F permite que o shape venha mais limpo.

teste <- shape.muni %>% 
  group_by(code_neighborhood, name_neighborhood) %>% 
  summarise(geometry = st_union(geom)) 

plot(teste$geom)


  

plot(teste$geometry)

# MODO CORRETO
plot(malha.rj$geometry)




######################

# Shapefile da cidade por bairros (fiz no Qgis para remover as inconsistências no arquivo do IBGE).
malha.rj <- read_sf('input/bairros_rj_editado_igor/bairros_rj.shp')

# A função st_transform serve para mudar o código do CRS do shapefile para o 4674, que é o mesmo usado pelo IBGE no shapefile da cidade. Só assim eu consigo usar o st_intersection no outro comando mais abaixo.
trajetos.brt <- st_transform(read_sf('input/trajeto_BRT/Trajetos_BRT.shp', options = "ENCODING=UTF-8"), crs = 4674) %>% 
  mutate(OBJECTID = "Sistema BRT")
trajetos.trem <- st_transform(read_sf('input/trajeto_Trem/Trajetos_Trem.shp', options = "ENCODING=UTF-8"), crs = 4674) 
trajetos.vlt <- st_transform(read_sf('input/trajeto_VLT/Trajeto_VLT.shp', options = "ENCODING=UTF-8"), crs = 4674) %>% 
  mutate(OBJECTID = "VLT")
trajetos.metro <- st_transform(read_sf('input/trajeto_Metro/Trajetos_Metro.shp', options = "ENCODING=UTF-8"), crs = 4674) %>% 
  mutate(OBJECTID = "Metrô")

# Recortar apenas a parte da ferrovia situada dentro da cidade do Rio.
trajetos.trem.cidade <- st_intersection(trajetos.trem,malha.rj) %>% 
  mutate(OBJECTID = "Trem")

# Aqui com o mutate eu substituo a coluna V001 em caractere com uma coluna chamada V001 em numeric. Se eu não usar o mutate, eu terei duas colunas V001, uma numeric e outra character.
dados <- readxl::read_xls('input/PessoaRenda_RJ.xls') %>% 
  select(Cod_setor, V001, V002, V003, V004, V005, V006, V007, V008, V009, V010, V020, V022) %>% 
  mutate(CD_GEOCODI=as.character(Cod_setor),
         V001=as.numeric(V001),
         V002=as.numeric(V002),
         V003=as.numeric(V003),
         V004=as.numeric(V004),
         V005=as.numeric(V005),
         V006=as.numeric(V006),
         V007=as.numeric(V007),
         V008=as.numeric(V008),
         V009=as.numeric(V009),
         V010=as.numeric(V010),
         V020=as.numeric(V020),
         V022=as.numeric(V022)
         ) 

# Passar os códigos dos bairros (GEOCODB) e dos setores censitários (GEOCODI) para o arquivo com os dados por setor censitário (GEOCODI).
dados <- inner_join(dados, micro.e.bairros[,-2])

# Incluir colunas com código e nome dos bairros na planilha de dados sobre renda de acordo com os microdados (VLOOKUP com inner_join)
dados <- left_join(malha.rj, dados[,c(-1,-14,-16)], by = "CD_GEOCODB")
dados[is.na(dados)] <- 0 # Troca NA por 0

# Participação sobre o total da quantidade de pessoas recebendo até meio SM por bairro. (V001)
dados.ate.meio.sm <- dados %>%   
  group_by(CD_GEOCODB, NM_BAIRRO) %>% 
  summarise(percentual.abaixo.meio.sm = sum(V001, na.rm = TRUE)/sum(V020, na.rm = TRUE)) 

# Participação sobre o total da quantidade de pessoas recebendo até um SM por bairro. (V001+V002)
dados.ate.um.sm <- dados %>%   
  group_by(CD_GEOCODB, NM_BAIRRO) %>% 
  summarise(percentual.ate.um.sm = sum(V001+V002, na.rm = TRUE)/sum(V020, na.rm = TRUE)) 

# Participação sobre o total  da quantidade de pessoas recebendo até dois SM por bairro. (V001+V002+V003)
dados.ate.dois.sm <- dados %>%   
  group_by(CD_GEOCODB, NM_BAIRRO) %>% 
  summarise(percentual.ate.dois.sm = sum(V001+V002+V003, na.rm = TRUE)/sum(V020, na.rm = TRUE)) 

# Participação sobre o total da quantidade de pessoas recebendo até três SM por bairro. (V001+V002+V003+V004)
dados.ate.tres.sm <- dados %>%   
  group_by(CD_GEOCODB, NM_BAIRRO) %>% 
  summarise(percentual.ate.tres.sm = sum(V001+V002+V003+V004, na.rm = TRUE)/sum(V020, na.rm = TRUE)) 

# Participação sobre o total  da quantidade de pessoas recebendo até cinco SM por bairro. (V001+V002+V003+V004+v005)
dados.ate.cinco.sm <- dados %>%   
  group_by(CD_GEOCODB, NM_BAIRRO) %>% 
  summarise(percentual.ate.cinco.sm = sum(V001+V002+V003+V004+V005, na.rm = TRUE)/sum(V020, na.rm = TRUE)) 

# Participação sobre o total  da quantidade de pessoas recebendo acima de cinco SM por bairro. (V006+V007+V008+V009)
dados.mais.de.cinco <- dados %>%   
  group_by(CD_GEOCODB, NM_BAIRRO) %>% 
  summarise(percentual.mais.de.cinco.sm = sum(V006+V007+V008+V009, na.rm = TRUE)/sum(V020, na.rm = TRUE)) 

# Participação sobre o total  da quantidade de pessoas recebendo mais de 10 salários mínimos ao mês (V007+V008+V009)
dados.mais.de.dez <- dados %>%   
  group_by(CD_GEOCODB, NM_BAIRRO) %>% 
  summarise(percentual.mais.de.dez.sm = sum(V007+V008+V009, na.rm = TRUE)/sum(V020, na.rm = TRUE)) 

# Participação sobre o total  da quantidade de pessoas recebendo mais de 15 salários mínimos ao mês (V008+V009)
dados.mais.de.quinze <- dados %>%   
  group_by(CD_GEOCODB, NM_BAIRRO) %>% 
  summarise(percentual.mais.de.quinze.sm = sum(V008+V009, na.rm = TRUE)/sum(V020, na.rm = TRUE)) 

# Participação sobre o total da quantidade de pessoas recebendo mais de 20 salários mínimos ao mês (V009)
dados.mais.de.vinte <- dados %>%   
  group_by(CD_GEOCODB, NM_BAIRRO) %>% 
  summarise(percentual.mais.de.vinte.sm = sum(V009, na.rm = TRUE)/sum(V020, na.rm = TRUE)) 

# Participação sobre o total da quantidade de pessoas sem rendimento (V010)
dados.sem.rendimento <- dados %>%   
  group_by(CD_GEOCODB, NM_BAIRRO) %>% 
  summarise(percentual.sem.rendimento = sum(V010, na.rm = TRUE)/sum(V020, na.rm = TRUE))

# Unificar todos os dados acima em uma tabela.
dados.ate.um.sm.df <- dados.ate.um.sm %>% 
                        as.data.frame() %>% 
                        select(-geometry) %>%  
                        select('percentual.ate.um.sm','CD_GEOCODB','NM_BAIRRO')

dados.ate.dois.sm.df <- dados.ate.dois.sm %>% 
                          as.data.frame() %>% 
                          select(-geometry) %>%  
                          select('percentual.ate.dois.sm','CD_GEOCODB','NM_BAIRRO')

dados.ate.tres.sm.df <- dados.ate.tres.sm %>% 
                          as.data.frame() %>% 
                          select(-geometry) %>%  
                          select('percentual.ate.tres.sm','CD_GEOCODB','NM_BAIRRO')

dados.ate.cinco.sm.df <- dados.ate.cinco.sm %>% 
                            as.data.frame() %>% 
                            select(-geometry) %>%  
                            select('percentual.ate.cinco.sm','CD_GEOCODB','NM_BAIRRO')

dados.mais.de.cinco.sm.df <- dados.mais.de.cinco %>% 
                              as.data.frame() %>% 
                              select(-geometry) %>%  
                              select('percentual.mais.de.cinco.sm','CD_GEOCODB','NM_BAIRRO')                                            

dados.mais.de.dez.sm.df <- dados.mais.de.dez %>% 
                              as.data.frame() %>% 
                              select(-geometry) %>%  
                              select('percentual.mais.de.dez.sm','CD_GEOCODB','NM_BAIRRO')    

dados.mais.de.quinze.sm.df <- dados.mais.de.quinze %>% 
                                as.data.frame() %>% 
                                select(-geometry) %>%  
                                select('percentual.mais.de.quinze.sm','CD_GEOCODB','NM_BAIRRO')  

dados.mais.de.vinte.sm.df <- dados.mais.de.vinte %>% 
                                as.data.frame() %>% 
                                select(-geometry) %>%  
                                select('percentual.mais.de.vinte.sm','CD_GEOCODB','NM_BAIRRO')  

dados.sem.rendimento.df <- dados.sem.rendimento %>% 
                              as.data.frame() %>% 
                              select(-geometry) %>%  
                              select('percentual.sem.rendimento','CD_GEOCODB','NM_BAIRRO')  

percentual.faixa.sm <- dados.ate.meio.sm %>%
                        full_join(dados.ate.um.sm.df, by = 'CD_GEOCODB') %>% 
                        full_join(dados.ate.dois.sm.df, by = 'CD_GEOCODB') %>% 
                        full_join(dados.ate.tres.sm.df, by = 'CD_GEOCODB') %>% 
                        full_join(dados.ate.cinco.sm.df, by = 'CD_GEOCODB') %>% 
                        full_join(dados.mais.de.cinco.sm.df, by = 'CD_GEOCODB') %>% 
                        full_join(dados.mais.de.dez.sm.df, by = 'CD_GEOCODB') %>% 
                        full_join(dados.mais.de.quinze.sm.df, by = 'CD_GEOCODB') %>% 
                        full_join(dados.mais.de.vinte.sm.df, by = 'CD_GEOCODB') %>% 
                        full_join(dados.sem.rendimento.df, by = 'CD_GEOCODB')
  
# Transformar em um arquivo no formato GeoPackage, pq shapefile não aceita nomes de variáveis longos e cria vários arquivos. 
# Usar o pacote sf para abrir. Este arquivo será usado com o Shiny.
sf::st_write(percentual.faixa.sm, append = F, dsn = 'temp/shapes_para_shiny/bairros_e_renda_rj.gpkg')

# Salvar Shapefiles dos modais com alterações
sf::st_write(trajetos.brt, append = F, dsn = 'temp/shapes_para_shiny/brt.gpkg')
sf::st_write(trajetos.trem.cidade, append = F, dsn = 'temp/shapes_para_shiny/trem.gpkg')
sf::st_write(trajetos.vlt, append = F, dsn = 'temp/shapes_para_shiny/vlt.gpkg')
sf::st_write(trajetos.metro, append = F, dsn = 'temp/shapes_para_shiny/metro.gpkg')

# Transforma paleta de cores de 9 cores para 10 cores (assim como o colors=mycolors no ggplot)
nb.cols <- 10
mycolors <- colorRampPalette(brewer.pal(9,'Greens'))(nb.cols)


# DAQUI PARA BAIXO NO GGPLOT:
# usar a função theme_classic() para tirar o fundo cinza e os quadriculados

# Gráfico até meio SM
ggplot(dados.ate.meio.sm)+
  geom_sf(aes(fill = percentual.abaixo.meio.sm), size = 0.05)+
  geom_sf(data = trajetos.brt, aes(col = 'BRT'), size = 0.75, show.legend = 'line')+ # Esse col dentro do AES define o nome da legenda
  geom_sf(data = trajetos.vlt, aes(col = 'VLT'), size = 0.75, show.legend = 'line')+ # Para o show legend line funcionar, todos os modais devem ter este atributo.
  geom_sf(data = trajetos.metro, aes(col = 'Metrô'), size = 0.75, show.legend = 'line')+
  geom_sf(data = trajetos.trem.cidade, aes(col = 'Trem'), size = 0.75, show.legend = 'line')+
  scale_color_manual(values = c("#D34945","#9045D3","#45CFD3","#88D345"))+ 
  labs(col="Modal de Transporte:")+ # Nome das legendas
  scale_fill_gradientn(colors = mycolors,
                       breaks = c(0,0.025,0.05)) +  #rev reverte a ordem
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

ggsave('output/01_mapas_renda/percentatémeioSM.png', width = 9, height = 6)

# Gráfico até dois SM
ggplot(dados.ate.dois.sm)+
  geom_sf(aes(fill = percentual.ate.dois.sm),size = 0.05)+
  geom_sf(data = trajetos.brt, aes(col = 'BRT'), size = 0.75, show.legend = 'line')+ # Esse col dentro do AES define o nome da legenda
  geom_sf(data = trajetos.vlt, aes(col = 'VLT'), size = 0.75, show.legend = 'line')+ # Para o show legend line funcionar, todos os modais devem ter este atributo.
  geom_sf(data = trajetos.metro, aes(col = 'Metrô'), size = 0.75, show.legend = 'line')+
  geom_sf(data = trajetos.trem.cidade, aes(col = 'Trem'), size = 0.75, show.legend = 'line')+
  scale_color_manual(values = c("#D34945","#9045D3","#45CFD3","#88D345"))+ 
  labs(col="Modal de Transporte:")+ # Nome das legendas
  scale_fill_gradientn(colors = mycolors) +  #rev reverte a ordem
  labs(fill='') + #Muda o nome da legenda com o fill.
  guides(colour = guide_legend(title.position = "top"))+
  annotation_scale(location='br')+ #Adiciona escala
  annotation_north_arrow(location='tl', 
                         style = north_arrow_fancy_orienteering())+
  theme_minimal()+
  theme(legend.position = 'bottom', legend.direction = "horizontal") + theme_bw() + #daqui p baixo tira o grid do mapa
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = 'bottom',
        legend.direction = "horizontal")

ggsave('output/01_mapas_renda/percentatédoisSM.png', width = 9, height = 6)

# Gráfico até cinco SM
ggplot(dados.ate.cinco.sm)+
  geom_sf(aes(fill = percentual.ate.cinco.sm),size = 0.05)+
  geom_sf(data = trajetos.brt, aes(col = 'BRT'), size = 0.75, show.legend = 'line')+ # Esse col dentro do AES define o nome da legenda
  geom_sf(data = trajetos.vlt, aes(col = 'VLT'), size = 0.75, show.legend = 'line')+ # Para o show legend line funcionar, todos os modais devem ter este atributo.
  geom_sf(data = trajetos.metro, aes(col = 'Metrô'), size = 0.75, show.legend = 'line')+
  geom_sf(data = trajetos.trem.cidade, aes(col = 'Trem'), size = 0.75, show.legend = 'line')+
  scale_color_manual(values = c("#D34945","#9045D3","#45CFD3","#88D345"))+ 
  labs(col = "Modal de Transporte:")+ # Nome das legendas
  scale_fill_gradientn(colors = mycolors,
                       breaks = c(0,0.2,0.4,0.6))+#rev reverte a ordem
  labs(fill = '') + #Muda o nome da legenda com o fill.
  guides(colour = guide_legend(title.position = "top"))+
  annotation_scale(location = 'br')+ #Adiciona escala
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering())+
  theme_minimal()+
  theme(legend.position = 'bottom', legend.direction = "horizontal") + theme_bw() + #daqui p baixo tira o grid do mapa
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = 'bottom',
        legend.direction = "horizontal")

ggsave('output/01_mapas_renda/percentatécincoSM.png', width = 9, height = 6)


# Mapa interativo até 5 SM utilizando tmaps e leaflet
dados.ate.cinco.sm <- dados.ate.cinco.sm %>% select(!CD_GEOCODB) # remove o código do bairro
tm_shape(dados.ate.cinco.sm) +
  tm_polygons("percentual.ate.cinco.sm", palette = "Greens", title = "Percentual sobre <br>o total do bairro",
              labels = c("10% - 20%",
                         "20% - 30%",
                         "30% - 40%",
                         "40% - 50%",
                         "50% - 60%",
                         "60% - 70%"))+
  tm_scale_bar()+
   #tm_text("NM_BAIRRO", scale=0.75)+
  tm_shape(trajetos.brt)+ #
    tm_lines(col = "#D34945", lwd = 2, labels = 'BRT')+ # adiciona o nome do modal ao passar o mouse por cima
  tm_shape(trajetos.vlt)+
    tm_lines(col = "#9045D3", lwd = 2, labels = 'VLT')+
  tm_shape(trajetos.metro)+
    tm_lines(col = "#45CFD3", lwd = 2, labels = 'Metrô')+
  tm_shape(trajetos.trem.cidade)+
    tm_lines(col = "#88D345", lwd = 2, labels = 'Trem') +
  tm_add_legend(type = 'fill', 
                col =c("#D34945","#9045D3","#45CFD3","#88D345"),
                size = 2,
                labels = c('BRT','VLT','Metrô','Trem'),
                title="Modal")
  
tmap_mode("view")
mapa.bairros.dinamico <- tmap_last()
mapa.bairros.dinamico
tmap_save(mapa.bairros.dinamico,'output/01_mapas_renda/mapa_bairros_rj.html')


# Gráfico acima de 5 SM

ggplot(dados.mais.de.cinco)+
  geom_sf(aes(fill = percentual.mais.de.cinco.sm),size = 0.05)+
  geom_sf(data = trajetos.brt, aes(col = 'BRT'), size = 0.75, show.legend = 'line')+ # Esse col dentro do AES define o nome da legenda
  geom_sf(data = trajetos.vlt, aes(col = 'VLT'), size = 0.75, show.legend = 'line')+ # Para o show legend line funcionar, todos os modais devem ter este atributo.
  geom_sf(data = trajetos.metro, aes(col = 'Metrô'), size = 0.75, show.legend = 'line')+
  geom_sf(data = trajetos.trem.cidade, aes(col = 'Trem'), size = 0.75, show.legend = 'line')+
  scale_color_manual(values = c("#D34945","#9045D3","#45CFD3","#88D345"))+ 
  labs(col="Modal de Transporte:")+ # Nome das legendas
  scale_fill_gradientn(colors = mycolors,
                       breaks = c(0,0.25,0.5))+  #rev reverte a ordem
  labs(fill='') + #Muda o nome da legenda com o fill.
  guides(colour = guide_legend(title.position = "top"))+
  annotation_scale(location='br')+ #Adiciona escala
  annotation_north_arrow(location='tl', 
                         style = north_arrow_fancy_orienteering())+
  theme_minimal()+
  theme(legend.position = 'bottom', legend.direction = "horizontal") + theme_bw() + #daqui p baixo tira o grid do mapa
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = 'bottom',
        legend.direction = "horizontal")

ggsave('output/01_mapas_renda/percentacimade5SM.png', width = 9, height = 6)

# Gráfico acima de dez SM
ggplot(dados.mais.de.dez)+
  geom_sf(aes(fill = percentual.mais.de.dez.sm),size = 0.05)+
  geom_sf(data = trajetos.brt, aes(col = 'BRT'), size = 0.75, show.legend = 'line')+ # Esse col dentro do AES define o nome da legenda
  geom_sf(data = trajetos.vlt, aes(col = 'VLT'), size = 0.75, show.legend = 'line')+ # Para o show legend line funcionar, todos os modais devem ter este atributo.
  geom_sf(data = trajetos.metro, aes(col = 'Metrô'), size = 0.75, show.legend = 'line')+
  geom_sf(data = trajetos.trem.cidade, aes(col = 'Trem'), size = 0.75, show.legend = 'line')+
  scale_color_manual(values = c("#D34945","#9045D3","#45CFD3","#88D345"))+ 
  labs(col="Modal de Transporte:")+ # Nome das legendas
  scale_fill_gradientn(colors = mycolors)+  #rev reverte a ordem
  labs(fill='') + #Muda o nome da legenda com o fill.
  guides(colour = guide_legend(title.position = "top"))+
  annotation_scale(location='br')+ #Adiciona escala
  annotation_north_arrow(location='tl', 
                         style = north_arrow_fancy_orienteering())+
  theme_minimal()+
  theme(legend.position = 'bottom', legend.direction = "horizontal") + theme_bw() + #daqui p baixo tira o grid do mapa
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = 'bottom',
        legend.direction = "horizontal")

ggsave('output/01_mapas_renda/percentacimade10SM.png', width = 9, height = 6)

# Gráfico acima de quinze SM
ggplot(dados.mais.de.quinze)+
  geom_sf(aes(fill = percentual.mais.de.quinze.sm),size = 0.05)+
  geom_sf(data = trajetos.brt, aes(col = 'BRT'), size = 0.75, show.legend = 'line')+ # Esse col dentro do AES define o nome da legenda
  geom_sf(data = trajetos.vlt, aes(col = 'VLT'), size = 0.75, show.legend = 'line')+ # Para o show legend line funcionar, todos os modais devem ter este atributo.
  geom_sf(data = trajetos.metro, aes(col = 'Metrô'), size = 0.75, show.legend = 'line')+
  geom_sf(data = trajetos.trem.cidade, aes(col = 'Trem'), size = 0.75, show.legend = 'line')+
  scale_color_manual(values = c("#D34945","#9045D3","#45CFD3","#88D345"))+ 
  labs(col="Modal de Transporte:")+ # Nome das legendas
  scale_fill_gradientn(colors = mycolors) +  #rev reverte a ordem
  labs(fill='') + #Muda o nome da legenda com o fill.
  guides(colour = guide_legend(title.position = "top"))+
  annotation_scale(location='br')+ #Adiciona escala
  annotation_north_arrow(location='tl', 
                         style = north_arrow_fancy_orienteering())+
  theme_minimal()+
  theme(legend.position = 'bottom', legend.direction = "horizontal") + theme_bw() + #daqui p baixo tira o grid do mapa
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = 'bottom',
        legend.direction = "horizontal")

ggsave('output/01_mapas_renda/percentacimade15SM.png', width = 9, height = 6)

# Gráfico acima de vinte SM
ggplot(dados.mais.de.vinte)+
  geom_sf(aes(fill = percentual.mais.de.vinte.sm), size = 0.05) +
  geom_sf(data = trajetos.brt, aes(col = 'BRT'), size = 0.75, show.legend = 'line') + # Esse col dentro do AES define o nome da legenda
  geom_sf(data = trajetos.vlt, aes(col = 'VLT'), size = 0.75, show.legend = 'line') + # Para o show legend line funcionar, todos os modais devem ter este atributo.
  geom_sf(data = trajetos.metro, aes(col = 'Metrô'), size = 0.75, show.legend = 'line') +
  geom_sf(data = trajetos.trem.cidade, aes(col = 'Trem'), size = 0.75, show.legend = 'line') +
  scale_color_manual(values = c("#D34945","#9045D3","#45CFD3","#88D345")) + 
  labs(col="Modal de Transporte:") + # Nome das legendas
  scale_fill_gradientn(colors = mycolors) +  #rev reverte a ordem
  labs(fill='') + #Muda o nome da legenda com o fill.
  guides(colour = guide_legend(title.position = "top"))+
  annotation_scale(location='br')+ # Adiciona escala
  annotation_north_arrow(location='tl', 
                         style = north_arrow_fancy_orienteering())+
  theme_minimal()+
  theme(legend.position = 'bottom', legend.direction = "horizontal") + theme_bw() + #daqui p baixo tira o grid do mapa
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = 'bottom',
        legend.direction = "horizontal")

ggsave('output/01_mapas_renda/percentacimade20SM.png', width = 9, height = 6)

# Pessoas sem renda acima de 10 anos de idade
ggplot(dados.sem.rendimento)+
  geom_sf(aes(fill = percentual.sem.rendimento),size = 0.05)+
  geom_sf(data = trajetos.brt, aes(col = 'BRT'), size = 0.75, show.legend = 'line')+ # Esse col dentro do AES define o nome da legenda
  geom_sf(data = trajetos.vlt, aes(col = 'VLT'), size = 0.75, show.legend = 'line')+ # Para o show legend line funcionar, todos os modais devem ter este atributo.
  geom_sf(data = trajetos.metro, aes(col = 'Metrô'), size = 0.75, show.legend = 'line')+
  geom_sf(data = trajetos.trem.cidade, aes(col = 'Trem'), size = 0.75, show.legend = 'line')+
  scale_color_manual(values = c("#D34945","#9045D3","#45CFD3","#88D345"))+ 
  labs(col="Modal de Transporte:")+ # Nome das legendas
  scale_fill_gradientn(colors = mycolors) +  #rev reverte a ordem
  labs(fill='') + #Muda o nome da legenda com o fill.
  guides(colour = guide_legend(title.position = "top"))+
  annotation_scale(location='br')+ #Adiciona escala
  annotation_north_arrow(location='tl', 
                         style = north_arrow_fancy_orienteering())+
  theme_minimal()+
  theme(legend.position = 'bottom', legend.direction = "horizontal") + theme_bw() + #daqui p baixo tira o grid do mapa
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = 'bottom',
        legend.direction = "horizontal")

ggsave('output/01_mapas_renda/percentsemrendimento.png', width = 9, height = 6)