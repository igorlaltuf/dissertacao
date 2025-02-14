# Dados dos financiamentos diretos e indiretos do BNDES - 2002 - 2020
# Download Data 29-01-2021
# Novos arquivos 08-02-2021 (dados at� 31-12-2020)
# Fontes https://www.bndes.gov.br/wps/portal/site/home/transparencia/centraldedownloads/!ut/p/z1/pVLLUsIwFP0VWHTZ5mJaaN0VRV5lcIZBIBumj7SNlqSkgerfG5CFIjDjeHc5c3Mecy4iaIkID_csCxUTPCz0e0Xa68Ab9wb2FAKYdTH4j52hE_Q9sEcYLY4LcGV8QOTHf_z8BP4gaM3tSRfuxjZ6QQSRmKtS5WgV8YRWa8YrxdQuPjowIBcbaoCSIa_KUFIes9CAmHKNFI2ENhJR80KESXVgKmOWoJVHKaYAHRNCF0zbST3TjSPPxB1IXacd0ShJTs5vRCO3gy0Oet8ZpkPc0-EeAsfujVt9D58WbnCstIfOVQ_dFlrsGa3RnAu50W3M_hhxAGh0VsCvlLph9rrdEl_XILii7wot_9mD1swKEX2dj88j7GaISJpSSaW1kxrOlSqrewMMqOvaOspZmdhbkdRIWRlQCqkOohVT9KL0JepcVNr8OSMqN_ONiz_Mt3TSwzaJnGIf-H6z-QkS4bK7/dz/d5/L2dBISEvZ0FBIS9nQSEh/
# Eu precisei mudar o formato no pr�prio excel de algumas planilhas para n�mero pq o R n�o estava reconhecendo como numeric msmo convertendo (autom�tica 2017-2020 e n�o autom�tica).

rm(list = ls())
# Carregar pacotes
library(tidyverse) 
library(readxl)
library(lubridate)
library(geobr)
library(reshape2)
library(formattable)
library(deflateBR) # usar para baixar o IGP-DI https://github.com/meirelesff/deflateBR
library(gganimate) # fazer anima��es

options(scipen = 9999)

# Import excel files
n.auto <- read_excel('input/dados_fin_bndes/naoautomaticas.xlsx', sheet = 1, skip = 4) %>% 
  select(-c(3,7,12,13,14,15,20,30,31,32,33,34))
auto02.09 <- read_excel('input/dados_fin_bndes/operacoes_indiretas_automaticas_2002-01-01_ate_2009-01-01.xlsx', sheet = 1, skip = 4) %>% 
  select(-c(10,11,12,13,18,28,29,30))
auto09.11 <- read_excel('input/dados_fin_bndes/operacoes_indiretas_automaticas_2009-01-01_ate_2011-01-01.xlsx', sheet = 1, skip = 4) %>% 
  select(-c(10,11,12,13,18,28,29,30))
auto11.12 <- read_excel('input/dados_fin_bndes/operacoes_indiretas_automaticas_2011-01-01_ate_2012-01-01.xlsx', sheet = 1, skip = 4) %>% 
  select(-c(10,11,12,13,18,28,29,30))
auto12.13 <- read_excel('input/dados_fin_bndes/operacoes_indiretas_automaticas_2012-01-01_ate_2013-01-01.xlsx', sheet = 1, skip = 4) %>% 
  select(-c(10,11,12,13,18,28,29,30))
auto13.14 <- read_excel('input/dados_fin_bndes/operacoes_indiretas_automaticas_2013-01-01_ate_2014-01-01.xlsx', sheet = 1, skip = 4) %>% 
  select(-c(10,11,12,13,18,28,29,30))
auto14.15 <- read_excel('input/dados_fin_bndes/operacoes_indiretas_automaticas_2014-01-01_ate_2015-01-01.xlsx', sheet = 1, skip = 4) %>% 
  select(-c(10,11,12,13,18,28,29,30))
auto15.17 <- read_excel('input/dados_fin_bndes/operacoes_indiretas_automaticas_2015-01-01_ate_2017-01-01.xlsx', sheet = 1, skip = 4) %>% 
  select(-c(10,11,12,13,18,28,29,30))
auto17.20 <- read_excel('input/dados_fin_bndes/operacoes_indiretas_automaticas_2017-01-01_ate_2020-12-31.xlsx', sheet = 1, skip = 5) %>% 
  select(-c(10,11,12,13,18,28,29,30))

# Checar nomes das colunas
a <- names(n.auto)
b <- names(auto02.09)
setdiff(a,b)

# Passar nome das colunas de um para o outro
colnames(auto02.09) <- colnames(n.auto)
colnames(auto09.11) <- colnames(n.auto)
colnames(auto12.13) <- colnames(n.auto)
colnames(auto13.14) <- colnames(n.auto)
colnames(auto14.15) <- colnames(n.auto)
colnames(auto15.17) <- colnames(n.auto)
colnames(auto17.20) <- colnames(n.auto)

# Colocar as datas dessa planilha no mesmo formato das demais (o arquivo veio com / no lugar de - separando as datas)
auto17.20$data_da_contratacao <- as.Date(auto17.20$data_da_contratacao, format = "%d/%m/%Y")

# Juntar os dados
all.data <- rbind(n.auto,auto02.09,auto09.11,auto12.13,auto13.14,auto14.15,auto15.17,auto17.20)

# Tirar vari�veis que n�o vou mais usar da mem�ria.
rm(n.auto,auto02.09,auto09.11,auto11.12,auto12.13,auto13.14,auto14.15,auto15.17,auto17.20) 

# Ver se as colunas est�o nos formatos corretos de vari�veis (data, character etc).
colnames(all.data)

# Padronizar os nomes das colunas
colnames(all.data)[1:22] <- c("cliente", "cnpj", "uf","munic�pio","munic�pio_c�digo",
                              "data_da_contratacao","valor_contratado_em_reais",
                              "valor_desembolsado_em_reais","fonte_de_recursos_desembolsos",
                              "modalidade_de_apoio","forma_de_apoio","produto",
                              "instrumento_financeiro","�rea_operacional","setor_cnae",
                              "subsetor_cnae_agrupado","subsetor_cnae_codigo","subsetor_cnae_nome",
                              "setor_bndes","subsetor_bndes","porte_do_cliente","natureza_do_cliente")

# Mudar coluna que est� com character para numeric
all.data$valor_desembolsado_em_reais <- as.numeric(all.data$valor_desembolsado_em_reais)
all.data$valor_contratado_em_reais <- as.numeric(all.data$valor_contratado_em_reais)

# Cria a coluna apenas com o ano a partir da data
all.data <- mutate(all.data, ano = as.character(year(as.Date(all.data$data_da_contratacao, 
                                                             format = "%Y/%m/%d"))))

# Substitui os NAs por 0 para somar corretamente. � extremamente importante que isso seja feito com os dados antes deles serem somados por categorias (Por ano, cidade etc).
#Ex: se uma cidade tem v�rios valores e apenas um NA, se eu somo todos, o resultado ser� NA e caso eu fa�a a transforma��o depois, ele vai desconsiderar os valores das outras cidades que apresentam valores.
all.data$valor_contratado_em_reais[is.na(all.data$valor_contratado_em_reais)] <- 0 # fazer isso apenas com numeric!
all.data$valor_desembolsado_em_reais[is.na(all.data$valor_desembolsado_em_reais)] <- 0 

em_milhoes <- 1000000
em_bilhoes <- 1000000000

# Corrigir pelo IGP-DI MENSAL
# Incluir coluna com valor deflacionado ao m�s
all.data <- mutate(all.data,
                   valor_contratado_deflac_ao_mes = deflate(valor_contratado_em_reais,
                                                            as.Date(ymd(all.data$data_da_contratacao)),
                                                            '11/2020','igpdi')
                   )
# IMPORTANTE, ELE DEFLACIONA INCLUSIVE OS VALORES DO M�S QUE ESTOU TRAZENDO O VALOR.
# Eu coloquei 11/2020 para o R n�o deflacionar os valores de dezembro de 2020, j� que eu quero os valores ao m�s de dezembro de 2020.
# Se eu colocar 12/2020, o R corrige os valores de dezembro pelo IGP-DI de dezembro.
# Como eu n�o preciso corrigir o �ltimo m�s, eu calculo com menos 1 m�s para n�o corrigir os valores de dezembro.
                  
# Separa as vari�veis que vou usar para calcular valores anuais.
year.data <- all.data %>% 
             select(ano, valor_contratado_em_reais, valor_desembolsado_em_reais, valor_contratado_deflac_ao_mes) 

year.data <- group_by(year.data, ano) %>% 
             summarise(across(everything(), list(sum)))

year.data <- year.data %>% 
             mutate(valor_contr_bi = round(valor_contratado_em_reais_1/em_bilhoes,1),
                    valor_contr_defl_ao_mes_bi = round(valor_contratado_deflac_ao_mes_1/em_bilhoes,1),
                    valor_desemb_bi = round(valor_desembolsado_em_reais_1/em_bilhoes,1)
                    )

# Gr�fico com contrata��es deflacionadas
year.data$ano <- as.integer(year.data$ano)
p <- ggplot(year.data, aes(x = ano, y = valor_contr_defl_ao_mes_bi))+
  geom_col(fill='#266DD3')+
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+ # retira o gap entre o eixo x e a barra
  geom_text(aes(label = as.character(round(valor_contr_defl_ao_mes_bi,2)), vjust = -1)) +  # as.character e round � para n�o mostrar os decimais no gg animate
  xlab('Ano') + ylab('Valor em R$ bilh�es') +
  #daqui para baixo � para tirar o fundo do gr�fico
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
p
ggsave('output/01_bndes/BNDES contrata��es defl para dez 2020.png', width = 9, height = 6, dpi = 300)

# Anima��o no gr�fico
# p <- p + transition_states(ano, wrap = FALSE) +
#   shadow_mark() + 
#   enter_grow() +
#   enter_fade() 
# 
# animate(p, fps=8, renderer = gifski_renderer(loop = FALSE)) # parar no primeiro loop
# 
# anim_save('bndes_anima��o.gif')

# Gr�fico com contrata��es em valores correntes
p <- ggplot(year.data, aes(x = ano, y = valor_contr_bi))+
  geom_col(fill='#266DD3')+
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+ # retira o gap entre o eixo x e a barra
  geom_text(aes(label = valor_contr_bi, vjust = -1))+
  xlab('Ano') + ylab('Valor em R$ bilh�es')+
  #daqui para baixo � para tirar o fundo do gr�fico
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p
ggsave('output/01_bndes/BNDES contrata��es valores correntes.png', width = 9, height = 6, dpi = 300)

# Gr�fico com desembolsos em valores correntes
p <- ggplot(year.data, aes(x = ano, y = valor_desemb_bi))+
  geom_col(fill='#266DD3')+
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+ # retira o gap entre o eixo x e a barra
  geom_text(aes(label = valor_desemb_bi, vjust = -1))+
  xlab('Ano') + ylab('Valor em R$ bilh�es')+
  #daqui para baixo � para tirar o fundo do gr�fico
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p
ggsave('output/01_bndes/BNDES desembolsos valores correntes.png', width = 9, height = 6, dpi = 300)



# Separar valores para a Cidade do Rio de Janeiro
RJ <- all.data %>% 
  dplyr::filter(munic�pio_c�digo==3304557) %>% 
  mutate(valor_contr_bi = round(valor_contratado_em_reais/em_bilhoes,2),
         valor_desemb_bi = round(valor_desembolsado_em_reais/em_bilhoes,2),
         valor_contr_def_bi = round(valor_contratado_deflac_ao_mes/em_bilhoes,2))

RJ.data <- RJ %>% 
  select(ano, valor_contr_bi, valor_desemb_bi, valor_contr_def_bi)

RJ.data <- group_by(RJ.data,ano) %>% 
  summarise(across(everything(), list(sum)))


ggplot(RJ.data, aes(x = ano, y = valor_contr_bi_1))+
  geom_col(fill='#266DD3')+
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+ # retira o gap entre o eixo x e a barra
  geom_text(aes(label = valor_contr_bi_1, vjust = -1))+
  xlab('Ano') + ylab('Valor em R$ Bilh�es')+
  #daqui para baixo � para tirar o fundo do gr�fico
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave('output/01_bndes/BNDES contrata��es cidade do RJ.png', width = 9, height = 6, dpi = 300)


# valor das contrata��es deflacionado
ggplot(RJ.data, aes(x = ano, y = valor_contr_def_bi_1))+
  geom_col(fill='#266DD3')+
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+ # retira o gap entre o eixo x e a barra
  geom_text(aes(label = valor_contr_def_bi_1, vjust = -1), size = 3)+
  xlab('Ano') + ylab('Valor em R$ Bilh�es')+
  #daqui para baixo � para tirar o fundo do gr�fico
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave('output/01_bndes/BNDES contrata��es deflac muni RJ.png', width = 9, height = 6, dpi = 300)


# Valor por cidades: O Rio foi a cidade que mais recebeu recursos do BNDES
cidades <- all.data %>%
  select(munic�pio, valor_contratado_em_reais, valor_desembolsado_em_reais, valor_contratado_deflac_ao_mes) %>%
  group_by(munic�pio) %>%
  summarise(valor_contr = round(sum(valor_contratado_em_reais)/em_bilhoes,2),
            valor_desemb = round(sum(valor_desembolsado_em_reais)/em_bilhoes,2),
            valor_contratado_deflac = round(sum(valor_contratado_deflac_ao_mes)/em_bilhoes,2)) 
 
dez.maiores.cidades <- cidades[with(cidades,order(-valor_contratado_deflac)),]
dez.maiores.cidades <- head(dez.maiores.cidades,11) 
dez.maiores.cidades <- dez.maiores.cidades[-1,] # Exclui a primeira "cidade" sem munic�pio.


# Fazer Gr�fico com as dez maiores cidades
ggplot(dez.maiores.cidades, aes(x = munic�pio , y = valor_contratado_deflac ))+
  geom_col(fill='#266DD3')+
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+ # retira o gap entre o eixo x e a barra
  geom_text(aes(label = valor_contratado_deflac, vjust = -1))+
  xlab('Munic�pio') + ylab('Valor em R$ Bilh�es')+
  #daqui para baixo � para tirar o fundo do gr�fico
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave('output/01_bndes/Dez maiores cidades contrat defl.png', width = 9, height = 6)


# Valor por Estado em bilh�es: SP � o maior.
estados <- all.data %>%
  select(uf, valor_contratado_em_reais, valor_desembolsado_em_reais, valor_contratado_deflac_ao_mes) %>%
  group_by(uf) %>%
  summarise(valor_contr = round(sum(valor_contratado_em_reais)/em_bilhoes,2),
            valor_desemb = round(sum(valor_desembolsado_em_reais)/em_bilhoes,2),
            valor_contratado_deflac = round(sum(valor_contratado_deflac_ao_mes)/em_bilhoes,2))

dez.maiores.estados <- estados[with(estados,order(-valor_contratado_deflac)),]
dez.maiores.estados <- head(dez.maiores.estados,11) 
dez.maiores.estados <- dez.maiores.estados[-2,] # Exclui empr�stimos interestaduais.

# Fazer Gr�fico com os dez maiores estados
ggplot(dez.maiores.estados, aes(x = uf , y = valor_contratado_deflac ))+
  geom_col(fill='#266DD3')+
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+ # retira o gap entre o eixo x e a barra
  geom_text(aes(label = valor_contratado_deflac, vjust = -1))+
  xlab('Estado') + ylab('Valor em R$ Bilh�es')+
  #daqui para baixo � para tirar o fundo do gr�fico
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave('output/01_bndes/Dez maiores Estados defl contrat.png', width = 9, height = 6)


# Gr�fico din�mico com 10 maiores estados
library(tmaptools)
library(sf)
library(tmap)
library(tmaptools)
library(leaflet)

estados.br <- read_state(code_state = "all",year=2019)

estados.mapa <- all.data %>%
  select(uf, valor_contratado_em_reais, valor_desembolsado_em_reais, valor_contratado_deflac_ao_mes) %>%
  group_by(uf) %>%
  summarise(valor_contr = round(sum(valor_contratado_em_reais)/em_bilhoes,2),
            valor_desemb = round(sum(valor_desembolsado_em_reais)/em_bilhoes,2),
            valor_contratado_deflac = round(sum(valor_contratado_deflac_ao_mes)/em_bilhoes,2))

estados.mapa <- estados.mapa[-10,] # Exclui a linha IE (interestadual)
colnames(estados.br)[2] <- "uf"

estados.br <- left_join(estados.br,estados.mapa, by = "uf")
                              
                              
tm_shape(estados.br) +
  tm_polygons("valor_contr", id = "uf", palette = "Greens", title = "Valor em R$ bilh�es", 
              labels = c("0 - 50", 
                         "50 - 100",
                         "100 - 150", 
                         "150 - 200",
                         "200 - 250", 
                         "250 - 300",
                         "300 - 350"))


tmap_mode("view")
mapa.estados <- tmap_last()
tmap_save(mapa.estados,'output/01_bndes/mapa_estados_bndes.html')


#transformar mapa em javascript interativo (ir em viewer e clicar em show in new window)

# Financiamentos para empresas do BRT em todo o pa�s
empreiteiras.brt <- all.data %>% 
           filter(str_detect(cliente,'FUNDACAO ODEBRECHT|ODEBRECHT AMBIENTAL S.A.|ODEBRECHT EQUIPAMENTOS LTDA|ODEBRECHT S.A.|ODEBRECHT SERVICOS E PARTICIPACOES S/A|OAS S.A. - EM|CONSTRUTORA OAS|BRASKEM|CONSTRUTORA QUEIROZ GALVAO|QUEIROZ GALVAO DESENV|QUEIROZ GALVAO ENER|ANDRADE GUTIERREZ|CARIOCA CHRISTIANI|CONTERN CONSTRUCOES|GLOBO CONSTR|SANERIO|MASCARENHAS BARBOSA ROSCOE|CONSTRUCAP CCPS|MJRE|DELTA CONSTRUCOES S/A')) 

# GALVAO ENGENHARIA N�O � DA QUEIROZ GALVAO.
# COPASA QUE O BNDES FINANCIA N�O E A MESMA QUE PARTICIPOU DO BRT.

# � assim que filtra linha contendo strings (case_when e grepl)
# Agrupar empresas do BRT por grupos econ�micos
empreiteiras.brt.sintese <- empreiteiras.brt %>% 
  mutate(grupo_economico = case_when(grepl('ODEBRECHT', cliente) ~ 'Grupo Odebrecht',
                                     grepl('BRASKEM', cliente) ~ 'Grupo Odebrecht',
                                     grepl('OAS', cliente) ~ 'Grupo OAS',
                                     grepl('QUEIROZ GALVAO', cliente) ~ 'Grupo Queiroz Galv�o',
                                     grepl('ANDRADE GUTIERREZ', cliente) ~ 'Grupo Andrade Gutierrez',
                                     grepl('CARIOCA CHRISTIANI', cliente) ~ 'Grupo Carioca Christiani-Nielsen Engenharia',
                                     grepl('CONTERN CONSTRUCOES', cliente) ~ 'Grupo Contern',
                                     grepl('GLOBO CONSTR', cliente) ~ 'Globo Constru��es e Terraplanagem Ltda.',
                                     grepl('SANERIO', cliente) ~ 'Sanerio Engenharia Ltda.',
                                     grepl('MASCARENHAS BARBOSA ROSCOE', cliente) ~ 'Grupo MBR',
                                     grepl('CONSTRUCAP CCPS', cliente) ~ 'Grupo Construcap',
                                     grepl('MJRE', cliente) ~ 'MJRE Construtora',
                                     grepl('DELTA CONSTRUCOES S/A', cliente) ~ 'Grupo Delta')) %>% 
  select(grupo_economico, valor_contratado_em_reais, valor_desembolsado_em_reais, valor_contratado_deflac_ao_mes) %>%
  group_by(grupo_economico) %>%
  summarise(valor_contr = round(sum(valor_contratado_em_reais)/em_milhoes,2),
            valor_desemb = round(sum(valor_desembolsado_em_reais)/em_milhoes,2),
            valor_contratado_deflac = round(sum(valor_contratado_deflac_ao_mes)/em_milhoes,2)) %>% 
  arrange(desc(valor_contr)) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~ "Total")))

empreiteiras.brt.tabela.nacional <- empreiteiras.brt.sintese
colnames(empreiteiras.brt.tabela.nacional)[1:4] <- c("Grupo Econ�mico","Valor Contratado","Valor Desembolsado", "Valor Contratado Deflacionado")
formattable(empreiteiras.brt.tabela.nacional,
            align =c("l","c","c","c"),
            list(`Grupo Econ�mico` = formatter(
              "span", style = ~ style(font.weight = "bold"))))

write.csv2(empreiteiras.brt.tabela.nacional, file = 'output/01_bndes/grupos_brt_brasil.csv')


# Gr�fico do montante de empr�stimos agrupado por grupos economicos entre 2002 e 2020
ggplot(empreiteiras.brt.sintese, aes(x = grupo_economico , y = valor_desemb))+
  geom_col(fill='#266DD3')+
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+ # retira o gap entre o eixo x e a barra
  geom_text(aes(label = valor_desemb, hjust = -0.25))+
  xlab('Grupo econ�mico') + ylab('Valor em R$ milh�es')+
  #daqui para baixo � para tirar o fundo do gr�fico
  coord_flip()+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave('output/01_bndes/Empreiteiras total no per�odo nacional.png', width = 9, height = 6)   


# Empr�stimos do BNDES ano a ano aos grupos econ�micos do BRT de 2002 a 2020.
empreiteiras.brt.sintese.ano <- empreiteiras.brt %>% 
  select(valor_contratado_em_reais, valor_desembolsado_em_reais, valor_contratado_deflac_ao_mes, ano) %>%
  group_by(ano) %>%
  summarise(valor_contr = round(sum(valor_contratado_em_reais)/em_bilhoes,2),
            valor_desemb = round(sum(valor_desembolsado_em_reais)/em_bilhoes,2),
            valor_contratado_deflac = round(sum(valor_contratado_deflac_ao_mes)/em_bilhoes,2))

# Gr�fico
ggplot(empreiteiras.brt.sintese.ano, aes(x = ano , y = valor_contratado_deflac))+
  geom_col(fill='#266DD3')+
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+ # retira o gap entre o eixo x e a barra
  geom_text(aes(label = valor_contratado_deflac, vjust = -1))+
  xlab('Ano') + ylab('Valor em R$ bilh�es')+
  #daqui para baixo � para tirar o fundo do gr�fico
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave('output/01_bndes/Empreiteiras total no per�odo nacional.png', width = 9, height = 6)   




# Tabela com projetos destinados � cidade do Rio por grupos econ�micos do BRT entre 2002 e 2020 - CIDADE DO RIO.
empreiteiras.brt.rj <- all.data %>% 
  filter(str_detect(cliente,'FUNDACAO ODEBRECHT|ODEBRECHT AMBIENTAL S.A.|ODEBRECHT EQUIPAMENTOS LTDA|ODEBRECHT S.A.|ODEBRECHT SERVICOS E PARTICIPACOES S/A|OAS S.A. - EM|CONSTRUTORA OAS|BRASKEM|CONSTRUTORA QUEIROZ GALVAO|QUEIROZ GALVAO DESENV|QUEIROZ GALVAO ENER|ANDRADE GUTIERREZ|CARIOCA CHRISTIANI|CONTERN CONSTRUCOES|GLOBO CONSTR|SANERIO|MASCARENHAS BARBOSA ROSCOE|CONSTRUCAP CCPS|MJRE|DELTA CONSTRUCOES S/A')) %>% 
  filter(str_detect(munic�pio_c�digo,'3304557'))

empreiteiras.brt.sintese.rj <- empreiteiras.brt.rj %>% 
  mutate(grupo_economico = case_when(grepl('ODEBRECHT', cliente) ~ 'Grupo Odebrecht',
                                     grepl('BRASKEM', cliente) ~ 'Grupo Odebrecht',
                                     grepl('OAS', cliente) ~ 'Grupo OAS',
                                     grepl('QUEIROZ GALVAO', cliente) ~ 'Grupo Queiroz Galv�o',
                                     grepl('ANDRADE GUTIERREZ', cliente) ~ 'Grupo Andrade Gutierrez',
                                     grepl('CARIOCA CHRISTIANI', cliente) ~ 'Grupo Carioca Christiani-Nielsen Engenharia',
                                     grepl('CONTERN CONSTRUCOES', cliente) ~ 'Grupo Contern',
                                     grepl('GLOBO CONSTR', cliente) ~ 'Globo Constru��es e Terraplanagem Ltda.',
                                     grepl('SANERIO', cliente) ~ 'Sanerio Engenharia Ltda.',
                                     grepl('MASCARENHAS BARBOSA ROSCOE', cliente) ~ 'Grupo MBR',
                                     grepl('CONSTRUCAP CCPS', cliente) ~ 'Grupo Construcap',
                                     grepl('MJRE', cliente) ~ 'MJRE Construtora',
                                     grepl('DELTA CONSTRUCOES S/A', cliente) ~ 'Grupo Delta')) %>% 
  select(grupo_economico, valor_contratado_em_reais, valor_desembolsado_em_reais, valor_contratado_deflac_ao_mes) %>%
  group_by(grupo_economico) %>%
  summarise(valor_contr = round(sum(valor_contratado_em_reais)/em_milhoes,2),
            valor_desemb = round(sum(valor_desembolsado_em_reais)/em_milhoes,2),
            valor_contratado_deflac = round(sum(valor_contratado_deflac_ao_mes)/em_milhoes,2)) %>% 
  arrange(desc(valor_contr)) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~ "Total")))


empreiteiras.brt.tabela.rj <- empreiteiras.brt.sintese.rj
colnames(empreiteiras.brt.tabela.rj)[1:4] <- c("Grupo Econ�mico","Valor Contratado","Valor Desembolsado", "Valor Contratado Deflacionado")
formattable(empreiteiras.brt.tabela.rj,
            align =c("l","c","c","c"),
            list(`Grupo Econ�mico` = formatter(
              "span", style = ~ style(font.weight = "bold"))))

write.csv2(empreiteiras.brt.tabela.rj, file = 'output/01_bndes/grupos_brt_rio_de_janeiro.csv')

# Empr�stimos do BNDES ano a ano aos grupos econ�micos do BRT de 2002 a 2020 na cidade do RIo.
empreiteiras.brt.sintese.rj <- empreiteiras.brt.rj %>% 
  select(valor_contratado_em_reais, valor_desembolsado_em_reais, valor_contratado_deflac_ao_mes, ano) %>%
  group_by(ano) %>%
  summarise(valor_contr = round(sum(valor_contratado_em_reais)/em_milhoes,2),
            valor_desemb = round(sum(valor_desembolsado_em_reais)/em_milhoes,2),
            valor_contratado_deflac = round(sum(valor_contratado_deflac_ao_mes)/em_milhoes,2))

# Gr�fico
ggplot(empreiteiras.brt.sintese.rj, aes(x = ano , y = valor_contratado_deflac))+
  geom_col(fill='#266DD3')+
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+ # retira o gap entre o eixo x e a barra
  geom_text(aes(label = valor_contratado_deflac, vjust = -1))+
  xlab('Ano') + ylab('Valor em R$ milh�es')+
  #daqui para baixo � para tirar o fundo do gr�fico
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave('output/01_bndes/Empreiteiras total no per�odo cidade do rio.png', width = 9, height = 6)  


# CURIOSAMENTE OS FINANCIAMENTOS A ODEBRETCH N�O FORAM NO RJ. DO BRT FOI A PREFEITURA.
# PARA QUAIS PROJETOS FORAM ESTES VALORES DA ODEBRECHT?


# Financiamento de fabricantes de carrocerias (NACIONAL).
fabr.carrocerias.brt <- all.data %>% 
  filter(str_detect(cliente,'MARCOPOLO SA|CAIO - INDUSCAR|COMIL ONIBUS|MASCARELLO - CARR')) 

fabr.carrocerias.brt.sintese <-  fabr.carrocerias.brt %>% 
  mutate(grupo_economico = case_when(grepl('MARCOPOLO', cliente) ~ 'Grupo Marcopolo',
                                     grepl('CAIO - INDUSCAR', cliente) ~ 'Grupo Caio',
                                     grepl('COMIL ONIBUS', cliente) ~ 'Grupo Mascarello',
                                     grepl('MASCARELLO - CARR', cliente) ~ 'Grupo Mascarello')) %>% 
  select(grupo_economico, valor_contratado_em_reais, valor_desembolsado_em_reais, valor_contratado_deflac_ao_mes) %>%
  group_by(grupo_economico) %>%
  summarise(valor_contr = round(sum(valor_contratado_em_reais)/em_milhoes,2),
            valor_desemb = round(sum(valor_desembolsado_em_reais)/em_milhoes,2),
            valor_contratado_deflac = round(sum(valor_contratado_deflac_ao_mes)/em_milhoes,2)) %>% 
  arrange(desc(valor_contr)) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~ "Total")))


# Tabelas total nacional
fabr.carrocerias.brt.tabela <- fabr.carrocerias.brt.sintese
colnames(fabr.carrocerias.brt.tabela)[1:4] <- c("Grupo Econ�mico","Valor Contratado","Valor Desembolsado", "Valor Contratado Deflacionado")
formattable(fabr.carrocerias.brt.tabela,
            align =c("l","c","c","c"),
            list(`Grupo Econ�mico` = formatter(
              "span", style = ~ style(font.weight = "bold")))) 

ggsave('output/01_bndes/encarro�adoras total no per�odo cidade do rio.png', width = 9, height = 6) 




# Financiamentos de infraestrutura na cidade do Rio de Janeiro
infra.rj <- all.data %>% 
  filter(str_detect(munic�pio_c�digo,'3304557'),
         str_detect(setor_bndes,'INFRAESTRUTURA|INFRA-ESTRUTURA')) 

infra.rj.ano <- infra.rj %>% 
  select(ano, valor_contratado_em_reais, valor_desembolsado_em_reais, valor_contratado_deflac_ao_mes) 

infra.rj.ano <- group_by(infra.rj.ano, ano) %>% 
  summarise(across(everything(), list(sum)))

infra.rj.ano <- infra.rj.ano %>% 
  mutate(valor_contr_bi = round(valor_contratado_em_reais_1/em_bilhoes,1),
         valor_contr_defl_ao_mes_bi = round(valor_contratado_deflac_ao_mes_1/em_bilhoes,1),
         valor_desemb_bi = round(valor_desembolsado_em_reais_1/em_bilhoes,1)
  )

# Gr�fico
x <- ggplot(infra.rj.ano, aes(x = ano, y = valor_contr_defl_ao_mes_bi))+
  geom_col(fill='#266DD3')+
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+ # retira o gap entre o eixo x e a barra
  geom_text(aes(label = as.character(round(valor_contr_defl_ao_mes_bi,2)), vjust = -1)) +  # as.character e round � para n�o mostrar os decimais no gg animate
  xlab('Ano') + ylab('Valor em R$ bilh�es') +
  #daqui para baixo � para tirar o fundo do gr�fico
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

x
ggsave('output/01_bndes/valor_contr_defl_infraestrutura_rj.png', dpi = 300, width = 9, height = 6)

# Financiamentos de infraestrutura na cidade do Rio de Janeiro por subsetor cnae agrupado
infra.rj$subsetor_cnae_agrupado <- tolower(infra.rj$subsetor_cnae_agrupado)

infra.rj <- infra.rj %>% 
  select(subsetor_cnae_agrupado, valor_contratado_em_reais, valor_desembolsado_em_reais, valor_contratado_deflac_ao_mes) 

infra.rj <- group_by(infra.rj, subsetor_cnae_agrupado) %>% 
  summarise(across(everything(), list(sum)))

infra.rj <- infra.rj %>% 
  mutate(valor_contr_bi = round(valor_contratado_em_reais_1/em_bilhoes,1),
         valor_contr_defl_ao_mes_bi = round(valor_contratado_deflac_ao_mes_1/em_bilhoes,1),
         valor_desemb_bi = round(valor_desembolsado_em_reais_1/em_bilhoes,1)
  )

# Exportar em CSV
write.csv2(infra.rj, 'output/01_bndes/bndes_fin_subsetor_cnae_agrupado.csv')


# grupos
unique(infra.rj$subsetor_cnae_agrupado)

