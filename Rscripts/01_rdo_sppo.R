# RDO SPPO

rm(list = ls())
source('Rscripts/00_carregar_bibliotecas.R')
options(scipen = 9999)

# importar os arquivos no formato xlsx ------------------------------------------------
lista.de.arquivos <- list.files(path = "input/rdo-rem/dados_rem/", recursive = TRUE,
                                pattern = "\\.rem$", 
                                full.names = TRUE)

dicionario_domi <- fwf_positions(
  start = c(1,10,16,17,18,20,24,26,28,29,35,40,45,50,55,62,67,72,77,82,87,92,98,103,108,120,125,130,142,147,159,164,176,183,195),
  end = c(9,15,16,17,19,23,25,27,28,34,39,44,49,54,61,66,71,76,81,86,91,97,102,109,119,124,129,141,146,158,163,175,182,194,199),
  col_names = c('termo', 'num_linha', 'tipo_servico', 'ordem_de_servico',
                'tipo_veiculo', 'ano', 'mes', 'dia', 'tarifa_determinada',
                'tarifa_praticada', 'frota_determinada', 'frota_licenciada_por_linha',
                'frota_operacional_do_dia','numero_de_viagens_linha_dia', 
                'km_coberta', 'gratuidades_idosos','gratuidades_pne','gratuidades_estud_uniao',
                'gratuidades_estud_estado','gratuidades_estud_municip','gratuidades_funcio_empresas',
                'total_gratuidades','buc_1_perna','buc_2_perna','receita_buc','buc_supervia_1_perna',
                'buc_supervia_2_perna','receita_buc_supervia','cartoes_perna_unica',
                'receita_cartoes_perna_unica','passageiros_pagantes_especie',
                'receita_passageiros_pagantes_especie','qtd_total_passageiros','receita_total_dia',
                'passe_livre_universitario'))

i <- 1

while (i <= length(lista.de.arquivos)) {
  
  assign(paste("brt", i, sep = '_'), read_fwf(paste(lista.de.arquivos[i]), col_positions = dicionario_domi))  
  
  i <- i + 1
  
}

# Agrupar dados em apenas 1 dataframe--------------------------------------------------
vars_brt <- paste0("brt_",1:343) 

rdo <- data.frame()

i <- 1

while (i <= length(lista.de.arquivos)) {
  
  rdo <- rbind(rdo, get(vars_brt[i]))
  
  i <- i + 1
  
}

rm(list = vars_brt) # remove as variáveis que não serão mais utilizadas

# alterar tipos
rdo1 <- rdo %>% 
  mutate_at(c(1:3), as.character) %>% 
  mutate_at(c(4,5), as.numeric) %>% 
  mutate_at(c(6:9), as.character) %>% 
  mutate_at(c(10:35), as.numeric) %>% 
  mutate(tarifa_praticada = tarifa_praticada / 100)
           
colnames(rdo1)

# Evolução da frota -----------------------------------------------
# Calcular a frota média por mês - ônibus troncais do BRT (exclui linhas alimentadoras)
unique(rdo$tipo_veiculo)

rdo_frota <- rdo1 %>% 
  dplyr::filter(tipo_veiculo == 45)









  select(ano, mes, dia, frota_operacional_do_dia, tipo_servico, tipo_veiculo, ordem_de_servico) 
  
  
  dplyr::filter(class_servico %in% 'Troncal do BRT') %>% 
   
  group_by(ano, mes, dia) %>% 
  summarise(frota_determinada = sum(frota_determinada),
            frota_licenciada = sum(frota_licenciada),
            frota_operante = sum(frota_operante)) %>%  
  group_by(ano, mes) %>% 
  summarise(frota_determinada = round(mean(frota_determinada),0),
            frota_licenciada = round(mean(frota_licenciada),0),
            frota_operante = round(mean(frota_operante),0)) %>% 
  mutate(data = my(paste(mes, ano, sep = '-'))) %>% 
  arrange(data) %>% 
  dplyr::filter(ano != 2021)





# Gráfico da frota -----------------------------

ggplot(rdo_frota, aes(data, frota_operante))+
  ggtitle("")+
  geom_line(color='blue')+
  ylab('Frota Troncal do BRT')+
  scale_x_date(name = 'Ano', breaks = '1 year', limits = c()) +
  theme(panel.background = element_blank()) +
  theme(panel.background = element_blank(),plot.title = element_text(hjust = 0.5))# centraliza o texto

ggsave(path = 'output/01_rdo_brt/', dpi = 300, filename = 'frota_BRT.png')


