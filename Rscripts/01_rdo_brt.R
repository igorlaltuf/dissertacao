# RDO BRT
rm(list = ls())
source('Rscripts/00_carregar_bibliotecas.R')
options(scipen = 9999)

# importar os arquivos no formato xlsx ------------------------------------------------
lista.de.arquivos <- list.files(path = "input/rdo-brt-novo/", recursive = TRUE,
                                pattern = "\\.xlsx$", 
                                full.names = TRUE)

# importante: a função read_xlsx desconsidera os filtros aplicados no excel.

i <- 1

while (i <= length(lista.de.arquivos)) {
  
  assign(paste("brt", i, sep = '_'), read_xlsx(paste(lista.de.arquivos[i]), skip = 3, col_names = F))  
  
  i <- i + 1
  
}


# Agrupar dados em apenas 1 dataframe--------------------------------------------------
vars_brt <- paste0("brt_", 1:85) 

rdo <- data.frame()

i <- 1

while (i <= length(lista.de.arquivos)) {
  
  rdo <- rbind(rdo, get(vars_brt[i]))
  
  i <- i + 1
  
}


colnames(rdo) <- c('termo',	'linha',	'tipo_servico',	'termo_2', 'codigo_veiculo',
                   'ano',	'mes',	'dia',	'tarifa_codigo',	'tarifa_valor',	'frota_determinada',
                   'frota_licenciada',	'frota_operante',	'qtd_viagens',	'qtd_km_cobertos',
                   'qtd_grt_idoso',	'qtd_grt_especial',	'qtd_grt_estud_federal',
                   'qtd_grt_estud_estadual', 'qtd_grt_estud_municipal',	'qtd_grt_rodoviario',	
                   'qtd_grt_total',	'qtd_buc_1_perna',	'qtd_buc_2_perna_integracao',	'receita_buc',
                   'qtd_buc_supervia_1_perna',	'qtd_buc_supervia_2_perna_integracao',	'receita_buc_supervia',
                   'qtd_vale_transp',	'receita_vale_transp',	'qtd_pgto_especie',	'receita_especie',
                   'passageiros_transportados', 'receita_total', 'tipo_de_informacao','qtd_grt_passe_livre_univers')

# rm(list = vars_brt) # remove as variáveis que não serão mais utilizadas



# Criar dataframe com valores anuais em milhões - incluindo as linhas alimentadoras ----------------------------
# rdo_total <- rdo %>% 
#   select(ano, passageiros_transportados, receita_total) %>% 
#   group_by(ano) %>% 
#   summarise(across(everything(), ~ sum(., is.na(.), 0))) %>% 
#   mutate(passageiros_transportados_mi = passageiros_transportados/1000000,
#          receita_total_mi = receita_total/1000000) %>% 
#   select(-passageiros_transportados,-receita_total)
# 
# # incluir valores de 2021 segundo dados do site da prefeitura
# 
# rdo_total[7,2] <- 75.97
# 
# rdo_total[7,3] <- 197.719066 

# Deflacionar valores da receita total caso me enviem a planilha de 2021



# Gráficos

# Quantidade de passageiros em milhões
x <- ggplot(rdo_total) +
  aes(x = ano, weight = qtd_total_passageiros_transportados_mi) +
  geom_bar(fill = "#112446") +
  labs(x = "Ano", y = "Quantidade de passageiros transportados (milhões)") +
  theme_classic()

ggsave(x, path = 'output/01_rdo_brt/', dpi = 300, filename = 'qtd_passageiros.png')

# Receita total (valores correntes R$ milhões)
y <- ggplot(rdo_total) +
  aes(x = ano, weight = receita_total_mi) +
  geom_bar(fill = "#112446") +
  labs(x = "Ano", 
       y = "Receita total (em R$ milhões)") +
  theme_classic()

ggsave(y, path = 'output/01_rdo_brt/', dpi = 300, filename = 'receita_total.png')


# Evolução da frota -----------------------------------------------
# Calcular a frota média por mês - ônibus troncais do BRT (exclui linhas alimentadoras)

rdo_frota <- rdo %>% 
  dplyr::filter(codigo_veiculo == 45) %>% 
  select(ano, mes, dia, frota_determinada, frota_licenciada, frota_operante) %>% 
  group_by(ano, mes, dia) %>%   
  summarise(frota_determinada = sum(frota_determinada, na.rm = T),
            frota_licenciada = sum(frota_licenciada, na.rm = T),
            frota_operante = sum(frota_operante, na.rm = T)) %>% 
  mutate(data = my(paste(mes, ano, sep = '-')))


  


  # group_by(ano, mes) %>% 
  # summarise(frota_determinada = round(mean(frota_determinada, na.rm = T),0),
  #           frota_licenciada = round(mean(frota_licenciada, na.rm = T),0),
  #           frota_operante = round(mean(frota_operante, na.rm = T),0)) %>% 
  # mutate(data = my(paste(mes, ano, sep = '-'))) %>% 
  # arrange(data) 





# Gráfico da frota -----------------------------

ggplot(rdo_frota, aes(data, frota_operante))+
  ggtitle("")+
  geom_line(color='blue')+
  ylab('Frota de articulados do BRT')+
  scale_x_date(name = 'Ano', breaks = '1 year', limits = c()) +
  scale_y_continuous(limits = c(0,500)) +
  theme(panel.background = element_blank()) +
  theme(panel.background = element_blank(),plot.title = element_text(hjust = 0.5))# centraliza o texto

ggsave(path = 'output/01_rdo_brt/', dpi = 300, filename = 'frota_BRT.png')
