source('Rscripts/00_carregar_bibliotecas.R')
rm(list = ls())
options(scipen = 9999)
# importar os arquivos no formato xlsx ------------------------------------------------
lista.de.arquivos <- list.files(path = "input/rdo-brt/", recursive = TRUE,
                                pattern = "\\.xlsx$", 
                                full.names = TRUE)

i <- 1

while (i <= length(lista.de.arquivos)) {
  
  assign(paste("brt", i, sep = '_'), read_xlsx(paste(lista.de.arquivos[i])))  
  
  i <- i + 1
  
}

# Agrupar dados em apenas 1 dataframe--------------------------------------------------
vars_brt <- paste0("brt_",1:78) 

rdo <- data.frame()

i <- 1

while (i <= length(lista.de.arquivos)) {
  
  rdo <- rbind(rdo, get(vars_brt[i]))

  i <- i + 1

}

rm(list = vars_brt) # remove as variáveis que não serão mais utilizadas


# Criar dataframe com valores anuais em milhões - incluindo as linhas alimentadoras ----------------------------
rdo_total <- rdo %>% 
  janitor::clean_names() %>% 
  select(ano, qtd_total_passageiros_transportados, receita_total) %>% 
  group_by(ano) %>% 
  summarise(across(everything(), ~ sum(., is.na(.), 0))) %>% 
  mutate(qtd_total_passageiros_transportados_mi = qtd_total_passageiros_transportados/1000000,
         receita_total_mi = receita_total/1000000) %>% 
  select(-qtd_total_passageiros_transportados,-receita_total)

# incluir valores de 2021 segundo dados do site da prefeitura

rdo_total[7,2] <- 75.97
rdo_total[7,3] <- 197.719066 


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

# Criar dataframe com valores anuais - excluindo as linhas alimentadoras ----------------------------

# rdo_articulados <- rdo %>% 
#   janitor::clean_names() %>% 
#   dplyr::filter(class_servico != c('Alimentadora do BRT')) %>% 
#   select(ano, qtd_total_passageiros_transportados, receita_total) %>% 
#   group_by(ano) %>% 
#   summarise(across(everything(), ~ sum(., is.na(.), 0)))

