source('Rscripts/00_carregar_bibliotecas.R')
rm(list = ls())

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


# Criar dataframe com valores anuais - incluindo as linhas alimentadoras ----------------------------
rdo_total <- rdo %>% 
  janitor::clean_names() %>% 
  select(ano, qtd_total_passageiros_transportados, receita_total) %>% 
  group_by(ano) %>% 
  summarise(across(everything(), ~ sum(., is.na(.), 0)))

# Criar dataframe com valores anuais - excluindo as linhas alimentadoras ----------------------------

# rdo_articulados <- rdo %>% 
#   janitor::clean_names() %>% 
#   dplyr::filter(class_servico != c('Alimentadora do BRT')) %>% 
#   select(ano, qtd_total_passageiros_transportados, receita_total) %>% 
#   group_by(ano) %>% 
#   summarise(across(everything(), ~ sum(., is.na(.), 0)))


# incluir outros meses de 2021 (esperando resposta via LAI).


