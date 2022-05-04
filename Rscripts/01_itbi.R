rm(list = ls())
source('Rscripts/00_carregar_bibliotecas.R')
options(scipen = 9999)

# 1 - unificar shapes de trechos de ruas em ruas inteiras -------------------------
# rodar apenas 1 vez (demora 25 minutos).


# shapes_ruas <- sf::st_read('input/Logradouros/Logradouros.shp') %>% 
#   janitor::clean_names() %>%   
#   select(cl) 
# 
# inicio <- Sys.time()
# 
# shapes_ruas <- shapes_ruas %>% 
#   group_by(cl) %>% 
#   summarise(geometry = st_union(geometry))
# 
# fim <- Sys.time()
# 
# fim-inicio
# 
# sf::st_write(shapes_ruas, 'output/shape_ruas_rj/ruas_rio.shp')

# shapes_ruas_1 <- st_set_precision(shapes_ruas, units::set_units(1000, mm))

# plot(shapes_ruas$geometry)



# 2 - Cálculo do valor do m² por rua -------------------------------------------------------------------

# 01 residencial
# 02 comercial
# tipologia apartamento
# principal transação do mercado : compra e venda!

itbi <- read_csv('input/itbi_rio_de_janeiro.csv') %>% 
  janitor::clean_names() %>% 
  as_tibble() %>% 
  dplyr::filter(cd_utilizacao == '01', # residencial
                principal_transacao_mercado == 'COMPRA E VENDA',
                principais_tipologias == 'APARTAMENTO') %>% 
  group_by(cl,logradouro, codbairro, bairro, ano_transacao, total_transacoes) %>% 
  summarise(total_transacoes = sum(total_transacoes),
            media_area_construida_m2 = sum(media_area_construida),
            media_valor_imovel = sum(media_valor_imovel),
            media_valor_m2 = media_valor_imovel / media_area_construida_m2)  

# remover ponto de alavancagem (valor muito acima da média dos outros anos e do bairro): 
itbi <- itbi[!(itbi$cl == "138982" & itbi$ano_transacao == 2011 & itbi$bairro == 'Jacarepaguá'),]


# shape das ruas para fazer left join pela variável cl (ainda não fiz)
shape_ruas <- sf::st_read('output/shape_ruas_rj/ruas_rio.shp')

# deflacionar valores acima (e recalcular scripts abaixo).


# valor por rua
itbi_m2_rua <- itbi %>% 
  as_tibble() %>% 
  pivot_wider(names_from = ano_transacao, values_from = media_valor_m2) %>%
  select(1:4, 8:20) %>% 
  janitor::clean_names() %>% 
  group_by(cl, logradouro, codbairro, bairro) %>% 
  summarise(across(starts_with("x"), ~ sum(., na.rm = T)),
            across(starts_with("x"), ~ round(., 0))
            ) %>% 
  select(1:4,paste0('x', 2010:2021)) 


# 3 - valor do m² por bairro --------------------------------------------------------------------------
itbi_m2_bairro <- itbi_m2_rua %>% 
  group_by(codbairro, bairro) %>% 
  summarise(across(starts_with("x"), ~ mean(., na.rm = T)),
            across(starts_with("x"), ~ round(., 0))) 
