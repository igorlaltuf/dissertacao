# Rede das empresas Operadoras do Cons�rcio Operacional BRT

# Liga��es DIRETAS E INDIRETAS

# Dados importados do projeto https://github.com/belisards/mapa-onibus-rj 

# 1 - Carregar pacotes ----------------------------------------------------------

library(visNetwork)
library(networkD3)
library(dplyr)
library(stringr)

# 2 - Importar dados ------------------------------------------------------------

# o arquivo nodes precisa ter apenas uma coluna com um valor diferente para cada linha (id)

nodes <- read.csv('input/csv onibus/nodes.csv') %>% 
  janitor::clean_names() %>% 
  dplyr::filter(tipo != 'Cons�rcio') %>% # remover os cons�rcios
  mutate(label = str_to_title(label),
         value = 1) %>% 
  add_row(id = 476, # adicionar o node do BRT
          nome = "Cons�rcio Operacional BRT", 
          label = "BRT", 
          tipo = 'Cons�rcio',
          value = 8) %>% 
  mutate(value = case_when(tipo == "Cons�rcio" ~ 10,
                           tipo == "Operadora" ~ 5,
                           tipo == "Ex-operadora" ~ 5,
                           tipo == "Pessoa F�sica" ~ 1,
                           tipo == "Empresa" ~ 1))
               
# para a fun��o visNetwork funcionar, o arquivo edges precisa ter
# duas colunas que v�o fazer o elo com a coluna id da outra tabela: from e to.

edges <- read.csv('input/csv onibus/edges.csv') %>% 
  rename(from = Source,
         to = Target) 







# 3 - Criar linhas com as liga��es diretas e indiretas (edges) ---------------------------------------

# Empresas de �nibus participantes do Cons�rcio Operacional BRT
empresas <- c('Auto Viacao Jabour Ltda',
              'Transportes Santa Maria Ltda',
              'Expresso Pegaso Ltda',
              'Transportes Barra Ltda',
              'Transportes Futuro Ltda',
              'Translitoral Transporte Ltda.',
              'Viacao Redentor Ltda',
              'Viacao Normandy Do Triangulo Ltda',
              'Auto Viacao Tres Amigos S A',
              'Viacao Madureira Candelaria Ltda',
              'Caprichosa Auto Onibus Ltda',
              'Transportes Paranapuan S A',
              'Auto Viacao Tijuca S/a ',
              'Real Auto Onibus Ltda ',
              'Auto Viacao Bangu Ltda',
              'Transportes Campo Grande Ltda',
              'Transurb S/a')

# Percentual de participa��o sobre o cons�rcio operacional BRT
participacao <- c(18, 13, 11.4, 10.6, 7.7, 
                  7.7, 7.3, 5.9, 4.4, 3.7, 
                  3.4, 2.4, 1.5, 1.2, 0.7, 
                  0.7, 0.6)


# Criar um df de edges com dados de liga��es diretas

brt_part <- data.frame(empresas, participacao) %>% 
  left_join(nodes, by = c('empresas' = 'nome'))

edges_diretos_from <- edges %>% 
  dplyr::filter(from %in% brt_part$id)

edges_diretos_to <- edges %>% 
  dplyr::filter(to %in% brt_part$id)

edges_diretos <- rbind(edges_diretos_from, edges_diretos_to)


# Criar um df de edges com liga��es indiretas com o BRT id(146 e 118)

edges_indiretos_from <- edges %>% 
  dplyr::filter(from %in% c(146, 118))

edges_indiretos_to <- edges %>% 
  dplyr::filter(to %in% c(146, 118))

edges_indiretos <- rbind(edges_indiretos_from, edges_indiretos_to)

# Juntar os dois tipos
edges_brt <- rbind(edges_diretos, edges_indiretos)


# Adicionar liga��es das empresas com o Cons�rcio Operacional BRT
from <- brt_part$id
to <- rep(476, times = 17)
brt_part <- data.frame(from, to) %>% 
  mutate(Type = 'Directed',
         id = NA,
         label = NA,
         timeset = NA,
         weight = 1)

edges_brt <- rbind(edges_brt, brt_part) %>% unique()



# 4 - Criar linhas com os nodes relacionados ao BRT----------------------------------


nodes_id_brt <- append(edges_brt$from,edges_brt$to) %>% 
  unique()

nodes_brt <- nodes %>% 
  dplyr::filter(id %in% nodes_id_brt) %>% 
  mutate(color.background = case_when(tipo == "Cons�rcio" ~ '#8983B8',
                                      tipo == "Operadora" ~ '#DACA8F',
                                      tipo == "Ex-operadora" ~ '#DACA8F',
                                      tipo == "Pessoa F�sica" ~ '#F7F2F6',
                                      tipo == "Empresa" ~ '#75C987'),
         font.size =  case_when(tipo == "Cons�rcio" ~ 30,
                                tipo == "Operadora" ~ 24,
                                tipo == "Ex-operadora" ~ 24,
                                tipo == "Pessoa F�sica" ~ 10,
                                tipo == "Empresa" ~ 10) 
  ) 

 
x <- visNetwork(nodes = nodes_brt, edges = edges_brt)
  visLayout(randomSeed = 12) # to have always the same network  
                

# edge com grossura diferente


x

visSave(x, 'output/rede_brt.html', selfcontained = TRUE, background = "white")

