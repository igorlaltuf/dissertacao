source('Rscripts/00_carregar_bibliotecas.R')

# Credenciais 
credencial <- Sys.getenv("CREDENCIAL_BASE_DOS_DADOS")
basedosdados::set_billing_id(credencial)

# Importar dados
cod.bairros <- read_excel('Input/RAIS_vinculos_layout2018e2019.xls',sheet=4,skip=1)

# Consulta via Big Query
query <- "SELECT id_municipio, ano, bairros_rj, cnae_2, vinculo_ativo_3112 FROM `basedosdados.br_me_rais.microdados_vinculos`
          WHERE ano = 2019 AND id_municipio = '3304557' AND vinculo_ativo_3112 = '1'"

bairros.rj <- read_sql(query)

# 1 - Criar mapa de vínculos por bairros
bairros.rj.ativos <- bairros.rj %>% 
                     select(3,5) %>% 
                     group_by(bairros_rj) %>% 
                     summarise(vinc_ativo_bairro = sum(vinculo_ativo_3112, na.rm = TRUE))

colnames(cod.bairros) <- c('categoria','bairro','codigo')
cod.bairros$codigo <- as.integer(cod.bairros$codigo)
bairros.rj.ativos$bairros_rj <- as.integer(bairros.rj.ativos$bairros_rj)

bairros.rj.ativos <- left_join(bairros.rj.ativos,cod.bairros,by=c('bairros_rj'='codigo'))

# Vagas por bairro (verificar NAs)
128522/sum(bairros.rj.ativos$vinc_ativo_bairro) # 5,71% dos valores estão como NA
 
bairros.rj.ativos <- bairros.rj.ativos %>% 
                     drop_na() # remove linhas que contenham um campo com NA





# Continuar daqui (plotar mapa)



# 2 - Evolução dos vínculos ativos na cidade do Rio de Janeiro
query <- "SELECT id_municipio,ano,SUM(vinculo_ativo_3112) as vinculos_ativos FROM `basedosdados.br_me_rais.agregado_vinculos_municipio_vinculo_ativo_cbo_cnae_natureza_juridica_idade_sexo_raca`
          WHERE id_municipio = 3304557
          GROUP BY id_municipio, ano"
df <- read_sql(query)
df$vinculos_ativos <- as.numeric(df$vinculos_ativos)
df$ano <- as.numeric(df$ano)


# Gráfico com a evolução anual de vínculos ativos (em milhões)
ggplot(df,aes(ano,vinculos_ativos/1000000))+
  ggtitle("Evolução dos vínculos ativos de emprego formais \n na cidade do Rio de Janeiro")+
  geom_line(color='blue', linetype = 'dashed')+
  geom_point(color='black')+
  ylab('Vinculos formais ativos (em milhões)')+
  scale_x_continuous(name = 'Ano', # dado do eixo x
                     limits = c(2010,2019),  # valor máximo e mínimo do eixo
                     n.breaks = 10) +
  theme(panel.background = element_blank())+
  theme(panel.background = element_blank(),plot.title = element_text(hjust = 0.5))# centraliza o texto

# 3 - Evolução dos empregos formais na cidade do Rio (todos os vínculos)
query <- "SELECT id_municipio,ano,SUM(numero_vinculos) as vinculos FROM `basedosdados.br_me_rais.agregado_vinculos_municipio_vinculo_ativo_cbo_cnae_natureza_juridica_idade_sexo_raca`
          WHERE id_municipio = 3304557
          GROUP BY id_municipio, ano"
df <- read_sql(query)

df$vinculos <- as.numeric(df$vinculos)
df$ano <- as.numeric(df$ano)

# Gráfico com a evolução anual do número de vínculos (em milhões)
ggplot(df,aes(ano,vinculos/1000000))+
  ggtitle("Evolução do número de vínculos de emprego formais \n na cidade do Rio de Janeiro")+
  geom_line(color='blue', linetype = 'dashed')+
  geom_point(color='black')+
  ylab('Vinculos formais (em milhões)')+
  scale_x_continuous(name = 'Ano', # dado do eixo x
                     limits = c(2010,2019),  # valor máximo e mínimo do eixo
                     n.breaks = 10) +
  theme(panel.background = element_blank(),plot.title = element_text(hjust = 0.5))# centraliza o texto

##### Recorte de funcionários de empresas de ônibus na cidade do Rio de Janeiro
# 4 - Empregados empresas de ônibus municipal ou reg metropolitana
# cod cnae 49213 Transporte rodoviário coletivo de passageiros, com itinerário fixo, municipal e em região metropolitana
query <- "SELECT id_municipio,ano,SUM(vinculo_ativo_3112) as vinculos_ativos FROM `basedosdados.br_me_rais.agregado_vinculos_municipio_vinculo_ativo_cbo_cnae_natureza_juridica_idade_sexo_raca`
          WHERE id_municipio = 3304557 AND cnae_2 = '49213'
          GROUP BY id_municipio, ano"
df.onibus <- read_sql(query)
df.onibus$vinculos_ativos <- as.numeric(df.onibus$vinculos_ativos)
df.onibus$ano <- as.numeric(df.onibus$ano)

ggplot(df.onibus,aes(ano,vinculos_ativos))+
  geom_line(color='blue', linetype = 'dashed')+
  geom_point(color='black')+
  ylab('Vinculos formais ativos de empregados em \n empresas de ônibus municipal ou na RM')+
  scale_y_continuous(limits = c(7500,9000),  # valor máximo e mínimo do eixo
                     n.breaks = 6)+
  scale_x_continuous(name = 'Ano', # dado do eixo x
                     limits = c(2008,2019),  # valor máximo e mínimo do eixo
                     n.breaks = 10) +
  theme(panel.background = element_blank())

ggsave('output/01_rais/01_cnae_49213_muni_rio_rais.png')


# Vinculos formais ativos da ocupação cobrador de transportes coletivos (exceto trem) CBO: 511215
query <- "SELECT id_municipio,ano,cbo_2002,SUM(vinculo_ativo_3112) as vinculos_ativos FROM `basedosdados.br_me_rais.agregado_vinculos_municipio_vinculo_ativo_cbo_cnae_natureza_juridica_idade_sexo_raca`
          WHERE id_municipio = 3304557 AND cbo_2002 = '511215'
          GROUP BY id_municipio, ano, cbo_2002"
df.trocador <- read_sql(query)

df.trocador$vinculos_ativos <- as.numeric(df.trocador$vinculos_ativos)
df.trocador$ano <- as.numeric(df.trocador$ano)

ggplot(df.trocador,aes(ano,vinculos_ativos))+
  geom_line(color='blue', linetype = 'dashed')+
  geom_point(color='black')+
  ylab('Vinculos formais ativos da ocupação \n cobrador de transportes coletivos (exceto trem)')+
  scale_x_continuous(name = 'Ano', # dado do eixo x
                     limits = c(2008,2019),  # valor máximo e mínimo do eixo
                     n.breaks = 10) +
  theme(panel.background = element_blank())

ggsave('output/01_rais/01_cob_511215_muni_rio_rais.png')


# Vínculos formais ativos da ocupação motorista de ônibus urbano CBO:782410
query <- "SELECT id_municipio,ano,cbo_2002,SUM(vinculo_ativo_3112) as vinculos_ativos FROM `basedosdados.br_me_rais.agregado_vinculos_municipio_vinculo_ativo_cbo_cnae_natureza_juridica_idade_sexo_raca`
          WHERE id_municipio = 3304557 AND cbo_2002 = '782410'
          GROUP BY id_municipio, ano, cbo_2002"
df.motorista <- read_sql(query)

df.motorista$vinculos_ativos <- as.numeric(df.motorista$vinculos_ativos)
df.motorista$ano <- as.numeric(df.motorista$ano)

ggplot(df.motorista,aes(ano,vinculos_ativos))+
  geom_line(color='blue', linetype = 'dashed')+
  geom_point(color='black')+
  ylab('Vinculos formais ativos da ocupação \n motorista de ônibus urbano')+
  scale_x_continuous(name = 'Ano', # dado do eixo x
                     limits = c(2003,2019),  # valor máximo e mínimo do eixo
                     n.breaks = 10) +
  theme(panel.background = element_blank())

# usar esta query para ver os campos da tabela do banco de dados.
query <- 'SELECT *  FROM `basedosdados.br_me_rais.agregado_vinculos_municipio_vinculo_ativo_cbo_cnae_natureza_juridica_idade_sexo_raca`
          LIMIT 5'

# Vinculos formais totais da ocupação cobrador de transportes coletivos (exceto trem) CBO: 511215
query <- "SELECT id_municipio,ano,cbo_2002,SUM(numero_vinculos) as vinculos FROM `basedosdados.br_me_rais.agregado_vinculos_municipio_vinculo_ativo_cbo_cnae_natureza_juridica_idade_sexo_raca`
          WHERE id_municipio = 3304557 AND cbo_2002 = '511215'
          GROUP BY id_municipio, ano, cbo_2002"
df.trocador <- read_sql(query)

df.trocador$vinculos <- as.numeric(df.trocador$vinculos)
df.trocador$ano <- as.numeric(df.trocador$ano)

ggplot(df.trocador,aes(ano,vinculos))+
  geom_line(color='blue', linetype = 'dashed')+
  geom_point(color='black')+
  ylab('Número de vinculos da ocupação \n cobrador de transportes coletivos (exceto trem)')+
  scale_x_continuous(name = 'Ano', # dado do eixo x
                     limits = c(2003,2019),  # valor máximo e mínimo do eixo
                     n.breaks = 10) +
  theme(panel.background = element_blank())