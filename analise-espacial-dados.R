# Instalando e carregando os pacotes necessários
if(!require(tidyverse)) install.packages("tidyverse");library(tidyverse)
if(!require(foreign)) install.packages("foreign");library(foreign)
if(!require(readxl)) install.packages("readxl");library(readxl)
if(!require(janitor)) install.packages("janitor");library(janitor)
if(!require(stringi)) install.packages("stringi");library(stringi)
if(!require(sf)) install.packages("sf");library(sf)
if(!require(mapsf)) install.packages("mapsf");library(mapsf)
if(!require(tmap)) install.packages("tmap");library(tmap)
if(!require(ggspatial)) install.packages("ggspatial");library(ggspatial)

setwd('C:/Users/ernan/Documents/R')
getwd()

# Importando dados vetoriais de pontos com a função read_sf() e salvando no objeto `ac_unidades`
ac_unidades <- read_sf('Dados/ac_unidades_saude_m.shp')

# Analisando o arquivo importado
ac_unidades

# Importando dados vetoriais de linhas com a função read_sf() e salvando no objeto `ac_vias`
ac_vias <- read_sf('Dados/ac_vias.shp')
# Analisando o arquivo importado
ac_vias

#Importando dados vetoriais de polígonos com a função read_sf() e salvando no objeto `ac_municipios`
ac_municipios <- read_sf('Dados/ac_municipios.shp')
# Analisando o arquivo importado
ac_municipios

# Visualizando informações sobre o objeto ac_municipios com a função st_geometry()
st_geometry(ac_municipios)

# Verificando o sistema de coordenadas de referência
st_crs(ac_municipios)

# Transformando o sistema de coordenadas do objeto `ac_municipios` com a função st_transform()
ac_municipios_5880 <- st_transform(x = ac_municipios, crs = 5880)
# Verificando a trasnformação do # sistema de coordenadas de referência
st_crs(ac_municipios_5880)

# Plotando e visualizando o objeto `ac_municipios`
plot(x = st_geometry(ac_municipios))

# Plotando e visualizando o objeto ac_municipios
plot(
  # Definindo o objeto que será utilizado para a visualização do mapa
  x = st_geometry(ac_municipios),
  
  # Definindo como verdadeiro o argumento para plotagem das coordenadas 
  # nos eixos do gráfico
  axes = TRUE,
  
  # Definindo como verdadeiro o argumento para plotagem da grade com as
  # coordenadas
  graticule = TRUE,
  
  # Definindo a cor de preenchimento (código hexadecimal)
  col = "#FAFAFA",
  
  # Definindo os rótulos dos eixos x e y
  xlab = "longitude",
  ylab = "latitude"
)

# Utilizando a função ggplot() para plotagem dos mapas
ggplot() +
  
  # Plotando o objeto de polígono com os contornos do estado
  # O argumento "fill" define a cor de preenchimento
  geom_sf(data = ac_municipios, fill = "#FAFAFA") +
  
  # Plotando o objeto de linhas para visualização de estradas
  # O argumento "color" define a cor das linhas
  geom_sf(data = ac_vias, color = "#BABABA") +
  
  # Plotando o objeto de pontos para visualização das unidades de saúde
  # O argumento "color" define a cor de preenchimento
  geom_sf(data = ac_unidades, color = "#FF0000")

# Utilizando a função ggplot() para plotagem dos mapas
ggplot()+
  
  # Adicionando camada com os limites poligonais do estado do AC com o 
  # objeto `ac_municipios` e a função geom_sf().
  # O argumento "fill" define o codinome da cor de preenchimento
  geom_sf(data = ac_municipios, aes(fill = "cor_municipios")) + 
  
  # Adicionando camada com objeto de linhas representando as vias com o 
  # objeto `ac_vias` e a função geom_sf()
  # O argumento "color" define o codinome da cor das linhas
  geom_sf(data = ac_vias, aes(color = "cor_ruas")) +
  
  # Adicionando camada com objeto de pontos representando unidades de
  # saúde com o objeto `ac_unidades` e a função geom_sf()
  # O argumento "color" define o codinome da cor de preenchimento
  geom_sf(data = ac_unidades, aes(color = "cor_unidades")) +
  # Definindo a escala gráfica do mapa com a função annotation_scale()
  annotation_scale(location = "br",
                   height = unit(.1, "cm")) +
  
  # Definindo a visualização de seta apontada para o Norte 
  annotation_north_arrow(location = "tr",
                         height = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering) +
  
  # Criando a legenda das linhas e pontos
  scale_color_manual(
    name = "",
    guide = guide_legend(override.aes = list(linetype = c("solid", "blank"), 
                                             shape = c(NA, 16))),
    values = c('cor_ruas' = '#BABABA', 'cor_unidades' = '#FF0000'),
    labels = c("Vias", "Unidades de saúde")
  ) +
  
  # Criando a legenda dos polígonos
  scale_fill_manual(
    name = "",
    values = c('cor_municipios' = '#FAFAFA'),
    labels = "Municípios"
  ) +
  
  # Definindo o título e rótulos dos eixos do gráfico 
  labs(title = "Unidades de saúde e arruamento do Estado do Acre, 2022.",
       x = "longitude",
       y = "latitude",
       caption = "Fonte dos dados: IBGE e Ministério da Saúde") +
  
  # Adicionando o tema
  theme_bw()

# Instalando e carregando o pacote `geobr`
if(!require(geobr)) install.packages("geobr");library(geobr)
# Realizando o download dos municípios do Estado do Acre
ac_municipios_geobr <- read_municipality("AC")
# Visualizando o objeto `ac_municipios_geobr`
plot(st_geometry(ac_municipios_geobr))

# Instalando e carregando o pacote `osmextract`
if(!require(osmextract)) install.packages("osmextract");library(osmextract)
# Realizando o download dos dados do Estado do Acre e filtrando apenas as vias
ac_via_osm <- oe_get(place = "Acre") |> 
  filter(!is.na(highway))

# Visualizando o objeto `ac_via_osm`
plot(st_geometry(ac_via_osm))

# Inspecionando o objeto ac_unidades com a função glimpse()
glimpse(ac_unidades)

# Carregando base de dados com a função read_xlsx()
tabela_unidades <- read_xlsx('Dados/ac_unidades_tabela.xlsx')
# Inspecionando o objeto tabela_unidades com a função glimpse()
glimpse(tabela_unidades)

# Salvando o objeto `ac_unidades_completo` pela união da tabela
# `ac_unidades` com a tabela tabela_unidades
ac_unidades_completo <- ac_unidades |>
  # Atilizando a função left_join() para união.
  # A ligação é feita pela correspondência entre as
  # colunas "cnes_n" e "codigo_cnes"
  left_join(tabela_unidades, by = c("cnes_n" = "codigo_cnes"))

glimpse(ac_unidades_completo)

# Carregando tabela com dados de população do acre para o objeto pop_ac
# utilizando a função read_csv2()
pop_ac <- read_csv2('Dados/pop_ac_09_21.csv', col_types = list("character"))

# Padronizando o nome das colunas utilizando a função `clean_names()`do pacote janitor
pop_ac <- clean_names(pop_ac)

# Unindo a tabela ac_municipios_5880 com pop_ac com a função left_join()
pop_geo_ac <- ac_municipios_5880 |>
  
  # A ligação é feita pela correspondência entre as colunas "cod_mun" e "codigo"
  left_join(pop_ac, by = c('cod_mun' = 'codigo'))

# Plotando o objeto pop_geo_ac com uso da função mf_map()
mf_map(pop_geo_ac) |>
  # Indicando o tamanho proporcional da população com uso da função mf_prop()
  mf_prop(var = "x2021")

# Definindo a cor de fundo por trás do mapa como "branco",
# com uso da função mf_theme()
mf_theme(bg = "white")

# Plotando o objeto pop_geo_ac com uso da função mf_map()
# O argumento "col" define a cor de preenchimento utilizando o código de
# cor hexadecimal
mf_map(pop_geo_ac, col = "#FAFAFA") |>
  # Indicando o tamanho proporcional da população com uso da função mf_prop()
  mf_prop(
    # Definindo a variável x2021 para cálculo do tamanho proporcional dos círculos
    var = "x2021",
    # Definindo a posição da legenda para "abaixo e à esquerda"
    leg_pos = "bottomleft",
    # Definindo o título do gráfico
    leg_title = "Populacao Acre 2021",
    # O argumento "col" define a cor de preenchimentto dos círculos utilizando
    # o código de cor hexadecimal
    col = "#FFAD48"
  )

# Definindo a posição da seta de norte para a região "superior e à direita"
# com a função mf_arrows()
mf_arrow(pos = "topright")

# Inserindo a escala com a função mf_scale()
mf_scale()

# Utilizando o pacote mapsf também é possível inserir título e notas de rodapé, 
#utilizando as funções mf_title() e mf_credits(). 
mf_theme(bg = "white")
mf_map(pop_geo_ac, col = "#FAFAFA") |> 
  mf_prop(
    var = "x2021",
    leg_pos = "bottomleft1",
    leg_title = "Populacao Acre 2021",
    col = "#FFAD48"
  ) 
mf_arrow(pos = "topright")
mf_scale(pos = "bottomright")
mf_title(txt = "Distribuicao espacial da populacao do Estado do Acre segundo municipio, 2021.", cex = .8)
mf_credits(txt = "Fonte: IBGE-2021.", pos = "bottomleft")

# Importação da base de casos do SINAN para o R, utilizando o pacote foreign.
base_hans <- read.dbf('Dados/base_hans_ac.dbf', as.is = TRUE)
casos_hans_ac_21 <- base_hans |>
  # Filtrando para os casos que o Tipo de Alta está em branco (TPALTA_N), 
  # denotando os casos em acompanhamento.
  filter(is.na(TPALTA_N)) |>
  # Sumarização dos casos por município de atendimento (MUNIRESAT).
  count(MUNIRESAT)
prev_casos_ac <- casos_hans_ac_21 |>
  # Unindo a tabela de casos à tabela de população pelo geocódigo do município de atendimento.
  left_join(pop_ac, by = c("MUNIRESAT" = "codigo")) |> 
  # Filtrando os municípios que não foram encontrados na união.
  filter(!is.na(MUNIRESAT))
# Calculando a prevalência por município e seleção das colunas necessárias para o mapa.
prevalencia_hans_ac <- prev_casos_ac |> 
  mutate(prevalencia_2021 = (n / x2021) * 10000) |> 
  select(MUNIRESAT, municipio, prevalencia_2021)
# Visualizando a tabela
prevalencia_hans_ac

# Unindo a tabela `prevalencia_hans_ac` com `ac_municipios_5880` 
# com a função left_join() e salvando no objeto `prevalencia_mun`
prevalencia_mun <- left_join(
  x = ac_municipios_5880,
  y = prevalencia_hans_ac,
  # A ligação é feita pela correspondência entre as colunas
  # "cod_mun" e "MUNIRESAT"
  by = c("cod_mun" = "MUNIRESAT")
) 

# Plotando o objeto prevalencia_mun com uso da função mf_map()
mf_map(prevalencia_mun) |>
  
  # Adicionando camada de mapa coroplético com a função mf_choro() 
  mf_choro(
    
    # Definindo a variável que será utilizada para a cor de preenchimento
    var = "prevalencia_2021",
    
    # Definindo os intervalos de valores
    breaks = "quantile",
    
    # Definindo a paleta de cores para "viridis"
    pal = "Viridis",
    
    # Definindo a posição da legenda para a região "inferior e à esquerda"
    leg_pos = "bottomleft1",
    
    # Definindo o título da legenda
    # 
    leg_title = "Prevalência \nHanseníase \n2021",
    
    # Inserindo caixa adicional para a legenda
    leg_no_data = "Sem dados"
  )

# Definindo a posição da seta de norte para a região "superior e à direita" com a 
função mf_arrows()
mf_arrow(pos = "topright")
# Inserindo a escala com a função mf_scale()
mf_scale()
# Definindo o título do mapa e definindo o tamanho do texto
# para encaixar melhor no título
mf_title(txt = "Distribuicao espacial da prevalência de Hanseníase no Estado do 
Acre segundo municipio, 2021.",
         cex = 0.8)
# Definindo informações complementares
mf_credits(txt = "Fonte: IBGE/2021 e SINAN/MS 2021.", pos = "bottomleft")

# Carregando a base de dados para o objeto ac_internacao utilizando 
# a função read.dbf()
ac_internacao <- read.dbf("Dados/ac_sih.dbf", as.is = TRUE)

# Visualizando as primeiras linhas da tabela importada
head(ac_internacao)

# Criando uma nova tabela com o nome ac_capitulo_15
ac_capitulo_15 <- ac_internacao |> 
  
  # Contando a frequência de registros por município de residência
  # (coluna MUNIC_RES) e município de internação (coluna MUNIC_MOV)
  count(MUNIC_RES, MUNIC_MOV) |> 
  
  # Filtrando os registros em que o município de residência difere
  # do município de internação e com uma frequência maior ou igual a 10
  filter(MUNIC_RES != MUNIC_MOV, n >= 10)

# Criando as linhas de ligação com a frequência de deslocamento
ac_deslocamentos <- mf_get_links(
  x = ac_municipios_5880, # um arquivo vetorial de municípios
  x_id = "cod_mun", # variável de ligação
  df = ac_capitulo_15, # arquivo de frequência de deslocamentos
  df_id = c("MUNIC_RES", "MUNIC_MOV") # variáveis de origem e destino
)

head(ac_deslocamentos)

# Plotando o objeto `ac_municipios_5880` com uso da função mf_map()
# O argumento "col" define a cor de preenchimento utilizando o código de
# cor hexadecimal
mf_map(ac_municipios_5880, col = "#FAFAFA")
# Inserindo camada indicando os descolamentos com uso da função mf_grad()
mf_grad(
  
  # Definindo o objeto com a frequência sobre os deslocamentos
  x = ac_deslocamentos,
  
  # Definindo a variável que indicará a espessura da linha conforme a frequência
  var = "n",
  
  # Definindo o número mínimo de intervalos para os valores de espessura
  nbreaks = 3,
  
  # Definindo o método de classificação dos intervalos para quantis
  breaks = "quantile",
  
  # Definindo a espessura da linha relativa a cada intervalo
  lwd = c(.7, 3, 7),
  
  # Definindo a posição da legenda para "inferior e à esquerda"
  leg_pos = "bottomleft1",
  
  # Definindo o título do gráfico
  leg_title = "Número de fluxos",
  
  # Definindo a cor da linha com o código de cor hexadecimal.
  # Para definir a opacidade, inserimos dois dígitos no final do
  # código. Dessa forma, nossas linhas ficarão mais transparentes,
  # melhorando a visualização
  col = "#5DC86395"
)
# Definindo a posição da seta de norte para a
# "superior e à direita" com a função mf_arrows()
mf_arrow(pos = "topright")
# Inserindo a escala com a função mf_scale()
mf_scale()
# Definindo o título do mapa e definindo o tamanho do texto
# para encaixar melhor no título
mf_title(txt = "Fluxo de deslocamento das internações de mulheres por gravidez, parto e puerpério, Acre, 2021.",
         cex = 0.8)
# Inserindo informações complementares
mf_credits(txt = "Fonte: IBGE/2021 e SINAN/MS 2021.", pos = "bottomleft")
