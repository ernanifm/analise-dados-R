library(dplyr)
library(lubridate)
library(foreign)
library(xts)
library(ggplot2)
library(readr)
library(tidyr)
library(purrr)
library(stringr)
library(forcats)

# Atribuição de dados para consultar a API do InfoDengue
url <- "https://info.dengue.mat.br/api/alertcity?" # inserimos o endereço do InfoDengue
geocode <- 4108304 # indicamos código do IBGE de Foz do Iguaçu
disease <- "dengue" # selecionamos a doença
format <- "csv" # indicamos o formato de arquivo que será baixado
ew_start <- 1 # indicamos o início da semana epidemiológica
ew_end <- 53 # indicamos o final da semana epidemiológica
ey_start <- 2012 # indicamos o início do ano a ser exportado
ey_end <- 2022 # indicamos o final do ano a ser exportado

# programando o R para ele trazer os dados diretamente da internet
consulta <- paste0(url,
                   "geocode=", geocode,
                   "&disease=", disease,
                   "&format=", format,
                   "&ew_start=", ew_start,
                   "&ew_end=", ew_end,
                   "&ey_start=", ey_start,
                   "&ey_end=", ey_end)

# visualizando o link armazenado no objeto {`consulta`}
consulta

# Armazenando o banco de dados do InfoDengue {consulta} no objeto {dengue_foz} para analisá-lo
dengue_foz <- read_csv(consulta)

# criando o objeto {`foz_ts`}
foz_ts <- xts(
  # selecionando a variável com os dados da incidência de dengue
  x = dengue_foz$p_inc100k,
  # selecionando a variável que contém as datas correspondentes
  order.by = dengue_foz$data_iniSE)

# Plotando o diagrama de controle x indicando a série temporal, main, colocando o título do gráfico
# escrevendo o título do eixo y
plot( x = foz_ts, main = 'Distribuição da incidência de dengue em Foz do Iguaçu/PR', ylab)

# Analisando as estatísticas básicas do banco de dengue importado
summary(dengue_foz$p_inc100k)  

# Plotando gráfico boxplot com incidência por (`~`) ano, segundo https://bvsms.saude.gov.br/bvs/publicacoes/pncd_2002.pdf
# Baixa incidência: até 100 casos por 100 mil habitantes;
# Média incidência: entre 101 e 299 casos por 100 mil habitantes, aqui utilizaremos como referência 200 casos;
# Alta incidência: 300 casos ou mais por 100 mil habitantes ou mais.
boxplot(dengue_foz$p_inc100k ~ year(dengue_foz$data_iniSE),
        ylab = 'Incidência por 100.000 hab.',
        xlab = "Ano de início da semana epidemiológica",
        # Inserindo o título do boxplot
        main = "Distribuição da incidência anual de dengue em Foz do Iguaçu-PR entre 2012-2022.")
# Criando linhas de análise a partir dos parâmetros que definimos
abline(
  h = c(300, 200, 70),
  lty = 2,
  lwd = 2,
  col = c('red', 'orange', 'blue')
)  

# O boxplot estratificado indica anos não-epidêmicos. Criando o objeto {`nao_epidemic`} com anos não epidêmicos
nao_epidemic <- c(2012, 2013, 2014, 2017, 2018)

# Criando o objeto {`dengue_2022`} com ano de 2022
dengue_2022 <- dengue_foz |>
  # Filtrando o ano para 2022
  filter(year(data_iniSE) == 2022) |>
  # Criando uma nova coluna chamada 'sem_epi', referente à semana epidemiológica
  mutate(sem_epi = epiweek(data_iniSE))

# Criando o gráfico com o diagrama de controle
dengue_stat <- dengue_foz |>
  # Filtrando os dados da série em que o ano não é epidêmico
  filter(year(data_iniSE) %in% nao_epidemic) |>
  # Criando uma nova coluna chamada 'sem_epi', referente à semana epidemiológica
  mutate(sem_epi = epiweek(data_iniSE)) |>
  # Agrupando os dados pela semana epidemiológica
  group_by(sem_epi) |>
  # Criando medidas-resumo e limites superior e inferior
  summarise(
    n = n(),
    media = mean(p_inc100k, na.rm = TRUE),
    desvio = sd(p_inc100k, na.rm = TRUE) ,
    sup = media + 2 * desvio,
    inf = media - 2 * desvio
  )

head(dengue_stat)
# diagrama de controle de dengue. O gráfico inclui os valores de incidên  cia da dengue às 
# semanas epidemiológicas do ano de 2022.
# Definindo a base a ser utilizada
ggplot(data = dengue_stat) +
  # Definindo argumentos estéticos com as variáveis usadas em x e em y
  aes(x = sem_epi, y = media) +
  # Adicionando a linha referente à incidência média de dengue, na cor azul e largura de 1.2 pixel
  geom_line(aes(color = 'cor_media_casos'), size = 1.2) +
  # Adicionando uma geometria de linha na cor laranja. Além disso, inserindo
  # um argumento estético para o eixo y que, no caso, é a variável de limite superior
  geom_line(aes(y = sup, color = 'cor_limite'), size = 1.2) +
  # Adicionando uma geometria de colunas utilizando a base de dados
  # {`dengue_2022`} e y como a incidência de dengue em 2022. O argumento
  # `fill` refere-se à cor das barras e `alpha` à transparência.
  geom_col(data = dengue_2022,
           aes(y = p_inc100k, fill = 'cor_incidencia'), alpha = 0.4) +
  # Arrumando o eixo x, definindo o intervalo de tempo que será utilizado (`breaks`)
  # uma sequência de semanas epidemiológicas de 1 a 53
  # o argumento `expand` ajuda nesse processo.
  scale_x_continuous(breaks = 1:53, expand = c(0, 0)) +
  # Definindo os títulos dos eixos x e y
  labs(x = 'Semana Epidemiológica',
       y = 'Incidência por 100 mil hab.',
       title = 'Diagrama de controle de dengue em Foz do Iguaçu/PR no ano de 2022.') +
  # Definindo o tema do gráfico
  theme_classic() +
  # Criando a legenda das linhas
  scale_color_manual(
    name = "",
    values = c('cor_media_casos' = 'darkblue', 'cor_limite' = 'red'),
    labels = c("Incidência média", "Limite superior")
  ) +
  # Criando a legenda das barras
  scale_fill_manual(
    name = "",
    values = c('cor_incidencia' = 'deepskyblue'),
  )
labels = "Incidência de dengue em 2022")

# Para um gráfico mais suavizado
# Definindo a base a ser utilizada
ggplot(data = dengue_stat) +
  # Definindo argumentos para as variáveis do eixo x e y do gráfico
  aes(x = sem_epi, y = media) +
  # Suavizando a linha referente à incidência média de dengue
  # o argumento `size` para definir largura da linha = 1.2 pixel
  # o argumento `se` = FALSE desabilita o intervalo de confiança
  # e o argumento `span` definindo o valor da suavização
  stat_smooth(
    aes(color = 'cor_incidencia_media'),
    size = 1.2,
    se = FALSE,
    span = 0.2
  ) +
  # Suavizando a linha referente ao limite superior
  stat_smooth(
    aes(y = sup, color = 'cor_limite'),
    size = 1.2,
    se = FALSE,
    span = 0.2
  ) +
  # Adicionando uma geometria de colunas utilizando a base de dados
  # {`dengue_2022`} e y como a incidência de dengue em 2022.
  geom_col(data = dengue_2022,
           aes(y = p_inc100k, fill = 'cor_incidencia'),
           alpha = 0.4) +
  # Arrumando o eixo x, definindo o intervalo de tempo que será utilizado (`breaks`)
  # uma sequência de semanas epidemiológicas de 1 a 53
  # o argumento `expand` ajuda nesse processo.
  scale_x_continuous(breaks = 1:53, expand = c(0, 0)) +
  # Definindo os títulos dos eixos x, y e também do gráfico
  labs(x = 'Semana Epidemiológica',
       y = 'Incidência por 100 mil hab.',
       title = 'Diagrama de controle de dengue em Foz do Iguaçu/PR no ano de 2022.') +
  # Definindo o tema do gráfico
  theme_classic() +
  # Criando a legenda das linhas
  scale_color_manual(
    name = "",
    values = c('cor_incidencia_media' = 'darkblue', 'cor_limite' = 'red'),
    labels = c("Incidência média de casos", "Limite superior")
  ) +
  # Criando a legenda das barras
  scale_fill_manual(
    name = "",
    values = c('cor_incidencia' = 'deepskyblue'),
    labels = "Incidência de dengue em 2022"
  )
