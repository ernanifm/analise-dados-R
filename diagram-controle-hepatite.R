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

# criando objeto do tipo dataframe (tabela) {sinan_hep_sp_2007_2016} com o banco de
# dados {sinan_hep_sp_2007_2016.dbf}
sinan_hep_sp_2007_2016 <- read.dbf("Dados/sinan_hep_sp_2007_2016.dbf", as.is = TRUE)
# criando objeto do tipo dataframe (tabela) {sinan_hep_sp_fev_2017} com o banco de
# dados {sinan_hep_sp_fev_2017.dbf}
sinan_hep_sp_fev_2017 <- read.dbf("Dados/sinan_hep_sp_fev_2017.dbf", as.is = TRUE)

# Criando a tabela {`sinan_hep_sp_cont_07_16`}
sinan_hep_sp_cont_07_16 <- sinan_hep_sp_2007_2016 |>
  # Utilizando a função `mutate()` para criar as novas colunas
  mutate(
    # Criando uma nova coluna chamada 'sem_epi', referente à semana
    # epidemiológica dos primeiros sintomas
    sem_epi = epiweek(x = DT_SIN_PRI),
    # Criando uma nova coluna chamada 'ano', referente ao ano dos primeiros sintomas
    ano = year(x = DT_SIN_PRI)) |>
  # Contando a frequência de notificações por ano e semana epidemiológica
  count(ano, sem_epi)

head(sinan_hep_sp_cont_07_16)

# Criando o objeto {`sinan_hep_sp_cont_fev17`} e
# realizando a contagem dos casos segundo a
# semana epidemiológica e ano dos primeiros sintomas
sinan_hep_sp_cont_fev17 <- sinan_hep_sp_fev_2017 |>
  mutate(sem_epi = epiweek(DT_SIN_PRI),
         ano = year(DT_SIN_PRI)) |>
  count(ano, sem_epi)

# Definindo a mediana geral de casos confirmados entre 2007 e 2016
mediana_geral <- median(sinan_hep_sp_cont_07_16$n)
# utilizando a função `boxplot()` para criar o gráfico
boxplot(
  # Definindo o cruzamento número de casos por ano
  # Aqui utilizamos o símbolo "~" para sinalizar o cruzamento das variáveis
  sinan_hep_sp_cont_07_16$n ~ sinan_hep_sp_cont_07_16$ano,
  # Definindo os títulos dos eixos x e y
  ylab = 'Número de casos confirmados de Hepatite A',
  xlab = 'Ano dos primeiros sintomas',
)
# Definindo o título do boxplot
main = 'Número de casos confirmados de Hepatite A em São Paulo/SP entre 2007-2016'
# Criando linhas de análise
abline(
  h = mediana_geral,
  lty = 2,
  lwd = 2,
  col = "red"
)

# criando o objeto {nao_epidemic} para armazenar os anos não epidêmicos
nao_epidemic <- c(2008:2013, 2015, 2016)

# Criando o objeto {`hep_stat`}
hep_stat <- sinan_hep_sp_cont_07_16 |>
  # Filtrando os anos contidos no grupo de anos não epidêmicos
  filter(ano %in% nao_epidemic) |>
  # Agrupando os dados pela semana epidemiológica
  group_by(sem_epi) |>
  # Criando medidas-resumo e limites superior e inferior
  summarise(
    media = mean(n, na.rm = TRUE),
    desvio = sd(n, na.rm = TRUE) ,
    sup = media + 2 * desvio,
    inf = media - 2 * desvio
  )

# Criando um novo objeto {`grafico_base`}
grafico_base <- ggplot(data = hep_stat) +
  # Definindo as variáveis usadas no eixo x e em y do gráfico
  aes(x = sem_epi, y = media) +
  # Adicionando uma geometria de linha com largura de 1.2 pixel
  # referente ao número médio de casos confirmados.
  # Além disso, inserindo um argumento estético para a cor,
  # que será convertida na legenda.
  geom_line(aes(color = "cor_media_casos"), size = 1.2) +
  # Adicionando uma geometria de linha referente ao limite superior.
  # Além disso, inserindo um argumento estético para o eixo y e
  # para a cor, que será convertida na legenda.
  geom_line(aes(y = sup, color = 'cor_limite'), size = 1.2) +
  # Arrumando o eixo x, definindo o intervalo de tempo que será utilizado (`breaks`)
  # uma sequência de semanas epidemiológicas de 1 a 53
  # o argumento `expand` ajuda nesse processo.
  scale_x_continuous(breaks = 1:53, expand = c(0, 0)) +
  # Definindo os títulos dos eixos x e y
  labs(x = '',
       y = '',
       title = 'Diagrama de controle') +
  # Definindo o tema do gráfico
  theme_classic() +
  # Criando a legenda das linhas
  scale_color_manual(
    name = "",
    values = c('cor_media_casos' = 'darkblue', 'cor_limite' = 'red'),
    labels = c("Média de casos confirmados", "Limite superior")
  )
#visualizando o gráfico criado
grafico_base

grafico_base_suavizado <- ggplot(data = hep_stat) +
  # Definindo as variáveis usadas no eixo x e em y do gráfico
  aes(x = sem_epi, y = media) +
  # Suavizando a linha referente ao número médio de casos.
  # o argumento `size` para definir largura da linha = 1.2 pixel
  # O argumento `se` = FALSE desabilita o intervalo de confiança
  # e o argumento `span` definindo o valor da suavização
  stat_smooth(
    aes(color = 'cor_media_casos'),
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
  # Arrumando o eixo x, definindo o intervalo de tempo que será utilizado (`breaks`)
  # uma sequência de semanas epidemiológicas de 1 a 53
  # o argumento `expand` ajuda nesse processo.
  scale_x_continuous(breaks = 1:53, expand = c(0, 0)) +
  # Definindo os títulos dos eixos x e y
  labs(x = '',
       y = '',
       title = 'Diagrama de controle') +
  # Definindo o tema do gráfico
  theme_classic() +
  # Criando a legenda das linhas
  scale_color_manual(
    name = "",
    values = c('cor_media_casos' = 'darkblue', 'cor_limite' = 'red'),
    labels = c("Média suavizada de casos confirmados", "Limite superior suavizado")
  )
#visualizando o gráfico criado
grafico_base_suavizado

# Gráfico criado anteriormente
grafico_base_suavizado +
  # Adicionando (+) uma geometria de colunas utilizando a base de dados
  # {`sinan_hep_sp_cont_fev17`}
  geom_col(data = sinan_hep_sp_cont_fev17,
           # O eixo y é definido como a frequência de casos (n).
           # As barras serão preenchidas com valor textual para definição da legenda.
           aes(y = n, fill = 'cor_n_casos'), alpha = 0.4) +
  # Definindo a legenda das barras, convertendo o valor textual em um nome de cor
  # e definindo o rótulo
  scale_fill_manual(
    name = "",
    values = c('cor_n_casos' = 'deepskyblue'),
    labels = "Número de casos até fev/2017"
  )

# Importando o banco de dados {`sinan_hep_sp_mar_2017.dbf`} para o `R`
sinan_hep_sp_mar_2017 <- read.dbf("Dados/sinan_hep_sp_mar_2017.dbf", as.is = TRUE)

# Criando o objeto {`sinan_hep_sp_cont_mar17`} e realizando a contagem dos casos segundo a
# semana epidemiológica e ano dos primeiros sintomas
sinan_hep_sp_cont_mar17 <- sinan_hep_sp_mar_2017 |>
  mutate(
    sem_epi = epiweek(DT_SIN_PRI),
    ano = year(DT_SIN_PRI)
  ) |>
  count(ano, sem_epi)

# Gráfico criado anteriormente
grafico_base_suavizado +
  # Adicionando uma geometria de colunas utilizando a base de dados
  # {`sinan_hep_sp_cont_mar17`}
  geom_col(data = sinan_hep_sp_cont_mar17,
           aes(y = n, fill = 'cor_n_casos'), alpha = 0.4) +
  # Definindo a legenda das barras, convertendo o valor textual em um nome de cor
  # e definindo o rótulo
  scale_fill_manual(
    name = "",
    values = c('cor_n_casos' = 'deepskyblue'),
    labels = "Número de casos até março/2017"
  )

# Importando o banco de dados {`sinan_hep_sp_dez_2017.dbf`} para o `R`
sinan_hep_sp_dez_2017<- read.dbf("Dados/sinan_hep_sp_dez_2017.dbf", as.is = TRUE)
sinan_hep_sp_cont_17 <- sinan_hep_sp_dez_2017 |>
  mutate(
    sem_epi = epiweek(DT_SIN_PRI),
    ano = year(DT_SIN_PRI)
  ) |>
  count(ano, sem_epi)
grafico_base_suavizado +
  geom_col(data = sinan_hep_sp_cont_17,
           aes(y = n, fill = 'cor_n_casos'), alpha = 0.4) +
  scale_fill_manual(
    name = "",
    values = c('cor_n_casos' = 'deepskyblue'),
    labels = "Número de casos até dez/2017"
  ) +
  ggtitle("Diagrama de controle de Hepatite A em São Paulo/SP entre 2006-2017")
