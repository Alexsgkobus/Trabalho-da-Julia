## Análises 1896-2022

# Limpa console
cat("\014")

# Bibliotecas
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(stringr)

#------------------------------------------------------------------------------#

# Base Olympic_Athlete_Bio (bio)
bio <- 
read.csv("C:/Users/DeLL-E6420/Documents/Júlia/Faculdade/PesquisaPET/Olympic_Athlete_Bio.csv")

# Quantos atletas foram registrados nessa base? 155861
n_distinct(bio$athlete_id)

# Quantas datas de nascimento não tem um ano no campo? 4167
textual_rows_born <- !grepl("\\b\\d{4}\\b", bio$born) & !is.na(bio$born)
sum(textual_rows_born)

# Criando uma nova base apenas com atletas que têm pelo menos o ano em 'born'
bio_final <- bio %>%
  filter(grepl("\\b\\d{4}\\b", born), !is.na(born))

textual_rows_born <- !grepl("\\b\\d{4}\\b", bio_final$born) & !is.na(bio_final$born)
sum(textual_rows_born)

sum(is.na(bio_final$born))

n_distinct(bio_final$athlete_id)  # 151694 atletas (total) com pelo menos o ano
                                  # (exatamente 155861-4167), quantidade que 
                                  # preciso de atletas na base final

#------------------------------------------------------------------------------#

# Base Olympic_Athlete_Event_Results (event)
event <- 
read.csv("C:/Users/DeLL-E6420/Documents/Júlia/Faculdade/PesquisaPET/Olympic_Athlete_Event_Results.csv")

#------------------------------------------------------------------------------#

# Base Olympics_Games (games)
games <- 
read.csv("C:/Users/DeLL-E6420/Documents/Júlia/Faculdade/PesquisaPET/Olympics_Games.csv")
print(games$start_date)

# Nova base (games_final) com somente as olimpíadas que de fato ocorreram
games_final <- 
  subset(games, !edition %in% c("1916 Summer Olympics", "1940 Summer Olympics", 
                                "1944 Summer Olympics", "2024 Summer Olympics", 
                                "2028 Summer Olympics", "2032 Summer Olympics", 
                                "1940 Winter Olympics", "1944 Winter Olympics",
                                "2026 Winter Olympics"))

# Arrumando as datas de início das Olimpíadas
games_final <- games_final %>%
  mutate(start_date = case_when(
    year == 1900 ~ "14 May",
    year == 1904 ~ "1 July",
    year == 1908 ~ "27 April",
    year == 1912 ~ "5 May",
    year == 1920 ~ "20 April",
    edition == "1924 Summer Olympics" ~ "4 May",
    edition == "1928 Summer Olympics" ~ "17 May",
    edition == "1956 Summer Olympics" ~ "21 November",
    edition == "1964 Summer Olympics" ~ "9 October",
    edition == "1988 Summer Olympics" ~ "16 September",
    edition == "2000 Summer Olympics" ~ "14 September",
    edition == "2008 Summer Olympics" ~ "7 August",
    edition == "2020 Summer Olympics" ~ "23 July",
    edition == "1924 Winter Olympics" ~ "25 January",
    edition == "1952 Winter Olympics" ~ "14 February",
    edition == "1972 Winter Olympics" ~ "2 February",
    edition == "1998 Winter Olympics" ~ "6 February",
    edition == "2018 Winter Olympics" ~ "8 February",
    edition == "1956 Equestrian" ~ "11 June",
    TRUE ~ start_date  # mantém os valores iniciais que não sofrem modificação
  ))

# Mudando o ano (year) de 2020 para 2021 nas Olimpíadas de Tokyo
games_final$year[games_final$edition == "2020 Summer Olympics"] <- 2021

# Contando quantas edições a base fornece
n_distinct(games_final$edition_id) # 55 id's distintos de edições 
length(games_final$edition_id) # 55 id's totais de edições
 # a base tem então 55 edições olímpicas

# Cria nova coluna: data de início completa da Olimpíada (dia, mês e ano) 
games_final$full_start_date <- ifelse(
  grepl("^[0-9]{1,2} [A-Za-z]+ [0-9]{4}$", trimws(games_final$start_date)),
  trimws(games_final$start_date),
  paste(trimws(games_final$start_date), games_final$year)
)
length(games_final$full_start_date) # 55 datas completas para as 55 edições

#------------------------------------------------------------------------------#

# Base bio_event_games (junção de bio_final, event e games_final)
bio_event_games <- bio_final %>%
  full_join(event, by = "athlete_id") %>%
  full_join(games_final, by = "edition_id") #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Tirando colunas duplicadas 
bio_event_games <- bio_event_games %>%
  select(-edition.x, -edition.y, -athlete, -country_noc.x, - country_noc.y)

# Com quantos atletas fiquei nessa base? 155867
n_distinct(bio_event_games$athlete_id)

# Filtrando para criar uma base final com os 151694 atletas de bio_final
bio_event_games_f <- bio_event_games %>%
  filter(athlete_id %in% bio_final$athlete_id)

n_distinct(bio_event_games_f$athlete_id)  #151694 atletas!

#------------------------------------------------------------------------------#

# Idade dos atletas na olimpíada em que participaram


# Identifica quem tem data de nascimento completa no formato "4 April 1949"
has_full_born_date <- grepl("^[0-9]{1,2} [A-Za-z]+ [0-9]{4}$", born_raw) 
length(has_full_born_date) # 310919 datas de nascimento totais (completas e in-
                           # completas) -> mesma data de nascimento pro mesmo 
                           # atleta repetida quando participou de + de 1 evento

sum(grepl("^[0-9]{1,2} [A-Za-z]+ [0-9]{4}$", born_raw)) # 306888 datas completas

sum(!(grepl("^[0-9]{1,2} [A-Za-z]+ [0-9]{4}$", born_raw)), 
    na.rm = TRUE) + sum(is.na(born_raw)) # 4031 datas incompletas ou NA
sum(is.na(bio_event_games_f$born)) # 0 NA  
# Então há 4031 datas incompletas (somente ano(s))


# Born externo à base para cálculo da idade
born_raw <- bio_event_games_f$born
sum(is.na(born_raw)) # 0 NA

# Inicializa a coluna idade
bio_event_games_f$age <- NA

# Converte datas completas
converted_born <- as.Date(parse_date_time(bio_event_games_f$born[has_full_born_date],
                                     orders = "d b Y", quiet = TRUE))
print(converted_born) # data completa de nascimento convertida
length(converted_born) # 306888 datas completas convertidas, 
                       # bateu com a linha 132

converted_full_start_date <- as.Date(parse_date_time(bio_event_games_f$full_start_date,
                                                     orders = "d b Y", quiet = TRUE))
# data completa de início convertida acima

length(unique(converted_full_start_date)) # 55 datas de início convertidas, bate
                                          # com a linha 89
print(converted_full_start_date)
# VOLTAR PARA IDADE DEPOIS!!


# Verificação de ID's 
bio_event_games_f %>%
  group_by(athlete_id) %>%
  summarize(nomes_distintos = n_distinct(name)) %>%
  filter(nomes_distintos > 1) -> atletas_com_multiplos_nomes
nrow(atletas_com_multiplos_nomes) # 0 atletas têm mais de 1 nome associado 
                                  # ao mesmo id

# 5 atletas tem mesmo nome, data de nascimento e país de origem iguais, mas em 
# algum momento id's diferentes
bio_event_games_f %>%
  group_by(name, born, country) %>%
  summarize(
    ids_distintos = n_distinct(athlete_id),
    n = n(),  # qtde de registros no total para essa combinação
    esporte = paste(sort(unique(sport)), collapse = "; "),
    evento = paste(sort(unique(event)), collapse = "; "),
    ano_olímpico = paste(sort(unique(year)), collapse = "; "),
    .groups = "drop"
  ) %>%
  filter(ids_distintos > 1) %>%  # só combinações com múltiplos IDs
  arrange(name, born, country) %>%
  View()

# Identifica os 5 atletas (nome, born, country) com múltiplos IDs
atletas_mult_ids <- bio_event_games_f %>%
  group_by(name, born, country) %>%
  summarize(
    ids_distintos = n_distinct(athlete_id),
    .groups = "drop"
  ) %>%
  filter(ids_distintos > 1) %>%
  slice_head(n = 5)  # pegar os 5 primeiros

# Filtra essas combinações e detalha por ID
bio_event_games_f %>%
  semi_join(atletas_mult_ids, by = c("name", "born", "country")) %>%
  group_by(name, born, country, athlete_id) %>%
  summarize(
    esporte = paste(sort(unique(sport)), collapse = "; "),
    evento = paste(sort(unique(event)), collapse = "; "),
    ano_olímpico = paste(sort(unique(year)), collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(name, born, country, athlete_id) %>%
  View()
# Verificar se são a mesma pessoa!!!!!

sum(is.na(bio_event_games_f$age)) #9946 NA em age
sum(!is.na(bio_event_games$age)) #304420 valores de idade em age
#revisar age -> verificar id dos atletas que participaram em mais de uma olimpíada, se for diferente,
#filtra por id pra nao repetir a idade por evento. Se for igual, filtra por id por 
#edição pra nao repetir a idade por evento.

#------------------------------------------------------------------------------#

# Resultados
# arrumar o nome da edição para os gráficos...


# Média, mínimo e máximo de idade geral: 25.5936, 10, 98
mean(bio_event_games$age, na.rm = TRUE) 
min(bio_event_games$age, na.rm = TRUE)
max(bio_event_games$age, na.rm = TRUE)


# Média, mínimo e máximo de idade por olimpíada
per_edition <- 
bio_event_games %>%
  group_by(edition.y) %>%
  summarise(
    media_idade = if (all(is.na(age))) NA else mean(age, na.rm = TRUE),
    idade_minima = if (all(is.na(age))) NA else min(age, na.rm = TRUE),
    idade_maxima = if (all(is.na(age))) NA else max(age, na.rm = TRUE),
    n = sum(!is.na(age))
  ) %>%
  arrange(edition.y)
print(per_edition, n = Inf)

# A tibble: 64 × 5
#edition.y                 media_idade  idade_minima idade_maxima n
#<chr>                         <dbl>        <int>        <int> <int>
#1 1896 Summer Olympics        23.5           10           40   363
#2 1900 Summer Olympics        NA             NA           NA     0
#3 1904 Summer Olympics        25.0           12           71  2344
#4 1906 Intercalated           NA             NA           NA     0
#5 1908 Summer Olympics        26.7           15           62  3508
#6 1912 Summer Olympics        26.9           13           67  5195
#7 1916 Summer Olympics        NA             NA           NA     0
#8 1920 Summer Olympics        29.4           13           72  4077
#9 1924 Summer Olympics        28.2           13           75  5564
#10 1924 Winter Olympics       28.6           11           64   554


# Gráfico de linha Média de Idade por Edição dos Jogos Olímpicos
age_per_edition <- bio_event_games %>%
  group_by(edition.y) %>%
  summarise(
    media_idade = mean(age, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(age_per_edition, aes(x = edition.y, y = media_idade, group = 1)) +
  geom_line(color = "violet", size = 1) +
  geom_point(color = "darkviolet", size = 2) +
  labs(
    title = "Média de Idade por Edição dos Jogos Olímpicos",
    x = "Edição",
    y = "Média de Idade"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Boxplot "Distribuição da Idade por Edição dos Jogos Olímpicos"
ggplot(bio_event_games, aes(x = edition.y, y = age)) +
  geom_boxplot(fill = "violet", color = "darkviolet") +
  labs(
    title = "Distribuição da Idade por Edição dos Jogos Olímpicos",
    x = "Edição",
    y = "Idade"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Média, mínimo e máximo de idade por esporte
per_sport <- 
  bio_event_games %>%
  group_by(sport) %>%
  summarise(
    media_idade = if (all(is.na(age))) NA else mean(age, na.rm = TRUE),
    idade_minima = if (all(is.na(age))) NA else min(age, na.rm = TRUE),
    idade_maxima = if (all(is.na(age))) NA else max(age, na.rm = TRUE),
    n = sum(!is.na(age))
  ) %>%
  arrange(sport)
print(per_sport, n = Inf)

# A tibble: 113 × 5
#sport                         media_idade idade_minima idade_maxima n
#<chr>                           <dbl>        <int>        <int> <int>
#1 3x3 Basketball                   28.3           19           41    64
#2 Aeronautics                      26             26           26     1
#3 Alpine Skiing                    23.5           14           55 10369
#4 Alpinism                         38.8           22           57    56
#5 American Football                21.8           16           30   160
#6 Archery                          27.8           14           71  2511
#7 Art Competitions                 45.7           14           98  3333
#8 Artistic Gymnastics              22.8           10           49 26908
#9 Artistic Swimming                22.5           15           40  1036
#10 Athletics                        25.1           12           55 45562

# Gráfico de barras Média de Idade por Esporte
age_per_sport <- bio_event_games %>%
  group_by(sport) %>%
  summarise(
    media_idade_esporte = mean(age, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(age_per_sport, aes(x = sport, y = media_idade_esporte, group = 1)) +
  geom_col(fill = "salmon") +
  labs(
    title = "Média de Idade por Esporte",
    x = "Esporte",
    y = "Média de Idade"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot "Distribuição da Idade por Esporte"
ggplot(bio_event_games, aes(x = sport, y = age)) +
  geom_boxplot(fill = "lightpink", color = "salmon") +
  labs(
    title = "Distribuição da Idade por Esporte",
    x = "Espote",
    y = "Idade"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Média, mínimo e máximo de idade por sexo
per_sex <- 
  bio_event_games %>%
  group_by(sex) %>%
  summarise(
    media_idade = if (all(is.na(age))) NA else mean(age, na.rm = TRUE),
    idade_minima = if (all(is.na(age))) NA else min(age, na.rm = TRUE),
    idade_maxima = if (all(is.na(age))) NA else max(age, na.rm = TRUE),
    n = sum(!is.na(age))
  ) %>%
  arrange(sex)
print(per_sex, n = Inf)

# A tibble: 3 × 5
#sex       media_idade idade_minima idade_maxima   n
#<chr>           <dbl>        <int>        <int> <int>
#1 Female        24.0           11           74  88958
#2 Male          26.3           10           98 215462

# Gráfico de barras Média de Idade por Sexo
age_per_sex <- bio_event_games %>%
  group_by(sex) %>%
  summarise(
    media_idade_sexo = mean(age, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(age_per_sex, aes(x = sex, y = media_idade_sexo, group = 1)) +
  geom_col(fill = "blue") +
  labs(
    title = "Média de Idade por Sexo",
    x = "Sexo",
    y = "Média de Idade"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # tirar essa barra NA

# Boxplot "Distribuição da Idade por Sexo"
ggplot(bio_event_games, aes(x = sex, y = age)) +
  geom_boxplot(fill = "steelblue", color = "lightblue") +
  labs(
    title = "Distribuição da Idade por Sexo",
    x = "Sex",
    y = "Idade"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Número de menores por Olimpíada
minors_per_edition <- bio_event_games %>%
  filter(!is.na(age), age < 18) %>%
  group_by(edition.y) %>%
  summarise(qtd_menores = n()) %>%
  arrange(desc(qtd_menores))  
print(minors_per_edition) #Máximo: 1039

# Proporção de menores por Olimpíada
minors_per_edition_proportion <- bio_event_games %>%
  filter(!is.na(age)) %>%
  group_by(edition.y) %>%
  summarise(
    total = n(),
    menores = sum(age < 18),
    proporcao_menores = menores / total
  ) %>%
  arrange(desc(proporcao_menores))  
print(minors_per_edition_proportion) 

ggplot(minors_per_edition_proportion, aes(x = edition.y, y = proporcao_menores)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Proporção de menores de idade por edição olímpica",
    x = "Edição dos Jogos Olímpicos",
    y = "Proporção de menores"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

# Número de menores por País
minors_per_country <- bio_event_games %>%
  filter(!is.na(age), age < 18) %>%
  group_by(country_noc) %>%
  summarise(qtd_menores = n()) %>%
  arrange(desc(qtd_menores))  
print(minors_per_country) 

# country_noc  qtd_menores
#<chr>               <int>
#1 USA                2111
#2 CAN                1194
#3 FRG                1039
#4 KOR                1020
#5 ESP                 887
#6 MEX                 853
#7 AUS                 845
#8 JPN                 828
#9 URS                 674
#10 CHN                614

# Número de menores por País - Gráfico
minors_per_country <- bio_event_games %>%
  group_by(country_noc) %>%
  summarise(
    menores_idade_pais = sum(age, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(minors_per_country, aes(x = country_noc, y = menores_idade_pais, group = 1)) +
  geom_col(fill = "blue") +
  labs(
    title = "Número de Menores de Idade por País",
    x = "País (Sigla)",
    y = "Quantidade"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Menores de idade e medalhas/classificações