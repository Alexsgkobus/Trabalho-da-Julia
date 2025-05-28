## Análises 1896-2022

# Limpa console
cat("\014")

# Bibliotecas
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(stringr)
library(tidyr)
library(scales)

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
print(games_final$edition)

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
  select(-edition.x, -athlete, -country_noc.x, - country_noc.y)

# Com quantos atletas fiquei nessa base? 155867
n_distinct(bio_event_games$athlete_id)

# Filtrando para criar uma base final com os 151694 atletas de bio_final
bio_event_games_f <- bio_event_games %>%
  filter(athlete_id %in% bio_final$athlete_id)
print(bio_event_games_f$edition.y) # edition agora é edition.y

n_distinct(bio_event_games_f$athlete_id)  #151694 atletas!

#------------------------------------------------------------------------------#

# Idade dos atletas na olimpíada em que participaram


# Born externo à base para cálculo da idade
born_raw <- bio_event_games_f$born
sum(is.na(born_raw)) # 0 NA

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

# Calcula idade em anos completos para quem tem data completa
bio_event_games_f$age[has_full_born_date] <- 
  floor(as.numeric(difftime(
    converted_full_start_date[has_full_born_date], 
    converted_born, 
    units = "days")) / 365.25)

sum(is.na(bio_event_games_f$age))  # 4031 NA restantes, bate com a quantidade de
                                   # datas de nascimento incompletas - linha 135


# Identifica quem tem data de nascimento incompleta
# Quem NÃO tem data de nascimento completa
incomplete_born <- !has_full_born_date

# Subconjunto das datas incompletas
born_incomplete_raw <- bio_event_games_f$born[incomplete_born]

# Extrai todos os anos 
all_years_found <- str_extract_all(born_incomplete_raw, "[0-9]{4}")

# Pega o menor ano encontrado em cada string (ou NA se nenhum)
extract_min_year <- function(years) {
  if (length(years) == 0) return(NA_integer_)
  return(min(as.numeric(years), na.rm = TRUE))
}
born_year <- sapply(all_years_found, extract_min_year)

# Extrai ano da Olimpíada correspondente
olympic_year <- as.numeric(format(converted_full_start_date[incomplete_born], "%Y"))

# Calcula a idade aproximada
ages_incomplete <- olympic_year - born_year

# Atribui de volta à base
bio_event_games_f$age[incomplete_born] <- ages_incomplete

sum(is.na(bio_event_games_f$age)) # 0 NA -> todas as idades foram calculadas

#------------------------------------------------------------------------------#

# Verificação de ID's 
bio_event_games_f %>%
  group_by(athlete_id) %>%
  summarize(nomes_distintos = n_distinct(name)) %>%
  filter(nomes_distintos > 1) -> atletas_com_multiplos_nomes
nrow(atletas_com_multiplos_nomes) # 0 atletas têm mais de 1 nome associado 
                                  # ao mesmo id

# 10 atletas (2 a 2) tem mesmo nome, data de nascimento e país de origem, mas em 
# algum momento id's diferentes:
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

# Identifica os 10 atletas (nome, born, country) com múltiplos IDs
atletas_mult_ids <- bio_event_games_f %>%
  group_by(name, born, country) %>%
  summarize(
    ids_distintos = n_distinct(athlete_id),
    .groups = "drop"
  ) %>%
  filter(ids_distintos > 1)

# Filtra essas combinações e detalha por ID
bio_event_games_f %>%
  semi_join(atletas_mult_ids, by = c("name", "born", "country")) %>%
  group_by(name, born, country, athlete_id) %>%
  summarize(
    esporte = paste(sort(unique(sport)), collapse = "; "),
    evento = paste(sort(unique(event)), collapse = "; "),
    ano_olímpico = paste(sort(unique(year)), collapse = "; "),
    posição = paste(sort(unique(pos)), collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(name, born, country, athlete_id) %>%
  View()
# Pesquisando na internet, dos 10 atletas, 4 não consegui identificar se são a 
# mesma pessoa ou não, então vou mantê-los

# Testando se um mesmo atleta que competiu em mais de uma olimpíada tem o mesmo
# id nas diferentes edições

# Filtra registros com nome "Michael Jordan"
jordan_data <- bio_event_games_f[bio_event_games_f$name == "Michael Jordan", ]
# Exibe os anos e os ids encontrados
unique(jordan_data[, c("year", "athlete_id")])

#        year athlete_id
# 156416 1984       6342
# 156417 1992       6342 -> mesmo id

#------------------------------------------------------------------------------#

# Resultados

# Cores: lighblue/blue edições de inverno, pink/darkred edições de verão, 
#        lightgreen/green geral.

# Sumário da idade:
summary(bio_event_games_f$age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 10.00   21.00   25.00   25.62   28.00   97.00 

# Média, mínimo e máximo de idade por olimpíada
age_per_edition <- 
bio_event_games_f %>%
  group_by(edition.y) %>%
  summarise(
    media_idade = mean(age, na.rm = TRUE),
    idade_minima = min(age, na.rm = TRUE),
    idade_maxima = max(age, na.rm = TRUE),
    n = sum(!is.na(age))
  ) %>%
  arrange(edition.y)
print(age_per_edition, n = Inf)

# A tibble: 55 × 5
# edition.y                 media_idade idade_minima idade_maxima   n
#  <chr>                         <dbl>        <dbl>        <dbl> <int>
#  1 1896 Summer Olympics        23.6           10           41   449
#  2 1900 Summer Olympics        28.4           13           71  2582
#  3 1904 Summer Olympics        25.1           12           71  2449
#  4 1906 Intercalated           24             24           24     7
#  5 1908 Summer Olympics        26.6           14           62  3680
#  6 1912 Summer Olympics        26.7           13           67  5344
#  7 1920 Summer Olympics        29.2           13           72  4296
#  8 1924 Summer Olympics        28.1           13           75  6188
#  9 1924 Winter Olympics        28.4           11           64   582
#  10 1928 Summer Olympics       28.2           11           97  5418


# Gráfico de linha Média de Idade nos Jogos Olímpicos (de 4 em 4 anos)
age_per_edition_mean <- bio_event_games_f %>%
  group_by(year) %>%
  summarise(
    media_idade = mean(age, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(age_per_edition_mean, aes(x = year, y = media_idade, group = 1)) +
  geom_line(color = "lightgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 2) +
  labs(
    title = "Média de Idade nos Jogos Olímpicos",
    x = "Edição",
    y = "Média de Idade"
  ) +
  scale_x_continuous(breaks = seq(min(age_per_edition_mean$year), max(age_per_edition_mean$year), by = 4)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Gráfico de linha Média de Idade por Edição dos Jogos Olímpicos de Verão
age_per_edition_mean_s <- bio_event_games_f %>%
  filter(str_detect(edition.y, "Summer")) %>%
  group_by(year) %>%
  summarise(
    media_idade = mean(age, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(age_per_edition_mean_s, aes(x = year, y = media_idade, group = 1)) +
  geom_line(color = "pink", linewidth = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(
    title = "Média de Idade por Edição dos Jogos Olímpicos de Verão",
    x = "Edição",
    y = "Média de Idade"
  ) +
  scale_x_continuous(breaks = age_per_edition_mean_s$year) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Gráfico de linha - Média de Idade por Edição dos Jogos Olímpicos de Inverno
age_per_edition_mean_w <- bio_event_games_f %>%
  filter(str_detect(edition.y, "Winter")) %>%
  group_by(year) %>%
  summarise(
    media_idade = mean(age, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(age_per_edition_mean_w, aes(x = year, y = media_idade, group = 1)) +
  geom_line(color = "lightblue", linewidth = 1) +
  geom_point(color = "darkblue", size = 2) +
  labs(
    title = "Média de Idade por Edição dos Jogos Olímpicos de Inverno",
    x = "Edição",
    y = "Média de Idade"
  ) +
  scale_x_continuous(breaks = age_per_edition_mean_w$year) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Gráfico de linha Mediana de Idade dos Jogos Olímpicos
age_per_edition_median <- bio_event_games_f %>%
  group_by(year) %>%
  summarise(
    mediana_idade = median(age, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(age_per_edition_median, aes(x = year, y = mediana_idade, group = 1)) +
  geom_line(color = "lightgreen", size = 1) +
  geom_point(color = "darkgreen", size = 2) +
  labs(
    title = "Mediana de Idade nos Jogos Olímpicos",
    x = "Edição",
    y = "Média de Idade"
  ) +
  scale_x_continuous(breaks = seq(min(age_per_edition_median$year), max(age_per_edition_median$year), by = 4)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Boxplot "Distribuição da Idade por Edição dos Jogos Olímpicos de Verão"
bio_event_games_f %>%
  filter(str_detect(edition.y, "Summer")) %>%
  ggplot(aes(x = as.factor(year), y = age)) +
  geom_boxplot(fill = "pink", color = "darkred") +
  labs(
    title = "Distribuição da Idade nos Jogos Olímpicos de Verão",
    x = "Ano",
    y = "Idade"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Boxplot "Distribuição da Idade por Edição dos Jogos Olímpicos de Inverno"
bio_event_games_f %>%
  filter(str_detect(edition.y, "Winter")) %>%
  ggplot(aes(x = as.factor(year), y = age)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(
    title = "Distribuição da Idade nos Jogos Olímpicos de Inverno",
    x = "Ano",
    y = "Idade"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Média, mínimo e máximo de idade por esporte
per_sport <- 
  bio_event_games_f %>%
  group_by(sport) %>%
  summarise(
    media_idade = mean(age, na.rm = TRUE),
    idade_minima = min(age, na.rm = TRUE),
    idade_maxima = max(age, na.rm = TRUE),
    n = sum(!is.na(age))
  ) %>%
  arrange(sport)
print(per_sport, n = Inf)

# A tibble: 107 × 5
# sport                         media_idade idade_minima idade_maxima    n
#  <chr>                              <dbl>        <dbl>        <dbl> <int>
#  1 3x3 Basketball                   28.3           19           41    64
#  2 Aeronautics                      26             26           26     1
#  3 Alpine Skiing                    23.5           14           55 10441
#  4 Alpinism                         38.8           22           57    56
#  5 American Football                21.9           16           31   166
#  6 Archery                          28.0           14           71  2555
#  7 Art Competitions                 45.5           14           97  3471
#  8 Artistic Gymnastics              22.8           10           49 27271
#  9 Artistic Swimming                22.5           15           40  1036
#  10 Athletics                       25.1           12           70 46898


# Gráfico de barras Média e Mínimo de Idade por Esporte
age_per_sport <- bio_event_games_f %>%
  group_by(sport) %>%
  summarise(
    media_idade_esporte = mean(age, na.rm = TRUE),
    minimo_idade_esporte = min(age),
    .groups = "drop"
  )

ggplot(age_per_sport, aes(x = sport, y = media_idade_esporte, group = 1)) +
  geom_line(color = "lightgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 2) +
  labs(
    title = "Média de Idade por Esporte",
    x = "Esporte",
    y = "Média de Idade"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(age_per_sport, aes(x = sport, y = minimo_idade_esporte, group = 1)) +
  geom_line(color = "lightgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 2) +
  labs(
    title = "Mínimo de Idade por Esporte",
    x = "Esporte",
    y = "Média de Idade"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Boxplot "Distribuição da Idade por Esporte"
ggplot(bio_event_games_f, aes(x = sport, y = age)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(
    title = "Distribuição da Idade por Esporte",
    x = "Espote",
    y = "Idade"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Top 15 Esportes com os Menores Mínimos de Idade Registradas
top15_min_age_sports <- bio_event_games_f %>%
  group_by(sport) %>%
  summarise(
    idade_minima = min(age, na.rm = TRUE),
    media_idade = mean(age, na.rm = TRUE),
    n = sum(!is.na(age)),
    .groups = "drop"
  ) %>%
  arrange(idade_minima) %>%
  slice_head(n = 15)
print(top15_min_age_sports)

ggplot(top15_min_age_sports, aes(x = reorder(sport, idade_minima), y = idade_minima)) +
  geom_col(fill = "lightgreen") +
  geom_text(aes(label = idade_minima), vjust = -0.5, size = 3) +
  labs(
    title = "Top 15 Esportes com Menores Idades Mínimas",
    x = "Esporte",
    y = "Idade Mínima"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Top 15 Esportes com Maior Quantidade de Menores de Idade - Acumulada 
top15_sports_minors <- bio_event_games_f %>%
  filter(age < 18) %>%
  group_by(sport) %>%
  summarise(
    qtd_menores = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(qtd_menores)) %>%
  slice_head(n = 15)
print(top15_sports_minors)

ggplot(top15_sports_minors, aes(x = reorder(sport, qtd_menores), y = qtd_menores)) +
  geom_col(fill = "lightgreen") +
  geom_text(aes(label = qtd_menores), hjust = -0.1, size = 3) +
  labs(
    title = "Top 15 Esportes com Maior Quantidade de Menores de Idade",
    x = "Esporte",
    y = "Quantidade de Menores"
  ) +
  coord_flip() +  
  theme_minimal()

# Top 15 Esportes com Menor Quantidade de Menores de Idade - Acumulada 
top15_sports_minors_lowest <- bio_event_games_f %>%
  filter(age < 18) %>%
  group_by(sport) %>%
  summarise(
    qtd_menores = n(),
    .groups = "drop"
  ) %>%
  arrange(qtd_menores) %>%  # ordem crescente
  slice_head(n = 15)

print(top15_sports_minors_lowest)

ggplot(top15_sports_minors_lowest, aes(x = reorder(sport, qtd_menores), y = qtd_menores)) +
  geom_col(fill = "lightgreen") +
  geom_text(aes(label = qtd_menores), hjust = -0.1, size = 3) +
  labs(
    title = "Top 15 Esportes com Menor Quantidade de Menores de Idade",
    x = "Esporte",
    y = "Quantidade de Menores"
  ) +
  coord_flip() +
  theme_minimal()


# Média, mínimo e máximo de idade por sexo 
bio_event_games_f %>%
  group_by(sex) %>%
  summarise(
    media_idade = mean(age, na.rm = TRUE),
    idade_minima = min(age, na.rm = TRUE),
    idade_maxima = max(age, na.rm = TRUE),
    n = sum(!is.na(age))
  ) %>%
  arrange(sex)

# A tibble: 2 × 5
# sex    media_idade idade_minima idade_maxima      n
# <chr>        <dbl>        <dbl>        <dbl>  <int>
# 1 Female        24.0           11           74  89161
# 2 Male          26.3           10           97 221758


# Gráfico de barras Média de Idade por Sexo
age_per_sex <- bio_event_games_f %>%
  group_by(sex) %>%
  summarise(
    media_idade_sexo = mean(age, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(age_per_sex, aes(x = sex, y = media_idade_sexo, fill = sex)) +
  geom_col() +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "lightblue")) +
  labs(
    title = "Média de Idade por Sexo",
    x = "Sexo",
    y = "Média de Idade"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Boxplot "Distribuição da Idade por Sexo"
ggplot(bio_event_games_f, aes(x = sex, y = age, fill = sex)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "lightblue")) +
  labs(
    title = "Distribuição da Idade por Sexo",
    x = "Sexo",
    y = "Idade"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Quantidade de Menores de Idade por Sexo e Edição Olímpica
minors_per_edition_sex <- bio_event_games_f %>%
  filter(age < 18) %>%  
  mutate(
    edition_type = str_extract(edition.y, "Summer|Winter")  
  ) %>%
  filter(!is.na(edition_type)) %>%  
  group_by(year, sex, edition_type) %>%
  summarise(qtd_menores = n(), .groups = "drop")

# Cria vetor de todos os anos presentes no dataset original
all_years <- sort(unique(bio_event_games_f$year))

# Ajusta o data frame para year ser fator com todos os anos como níveis
minors_per_edition_sex <- minors_per_edition_sex %>%
  mutate(year = factor(year, levels = all_years))


ggplot(minors_per_edition_sex, aes(x = year, y = qtd_menores, fill = sex)) +
  geom_col(position = position_dodge(width = 0.8)) +  
  scale_fill_manual(values = c("Female" = "pink", "Male" = "lightblue")) +
  labs(
    title = "Quantidade de Menores de Idade por Sexo e Edição Olímpica",
    x = "Ano",
    y = "Quantidade de Menores",
    fill = "Sexo"
  ) +
  facet_wrap(~edition_type, scales = "free_x") + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )


# Número total de menores por Olimpíada
minors_per_edition <- bio_event_games_f %>%
  filter(age < 18) %>%
  group_by(edition.y) %>%
  summarise(qtd_menores = n(), .groups = "drop") %>%
  mutate(year = as.numeric(str_extract(edition.y, "\\d{4}"))) %>%
  arrange(year) %>%
  mutate(edition_factor = factor(edition.y, levels = unique(edition.y))) # cria fator na ordem do ano

ggplot(minors_per_edition, aes(x = edition_factor, y = qtd_menores)) +
  geom_col(fill = "lightgreen") +
  labs(
    title = "Quantidade Total de Menores de Idade por Olimpíada",
    x = "Edição (Ano)",
    y = "Quantidade de Menores"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Número total de menores por Olimpíada (ordem decrescente)
minors_per_edition_dec <- bio_event_games_f %>%
  filter(age < 18) %>%
  group_by(edition.y) %>%
  summarise(qtd_menores = n()) %>%
  arrange(desc(qtd_menores))  
print(minors_per_edition_dec) 

# edition.y              qtd_menores
# <chr>                        <int>
# 1 1972 Summer Olympics        1039
# 2 1976 Summer Olympics         940
# 3 1996 Summer Olympics         920
# 4 1988 Summer Olympics         916
# 5 1992 Summer Olympics         887
# 6 1968 Summer Olympics         853
# 7 1984 Summer Olympics         740
# 8 2000 Summer Olympics         688
# 9 1980 Summer Olympics         675
#10 2004 Summer Olympics         591

ggplot(minors_per_edition_dec, aes(x = reorder(edition.y, qtd_menores), y = qtd_menores)) +
  geom_col(fill = "lightgreen") +
  coord_flip() + 
  labs(
    title = "Quantidade de Menores de Idade por Edição Olímpica",
    x = "Edição",
    y = "Quantidade de Menores"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Proporção de menores por Olimpíada em ordem decrescente
minors_per_edition_proportion <- bio_event_games_f %>%
  group_by(edition.y) %>%
  summarise(
    total = n(),
    menores = sum(age < 18, na.rm = TRUE),
    proporcao_menores = menores / total,
    .groups = "drop"
  ) %>%
  arrange(desc(proporcao_menores))
print(minors_per_edition_proportion)

# edition.y              total menores proporcao_menores
# 1 1976 Summer Olympics  8966     940            0.105 
# 2 1972 Summer Olympics 10856    1039            0.0957
# 3 1980 Summer Olympics  7361     675            0.0917
# 4 1968 Summer Olympics  9325     853            0.0915
# 5 1984 Summer Olympics  9985     740            0.0741
# 6 1988 Summer Olympics 12746     916            0.0719
# 7 1992 Summer Olympics 13490     887            0.0658
# 8 1996 Summer Olympics 14013     920            0.0657
# 9 1976 Winter Olympics  1878     118            0.0628
#10 1988 Winter Olympics  3105     162            0.0522

minors_per_edition_proportion <- minors_per_edition_proportion %>%
  mutate(year = as.numeric(str_extract(edition.y, "\\d{4}")))


minors_per_edition_proportion %>%
  filter(str_detect(edition.y, "Summer")) %>%
  ggplot(aes(x = year, y = proporcao_menores)) +
  geom_col(fill = "pink") +
  labs(
    title = "Proporção de Menores de Idade por Edição de Verão",
    x = "Ano",
    y = "Proporção de menores"
  ) +
  scale_x_continuous(breaks = seq(min(age_per_edition_mean$year), max(age_per_edition_mean$year), by = 4)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) 

minors_per_edition_proportion %>%
  filter(str_detect(edition.y, "Winter")) %>%
  ggplot(aes(x = year, y = proporcao_menores)) +
  geom_col(fill = "pink") +
  labs(
    title = "Proporção de Menores de Idade por Edição de Inverno",
    x = "Ano",
    y = "Proporção de menores"
  ) +
  scale_x_continuous(breaks = seq(min(age_per_edition_mean$year), max(age_per_edition_mean$year), by = 4)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) 

# Número de menores por País e Edição de Verão
minors_by_country_year_s <- bio_event_games_f %>%
  filter(!is.na(age), age < 18, str_detect(edition.y, "Summer")) %>%
  group_by(year, country_noc) %>%
  summarise(qtd_menores = n(), .groups = "drop")
print(minors_by_country_year_s)

# Média de menores por Edição de Verão
mean_minors_per_sedition <- minors_by_country_year_s %>%
  group_by(year) %>%
  summarise(media_menores = mean(qtd_menores), .groups = "drop")
print(mean_minors_per_sedition)

mean_minors_per_sedition %>% filter(year == 1972) #1039 menores no verão de 1972

ggplot(mean_minors_per_sedition, aes(x = year, y = media_menores)) +
  geom_line(color = "pink", linewidth = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(
    title = "Média de Atletas Menores de Idade por Edição de Verão",
    x = "Ano",
    y = "Média de menores"
  ) +
  scale_x_continuous(breaks = unique(mean_minors_per_sedition$year)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Número de menores por País e Edição de Inverno
minors_by_country_year_w <- bio_event_games_f %>%
  filter(!is.na(age), age < 18, str_detect(edition.y, "Winter")) %>%
  group_by(year, country_noc) %>%
  summarise(qtd_menores = n(), .groups = "drop")
print(minors_by_country_year_w)

# Média de menores por Edição de Inverno
mean_minors_per_wedition <- minors_by_country_year_w %>%
  group_by(year) %>%
  summarise(media_menores = mean(qtd_menores), .groups = "drop")
print(mean_minors_per_wedition)

ggplot(mean_minors_per_wedition, aes(x = year, y = media_menores)) +
  geom_line(color = "lightblue", linewidth = 1) +
  geom_point(color = "darkblue", size = 2) +
  labs(
    title = "Média de Atletas Menores de Idade por Edição de Inverno",
    x = "Ano",
    y = "Média de menores"
  ) +
  scale_x_continuous(breaks = unique(mean_minors_per_wedition$year)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Menores de idade e medalhas/classificações
# Esportes com os menores minimos de idade