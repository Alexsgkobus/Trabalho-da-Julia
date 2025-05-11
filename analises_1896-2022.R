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
bio <- read.csv("C:/Users/DeLL-E6420/Documents/Júlia/Faculdade/PesquisaPET/Olympic_Athlete_Bio.csv")
View(bio)

#------------------------------------------------------------------------------#

# Base Olympic_Athlete_Event_Results (event)
event <- read.csv("C:/Users/DeLL-E6420/Documents/Júlia/Faculdade/PesquisaPET/Olympic_Athlete_Event_Results.csv")
View(event)

#------------------------------------------------------------------------------#

# Base Olympic_Athlete_Bio (bio) com Olympic_Athlete_Event_Results (event)
bio_event <- full_join(bio, event, by = 'athlete_id')
View(bio_event)

#------------------------------------------------------------------------------#

# Base Olympics_Games (games)
games <- read.csv("C:/Users/DeLL-E6420/Documents/Júlia/Faculdade/PesquisaPET/Olympics_Games.csv")
View(games)

#------------------------------------------------------------------------------#

# Base bio_event com Olympics_Games (games)
bio_event_games <- full_join(bio_event, games, by = 'edition_id')
View(bio_event_games)

#------------------------------------------------------------------------------#

# Idade média por Olimpíada

# Copia os campos relevantes
born_raw <- bio_event_games$born
bio_event_games$full_start_date <- ifelse(
  grepl("[0-9]{4}", bio_event_games$start_date),  # já tem o ano? (olimpiadas de Tokyo)
  bio_event_games$start_date,                    # então mantém como está
  paste(bio_event_games$start_date, bio_event_games$year)  # senão, junta com o ano
) #data de início da Olimpíada como dia, mês e ano


# Identifica quem tem data completa no formato "4 April 1949"
has_full_date <- grepl("^[0-9]{1,2} [A-Za-z]+ [0-9]{4}$", born_raw)

# Inicializa a coluna idade
bio_event_games$age <- NA

# Converte para datas completas quem tem dia/mês/ano
full_born <- parse_date_time(born_raw[has_full_date], orders = "d b Y", quiet = TRUE)
print(full_born)

# Cálculo da Idade em campos completos
bio_event_games$age[has_full_date] <- 
  as.integer(interval(full_born, bio_event_games$full_start_date[has_full_date]) / years(1))

# Verifique as datas que falham na conversão
# ISSO QUE PRECISA SER VISTO! NEM TODOS OS CAMPOS SÃO DATA COMPLETA OU SÓ ANO, TEM ALGUNS
# SÓ COM MESES, (1922 OU 1923), CIRCA 2010, CAMPOS COM TEXTOS, ETC...
failed_dates <- bio_event_games$born[is.na(parse_date_time(bio_event_games$born, orders = "d b Y", quiet = TRUE))]
print(failed_dates)

# Para os que têm só o ano
year_born <- as.numeric(born_raw[!has_full_date])
start_year <- year(start_date[!has_full_date])

# Cálculo da Idade para os casos -> (ano - ano)
bio_event_games$age[!has_full_date] <- start_year - year_born
