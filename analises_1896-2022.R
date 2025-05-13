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
sum(is.na(bio$born))
textual_rows_born <- grepl("[a-zA-Z]", bio$born) & !grepl("circa", bio$born, ignore.case = TRUE) & !is.na(bio$born)
sum(textual_rows_born) #149466

#------------------------------------------------------------------------------#

# Base Olympic_Athlete_Event_Results (event)
event <- read.csv("C:/Users/DeLL-E6420/Documents/Júlia/Faculdade/PesquisaPET/Olympic_Athlete_Event_Results.csv")
View(event)
sum(is.na(event$start_date))
textual_rows_start_date <- grepl("[a-zA-Z]", event$start_date) & !is.na(event$start_date)
sum(textual_rows_start_date)

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
# Idade dos atletas na olimpíada em que participaram

# Copia os campos relevantes
born_raw <- bio_event_games$born
print(born_raw)

# Trata a data de início da Olimpíada como dia, mês e ano
bio_event_games$full_start_date <- ifelse(
  grepl("[0-9]{1,2} [A-Za-z]+ [0-9]{4}", bio_event_games$start_date),
  bio_event_games$start_date,
  paste(bio_event_games$start_date, bio_event_games$year)
)
print(bio_event_games$full_start_date)

# Identifica quem tem data completa no formato "4 April 1949"
has_full_date <- grepl("^[0-9]{1,2} [A-Za-z]+ [0-9]{4}$", born_raw)
print(has_full_date)

# Inicializa a coluna idade
bio_event_games$age <- NA

# Converte datas completas
full_born <- parse_date_time(bio_event_games$born[has_full_date], orders = "d b Y", quiet = TRUE)
print(full_born)
full_start <- parse_date_time(bio_event_games$full_start_date[has_full_date], orders = "d b Y", quiet = TRUE)
print(full_start)

# Verificar se há valores NA nas datas
cat("NAs em full_born:", sum(is.na(full_born)), "\n")
cat("NAs em full_start:", sum(is.na(full_start)), "\n")

# Entradas problemáticas na conversão
failed_start_dates <- bio_event_games$start_date[has_full_date][is.na(full_start)]
cat("Entradas com erro na conversão de start_date:\n")
print(failed_start_dates)

# Cálculo da idade para datas completas
bio_event_games$age[has_full_date] <- as.integer(interval(full_born, full_start) / years(1))
cat("Total de NAs restantes em 'age':", sum(is.na(bio_event_games$age)), "\n")

# Função para extrair ano de campos parciais ou irregulares
extract_year_born <- function(born_str) {
  if (is.na(born_str) || trimws(born_str) == "") return(NA)
  
  born_str <- tolower(born_str)
  born_str <- gsub(",", "", born_str)
  born_str <- gsub("[()]", "", born_str)  # remove parênteses
  born_str <- trimws(born_str)
  
  # Termos genéricos irrelevantes
  termos_irrelevantes <- c("unknown", "desconhecido", "sem informação", "n/a", "não disponível")
  if (born_str %in% termos_irrelevantes || !grepl("\\d", born_str)) return(NA)
  
  # Caso "ou" (ex: "1922 ou 1923")
  if (grepl("ou", born_str)) {
    anos <- str_extract_all(born_str, "\\d{4}")[[1]]
    if (length(anos) >= 1) return(as.Date(paste0(anos[1], "-01-01")))
  }
  
  # Caso "circa" (ex: "circa 1910")
  if (grepl("circa", born_str)) {
    ano <- str_extract(born_str, "\\d{4}")
    if (!is.na(ano)) return(as.Date(paste0(ano, "-01-01")))
  }
  
  # Ano isolado (ex: "1922")
  if (grepl("^\\d{4}$", born_str)) {
    return(as.Date(paste0(born_str, "-01-01")))
  }
  
  # Mês e ano (ex: "March 1955")
  data_my <- suppressWarnings(parse_date_time(born_str, orders = "b Y"))
  if (!is.na(data_my)) return(as.Date(data_my))
  
  # Data completa (ex: "12 March 1955")
  data_full <- suppressWarnings(parse_date_time(born_str, orders = "d b Y"))
  if (!is.na(data_full)) return(as.Date(data_full))
  
  return(NA)
}

sum(is.na(bio_event_games$age)) #12423 NA em age
sum(!is.na(bio_event_games$age)) #304420 valores de idade em age

#------------------------------------------------------------------------------#
