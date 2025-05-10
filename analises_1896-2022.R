## Análises 1896-2022
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)

# Base Olympic_Athlete_Bio (bio)
bio <- read.csv("C:/Users/DeLL-E6420/Documents/Júlia/Faculdade/PesquisaPET/Olympic_Athlete_Bio.csv")
View(bio)

# Base Olympic_Athlete_Event_Results (event)
event <- read.csv("C:/Users/DeLL-E6420/Documents/Júlia/Faculdade/PesquisaPET/Olympic_Athlete_Event_Results.csv")
View(event)

# Base Olympic_Athlete_Bio (bio) com Olympic_Athlete_Event_Results (event)
bio_event <- full_join(bio, event, by = 'athlete_id')
View(bio_event)

# Base Olympics_Games (games)
games <- read.csv("C:/Users/DeLL-E6420/Documents/Júlia/Faculdade/PesquisaPET/Olympics_Games.csv")
View(games)

# Base bio_event com Olympics_Games (games)
bio_event_games <- full_join(bio_event, games, by = 'edition_id')
View(bio_event_games)

# Idade média por Olimpíada
# Data inicial completa da Olimpíada (day month year) - Nova coluna (full_start_date) unindo games$start_date e games$year
bio_event_games$full_start_date <- paste(bio_event_games$start_date, bio_event_games$year)

# Convertendo data de nascimento (bio_event_games$born) e data inicial completa da olimpíada (bio_event_games$full_start_date) para data 
# RETORNOU SOMENTE NA
bio_event_games$born <- dmy(bio_event_games$born)
View(bio_event_games)

falhas <- bio_event_games$born[is.na(dmy(bio_event_games$born))]
unique(falhas)

# Remove espaços extras, quebras de linha, etc.
bio_event_games$born_date <- gsub("\\s+", " ", bio_event_games$born)   # normaliza espaços internos
bio_event_games$born <- trimws(bio_event_games$born)              # remove espaços nas bordas
View(bio_event_games)




