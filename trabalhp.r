library(dplyr)
library(ggplot2)
library(plotly)

# Carregar os dados da nova base
dados <- athlete_events

# Filtrar para apenas a primeira participação de cada atleta
dados_primeira_participacao <- dados %>%
  arrange(ID, Year) %>%
  group_by(ID) %>%
  slice(1) %>%
  ungroup()

# Calcular média de idade por Olimpíada
media_idade_olimpiada <- dados_primeira_participacao %>%
  filter(!is.na(Age)) %>%
  group_by(Year) %>%
  summarise(media_idade = mean(Age, na.rm = TRUE))

# Gráfico de linha interativo
grafico_linha <- ggplot(media_idade_olimpiada, aes(x = Year, y = media_idade)) +
  geom_line(color = "blue") +
  geom_point() +
  theme_minimal() +
  labs(title = "Média de Idade dos Atletas na Primeira Olimpíada",
       x = "Ano",
       y = "Média de Idade")

ggplotly(grafico_linha)

# Criar boxplot interativo
boxplot_idade <- ggplot(dados_primeira_participacao %>% filter(!is.na(Age)), aes(x = as.factor(Year), y = Age)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.colour = "red", outlier.shape = 16) +
  theme_minimal() +
  labs(title = "Distribuição das Idades dos Atletas na Primeira Olimpíada",
       x = "Ano",
       y = "Idade") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplotly(boxplot_idade)

# Contar menores de idade por Olimpíada em porcentagem
total_atletas <- dados_primeira_participacao %>%
  group_by(Year) %>%
  summarise(total = n())

menores_idade <- dados_primeira_participacao %>%
  filter(!is.na(Age) & Age < 18) %>%
  group_by(Year) %>%
  summarise(contagem = n()) %>%
  left_join(total_atletas, by = "Year") %>%
  mutate(porcentagem = (contagem / total) * 100)

# Gráfico de barras interativo em porcentagem
grafico_barras <- ggplot(menores_idade, aes(x = as.factor(Year), y = porcentagem)) +
  geom_bar(stat = "identity", fill = "orange") +
  theme_minimal() +
  labs(title = "Porcentagem de Atletas Menores de Idade na Primeira Olimpíada",
       x = "Ano",
       y = "Porcentagem (%)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplotly(grafico_barras)
