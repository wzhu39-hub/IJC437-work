library(tidyverse)
library(janitor)
billboard_first <- read_csv("billboard_24years_lyrics_spotify.csv 01-09-02-102.csv")
head(billboard_first)
glimpse(billboard_first)
hits <- billboard_first %>% 
clean_names() %>% 
filter(year >= 2000, year <= 2023)
hits <- hits %>% 
mutate(is_top10 = if_else(ranking <= 10, 1, 0))
colnames(hits)
hits_small <- hits %>% 
  select(
    song, band_singer, year, ranking, is_top10,
    valence, energy, loudness, danceability,
    acousticness, tempo, duration_ms,
    speechiness, instrumentalness, liveness)
glimpse(hits_small)
#RQ1
#mean-data
yearly_stats <- hits_small %>% 
  group_by(year) %>% 
  summarise(
    mean_valence      = mean(valence, na.rm = TRUE),
    mean_energy       = mean(energy, na.rm = TRUE),
    mean_loudness     = mean(loudness, na.rm = TRUE),
    mean_danceability = mean(danceability, na.rm = TRUE),
    mean_acousticness = mean(acousticness, na.rm = TRUE),
    mean_tempo        = mean(tempo, na.rm = TRUE),
    mean_duration_ms  = mean(duration_ms, na.rm = TRUE))
head(yearly_stats)
#Time trend
ggplot(yearly_stats, aes(x = year, y = mean_energy)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Yearly mean energy (2000–2023)",
    x = "Year",
    y = "Mean energy")
#loundness
ggplot(yearly_stats, aes(x = year, y = mean_loudness)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Yearly mean loudness (2000–2023)",
    x = "Year",
    y = "Mean loudness (dB)")
#RQ2
#Data Framework
model_data <- hits_small %>% 
  select(is_top10, ranking, year,
         valence, energy, loudness, danceability,
         acousticness, tempo, duration_ms) %>% 
  drop_na()
#70% 30%
set.seed(250) 
n_row <- nrow(model_data)
train_id <- sample(1:n_row, size = floor(0.7 * n_row))

train_df <- model_data[train_id, ]
test_df  <- model_data[-train_id, ]
#Logistic regression model
logit_mod <- glm(
  is_top10 ~ valence + energy + loudness + danceability +
    acousticness + tempo + duration_ms,
  data = train_df,
  family = binomial())

hits_plot <- hits_small %>% 
  mutate(chart_group = if_else(is_top10 == 1, "Top 10", "11–100"))

ggplot(hits_plot, aes(x = chart_group, y = loudness)) +
  geom_boxplot() +
  labs(
    title = "Loudness of Top 10 vs other year-end hits",
    x = "Chart group",
    y = "Loudness (dB)")

summary(logit_mod)
#RQ3
#only audio
audio_vars <- model_data %>% 
  select(valence, energy, loudness, danceability,
         acousticness, tempo, duration_ms)
#clean
audio_scaled <- scale(audio_vars)
#PCA
pca_out <- prcomp(audio_scaled)
summary(pca_out)

pca_df <- as.data.frame(pca_out$x) %>% 
mutate(year = model_data$year)

ggplot(pca_df, aes(x = PC1, y = PC2, colour = year)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "PCA of audio features (PC1 vs PC2)",
    x = "PC1",
    y = "PC2",
    colour = "Year")
