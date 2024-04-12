library(dplyr)
library(ggplot2)
library(tidytext)
library(SentimentAnalysis)
library(xlsx)
library(plotly)
library(cluster)
library(tidyverse)
library(readxl)
library(vars)
library(sentimentr)
library(forecast)
library(data.table)


all <- read_excel("C:/Users/ntanb/Desktop/MusicHistory.xlsx")
events_data <- read_excel("C:/Users/ntanb/Desktop/HistoricalEvents.xlsx", sheet = "Sheet1")

all <- all %>%
  mutate(Score = sentiment_by(Lyrics)$ave_sentiment)


all <- all %>%
  mutate(
    # Create categories for generations
    generations = case_when(
      Year <= 1964 ~ "Boomers",
      Year > 1964 & Year <= 1980 ~ "Gen X",
      Year > 1980  & Year <= 1996 ~ "Millennials",
      Year > 1996 & Year <= 2012 ~ "Gen Z",
      Year > 2012 ~ "Gen Alpha"
    ),
    # Convert to factor
    generations = factor(
      generations,
      level = c("Boomers", "Gen X", "Millennials", "Gen Z", "Gen Alpha")
    )
  ) 


# Subsets of Boomers, Gex X, Millennials, Gen Z, Gen Alpha
boomers <- subset(all, generations == "Boomers")
genx <- subset(all, generations == "Gen X")
mill <- subset(all,generations == "Millennials")
genz <- subset(all, generations == "Gen Z")
alpha <- subset(all, generations == "Gen Alpha")




events <- events_data %>% 
  mutate(
    # Create categories
    generations  = dplyr::case_when(
      Year <= 1964            ~ "Boomers",
      Year > 1964 & Year <= 1980 ~ "Gen X",
      Year > 1980  & Year <= 1996 ~ "Millennials",
      Year > 1996 & Year <= 2012 ~ "Gen Z",
      Year > 2012 ~ "Gen Alpha"
    ),
    # Convert to factor
    generations = factor(
      generations,
      level = c("Boomers", "Gen X","Millennials","Gen Z", "Gen Alpha")
    )
  )

# Subsets of Boomers, Gex X, Millennials, Gen Z, Gen Alpha
#boomers_events <- subset(all, generations == "Boomers")
#genx_events <- subset(all, generations == "Gen X")
#mill_events <- subset(all,generations == "Millennials")
#genz_events <- subset(all, generations == "Gen Z")
#alpha_events <- subset(all, generations == "Gen Alpha")




intervention_points <- c(1950,1952,1954, 1955, 1957,  1959, 1960,1961, 1962,1963)  # Example intervention points


merged_data <- inner_join(all, events, by = "Year")


merged_data$intervention <- ifelse(merged_data$Year %in% intervention_points, 1, 0)


arima_model <- auto.arima(merged_data$Score, xreg = merged_data$intervention)


summary(arima_model)
plot(fitted(arima_model), main = "Observed vs. Fitted Sentiment Scores")
plot(residuals(arima_model), main = "Residuals")







#Event, Year, and Song (Interactive visual)
boomer_data <- merged_data[merged_data$generations.x == "Boomers", ]


boomer_plot <- ggplot(data = boomer_data, aes(x = Year, y = Score, text = paste(Song, "<br>Artist: ", Artist))) +
  geom_point() +
  geom_text(aes(label = paste("")), vjust = -1, hjust = 0, size = 3) +  # Add text labels for song name and artist
  labs(x = "Year", y = "Sentiment Score", title = "Observed Sentiment Scores for Boomer Generation") +
  theme_minimal() +
  geom_point(data = boomer_data[boomer_data$intervention == 1, ], aes(color = `Short Description`), size = 3) +
  geom_vline(data = boomer_data[boomer_data$intervention == 1, ], aes(xintercept = Year), linetype = "dashed", color = "red") +
  guides(color = guide_legend(title = "Historical Events")) +
  scale_x_continuous(breaks = seq(min(boomer_data$Year), max(boomer_data$Year), by = 1)) +
  scale_y_continuous(breaks = seq(-1.1, 1.5, by = 0.2), limits = c(-1.1, 1.5))

boomer_historical_song <- ggplotly(boomer_plot, tooltip = "text")
boomer_historical_song


boomer_result <- boomers %>%
  group_by(Year) %>%
  summarise(max_sentiment = max(Score),
            min_sentiment = min(Score),
            mean_sentiment = mean(Score))


boomer_plot <- ggplot(data = boomer_result, aes(x = Year)) +
  geom_point(aes(y = max_sentiment), color = "blue", size = 2) +
  geom_point(aes(y = min_sentiment), color = "red", size = 2) +
  geom_point(aes(y = mean_sentiment), color = "gray", size = 3) +
  geom_segment(aes(y = max_sentiment, xend = Year, yend = mean_sentiment), color = "blue") +
  geom_segment(aes(y = min_sentiment, xend = Year, yend = mean_sentiment), color = "red") +
  labs(x = "Year", y = "Sentiment Score", title = "Sentiment Scores for Boomer Generation") +
  scale_x_continuous(breaks = unique(boomer_result$Year), labels = unique(boomer_result$Year)) +
  scale_y_continuous(breaks = seq(-1.1, 1.5, by = 0.5), limits = c(-1.1, 1.5)) + 
  theme_minimal()

print(boomer_plot)



