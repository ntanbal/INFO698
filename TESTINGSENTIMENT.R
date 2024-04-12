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


all <- read_excel("C:/Users/ntanb/Desktop/MusicHistory.xlsx")


all <- all %>% 
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
boomers <- subset(all, generations == "Boomers")
genx <- subset(all, generations == "Gen X")
mill <- subset(all,generations == "Millennials")
genz <- subset(all, generations == "Gen Z")
alpha <- subset(all, generations == "Gen Alpha")



# Genre Count of Boomers
boomers_genre_counts <- table(boomers$Genre)
boomers_genre_counts_df <- as.data.frame(boomers_genre_counts)
colnames(boomers_genre_counts_df) <- c("Genre", "Count")
print(boomers_genre_counts_df)

# Genre Count of Gen X
genx_genre_counts <- table(genx$Genre)
genx_genre_counts_df <- as.data.frame(genx_genre_counts)
colnames(genx_genre_counts_df) <- c("Genre", "Count")
print(genx_genre_counts_df)

# Genre Count of Millennials
mill_genre_counts <- table(mill$Genre)
mill_genre_counts_df <- as.data.frame(mill_genre_counts)
colnames(mill_genre_counts_df) <- c("Genre", "Count")
print(mill_genre_counts_df)

# Genre Count of Gen Z
genz_genre_counts <- table(genz$Genre)
genz_genre_counts_df <- as.data.frame(genz_genre_counts)
colnames(genz_genre_counts_df) <- c("Genre", "Count")
print(genz_genre_counts_df)

# Genre Count of Gen X
genx_genre_counts <- table(genx$Genre)
genx_genre_counts_df <- as.data.frame(genx_genre_counts)
colnames(genx_genre_counts_df) <- c("Genre", "Count")
print(genx_genre_counts_df)

# Genre Count of Gen X
alpha_genre_counts <- table(alpha$Genre)
alpha_genre_counts_df <- as.data.frame(alpha_genre_counts)
colnames(alpha_genre_counts_df) <- c("Genre", "Count")
print(alpha_genre_counts_df)



# SENTIMENT ANALYSIS ON BOOMERS
nrc <- get_sentiments("nrc")

# Tokenize the lyrics column
boomer_words <- boomers %>%
  unnest_tokens(word, Lyrics) %>%
  filter(nchar(word) >= 2)

# Remove common English stopwords
boomers_words <- boomer_words %>%
  anti_join(stop_words)

boomer_word_freq <- boomers_words %>%
  count(word, sort = TRUE)

boomer_word_table <- inner_join(boomer_word_freq, nrc)

boomer_word_table$n <- as.numeric(boomer_word_table$n)

colnames(boomer_word_table)[2] = "Frequency"

#SENTIMENT ANALYSIS ON GEN X
genx_words <- genx %>%
  unnest_tokens(word, Lyrics) %>%
  filter(nchar(word) >= 2)

# Remove common English stopwords
genx_words <- genx_words %>%
  anti_join(stop_words)

genx_word_freq <- genx_words %>%
  count(word, sort = TRUE)

genx_word_table <- inner_join(genx_word_freq, nrc)

genx_word_table$n <- as.numeric(genx_word_table$n)

colnames(genx_word_table)[2] = "Frequency"


#SENTIMENT ANALYSIS ON MILL
mill_words <- mill %>%
  unnest_tokens(word, Lyrics) %>%
  filter(nchar(word) >= 2)

# Remove common English stopwords
mill_words <- mill_words %>%
  anti_join(stop_words)

mill_word_freq <- mill_words %>%
  count(word, sort = TRUE)

mill_word_table <- inner_join(mill_word_freq, nrc)

mill_word_table$n <- as.numeric(mill_word_table$n)

colnames(mill_word_table)[2] = "Frequency"

#SENTIMENT ANALYSIS ON GEN Z
genz_words <- genz %>%
  unnest_tokens(word, Lyrics) %>%
  filter(nchar(word) >= 2)

# Remove common English stopwords
genz_words <- genz_words %>%
  anti_join(stop_words)

genz_word_freq <- genz_words %>%
  count(word, sort = TRUE)

genz_word_table <- inner_join(genz_word_freq, nrc)

genz_word_table$n <- as.numeric(genz_word_table$n)

colnames(genz_word_table)[2] = "Frequency"

#SENTIMENT ANALYSIS ON GEN ALPHA

alpha_words <- alpha %>%
  unnest_tokens(word, Lyrics) %>%
  filter(nchar(word) >= 2)

# Remove common English stopwords
alpha_words <- alpha_words %>%
  anti_join(stop_words)

alpha_word_freq <- alpha_words %>%
  count(word, sort = TRUE)

alpha_word_table <- inner_join(alpha_word_freq, nrc)

alpha_word_table$n <- as.numeric(alpha_word_table$n)

colnames(alpha_word_table)[2] = "Frequency"

# Plot sentiment analysis for Boomers
ggplot(head(boomer_word_table, 10), aes(x = word, y = Frequency, fill = sentiment)) +
  geom_col(position = "dodge") +
  labs(title = "Top 10 Words for Boomers", x = "Word", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plot sentiment analysis for Gen X
ggplot(head(genx_word_table, 10), aes(x = word, y = Frequency, fill = sentiment)) +
  geom_col(position = "dodge") +
  labs(title = "Top 10 Words for Boomers", x = "Word", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plot sentiment analysis for MILL
ggplot(head(mill_word_table, 10), aes(x = word, y = Frequency, fill = sentiment)) +
  geom_col(position = "dodge") +
  labs(title = "Top 10 Words for Boomers", x = "Word", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plot sentiment analysis for Gen Z
ggplot(head(genz_word_table, 10), aes(x = word, y = Frequency, fill = sentiment)) +
  geom_col(position = "dodge") +
  labs(title = "Top 10 Words for Boomers", x = "Word", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plot sentiment analysis for Gen ALPHA
ggplot(head(alpha_word_table, 10), aes(x = word, y = Frequency, fill = sentiment)) +
  geom_col(position = "dodge") +
  labs(title = "Top 10 Words for Boomers", x = "Word", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#SENTIMENT ANALYSIS FOR CLUSTERING
#BOOMERS            
nrc <- get_sentiments("nrc")

boomers_words <- boomers %>%
  unnest_tokens(word, Lyrics) %>%
  filter(nchar(word) >= 2) %>%
  anti_join(stop_words)

boomers_sentiment <- boomers_words %>%
  group_by(Song) %>%
  inner_join(nrc) %>%
  count(Song, Artist, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  ungroup()


boomers_sentiment <- boomers_sentiment %>%
  mutate(across(positive:negative, ~ . / sum(.)))


set.seed(123) # for reproducibility
boomers_cluster <- kmeans(boomers_sentiment[, -c(1:2)], centers = 3)

boomers_sentiment <- cbind(boomers_sentiment, cluster = as.factor(boomers_cluster$cluster))

# maybe scale the x and y to the same values ?
max_sentiment <- max(max(boomers_sentiment$positive), max(boomers_sentiment$negative))

#make the clustrs with the plot
boomers_plot <- ggplot(boomers_sentiment, aes(x = positive, y = negative, color = cluster, 
                                              text = paste("Song:", Song, "<br>Artist:", Artist))) +
  geom_point() +
  labs(title = "Sentiment Analysis of Boomers Lyrics", x = "Positive Sentiment", y = "Negative Sentiment") +
  theme_minimal() +
  coord_fixed(xlim = c(0, max_sentiment), ylim = c(0, max_sentiment)) # Set same scale for x and y axes


boomers_plotly <- ggplotly(boomers_plot, tooltip = "text")

#print to see plot
boomers_plotly

#GEN X CLUSTER           

genx_words <- genx %>%
  unnest_tokens(word, Lyrics) %>%
  filter(nchar(word) >= 2) %>%
  anti_join(stop_words)

genx_sentiment <- genx_words %>%
  group_by(Song) %>%
  inner_join(nrc) %>%
  count(Song, Artist, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  ungroup()


genx_sentiment <- genx_sentiment %>%
  mutate(across(positive:negative, ~ . / sum(.)))


set.seed(123) # for reproducibility
genx_cluster <- kmeans(genx_sentiment[, -c(1:2)], centers = 3)

genx_sentiment <- cbind(genx_sentiment, cluster = as.factor(genx_cluster$cluster))

# maybe scale the x and y to the same values ?
max_sentiment <- max(max(genx_sentiment$positive), max(genx_sentiment$negative))

#make the clustrs with the plot
genx_plot <- ggplot(genx_sentiment, aes(x = positive, y = negative, color = cluster, 
                                              text = paste("Song:", Song, "<br>Artist:", Artist))) +
  geom_point() +
  labs(title = "Sentiment Analysis of Generation X Lyrics", x = "Positive Sentiment", y = "Negative Sentiment") +
  theme_minimal() +
  coord_fixed(xlim = c(0, max_sentiment), ylim = c(0, max_sentiment)) # Set same scale for x and y axes


genx_plotly <- ggplotly(genx_plot, tooltip = "text")

#print to see plot
genx_plotly

# MILL CLUSTER           

mill_words <- mill %>%
  unnest_tokens(word, Lyrics) %>%
  filter(nchar(word) >= 2) %>%
  anti_join(stop_words)

mill_sentiment <- mill_words %>%
  group_by(Song) %>%
  inner_join(nrc) %>%
  count(Song, Artist, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  ungroup()


mill_sentiment <- mill_sentiment %>%
  mutate(across(positive:negative, ~ . / sum(.)))


set.seed(123) # for reproducibility
mill_cluster <- kmeans(mill_sentiment[, -c(1:2)], centers = 3)

mill_sentiment <- cbind(mill_sentiment, cluster = as.factor(mill_cluster$cluster))

# maybe scale the x and y to the same values ?
max_sentiment <- max(max(mill_sentiment$positive), max(mill_sentiment$negative))

#make the clustrs with the plot
mill_plot <- ggplot(mill_sentiment, aes(x = positive, y = negative, color = cluster, 
                                        text = paste("Song:", Song, "<br>Artist:", Artist))) +
  geom_point() +
  labs(title = "Sentiment Analysis of Millennials Lyrics", x = "Positive Sentiment", y = "Negative Sentiment") +
  theme_minimal() +
  coord_fixed(xlim = c(0, max_sentiment), ylim = c(0, max_sentiment)) # Set same scale for x and y axes


mill_plotly <- ggplotly(mill_plot, tooltip = "text")

#print to see plot
mill_plotly

# GEN Z CLUSTER           

genz_words <- genz %>%
  unnest_tokens(word, Lyrics) %>%
  filter(nchar(word) >= 2) %>%
  anti_join(stop_words)

genz_sentiment <- genz_words %>%
  group_by(Song) %>%
  inner_join(nrc) %>%
  count(Song, Artist, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  ungroup()


genz_sentiment <- genz_sentiment %>%
  mutate(across(positive:negative, ~ . / sum(.)))


set.seed(123) # for reproducibility
genz_cluster <- kmeans(genz_sentiment[, -c(1:2)], centers = 3)

genz_sentiment <- cbind(genz_sentiment, cluster = as.factor(genz_cluster$cluster))

# maybe scale the x and y to the same values ?
max_sentiment <- max(max(genz_sentiment$positive), max(genz_sentiment$negative))

#make the clustrs with the plot
genz_plot <- ggplot(genz_sentiment, aes(x = positive, y = negative, color = cluster, 
                                        text = paste("Song:", Song, "<br>Artist:", Artist))) +
  geom_point() +
  labs(title = "Sentiment Analysis of Generation Z Lyrics", x = "Positive Sentiment", y = "Negative Sentiment") +
  theme_minimal() +
  coord_fixed(xlim = c(0, max_sentiment), ylim = c(0, max_sentiment)) # Set same scale for x and y axes


genz_plotly <- ggplotly(genz_plot, tooltip = "text")

#print to see plot
genz_plotly

# GEN ALPHA CLUSTER           

alpha_words <- alpha %>%
  unnest_tokens(word, Lyrics) %>%
  filter(nchar(word) >= 2) %>%
  anti_join(stop_words)

alpha_sentiment <- alpha_words %>%
  group_by(Song) %>%
  inner_join(nrc) %>%
  count(Song, Artist, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  ungroup()


alpha_sentiment <- alpha_sentiment %>%
  mutate(across(positive:negative, ~ . / sum(.)))


set.seed(123) # for reproducibility
alpha_cluster <- kmeans(alpha_sentiment[, -c(1:2)], centers = 3)

alpha_sentiment <- cbind(alpha_sentiment, cluster = as.factor(alpha_cluster$cluster))

# maybe scale the x and y to the same values ?
max_sentiment <- max(max(alpha_sentiment$positive), max(alpha_sentiment$negative))

#make the clustrs with the plot
alpha_plot <- ggplot(alpha_sentiment, aes(x = positive, y = negative, color = cluster, 
                                        text = paste("Song:", Song, "<br>Artist:", Artist))) +
  geom_point() +
  labs(title = "Sentiment Analysis of Generation Alpha Lyrics", x = "Positive Sentiment", y = "Negative Sentiment") +
  theme_minimal() +
  coord_fixed(xlim = c(0, max_sentiment), ylim = c(0, max_sentiment)) # Set same scale for x and y axes


alpha_plotly <- ggplotly(alpha_plot, tooltip = "text")

#print to see plot
alpha_plotly

#
events_data <- read_excel("C:/Users/ntanb/Desktop/HistoricalEvents.xlsx", sheet = "Sheet1")


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
boomers_events <- subset(all, generations == "Boomers")
genx_events <- subset(all, generations == "Gen X")
mill_events <- subset(all,generations == "Millennials")
genz_events <- subset(all, generations == "Gen Z")
alpha_events <- subset(all, generations == "Gen Alpha")






















