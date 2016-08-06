library(dplyr)
library(stringr)
library(jsonlite)
library(tidytext)
library(tibble)
library(ggplot2)

# Each line is a JSON object- the fastest way to process is to combine into a
# single JSON string and use fromJSON and flatten
setwd('R_expert/sentiment/')
file_review = 'yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_review.json'
reviews_combined <- str_c("[", str_c(readLines(file_review), collapse = ", "), "]")

reviews <- fromJSON(reviews_combined) %>%
  flatten() %>%
  tbl_df()

dim(reviews)
names(reviews)
head(reviews)

review_words <- reviews %>%
  select(review_id, business_id, stars, text) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

head(review_words)

AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)

AFINN

reviews_sentiment <- review_words %>%
  inner_join(AFINN, by = "word") %>%
  group_by(review_id, stars) %>%
  summarize(sentiment = mean(afinn_score))

head(reviews_sentiment)

theme_set(theme_bw())
ggplot(reviews_sentiment, aes(stars, sentiment, group = stars)) +
  geom_boxplot() +
  ylab("Average sentiment score")

review_words_counted <- review_words %>%
  count(review_id, business_id, stars, word) %>%
  ungroup()

head(review_words_counted)

word_summaries <- review_words_counted %>%
  group_by(word) %>%
  summarize(businesses = n_distinct(business_id),
            reviews = n(),
            uses = sum(n),
            average_stars = mean(stars)) %>%
  ungroup()

head(word_summaries)

word_summaries_filtered <- word_summaries %>%
  filter(reviews >= 200, businesses >= 10)

word_summaries_filtered

word_summaries_filtered %>%
  arrange(desc(average_stars))

word_summaries_filtered %>%
  arrange(average_stars)

ggplot(word_summaries_filtered, aes(reviews, average_stars)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  geom_hline(yintercept = mean(reviews$stars), color = "red", lty = 2) +
  xlab("# of reviews") +
  ylab("Average Stars")

words_afinn <- word_summaries_filtered %>%
  inner_join(AFINN)

ggplot(words_afinn, aes(afinn_score, average_stars, group = afinn_score)) +
  geom_boxplot() +
  xlab("AFINN score of word") +
  ylab("Average stars of reviews with this word")

dim(words_afinn)
head(words_afinn)
