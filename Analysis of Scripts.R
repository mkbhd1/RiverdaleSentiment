source("Get Scripts.R")

data("stop_words")

cleaned_scripts <- scriptTokens %>%
  anti_join(stop_words)

all_word_count <- cleaned_scripts %>%
  dplyr::count(word, sort = TRUE) 

names <- data.frame(names = tolower(babynames$name))

oddNames <- c("jughead", "fp", "jellybean", "lodge")

charNames <- data.frame(chararacterName = oddNames)

removed_characters_scripts <- cleaned_scripts %>% anti_join(names, by = c("word" = "names"))

removed_characters_scripts <- removed_characters_scripts %>% anti_join(charNames, by = c("word" = "chararacterName"))

no_chars_word_count <- removed_characters_scripts %>%
  dplyr::count(word, sort = TRUE) 


bing <- get_sentiments("bing")

maxWords <- removed_characters_scripts %>%
  group_by(Episode) %>%
  dplyr::summarize(Max = max(wordNumber, na.rm=TRUE))

removed_characters_scripts <-  merge(removed_characters_scripts, maxWords, by = 'Episode')

riverdaleSentiment <- removed_characters_scripts %>%
  inner_join(bing) %>%
  dplyr::count(Season, Episode,EpisodeNum, index = wordNumber / 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

riverdaleSentiment <- plyr::arrange(riverdaleSentiment, EpisodeNum)



sentimentOverEpisodeTime <- ggplot(riverdaleSentiment, aes(index, sentiment, fill = Episode)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(Season ~ EpisodeNum ~ Episode,  scales = "free")

bing_word_counts <- removed_characters_scripts %>%
  inner_join(bing) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  ungroup()


bing_word_counts %>%
  filter(n > 25) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Contribution to sentiment")