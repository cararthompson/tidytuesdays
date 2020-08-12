# Avatar
library(tidyverse)
library(tvthemes)
library(tidytuesdayR)
library(ggforce)


# Get the data ####
# tt_data <- tt_load(2020, 33)
# readme(tt_data)

# Get the data ready for plotting ####
avatar <- tt_data$avatar %>%
  mutate(character_word_count = sapply(strsplit(character_words, " "), length)) %>%
  group_by(book, chapter) %>%
  summarise(total_chat = sum(character_word_count), 
            chapter = unique(chapter),
            chapter_num = unique(chapter_num), 
            imdb = unique(imdb_rating))


# Plot it! ####

sweetspot <- ggplot(avatar, aes(x = chapter_num, y = total_chat, fill = imdb)) +
  geom_bar(stat = "identity") +
#  theme("avatar") + 
  labs(title = "Avatar: the last airbender - The anti sweet spot of chattiness") +
  facet_grid(book~.)


fire <- ggplot(avatar, aes(x = chapter_num, y = total_chat, colour = imdb)) +
  geom_bar(stat = "identity") +
  theme("avatar") + 
  labs(title = "Avatar: the last airbender - The anti-sweetspot of chattiness") +
  facet_grid(book~.)


# Export to create making-of gif 
ggsave(filename = file.path("../making-of/temp", paste0("202008-avatar", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
       dpi = 400, width = 10, height = 10)

# Export final plot
ggsave(filename = "../plots/202008_avatar.png", dpi = 400, width = 10, height = 10, type = cairo)
