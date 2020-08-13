# Avatar
library(tidyverse)
library(tvthemes)
library(tidytuesdayR)
library(ggforce)
library(extrafont)
loadfonts()
library(ggforce)
library(ggtext)


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

theme_darkAvatar <- theme_avatar %>%
  
  
  # Plot it! ####

sweetspot <- ggplot(avatar, 
                    aes(x = total_chat, y = imdb)) +
  geom_smooth(colour = "white", alpha = .6) +
  geom_point(aes(colour = book), alpha = .8, size = 3) +
  theme_avatar(title.font = "Slayer",
               text.font = "Slayer",
               title.size = 14) +
  scale_colour_manual(name = "",
                      # using avatar_pal with 4 colours
                      values = c("#015E05", "#FF4500", "#1DB4D3"),
                      labels = c("Earth", "Fire", "Water"),
                      guide = F) +
  labs(title = "The anti sweet spot of chattiness",
       subtitle = "Across all three books, IMDB ratings drop 
when the characters say between 2000 and 2200 words.",
       x = "Total number of words spoken by characters",
       y = "IMDB rating")

plotBook <- function(df = avatar, book, palette) {
  
}

avatar %>% 
  filter(book == "Fire") %>%
 # mutate( = -chapter_num) %>%
  arrange(-chapter_num) %>%
  mutate(id = row_number()) -> fire
  


ggplot(fire, aes(x = id, y = total_chat, fill = imdb)) +
  geom_bar(stat = "identity") + 
  theme_avatar(title.font = "Slayer",
               text.font = "Slayer",
               title.size = 14) +
  ylim(c(0, 2750)) +
  xlim(c(-10, 25)) +
  coord_polar(theta = "y", start = 4.71, clip = "off") +
  labs(title = "Fire") +
  geom_richtext(#data = NULL,
    aes(y = 0, label = chapter_num),
    size = 3,
    family = "Slayer",
    fill = NA,
    label.color = NA,
    vjust = .85
  ) +
  scale_fill_avatar(palette = "FireNation", nrow(avatar), type = "continuous") -> fire


# Export to create making-of gif 
ggsave(filename = file.path("../making-of/temp", paste0("202008-avatar", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
       dpi = 400, width = 10, height = 10)

# Export final plot
ggsave(filename = "../plots/202008_avatar.png", dpi = 400, width = 10, height = 10, type = cairo)
