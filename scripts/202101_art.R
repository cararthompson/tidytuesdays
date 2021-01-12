# Art!
library(tidyverse)
library(tidytuesdayR)
library(ggridges)
library(extrafont)

## Get the data ----
tt_data <- tt_load(2021, week = 3)
readme(tt_data)

## Rework the data ----
art_age <- tt_data$artwork %>%
  left_join(rename(tt_data$artists, artistId = id), 
        by = "artistId") %>%
  filter(artistRole == "artist",
         # Applied to groups, and resulted in meaningless age at creation
         !grepl("active|established|founded", dates),
         # Idem
         !grepl("British School", artist)) %>%
  mutate(age_at_creation = year - yearOfBirth) %>%
  # Remove small number of artworks dated as y.o.b-completion
  filter(age_at_creation > 0, 
         grepl("United Kingdom", placeOfBirth)) %>%
  arrange(yearOfBirth) %>%
  # To plot artists in order of year of birth
  mutate(artist = factor(artist, levels = unique(artist))) %>%
  add_count(artist) %>%
  # Turner had 37622 items in the collection; 
  # replacing with 1000 to allow better colour gradient
  mutate(n_upper_lim = ifelse(n > 1000, 1000, n))


## Build theme ----
theme_bw_art <- function() {
  theme_minimal() %+replace%
    theme(plot.background = element_rect(fill = "#e1d7cd", colour = "#e1d7cd"),
          panel.grid = element_line(color = "#e1d7cd"),
          panel.background = element_rect(fill = "#e1d7cd", colour = "#e1d7cd"),
          text = element_text(colour = "#242c28", family = "Arvo"), 
          plot.title = element_text(hjust = 0.5, size = 20, family = "Arvo"),
          axis.text.y = element_blank(),
          plot.subtitle = element_text(hjust = 0.5, color = "#242c28", size = 12),
          plot.caption = element_text(color = "#242c28", size = 10),
          axis.title = element_text(color = "#242c28", size = 10),
          axis.ticks = element_blank())
}
  

## Plot it! ----
ggplot(art_age, aes(x = age_at_creation, y = artist)) +
  geom_density_ridges(aes(alpha = n_upper_lim), colour = "#414f43", fill = "#606f60",
                      show.legend = F) +
  theme_bw_art() +
  labs(x = "Age at creation of artwork",
       y = "Artists born in the United Kingdom, in ascending order of
       \n< 1572   |   year of birth   |   1982 >",
       title = "Ebbs and Flows",
       subtitle = "\nEach line in this graph represents an artist born in the United Kingdom whose art
is owned by Tate or jointly owned with the National Galleries of Scotland.
As each artist's age increases along the x axis, the variation in the height of
their line along the y axis represents the proportion of their artwork
created at that age that is included in this collection. The greater the number
of pieces of art by the artist are included in this collection,
the darker the shadow under the line that represents
them and their work.",
       caption = "\n\n#TidyTuesday | Graphic: @cararthompson | Data: Tate Art Museum")


## Export to create making-of gif ----
ggsave(filename = file.path("../making-of/temp", paste0("202101_art-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
       dpi = 400, width = 10, height = 20)


## Export final plot ----
ggsave(filename = file.path("../plots", "202101_art.png"), 
       dpi = 600, width = 10, height = 20)

