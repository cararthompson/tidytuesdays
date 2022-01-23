# Chocolate!
library(tidyverse)
library(extrafont)
library(ggtext)
library(geomtextpath)

# Get the data ready ----
tt_data <- tidytuesdayR::tt_load('2022-01-18')

# Manipulate and plot it ----

background <- "#fdfeff"
purple <- "#642e89"
brown <- "#531302"

tt_data$chocolate %>%
  group_by(rating) %>%
   count() %>%
   ungroup() %>%
  mutate(prop = n/nrow(tt_data$chocolate)) %>%
  ggplot() +
  geom_area(aes(x = rating, y = prop*6 + rating),
            fill = purple,
            alpha = 0.6) +
  geom_area(aes(x = rating, y = rating),
            fill = background) +
  geom_textpath(aes(x = rating, y = prop*6 + rating*1.1),
                label = "Each entry is given a score of 1 to 4. The swirl is thickest as it passes 3.5, the score most attributed in the history of the project.",
                family = "DM Sans",
                colour = purple,
                text_only = T) +
  geom_textbox(data = tibble(x = c(1, 2, 3, 3.5, 4), 
                             y = c(1.6, 2.8, 5, 6, 5), 
                             label = c("1", "2", "3", "<span style=\"font-size:20pt\">3.5</span>", "4")),
               aes(x = x, y = y, label = label),
               hjust = 0.5,
               halign = 0.5,
               box.colour = NA,
               fill = NA,
               colour = brown,
               family = "Aller Display") +
  coord_polar() +
  expand_limits(x = 1) +
  ylim(c(0, NA)) +
  theme_void() +
  theme(text = element_text(family = "DM Sans"),
        plot.title = element_text(family = "Aller Display", size = 20, colour = brown, hjust = 0.5),
        plot.subtitle = element_text(size = 16, colour = purple, hjust = 0.5),
        plot.caption = element_text(size = 9, colour = purple, hjust = 0.5),
        plot.background = element_rect(colour = background, fill = background),
        panel.background = element_rect(colour = background, fill = background)) +
  labs(title = "\nNot all chocolates are created equal",
       subtitle = "But most of them are pretty good!",
       caption = "Graphic: @cararthompson | # TidyTuesday\nSource: Flavors of Cacao
       
       ")

# Export to create making-of gif
ggsave(filename = file.path("making-of/temp", paste0("2022_chocolate-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
       dpi = 400, width = 7, height = 8)

ggsave(filename = file.path("plots/", "202201_chocolate.png"), 
       dpi = 400, width = 7, height = 8)
