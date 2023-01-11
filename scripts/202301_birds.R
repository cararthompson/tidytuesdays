# Birds!
library(tidyverse)

# Data
birds <- tidytuesdayR::tt_load(x = "2023-01-10")

# Fonts
systemfonts::register_variant("Averia Light", "Averia Serif Libre", weight = "light")

# Get extra data for species names
species_codes <- read.csv("https://feederwatch.org/wp-content/uploads/2022/08/PFW-species-translation-table.csv") %>%
  janitor::clean_names() %>%
  rename(species_code = i_species_code)

# Only spotted 1-5 five times
rare_finds <- birds$PFW_2021_public %>%
  group_by(species_code) %>%
  summarise(count = sum(how_many)) %>%
  filter(count <= 5) %>%
  left_join(species_codes) %>%
  group_by(count) %>%
  mutate(y_pos = rank(desc(american_english_name)))

woodpecker <- "https://www.rspb.org.uk/globalassets/images/birds-and-wildlife/bird-species-illustrations/lesser-spotted-woodpecker_male_1200x675.jpg"

palette <- list(dark_text = "#3f4140",
                light_text = "#9d938a",
                bg = "#fbf9fa",
                red_text = "#7d3d3b")


# Plot
ggplot(rare_finds) +
  cowplot::draw_image(woodpecker, x = 4.25, y = 26, 
                      scale = 20, vjust = 0.5, hjust = 0.5) +
  ggtext::geom_textbox(aes(x = count,
                           y = y_pos,
                           label = gsub(" \\(.*?\\)", "", american_english_name)),
                       hjust = 0.5, 
                       halign = 0.5,
                       width = unit(25, "lines"),
                       size = 3,
                       fill = NA,
                       box.colour = NA,
                       colour = palette$dark_text,
                       family = "Averia Light") +
  ggtext::geom_textbox(aes(x = 5.5,
                           y = 42,
                           label = "Lesser spotted species"),
                       hjust = 1, 
                       halign = 1,
                       vjust = 1, valign = 1,
                       width = unit(25, "lines"),
                       size = 9,
                       fill = NA,
                       box.colour = NA,
                       colour = palette$dark_text,
                       family = "Averia Light") +
  ggtext::geom_textbox(aes(x = 5.5,
                           y = 38.5,
                           label = "Lists of all bird species spotted only once to five times in the dataset.<br>TIL that's not what lesser spotted means."),
                       hjust = 1, 
                       halign = 1,
                       vjust = 1, valign = 1,
                       width = unit(25, "lines"),
                       size = 3,
                       lineheight = 1.3,
                       fill = NA,
                       box.colour = NA,
                       colour = palette$light_text,
                       family = "Mukta") +
  ggtext::geom_textbox(aes(x = 5.5,
                           y = 35.9,
                           label = "Dataviz: @cararthompson | Source: Project FeederWatch | Image: rspb.org.uk"),
                       hjust = 1, 
                       halign = 1,
                       vjust = 1, valign = 1,
                       width = unit(25, "lines"),
                       size = 2,
                       lineheight = 1.3,
                       fill = NA,
                       box.colour = NA,
                       colour = "#c3bfba",
                       family = "Mukta") +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5),
                     labels = c("Once", "Twice", "Three times", "Four times", "Five times"),
                     limits = c(0.5, 5.5)) +
  theme_minimal() +
  theme(text = element_text(family = "Averia Light", colour = palette$light_text),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, colour = palette$red_text),
        panel.grid = element_blank(), 
        plot.caption = element_text(family = "Mukta", size = 6, colour = palette$light_text, margin = margin(12, 0, 0, 0),
                                    hjust = 0.92))
  
  

# Exports for Making of
ggsave(filename = file.path("making-of/temp", 
                            paste0("202201_birds-", 
                                   format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
       dpi = 400, width = 10, height = 6, bg = palette$bg)

# Final export
ggsave(filename = file.path("plots/", 
                            "202301_birds.png"), 
       dpi = 400, width = 10, height = 6, bg = palette$bg)
