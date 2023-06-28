# Populated places

library(tidyverse)

# Get the data
places <- tidytuesdayR::tt_load(x = "2023-06-27")
# tidytuesdayR::readme(places)

constitution_preamble <- "We the People of the United States, in Order to form a more perfect Union, establish Justice, insure domestic Tranquility, provide for the common defense, promote the general Welfare, and secure the Blessings of Liberty to ourselves and our Posterity, do ordain and establish this Constitution for the United States of America." 

constitution_colours <- c("People" = "#193235",
                          "United" = "#51646a",
                          "States" = "#7c597d",
                          "perfect" = "#98a197",
                          "Union" =  "#011d4c", 
                          "Justice" = "#303155",
                          "Tranquility" = "#6a869a",
                          "defense" = "#575160",
                          "Welfare" = "#375558",
                          "Blessings" = "#dd8c1f",
                          "Liberty" = "#437d76",
                          "Posterity" = NA,
                          "Constitution" = "#996202",
                          "America" = "#5d4355") %>%
  enframe() %>%
  rename(constitution_word = name,
         colour = value)

constitution_places <- places$us_place_names %>%
  mutate(constitution_word = 
           case_when(grepl("People", feature_name) ~ "People", #193235
                     grepl("United", feature_name) ~ "United", #51646a
                     grepl("States", feature_name) ~ "States", #7c597d
                     # To keep original capitalisation in preamble
                     grepl("[pP]erfect", feature_name) ~ "perfect", #98a197
                     grepl("Union", feature_name) ~ "Union", #ab8699
                     grepl("Justice", feature_name) ~ "Justice", #011d4c
                     grepl("Tranquility", feature_name) ~ "Tranquility", #6a869a
                     grepl("[Dd]efense", feature_name) ~ "defense", #575160
                     grepl("Welfare", feature_name) ~ "Welfare", #375558
                     grepl("Blessing", feature_name) ~ "Blessings", #dd8c1f
                     grepl("Liberty", feature_name) ~ "Liberty",  #6c5f4e
                     grepl("Posterity", feature_name) ~ "Posterity", 
                     grepl("Constitution", feature_name) ~ "Constitution", #8f3132
                     grepl("America", feature_name) ~ "America")) %>%
  # Join before turning into factor otherwise undoes the factor transformation
  left_join(places$us_place_history) %>%
  left_join(constitution_colours) %>%
  filter(!is.na(constitution_word)) %>%
  mutate(constitution_word = factor(constitution_word,
                                    levels = c("People", "United", "States", "perfect", "Union", 
                                               "Justice", "Tranquility", "defense", "Welfare",
                                               "Blessings", "Liberty", "Posterity", "Constitution", "America"), 
                                    # To allow us to create the plot by filtering by factor level
                                    ordered = TRUE)) %>%
  rowwise() %>%
  mutate(preamble = gsub(paste0(constitution_word, ".+"), 
                         paste0("<span style='color:", colour, "'>", 
                                constitution_word, "</span>"), 
                         constitution_preamble)) %>%
  ungroup() %>%
  arrange(constitution_word)

constitution_phrases <- constitution_places %>%
  select(constitution_word, preamble) %>%
  unique() %>%
  # To avoid losing the factor and levels
  mutate(constitution_character = as.character(constitution_word)) %>%
  # No places named posterity
  add_row(constitution_character = "Posterity", 
          preamble = gsub("Posterity.+", 
                          "Posterity", constitution_preamble)) %>%
  replace_na(list(constitution_word = "Posterity")) %>%
  arrange(constitution_word)


create_map <- function(progress_word,
                       places_df = constitution_places,
                       preamble_df = constitution_phrases,
                       palette = constitution_colours) {
  
  p <- ggplot() +
    geom_point(data = filter(places_df, 
                             constitution_word <= progress_word),
               aes(x = prim_long_dec,
                   y = prim_lat_dec,
                   colour = constitution_word),
               alpha = 0.7,
               size = 2) +
    geom_line(data = filter(places_df, 
                            constitution_word <= progress_word),
              aes(x = prim_long_dec,
                  y = prim_lat_dec,
                  colour = constitution_word),
              alpha = 0.5,
              size = 0.1) +
    ggtext::geom_textbox(data = NULL,
                         aes(x = -108, y = 68),
                         label = toupper("No place like Posterity"),
                         vjust = 1,
                         halign = 0.5,
                         family = "Averia Serif GWF",
                         fontface = "bold",
                         size = 6,
                         box.colour = NA,
                         width = unit(30, "lines"),
                         fill = NA,
                         colour = "#090911") +
    ggtext::geom_textbox(data = filter(preamble_df, 
                                       constitution_word <= progress_word),
                         aes(x = -108, y = 64,
                             label = preamble),
                         alpha = 0.8,
                         box.colour = NA,
                         family = "Averia Serif Libre",
                         vjust = 1,
                         width = unit(33.5, "lines"),
                         fill = NA,
                         colour = "#131321") +
    scale_colour_manual(breaks = palette$constitution_word, 
                        values = palette$colour) +
    labs(caption = "Place names in the USA which include each key word from the Constitution preamble. No place names containing the word \"Posterity\" were found.
      <br>Dataviz: Cara Thompson | #TidyTuesday | Source: US Board of Geographic Names") +
    coord_sf() +
    # To keep the map stable regardless of the range of the data
    xlim(c(-154, -62)) +
    ylim(c(18, 70.5)) +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          plot.caption = ggtext::element_textbox_simple(hjust = 0.5,
                                                        halign = 0.5,
                                                        family = "Averia Serif Libre",
                                                        size = 8,
                                                        colour = "#42435C"))
  
  
  # To get a background that works regardless of fixed coordinates:
  cowplot::ggdraw(p) + 
    theme(plot.background = element_rect(colour = "#E5E0E7", 
                                         fill = "#F9F6F3", size = 12.5))
  
  # Export final plot
  ggsave(filename = here::here("temp",
                               paste0("202306_places_", 
                                      # To get them in order
                                      grep(progress_word, levels(preamble_df$constitution_word)), 
                                      ".png")), 
         dpi = 400, width = 10, height = 7, bg = "#FFFFFF")
}

create_final_map <- function(places_df = constitution_places,
                             preamble = constitution_preamble,
                             palette = constitution_colours) {
  
  p <- ggplot() +
    geom_point(data = places_df,
               aes(x = prim_long_dec,
                   y = prim_lat_dec,
                   colour = constitution_word),
               alpha = 0.7,
               size = 2) +
    geom_line(data = places_df,
              aes(x = prim_long_dec,
                  y = prim_lat_dec,
                  colour = constitution_word),
              alpha = 0.5,
              size = 0.1) +
    ggtext::geom_textbox(data = NULL,
                         aes(x = -108, y = 68),
                         label = toupper("No place like Posterity"),
                         vjust = 1,
                         halign = 0.5,
                         family = "Averia Serif GWF",
                         fontface = "bold",
                         size = 6,
                         box.colour = NA,
                         width = unit(30, "lines"),
                         fill = NA,
                         colour = "#090911") +
    ggtext::geom_textbox(data = NULL,
                         aes(x = -108, y = 64),
                         label = preamble,
                         alpha = 0.8,
                         box.colour = NA,
                         family = "Averia Serif Libre",
                         vjust = 1,
                         width = unit(33.5, "lines"),
                         fill = NA,
                         colour = "#131321") +
    scale_colour_manual(breaks = palette$constitution_word, 
                        values = palette$colour) +
    labs(caption = "Place names in the USA which include each key word from the Constitution preamble. No place names containing the word \"Posterity\" were found.
      <br>Dataviz: Cara Thompson | #TidyTuesday | Source: US Board of Geographic Names") +
    coord_sf() +
    # To keep the map stable regardless of the range of the data
    xlim(c(-154, -62)) +
    ylim(c(18, 70.5)) +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          plot.caption = ggtext::element_textbox_simple(hjust = 0.5,
                                                        halign = 0.5,
                                                        family = "Averia Serif Libre",
                                                        size = 8,
                                                        colour = "#42435C"))
  
  
  # To get a background that works regardless of fixed coordinates:
  cowplot::ggdraw(p) + 
    theme(plot.background = element_rect(colour = "#E5E0E7", 
                                         fill = "#F9F6F3", size = 12.5))
  
  # Export final plot
  ggsave(filename = here::here("temp",
                               paste0("202306_places_15.png")), 
         dpi = 400, width = 10, height = 7, bg = "#FFFFFF")
}

create_empty_map <- function() {
  
  p <- ggplot() +
    ggtext::geom_textbox(data = NULL,
                         aes(x = -108, y = 68),
                         label = toupper("No place like Posterity"),
                         vjust = 1,
                         halign = 0.5,
                         family = "Averia Serif GWF",
                         fontface = "bold",
                         size = 6,
                         box.colour = NA,
                         width = unit(30, "lines"),
                         fill = NA,
                         colour = "#090911") +
    labs(caption = "Place names in the USA which include each key word from the Constitution preamble. No place names containing the word \"Posterity\" were found.
      <br>Dataviz: Cara Thompson | #TidyTuesday | Source: US Board of Geographic Names") +
    coord_sf() +
    # To keep the map stable regardless of the range of the data
    xlim(c(-154, -62)) +
    ylim(c(18, 70.5)) +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          plot.caption = ggtext::element_textbox_simple(hjust = 0.5,
                                                        halign = 0.5,
                                                        family = "Averia Serif Libre",
                                                        size = 8,
                                                        colour = "#42435C"))
  
  
  # To get a background that works regardless of fixed coordinates:
  cowplot::ggdraw(p) + 
    theme(plot.background = 
            element_rect(colour = "#E5E0E7", 
                         fill = "#F9F6F3", 
                         size = 12.5))
  
  # Export final plot
  ggsave(filename = here::here("temp",
                               paste0("202306_places_0", 
                                      ".png")), 
         dpi = 400, width = 10, height = 7, bg = "#FFFFFF")
}


# Export all the maps

create_empty_map()

purrr::map(levels(constitution_places$constitution_word),
           create_map)

create_final_map()
