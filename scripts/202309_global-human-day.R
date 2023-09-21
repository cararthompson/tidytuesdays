# Global Human Day

library(tidyverse)


# Data prep
human_day <- tidytuesdayR::tt_load(x = "2023-09-12")
tidytuesdayR::readme(human_day)

human_day_filtered <- human_day$all_countries %>%
  filter(country_iso3 %in% c("GBR", "FRA")) %>% {
    bind_rows(., group_by(., Subcategory, Category) %>%
                summarise(hoursPerDayCombined = mean(hoursPerDayCombined),
                          country_iso3 = "mean"))
  } %>% 
  group_by(Category, country_iso3) %>%
  mutate(category_total = sum(hoursPerDayCombined))

plot_title <- cowplot::ggdraw() +
  cowplot::draw_label("\nThe Global Human Day",
             fontfamily = "Kanit",
             fontface = "bold",
             colour = ophelia::ophelia_colours$dark_text,
             hjust = 0.5,
             size = 26)
plot_subtitle <- cowplot::ggdraw() +
  cowplot::draw_label("Exploring how we invest our time, around the globe and within countries\n",
             fontfamily = "Noah",
             colour = ophelia::ophelia_colours$light_text,
             hjust = 0.5,
             size = 14)
plot_caption <- cowplot::ggdraw() +
  cowplot::draw_label("Dataviz: Cara Thompson | @cararthompson | www.cararthompson.com\nSource: The Human Chronome Project | www.humanchronome.org via #TidyTuesday\n",
             fontfamily = "Noah",
             colour = ophelia::ophelia_colours$light_text,
             hjust = 0.5,
             size = 8)

# Create plots

library(ggalluvial)

fr <- ggplot(data = filter(human_day_filtered, country_iso3 == "FRA"),
             aes(axis1 = reorder(Category, -category_total),   # First variable on the X-axis
                 axis2 = reorder(Subcategory, -hoursPerDayCombined), # Second variable on the X-axis
                 y = hoursPerDayCombined)) +
  geom_alluvium(aes(fill = Category), width = 0.2,
                colour = ophelia::ophelia_colours$background) +
  geom_stratum(width = 0, fill = NA, color = NA) +
  ggtext::geom_textbox(stat = "stratum",
                       aes(label = paste0(sprintf("%.2f",hoursPerDayCombined), " hours", " - ", Subcategory),
                           alpha = sqrt(hoursPerDayCombined)),
                       halign = 0,
                       hjust = -0.1,
                       colour = ophelia::ophelia_colours$dark_text,
                       family = "Noah",
                       fill = NA,
                       size = 3.25,
                       width = unit(20, "lines"),
                       box.colour = NA) +
  labs(subtitle = "France") + 
  scale_x_discrete(expand = expansion(c(0, 0.66))) +
  ophelia::scale_fill_ophelia() +
  ophelia::theme_ophelia(void = TRUE) +
  theme(legend.position = "none",
        plot.subtitle = ggtext::element_textbox_simple(colour = ophelia::ophelia_colours$light_text, 
                                                       family = "Kanit", face = "bold",
                                                       hjust = 0.47, halign = 1, width = unit(12, "lines")))


average <- ggplot(data = human_day$all_countries %>%
                    group_by(Category, country_iso3) %>%
                    summarise(total_hours = sum(hoursPerDayCombined)) %>%
                    group_by(Category) %>%
                    summarise(mean_total = mean(total_hours)),
                  aes(axis1 = reorder(Category, -mean_total), # Second variable on the X-axis
                      y = mean_total)) +
  labs(subtitle = "Average Country") + 
  geom_alluvium(aes(fill = Category),
                colour = "#FFFFFF") +
  geom_stratum(width = 0.3, fill = NA, color = NA) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            colour = ophelia::ophelia_colours$light_text,
            family = "Noah") +
  scale_x_discrete(expand = expansion(c(0, 0))) +
  scale_alpha(range = c(0.05, 1)) +
  ophelia::scale_fill_ophelia() +
  ophelia::theme_ophelia(void = TRUE) +
  theme(legend.position = "none",
        plot.subtitle = ggtext::element_textbox_simple(colour = ophelia::ophelia_colours$light_text, 
                                                       family = "Kanit", face = "bold",
                                                       hjust = 0.5, halign = 0.5, width = unit(12, "lines")))

gb <- ggplot(data = filter(human_day_filtered, country_iso3 == "GBR"),
             aes(axis1 = reorder(Subcategory, -hoursPerDayCombined),   # First variable on the X-axis
                 axis2 = reorder(Category, -category_total), # Second variable on the X-axis
                 y = hoursPerDayCombined)) +
  geom_alluvium(aes(fill = Category), width = 0.2,
                colour = ophelia::ophelia_colours$background) +
  geom_stratum(width = 0, fill = NA, color = NA) +
  labs(subtitle = "United Kingdom") +
  ggtext::geom_textbox(stat = "stratum",
            aes(label = paste0(Subcategory, " - ", sprintf("%.2f",hoursPerDayCombined), " hours"),
                alpha = sqrt(hoursPerDayCombined)),
            halign = 1,
            hjust = 1.1,
            colour = ophelia::ophelia_colours$dark_text,
            family = "Noah",
            fill = NA,
            size = 3.25,
            width = unit(20, "lines"),
            box.colour = NA) +
  scale_x_discrete(expand = expansion(c(0.66, 0))) +
  scale_alpha(range = c(0.05, 1)) +
  ophelia::theme_ophelia(void = TRUE) +
  ophelia::scale_fill_ophelia() +
  theme(legend.position = "none",
        plot.subtitle = ggtext::element_textbox_simple(colour = ophelia::ophelia_colours$light_text, 
                                                       family = "Kanit", face = "bold",
                                                       hjust = 0.53, halign = 0, width = unit(12, "lines")))

plots <- cowplot::plot_grid(gb, average, fr, 
                   nrow = 1, rel_widths = c(0.4, 0.2, 0.4))

# Assemble components
cowplot::plot_grid(plot_title,
                   plot_subtitle,
                   plots,
                   plot_caption,
                   nrow = 4,
                   rel_heights = c(0.1, 0.05, 0.75, 0.1))


# Exports for Making of
ggsave(filename = file.path(here::here("making-of/temp", 
                                       paste0("202309_day-", 
                                              format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))), 
       dpi = 400, width = 18, height = 9, bg = ophelia::ophelia_colours$background)

# Export final plot
ggsave(filename = here::here("plots", "202309_global-human-day.png"), 
       dpi = 400, width = 18, height = 9, bg = ophelia::ophelia_colours$background)
