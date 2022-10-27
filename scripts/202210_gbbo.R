# Bake off!
library(tidyverse)
library(ggtext)
library(cowplot)

# Data
gbbo <- tidytuesdayR::tt_load(x = "2022-10-25")

# To get the cake drips in a sensible order for shrinking tiers!
baker_order <- tibble(baker = c("Dan", 
                                "Amelia", "Phil", "Michelle",
                                "Priya", "Henry", 
                                "David", "Alice", "Steph", 
                                "Rosie",  "Michael", 
                                "Helena", "Jamie"),
                      left_to_right = c(1:13)) %>%
  mutate(staggered_labels = case_when(left_to_right %% 2 == 0 ~ paste0("<br><br>", baker),
                                      TRUE ~ baker))

technicals <- gbbo$challenges %>%
  filter(series == 10) %>%
  left_join(baker_order) %>%
  group_by(episode) %>%
  filter(!is.na(technical)) %>%
  mutate(top_start = min(left_to_right),
         top_end = max(left_to_right),
         cake_depth = max(technical) + 5,
         rect_fill = (left_to_right - min(left_to_right)) / max(left_to_right)) %>%
  ungroup() %>%
  mutate(baker_label = case_when(episode == 1 ~ baker,
                                 TRUE ~ ""))

# Font
systemfonts::register_font("GBBO", 
                           plain = "../../../Downloads/OPTIAuvantGothic-DemiBold.ttf")

# Plot
p <- ggplot(technicals, 
       aes(x = left_to_right, y = -technical)) +  
  geom_rect(aes(ymin = 0.5, ymax = -cake_depth,
                xmin = top_start - 0.35, xmax = top_end + 0.35),
            fill = "#d3c399",
            colour = NA,
            size = 0.5) +
  annotation_custom(grid::rasterGrob(paste0("#f9f9f7", as.hexmode(1:240)), 
                                     width=unit(1,"npc"), 
                                     height = unit(1,"npc"), 
                                     interpolate = TRUE), 
                    xmin = -Inf, xmax = Inf, 
                     ymin = -Inf, ymax = 10) +
  # To get the outline of the tiers above the fade
  geom_rect(aes(ymin = 0.5, ymax = -cake_depth,
                xmin = top_start - 0.35, xmax = top_end + 0.35),
            fill = NA,
            colour = "#f4f5ec") +
  geom_point(aes(size = -technical,
                 colour = episode)) +
  facet_grid(-episode ~., scales = "free", 
             space = "free") +
  geom_textbox(aes(label = toupper(baker_label)),
               hjust = 0.5, halign = 0.5,
               vjust = 1.15, valign = 1,
               family = "GBBO",
               fill = NA,
               box.colour = NA,
               colour = "#543d37",
               size = 2.5) +
  scale_y_continuous(expand = c(0, 0.08)) +
  geom_segment(aes(y = 0.5, yend = 0.5,
                   x = top_start - 0.35, xend = top_end + 0.35,
                   colour = episode), lineend = "round", size = 5) +
  geom_curve(aes(y = 0.5, yend = -technical,
                 x = left_to_right-0.12, xend = left_to_right, 
                 size = -technical, colour = episode),
             curvature = -0.1) +
  geom_curve(aes(y = 0.5, yend = -technical,
                 x = left_to_right+0.12, xend = left_to_right,
                 size = -technical, colour = episode),
             curvature = 0.1) +
  geom_segment(aes(y = 0.5, yend = -technical,
                   x = left_to_right, xend = left_to_right,
                   size = -technical, colour = episode)) +
  scale_size(range = c(1.7, 5)) +
  xlim(c(-2, 14)) +
  labs(caption = "\nDataviz by @cararthompson | #TidyTuesday\nSource: {bakeoff} by @apreshill") +
  scale_fill_continuous(low = "blue", high = "white") +
  scale_colour_gradient2(low = "#e04121", mid = "#f7238a", high = "#ed9e00") +
  scale_alpha_continuous(guide=FALSE) +
  theme_void() +
  theme(legend.position = "none",
        panel.spacing.y=unit(0, "cm"),
        strip.text = element_blank(), 
        axis.text.x = element_blank(),
        plot.margin = margin(2, 0.5, 0.5, 2, unit = "cm"),
        plot.caption = element_text(family = "GBBO", colour = "#543d37",
                                    size = 6,
                                    hjust = 0.9,
                                    lineheight = 1.3))

logo_file <- here::here("making-of/temp/bake-off.png")

cowplot::ggdraw() + 
  cowplot::draw_plot(p) +
  cowplot::draw_text(text = "Race to the top", x = 0.08, y = 0.72,
                     hjust = 0, vjust = 1, #halign = 0, valign = 1, 
                     size = 24, family = "GBBO", color = "#543d37") +
  cowplot::draw_text(text = "Each contestant from the tenth \nseason is represented by a drip,\ncharting their way from the\noriginal baker's dozen\nto the final three.", 
                     x = 0.08, y = 0.65,
                     hjust = 0, vjust = 1, #halign = 0, valign = 1, 
                     size = 12, family = "GBBO", color = "#543d37") +
  cowplot::draw_text(text = "\nThe length of the drip shows\ntheir place in the technical.\nThe baker with the poorest\ntechnical score (the \nlongest drip) is\nseldom the one \nto leave the tent.", 
                     x = 0.08, y = 0.5,
                     hjust = 0, vjust = 1, #halign = 0, valign = 1, 
                     size = 10, family = "GBBO", color = "#543d37") +
  cowplot::draw_image(logo_file, x = 0.08, y = 0.95, 
                      hjust = 0, vjust = 1, halign = 0, valign = 1,
                      width = 0.3)


# Exports for Making of
ggsave(filename = file.path("making-of/temp", 
                            paste0("202210_gbbo-", 
                                   format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
       dpi = 400,width = 9, height = 8, bg = "#f9f9f7")

# Final export
ggsave(filename = file.path("plots/", 
                            "202210_gbbo.png"), 
       dpi = 400, width = 9, height = 8, bg = "#f9f9f7")
