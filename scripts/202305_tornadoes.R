# Tornadoes

library(tidyverse)

# Get the data
tornadoes <- tidytuesdayR::tt_load(x = "2023-05-16")
tidytuesdayR::readme(tornadoes)

# Plot it
tornadoes$tornados |>
  group_by(st) |>
  summarise(mean_magnitude = mean(mag, na.rm = TRUE),
            total_number = length(st),
            mean_len = mean(len, na.rm = TRUE),
            min_magnitude = min(mag, na.rm = TRUE),
            max_magnitude = max(mag, na.rm = TRUE)) |>
  left_join(tibble(state.abb, state.name) |>
              rename(st = state.abb)) |>
  filter(!is.na(state.name)) |>
  ggplot() +
  geom_rect(data = tibble(band = seq(500, 9500, 1000),
                          length = rep(1, 10)),
            aes(xmin = 0,
                xmax = Inf,
                ymin = band + 500,
                ymax = band - 500,
                fill = band),
            color = "#101c1c",
            size = 0) +
  geom_text(aes(x = mean_magnitude,
                y = total_number,
                label = state.name,
                size = mean_magnitude,
                alpha = total_number,
                colour = total_number),
            family = "IM FELL English") +
  scale_y_continuous(breaks = seq(1000, 10000, 1000),
                     limits = c(0, 10000),
                     expand = c(0, 0),
                     labels = function(x) format(x, big.mark = ",")) +
  scale_x_continuous(breaks = seq(0.1, 1.2, 0.1),
                     limits = c(0, 1.25),
                     expand = c(0, 0)) +
  scale_colour_gradient(high = "#7d9fa0", 
                        low = "#e0a063") +
  scale_fill_gradient(high = "#101c1c", 
                      low = "#3f4b4b") +
  scale_alpha(range = c(0.3, 1)) +
  labs(x = "Mean magnitude") +
  ggtext::geom_textbox(aes(x = 0.025,
                           y = 8050,
                           label = "Number of tornadoes and mean magnitudes per state<span style='color:#979c96; font-family:\"IM FELL English\"; font-size:32pt;'><br>More than its fair share</span><br>
                           From 1955 to 2022, NOAA's National Weather Service Storm Prediction Center recorded nearly 70,000 tornadoes. While **Arkansas** witnessed the strongest average magnitude, 
                           **Texas** experienced over twice as many tornadoes as the next most frequent state.<br>
                           <span style='font-size:8pt;'>Source: NOOA | Data visualisation: @cararthompson | #TidyTuesday"),
                       hjust = 0,
                       vjust = 1,
                       halign = 0.5,
                       fill = NA,
                       width = unit(24, "lines"),
                       box.colour = NA,
                       colour = "#88979a",
                       family = "Noah",
                       alpha = 0.5,
                       lineheight = 1.4) +
  theme_minimal() +
  theme(text = element_text(family = "IM FELL English"),
        legend.position = "none",
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "#101c1c", 
                                        colour = "#101c1c"),
        plot.background = element_rect(fill = "#101c1c", 
                                       colour = "#101c1c"),
        axis.line.x = element_line(colour = "#3d4e25"),
        axis.line.y = element_line(colour = "#101c1c"),
        axis.text.x = element_text(colour = "#3d4e25",
                                   size = 24,
                                   family = "IM FELL English"),
        axis.title.x = element_text(colour = "#3d4e25", hjust = 0.99),
        axis.text.y = element_text(colour = "#465655",
                                   size = 16),
        panel.grid = element_blank(),
        plot.margin = margin(10, 0, 5, 5, "mm"))


# Exports for Making of
ggsave(filename = file.path(here::here("making-of/temp", 
                                       paste0("202305_tornadoes-", 
                                              format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))), 
       dpi = 400, width = 11, height = 9, bg = "#101c1c")

# Export final plot
ggsave(filename = here::here("plots", "202305_tornadoes.png"), 
       dpi = 400, width = 11, height = 9, bg = "#101c1c")
