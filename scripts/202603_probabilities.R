# Setup ----
library(ggplot2)

probabilities <- tidytuesdayR::tt_load("2026-03-10")

# Get the data ready

ordered_terms <- probabilities$absolute_judgements |>
  dplyr::left_join(probabilities$respondent_metadata) |>
  dplyr::group_by(term) |>
  dplyr::summarise(mean = mean(probability)) |>
  dplyr::arrange(mean) |>
  dplyr::pull(term)

all_data <- probabilities$absolute_judgements |>
  dplyr::left_join(probabilities$respondent_metadata)

# Plot it!

all_data |>
  dplyr::mutate(term = factor(term, levels = ordered_terms)) |>
  ggplot() +
  geom_segment(
    aes(x = probability, xend = probability, y = 0, yend = 1),
    position = position_jitter(width = 0.9, height = 0.2),
    alpha = 0.01,
    colour = "white"
  ) +
  facet_grid(term ~ ., switch = "y") +
  theme_void() +
  scale_x_continuous(
    position = c("top"),
    expand = expansion(add = c(0.9, 0.9)),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "How likely is it, though?",
    caption = "<span style='font-size: 15pt; color:#f8f7f4; font-family:\"Karst\"'>**One thing's for sure, we like round numbers.**</span><br><br>Data Visualisation: Cara Thompson, Building Stories with Data LTD<br>Source: Kucharski AJ (2026) CAPphrase, doi: 10.5281/zenodo.18750055<br>#TidyTuesday"
  ) +
  theme(
    text = element_text(
      family = "Fira Mono",
      colour = "#bebcb9"
    ),
    axis.text.x.top = element_text(
      colour = "#bebcb9",
      size = 9,
      margin = margin_auto(10)
    ),
    strip.text.y.left = element_text(
      size = 9,
      margin = margin_auto(0, 10, 0, 0),
      hjust = 1
    ),
    plot.background = element_rect(
      fill = "#0F0F0F",
      colour = "#c8c7c4",
      linewidth = 10
    ),
    plot.caption = ggtext::element_textbox_simple(
      margin = margin(40, 0, 0, 20),
      halign = 1,
      size = 8
    ),
    plot.title = ggtext::element_textbox_simple(
      margin = margin(20, 0, 0, 0),
      size = 10,
      colour = "#f8f7f4"
    ),
    panel.background = element_rect(
      fill = alpha("#c8c7c4", 0.05)
    ),
    plot.margin = margin_auto(40)
  )


# Create a parameterised function ----

# Exporting for Making of
ggsave(
  filename = file.path(here::here(
    "making-of/temp",
    paste0("202503_probabilities-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
  )),
  dpi = 400,
  width = 8,
  height = 7,
  bg = "#ffffff"
)

# Final export ----
ggsave(
  filename = here::here("plots", "202603_probabilities.png"),
  dpi = 400,
  width = 8,
  height = 7
)
