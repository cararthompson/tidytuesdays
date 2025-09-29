# Setup ----
library(ggplot2)

chess <- tidytuesdayR::tt_load("2025-09-23")

ratings <- chess$fide_ratings_august |>
  dplyr::rename(aug_rating = rating, aug_games = games) |>
  dplyr::left_join(
    chess$fide_ratings_september |>
      dplyr::rename(sept_rating = rating, sept_games = games)
  ) |>
  dplyr::filter(aug_games != 0 & sept_games != 0) |>
  dplyr::group_by(sex) |>
  dplyr::mutate(
    rating_diff = sept_rating - aug_rating,
    sept_rank = rank(-sept_rating),
    aug_rank = rank(-aug_rating)
  ) |>
  dplyr::mutate(
    age_group = factor(
      dplyr::case_when(
        bday > 2025 - 13 ~ "Children",
        dplyr::between(bday, 2025 - 17, 2025 - 13) ~ "Teens",
        dplyr::between(bday, 2025 - 25, 2025 - 18) ~ "18-25",
        dplyr::between(bday, 2025 - 35, 2025 - 26) ~ "26-35",
        dplyr::between(bday, 2025 - 45, 2025 - 36) ~ "36-45",
        dplyr::between(bday, 2025 - 55, 2025 - 46) ~ "46-55",
        dplyr::between(bday, 2025 - 65, 2025 - 56) ~ "56-65",
        bday < 2025 - 65 ~ "66+"
      ),
      levels = c(
        "Children",
        "Teens",
        "18-25",
        "26-35",
        "36-45",
        "46-55",
        "56-65",
        "66+"
      )
    )
  ) |>
  dplyr::group_by(age_group, sex) |>
  dplyr::summarise(
    count = length(sept_rating),
    mean_rating = mean(sept_rating, na.rm = TRUE),
    mean_n_games = mean(sept_games, na.rm = TRUE),
    mean_rating_diff = mean(rating_diff, na.rm = TRUE)
  )

# Create a parameterised function ----

make_heat_map <- function(alpha_value, highlight_colour) {
  ggplot() +
    geom_rect(
      data = dplyr::filter(ratings, sex == "F"),
      aes(
        xmin = as.numeric(age_group) - 0.5,
        xmax = as.numeric(age_group) + 0.5,
        ymin = -Inf,
        ymax = Inf,
        alpha = get(alpha_value),
        fill = get(alpha_value)
      )
    ) +
    geom_rect(
      data = dplyr::filter(ratings, sex == "F"),
      aes(
        ymin = as.numeric(age_group) - 0.5,
        ymax = as.numeric(age_group) + 0.5,
        xmin = -Inf,
        xmax = Inf,
        alpha = get(alpha_value),
        fill = get(alpha_value)
      )
    ) +
    scale_y_continuous(expand = c(0, 0), breaks = c(1:8), labels = function(x) {
      levels(ratings$age_group)[x]
    }) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = c(1:8),
      labels = function(x) {
        levels(ratings$age_group)[x]
      }
    ) +
    scale_fill_gradient(low = "#461648", high = highlight_colour) +
    labs(x = "\nQUEENS", y = "KINGS\n") +
    coord_equal() +
    scale_alpha_continuous(range = c(0.5, 0.8)) +
    theme_minimal() +
    theme(
      text = element_text(family = "DM Sans", colour = "#1b0903"),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text = element_text(
        colour = "#1b0903",
        size = 8,
        hjust = 0.5,
        family = "DM Sans",
        margin = margin(32, 32, 32, 32)
      ),
      axis.text.y = element_text(
        angle = 90,
        hjust = 0.5
      ),
      axis.title = element_text(
        colour = "#3e1942",
        size = 10,
        family = "Lora",
        margin = margin(12, 0, 12, 0),
        vjust = 0
      ),
      panel.background = element_rect(
        fill = "white",
        colour = "white"
      ),
      plot.title = ggtext::element_textbox_simple(
        family = "Lora",
        face = "bold",
        size = 16,
        hjust = 0.5,
        halign = 0.5,
        valign = 0,
        vjust = 0,
        margin = margin(16 * 4, 0, 12, 0)
      ),
      plot.background = element_rect(fill = "#f6f6f6")
    )
}

# Build the output bit by bit ----

title_and_subtitle <- cowplot::plot_grid(
  cowplot::ggdraw() +
    cowplot::draw_text(
      text = "Queens and Kings of Chess",
      family = "Lora",
      hjust = 0.5,
      size = 32
    ),
  ggplot() +
    marquee::geom_marquee(
      aes(x = 1, y = 1),
      label = "Exploring the August and September ratings from FIDE, the International Chess Federation     
      In each grid, a brighter colour represents a higher value",
      family = "DM Sans",
      hjust = 0.5,
      style = marquee::classic_style(align = "center"),
      size = 5.5,
      width = 1
    ) +
    theme_void(),
  nrow = 2,
  rel_heights = c(2, 1)
)

caption <- cowplot::ggdraw() +
  cowplot::draw_text(
    text = "Data visualisation: Cara R Thompson | Source: FIDE | #TidyTuesday",
    family = "DM Sans",
    hjust = 0.5,
    size = 10
  )

boards <- cowplot::plot_grid(
  make_heat_map(alpha_value = "count", highlight_colour = "#ee5294") +
    labs(
      title = "How many players played at least once in both months?"
    ),
  make_heat_map(alpha_value = "mean_n_games", highlight_colour = "#f37d20") +
    labs(title = "How many games did they play on average in September?"),
  make_heat_map(alpha_value = "mean_rating", highlight_colour = "#8bf9fd") +
    labs(title = "What was the mean September rating in each group?"),
  make_heat_map(
    alpha_value = "mean_rating_diff",
    highlight_colour = "#43eead"
  ) +
    labs(title = "How did their ratings evolve from August to September?"),
  nrow = 2,
  ncol = 2,
  rel_heights = 1,
  rel_widths = 1,
  align = "hv"
)

full_set <- cowplot::plot_grid(
  title_and_subtitle,
  boards,
  caption,
  nrow = 3,
  rel_heights = c(0.15, 0.85, 0.05)
) +
  theme(
    plot.background = element_rect(fill = "#f6f6f6"),
    plot.margin = margin(32, 32, 32, 16)
  )

# Final export ----
ggsave(
  filename = here::here("plots", "202509_chess.png"),
  dpi = 400,
  width = 16,
  height = 14
)
