# Setup ----
library(ggplot2)

pi <- tidytuesdayR::tt_load("2026-03-24")

# Get the data ready
pi$pi_digits |>
  dplyr::filter(digit_position < 10001) |>
  dplyr::mutate(
    pi_y = sort(rep(c(1:100), 100)),
    pi_x = dplyr::case_when(
      digit_position %% 100 == 0 ~ 100,
      .default = digit_position %% 100
    )
  ) |>
  ggplot(aes(x = pi_x, y = pi_y)) +
  labs(caption = "Cara Thompson #TidyTuesday") +
  geom_text(
    aes(label = digit),
    colour = "#161719",
    family = "Sometype Mono",
    alpha = 0.7
  ) +
  scale_y_reverse(expand = expansion(c(0.03, 0.05))) +
  scale_x_continuous(expand = expansion(c(0.05, 0.05))) +
  theme_void() +
  theme(
    plot.caption = element_text(
      family = "Caveat",
      margin = margin(0, 25, 0, 0)
    ),
    plot.margin = margin(20, 20, 25, 20)
  )

# Exporting for Making of
ggsave(
  filename = file.path(here::here(
    "making-of/temp",
    paste0("202603_pi-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
  )),
  dpi = 400,
  width = 8,
  height = 8,
  bg = "#ffffff"
)

# Final export ----
ggsave(
  filename = here::here("plots", "202603_pi.png"),
  dpi = 400,
  width = 8,
  height = 8,
  bg = "#ffffff"
)
