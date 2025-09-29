# Getting the data
tuesdata <- tidytuesdayR::tt_load(2025, week = 10)

# Loading ggplot
library(ggplot2)

# Let's go!
tuesdata$public_response |>
  dplyr::left_join(tuesdata$pixar_films) |>
  dplyr::mutate(
    film_colour = dplyr::case_when(
      grepl("Toy Story", film) ~ "#feca36",
      grepl("Nemo|Dory|Bug|Ratatouille|Dinosaur", film) ~ "#6e139e",
      grepl("Monsters", film) ~ "#71b262",
      grepl("Cars|WALL-E", film) ~ "#f32f43",
      TRUE ~ "#e03db4"
    )
  ) |>
  ggplot(aes(x = release_date)) +
  geom_segment(
    aes(
      xend = release_date,
      y = rotten_tomatoes,
      alpha = release_date,
      yend = critics_choice
    ),
    colour = "#bbced2"
  ) +
  geom_point(
    aes(x = release_date, y = critics_choice, alpha = release_date),
    shape = 22,
    size = 5,
    fill = "#383240",
    colour = "#383240"
  ) +
  ggfx::with_inner_glow(
    geom_point(
      aes(
        y = rotten_tomatoes,
        size = run_time,
        fill = film_colour,
        colour = film_colour,
        alpha = release_date
      ),
      shape = 21
    ),
    "#e8f5fb",
    expand = 3,
    sigma = 20
  ) +
  scale_colour_identity() +
  scale_size(range = c(6, 18)) +
  scale_alpha(range = c(0.5, 0.9)) +
  scale_fill_identity() +
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.017))) +
  scale_y_continuous(expand = expansion(mult = c(0.03, 0.05))) +
  ggfx::with_outer_glow(
    ggtext::geom_textbox(
      data = data.frame(),
      aes(
        x = lubridate::ymd(c("1995-01-01", "2005-01-01", "2015-01-01")),
        y = rep(25, 3),
        label = c(1995, 2005, 2015)
      ),
      hjust = 0.5,
      halign = 0.5,
      family = "Commissioner",
      fontface = "bold",
      colour = "#56547c",
      fill = NA,
      box.colour = NA
    ),
    colour = "white"
  ) +
  ggfx::with_outer_glow(
    ggtext::geom_textbox(
      data = data.frame(),
      aes(
        x = lubridate::ymd("1990-01-01"),
        y = c(50, 75, 100, 102.5),
        label = c("50%", "75%", "100%", "Rating")
      ),
      hjust = 0.5,
      halign = 0.5,
      family = "Commissioner",
      fontface = "bold",
      colour = "#ffffff",
      fill = NA,
      box.colour = NA
    ),
    colour = "#56547c"
  ) +
  ggfx::with_outer_glow(
    ggtext::geom_textbox(
      data = data.frame(),
      aes(x = lubridate::ymd("1990-01-01"), y = 25, label = "Year"),
      family = "Commissioner",
      fontface = "bold",
      colour = "#56547c",
      fill = NA,
      box.colour = NA,
      hjust = 0.5,
      halign = 0.5
    ),
    colour = "white"
  ) +
  ggfx::with_outer_glow(
    ggtext::geom_textbox(
      data = data.frame(),
      aes(
        x = lubridate::ymd("1992-01-01"),
        y = 50,
        label = "The rotten tomatoes crowd has typically cheered the public response ratings<br>of <span style='font-family:Rowan'>PIXAR</span>'s films<br><br><span style='font-size:120pt; font-weight:900; font-family:Poppins;'>UP</span> compared to the Critics Choice scores through the years - unless the film is about cars or redheads!"
      ),
      hjust = 0,
      halign = 0.5,
      width = unit(13.2, "lines"),
      size = 5,
      family = "Commissioner",
      fontface = "bold",
      colour = "#ffffff",
      fill = NA,
      box.colour = NA
    ),
    colour = "#56547c",
    sigma = 10
  ) +
  ggfx::with_outer_glow(
    ggtext::geom_textbox(
      data = data.frame(),
      aes(
        x = lubridate::ymd("2012-01-01"),
        y = 55,
        label = "Each balloon represents a Rotten Tomatoes rating for a Pixar film. The dark square represents the Critics Choice rating. <br>**Cars 2** was one of the rare
                             films where the critics enjoyed the film much more than the general public."
      ),
      hjust = 0,
      halign = 0,
      width = unit(9, "lines"),
      size = 4,
      family = "Commissioner",
      colour = "#ffffff",
      fill = NA,
      box.colour = NA
    ),
    colour = "#56547c",
    sigma = 12
  ) +
  ggfx::with_outer_glow(
    ggtext::geom_textbox(
      data = data.frame(),
      aes(
        x = lubridate::ymd("1995-11-22"),
        y = 95,
        label = "This is **Toy Story**. Yes, that was 30 years ago!"
      ),
      hjust = 0.5,
      halign = 0.5,
      width = unit(8, "lines"),
      size = 4,
      family = "Commissioner",
      colour = "#ffffff",
      fill = NA,
      box.colour = NA
    ),
    colour = "#56547c",
    sigma = 12
  ) +
  ggfx::with_outer_glow(
    geom_segment(
      x = lubridate::ymd("1995-11-22"),
      xend = lubridate::ymd("1995-11-22"),
      y = 98,
      yend = 99,
      colour = "#bbced2",
      linewidth = 0.5
    ),
    colour = "#56547c",
    sigma = 10
  ) +
  labs(
    caption = "#TidyTuesday | Dataviz: Cara Thompson, Building Stories with Data LTD | Source: {pixarfilms}"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = grid::linearGradient(c("#c8cce7", "#558fd1")),
      colour = "#efeae6",
      linewidth = 10
    ),
    plot.margin = margin(rep(5, 4)),
    plot.caption = element_text(
      colour = "#56547c",
      margin = margin(0, 0, 20, 0),
      hjust = 0.5,
      family = "Commissioner",
      face = "bold"
    ),
    legend.position = "none"
  )


# Exporting for Making of
ggsave(
  filename = file.path(here::here(
    "making-of/temp",
    paste0("202503_pixar-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
  )),
  dpi = 400,
  width = 6.8,
  height = 10,
  bg = "#ffffff"
)

# Final export
ggsave(
  filename = here::here("plots", "202503_pixar.png"),
  dpi = 400,
  width = 6.8,
  height = 10,
  bg = "#ffffff"
)
