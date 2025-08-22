# Setup ----
library(ggplot2)


# Data Wrangling ----
munros <- tidytuesdayR::last_tuesday() |>
  tidytuesdayR::tt_load()

# For reproducible jitter
set.seed(2007)

munro_df <- munros$scottish_munros |>
  dplyr::select(Name, Height_m, `2021`) |>
  dplyr::filter(`2021` == "Munro") |>
  dplyr::arrange(Height_m) |>
  dplyr::mutate(
    y_spacing = Height_m + 10,
    y_total = cumsum(y_spacing),
    y_coord = y_total %% 18150
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    x_coord = ceiling(y_total / 18150) + sample(runif(100, -0.06, 0.06), 1),
  )

# Plotting ----
munro_plot <- munro_df |>
  ggplot(aes(x = x_coord, y = y_coord)) +

  ggfx::as_reference(
    geom_point(
      aes(size = Height_m^2),
      shape = 17,
      color = "white",
      show.legend = FALSE
    ),
    id = "mask_points"
  ) +
  ggfx::with_mask(
    annotation_raster(
      jpeg::readJPEG(here::here(
        "data/wilhelm-gunkel-Ly4VV6HHT-A-unsplash.jpg"
      )),
      xmin = -Inf,
      ymin = -Inf,
      xmax = Inf,
      ymax = Inf
    ),
    mask = ggfx::ch_alpha("mask_points")
  ) +
  ggiraph::geom_point_interactive(
    # Your visible points
    aes(
      size = Height_m^2,
      data_id = gsub("'", "", Name),
      tooltip = paste0(
        "<b>",
        Name,
        "</b><br>",
        format(janitor::round_half_up(Height_m), big.mark = ","),
        "m"
      )
    ),
    shape = 24,
    colour = "white",
    show.legend = FALSE
  ) +
  labs(
    title = "Onwards and Upwards",
    subtitle = "- The Munros, as classified in 2021 -",
    caption = "Data Visualisation: Cara R Thompson | [Building Stories with Data LTD](www.cararthompson.com)    
       Source: [The Database of British and Irish Hills v18.2](www.hills-database.co.uk) via #TidyTuesday  
       Background photo: [Wilhelm Gunkel](https://unsplash.com/@wilhelmgunkel?utm_content=creditCopyText&utm_medium=referral&utm_source=unsplash) on [Unsplash](https://unsplash.com/photos/a-close-up-of-a-green-and-black-background-Ly4VV6HHT-A?utm_content=creditCopyText&utm_medium=referral&utm_source=unsplash)"
  ) +
  scale_size(range = c(5, 12)) +
  theme_void() +
  theme(
    plot.caption = marquee::element_marquee(
      width = 1,
      hjust = 0.5,
      family = "Charter",
      colour = "#3e3e3e",
      size = 9,
      style = marquee::classic_style(align = "center")
    ),
    plot.title = element_text(
      family = "Charter",
      size = 36,
      hjust = 0.5,
      colour = "#222222"
    ),
    plot.subtitle = element_text(
      family = "Charter",
      size = 18,
      hjust = 0.5,
      margin = margin(9, 0, 18, 0),
      colour = "#3e3e3e"
    ),
    plot.margin = ggplot2::margin(rep(36, 4)),
    plot.background = element_rect(
      colour = "#ffffff",
      fill = "#f8f8f8",
      linewidth = 9
    )
  )

# For the interactive version (the element_marquee caption doesn't render well)
ggiraph::girafe(
  ggobj = munro_plot,
  options = list(
    ggiraph::opts_tooltip(
      css = "background-color:#222222;color:#f8f8f8;padding:7.5px;letter-spacing:0.025em;line-height:1.3;border-radius:5px;font-family:Charter"
    ),
    ggiraph::opts_hover(css = "")
  ),
  height_svg = 10,
  width_svg = 8
)

# Exporting for Making of
ggsave(
  munro_plot,
  filename = file.path(here::here(
    "making-of/temp",
    paste0(
      "202508_munros-",
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      ".png"
    )
  )),
  dpi = 400,
  width = 8,
  height = 10
)

# Final export ----
ggsave(
  munro_plot,
  filename = here::here("plots", "202508_munros.png"),
  dpi = 400,
  width = 8,
  height = 10
)
