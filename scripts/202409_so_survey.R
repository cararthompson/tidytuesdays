
# Setup
library(tidyverse)

tt_data <- tidytuesdayR::tt_load(tidytuesdayR::last_tuesday())

tt_data$qname_levels_single_response_crosswalk |> 
  # The order of the answers is alphabetical, not ordinal!
  filter(qname == "so_visit_freq")

# Get the wording of the question
tt_data$stackoverflow_survey_questions |>
  filter(qname == "so_visit_freq")


# So colours
so_palette <- list(orange = "#f48024",
                   black = "#222426",
                   grey = "#bcbbbb")

freq_data <- tt_data$stackoverflow_survey_single_response |> 
  group_by(so_visit_freq) |>
  tally() |>
  rename(level = so_visit_freq) |>
  left_join(tt_data$qname_levels_single_response_crosswalk |> 
              # The order of the answers is alphabetical, not ordinal!
              filter(qname == "so_visit_freq")) |>
  replace_na(list(label = "It's rude to ask! (NA)")) |>
  mutate(label = factor(label, levels = c("Less than once per month or monthly",
                                          "A few times per month or weekly",
                                          "A few times per week",
                                          "Daily or almost daily",
                                          "Multiple times per day",
                                          "It's rude to ask! (NA)"),
                        ordered = TRUE),
         fill = case_when(label %in% c("Less than once per month or monthly",
                                       "A few times per week",
                                       "Multiple times per day") ~ so_palette$grey,
                          label == "It's rude to ask! (NA)" ~ so_palette$orange,
                          TRUE ~ "#000000"),
         percentage = janitor::round_half_up(n / nrow(tt_data$stackoverflow_survey_single_response) * 100)) |>
  arrange(label) |>
  uncount(percentage) |>
  select(fill)

# Thanks Nicola Rennie! https://nrennie.rbind.io/blog/sketchy-waffle-charts-r/
make_square <- function(x0, y0, width = 1) {
  sf::st_polygon(
    list(
      cbind(
        c(x0, x0 + width, x0 + width, x0, x0),
        c(y0, y0, y0 + width, y0 + width, y0)
      )
    )
  )
}

poly_list <- purrr::map2(
  .x = rep(1:5, times = 20),
  .y = rep(1:20, each = 5),
  .f = ~ make_square(.x, .y, width = 0.8)
)

plot_sf <- sf::st_sf(freq_data, geometry = poly_list)

roughsf::roughsf(
  plot_sf,
  width = 300,
  height = 700,
)

# Here I exported the plot via the RStudio viewer and added the annotations in Figma.
