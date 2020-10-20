# Crops!

# Load libraries -----
library(tidyverse)
library(ggimage)
library(treemapify)
library(ggtext)
library(ochRe)
library(extrafont)
library(patchwork)

# Get data -----
tt_data <- tidytuesdayR::tt_load(2020, 36)
tidytuesdayR::readme(tt_data)


# Set theme across plots ----
earthCol <-  "#3a2b1f"
accentCol <- "#AEAEAE" # Potato colour

theme_crops <- function() {
  theme_minimal() %+replace%
    theme(plot.background = element_rect(fill = earthCol, colour = earthCol),
          panel.grid = element_line(color = earthCol),
          panel.background = element_rect(fill = earthCol, colour = earthCol),
          legend.background = element_rect(fill = earthCol, colour = earthCol),
          text = element_text(colour = accentCol,
                              family = "EngraversGothic BT"), 
          plot.title = element_text(hjust = 0.5, size = 24), 
          plot.subtitle = element_text(hjust = 0.5, size = 18),
          axis.text = element_text(color = accentCol, size = 14),
          axis.title = element_text(color = accentCol, size = 16),
          strip.text = element_text(colour = accentCol, size = 16,
                                    margin = margin(1,0,0.5,0, "cm")), # facet title
          plot.caption = element_text(hjust = 0.5, size = 14), 
          # Right extra padding to avoid patchwork moving everything to the right
          plot.margin = margin(t = 1, r = 3, b = 1, l = 2, "cm"))
}

# Tree map -----
frenchCrops <- tt_data$key_crop_yields %>%
  filter(Entity == "France") %>%
  # turn into long format
  pivot_longer(cols = 4:last_col(),
               names_to = "crop", 
               values_to = "crop_production") %>% 
  mutate(crop = str_remove_all(crop, " \\(tonnes per hectare\\)")) %>% 
  set_names(nm = names(.) %>% tolower()) %>%
  filter(!is.na(crop_production))

treeMap <- 
  ggplot(filter(frenchCrops,  year %in% c(max(year), min(year))),
         aes(area = crop_production, fill = crop)) +
  geom_treemap(show.legend = F) +
  geom_treemap_text(aes(label=crop, family = "EngraversGothic BT"), 
                    color=earthCol, place = "centre") +
  scale_fill_ochre(palette="mccrea") +
  facet_wrap(~year, ncol = 2) +
  labs(title = "Proportion of Different Crops Produced in France\n1961 vs. 2018",
       subtitle = "\nRice and Peas lost out to Soybeans and Maize") +
  theme_crops()

# Tractor plot ----
tIcon <- "../making-of/temp/tractor.png"

tractors <- data.frame(x = rep(2020.5, 8),
                       y = filter(frenchCrops, year == 2018)$crop_production + 2, # for trajectory alignment
                       crop = filter(frenchCrops, year == 2018)$crop, 
                       image = rep(tIcon, 8))

texts <-
  tibble(
    year = c(2015, 2008),
    crop_production = c(30, 15),
    crop = c("Potatoes", "Potatoes"),
    text = c(
      'Potatoes',
      'Everything else'))

tractorPlot <- ggplot(frenchCrops, 
                      aes(x = year, y = crop_production, colour = crop)) + 
  scale_colour_ochre(palette="mccrea") +
  geom_line(size = 2, linetype = "twodash",
            show.legend = F) +
  geom_image(data = tractors,
             aes(image = image, x = x, y = y, colour = crop), 
             size = 0.08,
             angle = 0) +
  geom_textbox(data = texts,
               aes(year, crop_production, 
                   label = text),
               vjust = 0.5,
               colour = "white",
               box.colour = earthCol,
               size = 5,
               fill = earthCol,
               family = "EngraversGothic BT",
               maxwidth = unit(8, "lines"),
               hjust = .5,
               show.legend = F) +
  annotate("curve", x = 2005, xend = 2001, y = 30, yend = 34, curvature = -.3, 
           size = .75, arrow = arrow(length = unit(2, "mm")), colour = accentCol) +
  annotate("curve", x = 1998, xend = 1992, y = 15, yend = 10, curvature = 0.3, 
           size = .75, arrow = arrow(length = unit(2, "mm")), colour = accentCol) +
  xlab("Year") +
  ylab("Crop production (tonnes per hectare)") +
  labs(title = "\nPotatoes are the steady top of the crops",
       subtitle = "\nFrance has consistently produced 4 times more tonnes of potatoes\nthan the next leading crop since 1961.",
       caption = "
       
  @crthompson | #TidyTuesday | Source: Our World In Data") +
  theme_crops() +
  theme(legend.position = "none")

# Export image ----
# using {patchwork}
p <- treeMap / tractorPlot

ggsave(p, filename = "../plots/202009_crops.png", height = 14, width = 8.5, dpi = 400)
