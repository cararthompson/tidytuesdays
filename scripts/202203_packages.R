# Packages!
library(tidyverse)
library(extrafont)
library(ggtext)

# Get data
tt_data <- tidytuesdayR::tt_load('2022-03-15')

tidyverse_packages <- c("ggplot2",
                        "dplyr",
                        "tidyr",
                        "readr",
                        "purrr",
                        "tibble",
                        "stringr",
                        "forcats")

# Wrangle
cran_data <- tt_data$cran %>%
  group_by(package) %>%
  mutate(first_latest = case_when(version == min(version) ~ "first",
                                  version == max(version) ~ "latest",
                                  TRUE ~ "middle")) %>%
  filter(first_latest != "middle") %>%
  # keep only packages with 2 or more versions
  filter(length(unique(first_latest)) == 2) %>%
  mutate(include_name = case_when(package %in% tidyverse_packages ~ package,
                                  TRUE ~ "No"),
         total_vignettes = rnw + rmd) %>%
  ungroup()

# Plot it

## colors
background <- "#141227"
non_tv_color <- "#a2a1a8"
tidyverse_colors <- c("#c98434",
                      "#6a9033",
                      "#d0c645",
                      "#b31c26",
                      "#956598",
                      "#2f8a52",
                      "#c33c70",
                      "#0488b8")

## text color
tv_text <- paste0("<span style=\"color:",
                  c("#0488b8", tidyverse_colors), "\">",
                  str_split("tidyverse", "")[[1]],
                  "</span>",
                  collapse = "")

## plot
ggplot(filter(cran_data, include_name == "No"),
       aes(x = first_latest, y = total_vignettes)) +
  geom_line(show.legend = F,
            aes(group = package),
            alpha = 0.2, lineend = "butt",
            color = "white") +
  geom_point(data = cran_data %>% group_by(first_latest, total_vignettes) %>%
               count(), 
             aes(size = n),
             show.legend = F,
             shape = "\u2b22",
             color = non_tv_color) +
  geom_point(data = filter(cran_data, include_name != "No"),
             aes(color = include_name),
             shape = "\u2b22",
             show.legend = F,
             size = 5) +
  geom_line(data = filter(cran_data, include_name != "No"),
            aes(group = include_name, color = include_name),
            show.legend = F,
            size = 1.25,
            alpha = 0.8) +
  scale_size(range = c(2.5, 12)) +
  geom_textbox(data = filter(cran_data, include_name != "No",
                             first_latest == "latest"),
               aes(y = total_vignettes,
                   x = "latest",
                   label = package,
                   color = package),
               show.legend = F,
               box.color = NA,
               fill = "NA",
               size = 4.2,
               halign = c(rep(c(0.1, 0.5, 0.9), 2),
                          0.75, 0.25),
               hjust = 0,
               family = "Segoe UI",
               fontface = "bold") +
  labs(title = paste0("<br>Evolution of the ", tv_text, "<br>"),
       subtitle = paste0("Each line represents a package in the CRAN database, tracing its trajectory from  
the number of vignettes in its first version to the number of vignettes in its latest  
version. The size of each hex, other than the brightly coloured hexes, represents the  
number of packages with the corresponding number of vignettes.  

Somewhat surprisingly, the most common number of vignettes is 0! First and second  
prizes for the most vignettes go to {pla} and {catdata}. The **", tv_text, "** sits squarely  
within the norm, every package having more vignettes now than when it first  
appeared in CRAN. Sweet!"),
x = "",
y = "Number of vignettes",
caption = "#TidyTuesday | Graphic: @cararthompson | Data: Robert Flight") +
  scale_x_discrete(expand = expansion(add = c(0.1, 0.5)),
                   labels = c("First version", "Latest version")) +
  scale_color_manual(values = tidyverse_colors) +
  theme_minimal() +
  theme(text = element_text(family = "Segoe UI", color = "white"),
        plot.title = element_markdown(family = "Segoe UI", face = "bold", 
                                      size = 24, hjust = 0),
        plot.subtitle = element_markdown(size = 14, hjust = 0,
                                         color =  monochromeR::generate_palette("#141227", 
                                                                                modification = "go_lighter",
                                                                                n_colours = 8)[7],
                                         lineheight = 1.3),
        plot.caption = element_text(size = 8, 
                                    colour = monochromeR::generate_palette("#141227", 
                                                                           modification = "go_lighter",
                                                                           n_colours = 8)[7], hjust = 1),
        plot.background = element_rect(colour = background, fill = background),
        panel.background = element_rect(colour = background, fill = background),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid = element_line(color = monochromeR::generate_palette("#141227", 
                                                                        modification = "go_lighter",
                                                                        n_colours = 12)[2]),
        axis.text = element_text(size = 8, color = monochromeR::generate_palette("#141227", 
                                                                                 modification = "go_lighter",
                                                                                 n_colours = 12)[7]),
        axis.title = element_text(size = 10, color = monochromeR::generate_palette("#141227", 
                                                                                   modification = "go_lighter",
                                                                                   n_colours = 12)[7]))



# Export to create making-of gif
ggsave(filename = file.path("making-of/temp", 
                            paste0("202203_packages-", 
                                   format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
       dpi = 400, width = 9, height = 11)

ggsave(filename = file.path("plots/", "202203_packages.png"), 
       dpi = 400, width = 8, height = 10)



