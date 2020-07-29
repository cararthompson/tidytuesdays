# Penguins!

library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(extrafont)

## Get the data ####
tt_data <- tidytuesdayR::tt_load(2020, 31)
readme(tt_data)

# Remove NAs on variables of interest
penguins <- tt_data$penguins %>%
  filter(!is.na(flipper_length_mm),
         !is.na(bill_length_mm),
         !is.na(sex))

## Do some stats ####
# Quick look at correlations
cors <- penguins %>% 
  group_by(species) %>%
  summarise(
    n = n(),
    fl_b = cor.test(bill_length_mm, flipper_length_mm)$p.value,
    fl_w = cor.test(body_mass_g, flipper_length_mm)$p.value,
    b_w = cor.test(body_mass_g, bill_length_mm)$p.value
  )
# Fit decision tree model
modFit <- caret::train(species ~ flipper_length_mm+bill_length_mm, 
                       data = penguins, method = "rpart")
# Visualise the model and check accuracy
rattle::fancyRpartPlot(modFit$finalModel)
confusionMatrix(as.factor(penguins$species), 
                predict(modFit, penguins))
# Allow plotting of prediction accuracy
penguins <- penguins %>%
  mutate(misclass = ifelse(species == predict(modFit, penguins),
                           "Correctly classified",
                           "Incorrectly classified"))

# Find out more about the outlier penguin - N72A2
outlier <- tt_data$penguins_raw %>%
  filter(grepl("Adelie", Species)) %>%
  # two stages to get the max within the subset filtering
  filter(`Flipper Length (mm)` == max(`Flipper Length (mm)`, na.rm = T))


## Plot it ####
# Aesthetics
penguin_hues <- c("#d2dbe4", "#8a5d24", "#646376", "#192029", "#acb3bf", "#596e94")

theme_penguin <- function() {
  theme_minimal() %+replace%
    theme(plot.background = element_rect(fill = penguin_hues[4], 
                                         colour = penguin_hues[4]),
          panel.grid = element_line(color = penguin_hues[4]),
          panel.background = element_rect(fill = penguin_hues[4], 
                                          colour = penguin_hues[4]),
          text = element_text(colour = penguin_hues[5], family = "Segoe UI"), 
          plot.title = element_text(family = "Georgia", 
                                    colour = penguin_hues[6], 
                                    hjust = 0, size = 20, face = "bold"),
          axis.text = element_text(size = 10),
          plot.subtitle = element_text(hjust = 0, size = 13, lineheight = 1),
          axis.title = element_text(size = 10),
          axis.ticks = element_blank(),
          legend.position = "top",
          legend.justification = "left",
          legend.direction = "horizontal",
          legend.box.spacing = unit(0, "cm"),
          legend.text = element_text(size = 11))
}

# Annotation texts
texts <- tibble(
  tibble(
    flipper_length_mm = c(218, 220, 179.5, 171.2),
    bill_length_mm = c(57, 37, 49.2, 40.2),
    text = c(
      "This little guy (\"N72A2\", from Torgersen) was the only penguin to defy the model on both counts - good effort, \"N72A2\"!",
      "Are you above this line? You're probably a Gentoo!",
      "Under that line and to the right of this one? Probably Chinstrap!",
      "Under and to the left? \nProbably Adélie!"),
    halign = c(.5, .5, 0, 1)
  ) 
)

# Plot!
ggplot(penguins, aes(x = bill_length_mm, y = flipper_length_mm, 
                     colour = species)) +
  geom_point(aes(size = body_mass_g, shape = misclass), 
             alpha = 0.7) +
  geom_hline(yintercept = modFit$finalModel$splits[1, c("index")],
             colour = penguin_hues[6], linetype = 5,
             size = 1) +
  geom_segment(x = modFit$finalModel$splits[4, c("index")], 
               xend = modFit$finalModel$splits[4, c("index")],
               y = -Inf,
               yend = modFit$finalModel$splits[1, c("index")],
               colour = penguin_hues[6],
               linetype = 5,
               size = 1) +
  geom_textbox(data = texts,
               aes(bill_length_mm, flipper_length_mm, 
                   label = text,
                   halign = halign),
               colour = penguin_hues[5],
               box.colour = NA,
               size = 4,
               fill = NA,
               family = "Segoe UI",
               maxwidth = unit(7.5, "lines"),
               vjust = .5,
               show.legend = F) +
  scale_colour_manual(name = "", 
                      values = c(penguin_hues[1:3]),
                      labels = c("Adélie", "Chinstrap", "Gentoo"),
                      guide = "topleft") +
  scale_shape(guide = "none") +
  scale_size(guide = "none") +
  annotate("curve", x = 37, xend = 37, y = modFit$finalModel$splits[1, c("index")], 
           yend = 216, curvature = 0, size = 2, arrow = arrow(length = unit(2, "mm")), 
           colour = penguin_hues[6]) +
  annotate("curve", x = modFit$finalModel$splits[4, c("index")], 
           xend = 42.7, y = 171.2, yend = 171.2, curvature = 0,
           size = 2, arrow = arrow(length = unit(2, "mm")), colour = penguin_hues[6]) +
  annotate("curve", x = modFit$finalModel$splits[4, c("index")], 
           xend = 46.5, y = 180.5, yend = 180.5, curvature = 0,
           size = 2, arrow = arrow(length = unit(3, "mm")), colour = penguin_hues[6]) +
  annotate("curve", x = 54.5, xend = 44.4, y = 223, yend = 211, curvature = 0.2,
           size = .5, arrow = arrow(length = unit(2, "mm")), colour = penguin_hues[1]) +
  guides(colour = guide_legend(title = "Species", title.position = "top")) +
  labs(title = "
Perfectly Proportional Penguins",
       subtitle = "  
Each dot represents a penguin. Across all three species, the longer the penguins' flipper, the 
longer their bills also. Their weight, represented by the size of the dot, was also correlated 
to their flipper and bill lengths. All things in perfect proportion! 
       
And these proportions are so consistent within species that we can fit a simple decision tree 
model to predict Species from Flipper length and Bill length alone, which correctly predicts 
the species of nearly 95% of the penguins! Those who were misclassified by the model are 
represented by triangles. 
       ",
       x = "Bill length (mm)",
       y = "Flipper length (mm)",
       caption = "#TidyTuesday | Graphic: @crthompson 
Source: Horst AM, Hill AP, Gorman KB (2020). palmerpenguins: Palmer Archipelago (Antarctica)
  penguin data. R package version 0.1.0. https://allisonhorst.github.io/palmerpenguins/") +
  theme_penguin()

# Export to create making-of gif
ggsave(filename = file.path("../making-of/temp", paste0("202007d_penguins-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
       dpi = 400, width = 10, height = 10)

# Export final plot
ggsave(filename = "../plots/202007d_penguins.png", dpi = 400, width = 10, height = 10)
