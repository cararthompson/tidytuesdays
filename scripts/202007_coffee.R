
# TidyTuesday | Exploring coffee ratings
library(tidytuesdayR)
library(tidyverse)
library(gridExtra)
library(grid)
library(extrafont)
library(ochRe)
library(ggtext)

# Get the data, reshape and do an initial analysis ####
tt_data <- tt_load(2020, week = 28) 

coffee <-  tt_data$coffee_ratings %>%
  filter(total_cup_points > 0,
         cupper_points >= 6) %>%
  gather(key = "attribute",
         value = "rating",
         aroma, flavor, aftertaste, acidity, body, 
         balance, uniformity, clean_cup, sweetness) %>%
  filter(rating >= 6) %>%
  # reading around suggested ratings are typically 6-10 in scoring sheets
  mutate(attribute = as.factor(attribute)) 

## Attribute analysis, using principles of item analysis ####
attributeStats <- tibble(
  attribute = names(by(coffee, coffee$attribute, function(x) {cor((x$total_cup_points - x$rating), x$rating)})),
  citc = by(coffee, coffee$attribute, function(x) {cor((x$total_cup_points - x$rating), x$rating)}),
  # Subjective measure: sweetness correlation nearly 0
  cupperc = by(coffee, coffee$attribute, function(x) {cor(x$cupper_points, x$rating)}),
  # mean score gained from each attribute
  meanscore = by(coffee, coffee$attribute, function(x) {mean(x$rating, na.rm = T)})
)

# Make plots ####
## Coffee theme to apply to both graphs #####
theme_coffee <- function() {
  theme_minimal() %+replace%
    theme(plot.background = element_rect(fill = "#150b0a", colour = "#150b0a"),
          panel.grid = element_line(linetype = "dotted", color = "#3b2e20"),
          panel.background = element_rect(fill = "#150b0a", colour = "#150b0a"),
          legend.background = element_rect(fill = "#150b0a", colour = "#3b2e20"),
          text = element_text(colour = "#B19375",
                              family = "Consolas"), 
          plot.title = element_text(hjust = 0.5, size = 14),
          axis.text = element_text(color = "#B19375", size = 10),
          axis.title = element_text(color = "#B19375", size = 10),
          # axis.ticks = element_blank(),
          legend.justification= "left",
          legend.key = element_rect(fill = "#150b0a", colour =  "#150b0a"))
}

## Annotation text for plot 1 ####
# With gratitude to Cédric Scherer (@CedScherer)'s X-Men plot code
texts <-
  tibble(
    total_cup_points = c(65, 87),
    rating = c(9, 6.5),
    attribute = c("sweetness", "flavour"),
    text = c(
      'The **sweetness** ratings showed the weakest correlation to total scores, with a corrected item-total correlation of r = 0.16. **Uniformity** and **clean cup** showed diminishing returns, with little effect beyond the lowest scoring coffee cups.',
      '**All the other attributes** showed a similar relationship to the total score, with a corrected item-total correlation around 0.6 or above.'),
    vjust = c(.5, .5)
  ) 

## Plot 1: relationships between all attributes and total score ####
total_pts <- ggplot(coffee, aes(x = total_cup_points, y = rating, 
                                colour = attribute)) +
  geom_smooth(level = 0, show.legend = F) +
  geom_count(show.legend = F) +
  geom_count(data = filter(coffee, attribute == "sweetness"),
             show.legend = F) + # to make sweetness stand out
  labs(title = "Sweetness showed the lowest 
correlation to total score...
       ",
       x = "Total cup score",
       y = "Attribute rating") + 
  scale_colour_ochre() +
  geom_textbox(data = texts,
               aes(total_cup_points, rating, 
                   label = text,
                   color = attribute,
                   vjust = vjust),
               size = 3.2,
               fill = "#3b2e20",
               family = "Consolas",
               maxwidth = unit(8, "lines"),
               hjust = .5,
               show.legend = F) +
  theme_coffee()

## Plot 2: mean vs correlation to cupper's overall appreciation ####
corrplot <- ggplot(attributeStats, 
                   aes(x = cupperc, y = meanscore, colour = attribute)) +
  geom_point(size = 4) +
  annotate("curve",
           x = 0.2, xend = .04, y = 8.7, yend = 9.87,
           curvature = .5, size = .5, arrow = arrow(length = unit(3, "mm")),
           colour = "#B19375") +
  geom_textbox(aes(.2, 8.3, 
                   label = "**Sweetness** typically added 10 points to the cup's total score but was unrelated to the cupper's appreciation of the coffee (r = 0.01)."), 
               colour = "#B19375",
               size = 3.2,
               family = "Consolas",
               fill = "#3b2e20",
               maxwidth = unit(14, "lines"),
               hjust = .5,
               show.legend = F) +
  labs(title = "

Boosting the total score
with no relationship to overall appreciation, 
time to scrap the sweetness rating?

       ",
       x = "
lowest <--- strength of relationship to overall appreciation ---> highest",
       y = "lowest  <--- mean rating --->  highest") +
  
  scale_colour_ochre() +
  theme_coffee() +
  theme(legend.title = element_blank()) +
  guides(colour=guide_legend(ncol=2))

# Export plot ####
png(filename = "202007_coffee.png",
    width = 600, height = 800, units = "px", pointsize = 12,
    bg = "#150b0a")

grid.arrange(total_pts, corrplot, 
             top = textGrob("

Riding the sweet coffee wave - but for how long?
 ", 
                            gp=gpar(col="#B19375", fontface = "bold", 
                                    fontfamily = "Consolas", fontsize = 18)),
             bottom = textGrob("

@crthompson | #TidyTuesday | Source: James LeDoux & Coffee Quality Database
                               ", 
                               gp=gpar(col="#B19375", 
                                       fontfamily = "Consolas", fontsize = 10)),
             # output is 3/5ths first image, 2/5ths second image
             layout_matrix = matrix(c(1, 1, 1, 2, 2), 
                                    ncol=1, byrow=TRUE))

dev.off()

