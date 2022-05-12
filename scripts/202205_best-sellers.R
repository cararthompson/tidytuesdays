# Packages!
library(tidyverse)
library(extrafont)
library(lubridate)

# Get data
nyt_titles <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')

nyt_shelves <- nyt_titles %>%
  mutate(decades = case_when(year < 1950 ~ "1930s-40s",
                             between(year, 1950, 1969) ~ "1950s-60s",
                             between(year, 1970, 1989) ~ "1970s-80s",
                             between(year, 1990, 2009) ~ "1990s-2000s",
                             between(year, 2010, 2029) ~ "2010s-20s"),
         first_week = ymd(first_week)) %>%  
  filter(total_weeks > 51)

# Solution from here: https://coolbutuseless.github.io/2019/03/07/custom-axis-breaks-on-facetted-ggplot/
limits_fun <- function(x) {
  if (between(ymd(min(x)), ymd("1930-01-01"), ymd("1949-12-31"))) {
    c(ymd("1930-01-01"), ymd("1949-12-31"))
  } else if (between(ymd(min(x)), ymd("1950-01-01"), ymd("1969-12-31"))) {
    c(ymd("1950-01-01"), ymd("1969-12-31"))
    # Something odd happens if we set the end date here to 1989-12-31
    # It's tied to an early 1990s book but I gave up troubleshooting
  } else if (between(ymd(min(x)), ymd("1970-01-01"), ymd("1989-11-10"))) {
    c(ymd("1970-01-01"), ymd("1989-12-31"))
  } else if (between(ymd(min(x)), ymd("2010-01-01"), ymd("2029-12-31"))) {
    c(ymd("2010-01-01"), ymd("2029-12-31"))
  } else {
    c(ymd("1989-11-11"), ymd("2009-12-31"))
  }
}

## plot
ggplot(nyt_shelves, 
       aes(x = first_week, y = 18-debut_rank, 
           fill = -best_rank,
           width = total_weeks),
       color = "#2C3D4F") +
  geom_bar(stat = "identity", 
           show.legend = F) +
  labs(title = "\nNew York Times Bestsellers",
       subtitle = "\nThese shelves contain all the books 
which remained on the bestseller list for a year
or more. They are arranged from left to right by debut week. 
       
The longer a book was on the list, the thicker its spine. The 
closest it was to Number 1 on its debut week, the taller it is. 
The closest it got to Number 1, the more purple it is.
       
Most books on this list made it to Number 1 at some point.
Different decades had different patterns, with more books in
the 1950s-60s starting lower down on the list. Only one book
in the 1980s remaining on the list for a year or more. The 1990s
and early 2000s saw the longest residencies, with four books 
remaining on the bestseller list for more than three years.

Can you name them?

",
caption = "\n\nDataviz: @cararthompson | #TidyTuesday | Source: Post45Data") +
  scale_fill_gradient2(low = "#165247",
                       high = "#371652",
                       mid = "#2C3D4F",
                       midpoint = -3) +
  facet_wrap(~ decades, ncol = 1, scales = "free_x",
             strip.position = "bottom") +
  scale_x_date(limits = limits_fun) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(colour = "#513420", fill = "#513420"),
        strip.text = element_text(family = "Bellefair", colour = "#e8e8e8", size = 14),
        panel.spacing = unit(1.2, "lines"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Bellefair", hjust = 0.5, size = 32),
        plot.subtitle = element_text(family = "Bellefair", hjust = 0.5, size = 18),
        plot.caption = element_text(family = "Bellefair", hjust = 0.5, size = 10))

# Export to create making-of gif
ggsave(filename = file.path("making-of/temp", 
                            paste0("202205_books-", 
                                   format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
       dpi = 400, width = 7, height = 12, bg = "#e2e2e2")

ggsave(filename = file.path("plots/", "202205_best-sellers.png"), 
       dpi = 400, width = 7, height = 12, bg = "#e2e2e2")

# Answers:
# The 4 books which remained on the list for more than 3 years were:
# - OH, THE PLACES YOU'LL GO!, Dr. SeussDr Seuss, 1990 (178 weeks)
# - THE CELESTINE PROPHECY, James Redfield, 1994 (165 weeks)
# - THE DA VINCI CODE, Dan Brown, 2003 (165 weeks)
# - THE BRIDGES OF MADISON COUNTY, Robert James Waller, 1992 (164 weeks)




