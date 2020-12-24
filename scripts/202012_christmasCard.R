
library(ggplot2)
library(reshape2)
library(magrittr)
library(gganimate)

# sierpinski code adapted from: ----
# https://stackoverflow.com/questions/21316363/plot-and-fill-chessboard-like-area-and-the-similars-in-r

t.h <- sin(2*pi/6)   # eq triangle unit height

sierpinski <- function(iter=3){
  n <- 2^iter
  points <- plyr::ldply((n-1):0, function(x){
    data.frame(
      y=rep(n-x-1,x)*t.h/n,
      x=seq((from=(0.5/n)+(n-x)*(0.5/n)),by=1/n,length.out=x)
    )  
  })
  
  points$id <- 1:nrow(points)
  
  rbind(
    points,
    points + matrix(c((t.h/n), (-0.5/n), 0),
                    nrow(points), ncol=3, byrow=T),
    points + matrix(c((t.h/n), (0.5/n), 0),
                    nrow(points),3,byrow=T)
  )
}

axiom<-data.frame(x=c(0,0.5,1),y=c(0,t.h,0))

iterations<-6

# Create snowflakes dataframe ----
snowflakes <- data.frame(x_flake = sample(seq(from = -.110, to = 1.110, by = 0.0001), 
                                          size = 1200, replace = F),
                         y_flake = sample(seq(from =  0, to = 1.110, by = 0.0001), 
                                          size = 1200, replace = F)) %>%
  dplyr::arrange(desc(y_flake)) %>%
  dplyr::mutate(flake_group = rep(sort(rep(c(1, 2), 30)), 20),
                x_shadow = x_flake + 0.002,
                y_shadow = y_flake - 0.002)

# Combine and animate ----
ggplot() + 
  theme_void() +
  theme(plot.background = element_rect(fill = "#293647", colour = "#293647")) +
  coord_fixed(ratio=1) +
  geom_polygon(data=axiom,aes(x,y), fill="#82998b") + 
  lapply(1:iterations,function(x){
    geom_polygon(data=sierpinski(x),aes(x,y,group=id), fill="#336947")
  }) +
  geom_point(data = snowflakes,
             aes(x = x_shadow, y = y_shadow), colour = "#1b232e") +
  geom_point(data = snowflakes,
             aes(x = x_flake, y = y_flake), colour = "#d5dce6") +
  geom_hline(yintercept = 0, colour = "#d5dce6") +
  geom_point(data = NULL, aes(x = 0.5, y = 0.87), shape = 8, 
             size = 10, colour = "#cfab1f") +
  transition_time(flake_group) 

# Export 
anim_save(filename = "../plots/202012_christmasCard.gif")

