# Remove all objects in workspace ----
rm(list = ls())
#setwd("C:/Users/User/Dropbox/Papa Work/Data Science/R/SMU/Readings")

# Please import necessary libraries ----
#pacman::p_unload("all")

#### Set up dependencies (Activate necessary packages) ----
pacman::p_load(tidyverse, lubridate,glue, forcats, knitr, rmarkdown,
               scales, gridExtra, ggthemes, ggrepel, cowplot, magick, plotrix, 
               patchwork, hexbin,
               data.table, styler, DT, blogdown,
               quantmod, bizdays, roptions, plotly,
               gtrendsR,rvest, Rcrawler, RCurl, httr2, crul, jsonlite, RSelenium, httr,
               mapdata, maps, ggmap,
               tidytext, wordcloud2, igraph, ggraph, textdata, tm, haven, readxl,
               jtools, huxtable, broom, modelr, skimr, psych, Hmisc, texreg, GGally,
               randomForest, gvlma, ggfortify, sandwich, car, ggstance, broom.mixed,
               interactions, distill, ggforce, gganimate
)


theme_set(theme_economist())
sessionInfo()

body <- 
  tibble(x = c(0,0,0),
         r = c(1,2,3), 
         y = c(11,8,3)
         )
face <-
  tibble(
    x = c(-0.5,0,0.5),
    y = c(11.25,11,11.25),
    type = c("1", "2", "1"),
    color = c("black", "tomato3", "black")
    )
left_arm <-
  tibble(
    x = c(-2,-3),
    y = c(8,9.25)
  )
right_arm <-
  tibble(
    x = c(2,3), 
    y = c(8,9.25)
  )
scarf <-
  tibble(
    x = c(0, -0.75, 0, 0.75, 0),
    y = c(10, 7.5, 6.5, 7.5, 10)
  )
dots_on_scarf <-
  tibble(
    x = c(0,0,0),
    y = c(8.5,8,7.5),
    type = c("23", "23", "23"),
    color = c("white", "green", "yellow")
  )
hat <-
  tibble(
    x = c(-0.75, 1, 0.5),
    y = c(12, 11.75, 13)
  )

## Falling snow

plot_window <- tibble(
  x = c(-4, 4),
  y = c(0, 13.5)
)

snow <- tibble(
  x = sample(seq(plot_window$x[1],plot_window$x[2], length.out = 1000), 
             500, replace = TRUE),
  y = sample(seq(plot_window$y[1],plot_window$y[2]+2, length.out = 1000), 
             500, replace = TRUE),
  id = 1:500
) 

###


p_snowman <-
  ggplot()+
  theme_void()+
  geom_circle(data = body, # plot body
              aes(x0 = x, y0 = y, r =r),
              fill = "#FAF9F6"
              )+
  geom_point(data = face, show.legend = FALSE, #plot face
             aes(x = x,
                 y = y,
                 shape = type,
                 color = color)
             )+
  geom_arc(aes(x0 = 0,y0 = 11, start = 2.1, end = 4.2, r = 0.5))+
  geom_line(data = left_arm, # plot left arm
            lineend = "round",
            size = 1.5,
            aes(x = x, y = y))+
  geom_line(data = right_arm, # plot right arm
            lineend = "round",
            size = 1.5,
            aes(x = x, y = y))+
  geom_polygon(data = scarf, fill = "red", #plot the red tie
               aes(x = x,
                   y = y))+
  geom_point(data = dots_on_scarf, show.legend = FALSE, # plot dots on tie
             aes(x=x, y=y, shape = type, color = color),
             size = 3)+
  geom_polygon(data = hat, show.legend = FALSE, # plot the beanie hat
               aes(x = x,
                   y = y),
               fill = "dodgerblue")+
  theme(panel.background = element_rect(fill = "#fefef9"),
        panel.grid.major = element_blank())+
  geom_point(data=snow, aes(group=id, x = x, y = y), 
             colour="#a3a3a3", shape=8, size=3) +
  transition_time(id)+
  coord_fixed()
p_snowman



