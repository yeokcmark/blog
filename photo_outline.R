rm(list = ls())
library(tidyverse)
library(imager) #package for image processing
library(gganimate)

# a low res photo with a "dull" background works best eg: pasport photo
img <-load.image("images/sulking fat tiger.jpg")
#cannyEdges() function to extract outlines of photo
output <- cannyEdges(img, alpha = 0.4)
#alpha controls the threshold adjustment factor (default is 1)
#play around with this factor to get the desired outcome

# the output is an array with 4 dimensions. you only want column 3 and 4
d <-
  output[,,1,1]
coord <-
  which(d, arr.ind = T)# convert to coordianate where T represents a point

num_rows <- nrow(coord)

df <-
  data.frame(x = coord[,1],
             y = coord[,2],
             n = rep((1:10), length.out = num_rows)) # n controls transition to complete image

#write_csv(df, "thankyou.csv") #Save coordinates of your best plot
#df<-read_csv("prof_rohV2_coord.csv")
p <-
  ggplot(data = df,
       aes(x = x,
           y = -y,
           group = n)
       ) +
  geom_point(size = 1,
             color = "white") +
  theme_set(theme_void()) + 
  theme(panel.background = element_rect(fill = 'black'),
        legend.position = "none") +
  transition_time(n) + 
  shadow_mark(color = "white")

animate(p, fps = 20, nframes = 200, end_pause = 60)

tiger_df <- 
  as.data.frame (img, wide = "c") %>% 
  mutate(rgb.val=rgb(c.1,c.2,c.3),
         n = rep(1:20, length.out = 338668))
tiger_p <- 
  ggplot(tiger_df,aes(x,y))+
  geom_point(aes(fill=rgb.val))

+scale_fill_identity()
tiger_p+scale_y_reverse()



##### explore burst fireworks
i <- 10 # number of clusters
s <- 150 # number of sparkles per center
#mean_time <- 1

# generate color table. c controls color, cluster_n is the cluster number
color_table <-
  tibble(cluster_n = 1:i,
         c = randomColor(i),
         #         t = round(rnorm(i, mean = mean_time, sd = 0.5))
  )

# generate 10,000 random dots per cluster

dots <-
  tibble(x = rnorm(n = 10000*i, mean = 0, sd = 20),
         y = rnorm(n = 10000*i, mean = 0, sd = 20)
  )
# s * i gives number of dots per display

sparkles <-
  tibble(x = sample(dots$x, s*i),
         y = sample(dots$y, s*i)
  )

km <- kmeans(sparkles,
             centers = i,
             iter.max = 50,
             nstart = 1)


center <- 
  tibble(c_x = km$centers[,1],
         c_y = km$centers[,2]) %>% 
  mutate(cluster_n = 1:n())


cluster <- 
  tibble(cluster_n = km$cluster)

fireworks <-
  bind_cols(sparkles, cluster) %>% 
  left_join(., color_table, by = "cluster_n") %>% 
  left_join(., center, by = "cluster_n") %>% 
  mutate(distance = sqrt((x - c_x)^2 + (y - c_y)^2)) %>% 
  group_by(cluster_n) %>% 
  arrange(distance) %>% 
  mutate(order = 1:n()) %>% 
  ungroup()

names(fireworks)

fireworks %>%
  
  p <-
  ggplot()+
  geom_point(data = fireworks %>% filter(cluster_n == 1),
             aes(x = x, y = y, color = c), size = 2) +
  geom_point(data = fireworks %>% filter(cluster_n == 2),
             aes(x = x, y = y, color = c), size = 2) +
  geom_point(data = fireworks %>% filter(cluster_n == 3),
             aes(x = x, y = y, color = c), size = 2) +
  geom_point(data = fireworks %>% filter(cluster_n == 4),
             aes(x = x, y = y, color = c), size = 2) +
  geom_point(data = fireworks %>% filter(cluster_n == 5),
             aes(x = x, y = y, color = c), size = 2) +
  geom_point(data = fireworks %>% filter(cluster_n == 6),
             aes(x = x, y = y, color = c), size = 2) +
  geom_point(data = fireworks %>% filter(cluster_n == 7),
             aes(x = x, y = y, color = c), size = 2) +
  geom_point(data = fireworks %>% filter(cluster_n == 8),
             aes(x = x, y = y, color = c), size = 2) +
  geom_point(data = fireworks %>% filter(cluster_n == 9),
             aes(x = x, y = y, color = c), size = 2) +
  geom_point(data = fireworks %>% filter(cluster_n == 10),
             aes(x = x, y = y, color = c), size = 2) +
  theme_set(theme_void()) + 
  theme(panel.background = element_rect(fill = 'black'),
        legend.position = "none")+
  transition_time(order)+
  shadow_mark(alpha = 0.5, size = rel(0.8))

animate(p, fps = 50, nframes = 500)


