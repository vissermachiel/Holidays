# Load packages
library(dplyr)
library(ggplot2)
library(png)

# Load images for peak of Christmas trees
my_image_1 <- readPNG("RStudio_logo.png")
my_image_2 <- readPNG("GLPG_logo.png")

# Christmas tree
tree <- data.frame(part = rep(c("trunk", "branch1", "branch2", "branch3"), each = 4),
                   colour = rep(c("tan4", "seagreen", "seagreen", "seagreen"), each = 4),
                   x = c(-1.25, 1.25, 1.25, -1.25,
                         -10, 10, 5, -5,
                         -7.5, 7.5, 2.5, -2.5,
                         -5, 5, 0, 0),
                   y = c(0, 0, 10, 10,
                         10, 10, 40, 40,
                         40, 40, 70, 70,
                         70, 70, 100, 100))

# Place tree left and right
tree_left <- tree %>% 
  mutate(x = x - 15)

tree_right <- tree %>% 
  mutate(x = x + 15)

# Gaussian garlands
garlands <- data.frame(x = c(seq(-8.75, 8.75, 0.25),
                             seq(-6.25, 6.25, 0.25),
                             seq(-3.75, 3.75, 0.25)),
                       y = c(15 + 150 * dnorm(seq(-8.75, 8.75, 0.25), mean = 0, sd = 2.5),
                             45 + 100 * dnorm(seq(-6.25, 6.25, 0.25), mean = 0, sd = 2),
                             75 +  50 * dnorm(seq(-3.75, 3.75, 0.25), mean = 0, sd = 1.5)),
                       z = c(rep("garland1", length(seq(-8.75, 8.75, 0.25))), 
                             rep("garland2", length(seq(-6.25, 6.25, 0.25))),
                             rep("garland3", length(seq(-3.75, 3.75, 0.25)))))

# Place garlands left and right
garlands_left <- garlands %>% 
  mutate(x = x - 15)

garlands_right <- garlands %>% 
  mutate(x = x + 15)

# Christmas balls
balls <- data.frame(ball = 1:13, #factor(paste0("ball_", letters[1:13])),
                    x = c(-2.5, 2.5, -5, 0, 5, -2.5, 2.5, -5, 0, 5, -2.5, 2.5, 0),
                    y = c(20, 20, 30, 30, 30, 40, 40, 50, 50, 50, 62.5, 62.5, 75),
                    colour = hcl(h = seq(15, 375, length = 13 + 1), c = 100, l = 65)[1:13])

# Place balls left and right
balls_left <- balls %>% 
  mutate(x = x - 15)

balls_right <- balls %>% 
  mutate(x = x + 15)

# GLPG colours
glpg_green <- rgb(red = 0, green = 77, blue = 67, maxColorValue = 255)
glpg_orange <- rgb(red = 245, green = 136, blue = 0, maxColorValue = 255)

# Plot Christmas tree
p <- ggplot() +
  geom_polygon(data = tree_left, aes(x = x, y = y, group = part), fill = tree$colour) +
  geom_polygon(data = tree_right, aes(x = x, y = y, group = part), fill = tree$colour) +
  annotation_raster(my_image_1, xmin = -1.5 - 15, xmax = 1.5 - 15, ymin = 92.5, ymax = 107.5) +
  annotation_raster(my_image_2, xmin = -1.5 + 15, xmax = 1.5 + 15, ymin = 92.5, ymax = 107.5) +
  geom_point(data = garlands_left, aes(x = x, y = y), colour = "powderblue", shape = 8) +
  geom_point(data = garlands_right, aes(x = x, y = y), colour = "powderblue", shape = 8) +
  geom_point(data = balls_left, aes(x = x, y = y), colour = balls$colour, size = 3) +
  geom_point(data = balls_right, aes(x = x, y = y), colour = balls$colour, size = 3) +
  ylim(0, 105) +
  labs(title = "Merry Christmas!",
       y = "merr-y",
       x = "x-mas") +
  theme(text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, colour = "black", face = "bold.italic"),
        axis.title.y = element_text(colour = glpg_green, face = "bold"),
        axis.title.x = element_text(colour = glpg_orange, face = "bold"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "grey80"),
        legend.position = "none")

ggsave("Merry Christmas Zoom.png", p, width = 160, height = 90, units = "mm")
