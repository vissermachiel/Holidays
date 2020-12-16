# Load packages
library(ggplot2)
library(png)

# Load image for peak of Christmas tree
my_image <- readPNG("RStudio_logo.png")

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

# Christmas balls
balls <- data.frame(ball = 1:13, #factor(paste0("ball_", letters[1:13])),
                    x = c(-2.5, 2.5, -5, 0, 5, -2.5, 2.5, -5, 0, 5, -2.5, 2.5, 0),
                    y = c(20, 20, 30, 30, 30, 40, 40, 50, 50, 50, 62.5, 62.5, 75),
                    colour = hcl(h = seq(15, 375, length = 13 + 1), c = 100, l = 65)[1:13])

# Plot Christmas tree
p <- ggplot() +
  geom_polygon(data = tree, aes(x = x, y = y, group = part), fill = tree$colour) +
  annotation_raster(my_image, xmin = -1.5, xmax = 1.5, ymin = 92.5, ymax = 107.5) +
  geom_point(data = garlands, aes(x = x, y = y), colour = "powderblue", shape = 8) +
  geom_point(data = balls, aes(x = x, y = y), colour = balls$colour, size = 5) +
  ylim(0, 105) +
  xlab("x-mas") +
  ylab("merr-y") +
  theme(text = element_text(size = 20),
        legend.position = "none")

ggsave("Merry Christmas.png", p, width = 150, height = 150, units = "mm")