# Load packages
library(dplyr)
library(ggplot2)
library(MASS)
library(png)

# Birthday boy
name <- "Machiel"
age <- 30

# Load image of birthday boy
bdayboy <- png::readPNG("bdayboy.png")

# Simulate multivariate normal distribution for balloon
set.seed(12345)
balloon_0 <- MASS::mvrnorm(n = 10000, mu = c(0, 0), Sigma = matrix(c(0.1, 0, 0, 1), ncol = 2))

# Function to rotate balloons
Rotate <- function(x, angle) {
  # Function to rotate a data matrix
  # Input:
  #   x     = data matrix to be rotated [n, 2]
  #   angle = rotation angle in radian
  # Output:
  #   x_rot = rotated data matrix
  rotation_matrix <- matrix(c(cos(angle), -sin(angle), 
                              sin(angle), cos(angle)), ncol = 2, byrow = TRUE)
  x_rot <- t(rotation_matrix %*% t(x))
  return(x_rot)
}

# Rotation angles from 0 to 45 degrees
theta <- c(0, seq(pi/4/45, pi/4, length.out = 45))

# Create rotated balloons
balloon_1 <- Rotate(x = balloon_0, angle = theta[6])
balloon_1[, 1] <- balloon_1[, 1] - 1

balloon_2 <- Rotate(x = balloon_0, angle = -theta[6])
balloon_2[, 1] <- balloon_2[, 1] + 1

balloon_3 <- Rotate(x = balloon_0, angle = theta[4])
balloon_3[, 1] <- balloon_3[, 1] - 0.5
balloon_3[, 2] <- balloon_3[, 2] + 2

balloon_4 <- Rotate(x = balloon_0, angle = -theta[4])
balloon_4[, 1] <- balloon_4[, 1] + 0.5
balloon_4[, 2] <- balloon_4[, 2] + 2

# Put balloon_0 last to make it appear in front of others
balloons_matrix <- rbind(balloon_1, balloon_2, balloon_3, balloon_4, balloon_0)

# Put all balloons in one dataframe
balloons_df <- data.frame(x = balloons_matrix[, 1],
                          y = balloons_matrix[, 2],
                          balloon = c(rep("balloon 1", 10000), 
                                      rep("balloon 2", 10000), 
                                      rep("balloon 3", 10000), 
                                      rep("balloon 4", 10000), 
                                      rep("balloon 5", 10000)))

# Place balloons left and right
balloons_df_left <- balloons_df %>% 
  mutate(x = x - 4 + age)

balloons_df_right <- balloons_df %>% 
  mutate(x = x + 4 + age)

# GLPG colours
glpg_green <- rgb(red = 0, green = 77, blue = 67, maxColorValue = 255)
glpg_orange <- rgb(red = 245, green = 136, blue = 0, maxColorValue = 255)

# Plot balloons around birthday boy
p <- ggplot() +
  geom_segment(aes(x = age + c(-4, -4, -4, -4, -4, 4, 4, 4, 4, 4),
                   y = c(-5, -5, -5, -5, -5, -5, -5, -5, -5, -5),
                   xend = age + c(-4, -5, -3, -4.5, -3.5, 4, 3, 5, 3.5, 4.5),
                   yend = c(0, 0, 0, 2, 2, 0, 0, 0, 2, 2))) +
  stat_ellipse(data = balloons_df_left, aes(x = x, y = y, fill = balloon), colour = "black",
               geom = "polygon", show.legend = FALSE) +
  stat_ellipse(data = balloons_df_right, aes(x = x, y = y, fill = balloon), colour = "black",
               geom = "polygon", show.legend = FALSE) +
  annotate("text", x = c(age - 4, age + 4), y = c(0, 0), label = age, size = 10) +
  annotation_raster(bdayboy, xmin = age - 1.45, xmax = age + 1.45, ymin = -6, ymax = 7) +
  scale_x_continuous(breaks = seq(age - 6, age + 6, 2)) +
  ylim(-6, 7) +
  labs(title = paste0("Fijne verjaardag, ", name, "!"),
       y = "Happy",
       x = "Birthday") +
  theme(text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, colour = "black", face = "bold.italic"),
        axis.title.y = element_text(colour = glpg_green, face = "bold"),
        axis.title.x = element_text(colour = glpg_orange, face = "bold"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = "grey80"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

ggsave("Happy Birthday.png", p, width = 160, height = 90, units = "mm")
