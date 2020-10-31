library(tidyverse)

ghost <- data.frame(x = seq(-10, 10, 0.01),
                    top = dnorm(x = seq(-10, 10, 0.01), mean = 0, sd = 4),
                    bottom = cos(2 * seq(-10, 10, 0.01)) / 250)
eyes  <- data.frame(x = c(-1, 1), y = 0.0625)
mouth <- data.frame(x = 0, y = 0.0375)

ggplot(data = ghost, aes(x = x)) +
  geom_ribbon(aes(ymin = bottom, ymax = top), fill = "white") +
  geom_line(aes(y = top), size = 2, colour = "black") +
  geom_line(aes(y = bottom), size = 2, colour = "black") +
  geom_point(data = eyes, aes(x = x, y = y), size = 10) +
  geom_point(data = mouth, aes(x = x, y = y), size = 20) +
  labs(title = "Paranormal Distribution", 
       x = "Happy Halloween!", 
       y = "") +
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5))