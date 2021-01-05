library(ggplot2)

fireworks <- data.frame(burst   = rep(paste("burst", 1:8, sep = "_"), each = 44),
                        n       = rep(1:44, times = 8),
                        curve   = rep(c(0.2, -0.2, 0.1, -0.2, -0.2, 0.2, -0.1, 0.2), each = 44),
                        x_start = rep(c(-14, -6, -12, -7, 14, 6, 12, 7), each = 44),
                        y_start = rep(c(10, 20, 17, 12, 10, 20, 17, 12), each = 44),
                        x_end   = c(3 * cos(1:44) - 14,
                                    4 * cos(1:44) - 6,
                                    4 * cos(1:44) - 12,
                                    3 * cos(1:44) - 7,
                                    3 * cos(1:44) + 14,
                                    4 * cos(1:44) + 6,
                                    4 * cos(1:44) + 12,
                                    3 * cos(1:44) + 7),
                        y_end   = c(3 * sin(1:44) + 10,
                                    4 * sin(1:44) + 20,
                                    4 * sin(1:44) + 17,
                                    3 * sin(1:44) + 12,
                                    3 * sin(1:44) + 10,
                                    4 * sin(1:44) + 20,
                                    4 * sin(1:44) + 17,
                                    3 * sin(1:44) + 12))

ggplot(data = fireworks) +
  geom_curve(aes(x = -10, y = -7, xend = -14, yend = 10),
             curvature =  0.2, colour = "#878787", linetype = "dotted", size = 1) +
  geom_curve(aes(x = -10, y = -7, xend =  -6, yend = 20),
             curvature = -0.2, colour = "#878787", linetype = "dotted", size = 1) +
  geom_curve(aes(x = -10, y = -7, xend = -12, yend = 17),
             curvature =  0.1, colour = "#878787", linetype = "dotted", size = 1) +
  geom_curve(aes(x = -10, y = -7, xend =  -7, yend = 12),
             curvature = -0.2, colour = "#878787", linetype = "dotted", size = 1) +
  geom_curve(aes(x =  10, y = -7, xend = 14, yend = 10),
             curvature = -0.2, colour = "#878787", linetype = "dotted", size = 1) +
  geom_curve(aes(x =  10, y = -7, xend =  6, yend = 20),
             curvature =  0.2, colour = "#878787", linetype = "dotted", size = 1) +
  geom_curve(aes(x =  10, y = -7, xend = 12, yend = 17),
             curvature = -0.1, colour = "#878787", linetype = "dotted", size = 1) +
  geom_curve(aes(x =  10, y = -7, xend =  7, yend = 12),
             curvature =  0.2, colour = "#878787", linetype = "dotted", size = 1) +
  geom_curve(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, colour = burst),
             curvature = 0.1, linetype = "dashed") +
  geom_point(aes(x = x_end, y = y_end, colour = burst), shape = 8) +
  labs(title = "Happy New Year!") +
  theme(legend.position = "none",
        text = element_text(size = 32),
        plot.title = element_text(hjust = 0.5, face = "bold.italic"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#252525", colour = "#252525")) +
  ggsave("Happy New Year Zoom.png", width = 320, height = 180, units = "mm")
