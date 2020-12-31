library(ggplot2)

fireworks <- data.frame(burst   = rep(paste("burst", 1:4, sep = "_"), each = 44),
                        n       = rep(1:44, times = 4),
                        curve   = rep(c(0.2, -0.2, 0.1, -0.2), each = 44),
                        x_start = rep(c(-4, 4, -2, 3), each = 44),
                        y_start = rep(c(10, 20, 17, 12), each = 44),
                        x_end   = c(3 * cos(1:44) - 4,
                                    4 * cos(1:44) + 4,
                                    4 * cos(1:44) - 2,
                                    3 * cos(1:44) + 3),
                        y_end   = c(3 * sin(1:44) + 10,
                                    4 * sin(1:44) + 20,
                                    4 * sin(1:44) + 17,
                                    3 * sin(1:44) + 12))

ggplot(data = fireworks) +
  geom_curve(aes(x = 0, y = -7, xend = -4, yend = 10),
             curvature =  0.2, colour = "#878787", linetype = "dotted") +
  geom_curve(aes(x = 0, y = -7, xend =  4, yend = 20),
             curvature = -0.2, colour = "#878787", linetype = "dotted") +
  geom_curve(aes(x = 0, y = -7, xend = -2, yend = 17),
             curvature =  0.1, colour = "#878787", linetype = "dotted") +
  geom_curve(aes(x = 0, y = -7, xend =  3, yend = 12),
             curvature = -0.2, colour = "#878787", linetype = "dotted") +
  geom_curve(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, colour = burst),
             curvature = 0.1, linetype = "dashed") +
  geom_point(aes(x = x_end, y = y_end, colour = burst), shape = 8) +
  labs(title = "Happy New Year!") +
  theme(legend.position = "none",
        text = element_text(size = 20),
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
  ggsave("Happy New Year.png", width = 15, height = 20, units = "cm")
