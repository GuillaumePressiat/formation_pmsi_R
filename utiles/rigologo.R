library(scico)
library(ggplot2)
library(ggforce)

cols <- scico(10, palette = 'cork')[c(2,8)]

png('~/Desktop/rigologo_pmeasyr.png', res = 150)
ggplot() +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1.2), col = NA, fill = cols[2], alpha = 0.95) +
  geom_circle(aes(x0 = -1, y0 = 0, r = 1.2), col = NA, fill = cols[1], alpha = 0.95) +
  geom_segment(aes(x = -1, xend = 0.2, y = 0, yend = 0), col = 'grey90', lwd = 3) +
  geom_segment(aes(x = -1, xend = 0.2, y = 0, yend = 0), col = cols[2], lwd = 2) +
  geom_circle(aes(x0 = -0.7, y0 = 0.8, r = 0.1), fill = "grey95", col = 'grey30') +
  geom_text(aes(x = -1.7, y = 0), label = "PMSi", size = 8, col = cols[2]) +
  geom_text(aes(x = 0.75, y = 0), label = "R", size = 8, col = cols[1], fontface = "bold") +
  theme_void() +
  coord_fixed() -> g
print(g)
dev.off()


png('~/Desktop/rigologo_pmeasyr_end.png', res = 150)
ggplot() +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1.2), col = NA, fill = cols[2], alpha = 0.95) +
  geom_circle(aes(x0 = - 1, y0 = 0, r = 1.2), col = NA, fill = cols[1], alpha = 0.95) +
  geom_segment(aes(x = - 1, xend = 0.2, y = 0, yend = 0), col = 'grey90', lwd = 3) +
  geom_segment(aes(x = - 1, xend = 0.2, y = 0, yend = 0), col = cols[2], lwd = 2) +
  geom_circle(aes(x0 = - 0.7, y0 = 0.8, r = 0.1), fill = "grey95", col = 'grey30') +
  geom_text(aes(x = - 0.7, y = 0.8), label = "%>%", size = 1.6, col = 'grey10') +
  geom_text(aes(x = - 1.7, y = 0), label = "PMSi", size = 8, col = cols[2]) +
  geom_text(aes(x = 0.75, y = 0), label = "R", size = 8, col = cols[1], fontface = "bold") +
  theme_void() +
  coord_fixed() -> g
print(g)
dev.off()
