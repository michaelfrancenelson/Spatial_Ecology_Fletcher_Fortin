
require(spatstat)
win_10 = owin(xrange = c(0, 10), yrange = c(0, 10))
ppp1 = rpoispp(200, win = win_10, nsim = 1)
ppp1

x_normalized = ppp1$x / max(ppp1$x)
y_normalized = ppp1$y / max(ppp1$y)



# png("cool_figure.png", 
#     width = 2500, 
#     height = 2500, res = 300)

tiff("cool_figure.tiff", 
    width = 2500, 
    height = 2500, res = 100)

par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))


# plot(
#   ppp1$x, ppp1$y, 
#   pch = "M",
#   cex = 2.02,
#   xaxs = "i", 
#   yaxs = "i",
#   col = rgb(
#     r = 0,
#     g = 0,
#     b = 0,
#     alpha = 0.9 *  (ppp1$y * ppp1$x) %% 1.0))
# 
# points(
#   ppp1$x, ppp1$y, 
#   pch = "M",
#   cex = 2.0,
#   xaxs = "i", 
#   yaxs = "i",
#   col = "white")


par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
plot(
  ppp1$x, ppp1$y, 
  pch = "M",
  cex = 2,
  xaxs = "i", 
  yaxs = "i",
  col = rgb(
    r = 0.5 * sin(ppp1$x) * cos(ppp1$y) + 0.5, 
    g = 0.5 * (cos(ppp1$y) * (0.5 * sin(ppp1$y)) ^ 2) + 0.5, 
    b = y_normalized,
    alpha = 0.1 *  (ppp1$y * ppp1$x) %% 1.0))
box()
dev.off()
