library(lme4)

n_locations <- 4
effect <- c(0.6, 0.8, 0.6, 0.7)
baseline <- 25
trend1 <- c(1.02, 1.02, 1.02, 1.02)
trend2 <- c(1.02, 0.8, 0.8, 1.02)
duration <- 30
intervention <- c(10, 15, 20, 25)

location <- rep(1:n_locations, each = duration)
time <- rep(1:duration, times = n_locations)
target <- rep(baseline, n_locations * duration)

trend <- mapply(c, 
       mapply(rep, x = trend1, times = intervention),
       mapply(rep, x = trend2, times = duration - intervention))
trend <- apply(trend, 2, cumprod)


iv_status <- rep(rep(0:1, time = n_locations), times = c(rbind(intervention, duration - intervention)))
rr <- rep(effect, each = duration)
rr[iv_status == 0] <- 1
target <- target * rr * c(trend)

set.seed(12)

d <- data.frame(
  location = location,
  time = time,
  iv = iv_status,
  outcome = rpois(length(rr), target)
)
d$location_iv = paste0(d$location, '-', d$iv)

m <- glmer(outcome ~ time  + (1 + iv|location), family = 'poisson', data = d)
d$p <- predict(m, newdata = d, type = 'response')
m2 <- glmer(outcome ~ time + (1 + iv|location) + (time|location_iv), family = poisson, data=d)
d$p2 <- predict(m2, newdata = d, type = 'response')

# plot

png('Fig1.png', height = 7, width = 6, units = 'in', res = 300)

layout(matrix(1:8, nrow = 4), widths = c(3, 1.7))
par(mar = c(1, 0, 0, 0), oma = c(5, 5, 0, 0), xpd = NA)
for(i in 1:n_locations) {
  plot(1, type = 'n', xlim = c(0, duration), ylim = c(0, max(d$outcome)), axes = F, xlab = NA, ylab = NA)
  with(d[d$location == i,], {
    rect(intervention[i], 0, duration + 1, max(d$outcome), col = 'grey96', border = NA)
    rect(0, 0, duration + 1, max(d$outcome))
    segments(intervention[i], 0, y1 = max(d$outcome), lty = 2)
    points(time, outcome)
    lines(time, p, lwd = 1, lty  = 3)
    lines(time, p2, lwd = 1, col = 'red', lty = 3)
    if(i == n_locations) {axis(1, pos = 0); title(xlab = 'Time period', line = 3)}
    axis(2, las = 2, pos = 0)
    text(1, max(d$outcome) * 0.9, paste0('Location ', i), font = 2, adj = 0)
  })
}
mtext('Event count', side = 2, outer = T, line = 3, cex = 0.7)
plot(1, type = 'n', xlim = c(0, 10), ylim = c(0, 10), axes = F, xlab = NA, ylab = NA)
rect(0, 0, 10, 10)
ys <- c(3, 7)
text(1, 9, 'Modelled rate', adj = 0, font = 2)
segments(1, ys, x1 = 4, col = c('red', 'black'), lty = 3)
text(5, ys, c('Assuming a step\nand a slope\nchange', 'Assuming a step\nchange only'), adj = 0)

dev.off()
