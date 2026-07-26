# end at 2026-07-25 22:58:54

png("output/toaster.png", width = 960*2,
    height = 720*2, res = 120*2)

par(mfrow = c(2,1),
    mai = c(1.5, 3.2, 2, .5),
    mar = c(1.5, 3.2, 2, .5),
    bty = "n",
    family = "Arial")

grid_h = seq(0,
             10*floor(max(pd$value, na.rm = TRUE) / 10),
             by = 10)

plot(x = pd$t2, y = pd$value,
     col = rgb(0,0,0,.2),
     panel.first = abline(h = grid_h,
                          v = as.POSIXct("2025-11-21 00:00:00 EST") +
                                seq(0,24, by = 6) * 3600,
                          lty = 'dotted',
                          col = "lightgrey"),
     type = 'l', lwd = .7,
     xaxt = "n",
     yaxt = "n",
     xlab = NA, ylab = NA)

abline(v = as.POSIXct("2025-11-21 13:08:54 EST"),
       lty = 2,
       col = 'grey40')

text(x = as.POSIXct("2025-11-21 13:08:54 EST") + 300,
     y = 55, labels = "made toast :)", adj = -.1)

lines(col = "red",
      x = l10$t2,
      y = l10$value,
      lwd = 1.8)

dd = pd[(last_24h), .SD[c(1, .N)]]

points(dd$t2, dd$value,
       col = "red",
       cex = .7,
       pch = 19)

axis(2,
     at = grid_h,
     lwd = 0,
     lwd.ticks = 1,
     cex.axis = .75,
     tcl = .3,
     padj = 1.4)

axis(1,
     at = as.POSIXct("2025-11-21 00:00:00 EST") +
            seq(0,24, by = 6) * 3600,
     labels = c("00:00", "06:00", "12:00", "18:00", "24:00"),
     padj = -2,
     cex.axis = .75,
     lwd = 0)

title(ylab = expression(paste(mu, "g / ", m^3, sep = "")),
      line = 1.7)

title(main = "PM10",
      adj = 0,
      cex.main = .9)

# bottom panel ------------------------------------------------------------

grid_h2 = seq(0,
              5*floor(max(pd2$value, na.rm = TRUE) / 5),
              by = 5)

plot(x = pd2$t2, y = pd2$value,
     col = rgb(0,0,0,.2),
     panel.first = abline(h = grid_h2,
                          v = as.POSIXct("2025-11-21 00:00:00 EST") +
                                seq(0,24, by = 6) * 3600,
                          lty = 'dotted',
                          col = "lightgrey"),
     type = 'l', lwd = .7,
     xaxt = "n",
     yaxt = "n",
     xlab = NA, ylab = NA)

abline(v = as.POSIXct("2025-11-21 13:08:54 EST"),
       lty = 2,
       col = 'grey40')

text(x = as.POSIXct("2025-11-21 13:08:54 EST") + 300,
     y = 55, labels = "used toaster", adj = -.1)


lines(col = "red",
      x = l2$t2,
      y = l2$value,
      lwd = 1.8)

dd = pd2[(last_24h), .SD[c(1, .N)]]

points(dd$t2, dd$value,
       col = "red",
       cex = .7,
       pch = 19)

axis(2,
     lwd = 0,
     at = grid_h2,
     lwd.ticks = 1,
     cex.axis = .75,
     tcl = .3,
     padj = 1.4)

axis(1,
     at = as.POSIXct("2025-11-21 00:00:00 EST") +
            seq(0,24, by = 6) * 3600,
     labels = c("00:00", "06:00", "12:00", "18:00", "24:00"),
     lwd = 0,
     cex.axis = .75,
     padj = -2)

title(ylab = expression(paste(mu, "g / ", m^3, sep = "")),
      line = 1.7)

title(main = "PM2.5",
      adj = 0,
      cex.main = .9)

dev.off()
