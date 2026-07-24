

# tpar(op)

pd = dat[variable == "PM10", .(t2, value, date_seg, last_24h,
                               date_seg)][,rbindlist(list(.SD, data.table(t2 = NA_real_,
                                                                          value = NA_real_,
                                                                          last_24h = NA))), by = date_seg]
# pd[,rbind(.SD, data.table(t2 = NA, value = NA)), by = date_seg]
# Insert NAs onto each group by date segment --> break segments
# see ?lines

l10 = pd[(last_24h)][,rbindlist(list(.SD, data.table(t2 = NA_real_,
                                                     value = NA_real_,
                                                     last_24h = TRUE))), by = date_seg]


pd2 = dat[variable != "PM10", .(t2, value, date_seg, last_24h,
                                date_seg)][,rbindlist(list(.SD, data.table(t2 = NA_real_,
                                                                           value = NA_real_,
                                                                           last_24h = NA))), by = date_seg]

l2 = pd2[(last_24h)][,rbindlist(list(.SD, data.table(t2 = NA_real_,
                                                     value = NA_real_,
                                                     last_24h = TRUE))), by = date_seg]


par(mfrow = c(2,1),
    mai = c(1.5, 3.2, 2, .5),
    mar = c(1.5, 3.2, 2, .5),
    bty = "n",
    family = "Arial")


# top panel ---------------------------------------------------------------

plot(x = pd$t2, y = pd$value,
     col = rgb(0,0,0,.2),
     panel.first = abline(h = seq(0,
                                  10*floor(max(pd$value, na.rm = TRUE) / 10),
                                  by = 10),
                          v = as.POSIXct("2025-11-21 00:00:00 EST") + seq(0,24, by = 6) * 3600,
                          lty = 'dotted',
                          col = "lightgrey"),
     type = 'l', lwd = .7,
     xaxt = "n",
     yaxt = "n",
     xlab = NA, ylab = NA)

lines(col = "red",
      x = l10$t2,
      y = l10$value,
      lwd = 1.8)

dd = pd[(last_24h), .SD[c(1, .N)]]

points(dd$t2, dd$value,
       col = "red",
       cex = .7,
       pch = 19)

axis(2, lwd = 0, lwd.ticks = 1, cex.axis = .85,
     tcl = .3,
     padj = 1.2)

axis(1,
     at = as.POSIXct("2025-11-21 00:00:00 EST") + seq(0,24, by = 6) * 3600,
     labels = c("00:00", "06:00", "12:00", "18:00", "24:00"),
     padj = -1.5,
     lwd = 0)

title(ylab = expression(paste(mu, "g / ", m^3, sep = "")),
      line = 1.7)

title(main = "PM10",
      adj = 0,
      cex.main = .85)

# bottom panel ------------------------------------------------------------

plot(x = pd2$t2, y = pd2$value,
     col = rgb(0,0,0,.2),
     panel.first = abline(h = seq(0,
                                  10*floor(max(pd2$value, na.rm = TRUE) / 10), by = 5),
                          v = as.POSIXct("2025-11-21 00:00:00 EST") + seq(0,24, by = 6) * 3600,
                          lty = 'dotted',
                          col = "lightgrey"),
     type = 'l', lwd = .7,
     xaxt = "n",
     yaxt = "n",
     xlab = NA, ylab = NA)

lines(col = "red",
      x = l2$t2,
      y = l2$value,
      lwd = 1.8)

dd = pd2[(last_24h), .SD[c(1, .N)]]

points(dd$t2, dd$value,
       col = "red",
       cex = .7,
       pch = 19)

axis(2, lwd = 0, lwd.ticks = 1, cex.axis = .85,
     tcl = .3,
     padj = 1.2)

axis(1,
     at = as.POSIXct("2025-11-21 00:00:00 EST") + seq(0,24, by = 6) * 3600,
     labels = c("00:00", "06:00", "12:00", "18:00", "24:00"),
     lwd = 0,
     padj = -1.5)

title(ylab = expression(paste(mu, "g / ", m^3, sep = "")),
      line = 1.7)

title(main = "PM2.5",
      adj = 0,
      cex.main = .85)

