strt = Sys.time()
suppressPackageStartupMessages(library(data.table))

get_t = function() {
  format(Sys.time() - strt,
         digits = 3,
         units = 's')
}

fread_class = function(f) {
  fread(f,
        colClasses = c(pm25 = "numeric",
                       pm10 = "numeric",
                       t    = "POSIXct"))
}

message(paste0("Setup : ", get_t()))

archived = readLines("data/archived.txt")

fl = list.files("data/pm_logs",
                full.names = TRUE,
                pattern = "tsv.gz")

to_archive = which(fl %notin% archived)

message(paste0("Detect new files: ", get_t()))

if (length(to_archive) > 0) {
  # Avoid re-reading a bunch of individual files each time

  message(paste0("Adding ",
                 length(to_archive),
                 " file(s) to archive."))

  cnd = length(to_archive) > 16 && is.null(mirai::nextget("n"))

  if (cnd) {
    message("...in parallel.")

    library(mirai); daemons(4) # takes about 175ms

    mirai::everywhere(library(data.table))

    dat <- mirai_map(fl[to_archive], fread_class)[]

  } else {

    dat <- lapply(fl[to_archive], fread_class)

  }

  dat <- rbindlist(dat)

  message(paste0("Finished read new files ", get_t()))

  fwrite(dat,
         file = "data/archive.tsv.gz",
         append = TRUE,
         sep = "\t")

  write(fl[to_archive],
        file = "data/archived.txt",
        append = TRUE)

  message(paste0("Added files to archive: ",
                 format(Sys.time() - strt,
                        digits = 3,
                        units = 's')))

}

dat = fread(cmd = "zcat data/archive.tsv.gz",
            colClasses = c(pm25 = "numeric",
                           pm10 = "numeric",
                           t    = "POSIXct"))

message(paste0("Read archive: ",
               format(Sys.time() - strt,
                      digits = 3,
                      units = 's')))

dat[, t := as.POSIXct(t, tz = "EST")]

setnames(dat, new = c("PM2.5", "PM10", "t"))

dat = melt(dat, id.vars = "t")

setorder(dat, t)

# if (interactive()) {
#
#   # PM2.5 vs PM10: ----
#   dat |>
#     collapse::mtt(value = log10(value) + rnorm(nrow(dat), sd = .008)) |>
#     dcast(t ~ variable, drop = TRUE) |>
#     plt(`PM10` ~ `PM2.5`, # 44ms
#         data = _,
#         alpha = .1,
#         pch = 16,
#         cex = .9)
#
#
#   # PM vs all time: ----
#   # plt(value ~ t | variable,
#       data = dat,
#       type = "area",
#       ylim = c(0, 1.02*max(dat$value))) # 109ms
#
#
# }

dat[, c("di", "ti", "gap") := cbind(IDateTime(t),
                                    difftime(t,
                                             shift(t, fill = dat$t[1]),
                                             units = "secs") > 601)]

dat[, `:=`(seg_id = cumsum(gap) + 1,
           last_24h = t > (last(t) - 24*3600) ,
           t2 = as.POSIXct("2025-11-21", tz = "EST") + ti)]

dat[, date_seg := fctr(.GRP, levels = 1:.NGRP), by = c("di", "seg_id")]

message(paste0("Modify archive: ",
               format(Sys.time() - strt,
                      digits = 3,
                      units = 's')))

add_NA_by_seg = \(d) {
  # Insert NAs onto each group by date segment --> break segments
  # see ?lines

  na_df = data.table(t2 = NA_real_,
                     value = NA_real_,
                     last_24h = NA)

  d[,rbindlist(list(.SD, na_df)), by = date_seg]
}

pd = dat[variable == "PM10",
         .(t2, value, date_seg, last_24h, date_seg)] |>
  add_NA_by_seg()

l10 = pd[(last_24h)] |>
  add_NA_by_seg()

pd2 = dat[variable != "PM10",
          .(t2, value, date_seg, last_24h, date_seg)] |>
  add_NA_by_seg()

l2 = pd2[(last_24h)] |>
  add_NA_by_seg()


# top panel ---------------------------------------------------------------

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

message(paste0("Latest measurement at: ",
               tail(dat$t, 1) |> format('%Y-%m-%d %H:%M')))

message(paste0("Next file at: ",
               (tail(dat$t, 1) + 3*3600) |> format('%Y-%m-%d %H:%M')))

message(paste0("Done in: ",
               format(Sys.time() - strt, digits = 3, units = 's')))

