strt = Sys.time()
suppressPackageStartupMessages(library(data.table))
# library(tinyplot); tinytheme("minimal")
#
# op = tpar()
#
# tpar(family = "Arial",
#      pch = 19,
#      mar = c(0,0,0,0) + .1)

get_t = \() format(Sys.time() - strt, digits = 3, units = 's')

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

  message(paste0("Adding ", length(to_archive) , " file(s) to archive."))

  if (length(to_archive) > 16 && is.null(mirai::nextget("n")) ) {
    message("...in parallel.")
    library(mirai); daemons(4) # takes about 175ms

    dat <- mirai_map(fl[to_archive], fread_class)[]
  } else {
    dat = lapply(fl[to_archive], fread_class)
  }

  dat <- rbindlist(dat)

  message(paste0("Finished read new files ", get_t()))

  fwrite(dat,
         file = "data/archive.tsv.gz",
         append = TRUE,
         sep = "\t")

  write(fl[to_archive], file = "data/archived.txt", append = TRUE)

  message(paste0("Added files to archive: ",
                 format(Sys.time() - strt,
                        digits = 3,
                        units = 's')))

}

dat = fread(cmd = "zcat data/archive.tsv.gz",
            colClasses = c(pm25 = "numeric",
                           pm10 = "numeric",
                           t    = "POSIXct"))

message(paste0("Read archive: ", format(Sys.time() - strt, digits = 3, units = 's')))

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

# change_date = function(x) {
#   format(x, format = "%y-11-21 %H:%M:%S") |> as.POSIXct()
# }

# dat[, t2 := t]
# dat[, t2 := IDate(t2)]
# dat[, c("di", "ti") := IDateTime(t)]
dat[, c("di", "ti", "gap") := cbind(IDateTime(t),
                                    difftime(t, shift(t, fill = dat$t[1]), units = "secs") > 601)]

# lubridate::date(dat$t2) <- "2025-11-21" # 1ms

# dat$t2 = change_date(dat$t) # 66ms

# dat[, `:=`(d = lubridate::date(t),
#            gap = difftime(t, shift(t, fill = dat$t[1]), units = "secs") > 601)]
# dat[, gap := difftime(t, shift(t, fill = dat$t[1]), units = "secs") > 601]

# dat$d = lubridate::date(dat$t) # 28ms
#
# dat[, gap := difftime(t, shift(t, fill = dat$t[1]), units = "secs") > 601] # 1ms

# uq_seg = unique(dat[,.(di, seg_id)])[,date_seg := .I][]

dat[, `:=`(seg_id = cumsum(gap) + 1,
           last_24h = t > (last(t) - 24*3600) ,
           t2 = as.POSIXct("2025-11-21", tz = "EST") + ti)]

dat[, date_seg := fctr(.GRP, levels = 1:.NGRP), by = c("di", "seg_id")]
# dat[, date_seg := fctr(date_seg)]

# dat = uq_seg[dat, on = c("di", "seg_id")]

# dat[, `:=`(date_seg = paste(di, seg_id, sep = "_"),
#   last_24h = t > (last(t) - 24*3600) ,
#   t2 = as.POSIXct("2025-11-21", tz = "EST") + ti)]


# dat[, date_seg := stringi::stri_paste(d, seg_id, sep = "_")] # 10ms
#
# dat[, last_24h := t > (last(t) - 24*3600) ]

message(paste0("Modify archive: ", format(Sys.time() - strt, digits = 3, units = 's')))

# dat[, c("gap", "seg_id", "date_seg", "last_24h") := {
#   t2 <- change_date(dat$t)
#   d <- lubridate::ymd(as.Date(t, tz = "EST"))
#   gap <-  difftime(t, shift(t, fill = dat$t[1]), units = "secs") > 601
#   seg_id <- cumsum(gap) + 1
#   date_seg <- stringi::stri_paste(d, seg_id, sep = "_")
#   last_24h <- t > (last(t) - 24*3600)
#
#   list(gap, seg_id, date_seg, last_24h)
# } ]

# xax_fn = function(x) format(x, "%H:%M")

# plt(value ~ t2 | date_seg, data = dat, type = "l",
#     facet = ~ variable,
#     facet.args = list(nrow = 2),
#     lwd = 1.5,
#     xaxl = xax_fn,
#     xlab = NA)

# png("output/traces.png",
# w = 960, h = 720, res = 120)

# if (interactive()) {
#   par(mar = c(1,1,1,1),
#       mai = c(1,1,1,1))
#   plt(value ~ t2 | date_seg,
#       data = dat,
#       type = "l",
#       facet = ~ variable,
#       facet.args = list(nrow = 2,
#                         free = TRUE),
#       lwd = 1.0,
#       xaxl = xax_fn,
#       col = "black",
#       alpha = .25,
#       xlab = NA,
#       ylab = NA,
#       legend = FALSE)
#
#   plt(add = TRUE,
#       value ~ t2 | date_seg,
#       facet.args = list(free = TRUE),
#       type = "l",
#       col = "red",
#       lwd = 1.8,
#       data = dat[(last_24h)],
#       facet = ~variable)
#
#   dot_dat = dat[(last_24h), .SD[c(1, .N)], by = variable]
#
#   plt(add = TRUE,
#       col = "red",
#       value ~ t2 | date_seg,
#       facet.args = list(free = TRUE),
#       data = dot_dat,
#       cex = .7,
#       facet = ~variable)
#
#   title(ylab = expression(paste(mu, "g / ", m^3, sep = "")),
#         line = 3.5)
#
# }

# dev.off()

# dat |>
#   ggplot(aes(t2, value)) +
#   geom_line(aes(group = date_seg,
#                 color = factor(d))) +
#   facet_wrap(vars(variable), nrow = 2) +
#   scale_color_manual(values = pals::cols25()) +
#   scale_x_datetime(breaks = seq(as.POSIXct("2025-11-21 00:00:00", tz = "EST"),
#                                 as.POSIXct("2025-11-22 00:00:00", tz = "EST"),
#                                 length.out = 7))

# plt(value ~ t2 | d, data = dat, type = "l",
#     facet = ~ variable,
#     log = "y",
#     facet.args = list(nrow = 2),
#     xaxl = xax_fn)


# base graphics plot ------------------------------------------------------


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
      cex.main = .9)

# bottom panel ------------------------------------------------------------

plot(x = pd2$t2, y = pd2$value,
     col = rgb(0,0,0,.2),
     panel.first = abline(h = seq(0,
                                  5*floor(max(pd2$value, na.rm = TRUE) / 5),
                                  by = 5),
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
      cex.main = .9)

message(paste0("Latest measurement at: ",
               tail(dat$t, 1) |> format('%Y-%m-%d %H:%M')))

message(paste0("Next file at: ",
               (tail(dat$t, 1) + 3*3600) |> format('%Y-%m-%d %H:%M')))

message(paste0("Done in: ",
               format(Sys.time() - strt, digits = 3, units = 's')))

