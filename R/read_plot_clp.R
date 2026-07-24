strt = Sys.time()
library(collapse) # The time to load the collapse namespace isn't worth it
library(tinyplot); tinytheme("minimal")

tpar(family = "Arial",
     pch = 19,
     mar = c(0,0,0,0) + .1)

cli::cli_alert_info("Setup : {format(Sys.time() - strt, digits = 3, units = 's')}")

fread_class = function(f) {
  data.table::fread(f,
                    colClasses = c(pm25 = "numeric",
                                   pm10 = "numeric",
                                   t    = "POSIXct"))
}

archived = readLines("data/archived.txt")

fl = list.files("data/pm_logs",
                full.names = TRUE,
                pattern = "tsv.gz")

to_archive = fl %!iin% archived

cli::cli_alert_info("Detect new files: {format(Sys.time() - strt, digits = 3, units = 's')}")

if (length(to_archive) > 0) {
  # Avoid re-reading a bunch of individual files each time

  cli::cli_alert_info("Adding {length(to_archive)} files to archive.")

  if (is.null(mirai::nextget("n")) & length(to_archive) > 7) {
    library(mirai)

    daemons(4)

    dat <- mirai_map(fl[to_archive], fread_class)[]
  } else {
    dat = lapply(fl[to_archive], fread_class)
  }

  dat <- rowbind(dat)

  data.table::fwrite(dat,
                     file = "data/archive.tsv.gz",
                     append = TRUE,
                     sep = "\t")

  write(fl[to_archive], file = "data/archived.txt", append = TRUE)
}

dat = data.table::fread(cmd = "zcat data/archive.tsv.gz",
                        colClasses = c(pm25 = "numeric",
                                       pm10 = "numeric",
                                       t    = "POSIXct"))

cli::cli_alert_info("Read archive: {format(Sys.time() - strt, digits = 3, units = 's')}")

settransform(dat, t = as.POSIXct(t, tz = "EST"))

data.table::setnames(dat, c("PM2.5", "PM10", "t"))

dat = pivot(dat, ids = "t")

data.table::setorderv(dat, "t")

if (interactive()) {

  # PM2.5 vs PM10: ----
  # dat |>
  #   collapse::mtt(value = log10(value) + rnorm(nrow(dat), sd = .008)) |>
  #   dcast(t ~ variable, drop = TRUE) |>
  #   plt(`PM10` ~ `PM2.5`, # 44ms
  #       data = _,
  #       alpha = .1,
  #       pch = 16,
  #       cex = .9)


  # PM vs all time: ----
  # plt(value ~ t | variable,
  #     data = dat,
  #     type = "area",
  #     ylim = c(0, 1.02*max(dat$value))) # 109ms


}

# change_date = function(x) {
#   format(x, format = "%y-11-21 %H:%M:%S") |> as.POSIXct()
# }

settransform(dat, t2 = get_elem(dat, "t"))

lubridate::date(dat$t2) <- "2025-11-21" # 1ms

# dat$t2 = change_date(dat$t) # 66ms

settransform(dat,
             d = lubridate::date(t),
             gap = difftime(t, data.table::shift(t, fill = dat$t[1]), units = "secs") > 601)

settransform(dat,
             seg_id = cumsum(gap) + 1)

settransform(dat,
             date_seg = stringi::stri_paste(d, seg_id, sep = "_"),
             last_24h = t > (data.table::last(t) - 24*3600))

cli::cli_alert_info("modify archive: {format(Sys.time() - strt, digits = 3, units = 's')}")

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

xax_fn = function(x) format(x, "%H:%M")

# plt(value ~ t2 | date_seg, data = dat, type = "l",
#     facet = ~ variable,
#     facet.args = list(nrow = 2),
#     lwd = 1.5,
#     xaxl = xax_fn,
#     xlab = NA)

# png("output/traces.png",
# w = 960, h = 720, res = 120)


if (interactive()) {
  par(mar = c(1,1,1,1),
      mai = c(1,1,1,1))
  plt(value ~ t2 | date_seg, data = dat, type = "l",
      facet = ~ variable,
      facet.args = list(nrow = 2, free = TRUE),
      lwd = 1.5,
      xaxl = xax_fn,
      col = "black",
      alpha = .25,
      xlab = NA,
      ylab = NA,
      legend = FALSE)

  plt(add = TRUE,
      value ~ t2 | date_seg,
      facet.args = list(free = TRUE),
      type = "l",
      col = "red",
      lwd = 1.8,
      data = dat[(last_24h)],
      facet = ~variable)

  dot_dat = dat[(last_24h), .SD[c(1, .N)], by = variable]

  plt(add = TRUE,
      col = "red",
      value ~ t2 | date_seg,
      facet.args = list(free = TRUE),
      data = dot_dat,
      cex = .7,
      facet = ~variable)

  title(ylab = expression(paste(mu, "g / ", m^3, sep = "")),
        line = 3.5)

}

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

cli::cli_alert_info("Latest measurement at: {tail(dat$t, 1) |> format('%Y-%m-%d %H:%M')}")

cli::cli_alert_info("Next file at: {(tail(dat$t, 1) + 3*3600) |> format('%Y-%m-%d %H:%M')}")

cli::cli_alert_info("Done in {format(Sys.time() - strt, digits = 3, units = 's')}")
