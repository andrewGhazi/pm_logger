# Sys.sleep(15)

library(serial)

# TODO: log PM10 too, log time, set/check working period,
# 10m instead of 2m wp, plotting, time logging, writing continuously


# fns ---------------------------------------------------------------------


raw_to_int = function(rw) {
    c(rawToBits(rw), rawToBits(raw(3))) |> packBits('integer')
}

conv_pm2.5 = function(lo_hi_bytes) {
    y = sapply(lo_hi_bytes, raw_to_int)

    ((y[2] * 256) + y[1]) / 10
}

get_lb = function(x, n_lb = 60, max_val = 20, sym = "#") {
    # map a from 0-20 to the appropriate number of pound symbols
    rep(sym, floor(n_lb * x / max_val)) |> paste(collapse="")
}

set_chk = function(cmd) {
    chk_sum = cmd[3:17] |> sapply(raw_to_int) |> sum() |> intToBits()
    chk_byt = chk_sum[1:8] |> packBits("raw")
    cmd[18] = chk_byt
    cmd
}

# initialize connection ---------------------------------------------------

con_x = serialConnection(name = "sens",
                         port = "ttyUSB0",
                         mode = "9600,n,8,1",
                         buffering = "none",
                         newline = 1,
                         translation = "binary")

cli::cli_alert("Opening connection")

if (!serial::isOpen(con_x)) open(con_x)

Sys.sleep(.5)

test_read = read.serialConnection(con_x)

cur_id = test_read[7:8]

set_id = as.raw(c(0xAA, 0xB4, 0x05, raw(10), 0xA0, 0x01, cur_id, 0xA7, 0xAB))
chk_sum = set_id[3:17] |> sapply(raw_to_int) |> sum() |> intToBits()
chk_byt = chk_sum[1:8] |> packBits("raw")

set_id[18] = chk_byt
# ^ cmd to set device ID to A001

# TODO: confirm device ID, only set if necessary

cli::cli_alert("Setting Device ID")

write.serialConnection(con_x, set_id)

query_cmd = as.raw(c(0xAA, 0xB4, 0x04, raw(12), 0xA0, 0x01, raw(1), 0xAB)) |> set_chk()

mode_query = as.raw(c(0xAA, 0xB4, 0x06, raw(12), 0xA0, 0x01, 0x07, 0xAB)) |> set_chk()
# e.g. write.serialConnection(con, mode_query); read.serialConnection(con)[1:10]
# ^ second data byte 3 (5th overall) will be current status: 1 for work or 0 for sleep
# aa c5 06 00 01 00 a0 01 a8 ab # "I'm working"
# aa c5 06 00 00 00 a0 01 a7 ab # "I'm sleeping"

#TODO: check mode on initialization, message

sleep_cmd = mode_query
sleep_cmd[4] = as.raw(0x01)
sleep_cmd = sleep_cmd |> set_chk()
# write.serialConnection(con, sleep_cmd); Sys.sleep(1); read.serialConnection(con)[1:10]
# aa c5 06 01 00 00 a0 01 a8 ff # "I'm now asleep"

wake_cmd = sleep_cmd
wake_cmd[5] = as.raw(0x01)
wake_cmd = wake_cmd |> set_chk()

nmin = 10
cli::cli_alert("Setting working period to {nmin} min")

get_work_per_cmd = \(nmin) {
    as.raw(c(0xAA, 0xB4, 0x08, 0x01,
                            as.raw(nmin), # This byte sets the period.
                            raw(10),
                            0xA0, 0x01, 0x00, 0xAB)) |>
      set_chk()

}

work_per_cmd = get_work_per_cmd(nmin)

write.serialConnection(con_x, work_per_cmd)

work_per_query = work_per_cmd
work_per_query[4:5] = as.raw(0x00)
work_per_query = work_per_query |> set_chk()

write.serialConnection(con_x, work_per_query); Sys.sleep(1); wp_query_res = read.serialConnection(con_x)[1:10]
# aa c5 08 00 0a 00 a0 01 ab ab # "I'm working for 30 seconds out of every 10 minutes"

# TODO: check work period on initialization, message
# TODO: verify check byte of received messages

cli::cli_alert("Closing connection")

close(con_x)

Sys.sleep(1)

# run loop ----------------------------------------------------------------

con = serialConnection(name = "sens", port = "ttyUSB0",
		 mode = "9600,n,8,1", buffering = "none",
		 newline = 1,
		 translation = "binary")

open(con)

write.serialConnection(con, work_per_query)

Sys.sleep(1)

wp_query_res = read.serialConnection(con)[1:10]
# aa c5 08 00 0a 00 a0 01 ab ab # "I'm working for 30 seconds out ofA every 10 minutes"

cli::cli_alert_info("Working period query result: {wp_query_res}")
cli::cli_alert_info("For 10min, should be:        aa, c5, 08, 00, 0a, 00, a0, 01, b3, and ab")
wp = 10

strt_date = Sys.Date()

d = vector(length = 100, "numeric")

res = data.frame(pm25 = d, pm10 = d, t = Sys.time())

empty_df = data.frame(pm25 = d, pm10 = d, t = Sys.time())

cli::cli_alert("Starting loop in 20s...")

Sys.sleep(20)

i = 0

while (TRUE) {
  strt = Sys.time()

  i = i+1

  n_wait = 0

  data_received = FALSE

  while (!data_received) {
    write.serialConnection(con, query_cmd)

    n_wait = n_wait + 1; Sys.sleep(1)

    resp = read.serialConnection(con)[1:10]

    n_wait = n_wait + 1; Sys.sleep(1)

    cnd = resp[2] == as.raw(0xC0)

    if (is.na(resp[2])) {
      cli::cli_alert("Received NA response, retrying...")
    } else if (resp[2] == as.raw(0xC0)) {
      data_received = TRUE
    } else {
      cli::cli_alert("Data not received, retrying...")
    }
  }

  x = resp[3:4] |> conv_pm2.5()

  x2 = resp[5:6] |> conv_pm2.5()

  cli::cli_alert_info("{i} PM2.5:{sprintf('%5.1f', x)} {get_lb(x, sym = '.')}")
  cli::cli_alert_info("{i} PM10 :{sprintf('%5.1f',x2)} {get_lb(x2)}")

  res$pm25[i] = x
  res$pm10[i] = x2
  res$t[i] = Sys.time()

  cnd = i %% 30 == 0

  if (cnd) {
    # Write out current results every 30 observations (3 hours for 10min wp)
    tsv_f = file.path("/home/aircon/pm_logs/", paste0(format(Sys.time(), "%Y_%m_%d_%H:%M:%S"), ".tsv.gz"))

    data.table::fwrite(res[1:i,], file = tsv_f, sep = "\t")

    # Then reset stuff:
    res = empty_df
    i = 0
  }

  t_end = Sys.time()

  delta = difftime(t_end, strt, units = "secs") |> as.numeric()

  Sys.sleep(wp*60 - delta)

}
