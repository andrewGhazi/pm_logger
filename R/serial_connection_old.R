Sys.sleep(15)

library(serial)

# TODO: log PM10 too, log time, set/check working period,
# 10m instead of 2m wp, plotting, time logging, writing continuously


source("~/fns.R")
source("~/init_con.R")

con = serialConnection(name = "sens", port = "ttyUSB0",
		 mode = "9600,n,8,1", buffering = "none",
		 newline = 1,
		 translation = "binary")

open(con)

n_min = 60 * 24
# n_min = 23*60

wp = 10

strt_date = Sys.Date()

cli::cli_alert("n_min: {n_min}")

cli::cli_alert("wp: {wp}")

n_period = floor(n_min / wp)

cli::cli_alert("n_period: {n_period}")

d = vector(length = n_period, "numeric")

d2 = vector(length = n_period, "numeric")

res = data.frame(pm25 = d, pm10 = d2, t = Sys.time())

cli::cli_alert("Starting loop in 20s...")

Sys.sleep(20)

for (i in 1:n_period) {

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
    #cnd = i %% 3 == 0
    cnd = i %% 12 == 0
    if (cnd) {
        # Write out current results every 12 periods i.e. every hour
        tsv_f = paste0(format(Sys.time(), "%Y_%m_%d_%H:%M:%S"), ".tsv")
        data.table::fwrite(res[1:i,], file = tsv_f, sep = "\t")
    }

    # Would be better to just time how long it takes to get here...
    Sys.sleep(wp*60 - n_wait)

    if (Sys.time() > paste(strt_date + 1, "16:00:00")) {
        # If it's after 4PM, break out of the loop so we can shut down
        # System powers down at 4:05
        break
    }
}

save(res, file = paste0("d_", Sys.time() |> format(), ".RData"))

sleep_goal = c(0xAA, 0xC5, 0x06, raw(3), 0xA0, 0x01, 0xA7, 0xAB) |> as.raw()

write.serialConnection(con_x, sleep_cmd)

Sys.sleep(1)

write.serialConnection(con_x, mode_query)

Sys.sleep(1)

mode_query_resp = read.serialConnection(con_x)[1:10]

if (mode_query_resp == sleep_goal) {
    cli::cli_alert_success("sensor went nighty night")
} else {
    cli::cli_alert_warning("sensor didn't get put to sleep right?!?!")
}

close(con)

cli::cli_alert_success("{Sys.time() |> format()}")

# plot(d)
# system("systemctl poweroff")


