
suppressPackageStartupMessages(library(serial))

con_x = serialConnection(name = "sens", port = "ttyUSB0",
		 mode = "9600,n,8,1", buffering = "none",
		 newline = 1,
		 translation = "binary")

cli::cli_alert("Opening connection")

if (!serial::isOpen(con_x)) open(con_x)

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

set_chk = function(cmd) {
    chk_sum = cmd[3:17] |> sapply(raw_to_int) |> sum() |> intToBits()
    chk_byt = chk_sum[1:8] |> packBits("raw")
    cmd[18] = chk_byt
    cmd
}

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
# write.serialConnection(con, work_per_query); Sys.sleep(1); read.serialConnection(con)[1:10]
# aa c5 08 00 0a 00 a0 01 ab ab # "I'm working for 30 seconds out of every 10 minutes"

# TODO: check work period on initialization, message
# TODO: verify check byte of received messages

cli::cli_alert("Closing connection")

close(con_x)
