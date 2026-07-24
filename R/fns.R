
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

