now <- function(format = "F") {
  #' Return the formatted current date and/or time quicker
  #'
  #' @description When we want the current date and/or time, we need awkward
  #'   and/or verbose syntax to get the correct formatting. This function offers
  #'   some simpler options. Basically just a wrapper around
  #'   `format(Sys.time(), %%)`, but "n" and "t" behave differently.
  #'
  #' @param format "string": Which format to return? See `strptime()`.
  #'
  #'     The options are:
  #'         "n"    "yyyy-mm-dd HH:MM:ss zzz"     e.g. "2023-07-29 15:24:43 AEST
  #'         "F"    "yyyy-mm-dd"                  e.g. "2023-07-29"
  #'         "t"    "HH:MM:ss zzz"                e.g. "15:24:43"
  #'         "Y"    yyyy                          e.g. 2023
  #'         "m"    m                             e.g. 7
  #'         "b"    "Mth"                         e.g. "Jul"
  #'         "B"    "Month"                       e.g. "July"
  #'         "d"    dd                            e.g. 29
  #'         ""     anything else accepted by `strptime()`
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  today <- Sys.time()

  # Format!
  now <- switch(format,
                "n" = today,                                  # "2022-11-23 15:24:43 AEST"
                "F" = format(today, "%F"),                    # "2022-11-23"
                "t" = format(today, "%H:%M:%s"),              # "15:24:43"
                "Y" = format(today, "%Y") |> as.integer(),    # 2022
                "m" = format(today, "%m") |> as.integer(),    # 11
                "b" = format(today, "%b"),                    # "Nov"
                "B" = format(today, "%B"),                    # "November"
                "d" = format(today, "%d") |> as.integer(),    # 23
                format(today, paste0("%", format)))           # See `strptime()`

  return(now)
}
