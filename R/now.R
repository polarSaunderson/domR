now <- function(format = "F") {
  #' Return the current date and/or time in easier formats
  #'
  #' @description When we want the current date and/or time, we need awkward
  #'   and/or verbose syntax to get the correct formatting. This function offers
  #'   some simpler options.
  #'
  #' @param format "string" or numeric: Which format to return? See `strptime()`.
  #'
  #'     The options are:
  #'         "k" (or 1)    "yyyy-mm-dd HH:MM:ss zzz"     e.g. "2023-07-29 15:24:43 AEST
  #'         "F" (or 2)    "yyyy-mm-dd"                  e.g. "2023-07-29"
  #'         "t" (or 3)    "HH:MM:ss zzz"                e.g. "15:24:43"
  #'         "Y" (or 4)    yyyy                          e.g. 2023
  #'         "m" (or 5)    m                             e.g. 7
  #'         "b" (or 6)    "Mth"                         e.g. "Jul"
  #'         "B" (or 7)    "Month"                       e.g. "July"
  #'         "d" (or 8)    dd                            e.g. 29
  #'         ""            anything else accepted by `strptime()`
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  today <- Sys.time()

  # Handle if argument is numeric
  if (is.numeric(format)) {
    format <- switch(format,
                     "k", "F", "t", "Y", "m", "b", "B", "d", format)
  }

  # Format!
  now <- switch(format,
                "k" = today,                                  # "2022-11-23 15:24:43 AEST"
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
