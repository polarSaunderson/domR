##
sift <- function(x, condition, inConditions = NA, noWarning = FALSE){
  #' Sift through a dataset and return only data matching a defined condition.
  #'
  #' @description Used to return only the rows of a data.frame (or matrix) which
  #'   match a certain condition. An example would be to return all the rows
  #'   from a matrix where the column containing the "January" value is greater
  #'   than "x". See the examples. This function is simply indexing into the
  #'   data, but wrapping it up in a simpler, more approachable syntax (to me!).
  #'   For multiple conditions, just chain the `sift()` calls together (see
  #'   example x4).
  #'
  #' @param x Which data needs to be sifted through?
  #' @param condition "string": Which condition should be met? There are 3
  #'     necessary parts to this defintion, which must each be separated with a
  #'     single space:
  #'         * ColumnName
  #'         * Operator
  #'         * Value
  #'
  #'     ColumnName must match the !Exact! name of a column in x, or be
  #'     "rownames".
  #'
  #'     The possible operators are:
  #'         * ">"                   Greater than
  #'         * "<"                   Less than
  #'         * "in"                  in a range / vector
  #'         * "!in" or "notIn"      not in a range / vector
  #'         * "="                   equals / the same as
  #'         * "=!"                  doesn't equal / is different to
  #'
  #'     If the "in" or "!in" Value is more than just an "a:b" vector, it is
  #'     necessary to supply the vector via the "inConditions" argument, and use
  #'     "ColumnName in vector", or "ColumnName !in vector" as the condition.
  #'     See the examples.
  #' @param inConditions vector: a vector of values to look for; requires the
  #'   condition to be "ColumnName in vector" or "ColumnName !in vector".
  #' @param noWarning binary: TRUE suppresses a warning when no data is returned
  #'   by the condition.
  #'
  #' @examples
  #'     x  <- data.frame("Year" = 1961:2000,
  #'                      "Melt" = sample(0:100, 40))
  #'     x1 <- sift(x, "Year > 1978")
  #'     x2 <- sift(x, "Year in 1978:1984")
  #'     x3 <- sift(x, "Year in vector", c(1978:1984, 1987, 1989))
  #'     x4 <- sift(x, "Melt > 50") |> sift("Year < 1980")
  #'
  #' @export

  # Code ----------------------------------------------------------------------!
  # Prep condition
  condition <- strsplit(condition, " ") |> unlist()
  if (is.na(inConditions[1])) {
    if ("" %in% condition || length(condition) != 3) {
      stopMessage <- paste0("\nSomething went wrong! \n",
                            "\nThe condition needs 3 parts: ",
                            "a ColumnName, an Operator, and a Value, ",
                            "with a single space between each. \n",
                            "e.g. 'Year > 2000'\n")
      stop(stopMessage)
    }
  }

  # We can either give a column name, or a column index
  colNames      <- colnames(x)

  # If no column names, use index
  if (is.null(colNames)) {
    colNumber <- as.numeric(condition[1])
  } else {
    colNumber <- which(colNames == condition[1])
    if (length(colNumber) == 0) {
      if (condition[1] != "rownames") {
        stop("There is no column with the name ", condition[1], ". \n",
             "Check the capitalisation, the names must be an exact match.")
      }
    }
  }

  # Retrieve data to compare against the input condition
  if (condition[1] == "rownames") {
    colData <- as.numeric(rownames(x))
  } else {
    colData   <- x[, colNumber]
  }

  # Sift the data
  if (condition[2] == ">") {
    y <- which(colData > as.numeric(condition[3]))
  } else if (condition[2] == "<") {
    y <- which(colData < as.numeric(condition[3]))
  } else if (condition[2] == "in" | condition[2] == "!in") {
    # Account for a "complex" vector (i.e. is it just "a:b", or "a, b, e, f, t")
    if (is.na(inConditions[1])) {
      condition[3] <- paste0("c(", condition[3], ")")
    } else {
      condition[3] <- paste0("c(", paste(inConditions, collapse = ",") ,")")
    }
    # The filtering condition
    if (condition[2] == "in") {
      y <- which(colData %in% eval(parse(text = condition[3])))
    } else if (condition[2] == "!in" | condition[2] == "notIn") {
      y <- which(colData %notIn% eval(parse(text = condition[3])))
    }
  } else if (condition[2] == "=" | condition[2] == "==") {
    y <- which(colData == condition[3])
  } else if (condition[2] == "!=") {
    y <- which(colData != condition[3])
  } else {
    stop("Please choose a valid operator!")
  }

  # Apply the filter
  y <- x[y, ]

  # Warning if no data matches the condition
  if (dim(y)[1] == 0) {
    if (isFALSE(noWarning)) {
      warning("No rows match this condition!")
    }
  }
  return(y)
}
