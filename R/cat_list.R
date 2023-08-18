cat_list <- function(list, name = NULL,
                     listDepth = 0,
                     nameLength = 1,
                     maxDepth = 1) {
  #' Print out a list in a slightly prettier way (in my opinion!)
  #'
  #' @description Printing a list doesn't look great, and `cat()` doesn't work.
  #'   This function is an alternative way to show the data in a list. It is
  #'   still in progress, and currently only handles vectors, matrices and
  #'   lists/data.frames.
  #'
  #' @param list The list to be printed out.
  #' @param name Name to be printed at the top of each list. If NULL (the
  #'   default), it matches the name of the 'list' argument. This is mainly here
  #'   for recursion so the "iiData" in the loop isn't used as the name for
  #'   every subsequent nested list.
  #' @param listDepth Used for recursive calls to track the depth; each
  #'   subsequent depth is marked by an extra "O" at the start of the line.
  #' @param nameLength Used for recursion to help things align.
  #' @param maxDepth Used for recursion to help things align.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # For the items in the list
  listLength <- length(list)
  listNames  <- names(list)
  allNames   <- get_list_names(list, name = deparse(substitute(list)))

  # For the recursion
  listDepth  <- listDepth + 1
  maxDepth   <- max(count_list_depth(list), maxDepth)   # so no reset each iter.
  nameLength <- max(nameLength, max(nchar(allNames), 1))# 1 = no error if empty

  # List Depth
  depthCircles <- strrep("O", listDepth)                # O for current depth
  if (listDepth <= maxDepth) {
    depthLines <- strrep("-", maxDepth - listDepth)     # - for further depths
    if (maxDepth < 6) {
      depthSpaces <- strrep(" ", 5 - maxDepth)          # >= 5 wide = alignment
    } else {
      depthSpaces <- ""
    }
  } else {
    depthLines  <- ""
    depthSpaces <- ""
  }
  depthBit <- paste0(depthCircles, depthLines, depthSpaces, " ") # e.g. O--
  intoBit  <- gsub("O", "º", paste0(depthBit))                   # item point

  # List title
  ## This lets us overrule the deparse in recursions so it isn't just "iiData"
  if (!is.null(name)) {
    listTitle <- name
  } else {
    listTitle <- deparse(substitute(list))
  }
  if (listTitle != "") listTitle <- paste0("'", listTitle, "'")

  # This is printed to tell us if the list is a list or a data frame
  if ("data.frame" %in% is(list)) {
    typeBit <- " D.F.:\n"
    lineBit <- "-"
  } else {
    typeBit <- " List:\n"
    lineBit <- "="
  }

  # Display the list title
  cat("\n", depthBit, listTitle, typeBit, sep = "")           # list info
  cat(strrep(lineBit, (nameLength + 9 + max(maxDepth, 5))),   # underline
      "\n", sep = "")

  # Loop through items in the list and print them
  if (listLength > 0) {
    for (ii in 1:listLength) {
      iiData <- list[[ii]]
      iiType <- is(iiData)

      iiName <- listNames[[ii]]
      if (!is.list(iiData)) {
        iiName <- paste0(iiName, strrep(" ", nameLength - nchar(iiName)))
      }

      # Print out the items!
      if ("matrix" %in% iiType) {
        cat(intoBit)
        cat(iiName, " ||  Matrix: \n")
        cat(strrep(".", (nameLength + 9 + max(maxDepth, 5) + 5)),   # underline
            "\n", sep = "")
        print(iiData)
      } else if ("list" %in% iiType) {
        cat_list(list = iiData, name = iiName, listDepth = listDepth,
                 maxDepth = maxDepth, nameLength = nameLength) # recursive call
        cat("\n")
      } else if ("vector" %in% iiType) {
        cat(intoBit)
        cat(iiName, " || ", iiData, "\n")
      } else {
        cat(intoBit)
        cat(iiName, " || \n")
        print(iiData)
        cat("\n")
      }
    }
  } else {
    cat(intoBit)
    cat("Empty List \n")
  }
}

# TESTING
#
# tt <- list("abcdd" = c(1:5),
#            "happy" = c("yellow", "blue", "orange"),
#            "emotions" = matrix(1:12, ncol = 3),
#            "good man" = "Ronak Shah",
#            c(1:5),
#            "tstingList" = list("inList1" = c(1:3),
#                                "inList2" = c("purple", "red")),
#            "Little Lion" = c("Dr", "Genius", "Runderrul"),
#            "anotherList" = list("yayyy" = c("yes", "yeah", "yep"),
#                                 "no" = list("nope" = c("nah", "nu-uh"),
#                                             "refusal" = c(1:4))),
#            "huuuu" = c(6:11),
#            "lemon" = c("fruit", "juice"),
#            "finalLists" = list("t" = list("a" = 1, "b" = matrix(5:10, nrow = 3)),
#                                "u" = 99:101,
#                                "v" = letters,
#                                "w" = list("onomatyiopeaiai" = list("noise" = c("plop", "drip", "drop"),
#                                                                  "sounds" = c("clip", "clop"),
#                                                                  "bells" = c("ding", "dong")),
#                                           "normal noises" = LETTERS),
#                                "x" = c(1:3)))
#
# cat_list(tt)

# tst <- list()
# tst[["testing"]] <- list("aaaa" = 1:3,
#                                  list("cc" = 4:5,
#                                       "dd" = 6:7))
# tst[["whoop"]] <- list("ee" = 1:3,
#                                       1:12)
# tst[["jj"]] <- terra::rast()
# tst[["kk"]] <- terra::ext(c(1, 2, 3, 4))
# tst[["pp"]] <- matrix(1:24, nrow = 6)
# tst[["mm"]] <- matrix(1:24, nrow = 6) |>
#   as.data.frame() |>
#   `colnames<-`(paste0("col", letters[1:4]))
# tst[["nn"]] <- list()
#


# SCRAPS #
#   listDepth  <- count_list_depth(list)
#   listLength <- length(list)
#   listNames  <- names(list)
#   allNames   <- get_list_names(list)
#   nameLength <- max(nameLength, max(nchar(allNames)))
#   depth <- depth + 1
#
#   # Print out list title
#   ## This lets us overrule the deparse in recursions so it isn't just "iiData"
#   if (!is.null(name)) {
#     listTitle <- name
#   } else {
#     listTitle <- deparse(substitute(list))
#   }
#   # This is printed to tell us if the list is a list or a data frame
#   if ("data.frame" %in% is(list)) {
#     typeBit <- "' D.F.:\n"
#   } else {
#     typeBit <- "' List:\n"
#   }
#   # Combine all the title part into this mess!
#   cat("\n",
#       strrep("º", depth), " '",
#       listTitle, typeBit,
#       strrep("=", max(nchar(listTitle), nameLength) + 9 + depth),
#       "\n", sep = "")
#
#   # Loop through items in the list and display info
#   for (ii in 1:listLength) {
#     # What is the name?
#     iiName <- listNames[[ii]]
#     iiName <- paste0(iiName, strrep(" ", nameLength - nchar(iiName)))
#
#     # What is the data?
#     iiData <- list[[ii]]
#     iiType <- is(iiData)
#
#     # Depth
#     depthCircles <- strrep("o", depth)
#     if ( depth <= maxDepth) {
#       depthLines <- strrep("-", listDepth - depth)
#       if (maxDepth < 6) {
#         depthSpace <- strrep("=", 5 - listDepth)
#       }
#     } else {
#       depthLines <- ""
#       depthSpace <- ""
#     }
#     depthBit <- paste0(depthCircles, depthLines, depthSpace, "\n")
#
#     # At the left of the screen to show what is there; not shown for lists
#     # intoBit <- paste(strrep("º", depth),
#                    # strrep("-", max(5, listDepth) - depth),
#                    # " ", sep = "")
#
#     # Printing out the data
#     if ("matrix" %in% iiType) {
#       cat(depthBit)
#       cat(iiName, " ||  Matrix: \n")
#       print(iiData)
#     } else if ("list" %in% iiType) {
#       Recall(iiData, iiName, depth)   # recursive calls
#       cat("\n")
#     } else {
#       cat(depthBit)
#       cat(iiName, " || ", iiData, "\n")
#     }
#   }
# }
