create_lean_bib <- function(inputFile, inputBib, outputBib) {
  #' Create a leaner .bib file for a .rmd file
  #'
  #' @description Instead of sharing a full Zotero library, this function
  #'   creates a leaner .bib file that only stores the citations that were used.
  #'   It has not been extensively tested, but it worked okay for use with the
  #'   phd01 manuscript. The code was modified from this StackOverflow answer:
  #'   https://stackoverflow.com/questions/65306015/in-rmarkdown-is-there-a-way-to-create-a-bib-file-for-only-those-keys-cited-in
  #'    It assumes that all references begin with `@` (e.g. `@Saun2022`). It may
  #'   therefore grab any email addresses in their, so it is not foolproof.
  #'
  #' @param inputFile "string": path to the .rmd file to check for citations in
  #' @param inputBib "string": path to the .bib file used for the inputFile
  #' @param outputBib "string": path location for the new, leaner .bib file
  #'
  #' @examples
  #' \dontrun{
  #'     create_lean_bib("path/to/manuscript/phd01_manuscript.Rmd",
  #'                     "path/to/existing/bibliography/zotero.bib",
  #'                     "path/to/new/lean/bib/tc-2022-94_bibliography.bib")
  #' }
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Get all text from the file
  lines   <- paste(readLines(inputFile), collapse = "")

  # Find references (they start with @)
  entryStart <- unlist(gregexpr(pattern = "@", lines))
  entryEnd   <- entryStart + 9
  entries    <- substring(lines, entryStart, entryEnd)

  # Remove known ones that begin with @, which aren't citations
  entries <- entries[-which(regexpr("@ref", entries) > -1)]
  entries <- entries[-which(regexpr("@monash.ed", entries) > -1)]

  # Remove punctuation
  entries <- sub("\\.",    replacement = "", entries)
  entries <- sub(";",      replacement = "", entries)
  entries <- sub(" ",      replacement = "", entries)
  entries <- sub("]",      replacement = "", entries)
  entries <- sub("\\(\\)", replacement = "", entries)
  entries <- sub("\\\\",   replacement = "", entries)
  entries <- sub(",",      replacement = "", entries)
  entries <- sub("@",      replacement = "", entries)

  # Get all the inputBib info
  bib <- paste(readLines(inputBib), collapse = "\n")
  bib <- unlist(strsplit(bib, "\n@"))

  # Which bib values are in our entries? # Not efficient or elegant... yet!
  matches    <- sapply(entries, text = bib, regexpr)
  bibIndices <- which(matches > 0, arr.ind = TRUE)[, 1]
  bibIndices <- unique(bibIndices)

  # Create new bib file with only the entries we used
  output <- bib[bibIndices]
  output <- paste0("@", output) # entries need to start with @ to be found
  writeLines(output, outputBib)

  # Alert user upon completion
  cat("\nSuccessfully created a leaner bibliography for:\n",
      basename(inputFile),
      "\n\nSaved to: \n", outputBib, "\n")
}

# cat("\014")

# f_testPath <- "../Desktop/today/clean_bib_test/"
# textFile   <- paste0(f_testPath, "phd01_manuscript_Saunderson.tex") # nope!
# rmdFile    <- paste0(f_testPath, "phd01_manuscript_Saunderson.rmd") # yep!
# bibFile    <- paste0(f_testPath, "Zotero4.bib")
# newBib     <- paste0(f_testPath, "tc-2022-94_bibliography.bib")

# test run
# create_lean_bib(rmdFile, bibFile, newBib)
