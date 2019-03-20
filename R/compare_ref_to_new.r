check_ref_to_new <- function(xref, xnew) {
  fail <- FALSE
  error_msg <- c()
  # reference files missing in new export
  trouble <- xref[!xref %in% xnew]
  if (length(trouble) > 0) {
    error_msg <- c(error_msg, crayon::red(glue::glue("\u2716 Missing in new: {trouble}\n\n")))
    fail <- TRUE
  }

  # new export files
  trouble <- xnew[!xnew %in% xref]
  if (length(trouble) > 0) {
    error_msg <- c(error_msg, crayon::red(glue::glue("\u2716 Something new: {trouble}\n\n")))
    fail <- TRUE
  }

  if (fail) {
    stop("\n", error_msg)
  }
}
