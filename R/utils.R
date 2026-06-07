# Internal helper (not exported)
# Checks that `df` is a data frame and contains all columns in `required`.
# Throws an informative error if either check fails.

validate_cols <- function(df, required, arg_name = "data") {
  if (!is.data.frame(df)) {
    stop("`", arg_name, "` must be a data frame, not ",
         class(df)[1], ".", call. = FALSE)
  }
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("`", arg_name, "` is missing required column(s): ",
         paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  invisible(TRUE)
}
