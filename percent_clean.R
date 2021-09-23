#helper for INF for removing Inf from Budget execution
percent_clean <- function(x, y) {
  ifelse(y > 0.000, (x / y), NA_real_)
}