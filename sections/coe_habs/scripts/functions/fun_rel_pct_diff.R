.x <- c(221, 55)
.y <- c(55, 221)
rel_pct_diff <- function(.x, .y) {
  # Check that the inputs are numeric.
  if (!is.numeric(.x) | !is.numeric(.y)) stop("Inputs must be numeric.")
  # Check that the inputs do not contain any NAs.
  if (any(is.na(.x)) | any(is.na(.y))) stop("Inputs must be not be NA.")
  # Find the difference between x and y.
  diff <- .x - .y
  # Find the sum of x and y.
  average <- mean(c(.x, .y))
  
  if (average == 0) {
    # If the average of x and y is zero, the equation below will return Inf.
    # To make it easier to handle these situations, zero is returned.
    return(0)
  } else {
    # This equation is from Ohio EPA (2018) document.
    # RPD = |(x1 - x2) / ((x1+ + x2) / 2)| * 100
    return(abs(diff / average * 100))
  }
}
