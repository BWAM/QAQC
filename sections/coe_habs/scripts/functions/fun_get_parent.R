get_parent <- function(.vec) {
  parent_vec <- rep(NA_character_, length(.vec))
  for (i in .vec) {
    match_vec <- ifelse(grepl(i, .vec) & i != .vec,
                        i,
                        NA_character_)
    parent_vec <- fixit::cross_fill(.target = parent_vec,
                                    .replace = match_vec)
  }
  return(parent_vec)
}