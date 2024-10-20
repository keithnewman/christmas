#' Secret Santa
#'
#' Create Secret Santa allocations
#'
#' @param names Vector of names.
#' @param seed Random seed.
#' @return Data frame of names

secret_santa <- function(names, seed = 2024) {
  if (length(unique(names)) != length(names)) {
    stop("Non-unique names provided - do you need to add a surname?")
  }
  set.seed(seed)
  random_names <- sample(names)
  output <- data.frame(
    gift_giver = random_names,
    gift_recipient = random_names[c(2:length(random_names), 1)]
  )
  return(output)
}
