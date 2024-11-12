#' Secret Santa
#'
#' Create Secret Santa allocations
#'
#' @param names Vector of names.
#' @param seed Random seed. Default 2512.
#' @return Data frame of names
#' @export

secret_santa <- function(names, seed = 2512) {
  if (anyDuplicated(names)) {
    stop("Non-unique names provided - do you need to add a surname?")
  }
  withr::with_seed(
    seed = seed,
    code = {
      random_names <- sample(names)
      output <- data.frame(
        gift_giver = random_names,
        gift_recipient = random_names[c(2:length(random_names), 1)]
      )
      output <- output[sample(seq_len(nrow(output))), ]
      rownames(output) <- seq_len(nrow(output))
      return(output)
    }
  )
}
