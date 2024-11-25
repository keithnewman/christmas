#' Secret Santa
#'
#' Create Secret Santa gift giving allocations.
#'
#' @param names Vector of names for the participants. You should have 4 or more player names to
#'   create a Secret Santa allocation that can remain secret.
#' @param seed Random seed. Default 2512.
#' @return Data frame of names providing the gift giver and their gift recipient's name.
#' @examples
#' secret_santa(c("Dasher", "Dancer", "Prancer", "Vixen"))
#' @export

secret_santa <- function(names, seed = 2512) {
  if (anyDuplicated(names)) {
    stop("Non-unique names provided - do you need to add a surname?")
  }

  if (length(names) < 3L) {
    stop("You need at least 3 players to play and at least 4 players for it to be a secret.")
  } else if (length(names) == 3L) {
    warning("It's not a secret with 3 players - participants will be able to determine who their gift is from.")
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
