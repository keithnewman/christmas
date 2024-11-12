test_that("secret_santa() won't assign a person to themself", {
  naughty_list <- secret_santa(letters)
  expect_false(any(naughty_list$gift_giver == naughty_list$gift_recipient))
})

test_that("secret_santa() warns if the same person appears twice in the list", {
  nice_list <- c(
    "Rudolph",
    "Dasher",
    "Dancer",
    "Prancer",
    "Vixen",
    "Comet",
    "Cupid",
    "Donner",
    "Blitzen",
    "Rudolph" # You again?!
  )
  expect_error(secret_santa(nice_list), "Non-unique names provided")
})
