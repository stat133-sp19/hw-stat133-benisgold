# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

# private function to check if input prob is a valid probability value
check_prob = function(prob) {
  if (is.numeric(prob) & prob >= 0 & prob <= 1) {
    return(TRUE)
  }
  stop('prob must be a number between 0 and 1')
}

# private function to check if input trials is a valid value for number of trials
check_trials = function(trials) {
  if (is.integer(trials) & trials >= 0) {
    return(TRUE)
  }
  stop('invalid trials value')
}
