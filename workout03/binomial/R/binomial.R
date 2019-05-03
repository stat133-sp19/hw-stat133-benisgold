# private function to check if input "prob" is a valid probability value
check_prob = function(prob) {
  ifelse(is.numeric(prob) & prob >= 0 & prob <= 1, "", stop('prob must be a number between 0 and 1'))
  return(TRUE)
}

# private function to check if input "trials" is a valid value for number of trials
check_trials = function(trials) {
  ifelse(as.integer(trials) == trials & trials >= 0, "", stop('invalid trials value'))
  return(TRUE)
}

# private function to check if input "success" is a valid value for number of successes
check_success = function(success, trials) {
  ifelse(as.integer(success) == success & success >= 0,
         ifelse(success <= trials, "", stop('success cannot be greater than trials')),
         stop('invalid success value'))
  return(TRUE)
}

# private function that calculates the mean of a binomial distribution
aux_mean = function(trials, prob) {
  return(trials * prob)
}

# private function that calculates the variance of a binomial distribution
aux_variance = function(trials, prob) {
  return(trials * prob * (1 - prob))
}

# private function that calculates the mode of a binomial distribution
aux_mode = function(trials, prob) {
  return(as.integer(trials * prob + prob))
}

# private function that calculates the skewness of a binomial distribution
aux_skewness = function(trials, prob) {
  return((1 - 2 * prob) / sqrt(trials * prob * (1 - prob)))
}

# private function that calculates the kurtosis of a binomial distribution
aux_kurtosis = function(trials, prob) {
  return((1 - 6 * prob * (1 - prob)) / (trials * prob * (1 - prob)))
}

#' @title Bin Choose
#' @description Calculates the number of combinations in which k successes can occur in n trials
#' @param k integer vector containing number of successes
#' @param n integer vector containing number of trials
#' @return integer vector containing number of combinations
#' @export
#' @examples
#' bin_choose(n = 5, k = 2)
#' bin_choose(5, 0)
#' bin_choose(5, 1:3)
bin_choose = function(n, k) {
  ifelse(k > n, stop("k cannot be greater than n"), factorial(n) / (factorial(k) * factorial(n - k)))
}

#' @title Bin Probability
#' @description Calculates the probability of getting a certain number of successes in a certain number of trials
#' @param success integer vector containing number of successes
#' @param trials integer vector containing number of trials
#' @param prob probability of success (between 0 and 1)
#' @return numeric vector containing probabilities
#' @export
#' @examples
#' # probability of getting 2 successes in 5 trials
#' # (assuming prob of success = 0.5)
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#'
#' # probabilities of getting 2 or less successes in 5 trials
#' # (assuming prob of success = 0.5)
#' bin_probability(success = 0:2, trials = 5, prob = 0.5)
#'
#' # 55 heads in 100 tosses of a loaded coin with 45% chance of heads
#' bin_probability(success = 55, trials = 100, prob = 0.45)
bin_probability = function(success, trials, prob) {
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)
  return(bin_choose(trials, success) * prob**success * (1 - prob)**(trials - success))
}

#' @title Bin Distribution
#' @description Calculates the probability distribution of number of successes
#' @param trials number of trials
#' @param prob probability of success (between 0 and 1)
#' @return data frame of primary class \code{"bindis"} containing number of success in the first column and probability in the second column
#' @export
#' @examples
#' # binomial probability distribution
#' bin_distribution(trials = 5, prob = 0.5)
bin_distribution = function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  output = data.frame()
  for (success in 0:trials) {
    output = rbind(output, c(success, bin_probability(success, trials, prob)))
  }
  names(output) = c("success", "probability")
  class(output) = c("bindis", "data.frame")
  return(output)
}

#' @export
plot.bindis = function(dist) {
  barplot(dist$probability, names.arg = dist$success, ylab = "probability", xlab = "successes")
}

#' @title Bin Cumulative
#' @description Calculates the probability distribution and cumulative probabilities of number of successes
#' @param trials number of trials
#' @param prob probability of success (between 0 and 1)
#' @return data frame of primary class \code{"bincum"} containing number of success in the first column, probability in the second column, and cumulative probability in the third column
#' @export
#' @examples
#' # binomial cumulative distribution
#' bin_cumulative(trials = 5, prob = 0.5)
bin_cumulative = function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  output = data.frame()
  sum = 0
  for (success in 0:trials) {
    p = bin_probability(success, trials, prob)
    sum = sum + p
    output = rbind(output, c(success, p, sum))
  }
  names(output) = c("success", "probability", "cumulative")
  class(output) = c("bincum", "data.frame")
  return(output)
}

#' @export
plot.bincum = function(dist) {
  plot(dist$success, dist$cumulative, ylab = "probability", xlab = "successes", type = "b")
}

#' @title Bin Variable
#' @description Creates \code{"binvar"} object given number of trials and probability of success
#' @param trials number of trials
#' @param prob probability of success (between 0 and 1)
#' @return object of class \code{"binvar"}
#' @export
#' @examples
#' # creates binomial random variable object with 10 trials and 0.5 probability of success
#' bin_variable(10, 0.5)
bin_variable = function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  output = list("trials"=trials, "prob"=prob)
  class(output) = "binvar"
  return(output)
}

#' @export
print.binvar = function(obj) {
  cat('"Binomial variable"\n\nParameters\n- number of trials: ', obj[["trials"]], "\n- prob of success : ", obj[["prob"]])
}

#' @export
summary.binvar = function(obj) {
  trials = obj[["trials"]]
  prob = obj[["prob"]]
  output = list("trials"=trials,
                "prob"=prob,
                "mean"=aux_mean(trials, prob),
                "variance"=aux_variance(trials, prob),
                "mode"=aux_mode(trials, prob),
                "skewness"=aux_skewness(trials, prob),
                "kurtosis"=aux_kurtosis(trials, prob))
  class(output) = "summary.binvar"
  return(output)
}

#' @export
print.summary.binvar = function(obj) {
  cat('"Summary Binomial"\n\nParameters\n- number of trials: ', obj[["trials"]], "\n- prob of success : ", obj[["prob"]], "\n\nMeasures\n- mean    : ", obj[["mean"]], "\n- variance: ", obj[["variance"]], "\n- mode    : ", obj[["mode"]], "\n- skewness: ", obj[["skewness"]], "\n- kurtosis: ", obj[["kurtosis"]])
}

#' @title Bin Mean
#' @description Calculates the mean of a binomial distribution
#' @param trials number of trials
#' @param prob probability of success (between 0 and 1)
#' @return Mean of the distribution
#' @export
#' @examples
#' bin_mean(10, 0.3)
bin_mean = function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials, prob))
}

#' @title Bin Variance
#' @description Calculates the variance of a binomial distribution
#' @param trials number of trials
#' @param prob probability of success (between 0 and 1)
#' @return Variance of the distribution
#' @export
#' @examples
#' bin_variance(10, 0.3)
bin_variance = function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials, prob))
}

#' @title Bin Mode
#' @description Calculates the mode of a binomial distribution
#' @param trials number of trials
#' @param prob probability of success (between 0 and 1)
#' @return Mode of the distribution
#' @export
#' @examples
#' bin_mode(10, 0.3)
bin_mode = function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials, prob))
}

#' @title Bin Skewness
#' @description Calculates the skewness of a binomial distribution
#' @param trials number of trials
#' @param prob probability of success (between 0 and 1)
#' @return Skewness of the distribution
#' @export
#' @examples
#' bin_skewness(10, 0.3)
bin_skewness = function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials, prob))
}

#' @title Bin Kurtosis
#' @description Calculates the kurtosis of a binomial distribution
#' @param trials number of trials
#' @param prob probability of success (between 0 and 1)
#' @return Kurtosis of the distribution
#' @export
#' @examples
#' bin_kurtosis(10, 0.3)
bin_kurtosis = function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials, prob))
}
