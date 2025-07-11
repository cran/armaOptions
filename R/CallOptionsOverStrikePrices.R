#' Call Option Values for Differently Priced European Call Options
#'
#' This function calculates the value of the a European call option for a list of strike price / buy values, given stock price data and a given future time.
#'
#' @param stock_data Numeric vector of stock prices data.
#' @param future_time Numeric constant of the future time
#' @param buy_values Numeric vector of the buy values at which to calculate the call option values
#' @param max.p The maximum order of the Auto Regressive part of the ARMA model (default is set to 5)
#' @param max.q The maximum order of the Moving Average part of the ARMA model (default is set to 5)
#' @param method The way that the ARMA model is calculated, accepted values are "ML", "CSS-ML" and "CSS"
#' @return Estimated values of a European call option at different buy values
#'
#' @examples
#' library(stats)
#' library(forecast)
#'
#' # Create simulated data
#' n = 100
#' set.seed(42)
#' arma_values = arima.sim(n = n, model = list(ar = c(0.6), ma = c(0.5, -0.5)))
#' linear_model = 5 + 1:n
#' stock_data = arma_values + linear_model
#'
#' future_time = 3
#' buy_values = seq(90, 110, length.out = 5)
#'
#' CallOptionsOverStrikePrices(stock_data, future_time, buy_values)
#'
#' @import forecast
#' @import stats
#' @importFrom forecast auto.arima forecast
#' @importFrom stats coef lm residuals
#' @export
#'
CallOptionsOverStrikePrices = function(stock_data, future_time, buy_values, max.p = 5, max.q = 5, method = 'CSS-ML') {
  # Check input
  if (!is.numeric(stock_data)) {
    stop("stock_data needs to be numeric")
  }

  if (length(stock_data) < 2) {
    stop("stock_data needs to have at least 2 values")
  }

  if (!is.numeric(future_time)) {
    stop("future_time needs to be numeric")
  }

  if (!is.numeric(buy_values)) {
    stop("buy_values needs to be numeric")
  }

  if (!(method %in% c("ML", "CSS-ML", "CSS"))) {
    stop("method must be one of 'ML', 'CSS-ML', or 'CSS'.")
  }

  n = length(buy_values)

  # Track Results
  results = data.frame(buy_values = buy_values, stock_option_values = rep(0,n), probability_of_profit = rep(0,n))

  for (i in 1:n) {
    results$stock_option_values[i] = europeanCallOptionValue(stock_data, future_time, buy_values[i], max.p, max.q, method)$stock_option_value
    results$probability_of_profit[i] = europeanCallOptionValue(stock_data, future_time, buy_values[i], max.p, max.q, method)$probability_of_profit
  }

  return(results)
}
