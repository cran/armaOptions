#' Time Sensitivity Analysis for European Call Option
#'
#' This function calculates the value of the a European put option for a list of future time values, given stock price data and a given buy value.
#'
#' @param stock_data Numeric vector of stock prices data.
#' @param future_times Numeric vector of the future times
#' @param sell_value Numeric value representing the sell value
#' @param max.p The maximum order of the autoregressive part of the ARMA model (default is 5).
#' @param max.q The maximum order of the moving average part of the ARMA model (default is 5).
#' @param method The way that the ARMA model is calculated, accepted values are "ML", "CSS-ML" and "CSS"
#' @return Estimated values of a European put option at different future times
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
#' sell_value = 110
#' future_times = c(1, 3, 5)
#'
#' PutOptionsOverTime(stock_data = stock_data, future_times = future_times, sell_value = sell_value)
#'
#' @importFrom forecast auto.arima forecast
#' @importFrom stats coef lm residuals
#' @export
PutOptionsOverTime = function(stock_data, future_times, sell_value, max.p = 5, max.q = 5, method = 'CSS-ML') {
  # Validate inputs
  if (!is.numeric(stock_data)) {
    stop("stock_data must be a numeric vector with at least two elements.")
  }

  if (length(stock_data) < 2) {
    stop("stock_data needs to have at least 2 values")
  }

  if (!is.numeric(future_times)) {
    stop("future_times must be a numeric vector with at least one element.")
  }

  if (!is.numeric(sell_value)) {
    stop("sell_value must be a single numeric value.")
  }

  if (!(method %in% c("ML", "CSS-ML", "CSS"))) {
    stop("method must be one of 'ML', 'CSS-ML', or 'CSS'.")
  }

  n = length(future_times)

  results = data.frame(future_times = future_times, stock_option_value = rep(0,n), probability_of_profit = rep(0,n))

  for (i in 1:n) {
    results$stock_option_value[i] = europeanPutOptionValue(stock_data, future_times[i], sell_value, max.p, max.q, method)$stock_option_value
    results$probability_of_profit[i] = europeanPutOptionValue(stock_data, future_times[i], sell_value, max.p, max.q, method)$probability_of_profit
  }

  # Return the results
  return(results)
}


