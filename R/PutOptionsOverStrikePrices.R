#' Strike Price Sensitivity Analysis for European Put Option
#'
#' This function calculates the value of the a European put option for a list of strike price / sell values, given stock price data and a given future time.
#'
#' @param stock_data Numeric vector of stock prices data.
#' @param future_time Numeric constant of the future time
#' @param sell_values Numeric vector of the sell values to calculate the put option values at
#' @param max.p The maximum order of the autoregressive part of the ARMA model (default is 5)
#' @param max.q The maximum order of the moving average part of the ARMA model (default is 5)
#' @param method The way that the ARMA model is calculated, accepted values are "ML", "CSS-ML" and "CSS"
#' @return Estimated values of a European put option at different sell values
#'
#' @examples
#' library(stats)
#' library(forecast)
#'
#' n = 100
#' set.seed(42)
#' arma_values = arima.sim(n = n, model = list(ar = c(0.6), ma = c(0.5, -0.5)))
#' linear_model = 5 + 1:n
#' stock_data = arma_values + linear_model
#'
#'
#' future_time = 2
#' sell_values = seq(90, 110, length.out = 5)
#'
#' PutOptionsOverStrikePrices(stock_data, future_time, sell_values)
#'
#'
#' @importFrom forecast auto.arima forecast
#' @importFrom stats coef lm residuals
#' @export
PutOptionsOverStrikePrices = function(stock_data, future_time, sell_values, max.p = 5, max.q = 5, method = 'CSS-ML') {
  # Validate inputs
  if (!is.numeric(stock_data)) {
    stop("stock_data must be a numeric vector")
  }

  if (length(stock_data) < 2) {
    stop("stock_data needs to have at least 2 values")
  }

  if (!is.numeric(future_time)) {
    stop("future_time must be a numeric value")
  }

  if (!is.numeric(sell_values)) {
    stop("sell_values must be a numeric vector")
  }

  if (!(method %in% c("ML", "CSS-ML", "CSS"))) {
    stop("method must be one of 'ML', 'CSS-ML', or 'CSS'.")
  }

  n = length(sell_values)

  results = data.frame(strike_price = sell_values, stock_option_value = rep(0,n), probability_of_profit = rep(0,n))

  for (i in 1:n) {
    results$stock_option_value[i] = europeanPutOptionValue(stock_data, future_time, sell_values[i], max.p, max.q, method)$stock_option_value
    results$probability_of_profit[i] = europeanPutOptionValue(stock_data, future_time, sell_values[i], max.p, max.q, method)$probability_of_profit
  }

  return(results)
}


