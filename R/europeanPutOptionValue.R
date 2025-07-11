#' Estimate European Put Option Value
#'
#' This function calculates the value of a European put option based on stock data, a future time value, and a sell value
#'
#' @param stock_data Numeric vector of stock prices data
#' @param future_time Numeric constant of the future time
#' @param sell_value The numeric sell value of the European put option.
#' @param max.p The maximum order of the autoregressive part of the ARMA model (default is set to 5)
#' @param max.q The maximum order of the moving average part of the ARMA model (default is set to 5)
#' @param method The way that the ARMA model is calculated, accepted values are "ML", "CSS-ML" and "CSS"
#' @return Estimate the value of a European put option, determine the probability of making profits, and model an appropriate ARMA model for the given stock data.
#'
#' @examples
#' library(stats)
#' library(forecast)
#' # Create simulated data
#' n = 100
#' set.seed(42)
#' arma_values = arima.sim(n = n, model = list(ar = c(0.6), ma = c(0.5, -0.5)))
#' linear_model = 5 + 1:n
#' stock_data = arma_values + linear_model
#' europeanPutOptionValue(stock_data = stock_data,future_time = 5,sell_value = 110,max.p = 5,max.q = 5)
#'
#' @import forecast
#' @import stats
#' @importFrom forecast auto.arima forecast
#' @importFrom stats coef lm residuals
#'
#' @export
europeanPutOptionValue = function(stock_data, future_time, sell_value, max.p = 5, max.q = 5, method = 'CSS-ML') {
  # Parameters:
  # stock_data Numeric vector of stock prices data
  # future_time Numeric constant of the future time
  # sell_value The numeric sell value of the European put option
  # max.p The maximum order of the autoregressive part of the ARMA model
  # max.q The maximum order of the moving average part of the ARMA model
  # method The way that the ARMA model is calculated

  # Check input
  if (!is.numeric(stock_data)) {
    stop("stock_data needs to be numeric vector")
  }

  if (length(stock_data) < 2) {
    stop("stock_data needs to have at least 2 values")
  }

  if (!is.numeric(future_time)) {
    stop("future_time needs to be numeric")
  }

  if (!is.numeric(sell_value)) {
    stop("sell_value needs to be numeric")
  }

  if (!(method %in% c("ML", "CSS-ML", "CSS"))) {
    stop("method must be one of 'ML', 'CSS-ML', or 'CSS'.")
  }

  # Define negative_conditional_expectation function
  negative_conditional_expectation = function(mu, sigma, S) {

    z = (S - mu) / sigma

    cdf = pnorm(z)
    pdf = dnorm(z)

    # Calculate E[X|X<S]
    E_X = mu - sigma * (pdf / cdf)

    return(E_X)
  }


  # Get linear model
  n = length(stock_data)
  time_index = 1:n

  # Fit Linear regression
  lm_model = lm(stock_data ~ time_index)

  # Get residuals for ARMA modeling
  residuals = residuals(lm_model)

  # Fit ARMA model to the residuals
  arma_model = auto.arima(residuals, max.p = max.p, max.q = max.q, stationary = TRUE, allowmean = FALSE, ic = "aic", method = method)
  # forecast future ARMA predictions
  future_prediction = forecast(arma_model, h = future_time)

  # Calculate expected future stock price
  expected_future_value = as.numeric(coef(lm_model)[1] + coef(lm_model)[2] * (n + future_time))

  # Calculate the target residual value for the specified sell_value
  profit_threshold = sell_value - expected_future_value

  # Determine the standard deviation of the future residual at future_time
  forecast_sd = sqrt((future_prediction$upper[future_time] - future_prediction$lower[future_time]) / (2 * 1.96))

  # Estimate the future residual
  future_expected_residual = future_prediction$mean[future_time]

  # Calculate the expected value given the option is profitable
  conditional_value = negative_conditional_expectation(future_expected_residual, forecast_sd, profit_threshold)
  if (is.na(conditional_value) || conditional_value == Inf) {
    conditional_value = profit_threshold
  }

  # Probability that the future residual exceeds the target value
  probability_to_exceed = pnorm(profit_threshold, mean = future_expected_residual, sd = forecast_sd, lower.tail = TRUE)

  # Return the expected profit of the put option
  expected_profit_when_profitable = profit_threshold-conditional_value

  # times the expected profit and the probability of profit
  stock_option_value = expected_profit_when_profitable * probability_to_exceed

  # Return the results
  return(list(
    stock_option_value = stock_option_value,
    probability_of_profit = probability_to_exceed,
    expected_profit_when_profitable = expected_profit_when_profitable,
    arma_coefficients = coef(arma_model),
    Regression_model_coefficients = lm_model$coefficients
  ))
}



