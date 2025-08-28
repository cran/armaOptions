# ARMA Models to European Value Stock Options

## Overview

This package provides ways to estimate the value of European stock options given stock price data. It includes functions for calculating option values based on Auto-Regressive–Moving-Average (ARMA) models and also returns information about these models. This package is made to be easy to understand and is built for financial analysis capabilities, however it can be used in many other situations. This package is dependent on the 'forecast' and 'stats' packages.

## Installation

The current version of the `armaOptions` package can be installed with:

```r
install.packages("armaOptions")
```

## Usage
Calculating put option values for simulated data.

```r
library(armaOptions)
library(forecast)
library(stats)

# Create simulated data
n = 100
set.seed(42)
arma_values = arima.sim(n = n, model = list(ar = c(0.6), ma = c(0.5, -0.5)))
linear_model = 5 + 1:n
stock_data = arma_values + linear_model

# Define a sell value and future times
sell_value = 110
future_times = c(1, 3, 5)

# Calculate put option values over a list of future times
results = PutOptionsOverTime(stock_data = stock_data, future_times = future_times, sell_value = sell_value)

# Print results
print(results)
```


```r
# Define a list of sell values
sell_values = seq(90, 110, length.out = 5)
future_time = 2

# Calculate put option values over a list of sell values
results = PutOptionsOverStrikePrices(stock_data = stock_data, future_time = future_time, sell_values = sell_values)

# Print results
print(results)
```


# armaOptions Package Theoretical Explanation

## Setup

These functions are based on the assumption that price data follow the following equation 

$$P_t = \beta_0 + \beta_1t + X_t$$ 

where $t$ is time, $\beta_0$ and $\beta_1$ are linear regression parameters, and $X_t$ are variables from an ARMA(p,q) model. 

An ARMA(p,q) model is defined as 

$$X_t = \epsilon_t + \sum_{i=1}^{p}\phi_iX_{t-i} + \sum_{q}^{j=i}\theta_j\epsilon_{t-j}$$

where $\epsilon_t\sim\mathcal{N}(0,1)~\forall t\in\mathbb{N}$, and $\phi$ and $\theta$ are stationary time series parameters.

So lets say our friend Bob wants to sell us a [European put option](https://www.investopedia.com/terms/p/putoption.asp#:~:text=A%20put%20option%20%28or%20%E2%80%9Cput%E2%80%9D%29%2C%20which%20gives%20the,This%20predetermined%20price%20is%20called%20the%20strike%20price.) at a price S, h days into the future. How do we find a fair price? This put option is only valuable if, in h-days, $P_{t+h}<S$ because then we can sell a security at a value greater then its price and the option is worthless if $P_{t+h}>S$ is true.

## Method

The expected value can therefore be written as

$$
\begin{align*}
\text{Expected Value} &= \int_{-\infty}^{\infty}\text{max}(S-P_{t+h},0)f(P_{t+h})dP_{t+h} \\
&= \int_{-\infty}^{S}(S-P_{t+h})f(P_{t+h})dP_{t+h} \\
&= S\int_{-\infty}^{S}f(P_{t+h})dP_{t+h} - \int_{-\infty}^{S}P_{t+h}f(P_{t+h})dP_{t+h} \\
&= S\Pr(P_{t+h} < S) - E(P_{t+h} | P_{t+h} \< S)\Pr(P_{t+h} \< S) \\
&= (S - E(P_{t+h} | P_{t+h} < S))\Pr(P_{t+h} < S)
\end{align*}
$$

where $f(P_{t+h})$ is the distribution function of $P_{t+h}$.

We have defined the stock price at point $t+h$ as $$P_{t+h} = \beta_0 + \beta_1(t+h) + X_{t_h}$$. Since $P_{t+h}<S \implies X_{t+h}<S-\beta_0 - \beta_1(t+h)$, if we assume knowledge of the regression parameters $\beta_0$ and $\beta_1$ then $S-\beta_0 - \beta_1(t+h)$ is a deterministic value and so 

$$(S - E(P_{t+h} | P_{t+h} < S))\Pr(P_{t+h} < S) = (S_r - E(X_{t+h} | X_{t+h} < S_r))\Pr(X_{t+h} < S_r)$$

where $S_r = S-\beta_0 - \beta_1(t+h)$.

For an ARMA(p,q) model, assuming knowledge of the model parameters, at a forecasted value $X_{t+h}$ forecasted h-days into the future this value follows a normal distribution with a distribution function $\mathcal{N}(\hat{X}(t+h),\hat{\sigma}(t+h))$ for more details about how the forecasting or time series parameter fitting I would recommend chapters 3 and 4 of the book "Time Series Analysis" by Lewis Hamilton or the source code and documentation to the R package "forecast". Once this final equation is found, it is relatively easy to solve for the expected value of the call option.

The logic for [Call options](https://www.investopedia.com/terms/c/calloption.asp) is the exact same just, in the other direction. 
