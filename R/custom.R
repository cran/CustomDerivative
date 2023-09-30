#' Custom Derivative R6 Class
#'
#' An R6 class to create and price custom derivatives.
#'
#' @field underlying_price The underlying asset price.
#' @field strike_price The strike price of the option.
#' @field time_to_maturity Time to maturity of the option.
#' @field volatility The volatility of the underlying asset.
#' @field risk_free_rate The risk-free rate.
#' @field payoff_function The function that determines the payoff of the option.
#'
#' @description
#' This class provides methods to create and price custom derivatives.
#'
#' @details
#' The class uses the Monte Carlo method for pricing. The price method simulates
#' the underlying asset price paths and applies the payoff function to determine
#' the option price.
#'
#' @section Methods:
#' \describe{
#'   \item{initialize(underlying_price, strike_price, time_to_maturity, volatility, risk_free_rate, payoff_function):}{
#'     Constructor method. Initializes the parameters for the custom derivative.
#'   }
#'   \item{price():}{
#'     Calculate the option price using the Monte Carlo method.
#'   }
#'   \item{delta():}{
#'     Calculate the Delta of the option.
#'   }
#'   \item{gamma():}{
#'     Calculate the Gamma of the option.
#'   }
#'   \item{theta():}{
#'     Calculate the Theta of the option.
#'   }
#'   \item{vega():}{
#'     Calculate the Vega of the option.
#'   }
#'   \item{rho():}{
#'     Calculate the Rho of the option.
#'   }
#' }

#'
#' @param underlying_price Initial price of the underlying asset.
#' @param strike_price Strike price of the option.
#' @param time_to_maturity Time to maturity in years.
#' @param volatility Volatility of the underlying asset.
#' @param risk_free_rate Risk-free rate (annual).
#' @param payoff_function A function that calculates the option payoff.
#'
#' @return For \code{price}, \code{delta}, \code{gamma}, \code{theta}, \code{vega}, and \code{rho}: a numeric value.
#'
#' @importFrom R6 R6Class
#' @export
#' @examples
#' # Define the payoff function for a European call option
#' call_payoff <- function(price) {
#'   return(max(price - 100, 0))
#' }
#'
#' # Create an instance of the CustomDerivative class
#' option <- CustomDerivative$new(100, 100, 1, 0.2, 0.05, call_payoff)
#'
#' # Print Option Price and Greeks
#' cat("Option Price:", option$price(), "\n")
#' cat("Delta:", option$delta(), "\n")
#' cat("Gamma:", option$gamma(), "\n")
#' cat("Theta:", option$theta(), "\n")
#' cat("Vega:", option$vega(), "\n")
#' cat("Rho:", option$rho(), "\n")

CustomDerivative <- R6::R6Class(
  "CustomDerivative",
  public = list(
    underlying_price = NULL,
    strike_price = NULL,
    time_to_maturity = NULL,
    volatility = NULL,
    risk_free_rate = NULL,
    payoff_function = NULL,
    #' @param underlying_price Initial price of the underlying asset.
    #' @param strike_price Strike price of the option.
    #' @param time_to_maturity Time to maturity in years.
    #' @param volatility Volatility of the underlying asset.
    #' @param risk_free_rate Risk-free rate (annual).
    #' @param payoff_function A function that calculates the option payoff.
    initialize = function(underlying_price, strike_price, time_to_maturity, volatility, risk_free_rate, payoff_function) {
      self$underlying_price <- underlying_price
      self$strike_price <- strike_price
      self$time_to_maturity <- time_to_maturity
      self$volatility <- volatility
      self$risk_free_rate <- risk_free_rate
      self$payoff_function <- payoff_function
    },
    #' Calculate the option price using the Monte Carlo method.
    #' @return Numeric value representing the option price.
    price = function() {
      num_simulations <- 10000
      simulated_prices <- numeric(num_simulations)

      for (i in 1:num_simulations) {
        simulated_prices[i] <- self$underlying_price * exp((self$risk_free_rate - 0.5 * self$volatility^2) * self$time_to_maturity +
                                                             self$volatility * sqrt(self$time_to_maturity) * rnorm(1))
      }

      payoffs <- sapply(simulated_prices, self$payoff_function)

      option_price <- mean(payoffs) * exp(-self$risk_free_rate * self$time_to_maturity)
      return(option_price)
    },
    #' Calculate the Delta of the option.
    #' @return Numeric value representing the Delta.
    delta = function() {
      eps <- 0.01

      # Clone the object and adjust the underlying price
      option_up <- self$clone()
      option_up$underlying_price <- option_up$underlying_price * (1 + eps)
      price_up <- option_up$price()

      option_down <- self$clone()
      option_down$underlying_price <- option_down$underlying_price * (1 - eps)
      price_down <- option_down$price()

      return((price_up - price_down) / (2 * self$underlying_price * eps))
    },

    #' Calculate the gamma of the option.
    #' @return Numeric value representing the gamma
    gamma = function() {
      eps <- 0.01

      # Clone the object and adjust the underlying price for "up" and "down" scenarios
      option_up <- self$clone()
      option_up$underlying_price <- option_up$underlying_price * (1 + eps)
      price_up <- option_up$price()

      option_down <- self$clone()
      option_down$underlying_price <- option_down$underlying_price * (1 - eps)
      price_down <- option_down$price()

      price_current <- self$price()

      return((price_up - 2 * price_current + price_down) / (self$underlying_price * eps)^2)
    },
    #' Calculate the theta of the option.
    #' @return Numeric value representing the theta

    theta = function() {
      eps <- 0.01

      # Clone the object and adjust the time to maturity
      option_later <- self$clone()
      option_later$time_to_maturity <- option_later$time_to_maturity - eps
      price_later <- option_later$price()

      price_now <- self$price()

      return((price_later - price_now) / eps)
    },
    #' Calculate the vega of the option.
    #' @return Numeric value representing the vega

    vega = function() {
      eps <- 0.01

      # Clone the object and adjust the volatility for "up" and "down" scenarios
      option_up <- self$clone()
      option_up$volatility <- option_up$volatility + eps
      price_up <- option_up$price()

      option_down <- self$clone()
      option_down$volatility <- option_down$volatility - eps
      price_down <- option_down$price()

      return((price_up - price_down) / (2 * eps * 100))  # Scaling by 100 as vega represents a 1% change in volatility
    },
    #' Calculate the rho of the option.
    #' @return Numeric value representing the rho


    rho = function() {
      eps <- 0.01

      # Clone the object and adjust the risk-free rate for "up" and "down" scenarios
      option_up <- self$clone()
      option_up$risk_free_rate <- option_up$risk_free_rate + eps
      price_up <- option_up$price()

      option_down <- self$clone()
      option_down$risk_free_rate <- option_down$risk_free_rate - eps
      price_down <- option_down$price()

      return((price_up - price_down) / (2 * eps * 100))  # Scaling by 100 as rho represents a 1% change in risk-free rate
    }
  )
)


