# Source for ZCB pricing: 
# http://www.diva-portal.org/smash/get/diva2:300991/FULLTEXT01.pdf
# Good Spreadsheet:
# www.stat.fsu.edu/~jfrade/HOMEWORKS/STA5167/.../Bond1.xls
# http://www.stat.fsu.edu/~jfrade/HOMEWORKS/STA5167/project2/project-final2/project/book_files/
#
# MAtlab parameterization:
# http://dsp.vscht.cz/konference_matlab/MATLAB07/prispevky/kladivko_k/kladivko_k.pdf
require(ggplot2)
require(reshape2)

CIRSimulation <- function(N, r0, kappa, theta, sigma, dt = 1/252) {
  # Method for simulating a single CIR run. Uses the Feller Condition. 
  # Source: http://www.thetaris.com/wiki/CIR_model
  #
  # Args: 
  #   N: The number of points to generate in each simulation. For example, 
  #      the number of days when simulating daily rates.
  #   r0: The initial interest rate. 
  #   kappa: The mean reversion rate. 
  #   theta: The mean rate or long term rate. 
  #   sigma: Volatility. 
  #   dt: The change in time between observations. Defaults to 1/252 because
  #       we assume generation of daily rates and there are 252 trading days 
  #       per year. 
  #
  # Returns:
  #   A vector of simulated short rates. 
  short.rates <- rep(0, N)
  short.rates[1] <- r0
  for (i in 2:N) {
    Z = rnorm(n = 1)
    if (4 * kappa * theta > sigma^2) { # Feller Condition
      term1 <- 1 / (1 + kappa*dt)
      term2 <- short.rates[i - 1] + kappa*theta*dt
      term3 <- sigma*sqrt(short.rates[i - 1])*sqrt(dt)*Z
      term4 <- 1/4*sigma^2*dt*(Z^2 - 1)
      short.rates[i] <- term1 * (term2 + term3 + term4)    
    } else {
      term1 <- kappa*(theta - max(short.rates[i - 1], 0))*dt
      term2 <- sigma*sqrt(max(short.rates[i - 1]))*sqrt(dt)*Z
      short.rates[i] <- short.rates[i - 1] + term1 + term2
    }
  }
  return(short.rates)
}

CIRSimulations <- function(M, N, r0, kappa, theta, sigma, dt = 1/252) {
  # Generates several runs of the CIR model.
  # 
  # Args: 
  #   M: The number of simulations to run. 
  #   N: The number of points to generate in each simulation. For example, 
  #      the number of days when simulating daily rates.
  #   r0: The initial interest rate. 
  #   kappa: The mean reversion rate. 
  #   theta: The mean rate or long term rate. 
  #   sigma: Volatility. 
  #   dt: The change in time between observations. Defaults to 1/252 because
  #       we assume generation of daily rates and there are 252 trading days 
  #       per year. 
  #
  # Returns:
  #   An N row by M column matrix of simulated short rates. 
  sim.mat <- matrix(nrow = N, ncol = M)
  for (i in 1:M) {
    sim.mat[, i] <- CIRSimulation(N, r0, kappa, theta, sigma, dt)
  }
  return(sim.mat)
}

CIRZeroCouponBondPrice <- function(r0, kappa, theta, sigma, years) {
  # Calculates th zero coupon bond price. 
  #
  # Args: 
  #   r0: The initial interest rate. 
  #   kappa: The mean reversion rate. 
  #   theta: The mean rate or long term rate. 
  #   sigma: Volatility. 
  #   years: The length or maturity of the bond.  
  #
  # Returns:
  #   A decimal price of the bond (i.e. 0.98 for 98). 
  h <- sqrt(kappa^2 + 2 * sigma^2)
  denominator <- 2*h + (kappa + h)*(exp(years*h) - 1)
  A <- (2*h*exp((kappa + h)*years/2) / denominator)^(2*kappa*theta/sigma^2)
  B <- 2*(exp(years*h) - 1) / denominator
  return(A * exp(-B * r0))
}

CIRYieldCurve <- function(r0, kappa, theta, sigma, max.maturity=10) {
  # Produces a yield curve from the CIR model with maturities ranging 
  # from 1 year to max.maturity.  
  #
  # Args: 
  #   r0: The initial interest rate. 
  #   kappa: The mean reversion rate. 
  #   theta: The mean rate or long term rate. 
  #   sigma: Volatility. 
  #   max.maturity: Maximum maturity in years (must be integer).
  #
  # Returns:
  #   A list of yields/spot rates that make up the yield curve.
  max.maturity <- as.integer(max.maturity)
  yields <- rep(0, max.maturity)
  for (y in 1:max.maturity) {
    yields[y] <- -log(CIRZeroCouponBondPrice(r0, kappa, theta, sigma, y))/y
  }
  return(yields)
}

PlotShorts <- function(M, N, r0, kappa, theta, sigma) {
  # Plots several runs of the CIR model.
  # 
  # Args: 
  #   M: The number of simulations to run. 
  #   N: The number of points to generate in each simulation. For example, 
  #      the number of days when simulating daily rates.
  #   r0: The initial interest rate. 
  #   kappa: The mean reversion rate. 
  #   theta: The mean rate or long term rate. 
  #   sigma: Volatility. 
  #
  # Returns:
  #   Nothing. Plots directly. 
  t <- (1:N)/252
  test.mat <- CIRSimulations(M, N, r0, kappa, theta, sigma)
  expected <- r0*exp(-kappa*t) + theta*(1 - exp(-kappa*t))
  stdev <- sqrt(r0*sigma^2/kappa*(exp(-kappa*t) - exp(-2*kappa*t)) 
                + theta*sigma^2/(2*kappa)*(1 - exp(-kappa*t))^2)
  df <- data.frame(t, test.mat, mu=expected, sig1=expected + 2*stdev, sig2=expected - 2*stdev)
  df <- melt(df, id=c('t', 'mu', 'sig1', 'sig2'))
  g <- ggplot(df, aes(x=t, y=value, colour=variable)) + geom_line() + theme(legend.position="none") + xlab('Year') + ylab('Short Rate')
  g <- g + geom_line(aes(x=t, y=mu), colour='grey0', linetype=3) + geom_line(aes(x=t, y=sig1), colour='grey0', linetype=3) + geom_line(aes(x=t, y=sig2), colour='grey0', linetype=3)
  g
}
