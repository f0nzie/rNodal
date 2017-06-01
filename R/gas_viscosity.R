
gas.visc.lee <- function(pressure = 120, temperature = 60, molweight = 19){
  
  p <- pressure
  t <- temperature
  MW <- molweight
  
  TT <- 460 + t
  sg <- MW / 28.9
  Z <- 1 / (1 + (p * 344400 * 10^1.78 * sg / TT^3.82)) # compressibility
  cat("z=", Z)
  rho <- p * MW / (10.7316 * TT * Z) * 0.0160185       # density
  
  K <- 0.0001*(7.77+0.0063 * MW) * TT^1.5 / (122.4+12.9*MW + TT)
  X <- 2.57+1914.5 / TT +0.0095*MW
  Y <- 1.11+0.04*X
  mu <- K * exp(X * rho^Y)
  return(mu)
}

gas.z.stkatz <- function() {
  
  
}

gas.z.lee <- function() {
  
}

gas.z.dranchuk <- function() {
  # Calculates compressibility factor for natural gas based on 
  # Dranchuk and Abou-Kassem equation of state (DAK - EOS).
  
}