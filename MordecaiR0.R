R0temp <- function (T) {
  eps = 1e-12;
  a <- max(0.000203*T*(T-11.7)*(42.3-T)^(1/2),eps);
  bc <- max(-0.54*T^2+25.2*T-206,eps);
  p <- max(-0.000828*T^2+0.0367*T+0.522,eps);
  mu <- -log(p);
  PDR <- max(0.000111*T*(T-14.7)*(34.4-T)^(1/2),eps);
  pEA <- max(-0.00924*T^2+0.453*T-4.77,eps);
  MDR <- max(0.000111*T*(T-14.7)*(34-T)^(1/2),eps);
  EFD <- max(-0.153*T^2+8.61*T-97.7,eps);
  
  N <- 1e6;
  r <- 1/200;
  
  R0n <- a^2*bc*exp(-mu/PDR)*EFD*pEA*MDR; # R0 numerator Mordecai
  R0n1 <- a^2*bc*exp(-mu/PDR*0.79)*EFD*pEA*MDR; # R0 numerator doubleblood
  R0d <- N*r*mu^3; # R0 denominator
  
  R0 <- sqrt(R0n/R0d); # R0 Mordecai
  R01 <- sqrt(R0n1/R0d) # R0 double blood
  
  R0[is.nan(R0)] <- 0
  R01[is.nan(R01)] <- 0
  
  R0out <- c(R0,R01,R01/R0) 
  
  return(R0out)
  # returns R0 Mordecai, R0 double blood, ratio R0 double blood over R0 Mordecai
}


R0temp_new <- function (T) {
  eps = 1e-12;
  a <- max(0.000203*T*(T-11.7)*(42.3-T)^(1/2),eps);
  bc <- max(-0.54*T^2+25.2*T-206,eps);
  p <- max(-0.000828*T^2+0.0367*T+0.522,eps);
  mu <- -log(p);
  PDR <- max(0.000111*T*(T-14.7)*(34.4-T)^(1/2),eps);
  pEA <- max(-0.00924*T^2+0.453*T-4.77,eps);
  MDR <- max(0.000111*T*(T-14.7)*(34-T)^(1/2),eps);
  EFD <- max(-1.908*T^2+100.353*T-1257.111,eps);
  
  N <- 1e6;
  r <- 1/200;
  
  R0n <- a^2*bc*exp(-mu/PDR)*EFD*pEA*MDR; # R0 numerator Mordecai
  R0n1 <- a^2*bc*exp(-mu/PDR*0.79)*EFD*pEA*MDR; # R0 numerator doubleblood
  R0d <- N*r*mu^3; # R0 denominator
  
  R0 <- sqrt(R0n/R0d); # R0 Mordecai
  R01 <- sqrt(R0n1/R0d) # R0 double blood
  
  R0[is.nan(R0)] <- 0
  R01[is.nan(R01)] <- 0
  
  R0out <- c(R0,R01,R01/R0) 
  
  return(R0out)
  # returns R0 Mordecai, R0 double blood, ratio R0 double blood over R0 Mordecai
}


R0temppop <- function (T, N) {
  eps = 1e-12;
  a <- max(0.000203*T*(T-11.7)*(42.3-T)^(1/2),eps);
  bc <- max(-0.54*T^2+25.2*T-206,eps);
  p <- max(-0.000828*T^2+0.0367*T+0.522,eps);
  mu <- -log(p);
  PDR <- max(0.000111*T*(T-14.7)*(34.4-T)^(1/2),eps);
  pEA <- max(-0.00924*T^2+0.453*T-4.77,eps);
  MDR <- max(0.000111*T*(T-14.7)*(34-T)^(1/2),eps);
  EFD <- max(-0.153*T^2+8.61*T-97.7,eps);
  
  r <- 1/200;
  
  R0n <- a^2*bc*exp(-mu/PDR)*EFD*pEA*MDR; # R0 numerator Mordecai
  R0n1 <- a^2*bc*exp(-mu/PDR*0.79)*EFD*pEA*MDR; # R0 numerator doubleblood
  R0d <- N*r*mu^3; # R0 denominator
  
  R0 <- sqrt(R0n/R0d); # R0 Mordecai
  R01 <- sqrt(R0n1/R0d) # R0 double blood
  
  R0[is.nan(R0)] <- 0
  R01[is.nan(R01)] <- 0
  
  R0out <- cbind(R0,R01,R01/R0) 
  return(R0out)
  # returns R0 Mordecai, R0 double blood, ratio R0 double blood over R0 Mordecai
}

