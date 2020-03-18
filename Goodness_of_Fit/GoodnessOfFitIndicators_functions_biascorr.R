## Script for calculation of model preformance measures with bias corrected
## First created: 28 Mar 2019
## Last changed: 28 Mar 2019 
## Creator: Amelie Schmolke
## Reference: notebook entry 28 Mar 2019

# Calculation of all measures
GOFM_bias <- function(O, P, O_low, O_high, P_low, P_high){
  if( length(O) != length(P) | length(O) != length(O_low) 
      | length (O) != length(O_high) | length (O) != length(P_low)
      | length (O) != length(P_high)){
    Warnings("Vectors not of same length!")
  }
  bias <- e_bias(O,P)
  bias_range <- e_bias_range(O_low, O_high, P_low, P_high)
  c(bias, e_bias_rel(O,P), 
    mae(O,P,bias), mae(O,P,bias)/mean(O), 
    rmse(O,P,bias), rmse(O,P,bias)/mean(O), 
    nse(O,P,bias), rsr(O,P,bias),
    bias_range, 
    mae_range(O_low, O_high, P,bias_range), mae_range(O_low, O_high, P,bias_range)/mean(O),
    rmse_range(O_low,O_high,P,bias_range), rmse_range(O_low,O_high,P,bias_range)/mean(O), 
    nse_range(O_low,O_high,P,bias_range), rsr_range(O_low,O_high,P,bias_range),
    I_overlap(O_low, O_high, P_low, P_high,bias_range), 
    adequacy(O_low, O_high, P_low, P_high,bias_range), 
    reliability(O_low, O_high, P_low, P_high,bias_range))
}

# Root mean square error: mean CCA data vs. mean BEEHAVE outputs
rmse <- function(O, P, bias){
  N <- length(O)
  s <- 0
  for(i in 1:N){s <- s + (O[i] - (P[i]-bias))^2}
  sqrt(s/N)
}

# Root mean square error: range of CCA data vs. mean BEEHAVE outputs
rmse_range <- function(O_low, O_high, P, bias_range){
  N <- length(O_low)
  s <- 0
  for(i in 1:N){
    if(P[i] < O_low[i]){ s <- s + (O_low[i] - (P[i]-bias_range))^2 }
    else{
      if(P[i] > O_high[i]){ s <- s + (O_high[i] - (P[i]-bias_range))^2 }
    }
  }
  sqrt(s/N)
}

# Mean absolute error: mean CCA data vs. mean BEEHAVE outputs
mae <- function(O, P, bias){
  N <- length(O)
  s <- 0
  for(i in 1:N){s <- s + abs(O[i] - (P[i]-bias))}
  s/N
}

# Mean absolute error: range of CCA data vs. mean BEEHAVE outputs
mae_range <- function(O_low, O_high, P, bias_range){
  N <- length(O_low)
  s <- 0
  for(i in 1:N){
    if(P[i] < O_low[i]){ s <- s + abs(O_low[i] - (P[i]-bias_range)) }
    else{
      if(P[i] > O_high[i]){ s <- s + abs(O_high[i] - (P[i]-bias_range)) }
    }
  }
  s/N
}

# Nash-Sutcliff coefficient of efficiency: mean CCA data vs. mean BEEHAVE outputs
nse <- function(O, P, bias){
  N <- length(O)
  mean_O <- mean(O)
  s1 <- 0
  s2 <- 0
  for(i in 1:N){
    s1 <- s1 + (O[i] - (P[i]-bias))^2
    s2 <- s2 + (O[i] - mean_O)^2
  }
  1 - (s1/s2)
}

# Nash-Sutcliff coefficient of efficiency: range of CCA data vs. mean BEEHAVE outputs
nse_range <- function(O_low, O_high, P, bias_range){
  N <- length(O_low)
  s1 <- 0
  s2 <- 0
  for(i in 1:N){
    if((P[i]-bias_range) < O_low[i]){ s1 <- s1 + (O_low[i] - (P[i]-bias_range))^2 }
    else{
      if((P[i]-bias_range) > O_high[i]){ s1 <- s1 + (O_high[i] - (P[i]-bias_range))^2}
    }
    s2 <- s2 + ((O_high[i] - O_low[i])/2)^2
  }
  1 - (s1/s2)
}

# RMSE-observed standard deviation ratio (RSR): mean CCA data vs. mean BEEHAVE outputs
rsr <- function(O, P, bias){
  N <- length(O)
  mean_O <- mean(O)
  s1 <- 0
  s2 <- 0
  for(i in 1:N){
    s1 <- s1 + (O[i] - (P[i]-bias))^2
    s2 <- s2 + (O[i] - mean_O)^2
  }
  sqrt(s1)/sqrt(s2)
}

# RMSE-observed standard deviation ratio (RSR): range of CCA data vs. mean BEEHAVE outputs
rsr_range <- function(O_low, O_high, P, bias_range){
  N <- length(O_low)
  s1 <- 0
  s2 <- 0
  for(i in 1:N){
    if((P[i]-bias_range) < O_low[i]){ s1 <- s1 + (O_low[i] - (P[i]-bias_range))^2 }
    else{
      if((P[i]-bias_range) > O_high[i]){ s1 <- s1 + (O_high[i] - (P[i]-bias_range))^2}
    }
    s2 <- s2 + ((O_high[i] - O_low[i])/2)^2
  }
  sqrt(s1)/sqrt(s2)
}

# Calculation of bias: mean CCA data vs. mean BEEHAVE outputs
e_bias <- function(O, P){
  N <- length(O)
  s <- 0
  for(i in 1:N){s <- s + (P[i] - O[i])}
  s/N
}

# Calculation of relative bias: mean CCA data vs. mean BEEHAVE outputs; devided by overall mean of O
e_bias_rel <- function(O, P){
  N <- length(O)
  mean_O <- mean(O)
  s <- 0
  for(i in 1:N){s <- s + (P[i] - O[i])/mean_O}
  s/N
}

# Calculation of bias: mean of range of CCA data vs. mean BEEHAVE outputs
e_bias_range <- function(O_low, O_high, P_low, P_high){
  N <- length(O_low)
  s <- 0
  for(i in 1:N){
    s <- s + (P_low[i]+P_high[i])/2 - (O_low[i]+O_high[i])/2
  }
  s/N
}

# Area overlap: range of CCA data vs. mean BEEHAVE outputs
I_overlap <- function(O_low, O_high, P_low, P_high, bias_range){
  N <- length(O_low)
  I <- 0
  for(i in 1:N){
    I <- min(O_high[i],(P_high[i]-bias_range)) - max(O_low[i],(P_low[i]-bias_range))
    if(I < 0){ I <- 0 }
  }
  I/N
}

# Model adequacy calculated from area overlap: range of CCA data vs. mean BEEHAVE outputs
adequacy <- function(O_low, O_high, P_low, P_high, bias_range){
  N <- length(O_low)
  s <- 0
  for(i in 1:N){
    O_spread <- O_high[i] - O_low[i]
    I <- I_overlap(O_low[i], O_high[i], P_low[i], P_high[i], bias_range)
    s <- s + I/O_spread
  }
  s/N
}

# Model reliability calculated from area overlap: range of CCA data vs. mean BEEHAVE outputs
reliability <- function(O_low, O_high, P_low, P_high, bias_range){
  N <- length(O_low)
  s <- 0
  for(i in 1:N){
    P_spread <- P_high[i] - P_low[i]
    I <- I_overlap(O_low[i], O_high[i], P_low[i], P_high[i], bias_range)
    s <- s + I/P_spread
  }
  s/N
}

