## Script for calculation of model preformance measures
## First created: 4 Mar 2019
## Last changed: 3 Apr 2019 
## Creator: Amelie Schmolke
## Reference: notebook entry 4 Mar, 28 Mar 2019 (bias calculation extended), 3 Apr 2019 (RSR added)

# Calculation of all measures
GOFM <- function(O, P, O_low, O_high, P_low, P_high){
  if( length(O) != length(P) | length(O) != length(O_low) 
      | length (O) != length(O_high) | length (O) != length(P_low)
      | length (O) != length(P_high)){
    Warnings("Vectors not of same length!")
  }
  c(e_bias(O,P), e_bias_rel(O,P), 
    mae(O,P), mae(O,P)/mean(O), 
    rmse(O,P), rmse(O,P)/mean(O), 
    nse(O,P), rsr(O,P),
    e_bias_range(O_low, O_high, P_low, P_high), 
    mae_range(O_low, O_high, P), mae_range(O_low, O_high, P)/mean(O),
    rmse_range(O_low,O_high,P), rmse_range(O_low,O_high,P)/mean(O), 
    nse_range(O_low,O_high,P), rsr_range(O_low,O_high,P),
    I_overlap(O_low, O_high, P_low, P_high), 
    adequacy(O_low, O_high, P_low, P_high), 
    reliability(O_low, O_high, P_low, P_high))
}

# Root mean square error (RMSE): mean CCA data vs. mean BEEHAVE outputs
rmse <- function(O, P){
  N <- length(O)
  s <- 0
  for(i in 1:N){s <- s + (O[i] - P[i])^2}
  sqrt(s/N)
}

# Root mean square error (RMSE): range of CCA data vs. mean BEEHAVE outputs
rmse_range <- function(O_low, O_high, P){
  N <- length(O_low)
  s <- 0
  for(i in 1:N){
    if(P[i] < O_low[i]){ s <- s + (O_low[i] - P[i])^2 }
    else{
      if(P[i] > O_high[i]){ s <- s + (O_high[i] - P[i])^2 }
    }
  }
  sqrt(s/N)
}

# Mean absolute error (MAE): mean CCA data vs. mean BEEHAVE outputs
mae <- function(O, P){
  N <- length(O)
  s <- 0
  for(i in 1:N){s <- s + abs(O[i] - P[i])}
  s/N
}

# Mean absolute error (MAE): range of CCA data vs. mean BEEHAVE outputs
mae_range <- function(O_low, O_high, P){
  N <- length(O_low)
  s <- 0
  for(i in 1:N){
    if(P[i] < O_low[i]){ s <- s + abs(O_low[i] - P[i]) }
    else{
      if(P[i] > O_high[i]){ s <- s + abs(O_high[i] - P[i]) }
    }
  }
  s/N
}

# Nash-Sutcliff coefficient of efficiency (NSE): mean CCA data vs. mean BEEHAVE outputs
nse <- function(O, P){
  N <- length(O)
  mean_O <- mean(O)
  s1 <- 0
  s2 <- 0
  for(i in 1:N){
    s1 <- s1 + (O[i] - P[i])^2
    s2 <- s2 + (O[i] - mean_O)^2
  }
  1 - (s1/s2)
}

# Nash-Sutcliff coefficient of efficiency (NSE): range of CCA data vs. mean BEEHAVE outputs
nse_range <- function(O_low, O_high, P){
  N <- length(O_low)
  s1 <- 0
  s2 <- 0
  for(i in 1:N){
    if(P[i] < O_low[i]){ s1 <- s1 + (O_low[i] - P[i])^2 }
    else{
      if(P[i] > O_high[i]){ s1 <- s1 + (O_high[i] - P[i])^2}
    }
    s2 <- s2 + ((O_high[i] - O_low[i])/2)^2
  }
  1 - (s1/s2)
}

# RMSE-observed standard deviation ratio (RSR): mean CCA data vs. mean BEEHAVE outputs
rsr <- function(O, P){
  N <- length(O)
  mean_O <- mean(O)
  s1 <- 0
  s2 <- 0
  for(i in 1:N){
    s1 <- s1 + (O[i] - P[i])^2
    s2 <- s2 + (O[i] - mean_O)^2
  }
  sqrt(s1)/sqrt(s2)
}

# RMSE-observed standard deviation ratio (RSR): range of CCA data vs. mean BEEHAVE outputs
rsr_range <- function(O_low, O_high, P){
  N <- length(O_low)
  s1 <- 0
  s2 <- 0
  for(i in 1:N){
    if(P[i] < O_low[i]){ s1 <- s1 + (O_low[i] - P[i])^2 }
    else{
      if(P[i] > O_high[i]){ s1 <- s1 + (O_high[i] - P[i])^2}
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
I_overlap <- function(O_low, O_high, P_low, P_high){
  N <- length(O_low)
  I <- 0
  for(i in 1:N){
    I <- min(O_high[i],P_high[i]) - max(O_low[i],P_low[i])
    if(I < 0){ I <- 0 }
  }
  I/N
}

# Model adequacy calculated from area overlap: range of CCA data vs. mean BEEHAVE outputs
adequacy <- function(O_low, O_high, P_low, P_high){
  N <- length(O_low)
  s <- 0
  for(i in 1:N){
    O_spread <- O_high[i] - O_low[i]
    I <- I_overlap(O_low[i], O_high[i], P_low[i], P_high[i])
    s <- s + I/O_spread
  }
  s/N
}

# Model reliability calculated from area overlap: range of CCA data vs. mean BEEHAVE outputs
reliability <- function(O_low, O_high, P_low, P_high){
  N <- length(O_low)
  s <- 0
  for(i in 1:N){
    P_spread <- P_high[i] - P_low[i]
    I <- I_overlap(O_low[i], O_high[i], P_low[i], P_high[i])
    s <- s + I/P_spread
  }
  s/N
}

