
#user-defined function, to convert from log-odds scale to probability scale
lgistic <- function(theta){
  exp(theta)/(1+exp(theta))
}

#Aim: take a trial structure, and simulate new data.
#Starting pt: One rotation of a 7-hut design. Each net stays in one hut for seven days

df <- data.frame('day' = as.integer(seq(0,343-1,1)/7) + 1)
df$hut <- rep(seq(1,7),49)
df$sleeper <- (df$day + df$hut)%%7
df[df$sleeper==0,]$sleeper <- 7
df$treatment <- NA

trt <- c('C','N1u','N1w','N2u','N2w','N3u','N3w')
trt2 <- rep(trt,2)

for(i in 1:7){
  df[df$day <= (i*7) & df$day > 7*(i-1), ]$treatment <- rep(trt2[i:(i+6)], 7)
}

df$total <- rnbinom(343, size = 2, mu = 10)
mean(df$total)
#Slightly more mosquitoes in control huts?
df[df$treatment=='C',]$total <- rnbinom(49, size = 2, mu = 14)
#Also slightly more mosquitoes in huts with washed nets, compared to those with unwashed nets?
df[df$treatment=='N1w'|df$treatment=='N2w'|df$treatment=='N3w',]$total <- rnbinom(147, size = 2, mu = 12)

df$unf_live <- NA
df$unf_dead <- NA
df$bf_live <- NA
df$bf_dead <- NA

for(i in 1:343){
  tz <- df$total[i]
  ww <- 0 #indicator variable for washed
  if(df$treatment[i]=='N1w' | df$treatment[i]=='N2w' | df$treatment[i]=='N3w'){
    ww <- 1
  }
  ww2 <- 0 #indicator variable for N1
  if(df$treatment[i]=='N1u' | df$treatment[i]=='N1w'){
    ww2 <- 1
  }
  
  if(tz == 0){
    df$unf_live[i] <- 0
    df$unf_dead[i] <- 0
    df$bf_live[i] <- 0
    df$bf_dead[i] <- 0
  }else{
    ntt <- 0
    if(df$treatment[i]=='C'){
      ntt <- 0
    }else ntt <- 1
    #How many died?
    aux <- rbinom(1, tz, lgistic(-2.9 + 2.1*(ntt - 0.55*ww2) - 0.6*ww + 0.15*df$sleeper[i] -0.2*df$hut[i] + rnorm(1,0,0.55)))
    #How many blood fed?
    if(aux == 0){
      df$unf_dead[i] <- 0
      df$bf_dead[i] <- 0
      aux2 <- rbinom(1, tz, lgistic(-0.8 - 0.8*ntt + 0.4*ww + rnorm(1,0,0.25)))
      df$unf_live[i] <- tz - aux2
      df$bf_live[i] <- aux2
    }else{
      aux3 <- rbinom(1,aux,lgistic(-0.8 - 0.8*ntt + 0.4*ww + rnorm(1,0,0.25)))
      df$unf_dead[i] <- aux - aux3
      df$bf_dead[i] <- aux3
      aux4 <- rbinom(1,tz - aux,lgistic(-0.8 - 0.8*ntt + 0.4*ww + rnorm(1,0,0.25)))
      df$unf_live[i] <- tz - aux - aux4
      df$bf_live[i] <- aux4
    }
  }
}

df$tot_dead <- df$unf_dead + df$bf_dead

#save data to RDS?
saveRDS(df, 'data_for_plot.rds')

