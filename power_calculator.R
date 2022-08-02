#

# What type of trial is it? superiority trial (trial=1); non-inferiority trial (trial=2)
#Or extend =3 & =4 to say whether washed & unwashed nets should be combined
#prior to testing the null hypothesis

# 1. Superiority between two trial arms
# 2. Superiority between two ITNs (combining data from washed & unwashed ITNs of two types i.e. involving 4 trial arms)
# 3. Non-inferiority between two trial arms
# 4. Non-inferiority between two ITNs (combining data from washed & unwashed ITNs of two types i.e. involving 4 trial arms)

trial <- 1

#There may be multiple nets in the trial. For superiority or non-inferiority, we need
#to specify which two arms are of interest (a.o.i)

#Trial may contain multiple products. We'll need to specify which arms are
# involved in the hypothesis testing
aoi1 <- c(4,6) # We'll test whether the latter arm is superior to the former
# In order, these should be ITN1 (unwashed), ITN1 (washed), ITN2 (unwashed), 
#ITN2 (washed). We'll test whether ITN2 is superior to ITN1
aoi2 <- c(4,5,6,7)
aoi3 <- c(4,6) # We'll test whether the latter arm is non-inferior to the former
# In order, these should be ITN1 (unwashed), ITN1 (washed), ITN2 (unwashed), 
#ITN2 (washed). We'll test whether ITN2 is non-inferior to ITN1
aoi4 <- c(4,5,6,7)

#May need a non-inferiority margin
NIM <- 0.7

#How many trial arms in total? 
tt <- 7
## How many nights should an ITN stay in a hut before the nets are rotated?
nn <- 6

#assumed mortality in each trial arm (including control)
mort <- rep(0,tt) #empty list, same length as the number of trial arms
#Fill in your own values!
mort <- c(0.04, 0.25, 0.15, 0.35, 0.25, 0.35, 0.25)

#Do we require that the number of trial arms equal the number of huts/sleepers?
n_volunteer <- tt
n_hut <- tt

#Length of trial. 
#Could either describe in terms of number of complete rotations of the LSD, 
rotations <- 1

#How many mosquitoes per night per hut?
meanMos <- 11
dispMos <- 1
#Should this be constant (det=1), or be sampled from a negative binomial distn (det=0)
# with the given mean (meanMos)
det <- 0


#Make a vector of trial arms
aux <- c('C',paste0('E',seq(1,tt-1)))
aux2 <- aux
for(k in 1:(tt-1)){
 aux2 <- rbind(aux2,c(aux[(tt-k+1):tt],aux[1:(tt-k)]))
}
aux2

mosdata <-
  expand.grid(
    hut = factor(1:ncol(aux2)),
    week = factor(1:nrow(aux2)),
    night = factor(1:nn)
)
mosdata <- mosdata[order(mosdata$hut, mosdata$week, mosdata$night),]

mosdata$net <- NA
count <- 1
for(i in 1:tt){
  for(j in 1:tt){
   mosdata$net[((nn*(count-1))+1):(nn*count)]  <- rep(aux2[i,j],nn)
   count <- count + 1
  }
}
table(mosdata$net, useNA = 'a')
mosdata$sleeper <- NA
#mosdata[mosdata$hut==1,]$sleeper <- rep(seq(1,tt),nn)

#mosdata[order(mosdata$week, mosdata$night),]

#mosdata2 <- mosdata[mosdata$hut==2,]
#mosdata2[order(mosdata2$week, mosdata2$night),]

aux3 <- sample(1:7)
# for(i in 1:(tt-1)){
#   aux3 <- rbind(aux3, sample(1:7))
# }

for(j in 1:tt){
  mosdata[mosdata$hut==j,]$sleeper <- rep(c(aux3[seq_len(j-1)], aux3[j : tt]  ) ,nn)  
  #print(c( j : tt , seq_len(j-1)  ) ,nn)
  print(c( aux3[j : tt] , aux3[seq_len(j-1)]  ))
  print('/n')
}
table(mosdata$sleeper)

for(i in 1:tt){
  print(paste0('Treatment is: ',aux[i]))
  mosdata2 <- mosdata[mosdata$net==aux[i],]
  print(table(mosdata2$hut))
  print(table(mosdata2$sleeper))
  print(table(mosdata2$week))
}

#Now make a unique identifier for each data point
mosdata$observation <- factor(formatC(1:nrow(mosdata), flag="0", width=3))
mosdata$sleeper <- factor(mosdata$sleeper)
mosdata$net <- factor(mosdata$net)

#Are we ready to simulate?

mosdata$n <- NA
if(det==1){
  mosdata$n <- meanMos
}else{
  l <- dim(mosdata)[1]
  mosdata$n <- rnbinom(l, mu = meanMos, size = dispMos)
}
hist(mosdata$n)

#OR1 <- (0.3/(1-0.3))/(0.05/(1-0.05))
#OR2 <- (0.4/(1-0.4))/(0.05/(1-0.05))
or_vec <- (mort[1:tt] / (1-mort[1:tt])) / (mort[1] / (1-mort[1]))
names(or_vec) <- aux

dfvv <- data.frame(or_vec)

mosdata <- 
  sim.glmm(design.data = mosdata,
           fixed.eff = list(
             intercept = qlogis(mort[1]),
             net = log(or_vec),  #log(c(C = 1, E1 = OR1, E2 = OR2, E3 = OR2,
                    #     E4 = OR2, E5 = OR2, E6 = OR2))),
           rand.V = c(hut = 0.3, sleeper = 0.3, observation = 0.8),
           distribution = "binomial")





