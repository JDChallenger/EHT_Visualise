library(lme4)
#devtools::install_github("pcdjohnson/GLMMmisc")
library(GLMMmisc)
library("optimx")

#user-defined function, to convert from log-odds scale to probability scale
InvLogit <- function(X){
  exp(X)/(1+exp(X))
}

#####################################################
# 1. Info required

#How many trial arms in total? 
tt <- 7
## How many nights should an ITN stay in a hut before the nets are rotated?
nn <- 6 #Must be less than or equal to number of trial arms

#assumed mortality in each trial arm (including control)
mort <- rep(0,tt) #empty list, same length as the number of trial arms
#Fill in your own values!
mort <- c(0.04, 0.25, 0.15, 0.35, 0.25, 0.35, 0.25)

#Do we require that the number of trial arms equal the number of huts/sleepers? Say 'yes'


#Length of trial. 
#Could either describe in terms of number of complete rotations of the LSD, 
rotations <- 1

#How many mosquitoes per night per hut?
meanMos <- 11
dispMos <- 1
#Should this be constant (det=1), or be sampled from a negative binomial distn (det=0)
# with the given mean (meanMos)
det <- 0

simulate_trial <- function(n_arms, npw, mos_det = 0, meanMos = 10, dispMos = 1,
                           rotations = 1, mortalities, varH, varS, varO ){
  #Check length(mortalities) == n_arms
  if(n_arms != length(mortalities)){
    print('Operation was not executed. Check the number of arms, and corresponding vector of mosquito mortalities')
    return(-9)
  }
  if(npw > n_arms){
    print('Operation was not executed. Rethink trial design (npw should be less than or equal to n_arms)')
    return(-9)
  }
  
  n_volunteer <- n_arms
  n_hut <- n_arms
  
  #Make a vector of trial arms
  aux <- c('C',paste0('E',seq(1,n_arms-1)))
  aux2 <- aux
  for(k in 1:(n_arms-1)){
    aux2 <- rbind(aux2,c(aux[(n_arms-k+1):n_arms],aux[1:(n_arms-k)]))
  }
  #aux2
  
  mosdata <-
    expand.grid(
      hut = factor(1:ncol(aux2)),
      week = factor(1:nrow(aux2)),
      night = factor(1:npw)
    )
  mosdata <- mosdata[order(mosdata$hut, mosdata$week, mosdata$night),]
  
  mosdata$net <- NA
  count <- 1
  for(i in 1:n_arms){
    for(j in 1:n_arms){
      mosdata$net[((npw*(count-1))+1):(npw*count)]  <- rep(aux2[i,j],npw)
      count <- count + 1
    }
  }
  #table(mosdata$net, useNA = 'a')
  mosdata$sleeper <- NA
  
  aux3 <- sample(1:7)
  
  for(j in 1:n_arms){
    mosdata[mosdata$hut==j,]$sleeper <- 
                        rep(c(aux3[seq_len(j-1)], aux3[j : n_arms]  ) ,npw)  
  }
  #table(mosdata$sleeper)
  for(i in 1:n_arms){
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
  
  mosdata$n <- NA
  if(det==1){
    mosdata$n <- meanMos
  }else{
    l <- dim(mosdata)[1]
    mosdata$n <- rnbinom(l, mu = meanMos, size = dispMos)
  }
  
  or_vec <- (mortalities[1:tt] / (1-mortalities[1:tt])) / (mortalities[1] / (1-mortalities[1]))
  names(or_vec) <- aux
  
  mosdata <- 
    sim.glmm(design.data = mosdata,
             fixed.eff = list(
               intercept = qlogis(mort[1]),
               net = log(or_vec)),
             rand.V = c(hut = varH, sleeper = varS, observation = varO),
             distribution = "binomial")
  
  return(mosdata)
  
}
#test
simulate_trial(n_arms = 7, npw = 6, mortalities = c(0.04, 0.3,0.2,0.4,0.34,0.4,0.34),
               varH=0.1, varS = 0.2, varO = 1)

hypothesis_test <- function(trial,aoi,NIM=0.7, dataset){ #add dataframe
  count <- 0
  if(trial == 1){
    if(length(aoi) != 2){
      print('Check aoi')
      return(-9)
    }
    #relevel
    dataset$net <- relevel(dataset$net,aux[aoi[1]]) 
    #levels(dataset$net)
    
    fit_model <-
      glmer(
        cbind(response, n - response) ~
          net + (1 | hut) + (1 | sleeper) + (1 | observation),
        family = binomial, data = dataset,  control = 
          glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                    optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
    summary(fit_model)
    
    labl <- paste0('net',aux[6])
    
    if(coef(summary(fit_model))[labl, "Pr(>|z|)"] <0.05 & 
       coef(summary(fit_model))[labl,"Estimate"]>0){
      return(1)
    }else{
      return(0)
    }
    
    count <- count + 1
  }
  if(trial == 2){
    if(length(aoi) != 4){
      print('Check aoi')
      return(-8)
    }
    dataset2 <- dataset
    dataset2$net[dataset2$net ==  aux[aoi[2]] ] <- aux[aoi[1]]
    dataset2$net[dataset2$net ==  aux[aoi[4]] ] <-  aux[aoi[3]]
    table(dataset2$net)
    
    dataset2$net <- relevel(dataset2$net,aux[aoi[1]]) 
    levels(dataset2$net)
    
    fit_model <-
      glmer(
        cbind(response, n - response) ~
          net + (1 | hut) + (1 | sleeper) + (1 | observation),
        family = binomial, data = dataset2, control = 
          glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                    optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
    summary(fit_model)
    
    labl <- paste0('net',aux[aoi[3]])
    
    if(coef(summary(fit_model))[labl, "Pr(>|z|)"] <0.05 & 
       coef(summary(fit_model))[labl,"Estimate"]>0){
      return(1)
    }else{
      return(0)
    }
    
    count <- count + 1
  }
  if(trial == 3){
    if(length(aoi) != 2){
      print('Check aoi')
      return(-7)
    }
    #relevel
    dataset$net <- relevel(dataset$net,aux[aoi[1]]) 
    #check the levels again
    levels(dataset$net)
    
    fit_n <-
      glmer(
        cbind(response, n - response) ~
          net + (1 | hut) + (1 | sleeper) + (1 | observation),
        family = binomial, data = dataset, control = 
          glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                   optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))) 
    summary(fit_n)
    
    labl <- paste0('net', aux[aoi[2]])
    
    exp(coef(summary(fit_n))[labl,'Estimate'])
    exp(coef(summary(fit_n))[labl,'Estimate'] - 1.96*coef(summary(fit_n))[labl,'Std. Error'])
    exp(coef(summary(fit_n))[labl,'Estimate'] + 1.96*coef(summary(fit_n))[labl,'Std. Error'])
    
    if(exp(coef(summary(fit_n))[labl,'Estimate'] -
           1.96*coef(summary(fit_n))[labl,'Std. Error']) > NIM){
      return(1)
    }else{
      return(0)
    }
    
    count <- count + 1
  }
  if(trial == 4){
    if(length(aoi) != 4){
      print('Check aoi')
      return(-6)
    }
    
    dataset2 <- dataset
    dataset2$net[dataset2$net ==  aux[aoi[2]] ] <- aux[aoi[1]]
    dataset2$net[dataset2$net ==  aux[aoi[4]] ] <-  aux[aoi[3]]
    table(dataset2$net)
    
    dataset2$net <- relevel(dataset2$net, aux[aoi[1]]) 
    levels(dataset2$net)
    
    fit_n <-
      glmer(
        cbind(response, n - response) ~
          net + (1 | hut) + (1 | sleeper) + (1 | observation),
        family = binomial, data = dataset2, control = 
          glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                    optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
    summary(fit_n)
    
    labl <- paste0('net',aux[aoi[3]])
    exp(coef(summary(fit_n))[labl,'Estimate'])
    exp(coef(summary(fit_n))[labl,'Estimate'] - 1.96*coef(summary(fit_n))[labl,'Std. Error'])
    exp(coef(summary(fit_n))[labl,'Estimate'] + 1.96*coef(summary(fit_n))[labl,'Std. Error'])
    
    if(exp(coef(summary(fit_n))[labl,'Estimate'] -
           1.96*coef(summary(fit_n))[labl,'Std. Error']) > NIM){
      return(1)
    }else{
      return(0)
    }
    
    count <- count + 1
  }
  if(count<1){
    return('Check value of trial (analyses not carried out)')
  }#else{
    #placeholder
    #count <- count + 1
  #}
  return('All quiet?')
}
#test
hypothesis_test(trial = 1, aoi = c(4,5), dataset = mosdata)
#Is this ok?
hypothesis_test(trial = 1, aoi = c(4,5), 
                dataset = simulate_trial(n_arms = 7, npw = 7, 
                                mortalities = c(0.04, 0.3,0.2,0.4,0.34,0.4,0.34),
                                         varH=0.1, varS = 0.2, varO = 1))

#############################################################
# Parallise code (Mac version)
library(parallel)
nsim <- 1000
ncores <- detectCores() - 1
t1 <- Sys.time()
sz <- mclapply(1:nsim, function(...) hypothesis_test(trial = 1, aoi = c(4,6), 
                        dataset = simulate_trial(n_arms = 7, npw = 7, 
                           mortalities = c(0.04, 0.3,0.2,0.4,0.2,0.49,0.34),
                               varH=0.1, varS = 0.2, varO = .3)))
t2 <- Sys.time()
t2 - t1
mm <- unlist(sz)
mean(mm)

print(paste0('Power Estimate: ',100*sum(mm)/length(mm),'%'))
#binom.test(table(factor(sim.pvals < 0.05, c(T, F))))$conf.int
#How many simulations is enough? Depends how much precision you need. 
#Let's look at the 95% CIs for the power estiamte
binom.test(table(factor(mm,c(1,0))))$conf.int
#Put all that together
print(paste0('Power Estimate: ',100*sum(mm)/length(mm),'%, 95% CI: [',
             round(binom.test(table(factor(mm,c(1,0))))$conf.int[1],3),',',
             round(binom.test(table(factor(mm,c(1,0))))$conf.int[2],3),']'))

#############################################################
# Parallise code (Windows version)
# ncores <- detectCores() - 1
# cl <- makeCluster(ncores)
# clusterExport(cl,'hypothesis_test')
# clusterExport(cl,'simulate_trial')
# clusterEvalQ(cl, {
#   library(lme4)
#   library(GLMMmisc)
#   library("optimx")
# })
# save3 <- parLapply(cl, 1:nsim, function(...) hypothesis_test(trial = 1, aoi = c(4,6), 
#             dataset = simulate_trial(n_arms = 7, npw = 7, 
#                         mortalities = c(0.04, 0.3,0.2,0.4,0.2,0.49,0.34),
#                             varH=0.1, varS = 0.2, varO = .3)))


