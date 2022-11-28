library(lme4)
#devtools::install_github("pcdjohnson/GLMMmisc")
library(GLMMmisc)
library("optimx") #Standard installation??
#library(ggplot2)
library(parallel)

#user-defined function, to convert from log-odds scale to probability scale
InvLogit <- function(X){
  exp(X)/(1+exp(X))
}

simulate_trial <- function(n_arms, npw, mos_det = 0, meanMos = 10, dispMos = 1,
                           rotations = 1, mortalities, varH = 0, varS = 0, varO = 0.8){
  #Check length(mortalities) == n_arms
  if(n_arms != length(mortalities)){
    print('Operation was not executed. Check the number of arms, and corresponding vector of mosquito mortalities')
    return(-9)
  }
  if(npw > n_arms){
    print('Operation was not executed. Rethink trial design (npw should be less than or equal to n_arms)')
    return(-9)
  }
  if(rotations < 1){ #At the moment, we need at least 1 rotation
    rotations <- 1
  }
  if(varS < 0 | varH <0 | varO < 0 ){ #At the moment, we need at least 1 rotation
    print('The variance of each random effect must be positive')
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
      week = 1:nrow(aux2),
      night = 1:npw
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
  #Do we have rotation > 1? What if rotation > 2??
  if(rotations > 1){
    if(rotations <= 2){
      mosdata$day <- npw*(mosdata$week - 1) + mosdata$night
      extra <- round((rotations - 1)*dim(mosdata)[1] / n_arms)
      mosdataX <- mosdata[mosdata$day <= extra,]
      mosdataX$day <- mosdataX$day + max(mosdata$day)
      mosdataX$week <- mosdataX$week + max(mosdata$week)
      #Now add it on to the existing trial design
      mosdata <- rbind(mosdata,mosdataX)
    }else{
      mosdata$day <- npw*(mosdata$week - 1) + mosdata$night
      intt <- as.integer(rotations - 1)
      #print(paste0('intt: ',intt))
      dec <- rotations - intt - 1
      mosdataY <- mosdata
      mosdataZ <- mosdata
      for(j in 1:floor(intt)){
        mosdataY$day <- mosdataZ$day + max(mosdataZ$day)*j
        mosdataY$week <- mosdataZ$week + max(mosdataZ$week)*j
        mosdata <- rbind(mosdata, mosdataY)
      }
      #print(paste0('dim is ',dim(mosdata)[1]))
      extra <- round(dec*dim(mosdataZ)[1] / n_arms)
      #print(paste0('extra: ',extra))
      mosdataX <- mosdataZ[mosdataZ$day <= extra,]
      mosdataX$day <- mosdataX$day + max(mosdata$day)
      mosdataX$week <- mosdataX$week + max(mosdata$week)
      #print(paste0('dimX is ',dim(mosdataX)[1]))
      #Now add it on to the existing trial design
      mosdata <- rbind(mosdata,mosdataX)
    }
    
  }
  
  #Now make a unique identifier for each data point
  mosdata$observation <- factor(formatC(1:nrow(mosdata), flag="0", width=3))
  mosdata$sleeper <- factor(mosdata$sleeper)
  mosdata$net <- factor(mosdata$net)
  
  mosdata$n <- NA
  if(mos_det==1){
    mosdata$n <- meanMos
  }else{
    l <- dim(mosdata)[1]
    mosdata$n <- rnbinom(l, mu = meanMos, size = dispMos)
  }
  
  or_vec <- (mortalities[1:n_arms] / (1-mortalities[1:n_arms])) / (mortalities[1] / (1-mortalities[1]))
  names(or_vec) <- aux
  
  mosdata <- 
    sim.glmm(design.data = mosdata,
             fixed.eff = list(
               intercept = qlogis(mortalities[1]),
               net = log(or_vec)),
             rand.V = c(hut = varH, sleeper = varS, observation = varO),
             distribution = "binomial")
  #print(dim(mosdata))
  #print(rotations)
  return(mosdata)
  
}
#test
# xc <- simulate_trial(n_arms = 7, npw = 5, mortalities = c(0.04, 0.3,0.2, 0.3,0.2, 0.3,0.2),
#                varH=0.1, varS = 0.2, varO = 1)
# dim(xc)
# head(xc)
# table(xc$net)
# table(xc$hut)
# table(xc$sleeper)

hypothesis_test <- function(trial,aoi,NIM=0.7, dataset){ 
  count <- 0
  
  l <- length(unique(dataset$net))
  aux <- c('C',paste0('E',seq(1,l-1)))
  
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
  #return('All quiet?')
}
#test
#hypothesis_test(trial = 1, aoi = c(3,4), dataset = xc)
#Is this ok?
#hypothesis_test(trial = 1, aoi = c(4,6), 
#                dataset = simulate_trial(n_arms = 7, npw = 7, 
#                                mortalities = c(0.04, 0.3,0.2,0.4,0.34,0.45,0.34),
#                                         varH=0.1, varS = 0.2, varO = 1))

#############################################################
# Parallise code (Mac version)
# library(parallel)
# nsim <- 100
# ncores <- detectCores() - 1
# t1 <- Sys.time()
# sz <- mclapply(1:nsim, function(...) hypothesis_test(trial = 1, aoi = c(4,6), 
#                         dataset = simulate_trial(n_arms = 7, npw = 5, rotations = 1, 
#                            mortalities = c(0.04, 0.3,0.2,0.4,0.2,0.49,0.34),
#                                varH=0.1, varS = 0.2, varO = .5)))
# t2 <- Sys.time()
# t2 - t1
# mm <- unlist(sz)
# mean(mm)
# 
# print(paste0('Power Estimate: ',100*sum(mm)/length(mm),'%'))
# #binom.test(table(factor(sim.pvals < 0.05, c(T, F))))$conf.int
# #How many simulations is enough? Depends how much precision you need. 
# #Let's look at the 95% CIs for the power estiamte
# binom.test(table(factor(mm,c(1,0))))$conf.int
# #Put all that together
# print(paste0('Power Estimate: ',100*sum(mm)/length(mm),'%, 95% CI: [',
#              round(binom.test(table(factor(mm,c(1,0))))$conf.int[1],3),',',
#              round(binom.test(table(factor(mm,c(1,0))))$conf.int[2],3),']'))

#nc <- detectCores() - 1
power_calculator <- function(parallelise = 0, trialX = 1, npwX = 6, rotationsX = 1, varO1 = 1, 
                 nsim1 = 1000, n_armsX = 7, mos_detX = 0, meanMosX = 10, dispMosX = 1,
                 aoiX = c(4,6), mortalitiesX){
  if(parallelise!=0 & parallelise!=1 & parallelise!=2){
    print('Operation was not executed. Parallelise must take a value of (i) 0 (code not parallelised); (ii) Parallelised for Windows; (iii) Parallilised for Mac. If in doubt, set to zero')
    return(-9)
  }
  nsim <- nsim1
  if(parallelise==0){#|parallelise==1){
    #t1 <- Sys.time()
    sz <- lapply(1:nsim1, function(...) hypothesis_test(trial = trialX, aoi = aoiX, 
                                                          dataset = simulate_trial(n_arms = n_armsX, npw = npwX, 
                                                                                   rotations = rotationsX, mos_det = mos_detX, meanMos = meanMosX, 
                                                                                   dispMos = dispMosX, mortalities = mortalitiesX,
                                                                                   varH=0.1, varS = 0.2, varO = varO1)))
    #t2 <- Sys.time()
    #t2 - t1
  }
  if(parallelise==1){ # Under construction
    ncores <- detectCores() - 1
    
    n_armsY <- n_armsX
    trialY <- trialX
    aoiY <- aoiX
    dispMosY <- dispMosX
    meanMosY <- meanMosX
    varOY <- varO1
    mortalitiesY <- mortalitiesX
    npwY <- npwX
    #print(npwY)
    mos_detY <- mos_detX
    rotationsY <- rotationsX
    
    cl <- makeCluster(ncores)
    clusterExport(cl,'npwY', envir = environment())
    clusterExport(cl,'rotationsY', envir = environment())
    clusterExport(cl,'mortalitiesY', envir = environment())
    clusterExport(cl,'varOY', envir = environment())
    clusterExport(cl,'mos_detY', envir = environment())
    clusterExport(cl,'meanMosY', envir = environment())
    clusterExport(cl,'dispMosY', envir = environment())
    clusterExport(cl,'n_armsY', envir = environment())
    clusterExport(cl,'aoiY', envir = environment())
    clusterExport(cl,'trialY', envir = environment())
    clusterExport(cl,'hypothesis_test')
    clusterExport(cl,'simulate_trial')
    clusterExport(cl,'lazy')
    clusterExport(cl,'InvLogit')
    clusterEvalQ(cl, {
      library(lme4)
      library(GLMMmisc)
      library("optimx")
    })
    #t1 <- Sys.time()
    sz <- parLapply(cl, 1:nsim, function(...) hypothesis_test(trial = trialY, aoi = aoiY,
                                                              dataset = simulate_trial(n_arms = n_armsY, npw = npwY,
                                                                                       rotations = rotationsY, mos_det = mos_detY, meanMos = meanMosY,
                                                                                       dispMos = dispMosY, mortalities = mortalitiesY,
                                                                                       varH=0.1, varS = 0.2, varO = varOY)))
    stopCluster(cl)
    t2 <- Sys.time()
    t2 - t1
  }
  if(parallelise==2){
    ncores <- detectCores() - 1
    #t1 <- Sys.time()
    sz <- mclapply(1:nsim1, function(...) hypothesis_test(trial = trialX, aoi = aoiX, 
                                                dataset = simulate_trial(n_arms = n_armsX, npw = npwX, 
                                                              rotations = rotationsX, mos_det = mos_detX, meanMos = meanMosX, 
                                                                dispMos = dispMosX, mortalities = mortalitiesX,
                                                                  varH=0.1, varS = 0.2, varO = varO1)))
  #t2 <- Sys.time()
  #t2 - t1
  }
  mm <- unlist(sz)
  print(paste0('Power Estimate: ',100*sum(mm)/length(mm),'%, 95% CI: [',
               100*round(binom.test(table(factor(mm,c(1,0))))$conf.int[1],3),',',
               100*round(binom.test(table(factor(mm,c(1,0))))$conf.int[2],3),']'))
  return(c(100*sum(mm)/length(mm),100*binom.test(table(factor(mm,c(1,0))))$conf.int[1],
           100*binom.test(table(factor(mm,c(1,0))))$conf.int[2]))
}
