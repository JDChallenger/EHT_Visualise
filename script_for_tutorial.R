#Load the libraries we will use in this tutorial. If you don't have these libraries installed, 
# click on the 'Tools' menu, then click on 'Install Packages', and search for the 
# desired package

library(lme4)
library(devtools)
library(MASS)
library(RColorBrewer)
library(cowplot)
library(ggplot2)
library(dplyr)
library(reshape2)

# The package GLMMmisc has a non-standard installation. It can be installed by running the following 
# command (remove the comment symbol '#')
#devtools::install_github("pcdjohnson/GLMMmisc")
library(GLMMmisc)

#user-defined function, to convert from log-odds scale to probability scale
InvLogit <- function(X){
  exp(X)/(1+exp(X))
}

###################################################################################
#######       Section 1. Load the data, and summarise it       ####################
###################################################################################

#Random seed here. Then generate dataset.
set.seed(12443)
source('sim_data_for_plot_function.R')

# Alternatively, you could load a previously generated dataset. 
# df <- readRDS('data_for_plot.rds')
# Or use a real dataset of interest

# Data is for a 7-arm trial, with one full rotation (343 data points)
#Let's look at the data
dim(df)
head(df)

str(df)
table(df$treatment)
table(df$hut)
table(df$sleeper)

###################################################################################
#########           Section 2. Visualise the EHT data          ####################
###################################################################################

#Load the functions we'll use to make the plots
source('plotting_functions.R')

#Let's visualise the dqta. We'll make a six-panel plot that will feature data from 3 trial arms 
# here we look at the untreated nets ('C'); and both unwashed & washed N1 nets (N1u, N1w) 

#The 1st three panels we'll make will show nightly mosquito counts in the trial arms
# We'll use colours to indicate both mortality & blood-feeding status
# I prefer to have a common range on the y axis for all 3 of these panels. 
#This is done by selecting the value of 'mx' 
mx <- max(df[df$treatment=='C'|df$treatment=='N1u'|df$treatment=='N1w',]$total)

#In the argument of the function, variable 'arm' needs to match the trial arm in the dataset
#'arm_title' is the name you want to appear on the plot-- you can choose anything you like!
#'mx' gives the range of counts that are used for the plot (as above)
#'pieX' gives the x position of the pie chart. You may wish to move this, if it blocks some of the bars
pnel1(dataa = df, arm = 'C', arm_title = 'Control', mx=mx, pieX = 0.15)
pnel1(dataa = df, arm = 'N1u', arm_title = 'ITN (Unwashed)', mx=mx, pieX = 0.15)
pnel1(dataa = df, arm = 'N1w', arm_title = 'ITN (Washed)', mx=mx, pieX = 0.15)

#In two of the panels, we'll estimate the nightly mortality.
#'arm' and 'arm_title' are defined as above
#By default, the data points will be coloured per hut. If you don't have hut information 
#you can turn this off by setting hut_info = 0
error_bar_prop(dataa = df, arm = 'N1u', arm_title = 'ITN (Unwashed)', hut_info = 1)
error_bar_prop(dataa = df, arm = 'N1w', arm_title = 'ITN (Washed)', hut_info = 1)

#The remaining plot will summarise the blood feeding rates in each arm. 
#If we see signs of deterrence in the data, we can also visualise this

#Is there evidence for deterrence?
tapply(df$total, df$treatment, mean)

# You should only trying to view deterrence if the mean mosquito counts in the control arm
# is greater than both ITN arms included in the plot.
# Select deterr = 1 to view deterrence
# Note: at the moment, the deterrence calculation only works if all trial arms have the same 
# number of data pts. We'll try to fix this

bfi(dataa = df, arm1 = 'C', arm2 = 'N1u', arm3 = 'N1w',
    arm_label1 = 'Control', arm_label2 = 'ITN Unwashed', arm_label3 = 'ITN Washed')
#Note how the percentages change, once deterrence is turned on (denominator changes)
bfi(dataa = df, deterr = 1, arm1 = 'C', arm2 = 'N1u', arm3 = 'N1w',
    arm_label1 = 'Control', arm_label2 = 'ITN Unwashed', arm_label3 = 'ITN Washed')

#Let's put all the panels together, and save the figure
plot_grid(pnel1(dataa = df, arm = 'C', arm_title = 'Control', mx=mx, pieX = 0.1),
          pnel1(dataa = df, arm = 'N1u', arm_title = 'ITN (Unwashed)', mx=mx, pieX = 0.1),
          pnel1(dataa = df, arm = 'N1w', arm_title = 'ITN (Washed)', mx=mx, leg_end = 0, pieX = 0.1),
          bfi(dataa = df, deterr = 1, arm1 = 'C', arm2 = 'N1u', arm3 = 'N1w', 
              arm_label1 = 'Control', arm_label2 = 'ITN (Unwashed)', arm_label3 = 'ITN (Washed)'),
          error_bar_prop(dataa = df, arm = 'N1u', arm_title = 'ITN (Unwashed)'),
          error_bar_prop(dataa = df, arm = 'N1w', arm_title = 'ITN (Washed)'),
          nrow=2, labels = c('a','b','c','d','e','f'))
ggsave('Six_panel_figure.pdf',height = 12.0, width = 17.0)

###################################################################################
##########       Section 3. Fit a regression model to the data     ################
###################################################################################

df$observation <- factor(formatC(1:nrow(df), flag="0", width=3))

fit <-
  glmer(
    cbind(tot_dead, total - tot_dead) ~
      treatment + (1 | hut) + (1 | sleeper) + (1 | observation),
    family = binomial, data = df) #,  control =
#  glmerControl(optimizer = "optimx", calc.derivs = FALSE,
#               optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
summary(fit)

#make new variable
df$net <- NA
df[df$treatment=='C',]$net <- 'C'
df[df$treatment=='N1u',]$net <- 'N1'
df[df$treatment=='N1w',]$net <- 'N1'
df[df$treatment=='N2u',]$net <- 'N2'
df[df$treatment=='N2w',]$net <- 'N2'
df[df$treatment=='N3u',]$net <- 'N3'
df[df$treatment=='N3w',]$net <- 'N3'
table(df$net, useNA = 'a')

fit_n <-
  glmer(
    cbind(tot_dead, total - tot_dead) ~
      net + (1 | hut) + (1 | sleeper) + (1 | observation),
    family = binomial, data = df) #,  control =
#  glmerControl(optimizer = "optimx", calc.derivs = FALSE,
#               optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
summary(fit_n)

#Convert to mortality scale
fit@beta
coef(summary(fit))["(Intercept)", "Estimate"]
coef(summary(fit))["treatmentN1u", "Estimate"]
#InvLogit(coef(summary(fit))["treatmentN1u", "Estimate"])
InvLogit(coef(summary(fit))["(Intercept)", "Estimate"] + coef(summary(fit))["treatmentN1u", "Estimate"])


###################################################################################
#######       Section 4. Modelling deterrence                ######################
###################################################################################

#If no random effects present, use glm.nb from the MASS package

tapply(df$total, df$treatment, mean)

fit_nb <- glmer.nb(total ~ treatment + (1|sleeper), data = df)
summary(fit_nb)

#Dispersion parameter
getME(fit_nb, "glmer.nb.theta")

coef(summary(fit_nb))["(Intercept)", "Estimate"]
coef(summary(fit_nb))["treatmentN1u", "Std. Error"]


#cxrME(fit_nb,k=1,l=1)
rhoX <- vcov(fit_nb)[1,2]/(sqrt(vcov(fit_nb)[1,1])*sqrt(vcov(fit_nb)[2,2]))
#Standard deviation for the difference in the fixed effects
sigmaX <- sqrt(vcov(fit_nb)[1,1] + vcov(fit_nb)[2,2] + 
                 2 * rhoX *(sqrt(vcov(fit_nb)[1,1]) *(sqrt(vcov(fit_nb)[2,2]))))
#central
ct <- coef(summary(fit_nb))[1,1] + coef(summary(fit_nb))[2,1]
ctl <- exp(ct)
upp <- exp(ct + 1.96*sigmaX)
low <- exp(ct - 1.96*sigmaX)

c(ctl,low,upp)
#Percentage deterrence, based on the central estimate?

100*(1-ctl/exp(coef(summary(fit_nb))[1,1]))

#Note: we have to use a function from another package (MASS), if we wish to fit a model without random effects
fit_nb2 <- MASS::glm.nb(total ~ treatment, data=df)
summary(fit_nb2)

cor <- summary(fit_nb2)$cov.unscaled[1,2]/(sqrt(summary(fit_nb2)$cov.unscaled[1,1]) * sqrt(summary(fit_nb2)$cov.unscaled[2,2]))
sig <- sqrt(summary(fit_nb2)$cov.unscaled[1,1] + summary(fit_nb2)$cov.unscaled[2,2] + 
              2*cor*sqrt(summary(fit_nb2)$cov.unscaled[1,1])*sqrt(summary(fit_nb2)$cov.unscaled[2,2]))
ctl2 <- exp(fit_nb2$coefficients[1] + fit_nb2$coefficients[2])
upp2 <- exp(log(ctl) + 1.96*sig)
low2 <- exp(log(ctl) - 1.96*sig)
c(ctl2,low2,upp2)

###################################################################################
#########       Section 5. Testing for superiority             ####################
###################################################################################

#####################################
# relevel factors
df$treatment <- as.factor(df$treatment)
#check the levels
levels(df$treatment)
#relevel
df$treatment <- relevel(df$treatment,"N1u") 
#check the levels again
levels(df$treatment)
#re-run regression model
fit <-
  glmer(
    cbind(tot_dead, total - tot_dead) ~
      treatment + (1 | hut) + (1 | sleeper) + (1 | observation),
    family = binomial, data = df) #,  control =
#  glmerControl(optimizer = "optimx", calc.derivs = FALSE,
#               optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
summary(fit)

rrho <- vcov(fit_n)[3,4]/(sqrt(vcov(fit_n)[3,3])*sqrt(vcov(fit_n)[4,4]))
ssigma <- sqrt(vcov(fit_n)[3,3] + vcov(fit_n)[4,4] - 2 * rrho *(sqrt(vcov(fit_n)[3,3]) *(sqrt(vcov(fit_n)[4,4]))))
OR <- exp(coef(summary(fit_n))["netN3", "Estimate"] - coef(summary(fit_n))["netN2", "Estimate"])
ORc <- coef(summary(fit_n))["netN3", "Estimate"]-coef(summary(fit_n))["netN2", "Estimate"]
ci2 <- exp(ORc + 1.96*ssigma)
ci1 <- exp(ORc - 1.96*ssigma)
print(paste0(OR,", (",ci1,", ",ci2,")"))

###################################################################################
#########       Section 6.Testing for non-inferiority       ######################
###################################################################################

#What if one was the intercept?
# relevel factors
df$net <- as.factor(df$net)
#check the levels
levels(df$net)
#relevel
df$net <- relevel(df$net,"N2") 
#check the levels
levels(df$net)

fit_n <-
  glmer(
    cbind(tot_dead, total - tot_dead) ~
      net + (1 | hut) + (1 | sleeper) + (1 | observation),
    family = binomial, data = df) #,  control =
#  glmerControl(optimizer = "optimx", calc.derivs = FALSE,
#               optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
summary(fit_n)

exp(coef(summary(fit_n))['netN3','Estimate'])
exp(coef(summary(fit_n))['netN3','Estimate'] - 1.96*coef(summary(fit_n))['netN3','Std. Error'])
exp(coef(summary(fit_n))['netN3','Estimate'] + 1.96*coef(summary(fit_n))['netN3','Std. Error'])

non_inf <- function(fit2, label_l = 'netPermanet3', label_k = 'netOlysetPlus', l=4, k=3){
  rrho <- vcov(fit2)[k,l]/(sqrt(vcov(fit2)[k,k])*sqrt(vcov(fit2)[l,l]))
  ssigma <- sqrt(vcov(fit2)[k,k] + vcov(fit2)[l,l] - 2 * rrho *(sqrt(vcov(fit2)[k,k]) *(sqrt(vcov(fit2)[l,l]))))
  OR <- exp(coef(summary(fit2))[label_l, "Estimate"]-coef(summary(fit2))[label_k, "Estimate"])
  ORc <- coef(summary(fit2))[label_l, "Estimate"]-coef(summary(fit2))[label_k, "Estimate"]
  ci2 <- exp(ORc + 1.96*ssigma)
  ci1 <- exp(ORc - 1.96*ssigma)
  print(paste0(OR,", (",ci1,", ",ci2,")"))
}

###################################################################################
########  Section 7. Simulate trials to estimate statistical power      ###########
###################################################################################

# First describe trial design

latsq <-
  rbind(
    c("C", "E1", "E2", "E3", "E4", "E5", "E6"),
    c("E6", "C", "E1", "E2", "E3", "E4", "E5"),
    c("E5", "E6", "C", "E1", "E2", "E3", "E4"),
    c("E4", "E5", "E6", "C", "E1", "E2", "E3"),
    c("E3", "E4", "E5", "E6", "C", "E1", "E2"),
    c("E2", "E3", "E4", "E5", "E6", "C", "E1"),
    c("E1", "E2", "E3", "E4", "E5", "E6", "C"))

colnames(latsq) <- paste("hut", 1:nrow(latsq), sep = "")
rownames(latsq) <- paste("week", 1:ncol(latsq), sep = "")
latsq

mosdata <-
  expand.grid(
  hut = factor(1:ncol(latsq)),
  week = factor(1:nrow(latsq)),
  night = factor(1:7)
  )
mosdata <- mosdata[order(mosdata$hut, mosdata$week, mosdata$night),]

mosdata$net <- factor(diag(latsq[mosdata$week, mosdata$hut]))
#mosdata[(1:22),]


#Also need to allocate the volunteers
aux <- rep(0,49) #enough data pts for a week
#k <- 7
for(k in 1:7){
  aux[(7*(k-1)+1):(7*(k-1)+7)] <- c( k : 7 , seq_len(k-1)  ) 
}
#repeat for all 7 weeks
mosdata$sleeper <- factor(rep(aux,7))
table(mosdata$hut)
table(mosdata$net)
table(mosdata$sleeper)
#Take a look at the data by running this command:
#View(mosdata)

# here 'n' is the total number of mosquitoes entering each hut each night
mosdata$n <- 25 #Could also be random e.g. 
#mosdata$n <- rnbinom(dim(mosdata)[1], mu = 10, size = 2)

#Now make a unique identifier for each data point
mosdata$observation <- factor(formatC(1:nrow(mosdata), flag="0", width=3))

head(mosdata)

#Now we'll use the function sim.glmm() to simulate mosquito mortality in the trial
#This function was developed by Johnston et al. (https://doi.org/10.1111/2041-210X.12306)
#Enter fixed effects as odds ratios (compared to the control [intercept]).
#Write the intercept on the log-odds scale. 
#We'll assume a mortality of 5% for trial arm with the untreated nets
OR1 <- (0.3/(1-0.3))/(0.05/(1-0.05))
OR2 <- (0.4/(1-0.4))/(0.05/(1-0.05))

mosdata <- 
  sim.glmm(design.data = mosdata,
           fixed.eff = list(
             intercept = qlogis(0.05),
             net = log(c(C = 1, E1 = OR1, E2 = OR2, E3 = OR2, E4 = OR2, E5 = OR2, E6 = OR2))),
           rand.V = c(hut = 0.3, sleeper = 0.3, observation = 0.8),
           distribution = "binomial")

#For the superiority test, should make E1 the intercept net
mosdata2 <- mosdata
levels(mosdata2$net)
#relevel
mosdata2$net <- relevel(mosdata2$net,"E1") 

fit_model <-
  glmer(
    cbind(response, n - response) ~
      net + (1 | hut) + (1 | sleeper) + (1 | observation),
    family = binomial, data = mosdata2)
summary(fit_model)

coef(summary(fit_model))["netE2", "Pr(>|z|)"]
if(coef(summary(fit_model))["netE2", "Pr(>|z|)"] <0.05 & coef(summary(fit_model))["netE2","Estimate"]>0){
  1
}else{
  0
}

#Now let's write a function to (i) Simulate a trial and then (ii) Check for superiority
sim_sup <- function(...){
  #Enter fixed effects as odds ratios (compared to the control [intercept]).
  #Write the intercept on the log-odds scale
  OR1 <- (0.3/(1-0.3))/(0.05/(1-0.05))
  OR2 <- (0.4/(1-0.4))/(0.05/(1-0.05))
  
  mosdata <- 
    sim.glmm(design.data = mosdata,
             fixed.eff = list(
               intercept = qlogis(0.05),
               net = log(c(C = 1, E1 = OR1, E2 = OR2, E3 = OR2, E4 = OR2, E5 = OR2, E6 = OR2))),
             rand.V = c(hut = 0.3, sleeper = 0.3, observation = 0.8),
             distribution = "binomial")
  
  #For the superiority test, should make E1 the intercept net
  mosdata2 <- mosdata
  #relevel
  mosdata2$net <- relevel(mosdata2$net,"E1") 
  
  fit_model <-
    glmer(
      cbind(response, n - response) ~
        net + (1 | hut) + (1 | sleeper) + (1 | observation),
      family = binomial, data = mosdata2)
  
  if(coef(summary(fit_model))["netE2", "Pr(>|z|)"] <0.05 & coef(summary(fit_model))["netE2","Estimate"]>0){
    1
  }else{
    0
  }
}
sim_sup()
nsim <- 100
simulations <- sapply(1:nsim, sim_sup)
print(paste0('Power Estimate: ',100*sum(simulations)/length(simulations),'%'))
#binom.test(table(factor(sim.pvals < 0.05, c(T, F))))$conf.int
#How many simulations is enough? Depends how much precision you need. 
#Let's look at the 95% CIs for the power estiamte
binom.test(table(factor(simulations,c(1,0))))$conf.int
#Put all that together
print(paste0('Power Estimate: ',100*sum(simulations)/length(simulations),'%, 95% CI: [',
             round(binom.test(table(factor(simulations,c(1,0))))$conf.int[1],3),',',
             round(binom.test(table(factor(simulations,c(1,0))))$conf.int[2],3),']'))
