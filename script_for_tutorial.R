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
library(fitdistrplus)
library(lattice)

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

#Random seed here. 
set.seed(12443)
# Running this code will generate a simulated EHT dataset.
source('sim_data_for_plot_function.R')

# Alternatively, you could load a previously generated dataset. 
# df <- readRDS('data_for_plot.rds')
# Or use a real dataset of interest

# Data in the simulated EHT is for a 7-arm trial, with one full rotation (343 data points)
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
#'pieX' gives the x position of the pie chart (in the range [0,1]). You may wish to move this, if it blocks some of the bars
#Similarly, 'pieY' gives the y position (in the range [0,1])
#You can add the option 'pie = 0' to generate the plot without the pie chart
pnel1(dataa = df, arm = 'C', arm_title = 'Control', mx=mx, pieX = 0.15, pie = 0)
pnel1(dataa = df, arm = 'N1u', arm_title = 'ITN (Unwashed)', mx=mx, pieX = 0.15)
pnel1(dataa = df, arm = 'N1w', arm_title = 'ITN (Washed)', mx=mx, pieX = 0.15)

#Next, we generate two panels that estimate the nightly mortality in a particular trial arm.
#'arm' and 'arm_title' are defined as above
#By default, the data points will be coloured per hut. If you don't have hut information 
#you can turn this off by setting hut_info = 0
#A legend for 'hut' can be added with the option 'leg_end=1'
error_bar_prop(dataa = df, arm = 'N1u', arm_title = 'ITN (Unwashed)', hut_info = 1)
error_bar_prop(dataa = df, arm = 'N1w', arm_title = 'ITN (Washed)', hut_info = 1, leg_end = 1)

#The remaining plot will summarise the blood feeding rates in each arm. 
#If we see signs of deterrence in the data, we can also visualise this

#Is there evidence for deterrence?
tapply(df$total, df$treatment, mean)

# You should only trying to view deterrence if the mean mosquito counts in the control arm
# is greater than both ITN arms included in the plot.
# Select deterr = 1 to view deterrence
# Note: deterrence is calculated relative to arm1, which here is the untreated control
# If negative deterrence is found, it will be set to zero, and the function will output a notification message

bfi(dataa = df, arm1 = 'C', arm2 = 'N1u', arm3 = 'N1w',
    arm_label1 = 'Control', arm_label2 = 'ITN Unwashed', arm_label3 = 'ITN Washed')
#Note how the percentages change, once deterrence is turned on (denominator changes)
#Here we also demonstrate how to change the size of the text labels
bfi(dataa = df, deterr = 1, arm1 = 'C', arm2 = 'N1u', arm3 = 'N1w', text_size = 4.4,
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

#Alternatively, you may wish to look at blood-feeding & deterrence for all arms 
# in the trial. This can be done using the function 'bfi_all_arms()'.
# To use this function, you need to make a list of the trial arms, as you 
# wish them to appear in the plot labels.

#Here are two examples
net_names <- unique(df$treatment) #i.e. as they appear in the dataset
unique(df$treatment) 
#Or, you can customise them. Note: the length of this list must match the number
# of arms in the trial, and the order of trial arms should match that displayed by
# running the command 'unique(df$treatment)'
net_names <- c('Untreated Net', 'ITN1 (Unwashed)','ITN1 (Washed)','ITN2 (Unwashed)',
               'ITN2 (Washed)','ITN3 (Unwashed)', 'ITN3 (Washed)')

#Now let's see the plots that this function can produce

#Without deterrence
bfi_all_arms(dataa = df, deterr = 0, arm_labels = net_names)
#Now, we include deterrence:
#We can also vary the size of the labels with the option 'text_size'
bfi_all_arms(dataa = df, deterr = 1, arm_labels = net_names, text_size = 4.7)

#Note: when calculating deterrence, this function expects the first arm to be the 
#control arm (you can view the order of the arms by running the
# command 'unique(df$treatment'). In other words, deterrence is calculated relative to the number of 
#mosquitoes found in this arm. If this is not the case, you can instruct the function
#using the additional argument 'control_arm'.

#For example, if the control arm is the arm number 3:
bfi_all_arms(dataa = df, deterr = 1, arm_labels = net_names,
             text_size = 4.7, control_arm = 3)
#Note that if deterrence is negative, it is set to zero by this function

#You can include this figure in the six-panel plot above, but it could be a bit 
# squashed if you have lots of trial arms!
plot_grid(pnel1(dataa = df, arm = 'C', arm_title = 'Control', mx=mx, pieX = 0.1),
          pnel1(dataa = df, arm = 'N1u', arm_title = 'ITN (Unwashed)', mx=mx, pieX = 0.1),
          pnel1(dataa = df, arm = 'N1w', arm_title = 'ITN (Washed)', mx=mx, leg_end = 0, pieX = 0.1),
          bfi_all_arms(dataa = df, deterr = 1, arm_labels = net_names, text_size = 4.6),
          error_bar_prop(dataa = df, arm = 'N1u', arm_title = 'ITN (Unwashed)'),
          error_bar_prop(dataa = df, arm = 'N1w', arm_title = 'ITN (Washed)'),
          nrow=2, labels = c('a','b','c','d','e','f'))

###################################################################################
##########       Section 3. Fit a regression model to the data     ################
###################################################################################

df$observation <- factor(formatC(1:nrow(df), flag="0", width=3))

fit <-
  glmer(
    cbind(tot_dead, total - tot_dead) ~
      treatment + (1 | hut) + (1 | sleeper) + (1 | observation),
    family = binomial, data = df) 
summary(fit)

#Aside: is the observation-level random effect, included to account for any overdispersion,
# important to include in the model? Let's look at the model that does not include it (fit0),
# and compare using a likelihood ratio test (LRT), carried out using the anova() function
fit0 <-
  glmer(
    cbind(tot_dead, total - tot_dead) ~
      treatment + (1 | hut) + (1 | sleeper),
    family = binomial, data = df) 
summary(fit0)
anova(fit,fit0)
#We consider a p value 'Pr(>Chisq)' <0.05 to indicate a significant improvement in model fit

#Here are a couple of ways to extract parameter values from the fitted model
fit@beta #Estimates for the fixed effects (on the log-odds scale)
coef(summary(fit))["(Intercept)", "Estimate"]
coef(summary(fit))["treatmentN1u", "Estimate"]

#Convert to mortality scale
#InvLogit(coef(summary(fit))["treatmentN1u", "Estimate"])
InvLogit(coef(summary(fit))["(Intercept)", "Estimate"])
InvLogit(coef(summary(fit))["(Intercept)", "Estimate"] + coef(summary(fit))["treatmentN1u", "Estimate"])
#InvLogit(coef(summary(fit))["(Intercept)", "Estimate"]+c(-1,1)*1.96*coef(summary(fit))["(Intercept)", "Std. Error"])

#Add confidence intervals
#This function will output the mortality estimate for one trial arm 
# (chose the trial arm using argument 'j', trial arms are ordered as per
# the output shown in 'summary(fit)'), and give
#the 95% confidence intervals. If you just want the output to be numeric,
# you can set 'num_only=1'. Otherwise the output will be formed as a string of 
# characters, which will include the name of the trial arm
mortality_conf <- function(mod = fit, j = 2, num_only = 0){
  if(j != 1){
    rho <- vcov(mod)[1,j]/(sqrt(vcov(mod)[1,1])*sqrt(vcov(mod)[j,j]))
    #Standard deviation for the difference in the fixed effects
    sigma <- sqrt(vcov(mod)[1,1] + vcov(mod)[j,j] + 
                  2 * rho *(sqrt(vcov(mod)[1,1]) *(sqrt(vcov(mod)[j,j]))))
    #Extract name(s)
    nz <- colnames(mod@pp$X)[j]
  
    central <- mod@beta[1] + mod@beta[j]
    ctl <- round(InvLogit(central),3)
    upp <- round(InvLogit(central + 1.96*sigma),3)
    low <- round(InvLogit(central - 1.96*sigma),3)
    if(num_only==0){
      return(paste0(nz,': ',ctl,' [',low,' ,',upp,']'))
    }else{
      return(c(ctl,low,upp))
    }
  }else{
    
    nz <- 'Control'
    sigma <- sqrt(vcov(mod)[1,1])
    central <- mod@beta[1]
    ctl <- round(InvLogit(central),3)
    upp <- round(InvLogit(central + 1.96*sigma),3)
    low <- round(InvLogit(central - 1.96*sigma),3)
    if(num_only==0){
      return(paste0(nz,': ',ctl,' [',low,' ,',upp,']'))
    }else{
      return(c(ctl,low,upp))
    }
  }
}
mortality_conf(mod = fit, j = 1)

#Make a function that'll work out how many trial arms you have and calculate 
#the mortality estimates & confidence intervals for all arms
mortality_summary <- function(modX){
  l <- length(modX@beta)
  dfe <- data.frame('Arm' = as.character(),
                    'Mortality' = as.numeric(),
                    'Lower_95pc_CI' = as.numeric(), 'Upper_95pc_CI' = as.numeric())
  for(i in 1:l){
      aux <- mortality_conf(mod = modX, j = i, num_only = 1)
      dfb <- #data.frame('Arm' = ifelse(i==1,'Control',substring(colnames(modX@pp$X)[i],10)),
        data.frame('Arm' = ifelse(i==1,'Control',colnames(modX@pp$X)[i]),
                    'Mortality' = aux[1],
                    'Lower_95pc_CI' = aux[2], 
                    'Upper_95pc_CI' = aux[3])
      dfe <- rbind(dfe,dfb)
    
  }
  return(dfe)
}
mortality_summary(modX = fit)

#Note: the functions above assume that the uncertainty in the fixed effect estimates is normally distributed
# You can check this by performing a bootstrap. However, this will be much slower (can take 30 minutes or longer).
#If you are preparing results for a presentation or publication, it could be worth checking the
# confidence intervals using this method. 
confint(fit, method = "boot", nsim = 750, parm = "beta_")
  
#What if we wanted to group data points by type of insecticide, and ignore washing effects?
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
    family = binomial, data = df) 
summary(fit_n)
mortality_summary(modX = fit_n)

#Task: what is the estimated mortality due to net 'N1'? 

###################################################################################
#######       Section 4. Modelling deterrence                ######################
###################################################################################

#If no random effects present, use function glm.nb() from the MASS package

#Let's look for indications of a deterrence effect, by calculating the mean
# mosquito counts in each trial arm. We'll use the tapply() function

tapply(df$total, df$treatment, mean)

#Let's see which distribution better describes the count data- a Poisson distribution
#or a negative binomial distribution
fig.pois <- fitdist(df$total, "pois") # Poisson distribution
plot(fig.pois)
fig.negbin <- fitdist(df$total, "nbinom") # negative binomial
plot(fig.negbin)
#Here we see the Negative binomial model has a lower Akaike's Information Criterion,
# Hence a better fit
gofstat(list(fig.pois,fig.negbin), fitnames = c("Poisson", "Negative Binomial"))

#Note: often the hut or sleeper random effect doesn't explain much variation in the data
# (indicating by the variance of the random effect being very close to 0).
# You may wish to drop one or both of these
fit_nb <- glmer.nb(total ~ treatment + (1|hut), data = df)
summary(fit_nb)

#Dispersion parameter
getME(fit_nb, "glmer.nb.theta")

coef(summary(fit_nb))["(Intercept)", "Estimate"]
coef(summary(fit_nb))["treatmentN1u", "Std. Error"]

#Number of mosquitoes per night in huts with untreated nets
exp(coef(summary(fit_nb))["(Intercept)", "Estimate"])

#Number of mosquitoes per night in huts with unwashed N1 nets
exp(coef(summary(fit_nb))["(Intercept)", "Estimate"] + coef(summary(fit_nb))["treatmentN1u", "Estimate"])

#Percentage deterrence, based on the central estimates
#TO DO: a function for the deterrence. Can you do all of them at once?
100*(1-exp(coef(summary(fit_nb))[1,1] + coef(summary(fit_nb))[2,1])/exp(coef(summary(fit_nb))[1,1]))

#Mosquito counts in each arm (corrected for random effects). 
#If you don't have random effects, you can just use tapply(), as above
mosq_counts <- function(mod= fit_nb, j = 2){ 
  if(j!=1){
    rhoX <- vcov(mod)[1,j]/(sqrt(vcov(mod)[1,1])*sqrt(vcov(mod)[j,j]))
    #Standard deviation for the sum in the fixed effects
    sigmaX <- sqrt(vcov(mod)[1,1] + vcov(mod)[j,j] + 
                     2 * rhoX *(sqrt(vcov(mod)[1,1]) *(sqrt(vcov(mod)[j,j]))))
    #Extract name(s)
    nz <- colnames(mod@pp$X)[j]
    
    #central
    ct <- coef(summary(mod))[1,1] + coef(summary(mod))[j,1]
    ctl <- round(exp(ct),3)
    
    upp <- round(exp(ct + 1.96*sigmaX),3)
    low <- round(exp(ct - 1.96*sigmaX),3)
    
    return(paste0(nz,': ',ctl,' [',low,', ',upp,']'))
  }else{
    #Change name of intercept category if you need to
    nz <- 'Untreated Control' #colnames(mod@pp$X)[1]
    #central
    ct <- coef(summary(mod))[1,1]
    ctl <- round(exp(ct),3)
    
    upp <- round(exp(ct + 1.96*coef(summary(mod))[1,2]),3)
    low <- round(exp(ct - 1.96*coef(summary(mod))[1,2]),3)
    
    return(paste0(nz,': ',ctl,' [',low,', ',upp,']'))
  }
}
mosq_counts(mod = fit_nb, j = 2)
mosq_counts(mod = fit_nb, j = 3)

#Mosquito counts in all 7 arms
sapply(1:7, mosq_counts, mod = fit_nb)

## Calculate % deterrence. Note: I'm not sure the confidence intervals are precisely right here..
deterrence <- function(mod = fit_nb, j = 2){ 
  if(j!=1){
    rhoX <- vcov(mod)[1,j]/(sqrt(vcov(mod)[1,1])*sqrt(vcov(mod)[j,j]))
    #Standard deviation for the sum in the fixed effects
    sigmaX <- sqrt(vcov(mod)[1,1] + vcov(mod)[j,j] + 
                     2 * rhoX *(sqrt(vcov(mod)[1,1]) *(sqrt(vcov(mod)[j,j]))))
    #Extract name(s)
    nz <- colnames(mod@pp$X)[j]
    
    #central
    ct <- coef(summary(mod))[1,1] + coef(summary(mod))[j,1]
    ctl <- round(exp(ct),3)
    
    #upp <- round(exp(ct + 1.96*sigmaX),3)
    #low <- round(exp(ct - 1.96*sigmaX),3)
    
    ctl_pc <- round(100*(1 - exp(ct) / exp(coef(summary(mod))[1,1])),2)
    upp_pc <- round(100*(1 - exp(ct - 1.96*sigmaX)  / exp(coef(summary(mod))[1,1])),2)
    low_pc <- round(100*(1 - exp(ct + 1.96*sigmaX)  / exp(coef(summary(mod))[1,1])),2)
    
    return(paste0(nz,' deterrence: ',ctl_pc,'% [',low_pc,'%, ',upp_pc,'%]'))
  }else{
    return('Deterrence not calculated for intercept category')
  }
}
deterrence()
sapply(1:7, deterrence, mod = fit_nb)


#Note: we have to use a function from another package (MASS), if we wish to fit a model without random effects
fit_nb2 <- MASS::glm.nb(total ~ treatment, data=df)
summary(fit_nb2)
fit_nb2$theta # This is the dispersion parameter fitted for this model

#And the notation for extracting the values is slightly different. We will have to 
#modify the functions above

#Note: this should produce very similar output to the tapply() function we used above
mosq_counts2 <- function(mod= fit_nb, j = 2){ 
  if(j!=1){
    rhoX <- summary(mod)$cov.unscaled[1,j]/(sqrt(summary(mod)$cov.unscaled[1,1]) * sqrt(summary(mod)$cov.unscaled[j,j]))
    #Standard deviation for the sum in the fixed effects
    sigmaX <- sqrt(summary(mod)$cov.unscaled[1,1] + summary(mod)$cov.unscaled[j,j] + 
                     2*rhoX*sqrt(summary(mod)$cov.unscaled[1,1])*sqrt(summary(mod)$cov.unscaled[j,j]))
    #Extract name(s)
    nz <- fit_nb2$xlevels[1]$treatment[j]
    
    #central
    ct <- coef(summary(mod))[1,1] + coef(summary(mod))[j,1]
    ctl <- round(exp(ct),3)
    
    upp <- round(exp(ct + 1.96*sigmaX),3)
    low <- round(exp(ct - 1.96*sigmaX),3)
    
    return(paste0(nz,': ',ctl,' [',low,', ',upp,']'))
  }else{
    #Change name of intercept category if you need to
    nz <- 'Untreated Control' #colnames(mod@pp$X)[1]
    #central
    ct <- coef(summary(mod))[1,1]
    ctl <- round(exp(ct),3)
    
    upp <- round(exp(ct + 1.96*coef(summary(mod))[1,2]),3)
    low <- round(exp(ct - 1.96*coef(summary(mod))[1,2]),3)
    
    return(paste0(nz,': ',ctl,' [',low,', ',upp,']'))
  }
}
mosq_counts2(mod = fit_nb2, j = 2)
mosq_counts2(mod = fit_nb2, j = 3)
#Mosquito counts in all 7 arms
sapply(1:7, mosq_counts2, mod = fit_nb2)

deterrence2 <- function(mod = fit_nb2, j = 2){ 
  if(j!=1){
    rhoX <- summary(mod)$cov.unscaled[1,j]/(sqrt(summary(mod)$cov.unscaled[1,1]) * sqrt(summary(mod)$cov.unscaled[j,j]))
    #Standard deviation for the sum in the fixed effects
    sigmaX <- sqrt(summary(mod)$cov.unscaled[1,1] + summary(mod)$cov.unscaled[j,j] + 
                                2*rhoX*sqrt(summary(mod)$cov.unscaled[1,1])*sqrt(summary(mod)$cov.unscaled[j,j]))
    #Extract name(s)
    nz <- fit_nb2$xlevels[1]$treatment[j]
    
    #central
    ct <- coef(summary(mod))[1,1] + coef(summary(mod))[j,1]
    ctl <- round(exp(ct),3)
    
    #upp <- round(exp(ct + 1.96*sigmaX),3)
    #low <- round(exp(ct - 1.96*sigmaX),3)
    
    ctl_pc <- round(100*(1 - exp(ct) / exp(coef(summary(mod))[1,1])),2)
    upp_pc <- round(100*(1 - exp(ct - 1.96*sigmaX)  / exp(coef(summary(mod))[1,1])),2)
    low_pc <- round(100*(1 - exp(ct + 1.96*sigmaX)  / exp(coef(summary(mod))[1,1])),2)
    
    return(paste0(nz,' deterrence: ',ctl_pc,'% [',low_pc,'%, ',upp_pc,'%]'))
  }else{
    return('Deterrence not calculated for intercept category')
  }
}
deterrence2()
sapply(1:7, deterrence2, mod = fit_nb2)

###################################################################################
#########       Section 5. Testing for superiority             ####################
###################################################################################

#####################################
# make treatment a factor variable in R
df$treatment <- as.factor(df$treatment)
#check the levels
levels(df$treatment)
#relevel
df$treatment <- relevel(df$treatment,"N2u") 
#check the levels again
levels(df$treatment)
#re-run regression model
fit <-
  glmer(
    cbind(tot_dead, total - tot_dead) ~
      treatment + (1 | hut) + (1 | sleeper) + (1 | observation),
    family = binomial, data = df) 
summary(fit)

#Will return a value of one if N3u is superior to N2u (via Wald z test)
if(coef(summary(fit))["treatmentN3u", "Pr(>|z|)"] < 0.05 &
   coef(summary(fit))["treatmentN3u", "Estimate"] > 0){
  1
}else{
  0
}

#Note: the remainder of section 5 was written in response to one of the reviewers of our article
# They wanted more detail on whether the Wald test (the default
# method of getting a p value out of a glmer model in the lme4 package)
# was the best method to use. Here we show other methods. This is primarily of 
# theoretical interest.

#Set up a Likelihood Ratio Test, to compare with Wald z test above
#For simplicity let's just look at two trial arms
df2 <- df[df$treatment=='N2u'|df$treatment=='N3u',]

#Let's look at the 'full model', which assumes a fixed effect is needed (i.e.) mosquito
#mortality is different across the two trial arms. On this smaller dataset, we've just included 
#one of the random-effects, for simplicity
fit2 <-
  glmer(
    cbind(tot_dead, total - tot_dead) ~
      treatment + (1 | observation),
    family = binomial, data = df2) 
summary(fit2)

#The simpler model assumes that the fixed effect is not needed
fit3 <-
  glmer(
    cbind(tot_dead, total - tot_dead) ~
      (1 | observation),
    family = binomial, data = df2) 
summary(fit3)
#Compare the two models using the anova() function
#This returns a p value for a chi-squared test (if the null hypothesis is rejected,
#the more complicated model-fit2- is justified)
anova(fit2,fit3)
#Could also check by bootstrapping confidence intervals, although this will be slower
confint(fit2, method = "boot", nsim = 1000, parm = "beta_")

#The test statistic in an LRT  is the change in the deviance (-2 times the loglikelihood). The 
#p value was calculated from a chi-squared distribution (degrees of freedom equal to the 
#difference in the number of parameters in the models). The profile() function evaluates the
#change in the deviance vs each parameter in the model:
trp <- profile(fit2, which = 'beta_')
#The LRT statistic will follow a quadratic curve, with respect to each parameter, 
#if the log-likehood function is quadratic.
#This function plots the square of the LRT statistic, on the basis that it is easier to 
#check whether a curve follows a straight line. To futher facilitate this, the function 
#returns a signed squared root (to the left of the parameter estimate, the value is multiplied 
#by minus 1).
pl1 <- lattice::xyplot(trp)
#The above quantity (indicated by the greek letter zeta) can be compared with the standard normal distribution N(0,1). We can use this
#to generate a plot for the corresponding density function

#We'll illustrate this for one of the parameters
#First we extract the relevant data:
dfr <- data.frame('p' = trp[trp$.par=='treatmentN3u',]$treatmentN3u, 
                  'sl' = trp[trp$.par=='treatmentN3u',]$.zeta)
#
dfr$dens <- dnorm(dfr$sl,mean=0,sd=1)
#Here's the plot, with the central estimate (dashed orange line) taken from the regression model
pl2 <- ggplot(dfr, aes(x=p,y=dens)) + geom_line() + geom_point() + theme_classic() + 
  xlab('Parameter N3u') + ylab('Probability Density') + 
  geom_vline(xintercept = coef(summary(fit2))["treatmentN3u", "Estimate"],
             color = 'orange', linetype = 'dashed', alpha = .5) + 
  theme(axis.text = element_text(size = 10), axis.title = element_text(size=11.5))
plot_grid(pl1,pl2,nrow = 1, rel_widths = c(0.55,0.45),labels = c('A','B'))

###################################################################################
#########       Section 6.Testing for non-inferiority       ######################
###################################################################################

#Let's test if a candidate net (N3) is non-inferior to net N2 ('active comparator)

# Again, it's easier if N2 ('active comparator' is the control)
# relevel factors
df$net <- as.factor(df$net)
#check the levels
levels(df$net)
#relevel
df$net <- relevel(df$net,"N2") 
#check the levels again
levels(df$net)

fit_n <-
  glmer(
    cbind(tot_dead, total - tot_dead) ~
      net + (1 | hut) + (1 | sleeper) + (1 | observation),
    family = binomial, data = df) 
summary(fit_n)

exp(coef(summary(fit_n))['netN3','Estimate'])
exp(coef(summary(fit_n))['netN3','Estimate'] - 1.96*coef(summary(fit_n))['netN3','Std. Error'])
exp(coef(summary(fit_n))['netN3','Estimate'] + 1.96*coef(summary(fit_n))['netN3','Std. Error'])

#Note: without changing the intercept category, the calculation becomes more complicated
# non_inf <- function(mod = fit_n, label_l = 'netN3', label_k = 'netN2', l=4, k=3){
#   rrho <- vcov(mod)[k,l]/(sqrt(vcov(mod)[k,k])*sqrt(vcov(mod)[l,l]))
#   ssigma <- sqrt(vcov(mod)[k,k] + vcov(mod)[l,l] - 2 * rrho *(sqrt(vcov(mod)[k,k]) *(sqrt(vcov(mod)[l,l]))))
#   OR <- exp(coef(summary(mod))[label_l, "Estimate"]-coef(summary(mod))[label_k, "Estimate"])
#   ORc <- coef(summary(mod))[label_l, "Estimate"]-coef(summary(mod))[label_k, "Estimate"]
#   ci2 <- exp(ORc + 1.96*ssigma)
#   ci1 <- exp(ORc - 1.96*ssigma)
#   print(paste0(OR,", (",ci1,", ",ci2,")"))
# }

###################################################################################
########  Section 7. Simulate trials to estimate statistical power      ###########
###################################################################################

#Note: here we show how to set up a scenario from scratch, i.e. how to define the required functions yourself.
# There are other R functions in the Github repository which will do the work for you, if you prefer.
# See 'power_calculator_user_script.R' for more details

set.seed(1244)
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

# Let's assume a mortality of 30% in net E1, and 40% in Net E2. 
# If we simulate a trial, can we detect superiority?

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

