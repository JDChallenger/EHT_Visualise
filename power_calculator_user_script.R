source('power_calculator_functions.R')


#####################################################
# 1. Info required

# What type of trial is it? 
# The code we've developed is designed to test for either 
# superiority or non-inferiority
# We can make this test between two trial arms. 
# However, in some trials both washed & unwashed nets of the same type are included.
# Therefore, we allow the option of combining data from washed & unwashed net of the 
# same type before hypothesis testing. The variable 'trial' determines the primary study
# question; it can take 4 values:

# 1. Superiority between two trial arms
# 2. Superiority between two ITNs (combining data from washed & unwashed ITNs of two types i.e. involving 4 trial arms)
# 3. Non-inferiority between two trial arms
# 4. Non-inferiority between two ITNs (combining data from washed & unwashed ITNs of two types i.e. involving 4 trial arms)

trial <- 1

#How many trial arms in total? 
n_arms <- 7

## How many nights should an ITN stay in a hut before the nets are rotated?
npw <- 7 #Must be less than or equal to number of trial arms

#assumed mortality in each trial arm (including control)
mortalities <- rep(0,n_arms) #empty list, same length as the number of trial arms
#Fill in your own values!
mortalities <- c(0.05, 0.05, 0.15, 0.25, 0.15, 0.30, 0.2)
length(mortalities)==n_arms

#Trial may contain multiple products. We'll need to specify which arms are
# involved in the hypothesis testing
aoi1 <- c(4,6) # We'll test whether the latter arm is superior to the former
aoi2 <- c(4,5,6,7)# We'll test whether ITN2 is superior to ITN1. In order, these should be ITN1 (unwashed), ITN1 (washed), ITN2 (unwashed), ITN2 (washed). 
aoi3 <- c(4,6) # We'll test whether the latter arm is non-inferior to the former
aoi4 <- c(4,5,6,7) # We'll test whether ITN2 is non-inferior to ITN1. # In order, these should be ITN1 (unwashed), ITN1 (washed), ITN2 (unwashed), #ITN2 (washed). 

#select which one we wish to use
aoi <- aoi1

# Length of trial. 
# Described in terms of number of complete 'rotations' of the trial. 
# For example, if you have 6 huts and 6 trial arms, one rotation would take 6 weeks
# to complete (each net spends one week in a single hut).
# Needs to be at least 1 at the moment, but you can enter things like '1.25' or '1.5'
rotations <- 1 

#How many mosquitoes per night per hut?
meanMos <- 15
#Should this be constant ('deterministic', det=1), or be sampled from a
#negative binomial distn (det=0) with the given mean (meanMos) and
#dispersion parameter (dispMos) ?
mos_det <- 0
dispMos <- 2.0

#random effect(s). 
varO <- 0.4 # Variance of the observation-level random effect

#Before calculating power, let's simulate 1 trial, to check everything looks OK
xc <- simulate_trial(n_arms = n_arms, npw = npw, mortalities = mortalities,
               varH=0.1, varS = 0.2, varO = varO)
dim(xc)
head(xc)
table(xc$net)
table(xc$hut)
table(xc$sleeper)

####
# The function below simulates many trials ('nsim1' specifies how many).
# For each one, hypothesis testing will be performed (defined by the values chosen for 
# 'aoi' and 'trial' above). Statistical power is given by the percentage of trials 
# for which the null hypothesis is rejected.

# This function can be slow to run, as a large number of trials need to be simulated
# It may be possible to parallise the code, if you have a multicore machine
# Running this command will tell you how many cores you can use
detectCores()
# To parallelise (i.e. speed up) this process requires different code for Mac & Windows
#computers. Here are the options:

# parallelise = 0; Don't parallise code (use this option if you are in doubt)
# parallelise = 1; Parallise code for Windows
# parallelise = 2; Parallise code for Mac OS

t1 <- Sys.time()
power_calculator(parallelise = 2, trialX = trial, npwX = npw, rotationsX = 1, varO1 = varO, 
     nsim1 = 200, n_armsX = n_arms, mos_detX = mos_det, meanMosX = meanMos, 
     dispMosX = dispMos,
     aoiX = aoi, mortalitiesX = mortalities)
t2 <- Sys.time()
t2 - t1
#system("say Just finished!")
