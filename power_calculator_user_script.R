source('power_calculator_functions.R')


#####################################################
# 1. Info required

# What type of trial is it? 

# 1. Superiority between two trial arms
# 2. Superiority between two ITNs (combining data from washed & unwashed ITNs of two types i.e. involving 4 trial arms)
# 3. Non-inferiority between two trial arms
# 4. Non-inferiority between two ITNs (combining data from washed & unwashed ITNs of two types i.e. involving 4 trial arms)

trial <- 1

#How many trial arms in total? 
tt <- 6
## How many nights should an ITN stay in a hut before the nets are rotated?
nn <- 6 #Must be less than or equal to number of trial arms

#assumed mortality in each trial arm (including control)
mort <- rep(0,tt) #empty list, same length as the number of trial arms
#Fill in your own values!
mort <- c(0.05, 0.05, 0.15, 0.20, 0.15, 0.35)

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

aoi <- aoi1

#Length of trial. 
#Described in terms of number of complete rotations of the LSD, 
#Needs to be at least 1 at the moment, but you can enter things like '1.25' or '1.5'
rotations <- 1 

#How many mosquitoes per night per hut?
meanMos <- 10
dispMos <- 1.0
#Should this be constant (det=1), or be sampled from a negative binomial distn (det=0)
# with the given mean (meanMos)
det <- 1

#random effect(s). 
varO <- 0.8 # Variance of the observation-level random effect

#Before calculating power, let's simulate 1 trial, to check everything looks OK
xc <- simulate_trial(n_arms = tt, npw = nn, mortalities = mort,
               varH=0.1, varS = 0.2, varO = varO)
dim(xc)
head(xc)
table(xc$net)
table(xc$hut)
table(xc$sleeper)

t1 <- Sys.time()
lazy(trialX = trial, npw1 = nn, rotations1 = 2, varO1 = varO, 
     nsim1 = 500, n_armsX = tt, mos_detX = det, meanMosX = meanMos, dispMosX = dispMos,
     aoiX = aoi, mortalitiesX = mort)
t2 <- Sys.time()
t2 - t1
#system("say Just finished!")



