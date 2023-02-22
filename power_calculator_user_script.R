source('power_calculator_functions.R')

#####################################################
# 1. Info required

# Does the hut trial involve insecticide-treated nets, or indoor residual spraying?
# The code we've developed is designed to test for either 
# superiority or non-inferiority (of either mortality or blood-feeding inhibition)
# We can make this test between two trial arms. 
# However, in some ITN trials, both washed & unwashed nets of the same type are included.
# Therefore, we allow the option of combining data from washed & unwashed net of the 
# same type before hypothesis testing. The variable 'trial' determines the primary study
# question; it can take different values (described below)

####################### EHTs involving ITNs ####################### 
#### MEASURING MORTALITY

# trial=1. Superiority between two trial arms
# trial=2. Superiority between two ITNs (combining data from washed & unwashed ITNs of two types i.e. involving 4 trial arms)
# trial=3. Non-inferiority between two trial arms
# trial=4. Non-inferiority between two ITNs (combining data from washed & unwashed ITNs of two types i.e. involving 4 trial arms)

#### MEASURING BLOOD-FEEDING INHIBITION

# trial=5. Superiority between two trial arms
# trial=6. Superiority between two ITNs (combining data from washed & unwashed ITNs of two types i.e. involving 4 trial arms)
# trial=7. Non-inferiority between two trial arms
# trial=8. Non-inferiority between two ITNs (combining data from washed & unwashed ITNs of two types i.e. involving 4 trial arms)

#How many trial arms in total? (n_arms)

## How many nights should an ITN stay in a hut before the nets are rotated? (npw)

#Expected behaviour in each arm (either for mosquito mortality, or blood-feeding inhibition)
mortalities <- c(0.05, 0.05, 0.15, 0.25, 0.15, 0.30, 0.2) 
blood_feeding <- c(0.50, 0.30, 0.30, 0.25, 0.30, 0.30, 0.25)
#Note: the length of this list should equal the number of trial arms
#length(mortalities)==n_arms
#length(blood_feeding)==n_arms

#Trial may contain multiple products. We'll need to specify which arms are
# involved in the hypothesis testing (aoi) and what type of trial we're conducting
#trial = 1; aoi <- c(4,6) # We'll test whether the latter arm is superior to the former
#trial = 2; aoi <- c(4,5,6,7)# We'll test whether ITN2 is superior to ITN1. In order, these should be ITN1 (unwashed), ITN1 (washed), ITN2 (unwashed), ITN2 (washed). 
#trial = 3; aoi <- c(4,6) # We'll test whether the latter arm is non-inferior to the former
#trial = 4; aoi <- c(4,5,6,7) # We'll test whether ITN2 is non-inferior to ITN1. # In order, these should be ITN1 (unwashed), ITN1 (washed), ITN2 (unwashed), #ITN2 (washed). 

# Length of trial (rotations). 
# Described in terms of number of complete 'rotations' of the trial. 
# For example, if you have 6 huts and 6 trial arms, one rotation would take 6 weeks
# to complete (each net spends one week in a single hut).
# Needs to be at least 1 at the moment, but you can enter things like '1.25' or '1.5'

####################### End of ITN-specific parameters ##################

#How many mosquitoes per night per hut? First we specify the mean number (meanMos) 
#Should the mosquito counts be a constant ('deterministic', det=1), or be sampled from a
#negative binomial distn (det=0) with the given mean and
#dispersion parameter (dispMos) ?

#random effect(s). Variance of the observation-level random effect (varO)

#Before calculating power, let's simulate 1 trial, to check everything looks OK
xc <- simulate_trial_ITN(n_arms = 7, npw = 6, responses = mortalities,
               varO = 0.9, mos_det = 0, meanMos = 10, dispMos = 1)
dim(xc)
head(xc)
table(xc$net)
table(xc$hut)
table(xc$sleeper)

### Another function performs the hypothesis testing. We have to provide the function
# with a dataset, and tell it what to test for.
#Here's an example, using the dataset we've just generated above (xc)

#is Arm #6 superior to Arm #4, in terms of mosquito mortality?
#This function returns a value of 1 if the null hypothesis is rejected; otherwise,
# it returns zero
hypothesis_test(trial = 1, aoi = c(4,6), dataset = xc)

####
# The function below simulates many trials ('nsim' specifies how many).
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
power_calculator_ITN(parallelise = 0, trial = 1, npw = 6, rotations = 1, 
     nsim = 100, n_arms = 7, mos_det = 1, meanMos = 12, varO = .9, 
     dispMos = 2, aoi = c(4,6), responses = mortalities)
t2 <- Sys.time()
t2 - t1
#system("say Just finished!")


####################### EHTs involving IRS ############################## 
# n_arms: how many trial arms (including untreated control) 
# rep_IRS: how many huts per IRS product? 
# rep_C: How many huts per untreated control?
# Then the number of huts in the trial will be rep_C + (n_arms - 1 * rep_IRS). This'll be the same as the number of volunteers required

#How many days will the trial last? (trial_days)
#mortalities (or blood-feeding) in each arm
mortalities_IRS <- c(0.10, 0.30, 0.50, 0.55)
#blood_feeding_IRS <- c(0.50, 0.30, 0.30, 0.25)

#### MEASURING MORTALITY

# trial=9. Superiority between two trial arms
# trial=10. Non-inferiority between two trial arms

#### MEASURING BLOOD-FEEDING INHIBITION

# trial=11. Superiority between two trial arms
# trial=12. Non-inferiority between two trial arms

####################### End of IRS-specific parameters ##################

#Before calculating power, let's simulate 1 trial, to check everything looks OK
#As before, we need info on mosquito numbers.

xd <- simulate_trial_IRS(n_arms = 4, rep_IRS = 4, rep_C = 2, responses = mortalities_IRS,
                  trial_days = 15, varO = 1, mos_det = 0, meanMos = 12, dispMos = 1.5)
dim(xd)
head(xd)
table(xd$net)
table(xd$hut)
table(xd$sleeper)
xd[xd$hut==1,]

#For the hypothesis testing, we can use the same function as for ITNs
#We just need to update the value of 'trial'

#For example: is Arm #4 superior to Arm #2, in terms of mosquito mortality?
#This function returns a value of 1 if the null hypothesis is rejected; otherwise,
# it returns zero
hypothesis_test(trial = 9, aoi = c(2,4), dataset = xd)

########## 
# Now let's simulate a large number of trials, 

# parallelise = 0; Don't parallise code (use this option if you are in doubt)
# parallelise = 1; Parallise code for Windows
# parallelise = 2; Parallise code for Mac OS

t1 <- Sys.time()
power_calculator_IRS(parallelise = 0, trial = 1, varO = 0.9, trial_days = 15,
                     rep_arms = 4, nsim = 300, n_arms = 4, mos_det = 1, meanMos = 11, 
                 dispMos = 1.4, aoi = c(3,4), responses = mortalities_IRS)
t2 <- Sys.time()
t2 - t1
