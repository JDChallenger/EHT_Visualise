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

#Explain these
aoi1 <- c(4,6)
aoi2 <- c(4,5,6,7)
aoi3 <- c(4,6)
aoi4 <- c(4,5,6,7)

#How many trial arms in total? 
tt <- 7
## How many nights should an ITN stay in a hut before the nets are rotated?
nn <- 7

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


