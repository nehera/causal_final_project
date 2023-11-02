n<-100
N<-2*n

#Create Covariates
#Gender
Male <- rbinom(N, 1, .5) #just have genders be 50/50 for now
#Age
AgeDiff <- runif(1, -10, 10)#size of age difference between trial and target pops
AgeTrial <- rpois(n, 50+AgeDiff*.5) #create vector of trial ages (integers, centered at 1/2 dist away from 50)
AgeTarget <- rpois(n, 50-AgeDiff*.5)
Age <- c(AgeTrial, AgeTarget) #vector with n trial then n target ages
#Race
RaceTrial <-sample(c("Black", "White", "Asian", "Hispanic", "Other"), n, replace=TRUE, prob=c(0.1, 0.5, .2, .15, .05))
RaceTarget <-sample(c("Black", "White", "Asian", "Hispanic", "Other"), n, replace=TRUE, prob=c(0.2, 0.4, .1, .25, .05))
Race <- c(RaceTrial, RaceTarget)
#BMI
#make this some function like: h(age, gender)+random term
#for now, just make normal
BMI<-rnorm(N, 24, 5)

## -- Differentially Specified Variable

# Set parameters
k <- 0.05
l <- 0.5
p <- 0.5
seed <- 1

sample_V <- function(n=100, k=0.05, l=0.5, p=0.5, seed=1) {
  # Generate Latent Variable
  set.seed(seed)
  UTrial<- rnorm(n)
  UTarget<- rnorm(n) # TODO are we fine with standard normal latent variables? Any normal variable can be centered and scaled... 
  
  # Cut Latent Variable
  Trial_cut_less <- rbinom(1, 1, prob = p) # Binary indicator for Trial cut point less than Target cut point
  a <- l - k/2
  b <- l + k/2
  a_thresh <- qnorm(a)
  b_thresh <- qnorm(b)
  if (Trial_cut_less==1) {
    VTrial <- ifelse(UTrial < a_thresh, "Low", "High")
    VTarget <- ifelse(UTarget < b_thresh, "Low", "High")
  } else {
    VTrial <- ifelse(UTrial < b_thresh, "Low", "High")
    VTarget <- ifelse(UTarget < a_thresh, "Low", "High")
  }
  
  V = factor(c(VTrial, VTarget), levels = c("Low", "High"))
  
  V_df <- data.frame(population = factor(c(rep("Trial", n), rep("Target", n))), V)
            
  return(V_df)
}

# Create other variables
ATrial<-rbinom(n, 1, .5)
A <- c(ATrial, rep(NA, n)) #1 if treat, 0 if control (only for those in trial!)
S <- c(rep(1,n), rep(0,n))#indicator variable for trial
Y0 <- 3*Male -.2*Age + 4*BMI+as.numeric(V=="High")*3+as.numeric(Race=="Black")*2 +rnorm(N, 100,sd=50) 
Y1 <- 3*Male -.2*Age + 4*BMI+as.numeric(V=="High")*3+as.numeric(Race=="Black")*2 +rnorm(N, 100,sd=50) +30
Y  <- ifelse(A==1, Y1,Y0) 
data <- data.frame(Male, Age, Race, BMI, V, A, S, Y0, Y1, Y)
