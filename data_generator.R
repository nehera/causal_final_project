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

#mispecified variable
Udiff<- 0
#runif(1, -1, 1)#size of underlying var difference between trial and target pops - I want this because there should be 2 differences in the population
#a) different levels of the underlying variable itself and b) differential coding
UTarget<- rnorm(n, 5+Udiff*.5, 1)
UTrial<- rnorm(n, 5-Udiff*.5, 1)
#cutoffs - find way to calculate these
c1<-5
c2<-4.5
VTrial <- ifelse(UTarget<c1, "Low", "High")
VTarget <- ifelse(UTarget<c2, "Low", "High")
V <- c(VTrial, VTarget)


# Create other variables
ATrial<-rbinom(n, 1, .5)
A <- c(ATrial, rep(NA, n)) #1 if treat, 0 if control (only for those in trial!)
S <- c(rep(1,n), rep(0,n))#indicator variable for trial
Y0 <- 3*Male -.2*Age + 4*BMI+as.numeric(V=="High")*3+as.numeric(Race=="Black")*2 +rnorm(N, 100,sd=50) 
Y1 <- 3*Male -.2*Age + 4*BMI+as.numeric(V=="High")*3+as.numeric(Race=="Black")*2 +rnorm(N, 100,sd=50) +30
Y  <- ifelse(A==1, Y1,Y0) 
data <- data.frame(Male, Age, Race, BMI, V, A, S, Y0, Y1, Y)
