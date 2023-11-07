library(ggplot2)

#Create Covariates
N = 200
N_Trial = 100
N_Target = N - N_Trial

##### Gender ##### 
set.seed(8485)
p_male = 0.396
Male_trial <- rbinom(N_Trial, 1, p_male)

set.seed(7485)
p_male = 0.396
Male_target <- rbinom(N_Target, 1, p_male)

Sex = c(Male_trial, Male_target)

##### Age ##### 
# Trial
set.seed(8485)
alpha <- 7.5
beta <- 5.4
Sim_age_Trial <- rbeta(N_Trial, alpha, beta)

# Adjust the simulated age data to your desired age range
min_age <- 15 
max_age <- 80   
Sim_age_Trial <- min_age + Sim_age_Trial * (max_age - min_age)

# min(Sim_age_Trial)
# max(Sim_age_Trial)
# mean(Sim_age_Trial)
# 
# ggplot(data = data.frame(Age = Sim_age_Trial), aes(x = Age)) +
#   geom_histogram(binwidth = 2, fill = "lightblue", color = "black") +
#   labs(title = "Simulated Age Data", x = "Age", y = "Frequency")
# 
# dt = cbind.data.frame(Sim_age_Trial,Male_trial)
# mean(dt$Sim_age_Trial[dt$Male_trial==1])
# mean(dt$Sim_age_Trial[dt$Male_trial==0])

# Target
set.seed(7485)
alpha <- 6
beta <- 5
Sim_age_Target <- rbeta(N_Target, alpha, beta)

# Adjust the simulated age data to your desired age range
min_age <- 18   
max_age <- 75   
Sim_age_Target <- min_age + Sim_age_Target * (max_age - min_age)

# min(Sim_age_Target)
# max(Sim_age_Target)
# mean(Sim_age_Target)
# 
# ggplot(data = data.frame(Age = Sim_age_Target), aes(x = Age)) +
#   geom_histogram(binwidth = 2, fill = "lightblue", color = "black") +
#   labs(title = "Simulated Age Data", x = "Age", y = "Frequency")
# 
# dt = cbind.data.frame(Sim_age_Target,Male_target)
# mean(dt$Sim_age_Target[dt$Male_target==1])
# mean(dt$Sim_age_Target[dt$Male_target==0])


Age = c(Sim_age_Trial, Sim_age_Target)

# dt = cbind.data.frame(Age,Sex)
# mean(dt$Age[dt$Sex==1])
# mean(dt$Age[dt$Sex==0])
# 
# ggplot(data = data.frame(Age = Age), aes(x = Age)) +
#   geom_histogram(binwidth = 2, fill = "lightblue", color = "black") +
#   labs(title = "Simulated Age Data", x = "Age", y = "Frequency")

#Race
Race_Trial <-sample(c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Asian", "Other"), 
                   n, replace=TRUE, prob=c(0.508, 0.234, 0.205, 0.032, 0.021))
Race_Target <-sample(c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Asian", "Other"), 
                    n, replace=TRUE, prob=c(0.544, 0.253, 0.16, 0.03, 0.013))
Race <- c(Race_Trial, Race_Target)

#BMI
slope_age <- 0.6
slope_sex <- 0.425
simulated_bmi <- slope_age  * Age - slope_sex * Sex  + rnorm(n, mean = 0, sd = 6.5)

# # View the first few rows of the simulated data
# simulated_data <- data.frame(BMI = simulated_bmi, Sex)
# mean(simulated_data$BMI)
# sd(simulated_data$BMI)
# 
# mean(simulated_data$BMI[simulated_data$Sex==1])
# mean(simulated_data$BMI[simulated_data$Sex==0])



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


################################################################################
# NHANES proportion(https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0255583)

# SEX
(53136+22967+23435+3405)/(53136+22967+23435+3405+35059+17803+12205+2243)

# AGE
(53136*54.3 + 22967*50.2 + 23435*45.7 + 3405*43)/(53136+22967+23435+3405)
(35059*57.1 + 17803*50.2 + 12205*45.7 + 2243*43)/(35059+17803+12205+2243)

# RACE
NHW = (53136+35059)
NHB = (22967+17803)
H = (23435+12205)
A = (3405+2243)
Other = 3673
Total = NHW+NHB+H+A+Other 

NHW/Total
NHB/Total
H/Total
A/Total
Other/Total

# BMI (29.8, 7.3)
(53136*29.09 + 22967*33.34 + 23435*31.06 + 3405*24.49)/(53136+22967+23435+3405) #30.3
(35059*29.02 + 17803*28.4 + 12205*29.75 + 2243*26.12)/(35059+17803+12205+2243) #28.9
(53136*29.09 + 22967*33.34 + 23435*31.06 + 3405*24.49 + 
    35059*29.02 + 17803*28.4 + 12205*29.75 + 2243*26.12)/
  (53136+22967+23435+3405+35059+17803+12205+2243)

(53136*7.69 + 22967*9.11 + 23435*7.43 + 3405*4.98 + 
    35059*6.14 + 17803*7.13 + 12205*6.69 + 2243*4.62)/
  (53136+22967+23435+3405+35059+17803+12205+2243)
