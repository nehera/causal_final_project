# Generate Latent Variable
Udiff <- runif(1, -1, 1) # Size of underlying var difference between trial and target pops
mu_trial = -Udiff*0.5
mu_target = Udiff*0.5
UTrial <- rnorm(N_Trial, mu_trial, 1)
UTarget <- rnorm(N_Target, mu_target, 1)
U = c(UTrial, UTarget)

l=0.5
k=0
optiaml_a1a2 = function(k,mu_trial=mu_trial,mu_target=mu_target){
  #flip = (-1)^rbinom(1,1,prob = 0.5)
  a <- 0.5 - k/2
  b <- 0.5 + k/2
  
  f =  function (a1) qnorm(a1,mu_trial,1) - qnorm(2*a-a1,mu_target,1)
  optimal_a1 = uniroot(f, lower = 0, upper = min(1,2*a))$root
  a2 = a-optimal_a1
  
  return(list(k=k, a1=optimal_a1, a2=a2))
}
a1 = optiaml_a1a2(k=k,mu_trial=mu_trial,mu_target=mu_target)$a1
flip = (-1)^rbinom(1,1,prob = 0.5)
a_thresh <- flip*qnorm(a1,mu_trial,1)
b_thresh <- -a_thresh
VTrial <- ifelse(UTrial < a_thresh, "Low", "High")
VTarget <- ifelse(UTarget < b_thresh, "Low", "High")


################# test run ###################

optiaml_a1a2(k=0,mu_trial=mu_trial,mu_target = mu_target)
a1 = optiaml_a1a2(k=0,mu_trial=mu_trial,mu_target=mu_target)$a1
flip = (-1)^rbinom(1,1,prob = 0.5)
a_thresh <- flip*qnorm(a1,mu_trial,1)
a_thresh
b_thresh <- -a_thresh
b_thresh

optiaml_a1a2(k=0.05,mu_trial=mu_trial,mu_target = mu_target)
a1 = optiaml_a1a2(k=0.05,mu_trial=mu_trial,mu_target=mu_target)$a1
flip = (-1)^rbinom(1,1,prob = 0.5)
a_thresh <- flip*qnorm(a1,mu_trial,1)
a_thresh
b_thresh <- -a_thresh
b_thresh
optiaml_a1a2(k=0.2,mu_trial=mu_trial,mu_target = mu_target)
optiaml_a1a2(k=0.5,mu_trial=mu_trial,mu_target = mu_target)
optiaml_a1a2(k=0.8,mu_trial=mu_trial,mu_target = mu_target)
a1 = optiaml_a1a2(k=0.8,mu_trial=mu_trial,mu_target=mu_target)$a1
a_thresh <- qnorm(a1,mu_trial,1)
a_thresh
b_thresh <- -a_thresh
b_thresh
VTrial <- ifelse(UTrial < a_thresh, "Low", "High")
VTarget <- ifelse(UTarget < b_thresh, "Low", "High")


############################################
res=c()
for (a1 in seq(0,a,0.001)){
  res = c(res,qnorm(a1,mu_trial,1) - qnorm(a-a1,mu_target,1))
}
cbind(res,seq(0,a,0.001))


f2 =  function (a1) qnorm(a-a1,mu_target,1)
uniroot(f2, lower = -0.1, upper = a-0.00001)$root

f3 =  function (a1) qnorm(a1,mu_trial,1)
uniroot(f3, lower = 0, upper = a)$root

a1 = 0.352282
qnorm(1-a1,mu_target,1)
qnorm(a1,mu_trial,1)
# k = 0.8
# a <- 0.5 - k/2 
# b <- 0.5 + k/2
# 
# f =  function (a1) qnorm(a1,mu_trial,1) - qnorm(a-a1,mu_target,1)
# optimal_a1 = uniroot(f, lower = 0, upper = a)$root
# a2 = a-optimal_a1
# c(optimal_a1,a2)

optiaml_a1a2 = function(k,mu_trial=mu_trial,mu_target=mu_target){
  a <- 0.5 - k/2
  b <- 0.5 + k/2
  
  f =  function (a1) qnorm(a1,mu_trial,1) - qnorm(a-a1,mu_target,1)
  optimal_a1 = uniroot(f, lower = 0, upper = a)$root
  a2 = a-optimal_a1
  
  return(list(k=k, a1=optimal_a1, a2=a2))
}

optiaml_a1a2(k=0,mu_trial=mu_trial,mu_target = mu_target)
optiaml_a1a2(k=0.05,mu_trial=mu_trial,mu_target = mu_target)
optiaml_a1a2(k=0.2,mu_trial=mu_trial,mu_target = mu_target)
optiaml_a1a2(k=0.5,mu_trial=mu_trial,mu_target = mu_target)
optiaml_a1a2(k=0.8,mu_trial=mu_trial,mu_target = mu_target)


##################### plot #####################
Udiff <- runif(1, -1, 1) 
a <- c(rnorm(10000, -Udiff*0.5,1),rnorm(10000,Udiff*0.5,1))
b <- rnorm(20000,0,1)
qqplot(a,b)
abline(a=0,b=1)

N = 5000
N_Trial = floor(0.5*N)
N_Target = N - N_Trial