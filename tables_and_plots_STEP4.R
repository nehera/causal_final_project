# Load results .rds file
results <- readRDS("step-4-results.rds")

## Base case: k=0
result_k0 <- results$k_0

## Case 2: k=0.05
result_k0.05 <- results$k_0.05

## Case 3: k=0.2
result_k0.2 <- results$k_0.2

## Case 4: k=0.5
result_k0.5 <- results$k_0.5

## Cass 5: k=0.8
result_k0.8 <- results$k_0.8

#### ---- Collect results

## Base case: k=0
Truth_W_tate = cbind.data.frame(truth = as.matrix(result_k0$true_tates,nrow=M),result_k0$tate_matrix)
Avg_truth_W_tate_k0 = colMeans(Truth_W_tate)
Avg_truth_W_tate_k0

Bias = result_k0$tate_matrix - result_k0$true_tates
Avg_bias_k0 = colMeans(Bias)
Avg_bias_k0

Avg_abs_bias_k0 = colMeans(abs(Bias))
Avg_abs_bias_k0

Avg_cov_k0 = colMeans(result_k0$coverage_matrix)
Avg_cov_k0

Avg_res_k0 = list(Avg_truth_W_tate = Avg_truth_W_tate_k0, 
                  Avg_bias = Avg_bias_k0, Avg_abs_bias = Avg_abs_bias_k0, 
                  Avg_cov = Avg_cov_k0)

## Case 2: k=0.05
Truth_W_tate = cbind.data.frame(truth = as.matrix(result_k0.05$true_tates,nrow=M),result_k0.05$tate_matrix)
Avg_truth_W_tate_k0.05 = colMeans(Truth_W_tate)
Avg_truth_W_tate_k0.05

Bias = result_k0.05$tate_matrix - result_k0.05$true_tates
Avg_bias_k0.05 = colMeans(Bias)
Avg_bias_k0.05

Avg_abs_bias_k0.05 = colMeans(abs(Bias))
Avg_abs_bias_k0.05

Avg_cov_k0.05 = colMeans(result_k0.05$coverage_matrix)
Avg_cov_k0.05

Avg_res_k0.05 = list(Avg_truth_W_tate = Avg_truth_W_tate_k0.05, 
                     Avg_bias = Avg_bias_k0.05, Avg_abs_bias = Avg_abs_bias_k0.05, 
                     Avg_cov = Avg_cov_k0.05)

## Case 3: k=0.2
Truth_W_tate = cbind.data.frame(truth = as.matrix(result_k0.2$true_tates,nrow=M),result_k0.2$tate_matrix)
Avg_truth_W_tate_k0.2 = colMeans(Truth_W_tate)
Avg_truth_W_tate_k0.2

Bias = result_k0.2$tate_matrix - result_k0.2$true_tates
Avg_bias_k0.2 = colMeans(Bias)
Avg_bias_k0.2

Avg_abs_bias_k0.2 = colMeans(abs(Bias))
Avg_abs_bias_k0.2

Avg_cov_k0.2 = colMeans(result_k0.2$coverage_matrix)
Avg_cov_k0.2

Avg_res_k0.2 = list(Avg_truth_W_tate = Avg_truth_W_tate_k0.2, 
                    Avg_bias = Avg_bias_k0.2, Avg_abs_bias = Avg_abs_bias_k0.2, 
                    Avg_cov = Avg_cov_k0.2)

## Case 4: k=0.5
Truth_W_tate = cbind.data.frame(truth = as.matrix(result_k0.5$true_tates,nrow=M),result_k0.5$tate_matrix)
Avg_truth_W_tate_k0.5 = colMeans(Truth_W_tate)
Avg_truth_W_tate_k0.5

Bias = result_k0.5$tate_matrix - result_k0.5$true_tates
Avg_bias_k0.5 = colMeans(Bias)
Avg_bias_k0.5

Avg_abs_bias_k0.5 = colMeans(abs(Bias))
Avg_abs_bias_k0.5

Avg_cov_k0.5 = colMeans(result_k0.5$coverage_matrix)
Avg_cov_k0.5

Avg_res_k0.5 = list(Avg_truth_W_tate = Avg_truth_W_tate_k0.5, 
                    Avg_bias = Avg_bias_k0.5, Avg_abs_bias = Avg_abs_bias_k0.5, 
                    Avg_cov = Avg_cov_k0.5)

## Case 5: k=0.8
Truth_W_tate = cbind.data.frame(truth = as.matrix(result_k0.8$true_tates,nrow=M),result_k0.8$tate_matrix)
Avg_truth_W_tate_k0.8 = colMeans(Truth_W_tate)
Avg_truth_W_tate_k0.8

Bias = result_k0.8$tate_matrix - result_k0.8$true_tates
Avg_bias_k0.8 = colMeans(Bias)
Avg_bias_k0.8

Avg_abs_bias_k0.8 = colMeans(abs(Bias))
Avg_abs_bias_k0.8

Avg_cov_k0.8 = colMeans(result_k0.8$coverage_matrix)
Avg_cov_k0.8

Avg_res_k0.8 = list(Avg_truth_W_tate = Avg_truth_W_tate_k0.8, 
                    Avg_bias = Avg_bias_k0.8, Avg_abs_bias = Avg_abs_bias_k0.8, 
                    Avg_cov = Avg_cov_k0.8)

#### ---- Create Tables
## overall
library(xtable)
print(xtable(avg_tb, type = "latex"))

## TATE + bias + absolute bias
avg_tb2 = rbind.data.frame(avg_vec_k0[-c(5,9,13,17,21)],avg_vec_k0.05[-c(5,9,13,17,21)],
                           avg_vec_k0.2[-c(5,9,13,17,21)],avg_vec_k0.5[-c(5,9,13,17,21)],
                           avg_vec_k0.8[-c(5,9,13,17,21)])

colnames(avg_tb2) = c("Truth","ATE_OR","bias_OR","Abs_bias_OR","ATE_IPW",
                      "bias_IPW","Abs_bias_IPW","ATE_IPW2","bias_IPW2",
                      "Abs_bias_IPW2","ATE_DRE1","bias_DRE1",
                      "Abs_bias_DRE1","ATE_DRE2","bias_DRE2","Abs_bias_DRE2")

rownames(avg_tb2) = c("K=0","K=0.05","K=0.2","K=0.5","K=0.8")

print(xtable(avg_tb2, type = "latex"))

## separate
ATE_sub = avg_tb %>% select(Truth,ATE_OR,ATE_IPW,ATE_IPW2,ATE_DRE1,ATE_DRE2)
print(xtable(ATE_sub, type = "latex"))

bias_sub = avg_tb %>% select(bias_OR,bias_IPW,bias_IPW2,bias_DRE1,bias_DRE2)
print(xtable(bias_sub, type = "latex"))

Abs_bias_sub = avg_tb %>% select(Abs_bias_OR,Abs_bias_IPW,Abs_bias_IPW2,
                                 Abs_bias_DRE1,Abs_bias_DRE2)
print(xtable(Abs_bias_sub, type = "latex"))

cov_sub = avg_tb %>% select(cov_OR,cov_IPW,cov_IPW2,cov_DRE1,cov_DRE2)
print(xtable(cov_sub , type = "latex"))


#### ---- Create Graphs

k_lst = c("0","0.05","0.2","0.5","0.8")

## ATE 
method = rep(c("Truth","OR" ,"IPW" ,"IPW2" ,"DRE1" ,"DRE2"),each = length(k_lst))
k = rep(k_lst,6)
ATE_lst = as.numeric(unlist(ATE_sub))
ATE_tb = cbind.data.frame(Method = method,K = k,ATE = ATE_lst)

library(ggplot2)
ggplot(data=ATE_tb, aes(x=K, y=ATE_lst, group=Method)) +
  geom_line(aes(color=Method))+
  geom_point(aes(color=Method))+ 
  xlab("K") +
  ylab("TATE") +
  ggtitle("TATE under different Method and different K")

## Bias 
method = rep(c("OR" ,"IPW" ,"IPW2" ,"DRE1" ,"DRE2"),each = length(k_lst))
k = rep(k_lst,5)
bias_lst = as.numeric(unlist(bias_sub))
bias_tb = cbind.data.frame(Method = method,K = k,bias = bias_lst)

library(ggplot2)
ggplot(data=bias_tb, aes(x=K, y=bias_lst, group=Method)) +
  geom_line(aes(color=Method))+
  geom_point(aes(color=Method))+ 
  xlab("K") +
  ylab("Bias") +
  ggtitle("Bias under different Method and different K")
 
## Absolute Bias
method = rep(c("OR" ,"IPW" ,"IPW2" ,"DRE1" ,"DRE2"),each = length(k_lst))
k = rep(k_lst,5)
Abs_bias_lst = as.numeric(unlist(Abs_bias_sub))
Abs_bias_tb = cbind.data.frame(Method = method,K = k,Abs_bias = Abs_bias_lst)

library(ggplot2)
ggplot(data=Abs_bias_tb, aes(x=K, y=Abs_bias_lst, group=Method)) +
  geom_line(aes(color=Method))+
  geom_point(aes(color=Method))+ 
  xlab("K") +
  ylab("Absolute Bias") +
  ggtitle("Absolute Bias under different Method and different K")

## Coverage
method = rep(c("OR" ,"IPW" ,"IPW2" ,"DRE1" ,"DRE2"),each = length(k_lst))
k = rep(k_lst,5)
cov_lst = as.numeric(unlist(cov_sub))
cov_tb = cbind.data.frame(Method = method,K = k,cov = cov_lst)

library(ggplot2)
ggplot(data=cov_tb, aes(x=K, y=cov_lst, group=Method)) +
  geom_line(aes(color=Method))+
  geom_point(aes(color=Method))+ 
  xlab("K") +
  ylab("Coverage") +
  ggtitle("Coverage under different Method and different K")
