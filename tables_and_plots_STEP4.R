library(xtable)
library(ggplot2)
library(reshape2)
library(dplyr)

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

Avg_ci_len_k0 = colMeans(result_k0$confidence_interval_length_matrix)
Avg_ci_len_k0

Avg_res_k0 = list(Avg_truth_W_tate = Avg_truth_W_tate_k0, 
                  Avg_bias = Avg_bias_k0, Avg_abs_bias = Avg_abs_bias_k0, 
                  Avg_cov = Avg_cov_k0, Avg_ci_len = Avg_ci_len_k0)

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

Avg_ci_len_k0.05 = colMeans(result_k0.05$confidence_interval_length_matrix)
Avg_ci_len_k0.05

Avg_res_k0.05 = list(Avg_truth_W_tate = Avg_truth_W_tate_k0.05, 
                     Avg_bias = Avg_bias_k0.05, Avg_abs_bias = Avg_abs_bias_k0.05, 
                     Avg_cov = Avg_cov_k0.05, Avg_ci_len = Avg_ci_len_k0.05)

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

Avg_ci_len_k0.2 = colMeans(result_k0.2$confidence_interval_length_matrix)
Avg_ci_len_k0.2

Avg_res_k0.2 = list(Avg_truth_W_tate = Avg_truth_W_tate_k0.2, 
                     Avg_bias = Avg_bias_k0.2, Avg_abs_bias = Avg_abs_bias_k0.2, 
                     Avg_cov = Avg_cov_k0.2, Avg_ci_len = Avg_ci_len_k0.2)

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

Avg_ci_len_k0.5 = colMeans(result_k0.5$confidence_interval_length_matrix)
Avg_ci_len_k0.5

Avg_res_k0.5 = list(Avg_truth_W_tate = Avg_truth_W_tate_k0.5, 
                    Avg_bias = Avg_bias_k0.5, Avg_abs_bias = Avg_abs_bias_k0.5, 
                    Avg_cov = Avg_cov_k0.5, Avg_ci_len = Avg_ci_len_k0.5)

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

Avg_ci_len_k0.8 = colMeans(result_k0.8$confidence_interval_length_matrix)
Avg_ci_len_k0.8

Avg_res_k0.8 = list(Avg_truth_W_tate = Avg_truth_W_tate_k0.8, 
                    Avg_bias = Avg_bias_k0.8, Avg_abs_bias = Avg_abs_bias_k0.8, 
                    Avg_cov = Avg_cov_k0.8, Avg_ci_len = Avg_ci_len_k0.8)

#### ---- Create Tables

## result
res2vec = function(avg_res){
  dt = matrix(c(as.numeric(avg_res$Avg_truth_W_tate[-1]),
                avg_res$Avg_bias,avg_res$Avg_abs_bias, avg_res$Avg_cov,
                avg_res$Avg_ci_len), ncol=5,nrow = 5)
  vec = round(c(as.numeric(avg_res$Avg_truth_W_tate[1]),c(t(dt))),4)
  return(vec)
}

avg_vec_k0 = res2vec(Avg_res_k0)
avg_vec_k0.05 = res2vec(Avg_res_k0.05)
avg_vec_k0.2 = res2vec(Avg_res_k0.2)
avg_vec_k0.5 = res2vec(Avg_res_k0.5)
avg_vec_k0.8 = res2vec(Avg_res_k0.8)

avg_tb = rbind.data.frame(avg_vec_k0,avg_vec_k0.05,avg_vec_k0.2,avg_vec_k0.5,avg_vec_k0.8)

colnames(avg_tb) = c("Truth","ATE_OR","bias_OR","Abs_bias_OR","cov_OR",
                     "CI_length_OR","ATE_IOW","bias_IOW","Abs_bias_IOW",
                     "cov_IOW","CI_length_IOW","ATE_IOW2","bias_IOW2",
                     "Abs_bias_IOW2","cov_IOW2","CI_length_IOW2","ATE_DRE1",
                     "bias_DRE1","Abs_bias_DRE1","cov_DRE1","CI_length_DRE1",
                     "ATE_DRE2","bias_DRE2","Abs_bias_DRE2","cov_DRE2",
                     "CI_length_DRE2")

rownames(avg_tb) = c("K=0","K=0.05","K=0.2","K=0.5","K=0.8")

avg_tb

## overall
print(xtable(avg_tb, type = "latex"))

## TATE + bias + absolute bias
avg_tb2 = rbind.data.frame(avg_vec_k0[-c(5,6,10,11,15,16,20,21,25,26)],
                           avg_vec_k0.05[-c(5,6,10,11,15,16,20,21,25,26)],
                           avg_vec_k0.2[-c(5,6,10,11,15,16,20,21,25,26)],
                           avg_vec_k0.5[-c(5,6,10,11,15,16,20,21,25,26)],
                           avg_vec_k0.8[-c(5,6,10,11,15,16,20,21,25,26)])

colnames(avg_tb2) = c("Truth","ATE_OR","bias_OR","Abs_bias_OR","ATE_IOW",
                      "bias_IOW","Abs_bias_IOW","ATE_IOW2","bias_IOW2",
                      "Abs_bias_IOW2","ATE_DRE1","bias_DRE1",
                      "Abs_bias_DRE1","ATE_DRE2","bias_DRE2","Abs_bias_DRE2")

rownames(avg_tb2) = c("K=0","K=0.05","K=0.2","K=0.5","K=0.8")

print(xtable(avg_tb2, type = "latex"))

## CI coverage + CI length
avg_tb3 = rbind.data.frame(avg_vec_k0[c(5,6,10,11,15,16,20,21,25,26)],
                           avg_vec_k0.05[c(5,6,10,11,15,16,20,21,25,26)],
                           avg_vec_k0.2[c(5,6,10,11,15,16,20,21,25,26)],
                           avg_vec_k0.5[c(5,6,10,11,15,16,20,21,25,26)],
                           avg_vec_k0.8[c(5,6,10,11,15,16,20,21,25,26)])

colnames(avg_tb3) = c("cov_OR","CI_length_OR","cov_IOW","CI_length_IOW",
                      "cov_IOW2","CI_length_IOW2","cov_DRE1",
                      "CI_length_DRE1","cov_DRE2","CI_length_DRE2")

rownames(avg_tb3) = c("K=0","K=0.05","K=0.2","K=0.5","K=0.8")

print(xtable(avg_tb3, type = "latex"))

## separate
ATE_sub = avg_tb %>% select(Truth,ATE_OR,ATE_IOW,ATE_IOW2,ATE_DRE1,ATE_DRE2)
print(xtable(ATE_sub, type = "latex"))

bias_sub = avg_tb %>% select(bias_OR,bias_IOW,bias_IOW2,bias_DRE1,bias_DRE2)
print(xtable(bias_sub, type = "latex"))

Abs_bias_sub = avg_tb %>% select(Abs_bias_OR,Abs_bias_IOW,Abs_bias_IOW2,
                                 Abs_bias_DRE1,Abs_bias_DRE2)
print(xtable(Abs_bias_sub, type = "latex"))

cov_sub = avg_tb %>% select(cov_OR,cov_IOW,cov_IOW2,cov_DRE1,cov_DRE2)
print(xtable(cov_sub , type = "latex"))

CI_length_sub = avg_tb %>% select(CI_length_OR,CI_length_IOW,CI_length_IOW2,
                                  CI_length_DRE1,CI_length_DRE2)
print(xtable(CI_length_sub , type = "latex"))

#### ---- Create Graphs

k_lst = c("0","0.05","0.2","0.5","0.8")

## ATE 
method = rep(c("Truth","OR" ,"IOW" ,"IOW2" ,"DRE1" ,"DRE2"),each = length(k_lst))
k = rep(k_lst,6)
ATE_lst = as.numeric(unlist(ATE_sub))
ATE_tb = cbind.data.frame(Method = method,K = k,ATE = ATE_lst)

ggplot(data=ATE_tb, aes(x=K, y=ATE_lst, group=Method)) +
  geom_line(aes(color=Method))+
  geom_point(aes(color=Method))+ 
  xlab("K") +
  ylab("TATE") +
  ggtitle("TATE under different Method and different K")

## Bias 
method = rep(c("OR" ,"IOW" ,"IOW2" ,"DRE1" ,"DRE2"),each = length(k_lst))
k = rep(k_lst,5)
bias_lst = as.numeric(unlist(bias_sub))
bias_tb = cbind.data.frame(Method = method,K = k,bias = bias_lst)

ggplot(data=bias_tb, aes(x=K, y=bias_lst, group=Method)) +
  geom_line(aes(color=Method))+
  geom_point(aes(color=Method))+ 
  xlab("K") +
  ylab("Bias") +
  ggtitle("Bias under different Method and different K")
 
## Absolute Bias
method = rep(c("OR" ,"IOW" ,"IOW2" ,"DRE1" ,"DRE2"),each = length(k_lst))
k = rep(k_lst,5)
Abs_bias_lst = as.numeric(unlist(Abs_bias_sub))
Abs_bias_tb = cbind.data.frame(Method = method,K = k,Abs_bias = Abs_bias_lst)

ggplot(data=Abs_bias_tb, aes(x=K, y=Abs_bias_lst, group=Method)) +
  geom_line(aes(color=Method))+
  geom_point(aes(color=Method))+ 
  xlab("K") +
  ylab("Absolute Bias") +
  ggtitle("Absolute Bias under different Method and different K")

## Coverage
method = rep(c("OR" ,"IOW" ,"IOW2" ,"DRE1" ,"DRE2"),each = length(k_lst))
k = rep(k_lst,5)
cov_lst = as.numeric(unlist(cov_sub))
cov_tb = cbind.data.frame(Method = method,K = k,cov = cov_lst)

ggplot(data=cov_tb, aes(x=K, y=cov_lst, group=Method)) +
  geom_line(aes(color=Method))+
  geom_point(aes(color=Method))+ 
  xlab("K") +
  ylab("Coverage") +
  ggtitle("Coverage under different Method and different K")


## CI length
create_long = function(result){
  Method = rep(c("OR" ,"IOW" ,"IOW2" ,"DRE1" ,"DRE2"),
               each = nrow(result$confidence_interval_length_matrix))
  CI_length = c(result$confidence_interval_length_matrix[,1],
             result$confidence_interval_length_matrix[,2],
             result$confidence_interval_length_matrix[,3],
             result$confidence_interval_length_matrix[,4],
             result$confidence_interval_length_matrix[,5])
  long_tb = cbind.data.frame(CI_length,Method)
  long_tb$Method = factor(long_tb$Method, 
                          levels = c("OR" ,"IOW" ,"IOW2","DRE1" ,"DRE2"))
  return(long_tb)
}
long_tb_k0 = create_long(result_k0)
long_tb_k0.05 = create_long(result_k0.05)
long_tb_k0.2 = create_long(result_k0.2)
long_tb_k0.5 = create_long(result_k0.5)
long_tb_k0.8 = create_long(result_k0.8)

long_tb_fivek = rbind.data.frame(long_tb_k0,long_tb_k0.05,long_tb_k0.2,
                                 long_tb_k0.5,long_tb_k0.8)
K_long = rep(paste0("k=",k_lst),each= nrow(long_tb_k0))

long_CI_len_all = cbind.data.frame(long_tb_fivek, K = K_long)

CI_plot <- ggplot(long_CI_len_all, aes(x=Method,y=CI_length,fill=Method))+
  geom_boxplot() +
  labs(title="Confidence Interval Length under different Method and different K") +facet_wrap(~K)+
  ylab("Confidence Interval Length") + theme(legend.position="bottom")
CI_plot
