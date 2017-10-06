# Analyze the results...
load("./results/results.RData")


# Dataset complexity analysis
load(paste(dir,"./results/clas.RData", sep = ""))
load(paste(dir,"./results/reg.RData", sep = ""))
tasks = rbind(clas_small, reg_small)
plot(tasks$number.of.instances, tasks$number.of.features, ylim = c(0,200))

# For the oob error rate

# How many times growing OOBCurves?
# Difference from bottom to the end
# Binary classification
n_bin = length(res_bin_mean)
diffs = diffs2 = diffs3 = diffs4 = diffs5 = numeric(n_bin)
diffs4_auc = diffs5_auc = numeric(n_bin)
for (i in 1:n_bin) {
  print(i)
  resi = res_bin_mean[[i]]
  diffs[i] = resi[2000, 1] - min(resi[11:250, 1])
  diffs2[i] = min(resi[251:2000,]) - min(resi[11:250, 1])
  diffs3[i] = resi[2000, 1] - min(resi[11:2000, 1])
  diffs4[i] = resi[11, 1] - resi[250, 1]
  diffs5[i] = resi[250, 1] - resi[2000, 1] 
  
  diffs4_auc[i] = resi[11, 5] - resi[250, 5]
  diffs5_auc[i] = resi[250, 5] - resi[2000, 5] 
}

par(mfrow = c(1, 1))
boxplot(diffs)
sum(diffs > 0.005, na.rm = T) 
sum(diffs > 0.005, na.rm = T) / length(diffs2)
sum(diffs[1:75] > 0.005, na.rm = T)
sum(diffs[76:149] > 0.005, na.rm = T)

mean(diffs)
hist(diffs)

boxplot(diffs2)
sum(diffs2 > 0.001, na.rm = T)
sum(diffs2 < -0.001, na.rm = T)
sum(diffs2[1:75] > 0.001, na.rm = T)
sum(diffs2[76:149] > 0.001, na.rm = T)

mean(diffs4)
mean(diffs5)
hist(diffs5)

mean(diffs4_auc)
mean(diffs5_auc)
# Difference from bottom to the end
# Multiclass classification
n_bin = length(res_multi_mean)
diffs = diffs2 = diffs3 = diffs4 = diffs5 = numeric(n_bin)
diffs4_auc = diffs5_auc = numeric(n_bin)
for (i in 1:n_bin) {
  print(i)
  resi = res_multi_mean[[i]]
  diffs[i] = resi[2000, 1] - min(resi[11:250, 1])
  diffs2[i] = min(resi[251:2000,]) - min(resi[11:250, 1])
  diffs3[i] = resi[2000, 1] - min(resi[11:2000, 1])
  diffs4[i] = resi[11, 1] - resi[250, 1]
  diffs5[i] = resi[250, 1] - resi[2000, 1] 
  
  diffs4_auc[i] = resi[11, 5] - resi[250, 5]
  diffs5_auc[i] = resi[250, 5] - resi[2000, 5] 
}

par(mfrow = c(1, 1))
boxplot(diffs)
sum(diffs > 0.005, na.rm = T)
sum(diffs > 0.005, na.rm = T) / length(diffs2)
sum(diffs[1:22] > 0.005, na.rm = T)
sum(diffs[22:45] > 0.005, na.rm = T)

boxplot(diffs2)
sum(diffs2 > 0.001, na.rm = T)
sum(diffs2 < -0.001, na.rm = T)
sum(diffs2[1:75] > 0.001, na.rm = T)
sum(diffs2[76:149] > 0.001, na.rm = T)

mean(diffs4, na.rm = T)
mean(diffs5, na.rm = T)
mean(diffs4_auc, na.rm = T)
mean(diffs5_auc, na.rm = T)

# Regression
n_reg = length(res_reg_mean)
diffs4 = diffs5 = diffs6_medae = diffs6_medse = diffs7_medae = diffs7_medse = numeric(n_reg)
for (i in 1:n_reg) {
  print(i)
  resi = res_reg_mean[[i]]
  diffs4[i] = resi[11, 5] - resi[250, 5]
  diffs5[i] = resi[250, 5] - resi[2000, 5] 
  diffs5[i] = resi[250, 5] - resi[2000, 5] 
  diffs6_medae[i] = min(resi[11, 3]) - resi[2000, 3]
  diffs6_medse[i] = min(resi[11, 4]) - resi[2000, 4]
  diffs7_medae[i] = min(resi[11:250, 3]) - resi[2000, 3]
  diffs7_medse[i] = min(resi[11:250, 4]) - resi[2000, 4]
}

mean(diffs4, na.rm = T)
mean(diffs5, na.rm = T)

mean(diffs6_medse > 0, na.rm = T)
mean(diffs6_medae > 0, na.rm = T)

mean(diffs7_medse, na.rm = T)
mean(diffs7_medae, na.rm = T)

mean(diffs7_medse < 0, na.rm = T)
mean(diffs7_medae < 0, na.rm = T)



sum(diffs7_medae[1:60] < 0, na.rm = T)
sum(diffs7_medae[61:119] < 0, na.rm = T)
mean(diffs7_medse < 0, na.rm = T)
sum(diffs7_medse[1:60] < 0, na.rm = T)
sum(diffs7_medse[61:119] < 0, na.rm = T)

# What is the average improvement from training 2000 instead of 11 trees
improvement_bin = matrix(NA, length(res_bin_mean), ncol(res_bin_mean[[i]]))
for (i in 1:length(res_bin_mean)) {
  print(i)
  improvement_bin[i, ] = res_bin_mean[[i]][2000, ] - res_bin_mean[[i]][11,]
}
apply(improvement_bin, 2, mean)

improvement_multi = matrix(NA, length(res_multi_mean), ncol(res_multi_mean[[1]]))
for (i in c(1:44)) {
  print(i)
  improvement_multi[i, ] = res_multi_mean[[i]][2000, ] - res_multi_mean[[i]][11,]
}
apply(improvement_multi, 2, function(x) mean(x, na.rm = T))

improvement_reg = matrix(NA, length(res_reg_mean), ncol(res_reg_mean[[i]]))
for (i in 1:length(res_reg_mean)) {
  print(i)
  improvement_reg[i, ] = res_reg_mean[[i]][2000, ] - res_reg_mean[[i]][11,]
}
apply(improvement_reg, 2, mean)[5]


mean(improvement_bin[,5])
mean((diffs4_auc + diffs5_auc))
mean(diffs4_auc)

# Correlation between the curves
# binary
cor_pearson_bin_list = cor_kendall_bin_list = cor_spearman_bin_list = list()
for(i in 1:length(res_bin_mean)) {
  print(i)
  cor_pearson_bin_list[[i]] = try(cor(res_bin_mean[[i]][, c(1,3,4,5)], use = "complete.obs", method = "pearson"), silent = TRUE)
  cor_kendall_bin_list[[i]] = try(cor(res_bin_mean[[i]][, c(1,3,4,5)], use = "complete.obs", method = "kendall"), silent = TRUE)
  cor_spearman_bin_list[[i]] = try(cor(res_bin_mean[[i]][, c(1,3,4,5)], use = "complete.obs", method = "spearman"))
}

done = which(sapply(cor_kendall_bin_list, is.matrix))
cor_pearson_bin = apply(simplify2array(cor_pearson_bin_list[done]), 1:2, mean)
cor_kendall_bin = apply(simplify2array(cor_kendall_bin_list[done]), 1:2, mean)
cor_spearman_bin = apply(simplify2array(cor_spearman_bin_list[done]), 1:2, mean)
apply(simplify2array(cor_kendall_bin_list[done]), 1:2, sd)

cor_pearson_bin = data.frame(cor_pearson_bin)
cor_pearson_bin[1, 1:4] = cor_kendall_bin[1, 1:4] 
cor_pearson_bin[2, 2:4] = cor_kendall_bin[2, 2:4] 
cor_pearson_bin[3, 3:4] = cor_kendall_bin[3, 3:4] 
cor_pearson_bin[4, 4] = cor_kendall_bin[4, 4] 
measure_names_bin = c("error rate", "balanced error rate", "Brier score", "logarithmic loss", "AUC")
colnames(cor_pearson_bin) = rownames(cor_pearson_bin) =  measure_names_bin[c(1,3,4,5)]
library(xtable)
xtable(cor_pearson_bin, caption = "Linear (bottom-left) and rank (top-right) correlation results for all dataset", 
  label = "tab:cor_bin")

# multiclass
cor_pearson_multi_list = cor_kendall_multi_list = cor_spearman_multi_list = list()
for(i in 1:length(res_multi_mean)) {
  print(i)
  cor_pearson_multi_list[[i]] = try(cor(res_multi_mean[[i]][, c(1,3,4,5)], use = "complete.obs", method = "pearson"), silent = TRUE)
  cor_kendall_multi_list[[i]] = try(cor(res_multi_mean[[i]][, c(1,3,4,5)], use = "complete.obs", method = "kendall"), silent = TRUE)
  cor_spearman_multi_list[[i]] = try(cor(res_multi_mean[[i]][, c(1,3,4,5)], use = "complete.obs", method = "spearman"))
}

done = which(sapply(cor_kendall_multi_list, is.matrix))
cor_pearson_multi = apply(simplify2array(cor_pearson_multi_list[done]), 1:2, mean)
cor_kendall_multi = apply(simplify2array(cor_kendall_multi_list[done]), 1:2, mean)
cor_spearman_multi = apply(simplify2array(cor_spearman_multi_list[done]), 1:2, mean)
apply(simplify2array(cor_kendall_multi_list[done]), 1:2, sd)

cor_pearson_multi = data.frame(cor_pearson_multi)
cor_pearson_multi[1, 1:4] = cor_kendall_multi[1, 1:4] 
cor_pearson_multi[2, 2:4] = cor_kendall_multi[2, 2:4] 
cor_pearson_multi[3, 3:4] = cor_kendall_multi[3, 3:4] 
cor_pearson_multi[4, 4] = cor_kendall_multi[4, 4] 
measure_names_multi = c("error rate", "balanced error rate", "brier score", "logarithmic loss", "AUC")
colnames(cor_pearson_multi) = rownames(cor_pearson_multi) =  measure_names_multi[c(1,3,4,5)]
library(xtable)
xtable(cor_pearson_multi, caption = "Linear (bottom-left) and rank (top-right) correlation results for all dataset", 
  label = "tab:cor_multi")


# regression
cor_pearson_reg_list = cor_kendall_reg_list = cor_spearman_reg_list = list()
for(i in 1:length(res_reg_mean)) {
  print(i)
  cor_pearson_reg_list[[i]] = try(cor(res_reg_mean[[i]][, c(1,2,4,3)], use = "complete.obs", method = "pearson"), silent = TRUE)
  cor_kendall_reg_list[[i]] = try(cor(res_reg_mean[[i]][, c(1,2,4,3)], use = "complete.obs", method = "kendall"), silent = TRUE)
  cor_spearman_reg_list[[i]] = try(cor(res_reg_mean[[i]][, c(1,2,4,3)], use = "complete.obs", method = "spearman"))
}

done = which(sapply(cor_kendall_reg_list, is.matrix))
cor_pearson_reg = apply(simplify2array(cor_pearson_reg_list[done]), 1:2, mean)
cor_kendall_reg = apply(simplify2array(cor_kendall_reg_list[done]), 1:2, mean)
cor_spearman_reg = apply(simplify2array(cor_spearman_reg_list[done]), 1:2, mean)
apply(simplify2array(cor_kendall_reg_list[done]), 1:2, sd)

cor_pearson_reg = data.frame(cor_pearson_reg)
cor_pearson_reg[1, 1:4] = cor_kendall_reg[1, 1:4] 
cor_pearson_reg[2, 2:4] = cor_kendall_reg[2, 2:4] 
cor_pearson_reg[3, 3:4] = cor_kendall_reg[3, 3:4] 
cor_pearson_reg[4, 4] = cor_kendall_reg[4, 4] 
measure_names_reg = c("mse", "mae", "medae", "medse")
colnames(cor_pearson_reg) = rownames(cor_pearson_reg) =  measure_names_reg[c(1,2,4,3)]
library(xtable)
xtable(cor_pearson_reg, caption = "Linear (bottom-left) and rank (top-right) correlation results for all dataset", 
  label = "tab:cor_reg")
