library(mlr)
library(OpenML)
require(devtools)
install_version("batchtools", version = "0.9.0", repos = "http://cran.us.r-project.org")
library(batchtools)
library(plyr)

dir =  "../Supplementary_File_5"
setwd(paste0(dir,"/results"))
options(batchtools.progress = TRUE)

regis = loadRegistry("ntree")

# Classification
# Binary
bin = which(clas_small$number.of.classes == 2)
res_bin = reduceResultsList(ids = bin, reg = regis)

# Averaging of the results
res_bin_mean = list()

done = !sapply(res_bin, is.null)
for(i in 1:length(res_bin)) { # slow but easy to understand;
  print(i)
  res_bin_mean[[i]] = matrix(NA, 2000, 5)
  
  if(isTRUE(done[i])) {
    res = res_bin[[i]]
    nas = lapply(res, `[[`, 3)
    nas = pmax(10, sapply(nas, function(x) max(which(is.na(x)))))
    
    for(j in 1:5){
      resj = lapply(res, `[[`, j)
      resj = as.data.frame(resj[1:1000], col.names = 1:1000)
      for(k in 1:ncol(resj))
        resj[1:nas[k], k] = NA
      resj = apply(resj, 1, function(x) mean(x, na.rm = T))
      res_bin_mean[[i]][, j] =  resj
    }
  }
}

measure_names_bin = c("error rate", "balanced error rate", "Brier score", "logarithmic loss", "AUC")
pdf("../results/graphics/binary_ntree.pdf", width = 9, height = 6)
for(i in 1:length(res_bin)) {
  if(isTRUE(done[i])) {
    par(mfrow = c(2, 2), mar = c(3, 3.3, 2, 0) + 0.1)
    main = paste("Binary classification ",i," // OpenML ID", OMLDATASETS[bin[i]])
    for(j in c(1,3:5)){ # omit balanced error rate
      plot(res_bin_mean[[i]][1:500, j], type = "l", main =  "", 
        xlab = "Number of trees", ylab = paste("Mean OOB", measure_names_bin[j]), mgp = c(2,1,0))
    }
    mtext(main, side = 3, line = -1.5, outer = TRUE)
  }
}
dev.off()

# Examples for the paper (with first trees included)
res = reduceResultsList(ids = 36, reg = regis)
nas = lapply(res[[1]], `[[`, 3)
nas = sapply(nas, function(x) max(which(is.na(x))))
res = lapply(res[[1]], `[[`, 1)
res = as.data.frame(res[1:1000], col.names = 1:1000)
for(i in 1:ncol(res))
  res[1:nas[i], i] = NA
res1 = apply(res, 1, function(x) mean(x, na.rm = T))

res = reduceResultsList(ids = 64, reg = regis)
nas = lapply(res[[1]], `[[`, 3)
nas = sapply(nas, function(x) max(which(is.na(x))))
res = lapply(res[[1]], `[[`, 1)
res = as.data.frame(res[1:1000], col.names = 1:1000)
for(i in 1:ncol(res))
  res[1:nas[i], i] = NA
res2 = apply(res, 1, function(x) mean(x, na.rm = T))

res = reduceResultsList(ids = 160, reg = regis)
nas = lapply(res[[1]], `[[`, 3)
nas = sapply(nas, function(x) max(which(is.na(x))))
res = lapply(res[[1]], `[[`, 1)
res = as.data.frame(res[1:1000], col.names = 1:1000)
for(i in 1:ncol(res))
  res[1:nas[i], i] = NA
res3 = apply(res, 1, function(x) mean(x, na.rm = T))

save(res1, res2, res3, file = "graphic_results.RData")

# Multiclass
multi = which(clas_small$number.of.classes != 2)
res_multi = reduceResultsList(ids = multi, reg = regis)

# Averaging of the results
res_multi_mean = list()

done = !sapply(res_multi, is.null)
for(i in 1:length(res_multi)) { # slow but easy to understand
  print(i)
  res_multi_mean[[i]] = matrix(NA, 2000, 5)
  
  if(isTRUE(done[i])) {
    res = res_multi[[i]]
    nas = lapply(res, `[[`, 3)
    nas = pmax(10, sapply(nas, function(x) max(which(is.na(x)))))
    
    for(j in 1:5){
      resj = lapply(res, `[[`, j)
      resj = as.data.frame(resj[1:1000], col.names = 1:1000)
      for(k in 1:ncol(resj))
        resj[1:nas[k], k] = NA
      resj = apply(resj, 1, function(x) mean(x, na.rm = T))
      res_multi_mean[[i]][, j] =  resj
    }
  }
}

measure_names_multi = c("error rate", "balanced error rate", "brier score", "logarithmic loss", "AUC")

pdf("../results/graphics/multiclass_ntree.pdf", width = 9, height = 6)
for(i in 1:length(res_multi)) {
  if(isTRUE(done[i])) {
    par(mfrow = c(2, 2), mar = c(3, 3.3, 2, 0) + 0.1)
    main = paste("Multiclass classification ",i," // OpenML ID", OMLDATASETS[multi[i]])
    for(j in c(1,3:5)){ # omit balanced error rate
      plot(res_multi_mean[[i]][1:500, j], type = "l", main =  "", 
        xlab = "Number of trees", ylab = paste("Mean OOB", measure_names_multi[j]), mgp = c(2,1,0))
    }
    mtext(main, side = 3, line = -1.5, outer = TRUE)
  }
}
dev.off()
# Effects at the beginning of the curves can partially be explained because of the 
# smaller data basis -> higher variance


# Regression
res_reg = reduceResultsList(ids = 194:312, reg = regis)

# Averaging of the results
res_reg_mean = list()

done = !sapply(res_reg, is.null)
for(i in 1:length(res_reg)) { # slow but easy to understand
  print(i)
  res_reg_mean[[i]] = matrix(NA, 2000, 5)
  res = res_reg[[i]]
  nas = lapply(res, `[[`, 3)
  nas = pmax(10, sapply(nas, function(x) max(which(is.na(x)))))
  
  for(j in 1:5){
    resj = lapply(res, `[[`, j)
    resj = as.data.frame(resj[1:1000], col.names = 1:1000)
    for(k in 1:ncol(resj))
      resj[1:nas[k], k] = NA
    resj = apply(resj, 1, function(x) mean(x, na.rm = T))
    res_reg_mean[[i]][, j] =  resj
  }
}

measure_names_reg = c("mse", "mae", "medae", "medse")

pdf("../results/graphics/regression_ntree.pdf", width = 9, height = 6)
for(i in 1:length(res_reg)) {
  par(mfrow = c(2, 2), mar = c(3, 3.3, 2, 0) + 0.1)
  main = paste("Regression ",i," // OpenML ID", OMLDATASETS[c(194:312)[i]])
  for(j in c(1:2,4, 3)){ # omit R-Squared which is equivalen to MSE
    plot(res_reg_mean[[i]][1:500, j], type = "l", main =  "", 
      xlab = "Number of trees", ylab = paste("Mean OOB", measure_names_reg[j]), mgp = c(2,1,0))
  }
  mtext(main, side = 3, line = -1.5, outer = TRUE)
}
dev.off()

save(res_bin_mean, res_multi_mean, res_reg_mean, file = "/home/probst/Paper/Ntree_RandomForest/experiments/results/results.RData")
load("../results/results.RData")

# Analyze the results...

# For the oob error rate
# Difference from bottom to the end
n_bin = length(res_bin_mean)
diffs = numeric(n_bin)
diffs2 = numeric(n_bin)
for (i in 1:n_bin) {
  print(i)
  resi = res_bin_mean[[i]]
  diffs[i] = resi[2000, 1] - min(resi[10:150, 1])
  diffs2[i] = min(resi[151:2000,]) - min(resi[10:150, 1])
}

par(mfrow = c(1, 1))
boxplot(diffs)
sum(diffs > 0.005, na.rm = T) 
sum(diffs > 0.005, na.rm = T) / length(diffs2)
sum(diffs[1:75] > 0.005, na.rm = T)
sum(diffs[76:149] > 0.005, na.rm = T)

boxplot(diffs2)
sum(diffs2 > 0.001, na.rm = T)
sum(diffs2 < -0.001, na.rm = T)
sum(diffs2[1:75] > 0.001, na.rm = T)
sum(diffs2[76:149] > 0.001, na.rm = T)

# What is the average improvement from training 2000 instead of 11 trees
improvement_bin = matrix(NA, length(res_bin_mean), ncol(res_bin_mean[[i]]))
for (i in 1:length(res_bin_mean)) {
  print(i)
  improvement_bin[i, ] = res_bin_mean[[i]][2000, ] - res_bin_mean[[i]][11,]
}
apply(improvement_bin, 2, mean)

improvement_multi = matrix(NA, length(res_multi_mean), ncol(res_multi_mean[[i]]))
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
colnames(cor_pearson_reg) = rownames(cor_pearson_reg) =  measure_names_reg[c(1,2,4,3)]
library(xtable)
xtable(cor_pearson_reg, caption = "Linear (bottom-left) and rank (top-right) correlation results for all dataset", 
  label = "tab:cor_reg")

