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


