load(paste(dir,"/results/clas.RData", sep = ""))
load(paste(dir,"/results/reg.RData", sep = ""))
tasks = rbind(clas_small, reg_small)
LEARNERIDS = c("randomForest")

OMLDATASETS = tasks$data.id[!(tasks$task.id %in% c(1054, 1071, 1065))] # Cannot guess task.type from data! for these 3

