library(randomForest)
library(OpenML)
library(mlr)
library(batchtools)

saveOMLConfig(apikey = "6df93acdb13899f48fd6bd689b0dd7cf", arff.reader = "RWeka", overwrite=TRUE)

dir =  "../Supplementary_File_5"
setwd(paste0(dir))
dir = getwd()
source(paste0(dir,"/code/ntree_defs.R"))

unlink("results/ntree", recursive = TRUE)
regis = makeExperimentRegistry("results/ntree", 
                               packages = c("mlr", "OpenML", "randomForest", "OOBCurve", "methods"), 
                               source = paste0(dir, "/code/ntree_defs.R"),
                               work.dir = paste0(dir, "/results"),
                               conf.file = paste0(dir, "/code/.batchtools.conf.R")
)
regis$cluster.functions = makeClusterFunctionsMulticore(ncpus = 10)

# add selected OML datasets as problems
for (did in OMLDATASETS) {
  data = list(did = did)
  addProblem(name = as.character(did), data = data)
}

addAlgorithm("eval", fun = function(job, data, instance, lrn.id, ...) {
  par.vals = list(...)
  oml.dset = getOMLDataSet(data$did)
  task = convertOMLDataSetToMlr(oml.dset)
  type = getTaskType(task)
  target = task$task.desc$target
  iters = 1000
  T = 2000
  run = vector("list", iters) 
  set.seed(105)
  for(i in 1:iters){
    print(paste(i))
    if (type == "classif") {
      data = droplevels(oml.dset$data) # drop not existing levels
      if(is.factor(data[, target]) == FALSE)
        data[, target] = as.factor(data[, target])
      mod = randomForest(as.formula(paste0(target, "~.")), data = data, ntree = T, keep.inbag = TRUE)
      run[[i]] = OOBCurve(mod, measures = list(mmce, ber, multiclass.brier, logloss, multiclass.au1u), task = task, data = data)
    } else {
      mod = randomForest(as.formula(paste0(target, "~.")), data = oml.dset$data, ntree = T, keep.inbag = TRUE)
      run[[i]] = OOBCurve(mod, measures = list(mse, mae, medae, medse, rsq), task = task, data = oml.dset$data)
    }
  }
  run
})

set.seed(130)
ades = data.frame()
for (lid in LEARNERIDS) {
ades = data.frame(t(c(1,2)))
}
addExperiments(algo.designs = list(eval = ades))

summarizeExperiments()
ids = chunkIds(findNotDone(), chunk.size = 1)
submitJobs(ids)

getStatus()
getErrorMessages()
findErrors()

min(getJobStatus()$started, na.rm = T)
max(getJobStatus()$done, na.rm = T) # 7 Minuten -> one night; ca. 1,30h mit 10 iter -> 150h mit 1000 iter

