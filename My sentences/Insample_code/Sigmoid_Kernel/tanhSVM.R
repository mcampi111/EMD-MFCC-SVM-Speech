tanhSVM <- list(type = "Classification",
              library = "kernlab",
              loop = NULL) 

prm <- data.frame(parameter = c("C", "scale", "offset"),
                  class = rep("numeric", 3),
                  label = c("Cost", "Scale", "Offset"))

tanhSVM$parameters <- prm

svmGrid <- function(x, y, len = NULL, search = "grid") {
  library(kernlab)
  ## This produces low, middle and high values for sigma 
  ## (i.e. a vector with 3 elements). 
  sigmas <- sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)  
  ## To use grid search:
  if(search == "grid") {
    out <- expand.grid(scale = mean(as.vector(sigmas[-2])), offset = 2 ^(1:(len-5) - 6), 
                       C = 2 ^((1:len) - 3) )
  } else {
    ## For random search, define ranges for the parameters then
    ## generate random values for them
    rng <- extendrange(log(sigmas), f = .75)
    out <- data.frame(scale = exp(runif(len, min = rng[1], max = rng[2])),  offset = 1:2,
                      C = 2^runif(len, min = -5, max = 8))
  }
  out
}

tanhSVM$grid <- svmGrid


svmFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) { 
  ksvm(x = as.matrix(x), y = y,
       kernel = tanhdot,
       kpar = list(scale = param$scale, offset = param$offset),
       C = param$C,
       prob.model = classProbs,
       ...)
}

tanhSVM$fit <- svmFit

svmPred <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
tanhSVM$predict <- svmPred


svmProb <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type="probabilities")
tanhSVM$prob <- svmProb


svmSort <- function(x) x[order(x$C),]
tanhSVM$sort <- svmSort


tanhSVM$levels <- function(x) lev(x)
function(x) levels(x@data@get("response")[,1])




# train_control<- trainControl(method="repeatedcv", 
#                              number=2, 
#                              classProbs=TRUE,  
#                              summaryFunction = twoClassSummary, 
#                              savePredictions = TRUE, 
#                              repeats = 1)
# 
# 
# 
# prova<- train(y = yf, 
#               x = x_s_ARMAARIMA_IF, 
#               trControl = train_control,
#               preProcess = c("center", "scale"), 
#               metric = 'ROC', 
#               method = tanhSVM,
#               tuneLength = 9)      # 

