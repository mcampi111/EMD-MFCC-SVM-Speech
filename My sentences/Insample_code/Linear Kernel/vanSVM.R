vanSVM <- list(type = "Classification",
              library = "kernlab",
              loop = NULL) 

prm <- data.frame(parameter = c("C"),
                  class = rep("numeric", 1),
                  label = c("Cost"))

vanSVM$parameters <- prm

svmGrid <- function(x, y, len = NULL, search = "grid") {
  library(kernlab)
   
  ## To use grid search:
  if(search == "grid") {
    
    out <- expand.grid( C = 2 ^((1:len) - 3))
    
  } else {
    ## For random search, define ranges for the parameters then
    ## generate random values for them
    rng <- extendrange(log(sigmas), f = .75)
    out <- data.frame(sigma = exp(runif(len, min = rng[1], max = rng[2])),
                      C = 2^runif(len, min = -5, max = 8))
  }
  out
}

vanSVM$grid <- svmGrid


svmFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) { 
  ksvm(x = as.matrix(x), y = y,
       kernel = vanilladot,
       kpar = list(),
       C = param$C,
       prob.model = classProbs,
       ...)
}

vanSVM$fit <- svmFit

svmPred <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
vanSVM$predict <- svmPred


svmProb <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type="probabilities")
vanSVM$prob <- svmProb


svmSort <- function(x) x[order(x$C),]
vanSVM$sort <- svmSort


vanSVM$levels <- function(x) lev(x)
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
#               method = lpSVM,
#               tuneLength = 8)      # 
