lpSVM <- list(type = "Classification",
              library = "kernlab",
              loop = NULL) 

prm <- data.frame(parameter = c("C", "sigma"),
                  class = rep("numeric", 2),
                  label = c("Cost", "Sigma"))

lpSVM$parameters <- prm

svmGrid <- function(x, y, len = NULL, search = "grid") {
  library(kernlab)
  ## This produces low, middle and high values for sigma 
  ## (i.e. a vector with 3 elements). 
  sigmas <- sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)  
  ## To use grid search:
  if(search == "grid") {
    out <- expand.grid(sigma = mean(as.vector(sigmas[-2])),
                       C = 2 ^((1:len) - 3))
  } else {
    ## For random search, define ranges for the parameters then
    ## generate random values for them
    rng <- extendrange(log(sigmas), f = .75)
    out <- data.frame(sigma = exp(runif(len, min = rng[1], max = rng[2])),
                      C = 2^runif(len, min = -5, max = 8))
  }
  out
}

lpSVM$grid <- svmGrid


svmFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) { 
  ksvm(x = as.matrix(x), y = y,
       kernel = laplacedot,
       kpar = list(sigma = param$sigma),
       C = param$C,
       prob.model = classProbs,
       ...)
}

lpSVM$fit <- svmFit

svmPred <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
lpSVM$predict <- svmPred


svmProb <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type="probabilities")
lpSVM$prob <- svmProb


svmSort <- function(x) x[order(x$C),]
lpSVM$sort <- svmSort


lpSVM$levels <- function(x) lev(x)
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

