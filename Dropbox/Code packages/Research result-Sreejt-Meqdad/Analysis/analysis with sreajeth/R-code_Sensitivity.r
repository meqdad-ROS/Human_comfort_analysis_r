library (plyr)
data.df<-read.csv("/home/meqdad/Dropbox/Code packages/Research result-Sreejt-Meqdad/Analysis/Meqdad_Analysis_Phase_1_V1/Sensitivity.csv",",",header=TRUE)
summary(data.df);nrow(data.df)
subset.2<-data.df[,1:4];summary(subset.2)
# "weighted.sd" takes the vector of values (x) and the weights (w), which are
# the loadings here, and returns the weighted standard deviation of the
# vector of values.
weighted.sd = function(x, w){
  sum.w <- sum(w)
  sum.w2 <- sum(w^2)
  mean.w <- sum(x * w) / sum(w)
  x.sd.w <- sqrt((sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2))
  return(x.sd.w)
}
# "re.scale" puts the scores back into the metric of the original questions.
re.scale <- function(f.scores, raw.data, loadings){
  fz.scores <- (f.scores + mean(f.scores))/(sd(f.scores))
  means <- apply(raw.data, 1, weighted.mean, w = loadings)
  sds <- apply(raw.data, 1, weighted.sd, w = loadings)
  grand.mean <- mean(means)
  grand.sd <- mean(sds)
  final.scores <- ((fz.scores * grand.sd) + grand.mean)
  return(final.scores)
}
get.scores.fun <- function(data){
  fact <- factanal(data, factors = 1, scores = "regression")
  f.scores <- fact$scores[,1]
  f.loads <- abs(fact$loadings[,1])
  rescaled.scores <- re.scale(f.scores, data, f.loads)
  output.list <- list(rescaled.scores, f.loads)
  names(output.list) <- c("rescaled.scores", "factor.loadings")
  return(output.list)
}
get.scores.fun(subset.2)
library(plyr)
Sensitivity<-get.scores.fun(subset.2)$rescaled.scores
summary(Sensitivity)
Sensitivity
