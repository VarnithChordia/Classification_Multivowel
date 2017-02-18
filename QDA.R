library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

###Function to run QDA on training data and estimate class of y_i's for test data

homemade.qda <- function(train = train.data, test = test.data){
  temp.train <- train
  temp.test <- test
  m <- dim(temp.test)[1]
  N <- dim(temp.train)[1]
  K <- max(unique(temp.train$y))
  
  delta.mat <- matrix(NA, nrow = m, ncol = K)
  predictions <- numeric(m)
  
  for (k in 1:K){
    data.k <- subset(temp.train, temp.train$y == k)
    X.k <- data.k[,-1]
    n.k <- dim(data.k)[1]
    pi.k <- n.k/N
    
    mu.k <- colSums(X.k)/n.k
    cov.k <- cov(X.k)
    eigen.k <- eigen(cov.k)
    U.k = eigen.k$vectors
    #eigen.k$values
    D.k <- diag(x = eigen.k$values)
    
    X.test <- temp.test[,-1]
    
    for (j in 1:m){
      x <- X.test[j,]
      delta <- -.5* t(t(U.k) %*% t(x - mu.k)) %*% solve(D.k) %*% (t(U.k) %*%   t(x-mu.k)) - .5*log(sum(eigen.k$values)) +log(pi.k)
      delta.mat[j,k] <- delta
    }
  }
  return(delta.mat)
}
my.delt <- homemade.qda(train = vowel.train, test = vowel.test)
predictions <- apply(my.delt, 1, which.max)
sum(vowel.test$y == predictions)/dim(vowel.test)[1] #error rate