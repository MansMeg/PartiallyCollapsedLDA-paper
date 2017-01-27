#' @title
#' simulateLDA
#' 
#' @description
#' Function to simulate data from a LDA model
#' 
#' @param K The number of topics
#' @param V The vocabulary size (1000 is default)
#' @param D The number of documents/observations (5000 is default)
#' @param mu_Nd The mean number of poissondistributed words per document (500 is default)
#' @param alphaPhi The dirichlet parameter for Phi (0.01 is default)
#' @param alphaTheta The dirichlet parameter for Theta (0.1 is default)
#' @param seed The random seed to use to simulate dataset.
#' 
#' @return
#' A list with simulated data and parameters.
#' 

simulateLDA <- function(K = 5, V = 1000, D = 1000, mu_Nd=100, alphaPhi=0.01, alphaTheta=0.1, seed=NULL){
  require("MCMCpack")

  if(!is.null(seed)) set.seed(seed)  
  params <- list(K=K, V=V,  D=D, mu_Nd=mu_Nd, alphaPhi=alphaPhi, alphaTheta=alphaTheta, seed=seed)
  
  # Phi matrix
  Phi <- r_dirichlet(n=K, alpha=rep(alphaPhi,V))
  while(any(is.nan(Phi))){
    logi <- apply(is.nan(Phi), 1, any)
    Phi[logi,] <- rdirichlet(n=sum(logi), alpha=rep(alphaPhi,K))
  }
  
  # Theta matrix
  Theta <- rdirichlet(n=D, alpha=rep(alphaTheta, K))
  while(any(is.nan(Theta))){
    logi <- apply(is.nan(Theta), 1, any)
    Theta[logi,] <- rdirichlet(n=sum(logi), alpha=rep(alphaTheta,K))
  }
  
  # Sample the number of Words for each document
  Nd <- rpois(n=D, lambda=mu_Nd)
  
  # Sample Topics and Words
  wordAndTopicList <- list()
  for (i in 1:D){
    topics <- sample(1:K, size=Nd[i], replace=TRUE, prob=Theta[i,])    
    words <- unlist(lapply(topics, 
                           FUN=function(X) sample(1:V, size=1, replace=TRUE, prob=Phi[X,])))
    wordAndTopicList[[i]] <- list(words)
    wordAndTopicList[[i]][[2]] <- topics
  }
  
  return(list(corpus = wordAndTopicList, Phi = Phi, Theta = Theta, Parameters = params))  
}
