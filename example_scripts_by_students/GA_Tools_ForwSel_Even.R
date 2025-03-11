#### Function for forward selection of CCA variables ####
### By Even Garvang

### Takes a species-plot matrix, an environmental variable-plot matrix, a significance level
### and number of iterations for the Monte Carlo test as input, and returns
### a vector of variables which significantly increase the variation explained

### Not thoroughly tested, and there is probably room for improvement
### Could probably easily be generalized to support both cca and rda

forward_selection <- function(ART, env, a = 0.05, n.iter = 999){
  
  variable <- NULL
  variables <- NULL
  x <- 0
  
  while(x == 0){
    if(ncol(env) == 0){
      break
    }
    eig <- rep(NA, ncol(env))
    pvals <- rep(NA, ncol(env))
    
    if(is.null(variables)){
      for(i in 1:ncol(env)){
        CCA <- cca(ART ~ env[[i]])
        eig[i] <- CCA$CCA$eig
        test_CCA <- permutest(CCA, permutations = n.iter)
        pvals[i] <- (sum(test_CCA$F.0 < test_CCA$F.perm) + 1) / (n.iter + 1)
      }
    }else{
      for(i in 1:ncol(env)){
        CCA <- cca(ART ~ env[[i]] + Condition(as.matrix(env2[variables])))
        eig[i] <- CCA$CCA$eig
        test_CCA <- permutest(CCA, permutations = n.iter)
        pvals[i] <- (sum(test_CCA$F.0 < test_CCA$F.perm) + 1) / (n.iter + 1)
      }
    }
    
    
    crit <- a/ncol(env)
    
    if(pvals[last(order(eig))] < crit){
      variable <- names(env[last(order(eig))])
      variables <- c(variables, variable)
      if(length(variables) == 1){
        env2 <- env[names(env) %in% variable]
      }else{
        env2 <- cbind(env2, env[names(env) %in% variable])
      }
      env <- env[!names(env) %in% variable]
    }else{x <- 1}
    
  }
  return(variables)
}
