##########
# functions needed: 
go_nogo_decisions <- function(n, N, lower, upper, rate) {
  result <- NULL
  nsim   <- 100000 
  for(i in rate){
    sample1   <- rbinom(nsim, size = n, prob = i) # the number of people in each trial differs 
    sample2   <- rbinom(nsim, size = N-n, prob = i)
    sample12  <- sample1+sample2 # putting those two sample together 
    pet       <- sum(sample1<lower)/nsim  
    prob.go   <- sum((sample12>=upper)& (sample1>=lower))/nsim  
    prob.nogo <- 1-prob.go # Since there are only two decisions then the opposite of go it no-go. 
    result1   <- c(i, pet, prob.go, prob.nogo) 
    result    <- rbind(result, result1)
  }
  return(result)
}
# finding the posterior distribution using Simon's output 
posterior <- function(n, LRV, TV, prior_a, prior_b) {
  LRVvec = TVvec = rep(NA,n+1)
  for(y in 0:n){
    LRVvec[y+1] <- 1-pbeta(LRV, y + prior_a, n - y + prior_b)
    TVvec[y+1]  <- 1-pbeta(TV,  y + prior_a, n - y + prior_b) 
  }
  prob = as_tibble(cbind(n= 0:n,  TVvec, LRVvec ))
  return(prob)
} 
# for the dual criteria 
dcdesign = function(n, orr, go_cut, nogo_cut){
  pgo   = 1 - pbinom(go_cut-1, n, orr) ### P(GO) 
  pnogo = pbinom(nogo_cut, n, orr)     ### P(NO-GO)
  pinc  = 1- pgo - pnogo               ### P(Inconclusive) 
  
  dcresult = cbind(rep(n, length(orr)), orr, pgo, pnogo, pinc)
  return(dcresult)
}
# finding the posterior distribution 
find_posterior <- function(n, threshold1, threshold2, LRV, TV,prior_a, prior_b){
  LRVvec1 = TVvec1 = rep(NA,n+1)
  for(y in 0:n){
    LRVvec1[y+1] <- 1-pbeta(LRV, y + prior_a, n - y + prior_b)
    TVvec1[y+1]  <- 1-pbeta(TV, y + prior_a, n - y + prior_b) 
  }
  okay  = (LRVvec1 >= threshold2)+(TVvec1 >= threshold1) 
  probs = cbind(0:n, TVvec1, LRVvec1, okay)
  return(probs)
}
decision_table <- function(n, threshold1, threshold2, orr, LRV, TV, prior_a, prior_b){
  dcres = NULL
  sample <- as_tibble(find_posterior(n, threshold1 ,  threshold2, LRV, TV,prior_a, prior_b) )
  go_cut = min(which(sample$okay==2))  # finding first instance of 2. both criteria met
  nogo_cut = max(which(sample$okay==0))# finding last instance of 0. neither criteria met
  dcresult = dcdesign(n, orr, go_cut, nogo_cut)
  dcres = rbind(dcres, dcresult)
  colnames(dcres) = c("N", "ORR", "P(GO)", "P(NO-GO)", "P(INDT)")
  dcres2 = as_tibble(dcres)
  return(dcres2)
}
TMAX <- 4 # specify maximal number of dynamic panels
TMAX2 <- 4 # specify maximal number of dynamic panels
# end of functions needed 
##########
