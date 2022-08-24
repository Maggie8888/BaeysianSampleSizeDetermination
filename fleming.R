############################################################################
#                      Fleming's One-Sample Multistage                     #
#  Papers:                                                                 #   
#  - Fleming, T. R. (1982). One-sample multiple testing procedure for      #
#        phase II clinical trials. Biometrics, 143-151.                    #
#  - Schultz, J. R., Nichol, F. R., Elfring, G. L., & Weed, S. D. (1973).  #
#        Multiple-stage procedures for drug screening. Biometrics, 293-300.#
#                                                                          #                                                       #
#  Variable Needed:                                                        #
#    - K:      number of Stages                                            #
#    - p0:     response rate of a poor treatment                           #
#    - pa:     response rate of a good treatment                           #
#    - sample: number of patients in each look                             #
#    - alpha:  type-1 error probabilities                                  #
############################################################################

# binomial function 
Bg = function(v, p, g,n) {
  choose(n[g], v)*p^v*(1-p)^(n[g]-v)
}

# recursive function, assumptions are: a_0 = -1, and r_0 = 1
Cg = 0
Cg_calc <- function(m,p,g, a, r, n){
  # lower and upper bounds definition 
  if(g-1 <= 0){
    Ee = max(-1+1, m-n[g]) # lower bound, a_0 = -1
    Ff = min(1-1, m)       # upper bound, r_0 = 1
  } else {
    Ee = max(a[g-1]+1, m-n[g]) # lower bound
    Ff = min(r[g-1]-1, m)      # upper bound 
  }
  # setting up the recursive statement 
  if(g == 0) {
    Cg = 1 # stop when g-1 is 0, bottom of the recursion 
  } else {
    if(Ee <= Ff){ # making sure lower bound is smaller than upper bound 
      for(j in Ee:Ff){
        Cg = Cg + Cg_calc(j, p, g-1, a,r,n)*Bg(m-j, p, g,n)
      }
    } else{
      Cg = 0 # if lower bound is bigger then upper then the summation is 0 
    }
  } 
  return(Cg) # returning the summation 
}

# the probability of accepting H0 at stage g is then: Lg
Lg_calc <- function(g, p, a, r, n){
  Lg = 0
  if(g-1 == 0){
    lower = 0 # here a_0 = -1 
  } else {
    lower = a[g-1]+1
  }
  if(lower<= a[g]){ # controlling for the summation not having proper bounds 
    for(m in (lower:a[g])){ # summation 
      Lg = Lg + Cg_calc(m, p, g, a, r, n)
    }
  }
  return(Lg)
}

# the probability of rejecting H0 at stage g is: Rg
Rg_calc <- function(g, p, a, r, n){
  Rg = 0
  if(g-1 == 0){
    upper = 1+n[g]-1 # here r_0 = 1 
  } else {
    upper = (r[g-1]+n[g]-1)
  }
  if(r[g] <= upper){ # controlling for the summation not having proper bounds 
    for(m in r[g]:upper){ # summation 
      Rg = Rg + Cg_calc(m, p, g, a, r, n)
    }
  }
  return(Rg)
}

# average sample number 
ASN_calc <- function(p, K, a, r, n){
  ASN = 0
  for(g in 1:K){
    ASN = ASN + sum(n[1:g])*(Lg_calc(g, p, a, r, n) + Rg_calc(g, p, a, r, n))
  }
  return(ASN)
}

# putting all the metrics together 
metrics <- function(p0, pa, K, a, r, n){
  # alpha 
  sum_Lg = 0
  for(i in 1:K){ 
    sum_Lg = sum_Lg + Lg_calc(i, p0, a, r, n)
  }
  alpha_prime = round(1 - sum_Lg, 3)
  
  # power
  sum_Rg = 0 
  for(i in 1:K){ 
    sum_Rg = sum_Rg + Rg_calc(i, pa, a, r, n)
  }
  power = round(sum_Rg, 3)
  
  # sample size 
  ASN_pa = ASN_calc(p=pa, K, a, r, n)
  ASN_p0 = ASN_calc(p=p0, K, a, r, n)
  
  # return the info 
  return(data.frame(ASN_p0, ASN_pa, alpha_prime, power))
}

# # Fleming Multistage Stage (putting it all together)
# fleming <- function(K, p0, pa, sample, alpha ) {
#   # variable initiation 
#   r  = rep(NA, K)
#   a  = rep(NA, K)
#   Cg = 0
#   za = -(qnorm(alpha))
#   N  = sum(sample)
#   
#   # setting the pa based on the other information 
#   pa_prime = (((N*p0)^(1/2))+((1-p0)^(1/2))*za)^2/(N+za^2)
#   
#   # setting up the acceptance region and rejection region 
#   for(i in 1:K){
#     r[i] = round(sum(sample[1:i])*p0 + za*(N*p0*(1-p0))^(1/2))+1
#     a[i] = round(sum(sample[1:i])*pa_prime - za*(N*pa_prime*(1-pa_prime))^(1/2))
#   }
#   
#   # setting the ASN for p0 and pa, alpha, and power 
#   metric_save = metrics(p0, pa, K, a, r, n = sample)
#   # returning all the information  
#   return(data.frame(
#     sample,
#     r,
#     a,
#     pa_prime,
#     ASN_p0 = metric_save$ASN_p0,
#     ASN_pa = metric_save$ASN_pa,
#     alpha_prime = metric_save$alpha_prime,
#     power= metric_save$power
#   ))
#   
# }

# fleming(K=2, p0=0.3, pa=0.5, sample = c(20, 15),     alpha = 0.05)
# fleming(K=3, p0=0.3, pa=0.5, sample = c(15, 10, 10), alpha = 0.05)
# fleming(K=2, p0=0.3, pa=0.5, sample = c(20, 20),     alpha = 0.05)
# fleming(K=2, p0=0.2, pa=0.4, sample = c(25, 20),     alpha = 0.05)
# fleming(K=3, p0=0.2, pa=0.4, sample = c(15, 15, 15), alpha = 0.05)



# Fleming Two-Stage (making the outputs looks more presentable)
fleming_twostage <- function(K, p0, pa, sample, alpha ) {
  # variable initiation 
  r  = rep(NA, K)
  a  = rep(NA, K)
  Cg = 0
  za = -(qnorm(alpha))
  N  = sum(sample)
  
  # setting the pa based on the other information 
  pa_prime = (((N*p0)^(1/2))+((1-p0)^(1/2))*za)^2/(N+za^2)
  
  # setting up the acceptance region and rejection region 
  for(i in 1:K){
    r[i] = round(sum(sample[1:i])*p0 + za*(N*p0*(1-p0))^(1/2))+1
    a[i] = round(sum(sample[1:i])*pa_prime - za*(N*pa_prime*(1-pa_prime))^(1/2))
  }
  
  # setting the ASN for p0 and pa, alpha, and power 
  metric_save = metrics(p0, pa, K, a, r, n = sample)
  # returning all the information  
  return(data.frame(
    N = sum(sample),
    pa_tilda = round(pa_prime,3),
    n1 = sample[1],
    n2 = sample[2],
    a1 = a[1],
    a2 = a[2],
    r1 = r[1],
    r2 = r[2],
    ASN_p0 = round(metric_save$ASN_p0,2),
    ASN_pa = round(metric_save$ASN_pa,2),
    alpha_prime = metric_save$alpha_prime,
    power = metric_save$power
  ))
  
}
# fleming_twostage(K=2, p0=0.3, pa=0.5, sample = c(20, 15), alpha = 0.05)
# fleming_twostage(K=2, p0=0.3, pa=0.5, sample = c(20, 20), alpha = 0.05)
# fleming_twostage(K=2, p0=0.2, pa=0.4, sample = c(25, 20), alpha = 0.05)