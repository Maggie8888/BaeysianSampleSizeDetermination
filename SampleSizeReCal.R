
sim.app.ex <- function(tm1,int.size,s.conds,fc.vec){


  # Set starting values for parameters
  eta    <- s.conds$eta
  zeta   <- s.conds$zeta
  xi     <- s.conds$xi
  r      <- s.conds$r
  d.star <- s.conds$d.star
  d.pts  <- s.conds$d.pts
  per.sp <- s.conds$per.sp
  tot.sp <- s.conds$tot.sp

  q1.c   <- s.conds$q0.c
  q1.e   <- s.conds$q0.e
  u1.e   <- s.conds$u0.e
  u1.c   <- s.conds$u0.c
  D.int  <- 0
  d1.int <- 0

  # Make matrices for changing pars
  a1     <- matrix(NA,ncol=length(int.size),nrow=length(tm1)/2)
  b1     <- matrix(NA,ncol=length(int.size),nrow=length(tm1)/2)

  # Fill first row of matrices
  a1[1,] <- s.conds$a0
  b1[1,] <- s.conds$b0

  # Make Matrices for all n.left and tots
  N.left <- matrix(NA,nrow=length(int.size),ncol=2)
  N.tot  <- matrix(NA,nrow=length(int.size),ncol=2)


  ## Calculate initial sample size required (N.left is sample size left EXC. interim)
  N.left[1,] <- ss.calc(a1[1,1], b1[1,1], eta, zeta, xi, r, q1.e, q1.c, d.star)[4]
  N.tot[1,]  <- N.left[1,1]

  # Set pars for scaling factors
  M       <- rep(NA,length(int.size)-1)
  ci.u    <- rep(NA,length(int.size)-1)
  scal    <- rep(NA,length(int.size)-1)
  fc.int  <- rep(NA,length(int.size)-1)

  # Collection of eq5
  eq5.col <- matrix(NA,ncol=length(int.size),nrow=length(tm1)/2)
  suc.con <- matrix(NA,ncol=length(int.size),nrow=length(tm1)/2)
  fut.con <- matrix(NA,ncol=length(int.size),nrow=length(tm1)/2)

  # Update parameters per 1 patient per group
  for(k in 1:(length(tm1)/2)){ # K IS THE SAMPLE SIZE COUNT PER GROUP


    #Select data from row for control and treatment
    data.c         <- tm1[1:k]
    data.e         <- tm1[(d.pts/2+1):(d.pts/2+k)]

    #Calculate means for the interim
    u.c.mean       <- mean(data.c)
    u.e.mean       <- mean(data.e)

    #Update consistent parameters
    H   <- sum((data.e - u.e.mean)^2) +
      sum((data.c - u.c.mean)^2) +
      (k*q1.c[1]*(u.c.mean-u1.c[1])^2)/(q1.c[1]+k) +
      (k*q1.e[1]*(u.e.mean-u1.e[1])^2)/(q1.e[1]+k)

    q1.c[k+1]        <- q1.c[1] + k
    q1.e[k+1]        <- q1.e[1] + k

    u1.c[k+1]        <- u1.c[1]*(q1.c[1]/q1.c[k+1]) + k*u.c.mean/q1.c[k+1]
    u1.e[k+1]        <- u1.e[1]*(q1.e[1]/q1.e[k+1]) + k*u.e.mean/q1.e[k+1]

    D.int[k+1]       <- (q1.e[k+1]*q1.c[k+1])/(q1.e[k+1]+q1.c[k+1])

    d1.int[k+1]      <- u1.e[k+1]-u1.c[k+1]

    # Changing pars

    #Matrix for a (for all int.sizes)
    a1[k+1,]         <- a1[1,] + k

    #matrix for b (for all int.sizes)
    b1[k+1,]         <- b1[1,] + H/2

    # Did we reach an interim?
    if((k*2) %in% int.size){

      #Which interim size are we at
      int.cont <- which((k*2)==int.size)

      #Regular re-estimation (what is left and total) NO ADAP
      N.left[int.cont,1]         <- ss.calc(a1[k+1,1], b1[k+1,1], eta, zeta, xi, r,
                                            q1.e[k+1], q1.c[k+1], d.star)[4]

      N.tot[int.cont,1]          <- N.left[int.cont,1]+k*2

      #Calculate the observed value for M
      M[int.cont-1]    <- (H*a1[1,int.cont])/((k*2)*b1[1,int.cont])

      #Determine fc.int based on vector input in function
      fc.int[int.cont-1] <- fc.vec

      #Find limits of the ppd of M according to width of interval in fc.int
      M.ci      <- qf(c(0.5-(fc.int[int.cont-1]/2),0.5+(fc.int[int.cont-1]/2)),k*2,2*a1[1,int.cont])

      #Test if M is within M.ci. If TRUE then it is within M.ci
      if(M[int.cont-1]<M.ci[2] & M[int.cont-1]>M.ci[1]){

        #No changing of original alpha0 so scale parameter = 1
        scal[int.cont-1] <- 1

        #save upper limit of ci
        ci.u[int.cont-1] <- M.ci[2]

      } else {

        #Test if M is lower than lower limit
        if(M[int.cont-1]<M.ci[1]){

          #Set scal equal such that df2=1
          scal[int.cont-1] <- 1/a1[1,1]

          #save upper limit of ci
          ci.u[int.cont-1] <- M.ci[2]

        } else {

          #DIfferent scale values for search PRECISION = 0.001
          scal.m  <-seq(0.001,1,0.001)

          #Find corresponding a0 values
          a1.m    <- a1[1,1]*scal.m

          #Find upper limit of CI of ppd
          ci.m.u  <- qf(0.5+(fc.int[int.cont-1]/2),k*2,2*a1.m)

          #Find first occurence where M is included in interval (-1 is because it finds first time M falls out)
          ind.ci  <- which(M[int.cont-1]>ci.m.u)[1]-1

          #Get corresponding index for the scal
          scal[int.cont-1] <- scal.m[ind.ci]

          #save upper limit of ci
          ci.u[int.cont-1] <- ci.m.u[ind.ci]
        }
      }

      #update original a0 with scaled parameter for each int.size
      a1[1,int.cont]          <- a1[1,1]*scal[int.cont-1]
      b1[1,int.cont]          <- b1[1,1]*scal[int.cont-1]

      #Update parameters using new a0 and b0
      a1[k+1,int.cont]        <- a1[1,int.cont] + k
      b1[k+1,int.cont]        <- b1[1,int.cont] + H/2

      #Re-estimated sample size with scale factor
      N.left[int.cont,2] <- ss.calc(a1[k+1,int.cont], b1[k+1,int.cont], eta, zeta, xi, r,
                                    q1.e[k+1], q1.c[k+1], d.star)[4]

      N.tot[int.cont,2]  <- N.left[int.cont,2]+k*2
    }


    #Evaluate criteria eq5 for all settings
    lhs.int         <- (D.int[k+1]*a1[k+1,])/b1[k+1,]
    t.eta.int       <- qt(eta, 2*a1[k+1,])
    t.zeta.int      <- qt(zeta, 2*a1[k+1,])
    rhs.int         <- ((t.eta.int+t.zeta.int)/d.star)^2
    eq5.col[k+1,]   <- lhs.int>=rhs.int

    #Evaluate suc or fut
    suc.bin         <- pt(d1.int[k+1]*sqrt(D.int[k+1]*a1[k+1,]/b1[k+1,]),2*a1[k+1,])
    fut.bin         <- pt((d.star-d1.int[k+1])*sqrt(D.int[k+1]*a1[k+1,]/b1[k+1,]),2*a1[k+1,])

    suc.con[k+1,]   <- suc.bin>=eta
    fut.con[k+1,]   <- fut.bin>=zeta

    #Break if all values of eq5 are true (for all situations)
    if(all(eq5.col[k+1,])==T){break}

  }

  # Fill out rest of N.left matrix with 0
  N.left[is.na(N.left)==T] <- 0

  # Fill out N.tot with interim sizes
  N.tot[which(is.na(N.tot[,1])),] <- int.size[which(is.na(N.tot[,1]))]

  # Look up point where eq5 is true for each situation (takes first true value of each column)
  eq5.inds <- apply(eq5.col,2,function(x) match(T,x))-1

  #make mat for per.left
  eq5.mat  <- cbind(rep(eq5.inds[1],length(int.size)),eq5.inds)*2

  # Calculate percentage of re-est ss
  per.left <- rounder((eq5.mat-cbind(int.size,int.size))/N.left,inc=0.01,"ceiling")

  #Set Inf values to 0
  per.left[per.left==-Inf | per.left==Inf] <- 0

  #Collect suc or fut results
  sucfut   <- c(diag(suc.con[eq5.inds,]),diag(fut.con[eq5.inds,]))

  #Put all results in a vector
  tot.res <- c(as.vector(N.tot),as.vector(per.left),sucfut,scal,M,ci.u,fc.int)

  tot.res
}


ex.conds.sim <- function(list.ex,fc.seq=seq(0.05,0.95,0.1)){



  #Expand grid of conditions
  g.cond <- expand.grid(list.ex)

  #Add variables to grid that need computing
  g.cond$a0       <- g.cond$prior/2
  g.cond$nu.alpha <- g.cond$prior/2
  g.cond$q0.e     <- 0.01 #Uninformative
  g.cond$q0.c     <- 0.01 #Uninformative
  g.cond$b0       <- g.cond$a0*g.cond$sd^2
  g.cond$nu.beta  <- g.cond$nu.alpha*g.cond$sd.data^2
  g.cond$d.star   <- g.cond$DE
  g.cond$d.data   <- g.cond$DE.data
  g.cond$u0.e     <- g.cond$d.data


  #SET SEED
  set.seed(g.cond$seed[1])

  #Create results matrix
  #sim.res  <- matrix(NA,nrow=nrow(g.cond),ncol=8)

  #Collect all sim results
  sim.list <- list()
  tm.list  <- list()
  cum.list <- list()
  col.mat  <- NULL

  #Indices to get nu.alpha and nu.beta to simulate the data
  sdd.dat <- c(match(unique(g.cond$sd.data),g.cond$sd.data),nrow(g.cond))

  #Loop over sd.data
  for (i in 1:length(unique(g.cond$sd.data))){

    #Generate control data points matrix
    m1     <- matrix(rnorm((g.cond$d.pts[sdd.dat[i]]/2)*g.cond$sims[sdd.dat[i]],g.cond$u0.c[sdd.dat[i]],
                           sqrt(1/(g.cond$nu.alpha[sdd.dat[i]]/g.cond$nu.beta[sdd.dat[i]]))),
                     nrow=g.cond$sims[sdd.dat[i]],byrow=T)


    #Generate exp data points matrix
    m2     <- matrix(rnorm((g.cond$d.pts[sdd.dat[i]]/2)*g.cond$sims[sdd.dat[i]],g.cond$u0.e[sdd.dat[i]],
                           sqrt(1/(g.cond$nu.alpha[sdd.dat[i]]/g.cond$nu.beta[sdd.dat[i]]))),
                     nrow=g.cond$sims[sdd.dat[i]],byrow=T)

    #cbind mat to form total sim dataset
    t.m    <- cbind(m1,m2)

    #Loop over the different CI intervals
    for (j in seq_along(fc.seq)){

      #counter
      cat(paste("i=",i," and j=",j),"\n")

      #Apply sim.bra.alt4 over all rows of the simulated data
      ap.res <- t(apply(t.m,1,function(tm1) sim.app.ex(tm1,unique(g.cond$int.size),g.cond[sdd.dat[i],],fc.vec=fc.seq[j])))

      #Save all results in list
      sim.list[[(i-1)*length(fc.seq)+j]] <- ap.res
      tm.list[[(i-1)*length(fc.seq)+j]]  <- t.m

      #Give ap.res columns names
      colnames(ap.res) <- c(paste0("N.tot_",unique(g.cond$int.size)),
                            paste0("A.N.tot_",unique(g.cond$int.size)),
                            paste0("per.left_",unique(g.cond$int.size)),
                            paste0("A.per.left_",unique(g.cond$int.size)),
                            paste0("suc_",unique(g.cond$int.size)),
                            paste0("fut_",unique(g.cond$int.size)),
                            paste0("scal_",unique(g.cond$int.size)[-1]),
                            paste0("M_",unique(g.cond$int.size)[-1]),
                            paste0("ci.u_",unique(g.cond$int.size)[-1]),
                            paste0("fc.int_",unique(g.cond$int.size)[-1]))


      #Store per.left probabilities in list
      cum.list[[(i-1)*length(fc.seq)+j]] <- ap.res[,7:12]

      #Extract point of re-est ss where xi=0.90
      xi.sim.90 <- matrix(apply(ap.res[,7:12],2,function(x) quantile(x,.90)),ncol=2)

      #Results matrix (only int.size prior sd.data and per.left with and without adap)
      res.mat <- cbind(g.cond[sdd.dat[i]:(sdd.dat[i]+2),c(2,3,4)],rep(fc.seq[j],nrow(xi.sim.90)),xi.sim.90)

      #Bind al res.mats together in col.mat
      col.mat <- rbind(col.mat,res.mat)


      ### BULK ###
      #NTOT AND NLEFT
      #Take mean and quantiles of normal and adaptation with int size as rows
      #m.N.tot    <- t(apply(ap.res[,1:3],2,function(x) c(mean(x),quantile(x,c(0.10,0.90)))))
      #m.N.a.tot  <- t(apply(ap.res[,4:6],2,function(x) c(mean(x),quantile(x,c(0.10,0.90)))))

      #SUCFUT
      #Take mean of sucfut
      #m.sucfut   <- apply(ap.res[,13:18],2,function(x) mean(x))

      #Make matrix with nrow as int.size
      #mat.sucfut <- matrix(c(rep(m.sucfut[1],5),m.sucfut[2:3],rep(m.sucfut[4],5),m.sucfut[5:6]),nrow=length(unique(g.cond$int.size)))
      #mat.sucfut <- cbind(m.sucfut[1:3],m.sucfut[4:6])

      #Store averaged results in sim.res FIX THIS
      #sim.res[ind.j[j]:(ind.j[j]+length(unique(g.cond$int.size))-1),] <- cbind(m.N.tot,m.N.a.tot,mat.sucfut)
      ###


      #Names for cum.list
      names(cum.list)[(i-1)*length(fc.seq)+j] <- paste0("sd.data=",unique(g.cond$sd.data)[i],"_fc.int=",fc.seq[j])
    }
  }

  colnames(col.mat) <- c("int.size","prior","sd.data","f.ci","perleft","Aperleft")

  out.list <- list(cum.list=cum.list,col.mat=col.mat)

  out.list
}


apply_plots4 <- function(sim.res,g.cond,save=T,perleft){

  out.list <- list()

  ## N LEFT PLOTS (WITHOUT AND WITH PP) (THINK OF WAY TO INCLUDE LINE FOR xi=90)

  #Select nleft and scenarios
  res     <- sim.res[,c("Ntot","Ntot.10","Ntot.90")]
  Ares    <- sim.res[,c("Atot","Atot.10","Atot.90")]
  scen    <- g.cond[,2:4]

  #Make total df
  tot.mat.n <- cbind(scen,res)
  Atot.mat.n <- cbind(scen,Ares)

  #make into factors
  tot.mat.n$prior    <- as.factor(tot.mat.n$prior)
  tot.mat.n$sd.data  <- as.factor(tot.mat.n$sd.data)

  Atot.mat.n$prior    <- as.factor(Atot.mat.n$prior)
  Atot.mat.n$sd.data  <- as.factor(Atot.mat.n$sd.data)

  #Labeling of panes (fixed to now include prior label and equal sign)
  levels(tot.mat.n$prior)  <- c(expression("n"[0] == 10), expression("n"[0] == 20))
  levels(Atot.mat.n$prior) <- c(expression("n"[0] == 10), expression("n"[0] == 20))

  #See earlier versions for colour plots code

  nleft.plot.GS <- ggplot(data=tot.mat.n, aes(x=int.size, y=Ntot, group=sd.data,
                                              shape=sd.data,colour=sd.data)) +
    geom_line(aes(linetype=sd.data),size=1.4) +
    geom_point(size=5) +
    geom_ribbon(aes(ymin=Ntot.10,ymax=Ntot.90,linetype=sd.data,colour=sd.data,fill=sd.data),alpha=0.2) +
    facet_grid(~prior,labeller=label_parsed) +
    scale_colour_grey(name=expression(sigma[R]/sigma[0]),start=0.3,end=0.7,guide=guide_legend(keywidth=2.5))+
    scale_fill_grey(name=expression(sigma[R]/sigma[0]),start=0.3,end=0.7,guide=guide_legend(keywidth=2.5))+
    theme_bw() +
    labs(x="Interim timing (total amount of patients)", y="Total required sample size at interim")+
    scale_shape_discrete(name=expression(sigma[R]/sigma[0]))+
    scale_linetype_discrete(name=expression(sigma[R]/sigma[0]))+
    theme(text=element_text(size=20,family="Palatino"),legend.title=element_text(face="bold",size=22),
          legend.title.align=0)

  Anleft.plot.GS <- ggplot(data=Atot.mat.n, aes(x=int.size, y=Atot, group=sd.data,
                                                shape=sd.data,colour=sd.data)) +
    geom_line(aes(linetype=sd.data),size=1.4) +
    geom_point(size=5) +
    geom_ribbon(aes(ymin=Atot.10,ymax=Atot.90,linetype=sd.data,colour=sd.data,fill=sd.data),alpha=0.2) +
    facet_grid(~ prior,labeller=label_parsed) +
    scale_colour_grey(name=expression(sigma[R]/sigma[0]),start=0.3,end=0.7,guide=guide_legend(keywidth=2.5))+
    scale_fill_grey(name=expression(sigma[R]/sigma[0]),start=0.3,end=0.7,guide=guide_legend(keywidth=2.5))+
    theme_bw() +
    labs(x="Interim timing (total amount of patients)", y="Total required sample size at interim")+
    scale_shape_discrete(name=expression(sigma[R]/sigma[0]))+
    scale_linetype_discrete(name=expression(sigma[R]/sigma[0]))+
    theme(text=element_text(size=20,family="Palatino"),legend.title=element_text(face="bold",size=22),
          legend.title.align=0)


  ## XI PLOTS

  scen2     <- g.cond[,2:4]
  tot.mat   <- cbind(id=seq(1,nrow(scen2)),scen2,sim.res)


  # Select eq5 pers
  res2  <- as.vector(abind(lapply(perleft,function(x) t(x)[1:4,]),along=1))
  Ares2 <- as.vector(abind(lapply(perleft,function(x) t(x)[5:8,]),along=1))


  #Repeat matrix as many times as sims
  scen2.rep <- do.call(rbind, replicate(g.cond$sims[1], scen2, simplify=FALSE)) # where m is your matrix

  #make plot df
  out.df  <- cbind(scen2.rep,res2)
  Aout.df <- cbind(scen2.rep,Ares2)

  #Make into factors
  out.df$prior    <- as.factor(out.df$prior)
  out.df$int.size <- as.factor(out.df$int.size)
  out.df$sd.data  <- as.factor(out.df$sd.data)

  Aout.df$prior    <- as.factor(Aout.df$prior)
  Aout.df$int.size <- as.factor(Aout.df$int.size)
  Aout.df$sd.data  <- as.factor(Aout.df$sd.data)

  #Filter out red line
  out.sel  <- out.df[out.df$sd.data!=0.75,]
  Aout.sel <- Aout.df[Aout.df$sd.data!=0.75,]

  #Make labels for facet grid
  levels(out.sel$prior)  <- c(expression("n"[0] == 10), expression("n"[0] == 20))
  levels(Aout.sel$prior) <- c(expression("n"[0] == 10), expression("n"[0] == 20))



  xi.plot.GS <- ggplot(out.sel, aes(x=res2, group=interaction(sd.data,int.size),
                                    colour=sd.data,linetype=int.size))+
    stat_ecdf(size=1.2)+
    scale_linetype_manual(values=c("solid", "dotted","dashed","dotdash"))+
    geom_hline(aes(yintercept=0.90),linetype="dashed")+
    geom_vline(aes(xintercept=1.0),linetype="dashed")+
    theme_bw()+
    xlim(0,2) +
    scale_colour_grey(name=expression(sigma[R]/sigma[0]),start=0.35,end=0.65,guide=guide_legend(keywidth=2.5))+
    theme(text=element_text(size=20,family="Palatino"),
          legend.title=element_text(face="bold",size=22),
          legend.title.align=0)+
    scale_linetype_discrete(name=expression(paste(N[I])),guide=guide_legend(keywidth=2.5))+
    facet_grid(~ prior,labeller=label_parsed) +
    labs(x="Ratio of re-estimated sample size", y=expression(xi[emp]))

  Axi.plot.GS <- ggplot(Aout.sel, aes(x=Ares2, group=interaction(sd.data,int.size),
                                      colour=sd.data,linetype=int.size))+
    stat_ecdf(size=1.2)+
    scale_linetype_manual(values=c("solid", "dotted","dashed","dotdash"))+
    geom_hline(aes(yintercept=0.90),linetype="dashed")+
    geom_vline(aes(xintercept=1.0),linetype="dashed")+
    facet_grid(~ prior,labeller=label_parsed) +
    theme_bw()+
    xlim(0,2) +
    scale_colour_grey(name=expression(sigma[R]/sigma[0]),start=0.35,end=0.65,guide=guide_legend(keywidth=2.5))+
    theme(text=element_text(size=20,family="Palatino"),
          legend.title=element_text(face="bold",size=22),
          legend.title.align=0)+
    scale_linetype_discrete(name=expression(paste(N[I])),guide=guide_legend(keywidth=2.5))+
    labs(x="Ratio of re-estimated sample size", y=expression(xi[emp]))


  if(save==T){
    ggsave(nleft.plot.GS,filename="plot_nleft_GS.pdf",width=19,height=10.2)
    ggsave(Anleft.plot.GS,filename="plot_Anleft_GS.pdf",width=19,height=10.2)
    ggsave(xi.plot.GS,filename="plot_xi_GS.pdf",width=19,height=10.2)
    ggsave(Axi.plot.GS,filename="plot_Axi_GS.pdf",width=19,height=10.2)}

  out.list <- list(nleft.plot,Anleft.plot,xi.plot,Axi.plot)

  out.list
}


plot.example <- function(col.mat,save=T){

  #Make sd.data into factor
  col.mat$sd.data <- as.factor(col.mat$sd.data)

  #Make f.ci into numeric
  col.mat$f.ci <- as.numeric(as.character(col.mat$f.ci))

  #Make int.size into factor
  col.mat$int.size <- as.factor(col.mat$int.size)

  #Labeling of panes (fixed to now include label and equal sign)

  levels(col.mat$int.size) <- c(expression("N"[I] == 0), expression("N"[I] == 20),expression("N"[I] == 50))

  #SEE OLDER VERSIONS OF THIS FUNCTION FOR COLOUR PLOTS!

  ### GRAPHS WITH SDD AS LINES ###

  sd.line.GS <-

    ggplot(data=col.mat,aes(y=perleft,x=f.ci,group=sd.data,colour=sd.data,shape=sd.data))+
    geom_line(data=col.mat,size=1.2) +
    geom_point(size=5) +
    facet_grid(.~ int.size,labeller=label_parsed)+
    theme_bw() +
    labs(x="Width of predictive interval for M",
         y=expression(atop(paste("Proportion of total re-estimated sample size"),
                           paste("such that ",xi[emp]," is at least 0.9")))) +
    scale_colour_discrete(name=expression(sigma[R]/sigma[0]),guide=guide_legend(keywidth=2.5))+
    scale_shape_discrete(name=expression(sigma[R]/sigma[0]),guide=guide_legend(keywidth=2.5))+
    scale_colour_grey(name=expression(sigma[R]/sigma[0]),start=0.2,end=0.8,guide=guide_legend(keywidth=2.5))+
    theme(text=element_text(size=20,family="Palatino"),legend.title=element_text(face="bold",size=22),
          legend.title.align=0)


  A.sd.line.GS <-

    ggplot(data=col.mat,aes(y=Aperleft,x=f.ci,group=sd.data,colour=sd.data,shape=sd.data))+
    geom_line(data=col.mat,size=1.2) +
    geom_point(size=5) +
    facet_grid(~ int.size,labeller=label_parsed)+
    theme_bw() +
    labs(x="Width of predictive interval for M",
         y=expression(atop(paste("Proportion of total re-estimated sample size"),
                           paste("such that ",xi[emp]," is at least 0.9")))) +
    scale_colour_discrete(name=expression(sigma[R]/sigma[0]),guide=guide_legend(keywidth=2.5))+
    scale_shape_discrete(name=expression(sigma[R]/sigma[0]),guide=guide_legend(keywidth=2.5))+
    scale_colour_grey(name=expression(sigma[R]/sigma[0]),start=0.2,end=0.8,guide=guide_legend(keywidth=2.5))+
    theme(text=element_text(size=20,family="Palatino"),legend.title=element_text(face="bold",size=22),
          legend.title.align=0)

  ### GRAPHS WITH CI AS LINES ###

  # Make selection of CIs to reduce amount of lines
  col.mat.sec <- col.mat[col.mat$f.ci==0.05|
                           col.mat$f.ci==0.35|
                           col.mat$f.ci==0.65|
                           col.mat$f.ci==0.95,]
  # Make f.ci into factor
  col.mat.sec$f.ci <- as.factor(col.mat.sec$f.ci)


  #This version adds a label to the linegraph
  cols <- c("LINE1"="#000000")

  CI.line.GS <-
    ggplot(data=col.mat.sec,aes(x=sd.data))+
    geom_line(aes(y=perleft,group=1,colour="LINE1"),size=1.2) +
    geom_point(aes(y=perleft,group=1,colour="LINE1"),size=5) +
    scale_colour_manual(name="PI width",values=cols,guide=guide_legend(keywidth=2.5),
                        labels=expression(infinity))+
    facet_grid(~ int.size,labeller=label_parsed)+
    theme_bw() +
    labs(x=expression(sigma[R]/sigma[0]),
         y=expression(atop(paste("Proportion of total re-estimated sample size"),
                           paste("such that ",xi[emp]," is at least 0.9")))) +
    theme(text=element_text(size=20,family="Palatino"),legend.title=element_text(face="bold",size=16),
          legend.text=element_text(size=16),legend.title.align=0)

  A.CI.line.GS <-
    ggplot(data=col.mat.sec,aes(y=Aperleft,x=sd.data,group=f.ci,colour=f.ci,shape=f.ci))+
    geom_line(data=col.mat.sec,size=1.2) +
    geom_point(size=5) +
    facet_grid(~ int.size,labeller=label_parsed)+
    theme_bw() +
    labs(x=expression(sigma[R]/sigma[0]), y=expression(atop(paste("Proportion of total re-estimated sample size"),
                                                            paste("such that ",xi[emp]," is at least 0.9")))) +
    scale_colour_discrete(name="PI Width",guide=guide_legend(keywidth=2.5))+
    scale_shape_discrete(name="PI Width",guide=guide_legend(keywidth=2.5))+
    scale_colour_grey(name="PI Width",start=0.2,end=0.8,guide=guide_legend(keywidth=2.5))+
    theme(text=element_text(size=20,family="Palatino"),legend.title=element_text(face="bold",size=16),
          legend.title.align=0)


  if(save==T){
    ggsave(sd.line.GS,filename="plot_sdline_GS.pdf",width=19,height=10.2)
    ggsave(A.sd.line.GS,filename="plot_Asdline_GS.pdf",width=19,height=10.2)
    ggsave(CI.line.GS,filename="plot_ciline_GS.pdf",width=19,height=10.2)
    ggsave(A.CI.line.GS,filename="plot_Aciline_GS.pdf",width=19,height=10.2)}

  out.list <- list(sd.line.GS,A.sd.line.GS,CI.line.GS,A.CI.line.GS)

  out.list
}


##########################################################
## Frequentist calculation of sample size               ##
##  R version of Listing 2 in Whitehead et al. (2008)   ##
##                                                      ##
## Input:  standard deviation, type 1 error (alpha)     ##
##         type II error (beta), delta star             ##
## Output: total required sample size for design        ##
##########################################################

freq.white.ss <- function(sd,r=1,alpha=0.025,beta=0.20,d.star=1.5){

  for (m in 2:10000){                                # Counter
    n <- (r+1)*m                                     # Sample size
    k <- (((r+1)^2)*sd^2)/(r*d.star^2)
    rhs <- k*(qt(1-alpha,n-2)+qt(1-beta,n-2))^2      # Right hand side

    # Output first time that n is greater than or equal to rhs
    if (n >= rhs) {
      results               <- n
      break
      break
    }}
  results
}


##########################################################
## Sample size caculation for unknown variance  (eq. 7) ##
## R version of  Listing 4 in Whitehead et al. (2008)   ##
##                                                      ##
## Input:  alpha_0, beta_0, eta, zeta, xi, q_0E, q_0C   ##
##         allocation ratio (r)                         ##
## Output: Sample size required to achieve              ##
##         design characteristics                       ##
##########################################################

ss.calc <- function(a0=100, b0=4, eta=0.95, zeta=0.90,
                    xi=0.80,r=1,q0.e=0,q0.c=0,d.star=0.1) {

  for (m in 1:1000000) {                                     # Counter
    n.calc      <- (r+1)*m                                   # Calculated sample size
    a1.hat      <- a0 + 0.5*n.calc                           # Posterior alpha
    t.eta       <- qt(eta, 2*a1.hat)                         # eta point on t-distribution
    t.zeta      <- qt(zeta, 2*a1.hat)                        # zeta point on t-distribution
    b.xi        <- qbeta(xi, 0.5*n.calc,a0)                  # xi point on beta

    n.calc.e    <- (r/(r+1))*n.calc                          # sample size in group e and c
    n.calc.c    <- (1/(r+1))*n.calc
    D.hat       <- (q0.e+n.calc.e)*(q0.c+n.calc.c)/(q0.e+q0.c+n.calc)  # calculation of D

    lhs         <- D.hat*(a1.hat/b0)*(1-b.xi)                # left hand side of equation
    rhs         <- ((t.eta+t.zeta)/d.star)^2                 # right hand side

    xi.crit     <- 1- (rhs*b0)/(a1.hat*D.hat)                # Corresponding critical value of xi (see eq. 10)
    xi.int      <- pbeta(xi.crit,(n.calc/2),a0)              # Xi for this design (should be at least 0.80 for sample size calculated here)

    #Output only the first row to satisfy the condition
    if (lhs >= rhs) {
      results               <- matrix(c(m,n.calc.e,n.calc.c,n.calc,t.eta,
                                        t.zeta,rhs,xi.crit,
                                        xi.int,a1.hat,D.hat),nrow=1,ncol=11)
      colnames(results)     <- c("Obs", "n.e", "n.c","n","t.eta","t.zeta",
                                 "rhs","xi.crit","xi.int","a1.hat","D")
      break
      break
    }}
  results
}


##########################################################
## Sample size caculation for unknown variance  (eq. 7) ##
## R version of  Listing 4 in Whitehead et al. (2008)   ##
##                                                      ##
## Input:  alpha_0, beta_0, eta, zeta, xi, q_0E, q_0C   ##
##         allocation ratio (r)                         ##
## Output: Sample size required to achieve              ##
##         design characteristics                       ##
##########################################################

ss.calc <- function(a0=100, b0=4, eta=0.95, zeta=0.90,
                    xi=0.80,r=1,q0.e=0,q0.c=0,d.star=0.1) {

  for (m in 1:1000000) {                                     # Counter
    n.calc      <- (r+1)*m                                   # Calculated sample size
    a1.hat      <- a0 + 0.5*n.calc                           # Posterior alpha
    t.eta       <- qt(eta, 2*a1.hat)                         # eta point on t-distribution
    t.zeta      <- qt(zeta, 2*a1.hat)                        # zeta point on t-distribution
    b.xi        <- qbeta(xi, 0.5*n.calc,a0)                  # xi point on beta

    n.calc.e    <- (r/(r+1))*n.calc                          # sample size in group e and c
    n.calc.c    <- (1/(r+1))*n.calc
    D.hat       <- (q0.e+n.calc.e)*(q0.c+n.calc.c)/(q0.e+q0.c+n.calc)  # calculation of D

    lhs         <- D.hat*(a1.hat/b0)*(1-b.xi)                # left hand side of equation
    rhs         <- ((t.eta+t.zeta)/d.star)^2                 # right hand side

    xi.crit     <- 1- (rhs*b0)/(a1.hat*D.hat)                # Corresponding critical value of xi (see eq. 10)
    xi.int      <- pbeta(xi.crit,(n.calc/2),a0)              # Xi for this design (should be at least 0.80 for sample size calculated here)

    #Output only the first row to satisfy the condition
    if (lhs >= rhs) {
      results               <- matrix(c(m,n.calc.e,n.calc.c,n.calc,t.eta,
                                        t.zeta,rhs,xi.crit,
                                        xi.int,a1.hat,D.hat),nrow=1,ncol=11)
      colnames(results)     <- c("Obs", "n.e", "n.c","n","t.eta","t.zeta",
                                 "rhs","xi.crit","xi.int","a1.hat","D")
      break
      break
    }}
  results
}



### PROCEDURE FOR DETERMINING SCALING PARAMETER (GAMMA PDCCPP)
#   depending on observed M and the ppd of M

##Find limits of the prior predictive distribution of M with width set by fc.int (= 1-c/2)
M.ci      <- qf(c(0.5-fc.int/2,0.5+fc.int/2),k*2,2*a1)

##Test if M is within M.ci. If TRUE then it is within M.ci
if(M<M.ci[2] & M>M.ci[1]){

  #No changing of original alpha0 so scale parameter (gamma PDCCPP) = 1
  scal <- 1

  #save upper limit of ci
  ci.u <- M.ci[2]

} else {

  #Test if M is lower than lower limit
  if(M<M.ci[1]){

    #Set scal equal such that second degree of freedom equals 1
    scal <- 1/a1

    #save upper limit of ci
    ci.u <- M.ci[2]

  } else { #Where M is higher than upper limit

    #Use search procedure to determine the scaling parameter (gamma PDCCPP)
    # PRECISION = 0.001
    scal.m  <-seq(0.001,1,0.001)

    #Find corresponding a0 values for each potential scaling value
    a1.m    <- a1*scal.m

    #Find upper limit of CI of ppd of M using these scaled alpha0 values
    ci.m.u  <- qf(0.5+(fc.int/2),k*2,2*a1.m)

    #Find first occurence where observed M is now included in the intervals
    # (determined using a scaled alpha0) Take the -1 value because it finds first time M falls out
    ind.ci  <- which(M>ci.m.u)[1]-1

    #Take found scaling factor and put into scal object
    scal <- scal.m[ind.ci]

    #save upper limit of ci
    ci.u[int.cont-1] <- ci.m.u[ind.ci]
  }
}


## The scale parameter obtained can then be used to downscale the prior information
## if it is in conflict with the variance observed in the collected sample.


