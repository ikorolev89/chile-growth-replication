
#*****************************
#*
#* This file contains self written functions and helper functions and loads libraries.
#* Find functions to compute KPSS-tests in part 1.
#* Find function to estimate the models in part 2.
#* Find functions for plotting the results in part 3.
#* 
#* Note: You can fold all functions in RStudio by pressing Alt + o
#*       You can unfold all function in RStudio by pressing Shift + Alt + o
#* 
#* Note: inputs of every function are described very briefly. 
#* Look at the code of the country-specific R files (e.g. "aus.R") to see how the functions are used .
#*  
#* **************************

#load libraries
library(tseries)
library(dlm)
library(KFAS)
library(mFilter)
library(numDeriv)

#***********************************
#*
#* Part 1: KPSS-tests for contegration and level and trend stationarity
#* 
#* *********************************

# Function to find cointegration relations for the target series in the whole sample
# inputs: 
# - target: the target country to test for cointegration in the whole data sample
# - data: the database, should be a data.frame
# - tstart, tend: time range for cointegration check (based on dim(data)[1] or nrow(data))
# - pval: choose p-value. Output will drop every result with lower p-value than chosen.
# - column, num: based on selected column of output, slected num will be the maximum value allowed. Every value>0 is dropped from output
# - log=TRUE, should be set, because data are in logs
# - substract, TRUE or FALSE. If FALSE, ouput of function is a single data frame which is only based on target, data, tstart, tend, pval and log. 
#                             If TRUE, the output is a list containing the same data frame as if FALSE in position 1 and a second one respecting the inputs column and num in position 2.
cointsearch<-function(target,data,tstart,tend,pval,column,num,substract=TRUE,log=TRUE){
  k=0
  n=0
  a=paste("GDP p.c.",tstart)
  
  ncolumn=dim(data)[2]
  if (log==TRUE){
    for (i in (2:ncolumn)){
      tryCatch({
        if (! NA%in% data[tstart:tend,i,1]){
          a<-kpss.test(target[tstart:tend]-data[tstart:tend,i,1])
          if (a$p.value>=pval) {
            n=n+1
          }
        }
      }, error=function(e){})
    }
    
    Namen<-matrix(0,n,1)
    Level<-matrix(0,n,1)
    p_val<-matrix(0,n,1)
    variance<-matrix(0,n,1)
    GDPpcdiff=matrix(0,n,1)
    GDPpcpre=matrix(0,n,1)
    
    for (i in (2:ncolumn)){
      tryCatch({
        if (! NA%in% data[tstart:tend,i,1]){
          a<-kpss.test(target[tstart:tend]-data[tstart:tend,i,1])
          if (a$p.value>=pval) {
            k=k+1
            Namen[k,1]<- names(data[i])
            Level[k,1]<- a$statistic
            p_val[k,1]<- a$p.value
            variance[k,1]<-var(target[tstart:tend]-data[tstart:tend,i,1])
            GDPpcdiff[k,1]<-mean(exp(target[tstart:tend])-exp(data[tstart:tend,i,1]))
            GDPpcpre[k,1]<-mean(exp(data[tstart:tend,i,1]))
          }
        } 
      }, error=function(e){})
    }
    TempMatrix<-data.frame(c(Namen),c(round(Level,4)),c(round(p_val,4)),c(round(variance,4)),round(GDPpcdiff,0),round(GDPpcpre,0))
    colnames(TempMatrix)<-c("Name","KPSS-Level","P.Value","Variance","Mean GDP p.c. difference pre 1999","Mean GDP p.c. pre 1999")
    Sortmatrix<-TempMatrix[order(TempMatrix$"KPSS-Level"),]
    colnames(Sortmatrix)<-c("Name","KPSS-Level","P.Value","Variance","Mean GDP p.c. difference pre 1999","Mean GDP p.c. pre 1999")
    
    if (substract==FALSE){
      return(Sortmatrix)
    }
    else {
      mat=Sortmatrix
      len<-nrow(mat)
      newmat=mat
      n=nrow(mat)
      for (i in 1:n){
        if (newmat[i,column] > num) {
          newmat[i,1]<-NA
          i=1
        }
      }
      newmat<-newmat[!(newmat[,1] %in% NA),]
      
      return(list(Sortmatrix,newmat))
    }
  }
  
  #if log=FALSE
  else {
    for (i in (2:ncolumn)){
      tryCatch({
        if (! NA%in% data[tstart:tend,i,1]){
          a<-kpss.test(target[tstart:tend]-(data[tstart:tend,i,1]))
          if (a$p.value>=pval) {
            n=n+1
          }
        }
      }, error=function(e){})
    }
    
    Namen<-matrix(0,n,1)
    Level<-matrix(0,n,1)
    p_val<-matrix(0,n,1)
    variance<-matrix(0,n,1)
    GDPpcdiff=matrix(0,n,1)
    GDPpcpre=matrix(0,n,1)
    
    for (i in (2:ncolumn)){
      tryCatch({
        if (! NA%in% data[tstart:tend,i,1]){
          a<-kpss.test(target[tstart:tend]-(data[tstart:tend,i,1]))
          if (a$p.value>=pval) {
            k=k+1
            Namen[k,1]<- names(data[i])
            Level[k,1]<- a$statistic
            p_val[k,1]<- a$p.value
            variance[k,1]<-var(target[tstart:tend]-(data[tstart:tend,i,1]))
            GDPpcdiff[k,1]<-mean(target[tstart:tend]-data[tstart:tend,i,1])
            GDPpcpre[k,1]<-mean(data[tstart:tend,i,1])
          }
        } 
      }, error=function(e){})
    }
    TempMatrix<-data.frame(c(Namen),c(round(Level,4)),c(round(p_val,4)),c(round(variance,4)),round(GDPpcdiff,0),round(GDPpcpre,0))
    colnames(TempMatrix)<-c("Name","KPSS-Level","P.Value","Variance","Mean GDP p.c. difference pre 1999","Mean GDP p.c. pre 1999")
    Sortmatrix<-TempMatrix[order(TempMatrix$"KPSS-Level"),]
    colnames(Sortmatrix)<-c("Name","KPSS-Level","P.Value","Variance","Mean GDP p.c. difference pre 1999","Mean GDP p.c. pre 1999")
    
    if (substract==FALSE){
      return(Sortmatrix)
    }
    else {
      mat=Sortmatrix
      len<-nrow(mat)
      newmat=mat
      n=nrow(mat)
      for (i in 1:n){
        if (newmat[i,column] > num) {
          newmat[i,1]<-NA
          i=1
        }
      }
      newmat<-newmat[!(newmat[,1] %in% NA),]
      
      Sortmatrix<-rbind(Sortmatrix)
      return(list(Sortmatrix,newmat))
      
    }
  }
}

# Function to test for level and trend stationarity using KPSS-tests
# inputs: 
# - y: matrix or time series matrix
# - t.i: time of intervention (based on nrow(y) or dim(y)[1])
# - ynames: column names of y 
LT.kpss.test<-function(y,t.i,ynames){

  out.df<-data.frame(matrix(NA,ncol=2,nrow=NCOL(y)+1))
  colnames(out.df)<-c("Level (with first differences)"	,"Trend")
  out.df[1,1]<-0.347
  out.df[1,2]<-0.119
  
  for(i in 1:NCOL(y)){
   suppressWarnings(out.df[(i+1),1]<-round(kpss.test(diff(y[1:t.i,i],1),"L")$statistic,4))
   suppressWarnings(out.df[(i+1),2]<-round(kpss.test(y[1:t.i,i],"T")$statistic,4))
  }
  
  rownames(out.df)<-c("Crit.value (10%)",ynames)
  
  out.df
}

#***********************************
#*
#* Part 2: State space form, update functions for maximum likelihood estimation
#*         and output and postestimation statistics
#* 
#* *********************************

# Function to put the model in state space form
# inputs:
# - y: time series matrix
# - reg: list having the following sublists as inputs at each position:
#     - var: matrix of regressors
#     - index: to which series index should the matrix of regressors be added 
#     - common: TRUE or FALSE, is the regressor matrix common to all series? If common=TRUE, index is irrelevant
#     - type: one of "obs", "obs98", "level", "slope". "obs98" is only used for time dummies previous to 1998 for the target variable. Makes it easier to separate intervention from outliers in code.
ssf<-function(y,reg){
  n=NCOL(y)
  In<-diag(n)
  Nulln<-matrix(0,n,n)

  
  OnesNullN<-cbind(rep(1,n),rep(0,n))
  Z<-cbind(OnesNullN,diag(n))
  R<-matrix(0,NCOL(Z),NCOL(Z))
  R[1,1]<-1
  Q<-matrix(0,NCOL(Z),NCOL(Z))
  Q[1,1]<-NA
  T=diag(NCOL(Z)) 
  T[1,2]=1
  a1<-rep(0,NCOL(Z))
  P1<-matrix(0,NCOL(Z),NCOL(Z))
  P1inf<-diag(NCOL(Z))
  P1inf[1,1]<-0
  U<-matrix(0,n,n)
  diag(U)<-rep(exp(NA),n)
  n.Ltri<-(n*(n+1))/2-n
  U[upper.tri(U)]<-rep(NA,n.Ltri)
  H<-crossprod(U)

  statenames<-c("c.trend","c.slope",paste0("mu.",colnames(y)))
  
  nobs<-0
  nslope=0
  X<-vector()
  type<-vector()
  
  #regression components
  if(length(reg)>0){
    for (i in 1:length(reg)){
      if (is.null(reg[[i]]$var)==F){
        var<-reg[[i]]$var
        index<-reg[[i]]$index
        type<-reg[[i]]$type
        if(type=="level"){
          index<-NULL
        }
        for (j in 1:NCOL(var)){
          if(is.null(colnames(var)[j])){
            colnames(var)[j]<-j
          }
          if(colnames(var)[j]%in%paste0(1:NCOL(var))){
            colnames(var)[j]<-paste0("reg.i",".",type,".",j,".",index)
          } else{
            colnames(var)[j]<-paste0("reg.",colnames(var)[j],".",type,".",j,".",colnames(y)[index])
          }
        }  
        X<-cbind(X,var)
      }
    }
    
    dimZ<-dim(Z)
    dimT<-dim(T)
    
    Ztemp<-matrix(0,n,dim(X)[2])
    Ttemp<-diag(1,dim(X)[2],dim(X)[2])
    Z<-cbind(Z,Ztemp)
    T<-as.matrix(bdiag(T,Ttemp))
    
    Za<-array(Z,c(dim(Z)[1],dim(Z)[2],nrow(y)))
    Ta<-array(T,c(dim(T)[1],dim(T)[2],nrow(y)))
    
    for (i in 1:length(reg)){
      if(is.null(reg[[i]]$var)==F){
        var<-as.matrix(reg[[i]]$var)
        index<-reg[[i]]$index
        common<-reg[[i]]$common
        type<-reg[[i]]$type
        
        if (common==TRUE){
          Zind=1:n
          Tind<-1
        } else {
          Zind<-index
          Tind<-index
        }
        
        for(xyz in 1:dim(Ta)[3]){
          if(type%in%c("obs","obs98")){
            Za[Zind,(dimZ[2]+1):(dimZ[2]+NCOL(var)),xyz]<-var[xyz,]
          }
          if( type=="slope"){
            indslope<-grep("slope",statenames)
            Ta[indslope[Tind],(dimT[2]+1):(dimT[2]+NCOL(var)),xyz]<-var[xyz,]
          }
          if( type=="level"){
            indlevel<-grep("trend",statenames)
            Ta[indlevel[Tind],(dimT[2]+1):(dimT[2]+NCOL(var)),xyz]<-var[xyz,]
          }
        }
        dimT[2]<-dimT[2]+NCOL(var)
        dimZ[2]<-dimZ[2]+NCOL(var)
      }
    }
    
    RX<-matrix(0,NCOL(X),NCOL(X))
    QX<-matrix(0,NCOL(X),NCOL(X))
    P1X<-matrix(0,NCOL(X),NCOL(X))
    P1infX<-matrix(0,NCOL(X),NCOL(X))
    diag(P1infX)<-1
    a1X<-rep(0,NCOL(X))
    statenamesX<-paste0(colnames(X))
    
    R<-as.matrix(bdiag(R,RX))
    Q<-as.matrix(bdiag(Q,QX))
    P1<-as.matrix(bdiag(P1,P1X))
    P1inf<-as.matrix(bdiag(P1inf,P1infX))
    a1<-c(a1,a1X)
    statenames<-c(statenames,statenamesX)
    Z<-Za
    T<-Ta
    
  }
  
  
  ssf<-SSModel(y~-1+SSMcustom(Z=Z,T=T,R=R,Q=Q,a1=a1,P1=P1,P1inf=P1inf,state_names=statenames)
               ,H=H)

  ssf
  
}

# Update function to estimate the model using maximum likelihood
# User does not have to use this function. It is part of function est.fn
fn<-function(pars,model,estimate=TRUE,y){	
  
  n<-NCOL(y)
  In<-diag(n)
  n.Ltri<-(n*(n+1))/2-n
  
  tH<-matrix(0,n,n)
  diag(tH)<-exp(pars[1:n])
  nn<-n+1
  tH[lower.tri(tH)]<-pars[nn:(nn+n.Ltri-1)]
  model["H"][,,1]<-crossprod(tH)
  
  nn<-nn+n.Ltri
  model["Q"][1,1,]<-exp(pars[nn])

  if (estimate==TRUE){
    -logLik(model)
  } else model
  
}

# Update function to estimate the model treating regression parameters as fixed and known 
# (so their parameters are estimated using ML and not by KF)
# User does not have to use this function. It is part of function est.fn
fn.regML<-function(pars,model,estimate=TRUE,y){	
  
  n<-NCOL(y)#2
  In<-diag(n)
  n.Ltri<-(n*(n+1))/2-n
  
  tH<-matrix(0,n,n)
  diag(tH)<-exp(pars[1:n])
  nn<-n+1
  tH[lower.tri(tH)]<-pars[nn:(nn+n.Ltri-1)]
  model["H"][,,1]<-crossprod(tH)
  
  nn<-nn+n.Ltri
  model["Q"][1,1,]<-exp(pars[nn])
  
  nn<-nn+1
  
  #regression components and mu
  place.reg<-grep("reg.",colnames(model$Z[,,1]))
  lpr<-length(place.reg)
  model$a1[place.reg]<-pars[nn:(nn+lpr-1)]
  diag(model$P1)[place.reg]<-0
  diag(model$P1inf)[place.reg]<-0
  
  if (estimate==TRUE){
    -logLik(model)
  } else model
  
}

# Function to estimate the models 
# inputs:
# - y: time series matrix
# - reg: the same list as described for function ssf()
# - time.intervention: date of intervention, i.e. the year of intervention
est.fn<-function(y,reg,time.intervention){
  
  # initialise H by variances of first differences
  # initialise Q by n times the mean of variances of first differences
  yy1<-diff(y,differences=1)
  n<-NCOL(y)
  
  init<-rep(0,50)
  n.Ltri<-(n*(n+1))/2-n
  
  # init H, offdiag=0
  init[1:n]<-log(diag(chol(var(yy1,na.rm=TRUE))));nH<-1:n
  # init Q
  nn<-n+n.Ltri+1
  init[nn]<-log(NCOL(y)*mean(diag(var(yy1,na.rm=TRUE))));nQ=nn
  
  # create state space form of the model
  ssf<-ssf(y,reg=reg)
  
  # estimate the model using update funcion fn. Regression parameters estimated by Kalman Smoother
  print("ML: regression components included in state vector") 
  opt.fn<-optim(f=fn,p=init,model=ssf,y=y,method="BFGS",control=list(trace=10,maxit=500))
  mod.opt.fn<-fn(opt.fn$p,ssf,estimate=FALSE,y=y)
  out.fn<-KFS(mod.opt.fn,smoothing=c("state","signal","mean","disturbance"),filterin=c("state","signal","mean"))
  p1<-opt.fn$par
  p1<-p1[which(p1!=0)]
  
  #*************
  # to compute GLS-residuals use update function fn.regML. Use estimated parameters from estimated model
  # this model will also be used to compute aic
  place.reg<-grep("reg.",colnames(ssf$Z[,,1]))
  lpr<-length(place.reg)
  
  ssf$a1[place.reg]<-out.fn$alphahat[1,place.reg]
  diag(ssf$P1)[place.reg]<-0
  diag(ssf$P1inf)[place.reg]<-0
  
  # no "real" estimation here, just take estimated parameters and put them in the model
  print("ML: regression coefficients treated as fixed and known")
  p1<-c(p1,out.fn$alphahat[1,place.reg])
  opt.fn.ML<-optim(f=fn.regML,p=p1,model=ssf,y=y,method="BFGS",control=list(trace=10,maxit=0))
  mod.opt.fn.ML<-fn(opt.fn.ML$p,ssf,estimate=FALSE,y=y)
  out.fn.ML<-KFS(mod.opt.fn.ML,smoothing=c("state","signal","mean","disturbance"),filterin=c("state","signal","mean"))
  p1<-opt.fn.ML$par
  print(opt.fn.ML$value)
  # finished model to compute GLS residuals
  #****************
  
  #*************##
  # Weights
  #*************##
  
  SIGMA_eps<-out.fn$model$H
  sigma_eps<-c(SIGMA_eps[2:n,1,1])
  SIGMA_eps<-out.fn$model["H"][2:n,2:n,1]
  beta<-solve(SIGMA_eps)%*%sigma_eps
  ii<-matrix(c(rep(1,(n-1))),ncol=1)
  linker_summand<-(1/(t(ii)%*%solve(SIGMA_eps)%*%ii))%*%t(ii)%*%solve(SIGMA_eps)
  prod<-(1/(t(ii)%*%solve(SIGMA_eps)%*%ii))[1,1]
  rechter_summand<-t(beta)%*%(diag(n-1)-prod*ii%*%t(ii)%*%solve(SIGMA_eps))
  weights<- linker_summand+rechter_summand
  ibeta<-t(ii)%*%beta
  colnames(weights)<-colnames(y)[-1]
  
  #***************
  # Statistical output
  #***************
  
  # t-statistic und p-value
  Tt<-nrow(y)
  w2=length(out.fn$model$Z[1,,1])-n # number of parameters in state vector and regression parameters
  tstat<-out.fn$alphahat[length(out.fn$alphahat[,1]),]/sqrt(diag(out.fn$V[,,length(out.fn$alphahat[,1])]))
  smooth_estimates=round(out.fn$alphahat[length(out.fn$alphahat[,1]),],5)
  sd=round(sqrt(diag(out.fn$V[,,length(out.fn$alphahat[,1])])),5)
  tstat=round(tstat,5)
  
  # p-values need degree of freedoms, which are added to the output here
  DF<-matrix(NA,NCOL(ssf$Z[,,1]),1)
  nc<-length(which(is.na(y)==FALSE))
  place.par.com<-grep("trend|slope|level",colnames(ssf$Z))
  DF[place.par.com,1]<-nc-length(place.par.com)
  for (i in 1:NCOL(y)){
    nyi<-length(which(is.na(y[,i])==FALSE))
    place.par.i<-grep(colnames(y)[i],colnames(ssf$Z))
    DF[place.par.i,1]<-nyi-length(place.par.i)
  }
  pval.one.side=round(1-pt(abs(tstat),DF),5)
  pval.two.side=round(2*(1-pt(abs(tstat),DF)),5)
  
  stat.output<-cbind(smooth_estimates,sd,tstat,pval.one.side,pval.two.side,DF)
  colnames(stat.output)[6]<-"DF"
  
  # aic based on model out.fn.ML, i.e. regression parameters are treated as fixed and known
  Tt<-nrow(y)
  w1=length(which(opt.fn.ML$par!=0)) # number of parameters to estimate for variance-covariance-matrix + number of regression parameters
  d=2
  # aic based on Durbin & Koopman 2012, S. 188, and Harvey 1989 S. 270
  # optim in R performs minimzation of logL
  # aic for hyperparameters and phi-parametervector with parameters corresponding to Z,T,H,R
  aicDK<-1/Tt*(-2*log(-opt.fn.ML$value)+2*(d+w1))# parameters in w2 are not part of ML estimation
  aic<-aicDK
  
  #*************
  #Synthetic control & common trend
  #*************
  
  ny1.98<-grep("obs98",colnames(out.fn$model$Z))
  ny1<-colnames(out.fn$model$Z)[which(grepl(colnames(y)[1],colnames(out.fn$model$Z))&grepl("obs",colnames(out.fn$model$Z)))]
  Zy1<-t(out.fn$model$Z[1,,]);class(Zy1)<-"matrix";colnames(Zy1)<-colnames(out.fn$model$Z)
  mu<-out.fn$alphahat[,3:(NCOL(y)+2)]
  mum1<-mu[,-1]
  y.m<-y
  if(dim(out.fn$model$Z)[3]>1){ #if dim(out.fn$model$Z)[3]>1, then there are regressors
    if(dim(out.fn$alphahat)[2]-(2+NCOL(y))>1){
      for (t in 1:dim(out.fn$model$Z)[3]){
        y.m[t,]<-y[t,]-out.fn$model$Z[,-c(1:(2+NCOL(y))),t]%*%out.fn$alphahat[t,-c(1:(2+NCOL(y)))]
      }
    }else{
      for (t in 1:dim(out.fn$model$Z)[3]){
        y.m[t,]<-y[t,]-out.fn$model$Z[,-c(1:(2+NCOL(y))),t,drop=F]%*%out.fn$alphahat[t,-c(1:(2+NCOL(y))),drop=F]
      }
    }
  }
  y.m<-y.m[,-1]
  
  eps1<-t(t(beta)%*%t(y.m-mum1- t(ii%*%t(out.fn$alphahat[,1]))))
  
  if(length(ny1.98)>0){
    if(NCOL(out.fn$alphahat[,ny1.98]*Zy1[,ny1.98])>1){
      syn.y1<-out.fn$alphahat[,1]+out.fn$alphahat[,3]+rowSums(out.fn$alphahat[,ny1.98]*Zy1[,ny1.98])+eps1
    } else{
      syn.y1<-out.fn$alphahat[,1]+out.fn$alphahat[,3]+out.fn$alphahat[,ny1.98]*Zy1[,ny1.98]+eps1
      
    }
  }else{
    syn.y1<-out.fn$alphahat[,1]+out.fn$alphahat[,3]+eps1
  }
  
  common.trend<-out.fn$alphahat[,1]+out.fn$alphahat[,3]
  
  #*************
  # Residuals
  #*************
  
  # get GLS-residuals from model out.fn.ML (regression parameters treated as fixed and known)
  res<-residuals(out.fn.ML,"recursive")
  rest<-rstandard(out.fn.ML,"recursive","marginal")
  restc<-rstandard(out.fn.ML,"recursive","cholesky")
  rest.state<-rstandard(out.fn.ML,"state",zerotol=0)
  suppressWarnings(irreg<-rstandard(out.fn.ML,"pearson"))
  
  if (sum(y[-c(1:time.intervention),1],na.rm=TRUE)>0){
    
    #*************
    # goodnes of fit
    #*************#
    
    # seeHarvey (1989, S. 268, S. 388ff.)
    vv<-res
    d=2
    n.par<-NCOL(Zy1[,ny1])
    Tt<-nrow(y)
    k=n.par
    v11<-vv[,1]
    sigmaf2<-(Tt-d)^-1*sum(v11[(d+1):Tt]^2)#~s2 (5.5.3)

    SSE<-(Tt-d)*sigmaf2
    s2<-((Tt-d)/(Tt-d-k))*sigmaf2
    dy<-diff(out.fn$model$y[,1],1)
    meandy<-mean(dy)
    o<-s2
    m<-sum((dy-meandy)^2)
    u<-(Tt-2)
    mu<-m/u
    RD2.target<-1-SSE/m
    RD2.adj.target<-1-s2/mu
    
    #***************#
    # residual analysis
    #***************#
    n.param=2
    
    #Hh-Test
    d=n.param #diffuse phase
    n<-Tt

    h<-round((n-d)/3)
    a<-vv[(n-h+1):n]
    b<-vv[(d+1):(d+h)]
    numerator_sum<-sum(a^2)
    denominator_sum<-sum(b^2)
    Hh<-numerator_sum/denominator_sum
    I_Hh<-1/Hh
    
    F0.005<-qf(0.01/2,h,h)
    F0.025<-qf(0.05/2,h,h)
    F0.05<-qf(0.1/2,h,h)
    F0.95<-qf(1-0.1/2,h,h)
    F0.975<-qf(1-0.05/2,h,h)
    F0.995<-qf(1-0.01/2,h,h)
    p_Hh_F<-(1-pf(Hh,h,h))*2
    p_I_Hh_F<-(1-pf(I_Hh,h,h))*2
    
    if (Hh<=1) {
      Hh_out<-matrix(c(I_Hh,F0.95,F0.975,F0.995,h,p_I_Hh_F), ncol=6,nrow=1)
      colnames(Hh_out)<-c("1/Hh","F0.95","F0.975","F0.995","h","p(1/Hh<F)")
    }
    if (Hh>1){
      Hh_out<-matrix(c(Hh,F0.95,F0.975,F0.995,h,p_Hh_F), ncol=6,nrow=1)
      colnames(Hh_out)<-c("Hh","F0.95","F0.975","F0.995","h","p(Hh<F)")
    }
    
    hh<-Hh_out
    box5<-Box.test(vv[(n.param+1):Tt,1],t="L",lag=5,fitdf=1)#round(log(T))= Anzahl der lag Länge# compare with Chi^2(4-2+1) (zwei Variance PArameter für diese Residualseries)
    box10<-Box.test(vv[(n.param+1):Tt,1],t="L",lag=10,fitdf=1)#round(log(T))= Anzahl der lag Länge# compare with Chi^2(4-2+1) (zwei Variance PArameter für diese Residualseries)
    jb<-jarque.bera.test(vv[(n.param+1):Tt,1])
  } else{
    hh<-paste0("not computed due to missing values in y[,1] after ",time(y)[(time.intervention)])
    box5<-paste0("not computed due to missing values in y[,1] after ",time(y)[(time.intervention)])
    box10<-paste0("not computed due to missing values in y[,1] after ",time(y)[(time.intervention)])
    jb<-paste0("not computed due to missing values in y[,1] after ",time(y)[(time.intervention)])
  }
  
  out.cholR<-list(ML=opt.fn,ML.ML=opt.fn.ML,KF.ML=out.fn.ML,KF=out.fn,model=mod.opt.fn,model.ML=mod.opt.fn.ML,stat.output=stat.output,syn.y1=syn.y1,common.trend=common.trend,rest=rest,rest.state=rest.state,irreg=irreg,restl=restc,res=res,weights=weights,aic=aicDK,H_test=hh,box5=box5,box10=box10,jb=jb)
  out.cholR
  
}

# Function to evaluate output statistics of an estimated model
# inputs:
# - output.model: stored output of est.fn()
# - y: time series matrix, same y as that of est.fn()
# - export.xlsx: should output be saved as xlsx-file? TRUE or FALSE,
# - path: path to save the output, only used if export.xlsx=TRUE
model.stats<-function(output.model,y,export.xlsx=F,path=NULL){
  
  if(class(output.model$box5)=="htest"){
    box<-rbind(c(output.model$box5$statistic,output.model$box5$parameter,output.model$box5$p.value,5),
               c(output.model$box10$statistic,output.model$box10$parameter,output.model$box10$p.value,10))
    colnames(box)<-c("X^2","df","p.value","lags")
    rownames(box)<-c("box5","box10")
    jb<-as.matrix(t(c(output.model$jb$statistic,output.model$jb$parameter,output.model$jb$p.value)))
    colnames(jb)<-c("X^2","df","p.value")
  }else{
    box<-output.model$box5
    jb<-box
  }
  
  gap.round<-ts(round(as.matrix(y[,1]-output.model$syn.y1),4),start=start(output.model$KF$model$y),f=1);rownames(gap.round)<-time(output.model$KF$model$y)
  colnames(gap.round)<-"gap"
  
  gap<-ts(as.matrix(y[,1]-output.model$syn.y1),start=start(output.model$KF$model$y),f=1);rownames(gap)<-time(output.model$KF$model$y)
  colnames(gap)<-"gap"
  
  aic<-as.matrix(t(c(output.model$aic,output.model$ML.ML$value)))
  colnames(aic)<-c("aic","-ML")
  
  out<-list(reg=output.model$stat.output,
            H_test=output.model$H_test,
            box.test=box,
            jb=jb,
            aic=aic,
            weights=output.model$weights,
            gap=gap,
            gap.round=gap.round
  )
  
  if(export.xlsx){
    if(is.null(path)){
      path<-paste0(paste0(ynames[1],paste0(ymd(today()),"_",hour(now()),"-",minute(now()),"-",round(second(now()),0))),"_R_export.xlsx")
    }
    out.exp<-out
    out.exp$reg<-round(out.exp$reg,4)
    out.exp$H_test<-round(out.exp$H_test,4)
    out.exp$box.test<-round(out.exp$box.test,4)
    out.exp$jb<-round(out.exp$jb,4)
    out.exp$aic<-round(out.exp$aic,4)
    out.exp$weights<-round(out.exp$weights,4)
    export(out.exp,file=path,row.names=TRUE,overwrite=TRUE)
  }
  out
  
}

#***********************************
#*
#* Part 3: Helper functions to plot specific output results 
#* 
#* *********************************

# Helper function to create legend
# Mainly the original R function, slightly adjusted to be able to create a border around a shaded area in legend
# User does not have to use this function. It is part of plotting functions
legend.v2 <- function (x, y = NULL, legend, fill = NULL, col = par("col"), 
                       border = "black", lty, lwd, pch, angle = 45, density = NULL, 
                       bty = "o", bg = par("bg"), box.lwd = par("lwd"), box.lty = par("lty"), 
                       box.col = par("fg"), pt.bg = NA, cex = 1, pt.cex = cex, pt.lwd = lwd, 
                       xjust = 0, yjust = 1, x.intersp = 1, y.intersp = 1, adj = c(0, 
                                                                                   0.5), text.width = NULL, text.col = par("col"), merge = do.lines && 
                         has.pch, trace = FALSE, plot = TRUE, ncol = 1, horiz = FALSE, 
                       title = NULL, inset = 0, xpd, title.col = text.col, title.adj = 0.5, 
                       seg.len = 2,xmfill=1,ymfill=1) 
{
  if (missing(legend) && !missing(y) && (is.character(y) || 
                                         is.expression(y))) {
    legend <- y
    y <- NULL
  }
  mfill <- !missing(fill) || !missing(density)
  if (!missing(xpd)) {
    op <- par("xpd")
    on.exit(par(xpd = op))
    par(xpd = xpd)
  }
  title <- as.graphicsAnnot(title)
  if (length(title) > 1) 
    stop("invalid title")
  legend <- as.graphicsAnnot(legend)
  n.leg <- if (is.call(legend)) 
    1
  else length(legend)
  if (n.leg == 0) 
    stop("'legend' is of length 0")
  auto <- if (is.character(x)) 
    match.arg(x, c("bottomright", "bottom", "bottomleft", 
                   "left", "topleft", "top", "topright", "right", "center"))
  else NA
  if (is.na(auto)) {
    xy <- xy.coords(x, y)
    x <- xy$x
    y <- xy$y
    nx <- length(x)
    if (nx < 1 || nx > 2) 
      stop("invalid coordinate lengths")
  }
  else nx <- 0
  xlog <- par("xlog")
  ylog <- par("ylog")
  rect2 <- function(left, top, dx, dy, density = NULL, angle, 
                    ...) {
    r <- left + dx
    if (xlog) {
      left <- 10^left
      r <- 10^r
    }
    b <- top - dy
    if (ylog) {
      top <- 10^top
      b <- 10^b
    }
    rect(left, top, r, b, angle = angle, density = density, 
         ...)
  }
  segments2 <- function(x1, y1, dx, dy, ...) {
    x2 <- x1 + dx
    if (xlog) {
      x1 <- 10^x1
      x2 <- 10^x2
    }
    y2 <- y1 + dy
    if (ylog) {
      y1 <- 10^y1
      y2 <- 10^y2
    }
    segments(x1, y1, x2, y2, ...)
  }
  points2 <- function(x, y, ...) {
    if (xlog) 
      x <- 10^x
    if (ylog) 
      y <- 10^y
    points(x, y, ...)
  }
  text2 <- function(x, y, ...) {
    if (xlog) 
      x <- 10^x
    if (ylog) 
      y <- 10^y
    text(x, y, ...)
  }
  if (trace) 
    catn <- function(...) do.call("cat", c(lapply(list(...), 
                                                  formatC), list("\n")))
  cin <- par("cin")
  Cex <- cex * par("cex")
  if (is.null(text.width)) 
    text.width <- max(abs(strwidth(legend, units = "user", 
                                   cex = cex)))
  else if (!is.numeric(text.width) || text.width < 0) 
    stop("'text.width' must be numeric, >= 0")
  xc <- Cex * xinch(cin[1L], warn.log = FALSE)
  yc <- Cex * yinch(cin[2L], warn.log = FALSE)
  if (xc < 0) 
    text.width <- -text.width
  xchar <- xc
  xextra <- 0
  yextra <- yc * (y.intersp - 1)
  ymax <- yc * max(1, strheight(legend, units = "user", cex = cex)/yc)
  ychar <- yextra + ymax
  if (trace) 
    catn("  xchar=", xchar, "; (yextra,ychar)=", c(yextra, 
                                                   ychar))
  if (mfill) {
    xbox <- xc * 0.7
    ybox <- yc * 0.5
    dx.fill <- xbox
  }
  do.lines <- (!missing(lty) && (is.character(lty) || any(lty > 
                                                            0))) || !missing(lwd)
  n.legpercol <- if (horiz) {
    if (ncol != 1) 
      warning("horizontal specification overrides: Number of columns := ", 
              n.leg)
    ncol <- n.leg
    1
  }
  else ceiling(n.leg/ncol)
  has.pch <- !missing(pch) && length(pch) > 0
  if (do.lines) {
    x.off <- if (merge) 
      -0.7
    else 0
  }
  else if (merge) 
    warning("'merge = TRUE' has no effect when no line segments are drawn")
  if (has.pch) {
    if (is.character(pch) && !is.na(pch[1L]) && nchar(pch[1L], 
                                                      type = "c") > 1) {
      if (length(pch) > 1) 
        warning("not using pch[2..] since pch[1L] has multiple chars")
      np <- nchar(pch[1L], type = "c")
      pch <- substr(rep.int(pch[1L], np), 1L:np, 1L:np)
    }
  }
  if (is.na(auto)) {
    if (xlog) 
      x <- log10(x)
    if (ylog) 
      y <- log10(y)
  }
  if (nx == 2) {
    x <- sort(x)
    y <- sort(y)
    left <- x[1L]
    top <- y[2L]
    w <- diff(x)
    h <- diff(y)
    w0 <- w/ncol
    x <- mean(x)
    y <- mean(y)
    if (missing(xjust)) 
      xjust <- 0.5
    if (missing(yjust)) 
      yjust <- 0.5
  }
  else {
    h <- (n.legpercol + (!is.null(title))) * ychar + yc
    w0 <- text.width + (x.intersp + 1) * xchar
    if (mfill) 
      w0 <- w0 + dx.fill
    if (do.lines) 
      w0 <- w0 + (seg.len + +x.off) * xchar
    w <- ncol * w0 + 0.5 * xchar
    if (!is.null(title) && (abs(tw <- strwidth(title, units = "user", 
                                               cex = cex) + 0.5 * xchar)) > abs(w)) {
      xextra <- (tw - w)/2
      w <- tw
    }
    if (is.na(auto)) {
      left <- x - xjust * w
      top <- y + (1 - yjust) * h
    }
    else {
      usr <- par("usr")
      inset <- rep(inset, length.out = 2)
      insetx <- inset[1L] * (usr[2L] - usr[1L])
      left <- switch(auto, bottomright = , topright = , 
                     right = usr[2L] - w - insetx, bottomleft = , 
                     left = , topleft = usr[1L] + insetx, bottom = , 
                     top = , center = (usr[1L] + usr[2L] - w)/2)
      insety <- inset[2L] * (usr[4L] - usr[3L])
      top <- switch(auto, bottomright = , bottom = , bottomleft = usr[3L] + 
                      h + insety, topleft = , top = , topright = usr[4L] - 
                      insety, left = , right = , center = (usr[3L] + 
                                                             usr[4L] + h)/2)
    }
  }
  if (plot && bty != "n") {
    if (trace) 
      catn("  rect2(", left, ",", top, ", w=", w, ", h=", 
           h, ", ...)", sep = "")
    rect2(left, top, dx = w, dy = h, col = bg, density = NULL, 
          lwd = box.lwd, lty = box.lty, border = box.col)
  }
  xt <- left + xchar + xextra + (w0 * rep.int(0:(ncol - 1), 
                                              rep.int(n.legpercol, ncol)))[1L:n.leg]
  yt <- top - 0.5 * yextra - ymax - (rep.int(1L:n.legpercol, 
                                             ncol)[1L:n.leg] - 1 + (!is.null(title))) * ychar
  if (mfill) {
    if (plot) {
      fill <- rep(fill, length.out = n.leg)
      rect2(left = xt, top = yt + ybox/2, dx = seg.len*(xchar), dy = ybox*ymfill, 
            col = fill, density = density, angle = angle, 
            border = border)
    }
    xt <- xt + dx.fill
  }
  if (plot && (has.pch || do.lines)) 
    col <- rep(col, length.out = n.leg)
  if (missing(lwd)) 
    lwd <- par("lwd")
  if (do.lines) {
    if (missing(lty)) 
      lty <- 1
    lty <- rep(lty, length.out = n.leg)
    lwd <- rep(lwd, length.out = n.leg)
    ok.l <- !is.na(lty) & (is.character(lty) | lty > 0)
    if (trace) 
      catn("  segments2(", xt[ok.l] + x.off * xchar, ",", 
           yt[ok.l], ", dx=", seg.len * xchar, ", dy=0, ...)")
    if (plot) 
      segments2(xt[ok.l] + x.off * xchar, yt[ok.l], dx = seg.len * 
                  xchar, dy = 0, lty = lty[ok.l], lwd = lwd[ok.l], 
                col = col[ok.l])
    xt <- xt + (seg.len + x.off) * xchar
  }
  if (has.pch) {
    pch <- rep(pch, length.out = n.leg)
    pt.bg <- rep(pt.bg, length.out = n.leg)
    pt.cex <- rep(pt.cex, length.out = n.leg)
    pt.lwd <- rep(pt.lwd, length.out = n.leg)
    ok <- !is.na(pch) & (is.character(pch) | pch >= 0)
    x1 <- (if (merge && do.lines) 
      xt - (seg.len/2) * xchar
      else xt)[ok]
    y1 <- yt[ok]
    if (trace) 
      catn("  points2(", x1, ",", y1, ", pch=", pch[ok], 
           ", ...)")
    if (plot) 
      points2(x1, y1, pch = pch[ok], col = col[ok], cex = pt.cex[ok], 
              bg = pt.bg[ok], lwd = pt.lwd[ok])
  }
  xt <- xt + x.intersp * xchar
  if (plot) {
    if (!is.null(title)) 
      text2(left + w * title.adj, top - ymax, labels = title, 
            adj = c(title.adj, 0), cex = cex, col = title.col)
    text2(xt, yt, labels = legend, adj = adj, cex = cex, 
          col = text.col)
  }
  invisible(list(rect = list(w = w, h = h, left = left, top = top), 
                 text = list(x = xt, y = yt)))
}

# Function to plot original variables, mean adjusted
# inputs:
# - y: time series matrix
# - colour: colours for each series in plot
# - t.i: time of intervention (based on nrow(y) or dim(y)[1])
# - vline: draw vertical lines in plot at given time points
# - ynames: names of each series in y
# - png.out, TRUE or FALSE, should the graph be saved as png-File
# - png.name: path or name of png if png.out=T
plot.orig.ma<-function(y,colour,t.i,vline=NULL,ynames,png.out=F,png.name=NULL){
  
  min.plot<-min(y[,1],na.rm=T)
  for (i in 2:ncol(y)){
    min.plot<-min(min.plot,y[,i]-mean(y[1:t.i,i]-y[1:t.i,1],na.rm=T),na.rm=T)
  }
  min.plot
  
  max.plot<-max(y[,1],na.rm=T)
  for (i in 2:ncol(y)){
    max.plot<-max(max.plot,y[,i]-mean(y[1:t.i,i]-y[1:t.i,1],na.rm=T),na.rm=T)
  }
  max.plot
  
  plot(y[,1],type="l",lwd=2,ylim=c(min.plot,max.plot),
       main=paste0(ynames[1][1]," and controls"),
       ylab="Log real GDP p.c.")
  for (i in 2:ncol(y)){
    lines(y[,i]-mean(y[1:t.i,i]-y[1:t.i,1]),col=colour[i-1])
  }
  abline(v=vline,lwd=2)
  
  control.names<-ynames[-1]
  l.width<-max(strwidth(c(paste0(ynames)),cex=0.8))
  
  legend("bottomright",inset=c(0.01,0.1),#x=c(1997,2017.5),y=c(10.4,10.1),
         legend=c(ynames[1],paste0(control.names)),
         col=c("black",colour[1:(ncol(y)-1)]),
         lty=1,
         y.intersp=1,
         x.intersp=1,
         lwd=2,
         cex=0.8,
         text.width=l.width,
         seg.len=4,
  )
  
  if(png.out==T){
    if(is.null(png.name)){
      png.name<-paste0(ynames[1],paste0(ymd(today()),"_",hour(now()),"-",minute(now()),"-",round(second(now()),0)))
    }
    png(filename=paste0(png.name,".png"),units="in",width=7,height=4.5,res=1200,pointsize=12)
    plot(y[,1],type="l",lwd=2,ylim=c(min.plot,max.plot),
         main=paste0(ynames[1][1]," and controls"),
         ylab="Log real GDP p.c.")
    for (i in 2:ncol(y)){
      lines(y[,i]-mean(y[1:t.i,i]-y[1:t.i,1]),col=colour[i-1])
    }
    abline(v=vline,lwd=2)
    
    control.names<-ynames[-1]
    l.width<-max(strwidth(c(paste0(ynames)),cex=0.8))
    
    legend("bottomright",inset=c(0.01,0.1),#x=c(1997,2017.5),y=c(10.4,10.1),
           legend=c(ynames[1],paste0(control.names)),
           col=c("black",colour[1:(ncol(y)-1)]),
           lty=1,
           y.intersp=1,
           x.intersp=1,
           lwd=2,
           cex=0.8,
           text.width=l.width,
           seg.len=4,
    )
    dev.off()
  }
  
}

# Function to plot variables in first differences, mean adjsuted
# inputs: same inputs as for function plot.orig.ma()
plot.diff.ma<-function(y,colour,t.i,vline=NULL,ynames,png.out=F,png.name=NULL){
  plot(y[,2]-y[,1]-mean(y[1:t.i,2]-y[1:t.i,1]),
       main=paste0("Differences of ",ynames[1], " and controls"),
       col=colour[1],type="l",ylim=c(-0.3,0.3),lwd=1,ylab="")
  for (i in 3:ncol(y)){
    lines(y[,i]-y[,1]-mean(y[1:t.i,i]-y[1:t.i,1]),col=colour[i-1])
  }
  abline(v=vline,lwd=2)
  
  control.names<-ynames[-1]
  l.width<-max(strwidth(c(paste0(control.names)),cex=0.8))
  legend("topright",inset=c(0.01,0.01),#x=c(1997,2017.5),y=c(10.4,10.1),
         legend=c(paste0(ynames[-1])),
         col=c(colour[1:(ncol(y)-1)]),
         lty=1,
         y.intersp=1,
         x.intersp=1,
         lwd=2,
         cex=0.8,
         text.width=l.width,
         seg.len=4,
  )
  
  if(png.out==T){
    if(is.null(png.name)){
      png.name<-paste0(ynames[1],paste0(ymd(today()),"_",hour(now()),"-",minute(now()),"-",round(second(now()),0)))
    }
    png(filename=paste0(png.name,".png"),units="in",width=7,height=4.5,res=1200,pointsize=12)
    plot(y[,2]-y[,1]-mean(y[1:t.i,2]-y[1:t.i,1]),
         main=paste0("Differences of ",ynames[1], " and controls"),
         col=colour[1],type="l",ylim=c(-0.3,0.3),lwd=1,ylab="")
    for (i in 3:ncol(y)){
      lines(y[,i]-y[,1]-mean(y[1:t.i,i]-y[1:t.i,1]),col=colour[i-1])
    }
    abline(v=vline,lwd=2)
    
    control.names<-ynames[-1]
    
    control.names<-ynames[-1]
    l.width<-max(strwidth(c(paste0(control.names)),cex=0.8))
    legend("topright",inset=c(0.01,0.01),#x=c(1997,2017.5),y=c(10.4,10.1),
           legend=c(paste0(ynames[-1])),
           col=c(colour[1:(ncol(y)-1)]),
           lty=1,
           y.intersp=1,
           x.intersp=1,
           lwd=2,
           cex=0.8,
           text.width=l.width,
           seg.len=4,
    )
    dev.off()
  }
  
}

# Function to plot the visual output of an estimated model
# inputs: same inputs as for function plot.orig.ma() and
# - output.model: stored output of est.fn() 
# - plot.type: one of "c.trend" or "pred.target". If "c.trend", then graph contains the common trend with confidence interval,
#                                                 If "pred.target" then graph contains the specific target prediction
#                                                 See the small differences of these settings by comparing Figure 2 and Figure 4. 
plot.model.output<-function(output.model,colour,plot.type="c.trend",vline=NULL,ynames,png.out=F,png.name=NULL){
  par(mfrow=c(1,1),mai=c(0.8,0.8,0.5,0.2))
  
  min.plot<-min(y[,1],na.rm=T)
  for (i in 2:ncol(y)){
    min.plot<-min(min.plot,y[,i]-mean(y[1:t.i,i]-y[1:t.i,1],na.rm=T),na.rm=T)
  }
  min.plot
  
  max.plot<-max(y[,1],na.rm=T)
  for (i in 2:ncol(y)){
    max.plot<-max(max.plot,y[,i]-mean(y[1:t.i,i]-y[1:t.i,1],na.rm=T),na.rm=T)
  }
  max.plot
  
  plot(y[,1],type="l",lwd=2,ylim=c(min.plot-.2,max.plot),ylab="Log real GDP p.c.",xlab="Year")
  
  #conf.common
  nn<-nrow(output.model$KF$alphahat)
  lows.c<-ts(c(output.model$common.trend-qnorm(0.99)*sqrt(c(output.model$KF$V[1,1,]+output.model$KF$V[3,3,]+2*output.model$KF$V[1,3,]))),start=start(y),f=1)
  ups.c<-ts(c(output.model$common.trend+qnorm(0.99)*sqrt(c(output.model$KF$V[1,1,]+output.model$KF$V[3,3,]+2*output.model$KF$V[1,3,]))),start=start(y),f=1)

  title(ynames[1],line=1)
  if(plot.type=="c.trend"){
    polygon(c(rev(time(output.model$KF$model$y)), time(output.model$KF$model$y)), c(rev(ups.c), lows.c), col = 'lavenderblush2', border = NA)
    lines(ups.c,col="purple",lwd=1)
    lines(lows.c,col="purple",lwd=1)
    legend.name<-"Common Trend & 99% CI"
    legend.fill<-"lavenderblush2"
    legend.border<-"purple"
    legend.lwd=1
    legend.lty=5
  }
  if(plot.type=="pred.target"){
    legend.name<-paste0("Predicted ",ynames[1])
    legend.fill<-"white"
    legend.border<-"white"
    legend.lwd=2
    legend.lty=2
  }

  for (i in 2:ncol(y)){
    lines(y[,i]-mean(y[1:t.i,i]-y[1:t.i,1]),col=colour[i-1])
  }
  
  lines(y[,1],lwd=2)
  
  lines(output.model$syn.y1,col="blue",lty=2,lwd=2)
  if(plot.type=="c.trend"){
    lines(output.model$common.trend,col="purple",lty=5,lwd=1)
  }
  if(plot.type=="pred.target"){
    lines(ts(predict(output.model$model)[[1]],start=start(y),f=1),col="purple",lwd=2,lty=5)
  }
  
  
  abline(v=vline,lwd=2)
  
  control.names<-ynames[-1]
  
  
  l.width1<-max(strwidth(c(paste0(ynames,paste0("  w=",format(round(output.model$weights,3),nsmall=3))),
                           paste0(c(ynames[1],legend.name,paste0("Synthetic ",ynames[1]),paste0(control.names)))),cex=0.8))
  l.width2<-max(strwidth(c(paste0(ynames,"  ")),cex=0.8))
  l.width3<-max(strwidth(c(paste0(c(ynames[1],legend.name,paste0("Synthetic ",ynames[1]),paste0(control.names))),cex=0.8)))
  
  
  l<-legend.v2("bottomright",inset=c(0.01,0.01),
               legend=paste0(c(ynames[1],legend.name,paste0("Synthetic ",ynames[1]),paste0(control.names))),
               col=c("black","purple","blue",colour[1:(ncol(y)-1)]),
               lty=c(1,legend.lty,2,rep(1,NCOL(y)-1)),
               fill=c(0,legend.fill,0,rep(0,length(ynames))),
               border=c("white",legend.border,"white",rep("white",length(ynames))),
               pch=-1,
               x.intersp = 1,
               text.width=l.width1,
               lwd=c(2,legend.lwd,2,rep(1,NCOL(y)-1)),
               cex=0.8,
               seg.len=4
  )
  
  legend("bottomright",inset=c(0.01,0.01),bty="n",
         legend=c(paste0("w=",format(round(output.model$weights,3),nsmall=3))),
         y.intersp=1,
         x.intersp=1,
         text.width=l.width1-l.width2,
         cex=0.8,
         seg.len=4,
  )
  if(png.out==T){
    if(is.null(png.name)){
      png.name<-paste0(ynames[1],paste0(ymd(today()),"_",hour(now()),"-",minute(now()),"-",round(second(now()),0)))
    }
    png(filename=paste0(png.name,".png"),units="in",width=7,height=4.5,res=1200,pointsize=12)
    par(mfrow=c(1,1),mai=c(0.8,0.8,0.5,0.2))
    plot(y[,1],type="l",lwd=2,ylim=c(min.plot,max.plot),ylab="Log real GDP p.c.",xlab="Year")
    
    #conf.common
    nn<-nrow(output.model$KF$alphahat)
    lows.c<-ts(c(output.model$common.trend-qnorm(0.99)*sqrt(c(output.model$KF$V[1,1,]+output.model$KF$V[3,3,]+2*output.model$KF$V[1,3,]))),start=start(y),f=1)
    ups.c<-ts(c(output.model$common.trend+qnorm(0.99)*sqrt(c(output.model$KF$V[1,1,]+output.model$KF$V[3,3,]+2*output.model$KF$V[1,3,]))),start=start(y),f=1)

    title(ynames[1],line=1)
    if(plot.type=="c.trend"){
      polygon(c(rev(time(output.model$KF$model$y)), time(output.model$KF$model$y)), c(rev(ups.c), lows.c), col = 'lavenderblush2', border = NA)
      lines(ups.c,col="purple",lwd=1)
      lines(lows.c,col="purple",lwd=1)
      legend.name<-"Common Trend & 99% CI"
      legend.fill<-"lavenderblush2"
      legend.border<-"purple"
      legend.lwd=1
      legend.lty=5
    }
    if(plot.type=="pred.target"){
      legend.name<-paste0("Predicted ",ynames[1])
      legend.fill<-"white"
      legend.border<-"white"
      legend.lwd=2
      legend.lty=2
    }
    
    for (i in 2:ncol(y)){
      lines(y[,i]-mean(y[1:t.i,i]-y[1:t.i,1]),col=colour[i-1])
    }
    
    lines(y[,1],lwd=2)
    
    lines(output.model$syn.y1,col="blue",lty=2,lwd=2)
    if(plot.type=="c.trend"){
      lines(output.model$common.trend,col="purple",lty=5,lwd=1)
    }
    if(plot.type=="pred.target"){
      lines(ts(predict(output.model$model)[[1]],start=start(y),f=1),col="purple",lwd=2,lty=5)
    }
    
    
    abline(v=vline,lwd=2)
    
    control.names<-ynames[-1]
    
    
    l.width1<-max(strwidth(c(paste0(ynames,paste0("  w=",format(round(output.model$weights,3),nsmall=3))),
                             paste0(c(ynames[1],legend.name,paste0("Synthetic ",ynames[1]),paste0(control.names)))),cex=0.8))
    l.width2<-max(strwidth(c(paste0(ynames,"  ")),cex=0.8))
    l.width3<-max(strwidth(c(paste0(c(ynames[1],legend.name,paste0("Synthetic ",ynames[1]),paste0(control.names))),cex=0.8)))
    
    
    l<-legend.v2("bottomright",inset=c(0.01,0.01),
                 legend=paste0(c(ynames[1],legend.name,paste0("Synthetic ",ynames[1]),paste0(control.names))),
                 col=c("black","purple","blue",colour[1:(ncol(y)-1)]),
                 lty=c(1,legend.lty,2,rep(1,NCOL(y)-1)),
                 fill=c(0,legend.fill,0,rep(0,length(ynames))),
                 border=c("white",legend.border,"white",rep("white",length(ynames))),
                 pch=-1,
                 x.intersp = 1,
                 #xmfill=1.9,
                 text.width=l.width1,
                 lwd=c(2,legend.lwd,2,rep(1,NCOL(y)-1)),
                 cex=0.8,
                 seg.len=4
    )
    
    legend("bottomright",inset=c(0.01,0.01),bty="n",
           legend=c(paste0("w=",format(round(output.model$weights,3),nsmall=3))),
           y.intersp=1,
           x.intersp=1,
           text.width=l.width1-l.width2,
           cex=0.8,
           seg.len=4,
    )
    dev.off()
  }
}

# Function to plot residuals of an estimated model
# inputs: same inputs as for function plot.model.output
plot.res<-function(output.model,ynames,png.out=F,png.name=NULL){
  
  #irreg
  nc<-ceiling(ncol(output.model$KF$model$y)/2)
  par(mfrow=c(nc+1,2))
  for(i in 1:NCOL(output.model$KF$model$y)){
    plot(output.model$irreg[,i],main=paste0("irregular ",colnames(output.model$KF$model$y)[i]))
    abline(h=0)
  }
  if(NCOL(output.model$KF$model$y)%in%c(1,3,5,7,9)){
    plot(0, type = "n",                         # Remove all elements of plot
         xlab = "", ylab = "",axes=F)
    abline(h=0)
  }
  
  #state
  plot(output.model$rest.state[,1],main=paste0("level state residuals"))
  abline(h=0)
  
  #innovations
  plot(output.model$rest[,1],main=paste0("innovations ",colnames(output.model$KF$model$y)[1]))
  abline(h=0)
  
  if(png.out==T){
    if(is.null(png.name)){
      png.name<-paste0(ynames[1],paste0(ymd(today()),"_",hour(now()),"-",minute(now()),"-",round(second(now()),0)))
    }
    png(filename=paste0(png.name,".png"),units="in",width=7,height=4.5,res=1200)
    
    #irreg
    par(mfrow=c(3,2))
    for(i in 1:NCOL(output.model$KF$model$y)){
      plot(output.model$irreg[,i],main=paste0("irregular ",colnames(output.model$KF$model$y)[i]))
      abline(h=0)
    }
    
    #state
    plot(output.model$rest.state[,1],main=paste0("level state residuals"))
    abline(h=0)
    
    #innovations
    plot(output.model$rest[,1],main=paste0("innovations ",colnames(output.model$KF$model$y)[1]))
    abline(h=0)
    
    dev.off()
  }
  par(mfrow=c(1,1))
}

#Function to plot the gap of original-synthetic control
# inputs: same inputs as for function plot.res except than
# - gap: a vector containing the gap of the output of function est.fn()
plot.gap<-function(gap,ynames,vline=NULL,png.out=F,png.name=NULL){
  
  plot(gap,main=paste0(ynames[1]," - synthetic ",ynames[1]),lwd=2,ylab="Difference")
  abline(v=vline,lwd=2)
  
  if(png.out==T){
    if(is.null(png.name)){
      png.name<-paste0(ynames[1],paste0(ymd(today()),"_",hour(now()),"-",minute(now()),"-",round(second(now()),0)))
    }
    png(filename=paste0(png.name,".png"),units="in",width=7,height=4.5,res=1200,pointsize=12)
    
    
    plot(gap,main=paste0(ynames[1]," - synthetic ",ynames[1]),lwd=2,ylab="Difference")
    abline(v=vline,lwd=2)
    
    dev.off()
  }
  
}
