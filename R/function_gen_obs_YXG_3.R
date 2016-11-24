
generate_obs_YXG <- function(Gcolmn = 6,Xcolmn = 8,actvXcolmn = 4,actvGcolmn = 3,
															laty,latx,latG1H1,latG2H2,latG,var_obs_x = 1,var_obs_h = 1,
															D = 0,Gprob = 0.2,gprob = 0.2,
															Xcenter = FALSE,Xmean = c("obsmean","latmean"),
															Gcenter = FALSE,Gmean = c("obsmean","latmean")){
  set.seed(12345678)
  Xmean <- tolower(Xmean)
  Xmean <- match.arg(Xmean)
  Gmean <- tolower(Gmean)
  Gmean <- match.arg(Gmean)
  
		n <- length(laty)
		p <- Gcolmn
	  q <- Xcolmn
  	m <- actvXcolmn
  	h <- actvGcolmn
  
	  ######## generate G matrix ############
		Pg11 <- gprob + D / Gprob
		Pg10 <- gprob - D / (1 - Gprob)
		
		Grank <- 0
		while(Grank != p){
  		g1 <- matrix(0,n,1)
  		g2 <- matrix(0,n,1)
  		for(i in 1:h){
  			g1col <- sapply(latG1H1[,1],function(x){ifelse(x == 1,rbinom(1,1,Pg11),rbinom(1,1,Pg10))})
  			g2col <- sapply(latG2H2[,1],function(x){ifelse(x == 1,rbinom(1,1,Pg11),rbinom(1,1,Pg10))})
  			g1col <- matrix(g1col,,1)
  			g2col <- matrix(g2col,,1)
  			g1 <- cbind(g1,g1col)
  			g2 <- cbind(g2,g2col)
  		}
  		g1 <- matrix(g1[,-1],n,)
  		g2 <- matrix(g2[,-1],n,)
  
  		h1 <- matrix(0,n,1)
  		h2 <- matrix(0,n,1)
  		for(i in 1:(p-h)){
  			h1col <- sapply(latG1H1[,2],function(x){ifelse(x == 1,rbinom(1,1,Pg11),rbinom(1,1,Pg10))})
  			h2col <- sapply(latG2H2[,2],function(x){ifelse(x == 1,rbinom(1,1,Pg11),rbinom(1,1,Pg10))})
  			h1col <- matrix(h1col,,1)
  			h2col <- matrix(h2col,,1)
  			h1 <- cbind(h1,h1col)
  			h2 <- cbind(h2,h2col)
  		}
  		h1 <- matrix(h1[,-1],n,)
  		h2 <- matrix(h2[,-1],n,)
  
  	  G <- cbind(g1,h1) + cbind(g2,h2)
  	  Grank <- qr(G)$rank
	  }
	  
	  
	  if(Gcenter){
	  	if(Gmean == "obsmean"){
	  		G <- demean(G)
	  	}else{
	  		G <- demean(G,c(rep(colMeans(latG)[1],h),rep(colMeans(latG)[2],p-h)))
	  	}
	  }
	  #######################################
  
	  ########## generate X matrix ##########
	  X <- apply(latx,1,function(x){c(rnorm(m,x[1],sqrt(var_obs_x)),rnorm(q-m,x[2],sqrt(var_obs_h)))})
	  X <- t(X)
	  
	  if(Xcenter){
	  	if(Xmean == "obsmean"){
	  		X <- demean(X)
	  	}else{
	  		X <- demean(X,c(rep(colMeans(latx)[1],m),rep(colMeans(latx)[2],q-m)))
	  	}
	  }
	  #######################################
  
	  ########### generate Y vector ########
	  Y <- laty
	  ########################################
	
  

  return(list(Y = Y,X = X,G = G,Pg11=Pg11,Pg10=Pg10))
}

