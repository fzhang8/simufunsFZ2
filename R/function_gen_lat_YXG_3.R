
simuLatYXG <- function(betay_xg = 3,betay_gx = 3,betax_g = 3, betax_h  =  3, 
												n = 20,var_yxg = 1,var_xg = 1,var_xh = 1, p = 0.2){
		set.seed(123456)
		
		###### latent G ###########  
		all <- rbinom(4*n,1,p)
		G1H1 <- matrix(all[1:(2*n)],n,2)
		G2H2 <- matrix(all[(2*n+1):(4*n)],n,2)
		GH <- G1H1 + G2H2
		###########################
		
		######## latent X #############
		beta <- matrix(rep(c(betax_g,betax_h),each=n),n,2)
		error <- matrix(c(rnorm(n,0,var_xg),rnorm(n,0,var_xh)),n,2)
		X <- beta * GH + error
		############################
		
		########### Y ###############
		Y <- betay_xg * X[,1] + betay_gx * GH[,1] + rnorm(n,0,var_yxg)
		Y <- matrix(Y,n,1)
		############################
		
		return(list(Y = Y, X = X, GH = GH,G1H1 = G1H1, G2H2 = G2H2))
}
