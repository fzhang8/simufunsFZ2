source("./code/function_gen_lat_YXG_3.R")
source("./code/function_gen_obs_YXG_3.R")
source("./code/function_svd_analysis.R")
source("./code/function_plot_singular.R")
source("./code/function_center.R")

library(MASS)
library(ggplot2)

Ltout <- simuLatYXG(betay_xg = 3,
										betay_gx = 3,
										betax_g = 3,
										betax_h  =  3, 
										n = 20,
										var_yxg = 1,
										var_xg = 1,
										var_xh = 1, 
										p = 0.2)



obs <- generate_obs_YXG(Gmod="d",
    												n = 10,
                            Gcolmn = 5,
                            Xcolmn = 3,
                            actvXcolmn = 1,
                            actvGcolmn = 2,
                            laty = Ltout$Z$y,
                            latx = Ltout$Z$x,
                            latg = Ltout$Z$g,
                            var_obs_y = 1,
                            var_obs_x = 1,
                            var_obs_g = 1,
                            p0hat = Ltout$p0hat,
                            p1hat = Ltout$p1hat,
                            p2hat = Ltout$p2hat,
                            Xcenter=TRUE,
                            Xmean="l",  #c("obsmean","latmean"),
														Gcenter=TRUE,
														Gmean="l")  #c("obsmean","latmean"))

result <- svd_analysis(obs$Y,obs$X,obs$G)

singular_plot(result$singulars)
    
#print(lapply(result,round,3))

