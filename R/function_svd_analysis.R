svd_analysis <- function(Y,X,G){
	q <- dim(X)[2]
	p <- dim(G)[2]
  xg <- cbind(X,G)
  
  c <- solve(t(G) %*% G) %*% t(G) %*% X
  
  Dg <- solve(t(G) %*% G) %*% t(G) %*% Y
  Dg <- diag(c(Dg))
  
  betaxg <- solve(t(xg) %*% xg) %*% t(xg) %*% Y
  Dx <- diag(c(betaxg)[1:q])
  
  w <- Dg %*% c %*% Dx
  
  alpha <- w / matrix(rep(diag(Dg),q),p,q)^2
  
  singulars <- svd(w)$d
  u <- svd(w)$u
  v <- svd(w)$v
  
  return(list(w=w,alpha=alpha,singulars=singulars,u=u,v=v))
}

