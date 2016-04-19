#' Produces simulations of data based on a current tlpl filtered distribution 
#' 
#' @param sckm a list defining a stochastic chemical kinetic model
#' @param n an integer determining how many forward points to simulate
#' @param tlpl a list defining a filtered distribution
#' @param engine use 'R' or 'C' code 
#' @param verbose level of verbosity curing calculations
#' @return list defining the states and the number of transitions
#' @seealso \code{\link{tlpl}}
#' @author Jarad Niemi \email{niemi@@iastate.edu}
#' @export tlpl_predict
#'
tlpl_predict = function(sckm, n, tlpl, engine="C", verbose) 
{
  ns = dim(tlpl$X)[1]
  np = dim(tlpl$X)[2]
  nt = dim(tlpl$X)[3]
  nr = length(sckm$theta)
	
  X = array(0, dim=c(ns, np, n+1))
  y = array(0, dim=c(nr, np, n))
    
  for (i in 1:np) 
  {
    if (verbose>0 && i%%100==0) cat("Particle",i,"(",i/np*100,"%)\n") 

    sckm$X = tlpl$X[,i,nt]
    sckm$theta = rgamma(nr, tlpl$hyper$rate$a[,i,nt], tlpl$hyper$rate$b[,i,nt])

    # Simulate true states
    tl = tau_leap(sckm, n, engine=engine)
    X[,i,] = t(tl$X)

    p = rbeta(nr, tlpl$hyper$prob$a[,i,nt], tlpl$hyper$prob$b[,i,nt])

    # Simulate data
    for (j in 1:nr) 
    {
      y[j,i,] = rbinom(n, tl$nr[,j], p[j])
    }
  }
  return(list(X=X,y=y))
}

