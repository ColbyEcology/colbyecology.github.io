# Load libraries
  library(package = "deSolve")

# Set up what we need for our numerical solver (ode())
  logmap <- function(time, state, pars) {
    with(as.list(c(state, pars)), {
      Nt1 <- Nt*lambda*(1 - Nt)
      return(list(c(Nt1)))
    })
  }
  
  time_vec <- 0:1000
  parm_vec <- c(lambda = NA)
  N0 <- c(Nt = 0.1)

# Empty data structure and lambdas
  n_lambdas <- 500
  lambda_vec <- seq(from = 0.1, to = 4, length.out = n_lambdas)
  lam_mat <- matrix(data = NA, nrow = n_lambdas, ncol = 10)

# For loop
  for (i in 1:n_lambdas) {
    lambda_i <- lambda_vec[i]
    parm_vec["lambda"] <- lambda_i
    out_i <- ode(y = N0, times = time_vec, parms = parm_vec, func = logmap, method = "iteration")
    lam_mat[i, ] <- out_i[991:1000, 2]
  }

# Make the bifurcation diagram
  plot(x = NA, type = "n", xlim = c(0.1, 4), ylim = c(0, 1), xlab = "Lambda", ylab = expression("Density at the last 10 steps, N"[991-1000]), las = 1)
  for (i in 1:n_lambdas) {
    lambda_i <- lambda_vec[i]
    # Repeat lambda for the nubmer of final steps you're saving
    lambda_veci <- rep(x = lambda_i, times = 10)
    points(x = lambda_veci, y = lam_mat[i,], pch = 19, col = "#0000FF22", cex = 0.5)
  }