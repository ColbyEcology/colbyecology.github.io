# Load libraries
	library(package = "deSolve")

# Flu data
	time_seq <- 0:13
	It <- c(1, 7, 28, 77, 222, 292, 255, 235, 189, 124, 67, 28, 10, 4)
	df <- data.frame(time = time_seq, I = It)

# Look at data
	plot(x = df$time, y = df$I, pch = 19, col = "aquamarine", cex = 2, las = 1, xlab = "Days", ylab = "Infectid")

# SIR ODE
	SIR <- function(time, state, pars) {
		with(as.list(c(state, pars)), {
			dS <- -beta*S*I
			dI <- beta*S*I - gamma*I
			dR <- gamma*I
			return(list(c(dS, dI, dR)))
		})
	}

	parm_vec <- c(beta = 0.00225, gamma = 0.5)
	time_vec <- seq(from = 0, to = 15, by = 0.1)
	init <- c(S = 762, I = 1, R = 0)
	
	out <- ode(y = init, times = time_vec, func = SIR, parms = parm_vec)
	
	plot(x = NA, type = "n", las = 1, xlab = "Days", xlim = c(0, 15), ylim = c(0, 800))
	lines(x = out[,1], y = out[,2])
	lines(x = out[,1], y = out[,3], col = "red")
	lines(x = out[,1], y = out[,4], col = "blue")
	points(x = df$time, y = df$I, pch = 19, col = "aquamarine")
	
# Calculate sum of square errors	
	sse_sir <- function(parms, data) {
		t <- data$time
		cases <- data$I
		ode_out <- ode(y = init, func = SIR, times = t, parms = parms, hmax = 0.01)
		out <- as.data.frame(ode_out)
		sse <- sum((out$I - cases)^2)
	}

# Minimize sum of square errors	
	fit <- optim(parm_vec, sse_sir, data = df)
	
# Plot fitted parameters to data
	fitted_parms <- fit$par
	out <- ode(y = init, times = time_vec, func = SIR, parms = fitted_parms)
	
	plot(x = NA, type = "n", las = 1, xlab = "Days", xlim = c(0, 15), ylim = c(0, 800))
	lines(x = out[,1], y = out[,2])
	lines(x = out[,1], y = out[,3], col = "red")
	lines(x = out[,1], y = out[,4], col = "blue")
	points(x = df$time, y = df$I, pch = 19, col = "aquamarine")
