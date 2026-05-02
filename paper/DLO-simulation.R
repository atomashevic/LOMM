# Simulation of emotion data based on Damped Linear Oscillator (DLO) model
# Dependencies: EGAnet package 2.0.2

library(EGAnet)

# @title Dynamics function for the DLO model
# '@description This function calculates the dynamics of a system using the DLO (Damped Linear Oscillator) model based on Equation 1 (Ollero et al., 2023).
#' The DLO model is a second-order differential equation that describes the behavior of a damped harmonic oscillator.
#' The function takes in the current state of the system, the derivative of the state, the damping coefficient, the time step,
#' and the values of the eta and zeta parameters. It returns the updated derivative of the state.
#'
#' @param x Numeric (length 2) 
#' The current state of the system (value of the latent score)
#' @param dxdt Numeric (length 2)
#' The derivative of the state (rate of change of the latent score)
#' @param q Numeric 
#' The damping coefficient
#' @param dt Numeric 
#' The time step
#' @param eta Numeric 
#' The eta parameter of the DLO model
#' @param zeta Numeric 
#' The zeta parameter of the DLO model
#'
#' @return A numeric vector containing the updated derivative of the state
#'
#' @examples
#' dynamics(c(0, 0), c(0, 0), 0.1, 0.01, 0.5, 0.1)
# Updated: 26.10.2023.
dynamics <- function(x, dxdt, q, dt, eta, zeta){
  dxdt_new <- dxdt
  dx2dt <- eta * x + zeta * dxdt + q
  return(c(dxdt_new, dx2dt))
}

# mvrnorm function from MASS package to remove dependency on MASS package
# Updated: 26.10.2023.
MASS_mvrnorm <- function(n = 1, mu, Sigma, tol = 1e-06, empirical = FALSE, EISPACK = FALSE){
  p <- length(mu)
  if (!all(dim(Sigma) == c(p, p))) 
    stop("incompatible arguments")
  if (EISPACK) 
    stop("'EISPACK' is no longer supported by R", domain = NA)
  eS <- eigen(Sigma, symmetric = TRUE)
  ev <- eS$values
  if (!all(ev >= -tol * abs(ev[1]))) 
    stop("'Sigma' is not positive definite")
  X <- matrix(rnorm(p * n), n)
  if (empirical) {
    X <- scale(X, TRUE, FALSE)
    X <- X %*% svd(X, nu = 0)$v
    X <- scale(X, FALSE, TRUE)
  }
  X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*% 
    t(X)
  nm <- names(mu)
  if (is.null(nm) && !is.null(dn <- dimnames(Sigma))) 
    nm <- dn[[1]]
  dimnames(X) <- list(nm, NULL)
  if (n == 1) 
    drop(X)
  else t(X)
  
}

#' @title  Generate a matrix of Dynamic Error values for the DLO simulation
#' @description This function generates a matrix of Dynamic Error values (q) for the DLO simulation.
#' @param num_steps Numeric integer
#' The number of time steps used in the simulation
#' @param sigma_q Numeric 
#' Standard deviation of the Dynamic Error
#' @return A \eqn{num_{steps} \times 3} matrix of Dynamic Error values for neutral, negative and positive emotion latent score
#' @examples
#' generate_q(100, 0.5)
#' Updated: 26.10.2023.
generate_q <- function(num_steps, sigma_q) {
  q <- matrix(rnorm(3 * num_steps, mean = c(0, 0, 0), sd = sigma_q), ncol = 3)
  return(q)
}

#' @title  Calculate the moving average for a time series
#' @description This function calculates the moving average for a time series.
#' @param data: Matrix or Data frame 
#' The time series data
#' @param window_size: Numeric integer
#' The size of the moving average window
#' @return Matrix or Data frame containing the moving average values
#' @examples
#' calculate_moving_average(c(1,2,3,4,5), 2)
#' Updated: 26.10.2023.
calculate_moving_average <- function(data, window_size) {
  n <- length(data)
  ma <- numeric(n)
  for (i in (window_size):(n - window_size)) {
    ma[i] <- mean(data[(i - window_size + 1):(i + window_size - 1)])
  }
  # Extend the moving average to the edges of the time series
  for (i in 1:(window_size - 1)) {
    ma[i] <- mean(data[1:(i + window_size - 1)])
  }
  for (i in (n - window_size + 1):n) {
    ma[i] <- mean(data[(i - window_size + 1):n])
  }
  return(ma)
}

#' @title Generate observable emotion scores data from latent variables
#' @description Function to generate observable data from 2 latent variables (negative and positive affect).
#' The function takes in the latent variable scores, the number of time steps, the number of observable variables per latent factor,
#' and the measurement error variance. It returns a matrix of observable data.
#' The factor loadings are not the same for all observable variables. They have uniform random noise added to them (between -0.15 and 0.15).
#' The loadings are scaled so that the sum of the loadings for each latent factor is 2, to introduce a ceiling effect and to differentiate the dynamics of specific emotions. This is further empahsized by adding small noise to the measurement error variance for each observed variable (between -0.01 and 0.01).
#' 
#' @param X Matrix or Data frame
#' The \eq{num_{steps} \times 2} matrix of latent variable scores
#' @param num_steps: Numeric integer
#' Number of time steps
#' @param num_obs: Numeric integer
#' The number of observable variables per latent factor
#' @param error: Numeric
#' Measurement error variance
#' @param loadings: Numeric (default = 0.8)
#' The default initial loading of the latent variable on the observable variable
#' @return A \eq{num_{steps}\times num_{obs}} Matrix or Data frame containing the observable variables
#' @examples
#' generate_observables(latent_scores, 100, 4, 0.05)
#' Updated: 26.10.2023.
generate_observables <- function(X, num_steps, num_obs, error, loadings=0.5){
  loads <- lapply(rep(loadings, 2), rep, num_obs)
  loads <- lapply(loads, function(x){x + runif(length(x), -0.15, 0.15)}) # add noise to loadings
  LoadMat <- as.matrix(Matrix::bdiag(loads))
  LoadMat <- LoadMat / (colSums(LoadMat) + runif(1, -0.05, 0.05))*2 # scale loadings so that sum of loadings for each latent factor is 2
  var <- rep(error, num_obs*2) + runif(num_obs*2, -0.01, 0.01) # add noise to measurement error variance
  Q <- diag(var,num_obs*2,num_obs*2)
  e <- t(MASS_mvrnorm(num_steps, matrix(0,num_obs*2,1),Q))
  obs.data <- LoadMat %*% t(X) + e
  obs.data <- as.data.frame(t(obs.data))
  obs.data[obs.data < 0] <- 0 # negative values of observed variables are not possible
  obs.data <- obs.data/2 # scale observed variables so that the sum of the observed variables for each latent factor is roughly 1
  return(obs.data)
}

#' @title Simulate latent and observed emotion scores for a single "video"
#' @description This function simulates emotions in a video using the DLO model implemented as continuous time state space model. The function takes in several parameters, including the time step, number of steps, number of observables, and various model parameters. It returns a data frame containing the simulated emotions and their derivatives, as well as smoothed versions of the observables.
#' The initial state of the video is always the same. Neutral score is 0.5 and both positive and negative emotion score is 0.25. They always sum up to 1. The derivative of the neutral score is higher initially, as there is strong tendency that some emotional state will be reached. The derivatives of the positive and negative emotions are the same initially.
#'
#' @param dt Numeric real
#' The time step for the simulation (in minutes)
#' @param num_steps Numeric real
#' Total length of the video (in minutes)
#' @param num_observables  Numeric integer 
#' The number of observables to generate per factor. Total number of observables generated is 2 x num_observables
#' @param eta_n Numeric
#' The eta parameter for the neutral state
#' #TODO Interpretation of the parameters
#' @param zeta_n Numeric
#' The zeta parameter for the neutral state
#' @param eta Numeric 
#' The eta parameter for the positive and negative emotions
#' @param zeta Numeric
#' The zeta parameter for the positive and negative emotions
#' @param sigma_q Numeric
#' The standard deviation of Dynamic Error of the q(t) function
#' @param sd_observable Numeric
#' The standard deviation of the measurement error
#' @param window_size Numeric integer
#' The window size for smoothing the observables
#' #TODO Change the output to a list with model parameters saved
#' @return A data frame \eq{num_{steps}\times (6 + num_{observables})} containing the latent scores for neutral score, positive emotions, negative emotions and their derivatives, as well as smoothed versions of the observables
#' @export
#' @examples
#' simulate_video(dt = 0.01, num_steps = 50, num_observables = 4, eta_n = 0.5, zeta_n = 0.5, eta = 0.5, zeta = 0.5, sigma_q = 0.1, sd_observable = 0.1, window_size = 10)
#' Updated: 26.10.2023.
simulate_video <- function(dt, num_steps, num_observables, eta_n, zeta_n, eta, zeta, sigma_q, sd_observable, window_size){
  # Initial condition for neutral state
  n_0 <- 0.5
  dndt_0 <- 4
  # Initial conditions for negative emotion
  x_0 <- 0.25
  dxdt_0 <- 2
  # Initial conditions for positive emotion
  y_0 <- 0.25
  dydt_0 <- 2
  
  n <- numeric(num_steps)
  dndt <- numeric(num_steps)
  x <- numeric(num_steps)
  dxdt <- numeric(num_steps)
  y <- numeric(num_steps)
  dydt <- numeric(num_steps)
  
  # Initial values for x1 and y1
  n[1] <- n_0
  dndt[1] <- dndt_0
  x[1] <- x_0
  dxdt[1] <- dxdt_0
  y[1] <- y_0
  dydt[1] <- dydt_0
  
  # Generate q(t)
  q_t <- generate_q(num_steps, sigma_q)
  
  # Euler method integration for x1, y1
  for (i in 2:num_steps) {
    # Update x1 and y1
    derivatives_n <- dynamics(n[i - 1], dndt[i - 1], q_t[i, 1], dt, eta_n, zeta_n)
    n[i] <- n[i - 1] + derivatives_n[1] * dt
    dndt[i] <- derivatives_n[2] * dt
    derivatives_x <- dynamics(x[i - 1], dxdt[i - 1], q_t[i, 2], dt, eta, zeta)
    x[i] <- x[i - 1] + derivatives_x[1] * dt
    dxdt[i] <- derivatives_x[2] * dt
    derivatives_y <- dynamics(y[i - 1], dydt[i - 1], q_t[i, 3], dt, eta, zeta)
    y[i] <- y[i - 1] + derivatives_y[1] * dt
    dydt[i] <- derivatives_y[2] * dt
    if (n[i] < 0) {
      n[i] <- 0
    }
    if (x[i] < 0) {
      x[i] <- 0
    }
    if (y[i] < 0) {
      y[i] <- 0
    }
    total_sum <-  n[i] + x[i] + y[i]
    if (total_sum != 1) {
      scaling_factor <- 1 / total_sum
      n[i] <- n[i] * scaling_factor
      x[i] <- x[i] * scaling_factor
      y[i] <- y[i] * scaling_factor
    }
  }
  factors <- cbind(x,y)
  observables <- generate_observables(factors, num_steps, num_observables, sd_observable)
  smoothed_x <- apply(observables[,1:num_observables], 2, calculate_moving_average, window_size)
  smoothed_y <- apply(observables[,(num_observables+1):(2*num_observables)], 2, calculate_moving_average, window_size)
  results <- as.data.frame(cbind(n,dndt, x, dxdt, y, dydt, smoothed_x, smoothed_y))
  colnames(results) <- c("Neu","dNeu","N","dN", "P","dP", paste0("N",1:num_observables), paste0("P",1:num_observables))
  return(results)
}

#' @title: Emphasize sudden jumps in emotion scores
#' @description This function emphasizes the effect of strong emotions expressions during the period where the derivative of the latent variable is high. The observable value of the strongest emotion from the positive or negative group will spike in the next k time steps. The probability of this happening is p at each time step in which the derivative of the latent variable is greater than 0.2. The jump is proportionate to the derivative of the latent variable and the sum of the observable values of the other emotions.
#' @param data: Data frame
#' The data frame containing the latent and observable variables created by the \code{simulate_video} function.
#' @param k: Numeric integer
#' The mumber of time steps to emphasize the effect of strong emotions on future emotions (default is 10). Alternatively: the length of a strong emotional episode.
#' @param p: Numeric
#' The probability of the strongest emotion being emphasized in the next k time steps (default is 0.5).
#' @return A data frame containing the updated observable variables.
#' @examples
#' emphasize(sim_results, 10, 0.5)
#' Updated: 26.10.2023.
emphasize <- function(data, k = 10, p = 0.5){
  # TODO Replace 0.2 with percentile limit
  # emphasize negative emotions
  neg_col <- 6+num_observables
  dn.inc <- which(data[,4]>0.2)
  for (i in dn.inc){
    if (i>20 & i<(num_steps-k)){
    dr.ratio <- data[i,4]/0.2
    q = runif(1)
    if (q < p){
      s = runif(1,0.25,0.5)
      for (j in 1:k){
        sum.p <- sum(data[i+j,7:neg_col])
        w = which(data[i+j,7:neg_col]==max(data[i+j,7:neg_col])) + 6
        r = which(7:neg_col!=w)+6
        data[i+j,w] = data[i+j,w] + s*sum.p*dr.ratio # the height of the spike is proportional to the sum of the other emotions and the ratio of the derivative of the latent variable
      }
    }
  }
  }
  # emphasize positive emotions
  pos_col <- 6+2*num_observables
  dp.inc <- which(data[,6]>0.2)
  for (i in dp.inc){
    if (i>20 & i<(num_steps-k)){
    dr.ratio <- data[i,6]/0.2
    q = runif(1)
    if (q < p){
      for (j in 1:k){
        sum.p <- sum(data[i+j,(neg_col+1):pos_col])
        w = which(data[i+j,(neg_col+1):pos_col]==max(data[i+j,(neg_col+1):pos_col])) + 6 + num_observables
        r = which((neg_col+1):pos_col!=w)+6+num_observables
        data[i+j,w] = data[i+j,w] + runif(1,0,0.25)*sum.p*dr.ratio
    }
    }
    }
  }
  data[,7:pos_col] <- apply(data[,7:pos_col], 2, calculate_moving_average, 2) # smooth the observable variables, even out the huge spikes due to large derivatives
  return(data)
}

#' @title: Run a large simulation of video data with specified parameters
#' @description Function to run a large simulation of video data with specified parameters
#' @param dt: Numeric real
#' The time step for the simulation (in minutes)
#' @param num_steps: Numeric real
#' Total length of the video (in minutes)
#' @param num_observables  Numeric integer
#' The number of observables to generate per factor. Total number of observables generated is 2 x num_observables
#' @param eta_n Numeric
#' The eta parameter for the neutral state
#' @param zeta_n Numeric
#' The zeta parameter for the neutral state
#' @param eta Numeric
#' The eta parameter for the positive and negative emotions
#' @param zeta Numeric
#' The zeta parameter for the positive and negative emotions
#' @param sigma_q Numeric
#' The standard deviation of Dynamic Error of the q(t) function
#' @param sd_observable Numeric
#' The standard deviation of the measurement error
#' @param window_size Numeric integer
#' The window size for smoothing the observables
#' @param n Numeric integer
#' The number of simulations to run
#' @return A matrix of simulation results, with each row representing a time step and each column representing an latent variable, its derivative and observable variables for a single simulation
#' @examples
#' large_simulation(dt = 0.01, num_steps = 50, num_observables = 4, eta_n = 0.5, zeta_n = 0.5, eta = 0.5, zeta = 0.5, sigma_q = 0.1, sd_observable = 0.1, window_size = 10, n = 100)
#' Updated: 26.10.2023.
large_simulation <- function(dt, num_steps, num_observables, eta_n, zeta_n, eta, zeta, sigma_q, sd_observable, window_size, n){
  results = c()
  for (i in 1:n) {
    id <- rep(i, num_steps)
    eta_n <- sample(eta_ns,1)
    zeta_n <- sample(zeta_ns,1)
    eta <- sample(etas,1)
    zeta <- sample(zetas,1)
    sigma_q <- sample(sigma_qs,1)
    sim_results <- simulate_video(dt, num_steps, num_observables, eta_n, zeta_n, eta, zeta, sigma_q, sd_observable, window_size)
    sim_results <- emphasize(sim_results, 10, 0.5)
    sim_results$id <- id
    results <-  rbind(results, sim_results)
  }
  return(results)
}

#' @title: Plot the latent or the observable emotion scores
#' @description Function to plot the latent or the observable emotion scores
#' @param df: Data frame
#' The data frame containing the latent and observable variables created by the \code{simulate_video} function.
#' @param mode: Character
#' The mode of the plot. Can be either 'latent', 'positive' or 'negative'
#' @return A plot of the latent or the observable emotion scores
#' @examples
#' plot_emotions(sim_results, 'latent')
#' Updated: 26.10.2023.
plot_emotions <- function(df, mode = 'latent', title = ' '){
  n_obs <- (ncol(df)-6)/2
  cols <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

  if (mode == 'latent')
  {
    title_label <-  ifelse(title==' ', paste0("Latent Emotion Scores"), title)
    plot(df$Neu, type = "l", col = "gray", ylim = c(0, 1), main = title_label, xlab = "Frames", ylab = "Scores")
    lines(df$N, col = "#F84C1E")
    lines(df$P, col = "#3751a1")
    legend("topleft", legend = c("Neutral","Negative","Positive"), col = c("gray","#F84C1E", "#3751a1"), lty = 1)
  }
  else if (mode=='positive') {
     title_label <-  ifelse(title==' ', paste0("Positive Emotion Scores"), title)
     plot(df$P1, type = "l", col = cols[1], ylim = c(0, 0.4), main = "Positive emotions", xlab = "Frames", ylab = "Scores")
     for (i in 2:n_obs) {
       lines(df[,6+n_obs+i], col = cols[i])
     }
      legend("topleft", legend = c(rep(paste0("P",1:n_obs))), col = cols[1:4], lty = 1)
  } else {
    title_label <-  ifelse(title==' ', paste0("Negative Emotion Scores"), title)
    plot(df$N1, type = "l", col = cols[1], ylim = c(0, 0.4), main = "Negative emotions", xlab = "Frames", ylab = "Scores")
    for (i in 2:n_obs) {
      lines(df[,6+i], col = cols[i])
    }
    legend("topleft", legend = c(rep(paste0("N",1:n_obs))), col =cols[1:4], lty = 1)
  }
}

# Demonstration of the functions

## Parameters

window_size <- 5
eta_n <- -0.1
zeta_n <- -2
eta <- -1
zeta <- -0.1
sigma_q <- 0.7
sd_observable <- 0.1
dt <- 0.15
num_steps <- 300
num_observables <- 4

# (-0.1, -2, -1, -0.1)
## Simulation, one video

sim_results <- simulate_video(dt, num_steps, num_observables, eta_n, zeta_n, eta, zeta, sigma_q, sd_observable, window_size)

sim_results <- emphasize(sim_results, k = 20)

plot_emotions(sim_results, 'latent')
plot_emotions(sim_results, 'positive')
plot_emotions(sim_results, 'negative')

# Simulate larger number of videos

eta_ns <- c(-0.2,-0.25,-0.3,-0.35)
zeta_ns <- c(1.4,1.5,1.6,1.7)

etas = c(-0.2,-0.25,-0.3,-0.35)
zetas = c(-0.2, -0.3, -0.4, -0.5)
sigma_qs= c(0.3,0.4, 0.5, 0.6, 0.7)

n = 30

simulation_1 <- large_simulation(dt, num_steps, num_observables,
                                 eta_ns, zeta_ns, etas, zetas, sigma_qs, sd_observable,
                                 window_size, n)


k = sample(1:n,1)

plot_emotions(simulation_1[simulation_1$id==k,], 'latent')
plot_emotions(simulation_1[simulation_1$id==k,], 'positive')
plot_emotions(simulation_1[simulation_1$id==k,], 'negative')


## Run dynEGA

dynega.0 <- dynEGA(
  data = simulation_1[,-(1:6)],
  id = (2*num_observables)+1,
  n.emdbed = 5,
  tau = 1,
  delta = 1,
  use.derivatives = 0,
  algorithm = 'louvain',
  level = c("individual", "population")
)

plot(dynega.0)

### Calculate network scores

net.data <- simulation_1[,-(1:6)]

net.data <- net.data[,-ncol(net.data)]

labels <- colnames(net.data)

network <- dynega.0$dynEGA$population$network

colnames(network) <- labels
rownames(network) <- labels

wc <- dynega.0$dynEGA$population$wc

names(wc) <- labels

net.scores.0 <- net.scores(net.data, network, wc)

results <- as.data.frame(cbind(net.scores.0$scores$std.scores, simulation_1$id, simulation_1$P, simulation_1$N))

colnames(results) <- c('D1', 'D2', 'id', 'P', 'N')

ids <- unique(results$id)

correlations <- matrix(0, nrow = length(ids), ncol = 2)

for (id in ids){
  correlations[id,1] <- cor(results[results$id==id,]$D1, results[results$id==id,]$N)
  correlations[id,2] <- cor(results[results$id==id,]$D2, results[results$id==id,]$P)
  }
  
  
### Run dynEGA with derivatives

dynega.1 <- dynEGA(
  data = simulation_1[,-(1:6)],
  id = (2*num_observables)+1,
  n.emdbed = 5,
  tau = 1,
  delta = 1,
  use.derivatives = 1,
  algorithm = 'louvain',
  level = c("individual", "population")
)

network <- dynega.1$dynEGA$population$network

colnames(network) <- labels
rownames(network) <- labels

wc <- dynega.1$dynEGA$population$wc

names(wc) <- labels

net.scores.1 <- net.scores(net.data, network, wc)

results <- as.data.frame(cbind(net.scores.0$scores$std.scores, simulation_1$id, simulation_1$dP, simulation_1$dN))

colnames(results) <- c('D1', 'D2', 'id', 'dP', 'dN')


ids <- unique(results$id)

correlations <- matrix(0, nrow = length(ids), ncol = 4)

for (id in ids){
  correlations[id,1] <- cor(results[results$id==id,]$D1, results[results$id==id,]$dN)
  correlations[id,2] <- cor(results[results$id==id,]$D2, results[results$id==id,]$dP)
  correlations[id,3] <- cor(results[results$id==id,]$D1, results[results$id==id,]$dP)
  correlations[id,4] <- cor(results[results$id==id,]$D2, results[results$id==id,]$dN)
}


## EII bootstrap test

EII.test.0 <- EGAnet::boot.ergoInfo(dynega.0, use = 'weighted')

EII.test.0

EII.test.1 <- EGAnet::boot.ergoInfo(dynega.1, use = 'weighted')

EII.test.1


# Emotion dynamics scenarios

window_size <- 5
sd_observable <- 0.05
dt <- 0.25
num_steps <- 300
num_observables <- 4

# Scenario 1: Neutral resistant politician (boring speech)

## Parameters

eta_n <- -0.1
zeta_n <- -2
eta <- -1
zeta <- -0.1
sigma_q <- 0.7

sim_results <- simulate_video(dt, num_steps, num_observables,
                              eta_n, zeta_n, eta, zeta, sigma_q,
                              sd_observable, window_size)

sim_results <- as.data.frame( apply(sim_results[,1:6], 2, calculate_moving_average, 2))


png("Figures/example-neutral-stable.png", width = 2000, height = 1500, res = 300)
plot_emotions(sim_results, 'latent', title = 'Population-level dynamics: Stable Neutral Expression')
rect(80, 0.4, 215, 0.6, col = "white")
text(150, 0.5, "eta_n = -0.1 zeta_n = -2 \n eta = -1 zeta = -0.1", cex = 1)
dev.off()

# Scenario 2: Mostly Neutral with brief emotional episodes

## Parameters

eta_n <- -0.5
zeta_n <- -1
eta <- -0.6
zeta <- -1
sigma_q <- 1.0
sd_observable <- 0.1

sim_results <- simulate_video(dt, num_steps, num_observables,
                              eta_n, zeta_n, eta, zeta, sigma_q,
                              sd_observable, window_size)

sim_results_obs <- emphasize(sim_results, k = 100)

sim_results <- as.data.frame( apply(sim_results, 2, calculate_moving_average, 5))

png("Figures/example-neutral-volatile.png", width = 2000, height = 1500, res = 300)
plot_emotions(sim_results, 'latent', title = 'Population-level dynamics: Volatile Neutral Expression')
rect(80, 0.8, 210, 0.95, col = "white")
text(150, 0.87, "eta_n = -0.5 zeta_n = -1 \n eta = -0.6 zeta = -1", cex = 1)
dev.off()

png("Figures/example-neutral-volatile-pos.png", width = 2000, height = 1500, res = 300)
plot_emotions(sim_results, 'positive', title = 'Population-level dynamics: Volatile Emotional Expressions')
dev.off()


# Scenario 3: Emotional with long, stable emotional episodes

## Parameters

eta_n <- -0.5
zeta_n <- -0.1
eta <- -0.1
zeta <- -1
sigma_q <- 0.7

sim_results <- simulate_video(dt, num_steps, num_observables,
                              eta_n, zeta_n, eta, zeta, sigma_q,
                              sd_observable, window_size)

sim_results_obs <- emphasize(sim_results, k = 100)

sim_results <- as.data.frame( apply(sim_results, 2, calculate_moving_average, 5))

png("Figures/example-emotional-stable.png", width = 2000, height = 1500, res = 300)
plot_emotions(sim_results, 'latent', title = 'Population-level dynamics: Relatively Stable Emotional Expression')
rect(160, 0.8, 280, 0.99, col = "white")
text(220, 0.9, "eta_n = -0.5 zeta_n = -0.1 \n eta = -0.1 zeta = -1", cex = 1)
dev.off()

png("Figures/example-emotional-stable-pos.png", width = 2000, height = 1500, res = 300)
plot_emotions(sim_results, 'positive', title = 'Population-level dynamics: Volatile Emotional Expressions')
dev.off()

png("Figures/example-emotional-stable-neg.png", width = 2000, height = 1500, res = 300)
plot_emotions(sim_results, 'negative', title = 'Population-level dynamics: Volatile Emotional Expressions')
dev.off()




# Scenario 4: Emotional with short, volatile emotional episodes

## Parameters

eta_n <- -1
zeta_n <- 1
eta <- -0.75
zeta <- 1
sigma_q <- 0.7

sim_results <- simulate_video(dt, num_steps, num_observables,
                              eta_n, zeta_n, eta, zeta, sigma_q,
                              sd_observable, window_size)

sim_results_obs <- emphasize(sim_results, k = 100)

sim_results <- as.data.frame( apply(sim_results, 2, calculate_moving_average, 5))

png("Figures/example-emotional-volatile.png", width = 2000, height = 1500, res = 300)
plot_emotions(sim_results, 'latent', title = 'Population-level dynamics: Volatile Emotional Expressions')
rect(170, 0.8, 280, 0.99, col = "white")
text(220, 0.9, "eta_n = -1 zeta_n = 1 \n eta = -0.75 zeta = 1", cex = 1)
dev.off()

png("Figures/example-emotional-volatile-pos.png", width = 2000, height = 1500, res = 300)
plot_emotions(sim_results, 'positive', title = 'Population-level dynamics: Volatile Emotional Expressions')
dev.off()

png("Figures/example-emotional-volatile-neg.png", width = 2000, height = 1500, res = 300)
plot_emotions(sim_results, 'negative', title = 'Population-level dynamics: Volatile Emotional Expressions')
dev.off()



