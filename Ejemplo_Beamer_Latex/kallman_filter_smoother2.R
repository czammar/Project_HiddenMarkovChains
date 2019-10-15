library(tidyverse)

function(y, F, G, Q, H, R, mu0, Sigma0){
  dy = nrow(y)
  T0 = ncol(y)
  dx = length(mu0)
  I = diag(dx)
  
  ## INITIALIZATION ##
  
  mu.p = matrix(0, nrow = dx, ncol = T0)
  Sigma.p = array(0, c(dx, dx, T0))
  mu.f = matrix(0, nrow = dx, ncol = T0)
  Sigma.f = array(0, c(dx, dx, T0))
  mu.s = matrix(0, nrow = dx, ncol = T0)
  Sigma.s = array(0, c(dx, dx, T0))
  
  ## FORWARD RECURSION ## Time 1
  mu.p[, 1] = F %*% mu0
  Sigma.p[, , 1] = F %*% Sigma0 %*% t(F) + G %*% Q %*% t(G)
  
  nu = y[, 1] - H %*% mu.p[, 1]
  S = H %*% Sigma.p[, , 1] %*% t(H) + R
  K = Sigma.p[, , 1] %*% t(H) %*% solve(S)
  mu.f[, 1] = mu.p[, 1] + K %*% nu
  Sigma.f[, , 1] = (I - K %*% H) %*% Sigma.p[, , 1]
  
  # Time 2:T
  
  for (t in (2:T0)){
    # Prediction
    mu.p[, t] = F %*% mu.f[, t - 1]
    Sigma.p[, , t] = F %*% Sigma.f[, , t - 1] %*% t(F) + G %*% Q %*% t(G)
    
    # Update
    nu = y[, t] - H %*% mu.p[, t]
    S = H %*% Sigma.p[, , t] %*% t(H) + R
    K = Sigma.p[, , t] %*% t(H) %*% solve(S)
    mu.f[, t] = mu.p[, t] + K %*% nu
    Sigma.f[, , t] = (I - K %*% H) %*% Sigma.p[, , t]}
  
  ## BACKWARD RECURSION ##
  
  mu.s[, T0] = mu.f[, T0]
  Sigma.s[, , T0] = Sigma.f[, , T0]
  for (t in (T0 - 1):1){
    J = Sigma.f[, , t] %*% t(F) %*% solve(Sigma.p[, , t + 1])
    mu.s[, t] = mu.f[, t] + J %*% (mu.s[, t + 1] - mu.p[, t + 1])
    Sigma.s[, , t] = Sigma.f[, , t] + J %*% (Sigma.s[, , t + 1] - Sigma.p[,, t + 1]) %*% t(J)
  }
  return(list(mu.f = mu.f, Sigma.f = Sigma.f, mu.p = mu.p, Sigma.p = Sigma.p,mu.s = mu.s, Sigma.s = Sigma.s))
}

T0 = 50
x = matrix(cos(c(1:T0)/10), 1, T0)
R = 0.2
mu0 = 0
Sigma0 = 1
G = 1
Q = 0.02
H = 1
F0 = 1
y = matrix(x + rnorm(T0, sd = sqrt(R)), nrow = 1, ncol = T0)
results.KF = kalman(y, F0, G, Q, H, R, mu0, Sigma0)
mu.f = results.KF$mu.f
Sigma.f = results.KF$Sigma.f
mu.p = results.KF$mu.p
mu.s = results.KF$mu.s


ggplot()+ geom_point(aes(1:50,mu.p[1,], color = 'red'))+geom_point(aes(1:50,y[1,], color = 'blue'))
