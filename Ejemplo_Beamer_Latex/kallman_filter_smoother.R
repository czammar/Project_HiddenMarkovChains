kalman = function(y, F, G, Q, H, R, mu0, Sigma0){
  dy = nrow(y)
  T = ncol(y)
  dx = length(mu0)
  I = diag(dx)
  
  ## INITIALIZATION ##
  
  mu.p = matrix(0, nrow = dx, ncol = T)
  Sigma.p = array(0, c(dx, dx, T))
  mu.f = matrix(0, nrow = dx, ncol = T)
  Sigma.f = array(0, c(dx, dx, T))
  mu.s = matrix(0, nrow = dx, ncol = T)
  Sigma.s = array(0, c(dx, dx, T))
  
  ## FORWARD RECURSION ## Time 1
  mu.p[, 1] = F %*% mu0
  Sigma.p[, , 1] = F %*% Sigma0 %*% t(F) + G %*% Q %*% t(G)
  
  nu = y[, 1] - H %*% mu.p[, 1]
  S = H %*% Sigma.p[, , 1] %*% t(H) + R
  K = Sigma.p[, , 1] %*% t(H) %*% solve(S)
  mu.f[, 1] = mu.p[, 1] + K %*% nu
  Sigma.f[, , 1] = (I - K %*% H) %*% Sigma.p[, , 1]
  
  # Time 2:T
  
  for (t in (2:T)){
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
  
  mu.s[, T] = mu.f[, T]
  Sigma.s[, , T] = Sigma.f[, , T]
  for (t in (T - 1):1){
    J = Sigma.f[, , t] %*% t(F) %*% solve(Sigma.p[, , t + 1])
    mu.s[, t] = mu.f[, t] + J %*% (mu.s[, t + 1] - mu.p[, t + 1])
    Sigma.s[, , t] = Sigma.f[, , t] + J %*% (Sigma.s[, , t + 1] - Sigma.p[,, t + 1]) %*% t(J)
  }
  return(list(mu.f = mu.f, Sigma.f = Sigma.f, mu.p = mu.p, Sigma.p = Sigma.p,mu.s = mu.s, Sigma.s = Sigma.s))
  }