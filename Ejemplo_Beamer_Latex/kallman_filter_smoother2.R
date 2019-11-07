library(tidyverse)

kalman<-function(y, F, G, Q, H, R, mu0, Sigma0){
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
set.seed(8675301) # Instance a seed for replication
y = matrix(x + rnorm(T0, sd = sqrt(R)), nrow = 1, ncol = T0)
results.KF = kalman(y, F0, G, Q, H, R, mu0, Sigma0)
mu.f = results.KF$mu.f
Sigma.f = results.KF$Sigma.f
mu.p = results.KF$mu.p
Sigma.p = results.KF$Sigma.p
mu.s = results.KF$mu.s
Sigma.s = results.KF$Sigma.s

# Find multiplier for 99% CI
z.val <- qnorm(1 - (1 - 0.99)/2)

# aux variables to plot CIs
LoCI_f <- mu.f - z.val * sqrt(Sigma.f[1,1,])
HiCI_f <- mu.f + z.val * sqrt(Sigma.f[1,1,])
LoCI_s <- mu.s - z.val * sqrt(Sigma.s[1,1,])
HiCI_s <- mu.s + z.val * sqrt(Sigma.s[1,1,])
LoCI_p <- mu.p - z.val * sqrt(Sigma.p[1,1,])
HiCI_p <- mu.p + z.val * sqrt(Sigma.p[1,1,])

# 
data_toplot <- tibble(x[1,],y[1,],mu.f[1,],LoCI_f[1,],HiCI_f[1,],mu.s[1,],LoCI_s[1,],HiCI_s[1,],mu.p[1,],LoCI_p[1,],HiCI_p[1,])
colnames(data_toplot) <- c("hidden_var","observation","mu.f","LoCI_f","HiCI_f","mu.s","LoCI_s","HiCI_s","mu.p","LoCI_p","HiCI_p")

# Filtering

## Hidden var vs filtering mean
ggplot(data_toplot,aes(x=1:50,y=data_toplot$observation))+ 
  geom_point(aes(1:50,data_toplot$observation, color = 'Hidden variable'))+ 
  geom_point(aes(1:50,mu.f, color = 'Filtering mean'))+ 
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  geom_smooth(data = data_toplot ,aes(y=mu.f, ymin = LoCI_f, ymax = HiCI_f),stat = "identity",linetype=0)+
  labs(x="Time", y = "Values") + # Etiquetas de los ejes
  ggtitle("Filtering mean and 99% credible intervals over time")+ # Titulo del plot
  theme_minimal()

## Hidden var vs filtering mean

ggplot(data_toplot,aes(x=1:50,y=data_toplot$hidden_var))+ 
  geom_point(aes(1:50,data_toplot$hidden_var, color = 'Hidden variable'))+ 
  geom_point(aes(1:50,mu.f, color = 'Filtering mean'))+ 
  scale_color_manual(values = c("#00AFBB", "black"))+
  geom_smooth(data = data_toplot ,aes(y=mu.f, ymin = LoCI_f, ymax = HiCI_f),stat = "identity",linetype=0,alpha=0.15)+
  labs(x="Time", y = "Values",color="Data") + # Etiquetas de los ejes
  #ggtitle("Hidden variable vs Filtering mean and 99% credible \n intervals over time")+ # Titulo del plot
  theme_minimal()

## Observations vs filtering mean

ggplot(data_toplot,aes(x=1:50,y=data_toplot$observation))+ 
  geom_point(aes(1:50,data_toplot$observation, color = 'Observations'))+ 
  geom_point(aes(1:50,mu.f, color = 'Filtering mean'))+ 
  scale_color_manual(values = c("#00AFBB", "red"))+
  geom_smooth(data = data_toplot ,aes(y=mu.f, ymin = LoCI_f, ymax = HiCI_f),stat = "identity",linetype=0,alpha=0.15)+
  labs(x="Time", y = "Values",color="Data") + # Etiquetas de los ejes
  #ggtitle("Observations vs Filtering mean and 99% credible \n intervals over time")+ # Titulo del plot
  theme_minimal()


## Hidden var vs smoothing mean

ggplot(data_toplot,aes(x=1:50,y=data_toplot$hidden_var))+ 
  geom_point(aes(1:50,data_toplot$hidden_var, color = 'Hidden variable'))+ 
  geom_point(aes(1:50,mu.s, color = 'Filtering mean'))+ 
  scale_color_manual(values = c("#00AFBB", "green"))+
  geom_smooth(data = data_toplot ,aes(y=mu.s, ymin = LoCI_s, ymax = HiCI_s),stat = "identity",linetype=0,alpha=0.15)+
  labs(x="Time", y = "Values",color="Data") + # Etiquetas de los ejes
  #ggtitle("Hidden variable vs Filtering mean and 99% credible \n intervals over time")+ # Titulo del plot
  theme_minimal()

## Observations vs smoothing mean

ggplot(data_toplot,aes(x=1:50,y=data_toplot$observation))+ 
  geom_point(aes(1:50,data_toplot$observation, color = 'Observations'))+ 
  geom_point(aes(1:50,mu.s, color = 'Smoothing mean'))+ 
  scale_color_manual(values = c("#00AFBB", "green"))+
  geom_smooth(data = data_toplot ,aes(y=mu.s, ymin = LoCI_s, ymax = HiCI_s),stat = "identity",linetype=0,alpha=0.15)+
  labs(x="Time", y = "Values",color="Data") + # Etiquetas de los ejes
  #ggtitle("Observations vs Smoothing mean and 99% credible \n intervals over time")+ # Titulo del plot
  theme_minimal()