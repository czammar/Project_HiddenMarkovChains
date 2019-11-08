#Bayes Factor 


#R code: Karla Alfaro and Ana B. Coronel 


library(tidyr)
library(dplyr)
library(ggplot2)

###### Example 1


x <- seq(0, 100, by=1)
datos_1 <- data.frame("x"= x, "type" =dbinom(x, 100, 0.7), "Politician"="A")
datos_2 <- data.frame("x"= x, "type" =dbinom(x, 100, 0.6), "Politician"="B")
datos <- rbind(datos_1, datos_2)
t1.rect1 <- data.frame (xmin=61.6, xmax=62.1, ymin=0, ymax=0.0191)
t1.rect2 <- data.frame (xmin=62, xmax=62.5, ymin=0, ymax=0.0754)


ggplot(datos[datos$type>0.0000001,], aes(x = x, y = type, fill = Politician)) +
  geom_col(position="dodge") + scale_fill_manual(values=c("#ec7474", "#a9c9ee"))+
  xlab('Persons in favor of death penalty') + ylab('Probability') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_blank()) +
  geom_rect(data=t1.rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#781414", alpha=0.8, inherit.aes = FALSE) +
  geom_rect(data=t1.rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#2e69ce", alpha=1, inherit.aes = FALSE)+
  geom_hline(yintercept =0.0754, col="#a9c9ee")+
  geom_hline(yintercept =0.0191, col="#ec7474")

p = seq(0,1, length=100)
plot(p, dbeta(p, 10, 10), ylab="density", type ="l", col=4)
lines(p, dbeta(p, 10, 10), type ="l", col=3)
lines(p, dbeta(p, 2, 2), col=2) 
lines(p, dbeta(p, 1, 1), col=1) 
legend(0.7,8, c("Be(100,100)","Be(10,10)","Be(2,2)", "Be(1,1)"),lty=c(1,1,1,1),col=c(4,3,2,1))


####### Example 4
#Model 1
p_M1 <- data_frame(theta = c(0.25, 0.5, 0.75), prior = c(0.25, 0.5, 0.25), 
                   modelo ="M1")

#Model 2
p = seq(0,1, length=100)

p <- rbeta(100, 4,4)

p_M2 <- data_frame(theta = seq(0, 1, length=100), prior = p/sum(p), 
                   modelo = "M2")

N <- 20 # coins
z <- 12 # heads


dists_h <- bind_rows(p_M1, p_M2) %>% 
  group_by(modelo) %>%
  mutate(
    Like = theta ^ z * (1 - theta) ^ (N - z),
    posterior = (Like * prior) / sum(Like * prior)) 

dists <- dists_h %>% # base de datos larga
  gather(dist, valor, prior, Like, posterior) %>% 
  mutate(dist = factor(dist, levels = c("prior", "Like", "posterior")))

ggplot(filter(dists, modelo == "M1"), aes(x = theta, y = valor)) +
  geom_bar(stat = "identity", fill = "#a9c9ee") +
  facet_wrap(~ dist, scales = "free") +
  scale_x_continuous(expression(theta), breaks = seq(0, 1, 0.2)) +
  labs(y = "")


ggplot(filter(dists, modelo == "M2"), aes(x = theta, y = valor)) +
  geom_bar(stat = "identity", fill = "#a9c9ee") +
  facet_wrap(~ dist, scales = "free") +
  scale_x_continuous(expression(theta), breaks = seq(0, 1, 0.2)) + 
  ylab("")


# Factor Bayes function 
factorBayes <- function(N, z){
  evidencia <- bind_rows(p_M1, p_M2) %>% 
    group_by(modelo) %>%
    mutate(
      Like = theta ^ z * (1 - theta) ^ (N - z), 
      posterior = (Like * prior) / sum(Like * prior)
    ) %>%
    summarise(evidencia = sum(prior * Like))
  print(evidencia)
  return(evidencia[1, 2] / evidencia[2, 2])
}


#### Factor bayes examples
factorBayes(100, 50)
factorBayes(100, 20)
factorBayes(100, 18)

