euclide <- function(u, v) sqrt(sum((u-v)^2)) 

core.E <- function(r) (3/4)*(1-r^2)*(abs(r) <= 1) 
core.Q <- function(r) (15/16)*((1 - r^2)^2)*(abs(r) <= 1) 
core.T <- function(r) (1 - abs(r))*(abs(r) <= 1) 
core.G <- function(r) (2*pi)^(-0.5)*exp(-0.5*(r^2)) 
core.P <- function(r) (0.5)*(abs(r) <= 1) 


Distances <- function(xl, z, metricFunction = euclide) {
  
  l <- nrow(xl)
  n <- ncol(xl)
  distances <- rep(0, l)
  for (i in 1:l)
    distances[i] <- metricFunction(xl[i, 1:(n-1)], z)
  return (distances)
}

parzen <- function(xl, h, distances, type_core) {
  
  l <- nrow(xl) 
  n <- ncol(xl) 
  
  classes <- xl[1:l, n] 
  weights <- table(classes) 
  weights[1:length(weights)] <- 0
  
  for (i in 1:l) { 
    class <- xl[i, n] 
    r <- distances[i] / h
    weights[class] <- weights[class] + type_core(r) 
  }
  
  if (max(weights) != 0) 
    return (names(which.max(weights))) 
  else
    return (0) 
}

LOO <- function(xl, type_core) {
  l <- nrow(xl)
  n <- ncol(xl)
  h_temp <- seq(0.1, 2, 0.1)
  sum <- rep(0, length(h_temp))
  
  for (i in 1:l) {
    cnt <- 1
    xi <- xl[i, 1:(n-1)]
    xt <- xl[-i, ]

    distances <- Distances(xt, xi)
    for (h in h_temp) {
      class <- parzen(xt, h, distances, type_core)
      if (class != xl[i, n] || class == 0) {
        sum[cnt] = sum[cnt] + 1/l
      }
      cnt <- cnt + 1
    }
  }
  return (sum)
}

Opt_H <- function(LOO_H) which.min(LOO_H) / 10

ClassMap <- function(xl, h, type_core) {
  l <- nrow(xl)
  n <- ncol(xl)
  ox <- seq(0, 7, 0.1)
  oy <- seq(0, 2.5, 0.1)
  classMatrix <- matrix(NA, length(ox)*length(oy), n)
  cnt <- 1
  for (i in ox)
    for (j in oy) {
      z <- c(i, j)
      distances <- Distances(xl, z)
      class <- parzen(xl, h, distances, type_core)
      if (class != 0) {
        classMatrix[cnt, ] <- c(i, j, class)
        cnt <- cnt + 1
      }
    }
  return (classMatrix)
}

DrowPlots <- function(xl, classMatrix, LOO_H, h) {
  l <- nrow(classMatrix)
  n <- ncol(classMatrix)
  colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
  h10 <- h*10
  par(mfrow=c(1, 2))
  
  
  plot(xl[,1:(n-1)], pch = 21, bg = colors[xl[, n]], col = colors[xl[, n]], main = "Метод парзеновского окна", xlab = "длина лепестка", ylab = "ширина лепестка", asp = 1)
  points(classMatrix[, 1:(n-1)], pch = 1, col = colors[classMatrix[, n]])
  

  plot(seq(0.1, 2, 0.1), LOO_H[1:length(LOO_H)], type = "l", bg = "red", col = "red", main = "Оцена оптимальности  h по LOO", xlab = "h", ylab = "LOO")
  points(h, LOO_H[h10], pch = 21, bg = "blue", col = "blue")
  label <- paste("h = ", h, "\n", "LOO = ", round(LOO_H[h10], 3))
  text(h, LOO_H[h10], labels = label, pos = 3)
}

main <- function(type_core) {
  xl <- iris[, 3:5]
  LOO_H <- LOO(xl, type_core)
  h <- Opt_H(LOO_H)
  classMatrix <- ClassMap(xl, h, type_core)
  DrowPlots(xl, classMatrix, LOO_H, h)
}

main(core.P)
