 uclideanDistance <- function(u, v)
 {
 sqrt(sum(u - v)^2)
 }
 sortObjectByDist <- function(xl, z, metricFunction = euclideanDistance)
 {
 l <- dim(xl)[1]
 n <- dim(xl)[2] - 1
 distances <- rep(0, l)
 for (i in 1:l)
 {
 distances[i] <- c(metricFunction(xl[i, 1:n], z))
 }
 orderedXl <- xl[order(distances), ]
   return (orderedXl)
 }
 ONN<-function(xl, z)
 {
 orderedXl<- sortObjectByDist(xl, z)
 n<-dim(orderedXl)[2]
 classes <- orderedXl[1:1, n]
 return (classes)
 }
res<-sample(c(1:150),30,replace=TRUE)
 colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
 plot(iris[res, 3:4], pch = 21, bg = colors[iris$Species[res]], col = colors[iris$Species[res]],asp=1)
 z <- c(0, 0)
 xl <- iris[res, 3:5]
 points(z[1], z[2], pch = 22, bg = colors[1])

 for(i in seq(0, 7, 0.1)){
  for(j in seq(0, 2.5, 0.1))
   {
     z <- c(i, j)
     class <- ONN(xl, z)
     points(z[1], z[2], pch = 1, bg = colors[class],col=colors[class])
 }
 }

 
