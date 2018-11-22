euclideanDistance <- function(u, v)                                      
{
  sqrt(sum((u - v)^2))
}

sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance)  
{                                                                                            
  l <- dim(xl)[1]        
  n <- dim(xl)[2] - 1  
  
  distances <- matrix(NA, l, 2)                                                          
  
  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  
  orderedXl <- xl[order(distances[, 2]), ]                                
  
  return (orderedXl);
}

KWNN <- function(xl, z, k, q) {                              
  orderedXl <- sortObjectsByDist(xl, z, euclideanDistance)  
  n <- dim(orderedXl)[2] - 1                                
  classes <- orderedXl[1:k, n + 1]                                        
  counts <- table(classes)                                 
  m <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)   
  for (i in seq(1:k)){                                      
    w <- q ^ i                                              
    m[[classes[i]]] <- m[[classes[i]]] + w                  
  }
 class <- names(which.max(m))                               
  return (class)
}

segment <- seq(from = 0.0, to = 1.0, by = 0.05)             

LOOKWNN <- function(classificator){                      
  j <- 1
  LOO <- seq(1, length(segment))
  
  for (q in segment) {
    Q <- 0                                                  
    for (i in 1:150) {
      x_el <-c(iris[i, 3], iris[i, 4])                      
      x_sample <- iris[-i, 3:5]                             
      class <- classificator(x_sample, x_el, k=6, q) 
      
      if (iris[i, 5] != class) { 
        Q <- Q + 1
      }
    }
    LOO[j] <-Q / dim(iris)[1]
    j <- j + 1
  }
  return (LOO)
}

arr <- LOOKWNN(KWNN)
p = which(arr == min(arr))
arr2 <- (seq(from = 0.0, to = 1.0, by = 0.05))
plot(arr2, arr, type = "l", xlab = "q", ylab="LOO", main = "LOO KWNN")
points(arr2[p], arr[p], pch = 19, col = "red")

