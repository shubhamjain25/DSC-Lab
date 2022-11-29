install.packages(factoextra)

library("dplyr")
library("ggplot2")
library("ggfortify")

#For fviz_nbclust()
library("factoextra")

data(iris)
head(iris)
print(iris)

print("YES")

data <- select(iris, c(1 : 4))
print(data)


wssplot <- function(data, nc=15, seed=1234){
  
  wss <- (nrow(data)-1) * sum(apply(data,2,var))
  
  for(i in 2:nc){
    set.seed(1234)
    wss[i] <- sum(kmeans(data,i)$withinss)
  }
  
  plot(1:nc, wss, type='b')
  
}

wssplot(data)

wssplot <- function(data, nc = 15, seed = 1234) {
  
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  
  for(i in 2 : nc) {
    set.seed(1234)
    
    wss[i] <- sum(kmeans(data, i)$withinss)
  }
  
  plot(1 : nc, wss, type = "b", xlab = "Number of clusters", ylab = "Within groups sum of squares")
  print(wss)
}

wssplot(data)

fviz_nbclust(data, kmeans, method = "wss") + geom_vline(xintercept = 3, linetype = 1)

clf <- kmeans(data, 2)
print(clf)
print(clf$centers)
autoplot(clf, data, frame = "TRUE")

