### Introduction to Topological Data Analysis with R 
### Part 6
### Peter Bubenik
### November 2020

#install.packages("TDA") # R TDA package
#install.packages("deldir") # R Delaunay and Voronoi (Dirichlet) package
#install.packages("SparseM") # R Sparse Matrix package used by the next package
#install.packages("e1071",type="binary") # R Support Vector Machines package
#install.packages("FNN") # R Fast Nearest Neighbor search package
library(TDA) 
library(deldir) 
library("Matrix") # package for sparse matrices
#library("SparseM")
library("e1071")
#library("FNN")
par(pty="s") # force the plotting region to be square

# Read MNIST handwritten characters
# Change the following to the folder containing the file MNIST_train_1000.csv
setwd("~/courses/4930/")
MNIST.train <- read.csv("MNIST_train_1000.csv")

###############################
########## Functions ##########
###############################

# Euclidean distance between vectors
euclidean.distance <- function(u, v) sqrt(sum((u - v) ^ 2))

# Sample points from an annulus
#sample.annulus <- function(num.pts, inner.radius, outer.radius){
#  theta <- runif(num.pts) * 2 * pi
#  radius <- sqrt(runif(num.pts, inner.radius^2, outer.radius^2))
#  x <- radius * cos(theta)
#  y <- radius * sin(theta)
#  cbind(x,y)
#}

# Plot the Voronoi cells and dual and Delaunay complex
plot.delaunay <- function(X){
  DelVor <- deldir(X[,1], X[,2], suppressMsge = TRUE)
  # Voronoi cells:
  plot(DelVor, pch=20, cmpnt_col=c("black","red","blue"), wlines= ("tess"))
  # Voronoi cells and their dual (the Delaunay complex):
  plot(DelVor, pch=20, cmpnt_col=c("black","red","blue"), wlines= ("both"))
  # Delaunay complex:
  plot(DelVor, pch=20, cmpnt_col=c("black","red","blue"), wlines= ("triang"))
}

# Plot the Vietoris-Rips complex up to some filtration value
#plot.rips <- function(X,max.filtration){
#  plot(X, pch=20, col="blue", asp=1)
#  num.pts <- dim(X)[1]
#  for(i in 1:num.pts)
#    for(j in 1:num.pts)
#      if (euclidean.distance(X[i,],X[j,]) < max.filtration)
#        lines(rbind(X[i,],X[j,]))
#}

# Plot representative cycles for Delaunay complex
plot.delaunay.cycle <- function(X){
  PH.output <- alphaComplexDiag(X, maxdimension = 1, library = c("GUDHI", "Dionysus"), 
                                location = TRUE)
  PD <- PH.output[["diagram"]]
  ones <- which(PD[, 1] == 1)
  persistence <- PD[ones,3] - PD[ones,2]
  cycles <- PH.output[["cycleLocation"]][ones[order(persistence)]]
  for (i in 1:length(cycles)){
    plot(X, pch=20, col="blue", asp=1)
    for (j in 1:dim(cycles[[i]])[1])
      lines(cycles[[i]][j,,])
  }
}

### Plot representative cycles for Vietoris-Rips complex
#plot.rips.cycle <- function(X){
#  PH.output <- ripsDiag(X, maxdimension = 1, maxscale = max.filtration, 
#                        library = c("GUDHI", "Dionysus"), location = TRUE)
#  PD <- PH.output[["diagram"]]
#  ones <- which(PD[, 1] == 1)
#  persistence <- PD[ones,3] - PD[ones,2]
#  cycles <- PH.output[["cycleLocation"]][ones[order(persistence)]]
#  for (i in 1:length(cycles)){
#    plot(X, pch=20, col="blue", asp=1)
#    for (j in 1:dim(cycles[[i]])[1])
#      lines(cycles[[i]][j,,])
#  }
#}

# Death Vector
death.vector <- function(PD){
  zeroes <- which(PD[, 1] == 0)
  PD.0 <- PD[zeroes,2:3]
  dv <- vector()
  if ((min(PD.0[,"Birth"]) == 0) && (max(PD.0[,"Birth"]) == 0))
    dv <- sort(PD.0[,2], decreasing=TRUE)
  return(dv)
}

# Plot Persistence Landscape
plot.landscape <- function(PL,t.vals){
  plot(t.vals,PL[1,],type="l",ylab="Persistence",xlab="Parameter values",col=1,ylim=c(min(PL),max(PL)))
  for(i in 2:dim(PL)[1])
    lines(t.vals,PL[i,],type="l",col=i)
}

# Matrix of death vectors from a list of persistence diagrams
death.vector.matrix <- function(PD.list){
  num.pts <- length(which(PD.list[[1]][,1] == 0))
  DVM <- matrix(0L, nrow = length(PD.list), ncol = num.pts - 1)
  for (c in 1 : length(PD.list))
    DVM[c,] <- death.vector(PD.list[[c]])[-1]
  return(DVM)
}

# Matrix of persistence landscape row vectors from list of persistence landscapes
landscape.matrix.from.list <- function(PL.list){
  n <- length(PL.list)
  m <- ncol(PL.list[[1]])
  max.depth <- integer(n)
  for (i in 1:n)
    max.depth[i] <- nrow(PL.list[[i]])
  K <- max(max.depth)
  PL.matrix <- Matrix(0, nrow = n, ncol = m*K, sparse = TRUE)
  for (i in 1:n)
    for (j in 1:max.depth[i])
      PL.matrix[i,(1+(j-1)*m):(j*m)] <- PL.list[[i]][j,]
  return(PL.matrix)
}

# Convert a vector to a persistence landscape
landscape.from.vector <- function(PL.vector, t.vals){
  m <- length(t.vals)
  K <- length(PL.vector)/m
  PL <- Matrix(0, nrow = K, ncol=m, sparse = TRUE)
  for (i in 1:K){
    PL[i,1:m] <- PL.vector[(1+(i-1)*m):(i*m)]
  }
  return(PL)
}

# Take difference of vectors of potentially different lengths
difference.vectors <-function(vector.1,vector.2){
  length.1 <- length(vector.1)
  length.2 <- length(vector.2)
  difference.vector = numeric(max(length.1,length.2))
  difference.vector = as(difference.vector, "sparseVector")
  difference.vector[1:length.1] = difference.vector[1:length.1] + vector.1
  difference.vector[1:length.2] = difference.vector[1:length.2] - vector.2
}

# Permutation test for two matrices consisting of row vectors
permutation.test <- function(M1 ,M2, num.repeats = 10000){
  # append zeros if necessary so that the matrices have the same number of columns
  num.columns <- max(ncol(M1),ncol(M2))
  M1 <- cbind(M1, Matrix(0,nrow=nrow(M1),ncol=num.columns-ncol(M1)))
  M2 <- cbind(M2, Matrix(0,nrow=nrow(M2),ncol=num.columns-ncol(M2)))
  t.obs <- euclidean.distance(colMeans(M1),colMeans(M2))
  k <- dim(M1)[1]
  M <- rbind(M1,M2)
  n <- dim(M)[1]
  count <- 0
  for (i in 1:num.repeats){
    permutation <- sample(1:n)
    t <- euclidean.distance(colMeans(M[permutation[1:k],]),colMeans(M[permutation[(k+1):n],]))
    if (t >= t.obs)
      count <- count + 1
  }
  return(count/num.repeats)
}

sample.digital.image <- function(gray.scale.matrix, num.pts=100){
  V <- as.vector(t(gray.scale.matrix))
  W <- integer(length=length(V))
  W[1] <- V[1]
  for (i in 2:length(V))
    W[i] <- W[i-1] + V[i]
  X <- matrix(0,nrow=num.pts,ncol=2)
  for (i in 1:num.pts){
    r <- runif(1)*W[length(W)]
    A <- W<r
    entry <- sum(A)+1
    b <- entry %/% 28
    a <- entry %% 28
    X[i,1] <- (a + runif(1))/29
    X[i,2] <- 1 - (b + runif(1))/29
  }
  return(X)
}

################################
########## Parameters ##########
################################

num.pts <- 200
num.repeats <- 50
t.steps <- 200

############################
########## Script ##########
############################

# choose a character
i <- 31
#i <- sample(1:dim(MNIST.train)[1],1)
character_label <- MNIST.train[i,1]
character_bitmap <- MNIST.train[i,-1]
V <- as.numeric(character_bitmap)
# plot the character
M <- matrix(as.numeric(character_bitmap),nrow=28,ncol=28,byrow=TRUE)
image(t(M[ncol(M):1,]),col=grey(seq(1, 0, length = 256)), axes=FALSE)
# coordinates of the pixels
pixels <- matrix(0,nrow=length(V),2)
for (i in 1:length(V))
  pixels[i,] <- c(1 + (i-1) %/% 28, 1 + (i-1) %% 28)

#Distance to Measure
#DTM.vector <- dtm(X = pixels, Grid = pixels, m0 = 0.1, weight = V)
#DTM.matrix <- matrix(DTM.vector,nrow=28,ncol=28,byrow=TRUE)
#image(t(DTM.matrix[ncol(M):1,]), col=grey(seq(0, 1, length = 100)), axes=FALSE)

# compute sublevel persistent homology of the cubical complex using negative pixel values
min.t <- -255
max.t <- 0
t.vals <- seq(min.t,max.t,(max.t-min.t)/t.steps)
PH.output <- gridDiag(X = pixels, FUNvalues = -M, sublevel = TRUE)
PD <- PH.output[["diagram"]]
plot(PD, asp=1, diagLim = c(min.t,max.t))
legend(0.5*(max.t+min.t), 0.75*min.t+0.25*max.t, c("Homology in degree 0","Homology in degree 1"), 
       col = c(1,2), pch = c(19,2), cex = .8, pt.lwd = 2)
plot(PD, diagLim = c(min.t,max.t), barcode=TRUE)
# Persistence Landscapes in dimensions 0 and 1
PL.0 <- t(landscape(PD,dimension=0,KK=1:100,tseq=t.vals))
plot.landscape(PL.0,t.vals)
PL.1 <- t(landscape(PD,dimension=1,KK=1:100,tseq=t.vals))
plot.landscape(PL.1,t.vals)

# sample points from the character
X <- sample.digital.image(M,num.pts)
# plot the sampled points
plot(X,xlim=c(0,1),ylim=c(0,1),axes=FALSE,xlab="",ylab="",pch=20,col="blue",asp=1)

# Compute persistent homology of the Delaunay complex on the points
min.t<- 0
max.t <- 0.15
t.vals <- seq(min.t,max.t,(max.t-min.t)/t.steps)
plot.delaunay(X)
PH.output <- alphaComplexDiag(X)
PD <- PH.output[["diagram"]]
PD[,2:3] <- sqrt(PD[,2:3])
plot(PD, asp=1, diagLim = c(0,max.t))
legend(0.5*max.t, 0.25*max.t, c("Homology in degree 0","Homology in degree 1"), 
       col = c(1,2), pch = c(19,2), cex = .8, pt.lwd = 2)
plot(PD, diagLim = c(0,max.t), barcode=TRUE)
#plot.delaunay.cycle(X)
# Death Vector
DV <- death.vector(PD)
DVr <- DV[-1]
plot(DVr, type="l", col="blue", ylab="Persistence")
# Persistence Landscapes in dimensions 1
PL.1 <- t(landscape(PD,dimension=1,KK=1:100,tseq=t.vals))
plot.landscape(PL.1,t.vals)


# Compare two classes of digits

MNIST.character.A <- 8
MNIST.character.B <- 3
MNIST.train.A <- MNIST.train[MNIST.train[,1]==MNIST.character.A,]
MNIST.train.B <- MNIST.train[MNIST.train[,1]==MNIST.character.B,]
digits.A <- MNIST.train.A[1:num.repeats,]
digits.B <- MNIST.train.B[1:num.repeats,]

# First method: use sublevel persistent homology on a cubical complex using negative pixel values
min.t <- -255
max.t <- 0
t.vals <- seq(min.t,max.t,(max.t-min.t)/t.steps)

# First class of samples
PD.list <- vector("list",num.repeats)
for (c in 1 : num.repeats){
  character_bitmap <- digits.A[c,-1]
  V <- as.numeric(character_bitmap)
  M <- matrix(as.numeric(character_bitmap),nrow=28,ncol=28,byrow=TRUE)
  PH.output <- gridDiag(X = pixels, FUNvalues = -M, sublevel = TRUE)
  PD.list[[c]] <- PH.output[["diagram"]]
}

PL.list.A.0 <- vector("list",num.repeats)
for (c in 1 : num.repeats)
  PL.list.A.0[[c]] <- t(landscape(PD.list[[c]],dimension=0,KK=1:100,tseq=t.vals))
PL.matrix.A.0 <- landscape.matrix.from.list(PL.list.A.0)
average.PL.vector.A.0 <- colMeans(PL.matrix.A.0, sparseResult = TRUE)
average.PL.A.0 <- landscape.from.vector(average.PL.vector.A.0,t.vals)
plot.landscape(average.PL.A.0,t.vals)

PL.list.A.1 <- vector("list",num.repeats)
for (c in 1 : num.repeats)
  PL.list.A.1[[c]] <- t(landscape(PD.list[[c]],dimension=1,KK=1:100,tseq=t.vals))
PL.matrix.A.1 <- landscape.matrix.from.list(PL.list.A.1)
average.PL.vector.A.1 <- colMeans(PL.matrix.A.1, sparseResult = TRUE)
average.PL.A.1 <- landscape.from.vector(average.PL.vector.A.1,t.vals)
plot.landscape(average.PL.A.1,t.vals)

# Second class of samples
PD.list.2 <- vector("list",num.repeats)
for (c in 1 : num.repeats){
  character_bitmap <- digits.B[c,-1]
  V <- as.numeric(character_bitmap)
  M <- matrix(as.numeric(character_bitmap),nrow=28,ncol=28,byrow=TRUE)
  PH.output <- gridDiag(X = pixels, FUNvalues = -M, sublevel = TRUE)
  PD.list.2[[c]] <- PH.output[["diagram"]]
}

PL.list.B.0 <- vector("list",num.repeats)
for (c in 1 : num.repeats)
  PL.list.B.0[[c]] <- t(landscape(PD.list.2[[c]],dimension=0,KK=1:100,tseq=t.vals))
PL.matrix.B.0 <- landscape.matrix.from.list(PL.list.B.0)
average.PL.vector.B.0 <- colMeans(PL.matrix.B.0, sparseResult = TRUE)
average.PL.B.0 <- landscape.from.vector(average.PL.vector.B.0,t.vals)
plot.landscape(average.PL.B.0,t.vals)

PL.list.B.1 <- vector("list",num.repeats)
for (c in 1 : num.repeats)
  PL.list.B.1[[c]] <- t(landscape(PD.list.2[[c]],dimension=1,KK=1:100,tseq=t.vals))
PL.matrix.B.1 <- landscape.matrix.from.list(PL.list.B.1)
average.PL.vector.B.1 <- colMeans(PL.matrix.B.1, sparseResult = TRUE)
average.PL.B.1 <- landscape.from.vector(average.PL.vector.B.1,t.vals)
plot.landscape(average.PL.B.1,t.vals)

# The difference in death vectors and persistence landscapes
difference.average.PL.vector.0 <- difference.vectors(average.PL.vector.A.0,average.PL.vector.B.0)
difference.average.PL.0 <- landscape.from.vector(difference.average.PL.vector.0,t.vals)
plot.landscape(difference.average.PL.0,t.vals)
difference.average.PL.vector.1 <- difference.vectors(average.PL.vector.A.1,average.PL.vector.B.1)
difference.average.PL.1 <- landscape.from.vector(difference.average.PL.vector.1,t.vals)
plot.landscape(difference.average.PL.1,t.vals)

# p values for differences in the average landscapes
permutation.test(PL.matrix.A.0, PL.matrix.B.0, num.repeats = 1000)
permutation.test(PL.matrix.A.1, PL.matrix.B.1, num.repeats = 1000)

# Assemble the data
data.labels <- c(rep(1,nrow(PL.matrix.A.1)), rep(2,nrow(PL.matrix.B.1)))
PL.vectors.0 <- rbind(PL.matrix.A.0,PL.matrix.B.0)
PL.vectors.1 <- rbind(PL.matrix.A.1,PL.matrix.B.1)

# Principal Components Analysis
pca.0 <- prcomp(PL.vectors.0,rank=10)
plot(pca.0,type="l")
plot(pca.0$x[,1:2], col=data.labels, pch=17+(2*data.labels), asp=1)
loading_vectors <- t(pca.0$rotation)
plot.landscape(landscape.from.vector(loading_vectors[1,],t.vals),t.vals)
plot.landscape(landscape.from.vector(loading_vectors[2,],t.vals),t.vals)
pca.1 <- prcomp(PL.vectors.1,rank=10)
plot(pca.1,type="l")
plot(pca.1$x[,1:2], col=data.labels, pch=17+(2*data.labels), asp=1)
loading_vectors <- t(pca.1$rotation)
plot.landscape(landscape.from.vector(loading_vectors[1,],t.vals),t.vals)
plot.landscape(landscape.from.vector(loading_vectors[2,],t.vals),t.vals)

# Support Vector Machine Classification
cost <- 10
num.folds <- 5
svm_model <- svm(PL.vectors.0,data.labels,scale=FALSE,type="C-classification",kernel="linear",cost=cost,cross=num.folds)
summary(svm_model)
svm_model <- svm(PL.vectors.1,data.labels,scale=FALSE,type="C-classification",kernel="linear",cost=cost,cross=num.folds)
summary(svm_model)

# Second method: use persistent homology of Delaunay complex on sampled points
min.t<- 0
max.t <- 0.15
t.vals <- seq(min.t,max.t,(max.t-min.t)/t.steps)

# First class of samples
PD.list <- vector("list",num.repeats)
for (c in 1 : num.repeats){
  character_bitmap <- digits.A[c,-1]
  V <- as.numeric(character_bitmap)
  M <- matrix(as.numeric(character_bitmap),nrow=28,ncol=28,byrow=TRUE)
  X <- sample.digital.image(M,num.pts)
  PH.output <- alphaComplexDiag(X)
  PD.list[[c]] <- PH.output[["diagram"]]
  PD.list[[c]][,2:3] <- sqrt(PD.list[[c]][,2:3])
}

DV.matrix <- death.vector.matrix(PD.list)
average.DV <- colMeans(DV.matrix)
plot(average.DV, type="l", col="blue", ylab = "Persistence")

PL.list <- vector("list",num.repeats)
for (c in 1 : num.repeats)
  PL.list[[c]] <- t(landscape(PD.list[[c]],dimension=1,KK=1:100,tseq=t.vals))
PL.matrix <- landscape.matrix.from.list(PL.list)
average.PL.vector <- colMeans(PL.matrix, sparseResult = TRUE)
average.PL <- landscape.from.vector(average.PL.vector,t.vals)
plot.landscape(average.PL,t.vals)

# Second class of samples
PD.list.2 <- vector("list",num.repeats)
for (c in 1 : num.repeats){
  character_bitmap <- digits.B[c,-1]
  V <- as.numeric(character_bitmap)
  M <- matrix(as.numeric(character_bitmap),nrow=28,ncol=28,byrow=TRUE)
  X <- sample.digital.image(M,num.pts)
  PH.output <- alphaComplexDiag(X)
  PD.list.2[[c]] <- PH.output[["diagram"]]
  PD.list.2[[c]][,2:3] <- sqrt(PD.list.2[[c]][,2:3])
}

DV.matrix.2 <- death.vector.matrix(PD.list.2)
average.DV.2 <- colMeans(DV.matrix.2)
plot(average.DV.2, type="l", col="blue", ylab = "Persistence")

PL.list.2 <- vector("list",num.repeats)
for (c in 1 : num.repeats)
  PL.list.2[[c]] <- t(landscape(PD.list.2[[c]],dimension=1,KK=1:100,tseq=t.vals))
PL.matrix.2 <- landscape.matrix.from.list(PL.list.2)
average.PL.vector.2 <- colMeans(PL.matrix.2, sparseResult = TRUE)
average.PL.2 <- landscape.from.vector(average.PL.vector.2,t.vals)
plot.landscape(average.PL.2,t.vals)

# The difference in death vectors and persistence landscapes
difference.average.DV <- average.DV - average.DV.2
plot(difference.average.DV, type="l", col="blue", ylab="Persistence")
difference.average.PL.vector <- difference.vectors(average.PL.vector,average.PL.vector.2)
difference.average.PL <- landscape.from.vector(difference.average.PL.vector,t.vals)
plot.landscape(difference.average.PL,t.vals)

# p values for differences in the average landscapes
permutation.test(DV.matrix, DV.matrix.2, num.repeats = 1000)
permutation.test(PL.matrix, PL.matrix.2, num.repeats = 1000)

# Assemble the data
data.labels <- c(rep(1,nrow(PL.matrix)), rep(2,nrow(PL.matrix.2)))
DV.vectors <- rbind(DV.matrix,DV.matrix.2)
PL.vectors <- rbind(PL.matrix,PL.matrix.2)

# Principal Components Analysis
pca.0 <- prcomp(DV.vectors,rank=10)
plot(pca.0,type="l")
plot(pca.0$x[,1:2], col=data.labels, pch=17+(2*data.labels), asp=1)
loading_vectors <- t(pca.0$rotation)
plot(loading_vectors[1,],type="l")
plot(loading_vectors[2,],type="l")
pca.1 <- prcomp(PL.vectors,rank=10)
plot(pca.1,type="l")
plot(pca.1$x[,1:2], col=data.labels, pch=17+(2*data.labels), asp=1)
loading_vectors <- t(pca.1$rotation)
plot.landscape(landscape.from.vector(loading_vectors[1,],t.vals),t.vals)
plot.landscape(landscape.from.vector(loading_vectors[2,],t.vals),t.vals)

# Support Vector Machine Classification
cost <- 10
num.folds <- 5
svm_model <- svm(DV.vectors,data.labels,scale=FALSE,type="C-classification",kernel="linear",cost=cost,cross=num.folds)
summary(svm_model)
svm_model <- svm(PL.vectors,data.labels,scale=FALSE,type="C-classification",kernel="linear",cost=cost,cross=num.folds)
summary(svm_model)

### Exercises
# - interpret the barcode of the persistent homology on the cubical complex on the negative pixel values
#   for the given digit
# - interpret the corresponding persistence landscape
# - note that intervals in the barcode for homology in degree 0 are not born at 0. So we don't 
#   have a death vector but we do have a persistence landscape
# - change Line 204 to choose different digits from the MNIST database, then plot, sample, compute PH, etc
# - do pairwise comparisons of different pairs of digits
# - which pairs can you tell apart and better yet, classify?
# - which pairs can you not tell apart or not classify?


