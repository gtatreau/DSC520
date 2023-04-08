library(ggplot2)
library(class)

setwd("/Users/gillian/Documents/Bellevue Grad Program/Fall 2022/DSC520/DSC520 Repo")

file_name <- "/Users/gillian/Documents/Bellevue Grad Program/Fall 2022/DSC520/DSC520 Repo/binary-classifier-data.csv"
binary <- read.csv(file_name)
head(binary)
binary$label <- as.factor(binary$label)
head(binary)

ggplot(binary, aes(x=x, y=y, shape=label, color=label)) + 
  geom_point()

nor <-function(x){ 
  (x - min(x)) / (max(x) - min(x))
}

set.seed(42)

bi_train <- sample(1:nrow(binary), 0.8 * nrow(binary))

binary_norm <- as.data.frame(lapply(binary[,c(2,3)], nor))

binary_train <- binary_norm[bi_train,] 

binary_test <- binary_norm[-bi_train,] 

binary_train_category <- binary[bi_train, 1]

binary_test_category <- binary[-bi_train, 1]

binary3 <- knn(binary_train, binary_test, cl = binary_train_category, k=3)
binary3

cm3 <- table(binary3, binary_test_category)
cm3

accuracy <- function(x){
  sum(diag(x)/(sum(rowSums(x)))) * 100
}

a3 <- accuracy(cm3)
a3

binary5 <- knn(binary_train, binary_test, cl = binary_train_category, k=5)
binary5
cm5 <- table(binary5, binary_test_category)
cm5
a5 <- accuracy(cm5)
a5

binary10 <- knn(binary_train, binary_test, cl = binary_train_category, k=10)
binary10
cm10 <- table(binary10, binary_test_category)
cm10
a10 <- accuracy(cm10)
a10

binary15 <- knn(binary_train, binary_test, cl = binary_train_category, k=15)
binary15
cm15 <- table(binary15, binary_test_category)
cm15
a15 <- accuracy(cm15)
a15

binary20 <- knn(binary_train, binary_test, cl = binary_train_category, k=20)
binary20
cm20 <- table(binary20, binary_test_category)
cm20
a20 <- accuracy(cm20)
a20

binary25 <- knn(binary_train, binary_test, cl = binary_train_category, k=25)
binary25
cm25 <- table(binary25, binary_test_category)
cm25
a25 <- accuracy(cm25)
a25

k_values <- c(3, 5, 10, 15, 20, 25)
binary_accuracy <- c(a3, a5, a10, a15, a20, a25)

binary_accuracy_df <- data.frame(k_values, binary_accuracy)

ggplot(binary_accuracy_df, aes(x=k_values, y=binary_accuracy)) + 
  geom_point()

file_name2 <- "/Users/gillian/Documents/Bellevue Grad Program/Fall 2022/DSC520/DSC520 Repo/trinary-classifier-data.csv"
trinary <- read.csv(file_name2)
head(trinary)
trinary$label <- as.factor(trinary$label)
head(trinary)

ggplot(trinary, aes(x=x, y=y, shape=label, color=label)) + 
  geom_point()

set.seed(43)
tri_train <- sample(1:nrow(trinary), 0.8 * nrow(trinary))

trinary_norm <- as.data.frame(lapply(trinary[,c(2,3)], nor))

trinary_train <- trinary_norm[tri_train,] 

trinary_test <- trinary_norm[-tri_train,] 

trinary_train_category <- trinary[tri_train, 1]

trinary_test_category <- trinary[-tri_train, 1]

trinary3 <- knn(trinary_train, trinary_test, cl = trinary_train_category, k=3)
trinary3
tri_cm3 <- table(trinary3, trinary_test_category)
tri_cm3
tri_a3 <- accuracy(tri_cm3)
tri_a3

trinary5 <- knn(trinary_train, trinary_test, cl = trinary_train_category, k=5)
trinary5
tri_cm5 <- table(trinary5, trinary_test_category)
tri_cm5
tri_a5 <- accuracy(tri_cm5)
tri_a5

trinary10 <- knn(trinary_train, trinary_test, cl = trinary_train_category, k=10)
trinary10
tri_cm10 <- table(trinary10, trinary_test_category)
tri_cm10
tri_a10 <- accuracy(tri_cm10)
tri_a10

trinary15 <- knn(trinary_train, trinary_test, cl = trinary_train_category, k=15)
trinary15
tri_cm15 <- table(trinary15, trinary_test_category)
tri_cm15
tri_a15 <- accuracy(tri_cm15)
tri_a15

trinary20 <- knn(trinary_train, trinary_test, cl = trinary_train_category, k=20)
trinary20
tri_cm20 <- table(trinary20, trinary_test_category)
tri_cm20
tri_a20 <- accuracy(tri_cm20)
tri_a20

trinary25 <- knn(trinary_train, trinary_test, cl = trinary_train_category, k=25)
trinary25
tri_cm25 <- table(trinary25, trinary_test_category)
tri_cm25
tri_a25 <- accuracy(tri_cm25)
tri_a25

trinary_accuracy <- c(tri_a3, tri_a5, tri_a10, tri_a15, tri_a20, tri_a25)

trinary_accuracy_df <- data.frame(k_values, trinary_accuracy)

ggplot(trinary_accuracy_df, aes(x=k_values, y=trinary_accuracy)) + 
  geom_point()

