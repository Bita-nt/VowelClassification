
############# Multivariate Analysis Final Project

train.data <- read.csv("/home/bita/Work/Study_2018-2020/Fall_2019/Multivar_Stat/final project/vowel-train.csv", sep = ",")
test.data <- read.csv("/home/bita/Work/Study_2018-2020/Fall_2019/Multivar_Stat/final project/vowel-test.csv", sep = ",")


X.train = train.data[,-c(1:2)]
Y.train = train.data[, 2]

X.test = test.data[,-c(1:2)]
Y.test = test.data[, 2]



##################### Section 1
pc.cov <- prcomp(X.train)
summary(pc.cov)
# Importance of components:
#                          PC1    PC2    PC3     PC4     PC5     PC6     PC7    PC8     PC9    PC10
# Standard deviation     1.4138 1.0529 0.9523 0.72544 0.56583 0.51127 0.45226 0.3978 0.31576 0.21647
# Proportion of Variance 0.3549 0.1968 0.1610 0.09345 0.05686 0.04642 0.03632 0.0281 0.01771 0.00832
# Cumulative Proportion  0.3549 0.5518 0.7128 0.80627 0.86313 0.90955 0.94587 0.9740 0.99168 1.00000


#Interpretation: The proportion of variance explained by PC1 is 0.3594 and the 
# commulative proportion of PC1 through PC7 is 0.94587 which means PC1 through PC7 explain most of 
# the variablity in the data (95%) that is most of the useful information in the data.


plot(pc.cov, type = "l", main = "Principal Components")



#Check if train and test data are similar enough
par(mfrow=c(1,2))
hist(as.matrix(X.train))
hist(as.matrix(X.test))
t.test(X.train, X.test)
# Welch Two Sample t-test
# 
# data:  X.train and X.test
# t = -0.3342, df = 9665.2, p-value = 0.7382
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.06575533  0.04659992
# sample estimates:
#   mean of x   mean of y 
# -0.10765000 -0.09807229 
ks.test(as.matrix(X.train), as.matrix(X.test))
# Two-sample Kolmogorov-Smirnov test
# 
# data:  as.matrix(X.train) and as.matrix(X.test)
# D = 0.019264, p-value = 0.3199
# alternative hypothesis: two-sided



#Tip: Use Silohette scores to compare clustering methods; use confusion matrix accuracy to 
# compare classifications and cluster vs classification performance and misclassification error
# for comparing classifications which is also 1-accuracy. .




##################### Section 2-3-4
#####LDA####
install.packages("MASS")
library(MASS)

lda.fit = lda(X.train, Y.train)
Y.pred = predict(lda.fit, X.train)$class
sum(Y.pred!=Y.train)/length(Y.train)
# [1] 0.3162879


lda.fit = lda(X.train, Y.train)
Y.pred.lda = predict(lda.fit, X.test)$class
sum(Y.pred.lda!=Y.test)/length(Y.test)
# [1] 0.5562771


lda.fit = lda(data.frame(pc.cov$x[,c(1:6)]), Y.train)
Y.pred = predict(lda.fit, data.frame(pc.cov$x[,c(1:6)]))$class
sum(Y.pred!=Y.train)/length(Y.train)
# [1]  0.405303
  

#To apply the PCA rule from the training data on the test data
pc.train.data = scale(X.train, pc.cov$center, pc.cov$scale)%*%as.matrix(pc.cov$rotation)
pc.test.data = scale(X.test, pc.cov$center, pc.cov$scale)%*%as.matrix(pc.cov$rotation)

#as.matrix(X.train)%*%as.matrix(pc.cov$rotation)

lda.fit = lda(pc.train.data[,c(1:6)], Y.train)
Y.pred.lda.pc = predict(lda.fit, pc.test.data[,c(1:6)])$class
sum(Y.pred.lda.pc!=Y.test)/length(Y.test)
# [1] 0.5909091

#Confusion matrix
cm.lda = table(Y.test, Y.pred.lda.pc)
cm.lda
#                 Y.pred
#    1  2  3  4  5  6  7  8  9 10 11
# 1  29 10  0  0  0  0  0  0  2  1  0
# 2  18 11  9  1  0  0  0  0  1  0  2
# 3   0 11 14 17  0  0  0  0  0  0  0
# 4   0  0  1 33  0  7  0  0  0  0  1
# 5   0  0  0 17 12  3 10  0  0  0  0
# 6   0  0  6  7  9 11  2  0  0  0  7
# 7   0  0  3  2 21  5  6  3  2  0  0
# 8   0  0  0  0  3  0  1 28  7  3  0
# 9   0  0  2  0  0  0  5  7 13 10  5
# 10  8  2  3  0  0  0  0  0 12 16  1
# 11  0  0  6  5  1  5  1  0  8  0 16

#  Accuracy :
sum(diag(cm.lda))/sum(cm.lda)
# 0.4090909


#Check equality of variance:
apply(X.train, 2, sd)
#   x.1       x.2       x.3       x.4       x.5       x.6       x.7       x.8       x.9      x.10 
# 0.9579655 1.1609696 0.7413629 0.7693611 0.7220107 0.6485472 0.4792544 0.5955797 0.6195841 0.5603173

# There seems to be some disparity among variances


#####QDA####
qda.fit = qda(X.train, Y.train)
Y.pred = predict(qda.fit, X.train)$class
sum(Y.pred!=Y.train)/length(Y.train)
# [1] 0.01136364


qda.fit = qda(X.train, Y.train)
Y.pred.qda = predict(qda.fit, X.test)$class
sum(Y.pred.qda!=Y.test)/length(Y.test)
# [1] 0.5281385


qda.fit = qda(pc.train.data[,c(1:6)], Y.train)
Y.pred = predict(qda.fit, pc.train.data[,c(1:6)])$class
sum(Y.pred!=Y.train)/length(Y.train)
# [1] 0.1231061


qda.fit = qda(pc.train.data[,c(1:6)], Y.train)
Y.pred.qda.pc = predict(qda.fit, pc.test.data[,c(1:6)])$class
sum(Y.pred.qda.pc!=Y.test)/length(Y.test)
# [1] 0.4458874

#Confusion Matrix
cm.qda = table(Y.test, Y.pred.qda.pc)
cm.qda
#                Y.pred
#     1  2  3  4  5  6  7  8  9 10 11
# 1  30 12  0  0  0  0  0  0  0  0  0
# 2  10 29  3  0  0  0  0  0  0  0  0
# 3   0 12 22  4  0  0  4  0  0  0  0
# 4   0  0  2 24  0 16  0  0  0  0  0
# 5   0  0  0  0 11 21 10  0  0  0  0
# 6   0  0  0  1  6 32  0  0  1  0  2
# 7   0  0  0  0  2  7 33  0  0  0  0
# 8   0  0  0  0  0  0 21 20  0  1  0
# 9   0  0  0  1  0  1  3 14 16  3  4
# 10 13  4  3  0  0  0  0  0  7 15  0
# 11  0  0  0  1  0  5  3  0  9  0 24

#  Accuracy :
sum(diag(cm.qda))/sum(cm.qda)
#0.5541126

###################### Section 5

# there are 42 items in each of the 11 classes. I take 42/2 = 22 as classification of 
# half of the observations accurately and want to find which classes are harder to find half right:
which(diag(cm.qda)<=22)
# 3 5  8 9 10


#So classes 3, 5, 8, 9, 10 are harder to find. 

#Repeat Section 4 for the reduced data:
# New data: Classes 3, 5, 8, 9, 10 removed
indices = Y.train == 1 | Y.train == 2|Y.train == 4| Y.train == 6| Y.train == 7 | Y.train == 11
Y.train1 = Y.train[indices]
X.train1 = X.train[indices,]

indices = Y.test == 1 | Y.test == 2| Y.test == 4| Y.test == 6| Y.test == 7| Y.test == 11
Y.test1 = Y.test[indices]
X.test1 = X.test[indices,]


#####LDA#####
lda.fit = lda(X.train1, Y.train1)
Y.pred = predict(lda.fit, X.train1)$class
sum(Y.pred!=Y.train1)/length(Y.train1)
# [1]  0.2326389


lda.fit = lda(X.train1, Y.train1)
Y.pred.lda = predict(lda.fit, X.test1)$class
sum(Y.pred.lda!=Y.test1)/length(Y.test1)
# [1]  0.3730159

#Confusion Matrix
cm.lda_new = table(Y.test1, Y.pred.lda)
cm.lda_new
#        Y.pred
#     1  4  6  7  8 11
# 1  31 11  0  0  0  0
# 2  19 17  0  3  0  3
# 4   0  0 31  8  0  3
# 6   0  0 11 21  1  9
# 7   0  1  1 13 26  1
# 11  0  0  0  9  1 32


#  Accuracy 
sum(diag(cm.lda_new))/sum(cm.lda_new)
#   0.6269841
# Or equivalently use confusionMatrix(cm.lda_new) from "caret" package.

#####QDA####
qda.fit = qda(X.train1, Y.train1)
Y.pred = predict(qda.fit, X.train1)$class
sum(Y.pred!=Y.train1)/length(Y.train1)
# [1] 0.01041667


qda.fit = qda(X.train1, Y.train1)
Y.pred.qda = predict(qda.fit, X.test1)$class
sum(Y.pred.qda!=Y.test1)/length(Y.test1)
# [1]  0.3412698

cm.qda_new = table(Y.test1, Y.pred.qda)
cm.qda_new
#        Y.pred
#     1  4  6  7  8 11
# 1  38  4  0  0  0  0
# 2  18 24  0  0  0  0
# 4   0  4 14 17  4  3
# 6   0  0  1 22 14  5
# 7   0  0  0  2 35  5
# 11  0  1  4  2  2 33

#  Accuracy 
sum(diag(cm.qda_new))/sum(cm.qda_new)
# 0.6587302




########################## Section 6
## New data: Classes 1, 3, 6, 10
indices2 = Y.train == 1 | Y.train == 3| Y.train == 6| Y.train == 10
Y.train2 = Y.train[indices2]
X.train2 = X.train[indices2,]

indices2 = Y.test == 1 | Y.test == 3| Y.test == 6| Y.test == 10
X.test2 = X.test[indices2,]
Y.test2 = Y.test[indices2]


###### K-means
library(stats)
km = kmeans(X.train2, 4)
km

#      x.1    x.2    x.3    x.4     x.5    x.6     x.7    x.8     x.9    x.10
# 1 -3.4102 0.2738 -0.0187 1.228  0.6293 0.5672 -0.1183 -0.4852 -0.6230  0.3409
# 2 -2.2117 0.5386 -1.4320 0.7391 -0.1448 1.4675 -0.0112  0.8105 -0.5137 -0.4731
# 3 -4.7899 2.9635  0.1098 0.4805 -0.0227 0.2701  0.0999  0.2524 -0.2213 -0.1995
# 4 -2.8401 1.7206 -0.4396 0.3812 -0.6693 0.5064  0.1461  0.2916 -0.0733  0.0845

# (between_SS / total_SS =  47.6 %)
km$cluster

par(mfrow = c(1, 2))
plot(X.train2[c("x.1", "x.2")], col = Y.train2, main = "True clusters", ylab = "x.3")
plot(X.train2[c("x.1", "x.2")], col = km$cluster, main = "Clusters from K-means", ylab = "x.3")
points(km$centers, col = 1:3, pch = 8, cex=2)



# Silhouette Score
ss.km <- silhouette(km$cluster, dist(X.train2))
mean(ss.km[, 3])
# [1] 0.2694399


#Confusion Matrix
#I obtain the confusion matrix on the test data so that it is comparable to the ones in the classification section.
km.test = kmeans(X.test2, 4)
cm.km.test = table(Y.test2, km.test$cluster)
cm.km.test
#     1  2  3  4
# 1  26  0 16  0
# 3   0 25 17  0
# 6   0 40  2  0
# 10  6  0 12 24

sum(apply(cm.km.test, 1, max))/sum(cm.km.test)
# 0.6845238



#Aglomorotive Hierarchial
dist.eu = dist(X.train2, "euclidean")
hc.eu.c = hclust(dist.eu, "complete")
hc.eu.c$merge
hc.eu.c$height

hA.cluster = cutree(hc.eu.c, k = 4)
hA.cluster

#Silhouette Score
hA.cluster = cutree(hc.eu.c, k = 4)

library(cluster)
ss.hA <- silhouette(hA.cluster, dist(X.train2))
mean(ss.hA[, 3])
# [1] 0.1925788


dist.eu = dist(X.train2, "euclidean")
hc.eu.c = hclust(dist.eu, "single")
hc.eu.c$merge
hc.eu.c$height

#Silhouette Score
hA.cluster = cutree(hc.eu.c, k = 4)
hA.cluster

ss.hA <- silhouette(hA.cluster, dist(X.train2))
mean(ss.hA[, 3])
# [1] 0.2318668


dist.eu = dist(X.train2, "euclidean")
hc.eu.c = hclust(dist.eu, "average")
hc.eu.c$merge
hc.eu.c$height

#Silhouette Score
hA.cluster = cutree(hc.eu.c, k = 4)
hA.cluster

ss.hA <- silhouette(hA.cluster, dist(X.train2))
mean(ss.hA[, 3])
# [1] 0.2382757

plot(hc.eu.c, hang = -1, main = "Agglomerative Cluster Dendrogram")
plot(as.dendrogram(hc.eu.c), horiz = T, main = "Agglomerative Cluster Dendrogram")



#Ward's hierarchichal
dist.eu = dist(X.train2, "euclidean")
hc.eu.w = hclust(dist.eu, "ward.D")
hc.eu.w$merge
hc.eu.w$height
plot(hc.eu.w, hang = -1)
plot(as.dendrogram(hc.eu.c), horiz = T,  main = "Ward's Cluster Dendrogram")

#Silhouette Score
hW.cluster = cutree(hc.eu.w, k = 4)$hw.cluster


ss.hW <- silhouette(hW.cluster, dist(X.train2))
mean(ss.hW[, 3])
# [1] 0.2556523



#Confusion Matrix
#I obtain the confusion matrix on the test data so that it is comparable to the ones in the classification section.
dist.eu.test = dist(X.test2, "euclidean")
hc.eu.w.test = hclust(dist.eu.test, "ward.D")
hW.cluster.test = cutree(hc.eu.w.test, k = 4)
hW.test = table(Y.test2, hW.cluster.test)
hW.test
hW.cluster.test
#     1  2  3  4
# 1   3 18 21  0
# 3  24 18  0  0
# 6  30 12  0  0
# 10  0 18  6 18

sum(apply(hW.test, 1, max))/sum(hW.test)
# 0.5535714



####Model-based clustering method
#install.packages("mclust")
library(mclust)

bic = mclustBIC(X.train2)
bic
plot(bic)

bic1 = mclustBIC(X.train2, G = seq(from=1, to = 4, by = 1),
                 modelNames=c("EII", "EEI", "VEV"))
plot(bic1)



mc <- Mclust(X.train2, modelNames = "EEE", G = seq(from=1, to = 4, by = 1))
mc

mc$parameters[1]
# $pro
# [1] 0.5061356 0.1250007 0.2121202 0.1567434
mc$parameters[2]
# $mean
#        [,1]       [,2]       [,3]        [,4]
# x.1  -3.38936229 -3.7642010 -2.4252736 -3.47332315
# x.2   1.97558605 -0.2859090  0.8912642  1.07217066
# x.3  -0.76298401  0.2603258 -0.5356020  0.02717875
# x.4   0.61652088  1.1493330  0.1296061  1.43437302
# x.5  -0.31937492  1.0148275 -0.2494988  0.12146913
# x.6   0.84976834  1.1489546  0.7208394 -0.09485301
# x.7   0.09912332  0.2010817  0.1629645 -0.51306861
# x.8   0.41902874 -0.6521647  0.5846732 -0.25434272
# x.9  -0.47132131 -0.7717499  0.1411157 -0.32025685
# x.10 -0.19687625  0.1005858 -0.2335314  0.55192451

mc$parameters[3]$variance$sigma
# , , 1
# 
# x.1         x.2          x.3         x.4         x.5
# x.1   1.093000052 -0.71531737 -0.502685005  0.08705640 -0.10304205
# x.2  -0.715317372  0.94774973  0.369373879 -0.24346595  0.02927767
# x.3  -0.502685005  0.36937388  0.571877253 -0.05557655 -0.03086917
# x.4   0.087056405 -0.24346595 -0.055576548  0.27965660 -0.06205131
# x.5  -0.103042048  0.02927767 -0.030869174 -0.06205131  0.22306906
# x.6   0.283183943 -0.42616579 -0.258691315  0.15080765 -0.01566427
# x.7  -0.034773834  0.06581358 -0.039013767 -0.06231277  0.01420160
# x.8   0.112155888 -0.18688189 -0.009167003  0.07819787  0.01531764
# x.9  -0.186218812  0.15566314  0.105078053  0.10811581 -0.09998637
# x.10  0.009669443  0.04557435  0.008647089 -0.05049904 -0.08358152
# x.6          x.7          x.8         x.9         x.10
# x.1   0.28318394 -0.034773834  0.112155888 -0.18621881  0.009669443
# x.2  -0.42616579  0.065813578 -0.186881889  0.15566314  0.045574349
# x.3  -0.25869132 -0.039013767 -0.009167003  0.10507805  0.008647089
# x.4   0.15080765 -0.062312772  0.078197866  0.10811581 -0.050499039
# x.5  -0.01566427  0.014201597  0.015317638 -0.09998637 -0.083581519
# x.6   0.36200996 -0.058288760  0.131468285 -0.06770064 -0.044825065
# x.7  -0.05828876  0.134929828 -0.054898931 -0.03248827  0.004466042
# x.8   0.13146828 -0.054898931  0.204266445 -0.06638797 -0.070074968
# x.9  -0.06770064 -0.032488273 -0.066387967  0.30871323  0.031997512
# x.10 -0.04482506  0.004466042 -0.070074968  0.03199751  0.256255045
# 
# , , 2
# 
# x.1         x.2          x.3         x.4         x.5
# x.1   1.093000052 -0.71531737 -0.502685005  0.08705640 -0.10304205
# x.2  -0.715317372  0.94774973  0.369373879 -0.24346595  0.02927767
# x.3  -0.502685005  0.36937388  0.571877253 -0.05557655 -0.03086917
# x.4   0.087056405 -0.24346595 -0.055576548  0.27965660 -0.06205131
# x.5  -0.103042048  0.02927767 -0.030869174 -0.06205131  0.22306906
# x.6   0.283183943 -0.42616579 -0.258691315  0.15080765 -0.01566427
# x.7  -0.034773834  0.06581358 -0.039013767 -0.06231277  0.01420160
# x.8   0.112155888 -0.18688189 -0.009167003  0.07819787  0.01531764
# x.9  -0.186218812  0.15566314  0.105078053  0.10811581 -0.09998637
# x.10  0.009669443  0.04557435  0.008647089 -0.05049904 -0.08358152
# x.6          x.7          x.8         x.9         x.10
# x.1   0.28318394 -0.034773834  0.112155888 -0.18621881  0.009669443
# x.2  -0.42616579  0.065813578 -0.186881889  0.15566314  0.045574349
# x.3  -0.25869132 -0.039013767 -0.009167003  0.10507805  0.008647089
# x.4   0.15080765 -0.062312772  0.078197866  0.10811581 -0.050499039
# x.5  -0.01566427  0.014201597  0.015317638 -0.09998637 -0.083581519
# x.6   0.36200996 -0.058288760  0.131468285 -0.06770064 -0.044825065
# x.7  -0.05828876  0.134929828 -0.054898931 -0.03248827  0.004466042
# x.8   0.13146828 -0.054898931  0.204266445 -0.06638797 -0.070074968
# x.9  -0.06770064 -0.032488273 -0.066387967  0.30871323  0.031997512
# x.10 -0.04482506  0.004466042 -0.070074968  0.03199751  0.256255045
# 
# , , 3
# 
# x.1         x.2          x.3         x.4         x.5
# x.1   1.093000052 -0.71531737 -0.502685005  0.08705640 -0.10304205
# x.2  -0.715317372  0.94774973  0.369373879 -0.24346595  0.02927767
# x.3  -0.502685005  0.36937388  0.571877253 -0.05557655 -0.03086917
# x.4   0.087056405 -0.24346595 -0.055576548  0.27965660 -0.06205131
# x.5  -0.103042048  0.02927767 -0.030869174 -0.06205131  0.22306906
# x.6   0.283183943 -0.42616579 -0.258691315  0.15080765 -0.01566427
# x.7  -0.034773834  0.06581358 -0.039013767 -0.06231277  0.01420160
# x.8   0.112155888 -0.18688189 -0.009167003  0.07819787  0.01531764
# x.9  -0.186218812  0.15566314  0.105078053  0.10811581 -0.09998637
# x.10  0.009669443  0.04557435  0.008647089 -0.05049904 -0.08358152
# x.6          x.7          x.8         x.9         x.10
# x.1   0.28318394 -0.034773834  0.112155888 -0.18621881  0.009669443
# x.2  -0.42616579  0.065813578 -0.186881889  0.15566314  0.045574349
# x.3  -0.25869132 -0.039013767 -0.009167003  0.10507805  0.008647089
# x.4   0.15080765 -0.062312772  0.078197866  0.10811581 -0.050499039
# x.5  -0.01566427  0.014201597  0.015317638 -0.09998637 -0.083581519
# x.6   0.36200996 -0.058288760  0.131468285 -0.06770064 -0.044825065
# x.7  -0.05828876  0.134929828 -0.054898931 -0.03248827  0.004466042
# x.8   0.13146828 -0.054898931  0.204266445 -0.06638797 -0.070074968
# x.9  -0.06770064 -0.032488273 -0.066387967  0.30871323  0.031997512
# x.10 -0.04482506  0.004466042 -0.070074968  0.03199751  0.256255045
# 
# , , 4
# 
# x.1         x.2          x.3         x.4         x.5
# x.1   1.093000052 -0.71531737 -0.502685005  0.08705640 -0.10304205
# x.2  -0.715317372  0.94774973  0.369373879 -0.24346595  0.02927767
# x.3  -0.502685005  0.36937388  0.571877253 -0.05557655 -0.03086917
# x.4   0.087056405 -0.24346595 -0.055576548  0.27965660 -0.06205131
# x.5  -0.103042048  0.02927767 -0.030869174 -0.06205131  0.22306906
# x.6   0.283183943 -0.42616579 -0.258691315  0.15080765 -0.01566427
# x.7  -0.034773834  0.06581358 -0.039013767 -0.06231277  0.01420160
# x.8   0.112155888 -0.18688189 -0.009167003  0.07819787  0.01531764
# x.9  -0.186218812  0.15566314  0.105078053  0.10811581 -0.09998637
# x.10  0.009669443  0.04557435  0.008647089 -0.05049904 -0.08358152
# x.6          x.7          x.8         x.9         x.10
# x.1   0.28318394 -0.034773834  0.112155888 -0.18621881  0.009669443
# x.2  -0.42616579  0.065813578 -0.186881889  0.15566314  0.045574349
# x.3  -0.25869132 -0.039013767 -0.009167003  0.10507805  0.008647089
# x.4   0.15080765 -0.062312772  0.078197866  0.10811581 -0.050499039
# x.5  -0.01566427  0.014201597  0.015317638 -0.09998637 -0.083581519
# x.6   0.36200996 -0.058288760  0.131468285 -0.06770064 -0.044825065
# x.7  -0.05828876  0.134929828 -0.054898931 -0.03248827  0.004466042
# x.8   0.13146828 -0.054898931  0.204266445 -0.06638797 -0.070074968
# x.9  -0.06770064 -0.032488273 -0.066387967  0.30871323  0.031997512
# x.10 -0.04482506  0.004466042 -0.070074968  0.03199751  0.256255045

# Silhouette Score
mb.cluster = mc$classification
ss.mb <- silhouette(mb.cluster, dist(X.train2))
mean(ss.mb[, 3])
# [1] 0.1346217






