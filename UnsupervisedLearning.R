# unspupervise-learning-r
library(ISLR2)

# Examine USArrests
states <- row.names(USArrests)
names(USArrests)

# 1a - Variances before PCA
apply(USArrests, 2, var)

# Perform PCA
pr.out <- prcomp(USArrests, scale = TRUE)

# 1b - PCA loadings
pr.out$rotation

# 1d - Variance of PCs
pr.var <- pr.out$sdev^2
pr.var

# 1e - Proportion of variance explained
pve <- pr.var / sum(pr.var)
pve

# PC1 explain approximately 62.01% of the total variance
# PC4 explains approximately 4.34%
# if the transformed data must be reduced from four dimensions to three
# dimensions, remove PC4 b/c it explains the smallest proportion of the total
# variance. Retaining PC1 - PC3 preserves approximately 95.66% of the total
# variance while discarding only about 4.34%

sum(pve[1:3])


set.seed(2)

x <- matrix(rnorm(50 * 2), ncol = 2)

x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4

km.out <- kmeans(x, 2, nstart = 20)
km.out$cluster

plot(x,
     col = (km.out$cluster + 1),
     main = "K-Means Clustering Results with K = 2",
     xlab = "",
     ylab = "",
     pch = 20,
     cex = 2)

set.seed(4)

km.out <- kmeans(x, 3, nstart = 20)
km.out

plot(x,
     col = (km.out$cluster + 1),
     main = "K-Means Clustering Results with K = 3",
     xlab = "",
     ylab = "",
     pch = 20,
     cex = 2)

nci.labs <- NCI60$labs
nci.data <- NCI60$data

dim(nci.data)

pr.out <- prcomp(nci.data, scale = TRUE)

summary(pr.out)


pve <- 100 * pr.out$sdev^2 / sum(pr.out$sdev^2)
plot(pve,
     type = "o",
     ylab = "PVE",
     xlab = "Principal Component",
     col = "blue")

# 3c. Choosing 7 principal components is appropriate because the first
# 7 PCs explain about 40% of the total variance in the NCI60 data.
# This reduces the original 6,830 dimensions to only 7 dimensions while
# still retaining a substantial amount of the important variation in the data.
# Therefore, using 7 PCs provides a much simpler representation of the
# dataset while preserving useful information for further analysis.
