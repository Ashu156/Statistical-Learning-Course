require(ISLR)
require(MASS)

# Linear Discriminant Analysis (LDA)
lda.fit = lda(Direction~Lag1+Lag2, data = Smarket, subset = Year<2005)
lda.fit
plot(lda.fit)
Smarket.2005 = subset(Smarket, Year == 2005)
lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.pred[1:5, ]
class(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class, Smarket.2005$Direction)
mean(lda.pred$class == Smarket.2005$Direction)

# Quadratic Discriminant Analysis
qda.fit = qda(Direction~Lag1+Lag2,
              data = Smarket, subset = Year<2005)
qda.fit
qda.pred = predict(qda.fit, Smarket.2005)
names(qda.pred)
class(qda.pred)
data.frame(qda.pred)[1:5,]
table(qda.pred$class, Smarket.2005$Direction)
mean(qda.pred$class == Smarket.2005$Direction)

# K-nearest neighbor classification
library(class)
XLag = cbind(Lag1, Lag2)
train = Year<2005
knn.pred = knn(XLag[train,], XLag[!train,], Direction[train], k=5)
table(knn.pred, Direction[!train])
mean(knn.pred == Direction[!train])
