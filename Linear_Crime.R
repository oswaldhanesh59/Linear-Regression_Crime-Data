summary(c)
boxplot(c$Age)
summary(c$Age)
bench=146+ 1.5*IQR(c$Age)
c$Age[c$Age > bench]= bench

boxplot(c$Ed)
boxplot(c$Ex0)
boxplot(c$Ex1)

summary(c$Ex1)


bench1=97+ 1.5*IQR(c$Ex1)
c$Ex1[c$Ex1 > bench1]= bench1

boxplot(c$LF)
boxplot(c$M)

summary(c$M)
bench2=992+ 1.5*IQR(c$M)
c$M[c$M > bench2]= bench2

boxplot(c$N)
summary(c$N)
bench3=41.50+ 1.5*IQR(c$N)
c$N[c$N > bench3]= bench3

boxplot(c$U1)
summary(c$U1)
bench4=104 + 1.5*IQR(c$U1)
c$U1[c$U1 > bench4]= bench4

boxplot(c$U2)

summary(c$U2)
bench5=38.50 + 1.5*IQR(c$U2)
c$U2[c$U2 > bench5]= bench5

boxplot(c$W)
boxplot(c$X)

summary(c)


boxplot(c$NW)
summary(c$NW)
bench6= 152 + 1.5*IQR(c$NW)
c$NW[c$NW > bench6]= bench6

summary(c$Ed)
c$Ed[is.na(c$Ed)]=mean(c$Ed[!is.na(c$Ed)])

boxplot(c)
summary(c)

library(caTools)
split=sample.split(c$R,SplitRatio = 0.7)
View(split)
Train=subset(c,split=="TRUE")
Test=subset(c,split=="FALSE")

library(car)
model= lm(R~.,data=Train)
summary(model)
vif(model)

colnames(Train)
AIC(model) 
BIC(model)

model= lm(R~Age+S+Ed+Ex0+LF+M+N+NW+U1+U2+W+X,data=Train)
summary(model)
vif(model)
AIC(model) 
BIC(model)

model= lm(R~Age+S+Ed+Ex0+LF+M+N+NW+W+X,data=Train)
summary(model)
vif(model)
AIC(model) 
BIC(model)

model= lm(R~Age+S+Ed+Ex0+LF+M+N+NW+X,data=Train)
summary(model)
vif(model)
AIC(model) 
BIC(model)

model= lm(R~ Age+Ed+Ex0+X+U1+U2, data=Train)
summary(model)
vif(model)

predicted=predict(model,Test) 
View(predicted)
a=cbind(predicted,Test)
View(a)

L

plot(Test$R,type="l",lty=1.8,col="green")
lines(predicted,type="l",col="blue")
AIC(model) 
BIC(model)
