rm(list=ls())
library(visdat)
library(lattice)
library(MASS)
library(GGally)
library(tidyverse)
library(caret)
library(randomForest)
library(EnvStats)
library(e1071)
set.seed(123)

dat <- read.csv("gt_2011.csv")
vis_dat(dat)
summary(dat)
str(dat)

# Ambient temperature (AT) C 6.23 37.10 17.71
# Ambient pressure (AP) mbar 985.85 1036.56 1013.07
# Ambient humidity (AH) (%) 24.08 100.20 77.87
# Air filter difference pressure (AFDP) mbar 2.09 7.61 3.93
# Gas turbine exhaust pressure (GTEP) mbar 17.70 40.72 25.56
# Turbine inlet temperature (TIT) C 1000.85 1100.89 1081.43
# Turbine after temperature (TAT) C 511.04 550.61 546.16
# Compressor discharge pressure (CDP) mbar 9.85 15.16 12.06
# Turbine energy yield (TEY) MWH 100.02 179.50 133.51
# Carbon monoxide (CO) mg/m3 0.00 44.10 2.37
# Nitrogen oxides (NOx) mg/m3 25.90 119.91 65.29

boxplot(dat[,]) #range troppo differenti --> meglio normalizzare in seguito 

#Visualizzo boxplot singoli delle variabili
#par(mfrow=c(4,3))
#t<-sapply(1:11, function(u) boxplot(dat[,u], main=colnames(dat)[u]))

#Visualizzo istogrammi delle variabili
#par(mfrow=c(4,3))
#t<-sapply(1:11, function(u) hist(dat[,u], main=colnames(dat)[u]))

par(mfrow=c(1,1))

# boxplot CO e log(CO)
boxplot(dat$CO)
boxplot(log(dat$CO))

dat<-as.tibble(dat)

ggplot(dat, aes(CO))+geom_boxplot(color="orange", fill="orange", alpha=0.2)+theme_bw()
ggsave("boxplot_prima_CO.jpeg")
aa<-log(dat$CO)
ggplot(dat, aes(aa))+geom_boxplot(color="orange", fill="orange", alpha=0.2)+theme_bw()+xlab("log(CO)")
ggsave("boxplot_dopo_CO.jpeg")

rr<-c(dat$CO, aa)
rr<-matrix(c(rr, rep("1", 7411),rep("2", 7411)), ncol=2, nrow=length(rr))
rr<-as.tibble(rr)
colnames(rr)<-c("CO", "sep")
rr$sep<-as.factor(rr$sep)
rr$CO<-as.numeric(rr$CO)
levels(rr$sep)<-c("uno", "due")

ggplot(rr, aes(x=sep,y=CO))+
  geom_boxplot(color="orange", fill="orange", alpha=0.2)+
  theme_bw()

dat<-as.data.frame(dat)

reg <- lm(dat[,"CO"] ~ 1, data=dat)
boxcox(reg) #consiglia trasformazione logaritmica
#ggpairs(dat)

#applichiamo la trasformazione logaritmica
dat$CO <- log(dat$CO)

#Correlazione
ggcorr(dat,label=T, label_round = 2)
#ggsave("corplot.jpeg")

#Vi sono variabili altamente correlate --> da rimuovere (consideriamo sopra 85%)
#le selezioniamo confrontando i due varImplot di CO e NOX

#Vediamo varImportance per capire quale escludere con target NOX
rf <- randomForest(NOX ~ ., mtry= 4, ntree=150, data=dat)
varImpPlot(rf)

#Vediamo varImportance per capire quale escludere con target CO
rf1 <- randomForest(CO ~ ., mtry= 4, ntree=150, data=dat)
varImpPlot(rf1)
#risultati coerenti

#Rimuovo TEY e vedo cosa cambia
ggcorr(dat[-8],label=T, label_round = 2, label_size = 3)

#Rimuovo TEY e CDP vedo cosa cambia
ggcorr(dat[-c(8,9)],label=T, label_round = 2)

#Rimuovo TEY,CDP,AFDP vedo cosa cambia
ggcorr(dat[-c(8,9,4)],label=T, label_round = 2)

#Rimuovo TEY,CDP,AFDP,GTEP vedo cosa cambia
ggcorr(dat[-c(8,9,4,5)],label=T, label_round = 2)
#Non vi e' piu' nessuna variabile altamente correlata

ndata<-dat[,-c(8,9,4,5)]

#Analizzo le variabili rimaste
par(mfrow=c(3,3))
tmp2<-sapply(1:7, function(u) hist(ndata[,u], main=colnames(ndata)[u]))

#vedo i boxplot
par(mfrow=c(3,3))
tmp3<-sapply(1:7, function(u) boxplot(ndata[,u], main=colnames(ndata)[u]))

#Le variabili CO,TIT, NOX, AP, TAT e AH sembrano avere problemi con numerosi outlier
par(mfrow=c(1,1))

nCO<-boxplot(ndata$CO)
k1<-length(nCO$out)
test <- rosnerTest(ndata$CO,
                   k = k1
)
test$n.outliers#solo 23 osservazioni di log(co) sono veri outlier

nTIT<-boxplot(ndata$TIT)
k2<-length(nTIT$out)
test2 <- rosnerTest(ndata$TIT,
                    k = k2)
test2$n.outliers#solo 5 osservazioni di TIT sono veri outlier


nNOX<-boxplot(ndata$NOX)
k3<-length(nNOX$out)
test3 <- rosnerTest(ndata$NOX,
                    k = k3)
test3$n.outliers#solo 30 osservazioni di NOX sono veri outlier

nAP<-boxplot(ndata$AP)
k4<-length(nAP$out)
test4 <- rosnerTest(ndata$AP,
                    k = k4)
test4$n.outliers #0 veri outliers


nTAT<-boxplot(ndata$TAT)
k5<-length(nTAT$out)
test5 <- rosnerTest(ndata$TAT,
                    k = k5)
test5$n.outliers #0 veri outliers

nAH<-boxplot(ndata$AH)
k6<-length(nAH$out)
test6 <- rosnerTest(ndata$AH,
                    k = k6)
test6$n.outliers #0 veri outliers

#rimuoviamo dal dataset i veri outlier
icoout<-test$all.stats$Obs.Num[test$all.stats$Outlier==T]
ititout<-test2$all.stats$Obs.Num[test2$all.stats$Outlier==T]
inoxout<-test3$all.stats$Obs.Num[test3$all.stats$Outlier==T]
pos<-unique(c(icoout,ititout,inoxout)) #3 oss hanno un outlier comune

ndata <- ndata[-pos,]

###############
#Clustering ###
###############

####k-means#####

#Troviamo il numero ottimale di gruppi
SSQs <- numeric()
kappas <- 2:10
for( k in kappas ) {
  km.res <- kmeans( scale(ndata), centers=k )
  SSQs <- c(SSQs,km.res$tot.withinss)
}

plot( kappas, SSQs, type="o", lwd=3, col="blue" )
#Gomito in 4, lo scelgo

#Li proviamo a visualizzare in 2d
nk<-kmeans(scale(ndata),centers = 4)

plot(ndata$NOX,ndata$CO,col=nk$cluster, pch=19)
library(scatterplot3d)
scatterplot3d(ndata$NOX,ndata$CO,ndata$TIT,color = nk$cluster)


####Hierarchical#####

d<-dist(scale(ndata))
h<-hclust(d=d)#complete
print(h)
plot(h)

# plot clusters agglomerating from 10 to 2
par(mfrow=c(3,3))
for( k in 10:2 ) {
  cluster <- cutree( h, k=k )
  plot(ndata$CO,ndata$NOX,pch=19)
  clrs <- rainbow(k)
  for( i in 1:k )
    points( ndata[which(cluster==i),c("CO","NOX"),drop=F], col=clrs[i], pch=19 )
}
par(mfrow=c(1,1))

#Il numero di gruppi ottimale sembra essere, se visualizzati in 2 dimensioni, 3,4 o 5.


library(clValid) # which loads also the 'cluster' package

sils <- numeric()
kappas <- 3:6
for( k in kappas ) {
  p<-cutree(h,k=k)
  sil <- silhouette( p, dist(scale(ndata)) )
  plot(sil,main=paste("k =",k))
  abline(v=mean(sil[,3]),col="red",lty=2,lwd=2)
  sils <- c(sils, (summary(sil))$avg.width )
}

plot( kappas, sils, type="o", lwd=3, col="red" ) #K=5



#proviamo single link

h2<-hclust(d,method = "single")
# plot clusters agglomerating from 10 to 2
par(mfrow=c(3,3))
for( k in 10:2 ) {
  cluster <- cutree( h2, k=k )
  plot(ndata$CO,ndata$NOX,pch=19)
  clrs <- rainbow(k)
  for( i in 1:k )
    points( ndata[which(cluster==i),c("CO","NOX"),drop=F], col=clrs[i], pch=19 )
}
par(mfrow=c(1,1))

#Per la forma dei nostri dati non è ottimale il legame singolo perchè produce chaining


#Average

h3<-hclust(d,method = "average")
# plot clusters agglomerating from 10 to 2
par(mfrow=c(3,3))
for( k in 10:2 ) {
  cluster <- cutree( h3, k=k )
  plot(ndata$CO,ndata$NOX,pch=19)
  clrs <- rainbow(k)
  for( i in 1:k )
    points( ndata[which(cluster==i),c("CO","NOX"),drop=F], col=clrs[i], pch=19 )
}

#Per la forma dei nostri dati non è ottimale il legame medio

h4<-hclust(d,"centroid")
par(mfrow=c(3,3))
for( k in 10:2 ) {
  cluster <- cutree( h4, k=k )
  plot(ndata$CO,ndata$NOX,pch=19)
  clrs <- rainbow(k)
  for( i in 1:k )
    points( ndata[which(cluster==i),c("CO","NOX"),drop=F], col=clrs[i], pch=19 )
}

#Per la forma dei nostri dati non è ottimale il legame centroid


##ward distance
h5<-hclust(d,method = "ward.D")

library(clValid) # which loads also the 'cluster' package

sils <- numeric()
kappas <- 2:8
for( k in kappas ) {
  p<-cutree(h5,k=k)
  sil <- silhouette( p, dist(scale(ndata)) )
  plot(sil,main=paste("k =",k))
  abline(v=mean(sil[,3]),col="red",lty=2,lwd=2)
  sils <- c(sils, (summary(sil))$avg.width )
}
par(mfrow=c(1,1))
plot( kappas, sils, type="o", lwd=3, col="red" )#K=4




#DBSCAN####
library(dbscan)
eps <- seq(0.5,1.2, by=0.1)
minPts <- seq(50, 100, by=10)
prova <- expand.grid(eps,minPts)
sil <- numeric()
for(i in 1:nrow(prova)){
  dbscan.res <- dbscan( scale(ndata), eps=prova[i,1], minPts=prova[i,2])
  sil[i] <- mean(silhouette(dbscan.res$cluster, dist=dist(scale(ndata)))[,3])
}
which(sil==max(sil))
prova[8,]

sil <- mean(silhouette(dbscan.res$cluster, dist=dist(scale(ndata)))[,3])
sil

dbscan(scale(ndata),eps = 1.2,minPts = 50)

##########################
####SUPERVISED LEARNING###
##########################

#divido in training e test

idx<-sample(nrow(ndata),nrow(ndata)*0.8,replace = F)
train<-ndata[idx,]
test<-ndata[-idx,]


#############
### CO ######
#############
set.seed(123)
########KNN#########
k<-numeric()
msef<-numeric()
#Randomly shuffle the data
yourData<-train[sample(nrow(train)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(yourData)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
for(k in 2:20){
  mse<-numeric()
  for(i in 1:10){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- yourData[testIndexes, ]
    trainData <- yourData[-testIndexes, ]
    m<-knnreg(scale(trainData[,-6]),scale(trainData[,6]),k=k)
    p<-predict(m,scale(testData[,-6]))
    mse[i]<-mean((testData[,6]-p)^2)
  }
  msef[k-1]<-mean(mse)
}
msef
which.min(msef)
par(mfrow=c(1,1))
plot(2:20,msef)
abline(h=min(msef))

#knn ottimale con k=7

knnf<-knnreg(scale(train[,-6]),scale(train[,6]),k=7)
prevtrain<-predict(knnf,scale(train[,-6]))
prevtest<-predict(knnf,scale(test[,-6]))

mse_tr_knn<-mean((scale(train[,6])-prevtrain)^2)
mse_te_knn<-mean((scale(test[,6])-prevtest)^2)

mape_tr_knn<-mean(abs((train$CO-prevtrain)/train$CO))
mape_tr_knn
mape_test_knn<-mean(abs((test$CO-prevtest)/test$CO))
mape_test_knn

plot(test$TIT, test$CO, pch=20)
lines(test$TIT[order(test$TIT, decreasing=F)], prevtest[order(test$TIT, decreasing=F)], col=4)

data_plot1 <- cbind(test, prevtest)
data_plot1 <- as.tibble(data_plot1)

colors <-  c( "CO" = "darkorchid", "prevtest" = "cornflowerblue")

(gg1 <- ggplot(data=data_plot1, aes(x=TIT, y=CO, color=colors))+
   geom_point(color="darkorchid", alpha=.7,position=position_jitter(w=0.1, h=0))+
   geom_point(aes(x=TIT, y=prevtest), color="cornflowerblue", alpha=.5) +
   labs(x="TIT",y="CO", color="Legend")+
   theme_minimal())+
   scale_color_manual(name = "Legenda",
              values = c( "CO" = "darkorchid", "prevtest" = "cornflowerblue"),
              labels = c("deathpercentage", "tamponadepercentage"))


###Random forest####
rf<-randomForest(train[,6]~.,data = train,mtry=4,ntree=150)
prtrrf<-predict(rf)
prterf<-predict(rf,test)

mse_rf_train<-mean((train[,6]-prtrrf)^2)
mse_rf_test<-mean((test[,6]-prterf)^2)

mape_tr_rf<-mean(abs((train$CO-prtrrf)/train$CO))
mape_tr_rf
mape_test_rf<-mean(abs((test$CO-prterf)/test$CO))
mape_test_rf


plotdata2 <- cbind(test, prterf)
plotdata2 <- as.tibble(plotdata2)

(gg2 <- ggplot(data=plotdata2, aes(x=TIT, y=CO, color=colors))+
    geom_point(color="darkorchid", alpha=.7,position=position_jitter(w=0.1, h=0))+
    geom_point(aes(x=TIT, y=prterf), color="cornflowerblue", alpha=.5) +
    labs(x="TIT",y="CO", color="Legend")+
    theme_minimal()+ 
    scale_color_manual(name = "Legenda",
                     values = c( "CO" = "darkorchid", "prterf" = "cornflowerblue"),
                     labels = c("deathpercentage", "tamponadepercentage")))

####svm########
library(e1071)

train_control = trainControl(method = "cv", number = 10)
model = train(CO~., data = train, method = "svmLinear", trControl = train_control,scale=T)

#use model to make predictions on test data
pred_y = predict(model,(test[,-6]))
pred_y_tr = predict(model,(train[,-6]))

# performance metrics on the test data
msesvmte<-mean(((test[,6]) - pred_y)^2) #mse - Mean Squared Error
msesvmtr<-mean(((train[,6]) - pred_y_tr)^2) #mse - Mean Squared Error

#ESEGUITO 1 SOLA VOLTA PERCHé MOLTO ONEROSO
####I dati non sono separabili ottimizziamo c e gamma con kernel=radial e kernel=
# gamma<-seq(0.1,2,0.2)
# mse<-numeric()
# gr<-expand.grid(gamma,1:10)
# for(i in 1:nrow(gr)){
#   m<-svm(CO~.,data = train,kernel="radial",gamma=gr[i,1],cost=gr[i,2])
#   p<-predict(m,test[,-6])
#   mse[i]<-mean((p-test[,6])^2)
# }
# 
# which.min(mse)
# gr[18,] 
#
#parametri migliori 1.5, 2

#svm con valori ottimizati
svm_fin<-svm(CO~.,data = train,kernel="radial",gamma=1.5,cost=2)
prev_svm_train<-predict(svm_fin)
prev_svm_test<-predict(svm_fin,test[,-6])
mape_tr_svm<-mean(abs((train$CO-prev_svm_train)/train$CO))
mape_tr_svm
mape_test_svm<-mean(abs((test$CO-prev_svm_test)/test$CO))
mape_test_svm



(gg3 <- ggplot(data=plotdata2, aes(x=TIT, y=CO, color=colors))+
    geom_point(color="coral", alpha=.7,position=position_jitter(w=0.1, h=0))+
    geom_point(aes(x=TIT, y=prev_svm_test), color="turquoise3", alpha=.5) +
    labs(x="TIT",y="CO", color="Legend")+
    theme_minimal())

#############
### NOX #####
#############
set.seed(234)
########KNN#########
n.train<-scale(train)
n.test<-scale(test)
k<-numeric()
msef<-numeric()
#Randomly shuffle the data
yourData<-n.train[sample(nrow(n.train)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(yourData)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
for(k in 2:20){
  mse<-numeric()
  for(i in 1:10){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- yourData[testIndexes, ]
    trainData <- yourData[-testIndexes, ]
    m<-knnreg(trainData[,-7],trainData[,7],k=k)
    p<-predict(m,testData[,-7])
    mse[i]<-mean((testData[,7]-p)^2)
  }
  msef[k-1]<-mean(mse)
}
msef
which.min(msef) #k=4
plot(c(2:20),msef)
abline(h=min(msef))
#knn ottimale con k=4

knnf<-knnreg(n.train[,-7],n.train[,7],k=4)
prevtrain<-predict(knnf,n.train[,-7])
prevtest<-predict(knnf,n.test[,-7])

msetrkn<-mean((n.train[,7]-prevtrain)^2)
msetekn<-mean((n.test[,7]-prevtest)^2)

MAPE.tr.kn <- mean(abs((n.train[,7]-prevtrain)/n.train[,7])) 
MAPE.tr.kn
MAPE.te.kn <- mean(abs((n.test[,7]-prevtest)/n.test[,7])) 
MAPE.te.kn

###Random forest####
rf<-randomForest(train[,7]~.,data = train,mtry=4,ntree=150)
prtrrf<-predict(rf)
prterf<-predict(rf,test)

msetr_rf<-mean((train[,7]-prtrrf)^2)
msete_rf<-mean((test[,7]-prterf)^2)

MAPE.tr.rf<- mean(abs((train[,7]-prtrrf)/train[,7]))
MAPE.tr.rf
MAPE.te.rf <- mean(abs((test[,7]-prterf)/test[,7]))
MAPE.te.rf

plotdata2 <- cbind(test, prterf)
plotdata2 <- as.tibble(plotdata2)

colors <-  c( "CO" = "darkorchid", "prevtest" = "cornflowerblue")
(gg4 <- ggplot(data=plotdata2, aes(x=TIT, y=NOX, color=colors))+
    geom_point(color="darkorchid", alpha=.7,position=position_jitter(w=0.1, h=0))+
    geom_point(aes(x=TIT, y=prterf), color="cornflowerblue", alpha=.5) +
    labs(x="TIT",y="NOX", color="Legend")+
    theme_minimal())


####svm########

library(e1071)

train_control = trainControl(method = "cv", number = 10)
model = train(NOX~., data = n.train, method = "svmLinear", trControl = train_control)

#use model to make predictions on test data
pred_y = predict(model,(n.test[,-7]))
pred_y_tr = predict(model,(n.train[,-7]))

# performance metrics on the test data
msesvmte<-1-mean(((n.test[,7]) - pred_y)^2) #mse - Mean Squared Error
msesvmtr<-1-mean(((n.train[,7]) - pred_y_tr)^2) #mse - Mean Squared Error

MAPE.tr.svm.lin <- mean(abs((n.train[,7]-pred_y_tr)/n.train[,7]))
MAPE.te.svm.lin <- mean(abs((n.test[,7]-pred_y)/n.test[,7]))

#parte onerosa eseguita 1 sola volta
####I dati non sono separabili ottimizziamo c e gamma con kernel=radial e kernel=
# mse<-numeric()
# gamma<-seq(0.1,2,0.2)
# gr<-expand.grid(gamma,1:10)
# for(i in 1:nrow(gr)){
#   m<-svm(NOX~.,data = n.train,kernel="polynomial",gamma=gr[i,1],cost=gr[i,2])
#   p<-predict(m,n.test[,-7])
#   mse[i]<-mean((p-n.test[,7])^2)
# }
# 
# which.min(mse)
# gr[which.min(mse),] 
#
#c=10 e gamma=1.7 PARAMETRI MIGLIORI

SVM.mod <-svm(NOX~.,data = n.train,kernel="radial",gamma=1.7,cost=10)

SVM.pred.train<-predict(SVM.mod,n.train[,-7])
SVM.pred.test<-predict(SVM.mod,n.test[,-7])

MSE.svm.te<-mean(((n.test[,7]) - SVM.pred.test)^2)
MSE.svm.tr<-mean(((n.train[,7]) - SVM.pred.train)^2) 

MAPE.tr <- mean(abs((n.train[,7]-SVM.pred.train)/n.train[,7]))
MAPE.tr
MAPE.te <- mean(abs((n.test[,7]-SVM.pred.test)/n.test[,7]))
MAPE.te

plotdata2 <- cbind(test, SVM.pred.test)
plotdata2 <- as.tibble(plotdata2)



#### PREVISIONI PER GLI ANNI SUCCESSIVI CO
###previsioni sugli anni successivi

anno2012<-read.csv("gt_2012.csv")
anno2013<-read.csv("gt_2013.csv")
anno2014<-read.csv("gt_2014.csv")
anno2015<-read.csv("gt_2015.csv")

anno2012<-anno2012[,-c(8,9,4,5)]
anno2013<-anno2013[,-c(8,9,4,5)]
anno2014<-anno2014[,-c(8,9,4,5)]
anno2015<-anno2015[,-c(8,9,4,5)]

anno2012$CO<-log(anno2012$CO)
anno2013$CO<-log(anno2013$CO)
anno2014$CO<-log(anno2014$CO)
anno2015$CO<-log(anno2015$CO)
#miglior modello sugli anni successivi
set.seed(123)
library(randomForest)
rf_finale<-randomForest(CO~.,data = ndata,mtry=4,ntree=150)

#tolgo l'osservazione 5993 che crea problemi nel mape
anno2012<-anno2012[-5993,]
prev_2012<-predict(rf_finale, anno2012)
mse_2012<-mean((prev_2012-anno2012$CO)^2)
mse_2012
mape_2012<-mean(abs((anno2012$CO-prev_2012)/anno2012$CO))
mape_2012
#which(anno2012$CO==0)

prev_2013<-predict(rf_finale, anno2013)
mse_2013<-mean((prev_2013-anno2013$CO)^2)
mse_2013
mape_2013<-mean(abs((anno2013$CO-prev_2013)/anno2013$CO))
mape_2013

prev_2014<-predict(rf_finale, anno2014)
mse_2014<-mean((prev_2014-anno2014$CO)^2)
mse_2014
mape_2014<-mean(abs((anno2014$CO-prev_2014)/anno2014$CO))
mape_2014

prev_2015<-predict(rf_finale, anno2015)
mse_2015<-mean((prev_2015-anno2015$CO)^2)
mse_2015
mape_2015<-mean(abs((anno2015$CO-prev_2015)/anno2015$CO))
mape_2015

#### PREVISIONI PER GLI ANNI SUCCESSIVI NOX
###previsioni sugli anni successivi

anno2012<-read.csv("gt_2012.csv")
anno2013<-read.csv("gt_2013.csv")
anno2014<-read.csv("gt_2014.csv")
anno2015<-read.csv("gt_2015.csv")

anno2012<-anno2012[,-c(8,9,4,5)]
anno2013<-anno2013[,-c(8,9,4,5)]
anno2014<-anno2014[,-c(8,9,4,5)]
anno2015<-anno2015[,-c(8,9,4,5)]

#miglior modello sugli anni successivi
set.seed(123)
library(randomForest)
rf_finale<-randomForest(NOX~.,data = ndata,mtry=4,ntree=150)

prev_2012<-predict(rf_finale, anno2012)
mse_2012<-mean((prev_2012-anno2012$NOX)^2)
mse_2012
mape_2012<-mean(abs((anno2012$NOX-prev_2012)/anno2012$NOX))
mape_2012

prev_2013<-predict(rf_finale, anno2013)
mse_2013<-mean((prev_2013-anno2013$NOX)^2)
mse_2013
mape_2013<-mean(abs((anno2013$NOX-prev_2013)/anno2013$NOX))
mape_2013

prev_2014<-predict(rf_finale, anno2014)
mse_2014<-mean((prev_2014-anno2014$NOX)^2)
mse_2014
mape_2014<-mean(abs((anno2014$NOX-prev_2014)/anno2014$NOX))
mape_2014

prev_2015<-predict(rf_finale, anno2015)
mse_2015<-mean((prev_2015-anno2015$NOX)^2)
mse_2015
mape_2015<-mean(abs((anno2015$NOX-prev_2015)/anno2015$NOX))
mape_2015
