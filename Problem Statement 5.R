n <- 500
set.seed(75080)

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 50
y   <- -100*z+ 1100 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt1 <- data.table('id'=1:500,'sat'=y,'income'=x,'group'=rep(1,n))

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 80
y   <- -80*z+ 1200 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt2 <- data.table('id'=501:1000,'sat'=y,'income'=x,'group'=rep(2,n))

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 30
y   <- -120*z+ 1000 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt3 <- data.table('id'=1001:1500,'sat'=y,'income'=x,'group'=rep(3,n))

dtable <- merge(dt1    ,dt2, all=TRUE)
dtable <- merge(dtable ,dt3, all=TRUE)

ggplot(dtable,aes(x=income,y=sat,color=as.factor(group)))+geom_point()
pool<- lm(sat~income,data=dtable)
within<- lm(sat~income+group-1,data=dtable)
model1 <- lm(sat~income,data=dtable[group==1])
model2 <- lm(sat~income,data=dtable[group==2])
model3 <- lm(sat~income,data=dtable[group==3])
summary(pool)
summary(within)
summary(model1)
summary(model2)
summary(model3)
#2
dtable$group <- as.factor(dtable$group)
dtab <- pdata.frame(dtable,index=c('id','group'))
model1 <- plm(sat~income,model='pooling',data=dtab)
summary(model1)
model2 <- lm(sat~income+group-1,data=dtable)
model3 <- lm(sat~income,data=dtable[group==1])
summary(model2)
summary(model3)
model5 <- lm(sat~income,data=dtable[group==2])
summary(model5)
model7 <- lm(sat~income,data=dtable[group==3])
summary(model7)
#3
plot(ctree(sat~income,data=dtable))
ctree(sat~income,data=dtable)
plot(ctree(sat~group,data=dtable))
ctree(sat~group,data=dtable)
plot(ctree(sat~group+income,data=dtable))
ctree(sat~group+income,data=dtable)
#4
plot(glmtree(sat~income|group,data = dtable))
AIC(glmtree(sat~income|group,data=dtable))
AIC(glmtree(sat~income|income,data=dtable))
AIC(glmtree(sat~income|group+income,data=dtable))
AIC(glmtree(sat~group|income,data=dtable))
AIC(glmtree(sat~group|group+income,data=dtable))
#5
kmeans.wss <- function(data,maxcluster=10,seed=1,nstart=10){
  wss <- rep(NA,maxcluster)
  for(i in 1:maxcluster) {
    set.seed(seed)
    model <- kmeans(data,centers = i,nstart = nstart)
    wss[i] <- model$tot.withinss
  }
  return(wss)
}
eratio <- function(wss) { # USE MINUS 1 FOR PCA
  # Creates the eigenvalue ratio estimator for the number of clusters
  n <- NROW(wss)
  dss <- -diff(wss) # Create differences in wss (eigenvalues)
  dss <- c(wss[1]/log(n),dss) # Assign a zero case
  erat <- dss[1:(n-1)]/dss[2:n] # Build the eigenvalue ratio statistic
  gss <- log(1+dss/wss) # Create growth rates
  grat <- gss[1:(n-1)]/gss[2:n] # Calculuate the growth rate statistic
  return(c(which.max(erat),which.max(grat))) # Find the maximum number for each estimator
}
wss <- kmeans.wss(dtable[,.(income,sat)])
wss
eratio(wss)
plot.wss <- function(wss){
  plot(1:NROW(wss),wss,type="b", xlab="Number of Clusters", ylab="Aggregate Within Group SS")
}
plot.wss(wss)
model1 <- kmeans(dtable[,.(income,sat)],2,nstart=10)
model1$centers
eratio(wss)

#6
dist(scale(dtable))
model <- hclust(dist(scale(dtable)))
plot(model)
summary(cutree(model,k=2))
cutree(model,k=5)

dtable[,lapply(.SD,mean),by=cutree(model,k=3)]
hclust.wss <- function (x,model=(hclust(dist(x))),mc=10){
  wss <- rep(NA,mc)
  for(j in 1:mc){
    gmean <- x[,lapply(.SD,mean),by=cutree(model,k=j)]
    demean <- x-gmean[cutree(model,k=j), 2:(ncol(x)+1)]
    wss[j] <- sum(demean^2)
  }
  return(wss)
}

hwss <- hclust.wss(dtable[,.(income,sat)])
hwss
eratio(hwss)
plot(hwss)
model <-hclust(dist(dtable[,.(income,sat)]))
dtable$hgrp <- as.factor(cutree(model,4))
table(dtable$hgrp,dtable$group)
(268+250+202+134)/1500
#7
dtable$kgrp <- as.factor(model1$cluster)
model1 <- lm(sat~income+kgrp-1,data=dtable)
summary(model1)
model2 <- lm(sat~income+hgrp-1,data=dtable)
summary(model2)
#8
kmincome <- kmeans.wss(dtable[,.(income)])
kmincome
eratio((kmincome))
plot.wss(kmincome)
model1 <- kmeans(dtable[,.(income)],3,nstart=10)
model1$centers
dtable$kgrp1 <- as.factor(model1$cluster)
table(dtable$kgrp1,dtable$group)
(489+500+484)/1500
#9
model1 <- kmeans(scale(dtable[,.(scale(income),scale(sat))]),3,nstart = 10)
model1$centers
kmscale <- kmeans.wss(dtable[,.(scale(income),scale(sat))])
kmscale
eratio(kmscale)
plot(kmscale)
dtable$kgrp2 <- as.factor(model1$cluster)
table(dtable$kgrp2,dtable$group)
(371+500+330)/1500
