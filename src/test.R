stemwijzer<-read.csv("stemwijzer2017.csv")
parties<-stemwijzer[,1]
stellingen<-stemwijzer[,2:31]
stelling<-colnames(stellingen)

pca.out<-prcomp(stellingen)#, scale.=TRUE)

plot(pca.out, type="l")
biplot(pca.out, xlabs=parties, ylabs=stelling)

# Add explained variances to axes. First, calculate variance per dimension:
vars<-pca.out$sdev^2
totvar<-sum(vars)
explvar<-vars/totvar
# Make labels for the axes:
axes.labs <- paste('PC', 1:2, sep='')
# Append the proportion of explained variance to the axis labels
axes.labs <- paste(axes.labs, 
                   sprintf('(%0.1f%% explained var.)', 
                           100 * explvar[1:2]))

colnames(pca.out$x)[1:2]<- axes.labs

biplot(pca.out, xlabs=parties, ylabs=stelling)

# Voeg punten toe adhv eigen voorkeuren. Hier voor iemand die het overal mee eens is:

agree<-rep(3,30)
disagree<-rep(1,30)
gem<-colMeans(stellingen)
# Kijk of goed gaat (controle voor VVD)
#vvd<-as.numeric(stellingen[1,])-gem
#vvdcoor<-vvd %*% pca.out$rotation[,1:2]
#pca.out$x[1,1:2]
#vvdcoor
## zelfde!

agree<-agree-gem
disagree<-disagree-gem
pca.agree<-agree %*% pca.out$rotation[,1:2]
points(pca.agree,col="blue")
pca.disagree<-disagree %*% pca.out$rotation[,1:2]
points(pca.disagree,col="green")
