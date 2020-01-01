origin=read.csv("HousePrice.csv",header=T)
data <- readRDS("DimReduction/DifferentDimensionHousePriceList.rds")
estate=cbind(data$all,data$categorical)
#####################################################################################
library(FactoMineR)
library(factoextra)
library(corrplot)
library(corpcor)
library(vegan)
library(gridExtra)
library(coRanking)
library(EBImage)
library(RColorBrewer)
library(ggplot2)
library(pheatmap)
library(coRanking)

#############################################ploting EDA##################################################
###correlation heatmap
correlation=cor(data$all)  #calculate pairwise correlation
all_pheatmap=pheatmap(correlation) #all correlation heatmap
corr=unique(as.integer(which(correlation[lower.tri(correlation)]>0.5)/nrow(correlation))) #choose the row is there is one correlation bigger than 0.5
bigger0.5_pheatmap=pheatmap(correlation[corr,corr]) #correlation bigger than 0.5 heatmap

#####################Scatterplot (visulaized by product type) #############################################
### House price by product type
type_price=ggplot(estate, aes(x=as.factor(type),y=data$price/10000,fill=as.factor(type)))+
  geom_boxplot()+
  ylim(c(150,1500))+
  labs(x='不同商業用途用地之房價',y='價格(萬元)')+
  guides(fill=guide_legend('房屋用途'))
type_price
### House price by square meter, filled with product type
square_price=ggplot(estate,aes(x=full_sq,y=log(data$price/10000),color=type))+
  geom_point()+
  xlim(c(0,250))+
  labs(x='房屋面積(平方公尺)',y='log(價格(萬元))')+
  guides(color=guide_legend('房屋用途'))
square_price
### Different distance against house price but the similar scatterplot
metrowalk_price=ggplot(estate,aes(x=log(metro_km_walk),y=log(data$price/10000),color=type))+
  geom_point()+
  labs(x='log(離地鐵之距離(公里))',y='log(價格(萬元))')+
  guides(color=guide_legend('房屋用途'))

king_price=ggplot(estate,aes(x=log(kindergarten_km),y=log(data$price/10000),color=type))+
  geom_point()+
  labs(x='log(離幼稚園之距離(公里))',y='log(價格(萬元))')+
  guides(color=guide_legend('房屋用途'))

school_price=ggplot(estate,aes(x=log(school_km),y=log(data$price/10000),color=type))+
  geom_point()+
  labs(x='log(離學校之距離(公里))',y='log(價格(萬元))')+
  guides(color=guide_legend('房屋用途'))

park_price=ggplot(estate,aes(x=log(park_km),y=log(data$price/10000),color=type))+
  geom_point()+
  labs(x='log(離公園之距離(公里))',y='log(價格(萬元))')+
  guides(color=guide_legend('房屋用途'))

grid.arrange(metrowalk_price,king_price,school_price,park_price,ncol=2)
#indicate that no obvious trend in the plot

### Different distance of cafe price against house price with similar scatterplot 
min_1500=ggplot(estate,aes(x=log(cafe_sum_1500_min_price_avg),y=log(data$price/10000),color=type))+
  geom_point()+
  labs(x='log(距離1500公尺內咖啡最低價格)',y='log(價格(萬元))')+
  guides(color=guide_legend('房屋用途'))
min_2000=ggplot(estate,aes(x=log(cafe_sum_2000_min_price_avg),y=log(data$price/10000),color=type))+
  geom_point()+
  labs(x='log(距離2000公尺內咖啡最低價格)',y='log(價格(萬元))')+
  guides(color=guide_legend('房屋用途'))
min_3000=ggplot(estate,aes(x=log(cafe_sum_3000_min_price_avg),y=log(data$price/10000),color=type))+
  geom_point()+
  labs(x='log(距離3000公尺內咖啡最低價格)',y='log(價格(萬元))')+
  guides(color=guide_legend('房屋用途'))

grid.arrange(min_1500,min_2000,min_3000)
#indicate that no obvious trend in the plot

#################### boxplot (Visualized by product type & radiation raion ########################)
### House price by radiation raion & product type ###
radiation_price=ggplot(estate,aes(x=type,y=log(data$price/10000),fill=radiation_raion))+
  geom_boxplot()+
  ylim(c(5,8))+
  labs(x='房屋用途',y='log(價格(萬元))')+
  guides(fill=guide_legend('環境是否有核輻射'))
radiation_price

### Different age population against radiation raion & product type ###
baby_all=ggplot(estate,aes(x=type,y=X0_6_all,fill=radiation_raion))+
  geom_boxplot()+
  ylim(c(0,12500))+
  labs(x='房屋用途',y='0-6歲人數')+
  guides(fill=guide_legend('環境是否有核輻射'))

child_all=ggplot(estate,aes(x=type,y=X7_14_all,fill=radiation_raion))+
  geom_boxplot()+
  ylim(c(0,12500))+
  labs(x='房屋用途',y='7-14歲人數')+
  guides(fill=guide_legend('環境是否有核輻射'))

teen_all=ggplot(estate,aes(x=type,y=X16_29_all,fill=radiation_raion))+
  geom_boxplot()+
  ylim(c(0,30000))+
  labs(x='房屋用途',y='16-29歲人數')+
  guides(fill=guide_legend('環境是否有核輻射'))

city_all=ggplot(estate,aes(x=type,y=full_all,fill=radiation_raion))+
  geom_boxplot()+
  ylim(c(0,200000))+
  labs(x='房屋用途',y='位於該都市之總人口數')+
  guides(fill=guide_legend('環境是否有核輻射'))

grid.arrange(baby_all,child_all,teen_all,city_all,ncol=4)
#Interpret what you see, I'm lazy~~~
##################################################################################
#You decide whether to do these independence test, it's up to you
chisq.test(nuclear_reactor_raion,radiation_raion)
chisq.test(thermal_power_plant_raion,radiation_raion)
chisq.test(oil_chemistry_raion,radiation_raion)

#############################################################################################################

#data scaling & correlation
scale_data=scale(data$all)
scale_data1=scale(data$`n>>p`)
scale_data2=scale(data$`n==p`)
scale_data3=scale(data$`n<<p`)

correlation=cor(data$all) 
######################################data n>>p#######################################
############################ PCA ################################
pca1=PCA(scale_data1,ncp=ncol(scale_data1),graph=F)
eig.val1=get_eigenvalue(pca1)
cummulative_variance1=eig.val1[,3] #choose 10 PCs
fviz_eig(pca1, addlabels = TRUE, ylim = c(0, 50)) #visualize scree plot

### Variables
var1=get_pca_var(pca1)
contribute_pc1=fviz_contrib(pca1, choice = "var", axes = 1, top = 10)
contribute_pc2=fviz_contrib(pca1, choice = "var", axes =2 , top = 10)
contribute_pc3=fviz_contrib(pca1, choice = "var", axes =3 , top = 10)
contribute_pc4=fviz_contrib(pca1, choice = "var", axes =4 , top = 10)
grid.arrange(contribute_pc1,contribute_pc2,contribute_pc3,contribute_pc4,ncol=2)

var.kms1=kmeans(var1$coord, centers = 3, nstart = 25)
kms.grp1=as.factor(var.kms1$cluster)
fviz_pca_var(pca1, col.var = kms.grp1, palette = c("blue", "green", "red"),
             legend.title = "Cluster")

###Individuals
ind1=get_pca_ind(pca1)
pca.data1.center=matrix(c(-2,0,10,0,12,-3,27,7),ncol=2,byrow=T)
ind.kms1=kmeans(ind1$coord[,1:2], centers=pca.data1.center, nstart = 25)
kms.grp.ind1=as.factor(ind.kms1$cluster)
#fviz_pca_ind(pca, col.ind = kms.grp.ind, palette = c("blue", "green", "red"),
#   repel = TRUE)
plot(ind1$coord[,1],ind1$coord[,2],col=kms.grp.ind1,xlab='PC1',
     ylab='PC2',main='PCA for n>>p data using kmeans')

dimensionreduction_pca_data1=ind1$coord[,1:10]

coranking_pca_data1=readRDS("DimReduction/coranking pca for data1.rds")
#imageplot(coranking_pca_data1)
lcmc_pca_data1=LCMC(coranking_pca_data1,K=1:10);lcmc_pca_data1
############################ MDS ################################
dist1=readRDS("DimReduction/distance for data1.rds")
mds1=readRDS("DimReduction/MDS for data1.rds")
mds.data1.center=matrix(c(-25,7,-12,-3,-8.5,2.5,3,0),ncol=2,byrow=T)
mds.kms1=kmeans(mds1$points[,1:2], centers = mds.data1.center, nstart = 25)
plot(mds1$points[,1],mds1$points[,2],col=as.factor(mds.kms1$cluster),
     xlab='MDS1',ylab='MDS2',main='MDS for n>>p data using kmeans')

dimensionreduction_mds_data1=mds1$points[,1:2]

coranking_mds_data1=readRDS("DimReduction/coranking mds for data1.rds")
#imageplot(coranking_mds_data1)
lcmc_mds_data1=LCMC(coranking_mds_data1,K=1:10);lcmc_mds_data1
############################ ISOMAP ################################
#iso1=isomap(dist1,k=1500) #testing for k=1500,2500,5000
iso1=readRDS("DimReduction/isomap for data1.rds") #k=2500
#iso1=isomap(dist1,k=5000)
plot(iso1$points[,1],iso1$points[,2],
     xlab='ISO1',ylab='ISO2',main='k=2500')
iso.data1.center=matrix(c(-30,7,-18,-10,-12,5,5,0),ncol=2,byrow=T)
iso.kms1=kmeans(iso1$points[,1:2], centers =iso.data1.center,nstart = 25)
plot(iso1$points[,1],iso1$points[,2],col=as.factor(iso.kms1$cluster),
     xlab='ISO1',ylab='ISO2',main='ISOMAP for n>>p data using kmeans')

dimensionreduction_iso_data1=iso1$points[,1:2]
coranking_iso_data1=readRDS("DimReduction/coranking isomap for data1.rds")
#coranking_iso_data1=coranking(scale_data1,dimensionreduction_iso_data1,input='data')
#saveRDS(coranking_iso_data1,'F:\\HDDA\\Final Report Data\\rds & code & plot\\ISOMAP for data1.rds')
#imageplot(coranking_iso_data1)
lcmc_iso_data1=LCMC(coranking_iso_data1,K=1:10);lcmc_iso_data1
#######################################data n==p####################################
############################ PCA ###################################
pca_corr2=cor(scale_data2)
pca_corr2_shrink=cor.shrink(scale_data2)
pca_result2=c(sum((pca_corr2-correlation)^2),sum((pca_corr2_shrink-correlation)^2))
names(pca_result2)=c('empirical mean square error','shrinkage mean square error');pca_result2
par(mfrow=c(1,3))
image(t(correlation)[,ncol(scale_data2):1], main="true cov", xaxt="n", yaxt="n")
image(t(pca_corr2)[,ncol(scale_data2):1], main="empirical cov", xaxt="n", yaxt="n")
image(t(pca_corr2_shrink)[,ncol(scale_data2):1], main="shrinkage cov", xaxt="n", yaxt="n") # no need to do shrinkage
dev.off()

pca2=PCA(scale_data2,ncp=ncol(scale_data2),graph=F)
eig.val2=get_eigenvalue(pca2)
cummulative_variance2=eig.val2[,3] #choose 10 PCs
fviz_eig(pca2, addlabels = TRUE, ylim = c(0, 50)) #visualize scree plot

var2=get_pca_var(pca2)
var.kms2=kmeans(var2$coord, centers = 3, nstart = 25)
kms.grp2=as.factor(var.kms2$cluster)
fviz_pca_var(pca2, col.var = kms.grp2, palette = c("blue", "green", "red"),
             legend.title = "Cluster")

###Individuals
ind2=get_pca_ind(pca2)
ind.kms2=kmeans(ind2$coord, centers=3, nstart = 25)
kms.grp.ind2=as.factor(ind.kms2$cluster)
#fviz_pca_ind(pca2, col.ind = kms.grp.ind2, palette = c("blue", "green", "red"),
#   repel = TRUE)
plot(ind2$coord[,1:2],col=as.factor(kms.grp.ind2),
     xlab='PC1',ylab='PC2',main='PCA for n==p data using kmeans')

dimensionreduction_pca_data2=ind2$coord[,1:10]

coranking_pca_data2=coranking(scale_data2,dimensionreduction_pca_data2,input='data')
imageplot(coranking_pca_data2)
lcmc_pca_data2=LCMC(coranking_pca_data2,K=1:10);lcmc_pca_data2
############################ MDS ###################################
dist2=dist(scale_data2)
mds2=cmdscale(dist2,k=2,eig=T)
mds.data2.center=matrix(c(-10,0,4,0,7,7),ncol=2,byrow=T)
mds.kms2=kmeans(cbind(mds2$points[,1],mds2$points[,2]), centers = mds.data2.center, nstart = 25)
plot(mds2$points[,1],mds2$points[,2],col=as.factor(mds.kms2$cluster),
     xlab='MDS1',ylab='MDS2',main='MDS for n==p data using kmeans')

dimensionreduction_mds_data2=mds2$points[,1:2]

coranking_mds_data2=coranking(scale_data2,dimensionreduction_mds_data2,input='data')
imageplot(coranking_mds_data2)
lcmc_mds_data2=LCMC(coranking_mds_data2,K=1:10);lcmc_mds_data2
############################ ISOMAP ###################################
par(mfrow=c(2,2))
for (i in 5:8){
  iso2=isomap(dist2,k=i)
  plot(iso2$points[,1],iso2$points[,2],
       xlab='ISO1',ylab='ISO2',main=paste('k=',i,sep=''))
} #pick k=6
dev.off()
iso2=isomap(dist2,k=6)
iso.data2.center=matrix(c(-37,0,0,12,17,0),ncol=2,byrow=T)
iso.kms2=kmeans(iso2$points[,1:2], centers =iso.data2.center,nstart = 25)
plot(iso2$points[,1],iso2$points[,2],col=as.factor(iso.kms2$cluster),
     xlab='ISO1',ylab='ISO2',main='ISOMAP for n==p data using kmeans')

dimensionreduction_iso_data2=iso2$points[,1:2]

coranking_iso_data2=coranking(scale_data2,dimensionreduction_iso_data2,input='data')
imageplot(coranking_iso_data2)
lcmc_iso_data2=LCMC(coranking_iso_data2,K=1:10);lcmc_iso_data2
######################################data n<<p#######################################
############################ PCA ################################
pca_corr3=cor(scale_data3)
pca_corr3_shrink=cor.shrink(scale_data3)
pca_result3=c(sum((pca_corr3-correlation)^2),sum((pca_corr3_shrink-correlation)^2))
names(pca_result3)=c('empirical mean square error','shrinkage mean square error');pca_result3
windows()
par(mfrow=c(1,3))
image(t(correlation)[,ncol(scale_data3):1], main="true cov", xaxt="n", yaxt="n")
image(t(pca_corr3)[,ncol(scale_data3):1], main="empirical cov", xaxt="n", yaxt="n")
image(t(pca_corr3_shrink)[,ncol(scale_data3):1], main="shrinkage cov", xaxt="n", yaxt="n") # need to do shrinkage
dev.off()

true_correlation_eigenvalue=eigen(correlation)$values
empirical_correlation_eigenvalue=eigen(pca_corr3)$values
shrinkage_correlation_eigenvalue=eigen(pca_corr3_shrink)$values
matplot(data.frame(true_correlation_eigenvalue, empirical_correlation_eigenvalue, 
                   shrinkage_correlation_eigenvalue), type = "l", ylab="eigenvalues", lwd=2)
legend("top", legend=c("true", "empirical", "shrinkage"), lwd=2, lty=1:3, col=1:3)

cummulative_variance3_shrinkage=cumsum(shrinkage_correlation_eigenvalue)/
  sum(shrinkage_correlation_eigenvalue) 
cummulative_variance3_true=cumsum(true_correlation_eigenvalue)/
  sum(true_correlation_eigenvalue)
matplot(data.frame(cummulative_variance3_true,cummulative_variance3_shrinkage), 
        type = "l", ylab="eigenvalues", lwd=2,main='cummulative proportions of eigenvalues')
abline(0.7,0,col='grey') #choose K=16
legend("bottomright", legend=c("true", "shrinkage"), lwd=2, col=c('black','red'))

dimensionreduction_pca_data3=scale_data3%*%eigen(pca_corr3_shrink)$vectors[,1:16]
plot(dimensionreduction_pca_data3[,1:2],
     xlab='PC1',ylab='PC2',main='PCA for n<<p data')

coranking_pca_data3=coranking(scale_data3,dimensionreduction_pca_data3,input='data')
imageplot(coranking_pca_data3)
lcmc_pca_data3=LCMC(coranking_pca_data3,K=1:10);lcmc_pca_data3
############################ MDS ###################################
dist3=dist(scale_data3)
mds3=cmdscale(dist3,k=2,eig=T)
plot(mds3$points[,1],mds3$points[,2],
     xlab='MDS1',ylab='MDS2',main='MDS for n<<p data')

dimensionreduction_mds_data3=mds3$points[,1:2]

coranking_mds_data3=coranking(scale_data3,dimensionreduction_mds_data3,input='data')
imageplot(coranking_mds_data3)
lcmc_mds_data3=LCMC(coranking_mds_data3,K=1:10);lcmc_mds_data3
############################ ISOMAP ###################################
par(mfrow=c(2,5))
for (i in 4:13){
  iso3=isomap(dist3,k=i)
  plot(iso3$points[,1],iso3$points[,2],
       xlab='ISO1',ylab='ISO2',main=paste('k=',i,sep=''))
} #pick k=12
dev.off()
iso3=isomap(dist3,k=12)
plot(iso3$points[,1],iso3$points[,2],
     xlab='ISO1',ylab='ISO2',main='ISOMAP for n<<p data')

dimensionreduction_iso_data3=iso3$points[,1:2]

coranking_iso_data3=coranking(scale_data3,dimensionreduction_iso_data3,input='data')
imageplot(coranking_iso_data3)
lcmc_iso_data3=LCMC(coranking_iso_data3,K=1:10);lcmc_iso_data3



################################Generalized the lcmc result##################################################
###By data size
n_bigger_p=matrix(,nrow=3,ncol=10)
n_bigger_p[1,]=lcmc_pca_data1
n_bigger_p[2,]=lcmc_mds_data1
n_bigger_p[3,]=lcmc_iso_data1
matplot(t(n_bigger_p), type = "l",xlab='k neighbor',ylab="LCMC", 
        lwd=2,xlim=c(0,10),ylim=c(0,1),main='Different method performance under n>>p')
legend("bottom", legend=c("PCA", "MDS", "ISOMAP"), lwd=2, lty=1:3, col=1:3)

n_equal_p=matrix(,nrow=3,ncol=10)
n_equal_p[1,]=lcmc_pca_data2
n_equal_p[2,]=lcmc_mds_data2
n_equal_p[3,]=lcmc_iso_data2
matplot(t(n_equal_p), type = "l",xlab='k neighbor',ylab="LCMC", 
        lwd=2, xlim=c(0,10),ylim=c(0,1),main='Different method performance under n==p')
legend("top", legend=c("PCA", "MDS", "ISOMAP"), lwd=2, lty=1:3, col=1:3)

n_smaller_p=matrix(,nrow=3,ncol=10)
n_smaller_p[1,]=lcmc_pca_data3
n_smaller_p[2,]=lcmc_mds_data3
n_smaller_p[3,]=lcmc_iso_data3
matplot(t(n_smaller_p), type = "l",xlab='k neighbor',ylab="LCMC", 
        lwd=2,xlim=c(0,10),ylim=c(0,1),main='Different method performance under n<<p')
legend("top", legend=c("PCA", "MDS", "ISOMAP"), lwd=2, lty=1:3, col=1:3)

###By method
pca_lcmc=matrix(,nrow=3,ncol=10)
pca_lcmc[1,]=lcmc_pca_data1
pca_lcmc[2,]=lcmc_pca_data2
pca_lcmc[3,]=lcmc_pca_data3
matplot(t(pca_lcmc), type = "l",xlab='k neighbor',ylab="LCMC", 
        lwd=2,xlim=c(0,10),ylim=c(0,1),main='PCA performance under different data size')
legend("top", legend=c("n>>p", "n==p", "n<<p"), lwd=2, lty=1:3, col=1:3)

mds_lcmc=matrix(,nrow=3,ncol=10)
mds_lcmc[1,]=lcmc_mds_data1
mds_lcmc[2,]=lcmc_mds_data2
mds_lcmc[3,]=lcmc_mds_data3
matplot(t(mds_lcmc), type = "l",xlab='k neighbor',ylab="LCMC", 
        lwd=2,xlim=c(0,10),ylim=c(0,1),main='MDS performance under different data size')
legend("top", legend=c("n>>p", "n==p", "n<<p"), lwd=2, lty=1:3, col=1:3)

iso_lcmc=matrix(,nrow=3,ncol=10)
iso_lcmc[1,]=lcmc_iso_data1
iso_lcmc[2,]=lcmc_iso_data2
iso_lcmc[3,]=lcmc_iso_data3
matplot(t(iso_lcmc), type = "l",xlab='k neighbor',ylab="LCMC", 
        lwd=2,xlim=c(0,10),ylim=c(0,1),main='ISOMAP performance under different data size')
legend("top", legend=c("n>>p", "n==p", "n<<p"), lwd=2, lty=1:3, col=1:3)




##############################################################################
#model predicting

price_predict_test=origin$price_doc[as.numeric(rownames(data$`n>>p`))]
a=summary(lm(price~.,data=as.data.frame
             (testdata)))


plot(ind1$coord[,1],ind1$coord[,2],col=price_predict_test,xlab='PC1',
     ylab='PC2',main='PCA for n>>p data using kmeans')











