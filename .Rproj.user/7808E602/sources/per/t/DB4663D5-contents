################################################################################################
###Whole data (data$all) #The three variable multicollinearity(107,109,111)
whole_data=as.data.frame(scale(cbind(data$all[,-c(107,109,111)],data$price)))
colnames(whole_data)[112]='price'
ls_lm=lm(price ~.,data=whole_data)
summary(ls_lm)

sum((ls_lm$residuals)^2)
######model predicting n>>p (use mds as example demonstrate poor result)#######################
price_n_bigger_p=scale(origin$price_doc[as.numeric(rownames(data$`n>>p`))])
data_predict_mds_n_bigger_p=as.data.frame(cbind(dimensionreduction_mds_data1,price_n_bigger_p))
names(data_predict_mds_n_bigger_p)[3]='price'
ggplot(data=data_predict_mds_n_bigger_p,aes(x=V1,y=V2))+
  geom_point(size=1,aes(color=scale(price_n_bigger_p)))+
  scale_color_gradient('房價(標準化)',low='#0000FF',high='#CC0000')+
  labs(x='MDS1',y='MDS2')
ggpairs(data=data_predict_mds_n_bigger_p)

###mds as example
reg_bigger_mds=lm(price~.,data=data_predict_mds_n_bigger_p)
summary(reg_bigger_mds)
sum((reg_bigger_mds$residuals)^2)
###So we won't focus on n>>p and n==p condition

library(olsrr)
library(glmnet)
library(selectiveInference)
library(caret)
library(cmprsk)
library(parcor)
###################model predicting n<<p (use mds as example demonstrate poor result)#######################
####Use traditional linear regression (fail)
price_n_smaller_p=origin$price_doc[as.numeric(rownames(data$`n<<p`))]
data_predict_n_smaller_p=scale(cbind(data$`n<<p`[,-c(107,109,111)],price_n_smaller_p))
test1=data_predict_n_smaller_p
lm_smaller=lm(price_n_smaller_p~.,data=as.data.frame(test1))
summary(lm_smaller)

###Using stepwise, backward, forward elimination (fail)
ols_step_backward_p(lm_smaller) #error because of multicollinerity


####Use dimension reduction data (Using mds as demonstration to explain the poor performance)
###lasso
lasso.cv=cv.glmnet(test1[,-112],test1[,112],
                   nfolds=nrow(data_predict_n_smaller_p), grouped=F,
                   type.measure="mse")
plot(lasso.cv)
axis(side=1, at=log(lasso.cv$lambda.min), labels=round(log(lasso.cv$lambda.min),2))
#coef(lasso.cv, s="lambda.min") #coefficients of cross validation model 

lasso=glmnet(test1[,-112],test1[,112],alpha=1, nlambda=100)
lasso.sigma=estimateSigma(test1[,-112],test1[,112])
lasso.beta = coef(lasso, s=lasso.cv$lambda.min)[-1]

lasso.inference = fixedLassoInf(test1[,-112],test1[,112],
                                lasso.beta,lasso.cv$lambda.min,sigma=lasso.sigma$sigmahat)
lasso.inference

correlation_test1=cor(test1[,-112])  #calculate pairwise correlation
test1_pheatmap=pheatmap(correlation_test1)

#plot(ls_lm$coefficients[lasso.inference$vars],lasso.inference$coef0)
#abline(0,1)
#text(ls_lm$coefficients[lasso.inference$vars]+0.0035,lasso.inference$coef0+0.01,
#	labels=lasso.inference$vars)

plot(lasso, xvar="lambda", label=TRUE, ylab=c("Lasso Coefficient")) #係數壓縮情形

#when the lasso fails
##https://insightr.wordpress.com/2017/06/14/when-the-lasso-fails/?utm_campaign=News&utm_medium=Community&utm_source=DataCamp.com

#must use adaptive lasso or others