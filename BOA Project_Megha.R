getwd()
bank of america.raw<-read.csv("Bank of America.csv")
View(bank of america.raw)

#Find the dimensions of this data set.
library(psych)
describe(bank of america.raw) 
dim(bank of america.raw)

#Summarize the data and showcase the structure of this dataset.
str(bank of america.raw)
summary(bank of america.raw)

#Visualize the PURCHASES using histogram
hist(bank of america.raw$PURCHASES)
hist(log(bank of america.raw$PURCHASES), xlim = c(0, 15))

#Remove the first column (CUST_ID) and known segments
bank of america.df <- bank of america.raw[,-2]
str(bank of america.df)
summary(bank of america.df)

#Create a function to aggregate the mean value by group
bank of america.mean <- aggregate (. ~ TENURE , data=bank of america.df, mean)
bank of america.mean
rownames(bank of america.mean) <- bank of america.mean [ , 1]   
bank of america.mean <- bank of america.mean [ , -1] 
bank of america.mean

#Heatmaps
library(gplots)
library(RColorBrewer)
heatmap.2(as.matrix(bank of america.mean),
          col=brewer.pal(9, "GnBu") , trace="none" , key=FALSE , dend="none",
          main="\n\n\n Brand attributes")

# remove the known segment assignments
bank of america.df<-bank of america.df[,-18]
str(bank of america.df)


bank of america.summ<-function(data,groups){
  aggregate(data,list(groups),
            function(x)mean(as.numeric(x)))
}
bank of america.summ(bank of america.df, bank of america.raw$TENURE)
#Summarize the data and showcase the structure of this dataset.


#hclust()
#Computes euclidian distance  
library(cluster)
bank of america.dist<-daisy(bank of america.df)
as.matrix(bank of america.dist)[1:4,1:4]

#Hierarchical clustering
bank of america.hc<-hclust(bank of america.dist, method = "complete")
plot(bank of america.hc)
#Examining Similarities
plot(cut(as.dendrogram(bank of america.hc), h=0.5)$lower[[1]])
#Comparing the dendrogram to the distance matrix
#In this case, CPCC > 0.5 indicates a relatively strong fit 
cor(cophenetic(bank of america.hc), bank of america.dist)

#Getting K groups from the tree
plot(bank of america.hc)
rect.hclust(bank of america.hc, k = 4, border = "Green")
#Getting segment membership
bank of america.hc.segment<-cutree(bank of america.hc, k = 4)
table(bank of america.hc.segment)
bank of america.summ(bank of america.df, bank of america.hc.segment)
#Is the result interesting?
#No
plot(jitter(as.numeric(bank of america.df$PURCHASES)) ~ jitter(as.numeric(bank of america.df$BALANCE)),      
     col=bank of america.hc.segment, yaxt="n", xaxt="n", ylab="", xlab="")

#kmeans()
#Remove the first column (CUST_ID)
bank of america.df.num <- bank of america.df
summary(bank of america.df.num)
str(bank of america.df.num)

#Find the K-means groups
set.seed(96743)  
bank of america.k<-kmeans(bank of america.df.num, centers = 4)
bank of america.summ(bank of america.df, bank of america.k$cluster)
#Comparing groups on 1 variable
boxplot(bank of america.df.num$PURCHASES~bank of america.k$cluster,
        xlab = "Purchases",
        ylab = "Segment",
        horizontal=TRUE)
#Visualizing the overall clusters
library(cluster)
clusplot(bank of america.df, bank of america.k$cluster, color = TRUE, 
         Shade = TRUE, labels = 4,
         main = "k-means cluster plot")
#mclust()
#Mclust for 6 groups
library(mclust)
bank of america.mc<-Mclust(bank of america.df.num)
summary(bank of america.mc)
#Mclust for 4 groups
bank of america.mc4<-Mclust(bank of america.df.num, G = 4)
summary(bank of america.mc4)
#Higher log-likelihood and lower BIC is better, so 4 clustering table is better
BIC(bank of america.mc, bank of america.mc4)
bank of america.summ(bank of america.df, bank of america.mc$class)
library(cluster)
clusplot(bank of america.df, bank of america.mc$class, color=TRUE, shade=TRUE, labels=4, main="Model-based cluster plot")

#Polytomous analysis
bank of america.df.cut <- bank of america.df
bank of america.df.cut$BALANCE<-factor(ifelse(bank of america.df$BALANCE<median(bank of america.df$BALANCE),
                                  "LessBALANCE", "MoreBALANCE")) 
bank of america.df.cut$BALANCE_FREQUENCY<-factor(ifelse(bank of america.df$BALANCE_FREQUENCY<median(bank of america.df$BALANCE_FREQUENCY), 
                                            "LessBALANCE_FREQUENCY", "MoreBALANCE_FREQUENCY")) 
bank of america.df.cut$PURCHASES<-factor(ifelse(bank of america.df$PURCHASES<median(bank of america.df$PURCHASES),
                                    "LessPURCHASES", "MorePURCHASES")) 
bank of america.df.cut$ONEOFF_PURCHASES<-factor(ifelse(bank of america.df$ONEOFF_PURCHASES<median(bank of america.df$ONEOFF_PURCHASES),
                                           "LessONEOFF_PURCHASES", "MoreONEOFF_PURCHASES")) 
bank of america.df.cut$INSTALLMENTS_PURCHASES<-factor(ifelse(bank of america.df$INSTALLMENTS_PURCHASES<median(bank of america.df$INSTALLMENTS_PURCHASES), "LessINSTALLMENTS_PURCHASES", "MoreINSTALLMENTS_PURCHASES")) 
bank of america.df.cut$CASH_ADVANCE<-factor(ifelse(bank of america.df$CASH_ADVANCE<median(bank of america.df$CASH_ADVANCE),
                                       "LessCASH_ADVANCE", "MoreCASH_ADVANCE")) 
bank of america.df.cut$PURCHASES_FREQUENCY<-factor(ifelse(bank of america.df$PURCHASES_FREQUENCY<median(bank of america.df$PURCHASES_FREQUENCY),
                                              "LessPURCHASES_FREQUENCY", "MorePURCHASES_FREQUENCY")) 
bank of america.df.cut$ONEOFF_PURCHASES_FREQUENCY<-factor(ifelse(bank of america.df$ONEOFF_PURCHASES_FREQUENCY<median(bank of america.df$ONEOFF_PURCHASES_FREQUENCY),
                                                     "LessONEOFF_PURCHASES_FREQUENCY", "MoreONEOFF_PURCHASES_FREQUENCY")) 
bank of america.df.cut$PURCHASES_INSTALLMENTS_FREQUENCY<-factor(ifelse(bank of america.df$PURCHASES_INSTALLMENTS_FREQUENCY<median(bank of america.df$PURCHASES_INSTALLMENTS_FREQUENCY),
                                                           "LessPURCHASES_INSTALLMENTS_FREQUENCY", "MorePURCHASES_INSTALLMENTS_FREQUENCY")) 
bank of america.df.cut$CASH_ADVANCE_FREQUENCY<-factor(ifelse(bank of america.df$CASH_ADVANCE_FREQUENCY<median(bank of america.df$CASH_ADVANCE_FREQUENCY),
                                                 "LessCASH_ADVANCE_FREQUENCY", "MoreCASH_ADVANCE_FREQUENCY")) 
bank of america.df.cut$CASH_ADVANCE_TRX<-factor(ifelse(bank of america.df$CASH_ADVANCE_TRX<median(bank of america.df$CASH_ADVANCE_TRX), 
                                           "LessCASH_ADVANCE_TRX", "MoreCASH_ADVANCE_TRX")) 
bank of america.df.cut$PURCHASES_TRX<-factor(ifelse(bank of america.df$PURCHASES_TRX<median(bank of america.df$PURCHASES_TRX), 
                                        "LessPURCHASES_TRX", "MorePURCHASES_TRX")) 
bank of america.df.cut$CREDIT_LIMIT<-factor(ifelse(bank of america.df$CREDIT_LIMIT<median(bank of america.df$CREDIT_LIMIT), 
                                       "LessCREDIT_LIMIT", "MoreCREDIT_LIMIT")) 
bank of america.df.cut$PAYMENTS<-factor(ifelse(bank of america.df$PAYMENTS<median(bank of america.df$PAYMENTS), 
                                   "LessPAYMENTS", "MorePAYMENTS")) 
bank of america.df.cut$MINIMUM_PAYMENTS<-factor(ifelse(bank of america.df$MINIMUM_PAYMENTS<median(bank of america.df$MINIMUM_PAYMENTS),
                                           "LessMINIMUM_PAYMENTS", "MoreMINIMUM_PAYMENTS")) 
bank of america.df.cut$PRC_FULL_PAYMENT<-factor(ifelse(bank of america.df$PRC_FULL_PAYMENT<median(bank of america.df$PRC_FULL_PAYMENT),
                                           "LessPRC_FULL_PAYMENT", "MorePRC_FULL_PAYMENT")) 
str(bank of america.df.cut)
summary(bank of america.df.cut)
#Create a reusable model formula
bank of america.f<-with(bank of america.df.cut, cbind(BALANCE, BALANCE_FREQUENCY, PURCHASES, ONEOFF_PURCHASES, INSTALLMENTS_PURCHASES, 
                              CASH_ADVANCE, PURCHASES_FREQUENCY, ONEOFF_PURCHASES_FREQUENCY, PURCHASES_INSTALLMENTS_FREQUENCY, 
                              CASH_ADVANCE_FREQUENCY, CASH_ADVANCE_TRX, PURCHASES_TRX, CREDIT_LIMIT, PAYMENTS, MINIMUM_PAYMENTS,
                              PRC_FULL_PAYMENT)~1) 

#Fit 4- and 6-group models
library(poLCA) 
set.seed(02807) 
bank of america.LCA6 <- poLCA(bank of america.f, data=bank of america.df.cut, nclass=6) 
bank of america.LCA4 <- poLCA(bank of america.f, data=bank of america.df.cut, nclass=4)  
#4-group model had stronger fit
bank of america.LCA6$bic
bank of america.LCA4$bic
#Examine the 4-group model
bank of america.summ(bank of america.df, bank of america.LCA4$predclass)
table(bank of america.LCA4$predclass)
library(cluster)
clusplot(bank of america.df, bank of america.LCA6$predclass, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="LCA plot (K=4)")
#Examine the 6-group model
bank of america.summ(bank of america.df, bank of america.LCA6$predclass)
table(bank of america.LCA6$predclass)
library(cluster)
clusplot(bank of america.df, bank of america.LCA6$predclass, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="LCA plot (K=6)")
#Comparing 4-group vs 6-group models
table(bank of america.LCA4$predclass, bank of america.LCA6$predclass)
library(mclust)
mapClass(bank of america.LCA4$predclass, bank of america.LCA6$predclass)
#"Correlation" for cluster assignments
set.seed(11021) 
random.data<-sample(4, length(bank of america.LCA4$predclass), replace=TRUE) 
adjustedRandIndex(random.data, bank of america.LCA4$predclass)
