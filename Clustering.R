rm(list=ls())


if (!'rstudioapi' %in% installed.packages()){
  install.packages('rstudioapi')
}
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


finalDenver <- read.csv("denver additional.csv")
head(finalDenver)
clusterDenver <- finalDenver[,-c(1:4,6,9,10,14:17)] #removing non numerical columns, including binary and cateorical columns.
#exploring the dataset
head(clusterDenver)
ncol(clusterDenver)
str(clusterDenver) 

#scaling the data
clusterDenverNorm = scale(clusterDenver)# calculates z score-scaling

#hierarchial clustering
head(clusterDenverNorm)
distance = dist(clusterDenverNorm, method = 'euclidean')
hcluster = hclust(distance,method='ward.D')
plot(hcluster)
clusterGroups = cutree(hcluster,k=5) 
table(clusterGroups)
str(clusterGroups)

n = length(clusterDenver)
a = matrix(0,n,5)

for (i in 1:length(clusterDenver)){
  a[i,] = tapply(clusterDenver[[i]], clusterGroups, mean)
}
rownames(a) <- names(clusterDenver)
print(a)

#creating a column in our final data frame called" cluster_num"
finalDenver$cluster_num <- clusterGroups
cluster_cordinates <- finalDenver[,c(12,13,27)]

write.csv(cluster_cordinates, file="cluster_cordinates.csv")


# to rank the clusters according to different variables, with 1-5 being lowest to highest:
colnames(a) <- c("cluster1","cluster2","cluster3","cluster4","cluster5")
hcdenver <- apply(a,1,rank) # created a df with rankings acc to variables
hcdenver <- hcdenver[,-(5:6)] #removing lat-long columns
apply(a,1,which.max) # seeing which cluster is max for which variable



# kmeans clustering:

set.seed(88)
KMC = kmeans(clusterDenverNorm, centers = 5, iter.max = 1000)
str(KMC)

n2 = length(clusterDenver)
b = matrix(0,n,5)
for (i in 1:length(clusterDenver)){
  b[i,] = tapply(clusterDenver[[i]], KMC$cluster, mean)
}
rownames(b) <- names(clusterDenver)
print(b)


#creating a df containing values of lat_long of cluster 5:
indexes5 <- which(clusterGroups==5)
cluster5_cordinates <- clusterDenver[indexes5,5:6] #5,6 are lat-long columns
length(indexes5)
write.csv(cluster5_cordinates, file="cluster5_cordinates.csv") # to plot in tableu 

#creating a df containing values of lat_long of cluster 4:
indexes4 <- which(clusterGroups==4)
cluster4_cordinates <- clusterDenver[indexes4,5:6] #5,6 are lat-long columns
length(indexes4)
write.csv(cluster4_cordinates, file="cluster4_cordinates.csv") # to plot in tableu 

#creating a df containing values of lat_long of cluster 3:
indexes3 <- which(clusterGroups==3)
cluster3_cordinates <- clusterDenver[indexes3,5:6] #5,6 are lat-long columns
length(indexes3)
write.csv(cluster3_cordinates, file="cluster3_cordinates.csv") # to plot in tableu 

#creating a df containing values of lat_long of cluster 2:
indexes2 <- which(clusterGroups==2)
cluster2_cordinates <- clusterDenver[indexes2,5:6] #5,6 are lat-long columns
length(indexes2)
write.csv(cluster2_cordinates, file="cluster2_cordinates.csv") # to plot in tableu 

#creating a df containing values of lat_long of cluster 1:
indexes1 <- which(clusterGroups==1)
cluster1_cordinates <- clusterDenver[indexes1,5:6] #5,6 are lat-long columns
length(indexes1)
write.csv(cluster1_cordinates, file="cluster1_cordinates.csv") # to plot in tableu 


#since we are proceeding with hierarchial clustering, taking our best cluster, cluster 4 for regression to explore the cluster further:
#by interpretation of clusters by looking at "hcdenver" df, it was found that cluster 4 is the best from the point of view of an investor. 


cluster4df <- finalDenver[indexes4,] # clsuter 4 df created
nrow(cluster4df)
head(cluster4df)
cluster4df <- cluster4df[,-c(1:4,6,9,12:14,18:26)] # removing columns not reqd such as name, link, zipcode,etc and all other cluster common colmns from 18-26
names(cluster4df)
na_rows <- which(is.na(cluster4df$neighbourhood)) # number of rows having NAs is 9. removing 9 rows from df.
cluster4df <- cluster4df[-na_rows,]
nrow(cluster4df) # num of rows now 188, 8 columns.

#cleaning neighbourhoods column:
cluster4df$neighbourhood<- as.factor(sub(",.*", "", cluster4df$neighbourhood)) #converting to a factor too 
head(cluster4df$neighbourhood) 
table(cluster4df$neighbourhood) #there are six neighborhoods
names(finalDenver)
#regression:
str(cluster4df)

#splitting the data set into training and test:
rownum_train <- abs(sample(nrow(cluster4df), (nrow(cluster4df))*0.60)) # due to lesser number of rows in the dataset- 188, splitting into 60:40
length(rownum_train) #112 rows
train <-  cluster4df[rownum_train,]
test <- cluster4df[-train,]
nrow(test) #76 rows


#regression:
lm.fit1 <- lm(rating~., data=train)
summary(lm.fit1)
#Multiple R-squared:  0.317,	Adjusted R-squared:  0.1286 
# data transformation
lm.fit2 = lm(rating~price_range+log(reviews)+neighbourhood+subcat_num+ log(dayselapsed)+final_category+branch, data=train)
summary(lm.fit2)
#Multiple R-squared:  0.2062,	Adjusted R-squared:  0.0893 , r squared reduced

lm.fit3 = lm(log(rating)~price_range+log(reviews)+neighbourhood+subcat_num+(dayselapsed)+final_category+branch, data=train)
summary(lm.fit3)
#Multiple R-squared:  0.2388,	Adjusted R-squared:  0.1267 

lm.fit4 = lm(log(rating)~price_range+(reviews)+neighbourhood+subcat_num+(dayselapsed)+final_category+branch, data=train)
summary(lm.fit4)
#Multiple R-squared:  0.3232,	Adjusted R-squared:  0.1365 

lm.fit5 = lm(log(rating)~log(price_range)+(reviews)+neighbourhood+subcat_num+(dayselapsed)+final_category+branch, data=train)
summary(lm.fit5)
#Multiple R-squared:  0.3212,	Adjusted R-squared:  0.134


lm.fit4 = lm(log(rating)~price_range+(reviews)+neighbourhood+subcat_num+(dayselapsed)+final_category+branch, data=train)
summary(lm.fit4)
#Multiple R-squared:  0.3232,	Adjusted R-squared:  0.1365 

# predicitng on the test set to calculate mean squared error:
mean(test$rating-predict(lm.fit4,test)^2)

#MSE=  2.152119, MSE is higher. Predictive accuracy of model is average.
#can't use cross validation because our test and training sets had different categories (total 17 categories), so cv.glm was not able to predict test data.


# 10-fold cross validation
library(boot)
set.seed(300)
lm.fit = glm(log(rating)~price_range+(reviews)+neighbourhood+subcat_num+(dayselapsed)+final_category+branch, data=train)
cv.error = cv.glm(train,lm.fit)$delta[1] 
summary(lm.fit)
#AIC: -168.97

#getting the following error:
#"Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
#factor final_category has new level Indian"

#best model with highest r sqaured:
lm.fit6 = lm(log(rating)~price_range+(reviews)+neighbourhood*final_category+subcat_num+(dayselapsed)+branch, data=train)
summary(lm.fit6)

#Multiple R-squared:  0.5311,	Adjusted R-squared:  0.2232  









