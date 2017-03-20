###################################################################################################
##--------------------------Adding Additional columns to the dataframe----------------------------##
###################################################################################################

rm(list=ls())

if (!'rstudioapi' %in% installed.packages()){
  install.packages('rstudioapi')
}
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## scrapped data
denver <- read.csv("scraping denver.csv")


## imputing the NAs in price range with the average of price range
denver$price_range <- ifelse(is.na(denver$price_range),
                             floor(mean(na.omit(denver$price_range))),
                             denver$price_range)

##separating the first category and the subcategory number
restaurant_id <- 0
first_category <- 0
subcat_num<- 0
df_cat_temp <- data.frame(restaurant_id,first_category,subcat_num)

for( i in 1:nrow(denver)){
  restaurant_id <- denver$restaurant_id[i]
  first_category <- unlist(strsplit(as.character(denver$category[i]),","))[1]
  subcat_num <- length(unlist(strsplit(as.character(denver$category[i]),",")))-1
  df2 <- data.frame(restaurant_id,first_category,subcat_num)
  df_cat_temp <- rbind(df_cat_temp,df2)
}


denver <- merge(denver,df_cat_temp)

##----- categorizing based on the first category-------##
denver$final_category <- rep(0,nrow(denver))

#Mexican
denver$final_category <- ifelse(denver$first_category=="NewMexicanCuisine","Mexican",
                                denver$final_category)
nrow(denver[denver$final_category=="Mexican",])
#Fast Food: sandwiches
denver$final_category <- ifelse(denver$first_category=="Sandwiches", "Fast Food", 
                                denver$final_category)
nrow(denver[denver$final_category=="Fast Food",])
#Fast Food: HotDogs
denver$final_category <- ifelse(denver$first_category=="HotDogs", "Fast Food", 
                                denver$final_category)
nrow(denver[denver$final_category=="Fast Food",])
#Fast Food: Pizza
denver$final_category <- ifelse(denver$first_category=="Pizza", "Fast Food", 
                                denver$final_category)
nrow(denver[denver$final_category=="Fast Food",])
#Fast Food: Burgers
denver$final_category <- ifelse(denver$first_category=="Burgers", "Fast Food", 
                                denver$final_category)
nrow(denver[denver$final_category=="Fast Food",])
#Bars- Chicken Wings
denver$final_category <- ifelse(denver$first_category=="ChickenWings", "Bars", 
                                denver$final_category)
nrow(denver[denver$final_category=="Bars",])
#bars, added Beer, BeerBar, Breweries, CocktailBars, Gastropubs, DiveBars                                             
denver$final_category <- ifelse(denver$first_category=="SportsBars"|
                                  denver$first_category=="TikiBars"|
                                  denver$first_category=="WineBars"|
                                  denver$first_category=="Pubs"|
                                  denver$first_category=="Beer"|
                                  denver$first_category=="BeerBar"|
                                  denver$first_category=="CocktailBars"|
                                  denver$first_category=="Gastropubs"|
                                  denver$first_category=="DiveBars"|
                                  denver$first_category=="Breweries", "Bars", 
                                denver$final_category)
nrow(denver[denver$final_category=="Bars",])
#American
denver$final_category <- ifelse(denver$first_category=="Barbeque"|
                                  denver$first_category=="American(Traditional)"|
                                  denver$first_category=="American(New)"|
                                  denver$first_category=="Southern"|
                                  denver$first_category=="Steakhouses","American", 
                                denver$final_category)
nrow(denver[denver$final_category=="American",])
#cafes, added bagels, bakeries, cafes, creperies
denver$final_category <- ifelse(denver$first_category=="Breakfast&Brunch"|
                                  denver$first_category=="Coffee&Tea"|
                                  denver$first_category=="Creperies"|
                                  denver$first_category=="Bagels"|
                                  denver$first_category=="Bakeries"|
                                  denver$first_category=="Cafes","Cafe", 
                                denver$final_category)
nrow(denver[denver$final_category=="Cafe",])
#Delis:
denver$final_category <- ifelse(denver$first_category=="Salad"|
                                  denver$first_category=="Cheesesteaks","Delis", 
                                denver$final_category)
nrow(denver[denver$final_category=="Delis",])
#mediteranean, added lebanese
denver$final_category <- ifelse(denver$first_category=="MiddleEastern"|
                                  denver$first_category=="Lebanese","Mediterranean", 
                                denver$final_category)
nrow(denver[denver$final_category=="Mediterranean",])
#Caribbean
denver$final_category <- ifelse(denver$first_category=="Cuban","Caribbean", 
                                denver$final_category)
nrow(denver[denver$final_category=="Caribbean",])
#Spanish, added Tex-Mex as Spanish not mexican
denver$final_category <- ifelse(denver$first_category=="Tex-Mex"|
                                  denver$first_category=="TapasBars"|
                                  denver$first_category=="Tapas/SmallPlates","Spanish", 
                                denver$final_category)
nrow(denver[denver$final_category=="Spanish",])
#Chinese, added "AsianFusion"
denver$final_category <- ifelse(denver$first_category=="DimSum"|
                                  denver$first_category=="AsianFusion","Chinese", 
                                denver$final_category)
nrow(denver[denver$final_category=="Chinese",])
#Japanese : "Ramen" which was under chinese, is Japanese cuisine.. so put here
denver$final_category <- ifelse(denver$first_category=="Ramen"|
                                  denver$first_category=="SushiBars","Japanese", 
                                denver$final_category)
nrow(denver[denver$final_category=="Japanese",])
#Others
denver$final_category <- ifelse(denver$first_category=="Seafood"|
                                  denver$first_category=="Vegan"|
                                  denver$first_category=="African"|
                                  denver$first_category=="Russian"|
                                  denver$first_category=="Hawaiian"|
                                  denver$first_category=="Food"|
                                  denver$first_category=="Vegetarian","Others", 
                                denver$final_category)
nrow(denver[denver$final_category=="Others",])
#FoodTrucks
denver$final_category <- ifelse(denver$first_category=="FoodStands"|
                                  denver$first_category=="FoodTrucks"|
                                  denver$first_category=="StreetVendors","Food Trucks", 
                                denver$final_category)
nrow(denver[denver$final_category=="Food Trucks",])

denver$final_category <- ifelse((denver$first_category=="American" | 
                                  denver$first_category=="Bars" | 
                                  denver$first_category=="Mexican" | 
                                  denver$first_category=="Fast Food" | 
                                  denver$first_category=="Cafe" | 
                                  denver$first_category=="Delis" | 
                                  denver$first_category=="French" | 
                                  denver$first_category=="Indian" | 
                                  denver$first_category=="Italian"| 
                                  denver$first_category=="Mediterranean" | 
                                  denver$first_category=="Spanish" | 
                                  denver$first_category=="Thai" | 
                                  denver$first_category=="Chinese" | 
                                  denver$first_category=="Vietnamese" | 
                                  denver$first_category=="Japanese"|
                                  denver$first_category=="Food Trucks")& denver$final_category==0, 
                                denver$first_category,
                                denver$final_category)

denver$final_category <- ifelse(denver$final_category==0,"Others",denver$final_category)

##----- categorizing based on the first category  END -------##

## determining whether it is a chain or not

restaurant_id <- rep(NA, nrow(denver))
branch <- rep(NA,nrow(denver))

sapply(denver$link, function(x){
  restaurant_id[x] <<- denver$restaurant_id[x]
  branch[x] <<- ifelse(
    is.na(
      as.numeric(unlist(strsplit(as.character(denver$link[x]),"-"))[length(unlist(strsplit(as.character(denver$link[x]),"-")))])),0,1) 
})

df_branch_temp <- data.frame(restaurant_id,branch)

denver <- merge(denver,df_branch_temp,all.x = TRUE)

## adding the incomes to the data frame

income <- read.csv("zipcode income.csv")

denver <- merge(denver,income,all.x = TRUE)

## adding no of restaurants by zipcode
num_restaurants <- with(denver,tapply(restaurant_id,zipcode,function(x) length(x)))
denver <- merge(denver,
                data.frame(zipcode=names(num_restaurants),num_restaurants=num_restaurants),
                all.x=TRUE)

## adding ratio of expensive restaurants by zipcode
ratio_ex_restaurant <- with(denver,tapply(price_range, zipcode, function(x){
  length(which(x==4))/length(x)
}))

denver <- merge(denver,
                data.frame(zipcode=names(ratio_ex_restaurant),ratio_ex_restaurant=ratio_ex_restaurant),
                all.x=TRUE)
                            
## adding ratio of inexpensive restaurants by zipcode
ratio_inex_restaurant <- with(denver,tapply(price_range, zipcode, function(x){
  length(which(x==1|x==2))/length(x)
}))

denver <- merge(denver,
                data.frame(zipcode=names(ratio_inex_restaurant),
                           ratio_inex_restaurant=ratio_inex_restaurant),
                all.x=TRUE)

## adding review_rate by restaurants
restaurant_id <- rep(NA,nrow(denver))
review_rate <- rep(NA,nrow(denver))
  sapply(denver$restaurant_id, function(x){
    restaurant_id[x] <<- denver$restaurant_id[x]
    review_rate[x] <<- denver$reviews[x]/denver$dayselapsed[x]
})
  
a <-   sapply(denver$restaurant_id,function(x){print(x)})
  
denver <- merge(denver, data.frame(restaurant_id,review_rate),all.x = TRUE)
a<- data.frame(restaurant_id,review_rate)
## adding avg_dayselapsed by zipcode
avg_dayselapsed <- with(denver,tapply(dayselapsed, zipcode, function(x){
  mean(na.omit(x))
}))

denver <- merge(denver,
                data.frame(zipcode=names(avg_dayselapsed),avg_dayselapsed=avg_dayselapsed),
                all.x=TRUE)

## removing the restaurants not present in denver
denver <- denver[is.na(denver$delete),][-14]

denver <- denver[!is.na(denver$population),]

denver <- denver[!is.na(denver$median_income),]

write.csv(denver,"denver additional.csv")


colSums(is.na(denver[,sapply(denver,is.numeric)]))
