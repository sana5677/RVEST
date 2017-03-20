rm(list=ls())

#install.packages("rvest")
library(rvest)

## location is set to denver, only restaurants with city as denver is taken
location <- "Denver"

denver_restaurant <- read_html(paste("https://www.yelp.com/search?find_desc=Restaurants&start=0&l=p:CO:",location,"::",sep=""))

## no of pages in the search results 
page_count <- denver_restaurant %>%
  html_nodes(".page-of-pages") %>%
  html_text() 

page_count <- as.numeric(gsub("[\n]","",strsplit(page_count," ",fixed=TRUE)[[1]][12]))

## scrapping the restaurant names and their individual links
restaurant_names_list <- NULL
restaurant_links_list <- NULL
for(i in seq(0,(page_count-1)*10,10)){
  denver_restaurant <- read_html(paste("https://www.yelp.com/search?find_desc=Restaurants&start=",i,"&l=p:CO:",location,"::",sep=""))
  
  restaurant_names <- denver_restaurant %>% 
    html_nodes(".indexed-biz-name span") %>%
    html_text() 
  
  restaurant_names_list <- c(restaurant_names_list,
                             restaurant_names)
  
  restaurant_links <- denver_restaurant %>%
    html_nodes(".indexed-biz-name a") %>%
    html_attr("href")
  
  restaurant_links_list <- c(restaurant_links_list,
                             restaurant_links)
}

restaurant_links_list <- sub("\\?search_key=.*","",restaurant_links_list)

## creating a data frame that has the restaurant names and the links
denver_data_frame <- data.frame(matrix(NA, nrow=length(restaurant_names_list),ncol=1))


#denver_data_frame$restaurant_name <- as.character(gsub("^\\s+","",
#                                                        substr(gsub("\n","",restaurant_names_list),
#                                                               unlist(gregexpr(pattern="\\.",restaurant_names_list))+1,
#                                                               nchar(gsub("\n","",restaurant_names_list)))))
denver_data_frame$restaurant_name <- as.character(restaurant_names_list)
denver_data_frame[,1] <- NULL

denver_data_frame$link <- paste("https://www.yelp.com",restaurant_links_list,sep="")

denver_data_frame <- unique(denver_data_frame)

denver_data_frame$restaurant_id <- 1:nrow(denver_data_frame)

## scrapping the price range from all the individual restaurant pages
denver_page_html_price_range <- rep(0,nrow(denver_data_frame))
for (i in 1:nrow(denver_data_frame)){
  denver_page_html <-read_html(denver_data_frame$link[i])
  
  denver_page_html_price_range[i] <- ifelse(is.numeric(denver_page_html %>% 
                                                          html_nodes(".bullet-after .price-range") %>%
                                                          html_text() %>%
                                                          nchar())==FALSE,
                                             NA,
                                             denver_page_html %>%
                                               html_nodes(".bullet-after .price-range") %>%
                                               html_text() %>%
                                               nchar())
  Sys.sleep(5)
}

## adding the price range to the main data frame
denver_data_frame$price_range<- denver_page_html_price_range

## scrapping the category from all the individual restaurants

denver_page_html_category <- rep(0,nrow(denver_data_frame))
for (i in 100:600){
  denver_page_html <-read_html(denver_data_frame$link[i])
  
  denver_page_html_category[i] <- ifelse(is.character(gsub("[ \r\n]", "",denver_page_html %>% 
                                                              html_nodes(".category-str-list") %>%
                                                              html_text()))==FALSE,
                                          NA,
                                          gsub("[ \r\n]", "",denver_page_html %>% 
                                                 html_nodes(".category-str-list") %>%
                                                 html_text()))
  Sys.sleep(5)
}

## adding the category to the main data frame 
denver_data_frame$category<- denver_page_html_category

## scraping the rating(stars) from all the individual restaurants
denver_page_html_rating <- rep(0,nrow(denver_data_frame))

for (i in 1:nrow(denver_data_frame)){
  denver_page_html <-read_html(denver_data_frame$link[i])
  denver_page_html_rating[i] <- ifelse(is.na((denver_page_html %>% 
                                                 html_nodes(".star-img") %>%
                                                 html_attr("title"))[1])==TRUE,
                                        NA,
                                        as.numeric(sub(pattern = "\"", replacement = "",
                                                       sub(pattern = "star.*", replacement = "",
                                                           sub(pattern = ".*title=", replacement = "",(denver_page_html %>% 
                                                                                                         html_nodes(".star-img"))[1])))))
  Sys.sleep(5)
  }

## adding the rating(stars) to the main data frame 
denver_data_frame$rating<- denver_page_html_rating

## scraping the reviews from all the individual pages
denver_page_html_reviews <- rep(0,nrow(denver_data_frame))
for (i in 1:nrow(denver_data_frame)){
  denver_page_html <-read_html(denver_data_frame$link[i])
  
  numofratings <- html_text(html_nodes(denver_page_html, ".biz-rating-very-large .review-count"))
  nrcleaned1 <- gsub("[ \r\n]", "", numofratings)
  nrcleaned2 <- (sub("reviews", "", nrcleaned1))
  denver_page_html_reviews[i] <- ifelse(is.character(nrcleaned2)==FALSE,
                                   NA,
                                   as.numeric(nrcleaned2))
  Sys.sleep(5)
}

## adding the reviews to the main data frame 
denver_data_frame$reviews<- denver_page_html_reviews

## scrapping the city from all the individual restaurant pages
denver_page_html_city <- rep(0,nrow(denver_data_frame))
for (i in 1:nrow(denver_data_frame)){
  denver_page_html <-read_html(denver_data_frame$link[i])
  address <- html_text(html_nodes(denver_page_html, xpath="//address")) 
  acleaned1 <- address[2:2]
  acleaned2 <-gsub("[ \r\n]", "", acleaned1) #removes all white spaces , page breaks and carriage returns
  acleaned3 <- sub(",CO.*", "", acleaned2)
  acleaned4 <- sub(".*,", "", acleaned3)
  
  denver_page_html_city[i] <- ifelse(is.numeric(nchar(acleaned4))==FALSE,
                                      NA,
                                      acleaned4)
  Sys.sleep(5)
  
}

## adding the city to the main data frame
denver_data_frame$city <- denver_page_html_city


## scrapping the neighbourhood from all the individual restaurant pages
denver_page_html_neighbourhood <- rep(0,nrow(denver_data_frame))
for (i in 1:nrow(denver_data_frame)){
  denver_page_html <-read_html(denver_data_frame$link[i])
  nhood <- html_text(html_nodes(denver_page_html, ".neighborhood-str-list")) 
  nhoodcleaned1 <- gsub("[ \r\n]", "", nhood)
  
  denver_page_html_neighbourhood[i] <- ifelse(is.numeric(nchar(nhoodcleaned1))==FALSE,
                                               NA,
                                               nhoodcleaned1)
  Sys.sleep(5)
}

## adding the nhood to the main data frame
denver_data_frame$neighbourhood<- denver_page_html_neighbourhood


##scraping the zipcodes from all the individual pages
denver_page_html_zipcode <- rep(NA,nrow(denver_data_frame))
for (i in 1:nrow(denver_data_frame)){
  denver_page_html <-read_html(denver_data_frame$link[i])
  
  zip <- html_text(html_nodes(denver_page_html, "address")) 
  zcleaned1 <- zip[1:1]
  zcleaned2 <-gsub("[ \r\n]", "", zcleaned1)
  zcleaned3 <- as.numeric(sub(".*CO", "", zcleaned2))
  
  
  
  denver_page_html_zipcode[i] <- ifelse(is.numeric(zcleaned3)==FALSE,
                                         NA,
                                         as.numeric(zcleaned3))
  Sys.sleep(5)
}
## adding the zipcode to the main data frame
denver_data_frame$zipcode<- as.factor(denver_page_html_zipcode)



##scraping the latitudes and longitudes from all the individual pages
denver_page_html_latitude <- rep(NA,nrow(denver_data_frame))
denver_page_html_longitude <- rep(NA,nrow(denver_data_frame))
for(i in 1:nrow(denver_data_frame)){
  denver_page_html <- read_html(denver_data_frame$link[i])
  node <- as.character(denver_page_html %>% html_nodes(".lightbox-map"))
  denver_page_html_latitude[i] <- as.numeric(strsplit(strsplit(node,"latitude&quot;: ",2)[[1]][2],",",1)[[1]][1])
  denver_page_html_longitude[i] <- as.numeric(strsplit(strsplit(node,"longitude&quot;: ",2)[[1]][2],"}",1)[[1]][1])
  Sys.sleep(5)
  
}

## adding the latitudes and longitudes to the main data frame
denver_data_frame$latitude <- denver_page_html_latitude
denver_data_frame$longitude <- denver_page_html_longitude


##scraping the time elapsed from all the individual pages
denver_page_html_dayselapsed <- rep(0,nrow(denver_data_frame))
for (i in 1:nrow(denver_data_frame)){
  
  denver_page_html <-read_html(denver_data_frame$link[i])
  
  lastreview_link <- paste(denver_data_frame$link[i],"?start=",(denver_data_frame$reviews[i]-1),sep="")
  lastreview_html <-read_html(lastreview_link)
  frclean1 <- html_text(html_nodes(lastreview_html, "#super-container .biz-rating-very-large .rating-qualifier"))
  frclean2 <- gsub("[ \r\n]", "", frclean1)
  frclean3 <- as.Date(frclean2, "%m/%d/%Y")
  frclean4 <- as.numeric(Sys.Date()-frclean3)
  denver_page_html_dayselapsed[i] <- ifelse(is.numeric(frclean4)==FALSE,
                           NA,
                           frclean4)
  Sys.sleep(5)
}

## adding the time elapsed to the main dataframe
denver_data_frame$dayselapsed <- denver_page_html_dayselapsed

write.csv(denver_data_frame,file="C:/Users/Saraswathi Nagappan/Documents/CUB/fall semester/Advanced Data Analytics/project/code/scraping denver.csv")

# rm(list=ls())
# denver_data_frame <- read.csv("scraping denver.csv")
# x <- read.csv("finalDenver.csv")
# x <- unique(x[,c("link","dayselapsed")])
# a <- unique(denver_data_frame[is.na(denver_data_frame$dayselapsed),-10])
# b <- unique(denver_data_frame[!is.na(denver_data_frame$dayselapsed),])
# 
# c <- merge(a,x,all.x = TRUE)
# d <- rbind(b,c)
# 
# write.csv(d,"scrapping denver new.csv")
