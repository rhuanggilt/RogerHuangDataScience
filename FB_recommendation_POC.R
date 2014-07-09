library(RCurl)
library(RJSONIO)
library(plyr)
## Roger's own token
access_token <- "your FB graph API access token"

## Get Roger's likes
get_likes <- function(user_id)
{likes_URL <-  paste("https://graph.facebook.com/",user_id,"/likes?fields=id,name,category,created_time,location&limit=10000&access_token=", access_token, sep = "")
likes_json <- getURL(likes_URL)
likes_list <- fromJSON(likes_json)
likes_list_data <- likes_list$data

## parse like detail and export into txt
get_likes_details <- function(likes_list_data)
{
likes_id <- likes_list_data['id'] 
likes_name <- likes_list_data['name']
likes_created_time <- likes_list_data['created_time']
likes_category <- likes_list_data['category']
likes_city <- tryCatch({likes_list_data$location$city},error=function(cond){return(NA)})
likes_state <- tryCatch({likes_list_data$location$state},error=function(cond){return(NA)})
likes_country <- tryCatch({likes_list_data$location$country},error=function(cond){return(NA)})
likes_zipcode <- tryCatch({likes_list_data$location$zip},error=function(cond){return(NA)})
likedetails <- cbind(user_id,likes_id,likes_name,likes_created_time,likes_category,likes_city,likes_state,likes_country,likes_zipcode)
return(likedetails)
}

likes_list_export <- lapply(likes_list_data,FUN=get_likes_details)
sapply(likes_list_export, write.table, file="likes.txt", append=TRUE, sep="\t", row.names = FALSE,col.names = FALSE, fileEncoding="UTF-8", quote=T)
}

##get my own likes first
get_likes('your facebook id')

## get Roger's FB friendlist
friendlist_url <- paste("https://graph.facebook.com/me/friends?limit=100000&access_token=", access_token, sep = "")
friendlist_json <- getURL(friendlist_url)
friendlist <- fromJSON(friendlist_json)
friend_name_id <- friendlist$data

get_friendlist <- function(friend_list)
{
  return(cbind(friend_list['id'] ,friend_list['name']))
}

## get friend list and get their likes
if(length(friend_name_id)>=0)
{
friendlist_export <- lapply(friend_name_id,FUN=get_friendlist)
lapply(friendlist_export, write.table, file="friendlist.txt", append=TRUE, sep="\t", row.names = FALSE,col.names = FALSE, fileEncoding="UTF-8", quote=T)  
  
for(i in 1:length(friend_name_id))
{friend_name <- friend_name_id[[i]]['name']
friend_id <- friend_name_id[[i]]['id']
get_likes(friend_id)
}
}

