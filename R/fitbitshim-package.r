#' fitbitshim.
#' 
#' A shim package for use with FitBit and PiLR API. 
#'
#' @name fitbitshim
#' @docType package
#' @import httr jsonlite pilrapi

## Create OAuth and retrieve Fitbit data
#' @export
read_fitbit <- function(key, secret, pilr_pt, 
                        start_date = "2013-01-01", end_date = "today") {
  token_url = "http://api.fitbit.com/oauth/request_token"
  access_url = "http://api.fitbit.com/oauth/access_token"
  auth_url = "http://www.fitbit.com/oauth/authorize"
  fbr = oauth_app('StepTrack',key,secret)
  fitbit = oauth_endpoint(token_url,auth_url,access_url)
  token = oauth1.0_token(fitbit,fbr)
  sig <- config(token = token)
  
  data <- GET(paste0("http://api.fitbit.com/1/user/-/activities/steps/date/",
                     start_date, "/", end_date, ".json"), sig)
  
  data <- jsonlite::fromJSON(as.character(steps))[[1]]
  data
}

## Add PiLR metadata and write to PiLR system
#' @export
write_fitbit <- function(data) {
  data$id <- as.character(10001:(10001+length(data[,1])-1))
  data$timestamp <- data$dateTime
  data$pt <- as.character(pt)
  data$value <- as.numeric(data$value)
  
  write_pilr(data_set = "pilrhealth:fitbit:steps", schema = "1", data = data)
}

##
#' @export
accept_notification <- function(message) {
  notif <- fromJSON(notif)
}

## Some testing code; delete later
#View subscriptions
#GET("http://api.fitbit.com/1/user/-/apiSubscriptions.json", sig)

#Add subscription (101 is ID)
#POST("http://api.fitbit.com/1/user/-/activities/apiSubscriptions/101-activities.json", sig)

#Endpoint URL??
#"http://107.170.188.61/ocpu/github/tdschenk/fitbitshim/R/accept_notification"
