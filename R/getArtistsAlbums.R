#' @title
#'Get an Artist’s Albums
#' @description
#'Get an Artist’s Albums
#'
#'
#'function to get albums from an artist using their ID
#'@param artist_id The Spotify ID for the artist. (Guns n Roses: "3qm84nBOXUEQ2vnTfUTTFC")
#'@param type A comma-separated list of keywords that will be used to filter the response (album, single, appears_on, compilation)
#'@param limit The number of results per HTTP request. Max by the API is 50
#'@param market Optional. An ISO 3166-1 alpha-2 country code. Supply this parameter to limit the response to one particular geographical market. For example, for albums available in Sweden: market=SE.
#'@return 'Get an Artist’s Albums
#'@param token An OAuth token created with \code{spotifyOAuth}.
#'@export
getArtistsAlbums<-function(artist_id,type="album,single,compilation",market="US",limit = 50, token){
  require(dplyr)
    require(httr)
  resp= data.frame()
  hasMore = TRUE
  URI = paste0('https://api.spotify.com/v1/artists/', artist_id, '/albums?offset=0&limit=', limit,
                '&album_type=', type, '&market=', market)
  while(hasMore){
    req<- httr::GET(URI, httr::config(token = token))
    ctnt <- httr::content(req)

    if(req$status_code != 200) {
      print(paste0("ERROR #",req$status_code," - ERROR MESSAGE: ",ctnt$error$message))
      break()
    }
    if(is.null(ctnt[["next"]])){
      hasMore = FALSE
    } else {
      URI = ctnt[["next"]]
    }
    resp = rbind(resp, lapply(httr::content(req)$items, function(x) data.frame(
         id = x$id,
         name = x$name,
         album_type = x$album_type,
         available_markets = paste(x$available_markets, collapse = ";"),
         stringsAsFactors = F)
      ) %>% bind_rows())
  }
  return(resp)
}


