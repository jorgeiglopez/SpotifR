#' @title
#'Get an Album’s Tracks
#'
#'@description
#'Get an Album’s Tracks
#'
#'Get Spotify catalog information about an album’s tracks.
#'@param album_id The Spotify ID for the album. (Use Your Illusion I : "4L5pz06MVlsWaTEjSQPN8h")
#'@param limit The number of results per request. Max 50
#'@param market Optional. An ISO 3166-1 alpha-2 country code. Supply this parameter to limit the response to one particular geographical market. For example, for albums available in Sweden: market=SE.
#'@return get Album from an artist using their ID
#'@param token An OAuth token created with \code{spotifyOAuth}.
#'@export

getAlbumsTracks<-function(album_id, market="US",limit = 50, token){
  resp<- data.frame()
  hasMore = TRUE
  if(limit <=1 || limit > 50) limit = 50
  URI <- paste0("https://api.spotify.com/v1/albums/", album_id,"/tracks?offset=0&limit=",limit,"&market=",market)
  while(hasMore){
    req<-httr::GET(URI, httr::config(token = token))
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
    json<-jsonlite::fromJSON(jsonlite::toJSON(ctnt))$items
    resp <- rbind(resp,json[c("id","name","duration_ms","track_number","disc_number","preview_url")] )
  }
  return(resp)
}


