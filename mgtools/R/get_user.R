#' get user info for a connection
#'
#' This function allows you to get the user name for a saved connection!
#' @examples
#' user <- get_user(dbName)
#' @param dbName Name of connection which needs to be connected. Use list_connection() in case you are not sure about the connection name.

get_user <- function(dbName="NA"){
  library(RJDBC)
  if(dbName=="NA"){
    print("Provide a connection name!")
  }
  else {
    Path <- paste0("C:/Users/",Sys.info()[["user"]],"/Connections/connections.rds")
    if(!file.exists(Path)){
      print("No Connections Found!! Use 'create_connection()' to add.")
    }
    else{
      connections <- readRDS(Path)
      if(!tolower(dbName) %in% connections$Connection_Name){
        print("No Connection with the provided name. Use create_connection() to add or use the right connection name")
      }
      else if (tolower(dbName) %in% connections$Connection_Name){
        selected_connection <- connections[which(connections$Connection_Name %in% tolower(dbName)),]
		user <- selected_connection$UserName
        return(user)
      }
    }
  }
}