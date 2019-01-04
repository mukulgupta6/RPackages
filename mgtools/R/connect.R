#' Connect to a database
#'
#' This function allows you to see all the saved connections of different DBs.
#' @examples
#' conn <- connect(dbName)
#' @param dbName Name of connection which needs to be connected. Use list_connection() in case you are not sure about the connection name.

connect <- function(dbName="NA"){
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
        drv <- RJDBC::JDBC(selected_connection$class_path,paste0("C:/Users/",Sys.info()[["user"]],"/Drivers/",selected_connection$driver_name))
        conn <- RJDBC::dbConnect(drv, selected_connection$driver_url, selected_connection$UserName, selected_connection$Password)
        return(conn)
      }
    }
  }
}