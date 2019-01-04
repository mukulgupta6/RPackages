#' List existing connections
#'
#' This function allows you to see all the saved connections of different DBs.
#' @examples
#' df <- list_connections()

list_connections <- function(){
  suppressMessages(library(svDialogs))
  Path <- paste0("C:/Users/",Sys.info()[["user"]],"/Connections/connections.rds")
  if(!file.exists(Path)){
    svDialogs::dlgMessage("No Connections Found!! Use 'create_connection()' to add.", type = c("ok"),gui = .GUI)
  }
  else{
    connections <- readRDS(Path)
    if(nrow(connections)==0){
      svDialogs::dlgMessage("No Connections Found!! Use 'create_connection()' to add.", type = c("ok"),gui = .GUI)
    }
    else{
      return(connections)
    }
  }
}