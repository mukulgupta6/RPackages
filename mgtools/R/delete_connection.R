#' Delete existing connections
#'
#' This function allows you to delete a saved connections.
#' @examples
#' delete_connection(Connection_Name="")
#' @param Connection_Name Name of connection which needs to be deleted. Use list_connection() in case you are not sure about the connection name.

delete_connection <- function(Connection_Name="NA"){
  Connection_Name <- tolower(Connection_Name)
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
    else if(Connection_Name %in% connections$Connection_Name){
      delete <- svDialogs::dlgMessage(paste0("Confirm to delete connection: ",Connection_Name), type = c("yesno"),gui = .GUI)$res
      if(delete=='yes') {
        connections <- connections[which(connections$Connection_Name %in% Connection_Name) * -1,]
        saveRDS(connections,Path)
        svDialogs::dlgMessage("Connection deleted successfully", type = c("ok"),gui = .GUI)
      }
    }  
    else {
      svDialogs::dlgMessage(paste0("No Connections Found by the provided name: ",Connection_Name), type = c("ok"),gui = .GUI)
    }
  }
}