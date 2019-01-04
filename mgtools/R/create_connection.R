#' Create Connection
#'
#' This function allows you to create connections to different DBs.
#' @examples
#' create_connection()

create_connection <- function(){
  x <- 0
  suppressMessages(library(svDialogs))
  user <- tolower(svDialogs::dlgInput(message = "Enter login user name (for connection)",default=Sys.info()["user"],  gui = .GUI)$res)
  if(length(user)){
	pwd <- svDialogs::dlgInput(message = "Enter password (for connection)",  gui = .GUI)$res
	if(length(pwd)){
	  datasource <- tolower(svDialogs::dlgInput(message = "Enter a name for the connection",default="APG",  gui = .GUI)$res)
	  if(length(datasource)){
		class_path <- svDialogs::dlgInput(message = "Enter the driver class path",default="com.facebook.presto.jdbc.PrestoDriver",  gui = .GUI)$res
		if(length(class_path)){
		  driver_url <- svDialogs::dlgInput(message = "Enter the JDBC URL",default="jdbc:presto://presto.swiggy.com:24563/hive?SSL=true",gui = .GUI)$res
		  if(length(driver_url)){
			driver_name <- svDialogs::dlgInput(message = "Enter the JDBC URL",default="presto-jdbc-0.185.jar",gui = .GUI)$res
			 if(length(driver_name)){
				x <- 1
			 }
			 else svDialogs::dlgMessage("Aborted", type = c("ok"),gui = .GUI)
		  }
		  else svDialogs::dlgMessage("Aborted", type = c("ok"),gui = .GUI)
		}  
		else svDialogs::dlgMessage("Aborted", type = c("ok"),gui = .GUI)
	  }
	  else svDialogs::dlgMessage("Aborted", type = c("ok"),gui = .GUI)
	}
	else svDialogs::dlgMessage("Aborted", type = c("ok"),gui = .GUI)
  }
  else svDialogs::dlgMessage("Aborted", type = c("ok"),gui = .GUI)
  
  if(x==1){
    df <- data.frame(Connection_Name=datasource, 
                     UserName=user,
					 Password=pwd,
                     class_path = class_path,
                     driver_url= driver_url,
					 driver_name=driver_name,
                     stringsAsFactors=FALSE)
    
    Path <- paste0("C:/Users/",Sys.info()[["user"]],"/Connections/connections.rds")
	Path1 <- paste0("C:/Users/",Sys.info()[["user"]],"/Connections")
	if(!dir.exists(Path1)){
		dir.create(Path1)
	}
    if(!file.exists(Path)){
      saveRDS(df,Path)
	  svDialogs::dlgMessage("Connection Added Successfully", type = c("ok"),gui = .GUI)		
    }
    else{
      connections <- readRDS(Path)
      if(!datasource %in% connections$Connection_Name){
        connections <- rbind(connections,df)
        saveRDS(connections,Path)
        svDialogs::dlgMessage("Connection Added Successfully", type = c("ok"),gui = .GUI)
      }
      else if(datasource %in% connections$Connection_Name){
        overwrite <- svDialogs::dlgMessage(paste0("A connection already exists with name: ",datasource,".. Overwrite old connection?"), type = c("yesno"),gui = .GUI)$res
        if(overwrite=='yes'){
          connections <- connections[which(connections$Connection_Name %in% datasource) * -1,]
          connections <- rbind(connections,df)
		  saveRDS(connections,Path)
        }
        else svDialogs::dlgMessage(paste0("Try to create the connection again!"), type = c("ok"),gui = .GUI)
      }
    }
  }
}