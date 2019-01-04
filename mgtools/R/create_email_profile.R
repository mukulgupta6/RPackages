#' Create/Update Email connection
#'
#' This function allows you to create/update email profiles to send out emails.
#' @examples
#' create_email_profile()

create_email_profile <- function(){
  x <- 0
  suppressMessages(library(svDialogs))
  sender <- svDialogs::dlgInput(message = "Enter sender email ID",default=paste0(Sys.info()["user"],"@swiggy.in"),  gui = .GUI)$res
  if(length(sender)){
    pwd <- svDialogs::dlgInput(message = "Enter app password (Read documentation for details)",  gui = .GUI)$res
    if(length(pwd)){
      x <- 1
    }
    else svDialogs::dlgMessage("Aborted", type = c("ok"),gui = .GUI)
  }
  else svDialogs::dlgMessage("Aborted", type = c("ok"),gui = .GUI)
  if(x==1){
    df <- data.frame(Sender=sender, 
                     Password=pwd,
                     stringsAsFactors=FALSE)
    
    Path <- paste0("C:/Users/",Sys.info()[["user"]],"/Connections/email.rds")
    Path1 <- paste0("C:/Users/",Sys.info()[["user"]],"/Connections")
    if(!dir.exists(Path1)){
      dir.create(Path1)
    }
    if(!file.exists(Path)){
      saveRDS(df,Path)
      svDialogs::dlgMessage("Email Added Successfully", type = c("ok"),gui = .GUI)		
    }
    else{
      connections <- readRDS(Path)
      if(!sender %in% connections$Sender){
        connections <- rbind(connections,df)
        saveRDS(connections,Path)
        svDialogs::dlgMessage("Email Added Successfully", type = c("ok"),gui = .GUI)
      }
      else if(sender %in% connections$Sender){
        overwrite <- svDialogs::dlgMessage(paste0("A profile is already set up with the same name: ",sender,".. Overwrite old profile?"), type = c("yesno"),gui = .GUI)$res
        if(overwrite=='yes'){
          connections <- connections[which(connections$Sender %in% sender) * -1,]
          connections <- rbind(connections,df)
          saveRDS(connections,Path)
        }
        else svDialogs::dlgMessage(paste0("Try to create the profile again!"), type = c("ok"),gui = .GUI)
      }
    }
  }
}