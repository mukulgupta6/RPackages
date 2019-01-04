#' Send emails from R
#'
#' This function allows you to send emails with attachments. One can use mailR library as well.
#' 
#' @param from A valid email address of the sender.
#' @param to A character vector of recipient valid email addresses.
#' @param subject Subject of the email.
#' @param body Body of the email as text. If the parameter body refers to an existing file location, the text of the file is parsed as body of the email. For Adding tables and graphs (see details for more info)
#' @param attachments A character vector of paths in the file system linking to files or *valid* URLs to be attached to the email (see details for more info on attaching URLs)
#' @param html Default value False. Change it to true if there are tables or graphs to be inserted.
#' @param inline Default value False. Change it to true if there are tables or graphs to be inserted.
#' @details Using 'attachments' you can attach files or webpages hosted on the web (for e.g. on Dropbox). Currently, URLs hostnames must be prepending with http:// or https://.
#' 
#' @examples
#' sender <- "sender@gmail.com"  # Replace with a valid address
#' recipients <- c("receiver1@gmail.com")  # Replace with one or more valid addresses
#' attachments <- c("C:/file_name.txt","https://gmail.com")
#' send_mail(from = sender,
#'          to = recipients,
#'          subject="Subject of the email",
#'          body = "Body of the email",
#'          attachment = attachments)
#' 
#' library(xtable)
#' file_name <- "C:/Users/Admin/Documents/sample_data.csv"
#' sample_data <- read.csv(file_name)
#' plot_file <- "sample_plot.png"
#'	## Converts table in html format to be attached inside mail body
#'	sample_table <- print(xtable(dataframe,caption = "Heading for the table"), type="html", caption.placement = "top")
#'	## Set sender and recepient
#'	sender <- "qv_mailer@swiggy.in"
#'	recipients <- c("to_list_1@swiggy.in","to_list_2@swiggy.in")
#'	## Export sample plot to image file
#'	png(filename=plot_file)
#'	plot(sample_data)
#'	dev.off()
#'	## Set subject of the mail
#'	subject <- "Subject Line"
#'	## Mail body opening
#'	mail_body <- paste0("greetings here, </b><br><br> additional comments here")
#'	## Attach table
#'	mail_body <- paste0(mail_body,"</b><br><br>",sample_table)
#'	## Embed charts
#'	mail_body <- paste0(mail_body,'</b><br> Sample plot: </b><br><img src="sample_plot.png">')
#'  ## Adding P.S. and signature
#'	mail_body <- paste0(mail_body,"</b><br>more comments here.<br><br>signature here")

send_mail <- function(from,to,cc=NULL, bcc=NULL,subject="",body=" ",attachments=NULL, html=FALSE, inline=FALSE){
  Path <- paste0("C:/Users/",Sys.info()[["user"]],"/Connections/email.rds")
  if(!file.exists(Path)){
    print("No profiles found!! Use 'create_email_profile()' to add.")
  }
  else {
    connections <- readRDS(Path)
    if(!tolower(from) %in% connections$Sender){
      print("No profiles found with the provided name. Use 'create_email_profile()' to add!")
    }
    else if(tolower(from) %in% connections$Sender){
      selected_profile <- connections[which(connections$Sender %in% tolower(from)),]
      selected_sender <- selected_profile$Sender
      pwd <- selected_profile$Password
      suppressMessages(library(mailR))
      if(length(cc)>0 && length(bcc)>0){
        send.mail(from=selected_sender,to=to,cc=cc,bcc=bcc,subject=subject,
                  body = body, encoding = "iso-8859-1",
                  html = html, 
                  inline = inline,
                  smtp = list(host.name = "smtp.gmail.com", 
                              port = 587, 
                              user.name=selected_sender,
                              passwd=pwd,tls=TRUE), 
                  authenticate = TRUE,
                  send = TRUE, attach.files = attachments,debug = FALSE)
      }
      else if(length(cc)>0){
        send.mail(from=selected_sender,to=to,cc=cc,subject=subject,
                  body = body, encoding = "iso-8859-1",
                  html = html, 
                  inline = inline,
                  smtp = list(host.name = "smtp.gmail.com", 
                              port = 587, 
                              user.name=selected_sender,
                              passwd=pwd,tls=TRUE), 
                  authenticate = TRUE,
                  send = TRUE, attach.files = attachments,debug = FALSE)
      }
      else if(length(bcc)>0){
        send.mail(from=selected_sender,to=to,bcc=bcc,subject=subject,
                  body = body, encoding = "iso-8859-1",
                  html = html, 
                  inline = inline, 
                  smtp = list(host.name = "smtp.gmail.com", 
                              port = 587, 
                              user.name=selected_sender,
                              passwd=pwd,tls=TRUE), 
                  authenticate = TRUE,
                  send = TRUE, attach.files = attachments,debug = FALSE)
      }
      else {
        send.mail(from=selected_sender,to=to,subject=subject,
                  body = body, encoding = "iso-8859-1",
                  html = html, 
                  inline = inline, 
                  smtp = list(host.name = "smtp.gmail.com", 
                              port = 587, 
                              user.name=selected_sender,
                              passwd=pwd,tls=TRUE), 
                  authenticate = TRUE,
                  send = TRUE, attach.files = attachments,debug = FALSE)
      }
    }
  }
}