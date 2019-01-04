#' Fetch data into R from a DB
#'
#' This function allows you to Fetch data from a source.
#' @examples
#' df <- fetch_data(connection_name="",Query="",..)
#' @param connection_name Name of connection on which the query needs to be executed
#' @param Query Complete query to be executed
#' @param bulk_load (Optional) boolean field to be changed to one only if volume of data is high
#' @param memory (Optional) Java Memory in GB that needs to be set if bulk_load fails. Max Value 20GB 

fetch_data <- function(connection_name="NA",Query="NA",bulk_load=0,memory=8){
  if(!Query=="NA" && !connection_name=="NA"){
    conn <- Swiggy::connect(connection_name)
    if(!class(conn)=="character" && bulk_load==0 && memory<=20 && memory>0){
        t <- RJDBC::dbSendQuery(conn,Query)
        df <- RJDBC::fetch(t,n=-1,block=999)
        return(df)
    }
    else if((!class(conn)=="character" && bulk_load==1) || (!class(conn)=="character" && memory>0 && memory <=20)){
      param <- memory*1024
      total_mem <- max(8192,param)
      options(java.parameters = paste0("-Xmx",total_mem,"m"))
      t <- dbSendQuery(conn,Query)
      df <- fetch(t,n=-1,block=999)
    }
    else if(memory>20 || memory<0){
      print("Incorrect memory value. Max Memory allowed is 20. Default is 8")
    }
  }
  else {
    print("Either query or connection name is missing")
  }  
}