#' Executes a Query
#'
#' This function allows you to pass a complete code where individual queries are seperated by ';'. It returns a dataframe with whether the code was successfully executed or not. Should only be used when the queries start with Create, insert or drop commands.
#'
#' @param connection_name Name of connection on which the query needs to be executed
#' @param Query Complete query to be executed
#' @param prefix Prefix to be added before every temp table
#' @param split Set it to TRUE if one needs to execute queries one by one. Split happens using the delimiter ()
#' @param retry Number of retry attempts on failure
#' @param sleep Number of seconds to sleep before retrying
#' @param logs No need to provide
#' @examples
#' df <- execute_query(connection_name,Query)

execute_query <- function(connection_name="ADPG",Query="NA",prefix = "NA", split=FALSE, retry=0, sleep=0,logs = getwd()){
  if(file.exists("../Configurations and Logs/ConfigFile.txt")){
    logs <- readr::read_file("../Configurations and Logs/ConfigFile.txt")
  }
  if(!Query=="NA" && !connection_name=="NA" && !prefix =="NA"){
    conn <- Swiggy::connect(connection_name)
    if(!class(conn)=="character"){
      u <- prefix
      Query <- gsub("temp\\.",paste0("temp\\.",u,"_"),Query)
      df <- data.frame(Step=integer(),
                       Status=character(), 
                       error_msg = character(),
                       Execution_time=double(), 
                       stringsAsFactors=FALSE)
      if(split==TRUE){
        Query_array <- strsplit(Query, ";")[[1]]
        status <- NULL
        for (i in 1:length(Query_array)){
          if(nchar(Query_array[i])<5 || class(status)=="try-error"){
            next
          }
          attempt <- 1
          start <- Sys.time()
          conn <- Swiggy::connect(connection_name)
          print(paste0("Executing Query #",i," out of ",length(Query_array)))
          status <- try(dbSendUpdate(conn,Query_array[i]))
          dbDisconnect(conn)
          while(class(status) == "try-error" && attempt < retry+1) {
            conn <- Swiggy::connect(connection_name)
            attempt <- attempt+1
            print(paste0(Sys.time(),"> Ran into an error. Retry no:",attempt))
            print(paste0(Sys.time(),"> Sleeping for: ",sleep," seconds"))
            Sys.sleep(sleep)
            print(paste0("Executing Query #",i," out of ",length(Query_array)))
            status <- try(dbSendUpdate(conn,Query_array[i]))
            dbDisconnect(conn)
          }
          end <- Sys.time()
          if(!class(status) == "try-error"){
            step_df <- data.frame(Step=i,
                                  Status='Pass', 
                                  error_msg = "None",
                                  Execution_time=difftime(end,start,units="secs"), 
                                  stringsAsFactors=FALSE)
            df <- rbind(df,step_df)
            if(file.exists(paste0("../Configurations and Logs/Logs/",basename(getwd()),".rds"))){
              log_data <- readRDS(paste0("../Configurations and Logs/Logs/",basename(getwd()),".rds"))
              log_data$StepsCompleted <- paste0(i,"/",total_steps)
              saveRDS(log_data,paste0("../Configurations and Logs/Logs/",basename(getwd()),".rds"))
            }
          }else if(class(status)=="try-error"){
            step_df <- data.frame(Step=i,
                                  Query=Query_array[i],
                                  Status='Failed', 
                                  error_msg = as.character(status),
                                  stringsAsFactors=FALSE)
            df <- step_df			
          }
        }		
      } else{
        attempt <- 1
        start <- Sys.time()
        conn <- Swiggy::connect(connection_name)
        print(paste0("Executing Query"))
        status <- try(dbSendUpdate(conn,Query))
        dbDisconnect(conn)
        while(class(status) == "try-error" && attempt < retry+1) {
          conn <- Swiggy::connect(connection_name)
          attempt <- attempt+1
          print(paste0(Sys.time(),"> Ran into an error. Retry no:",attempt))
          print(paste0(Sys.time(),"> Sleeping for: ",sleep," seconds"))
          Sys.sleep(sleep)
          print(paste0("Executing Query"))
          status <- try(dbSendUpdate(conn,Query))
          dbDisconnect(conn)
        }
        end <- Sys.time()
        if(!class(status) == "try-error"){
          step_df <- data.frame(Step=1,
                                Status='Pass', 
                                error_msg = "None",
                                Execution_time=difftime(end,start,units="secs"), 
                                stringsAsFactors=FALSE)
          df <- rbind(df,step_df)
        }else if(class(status)=="try-error"){
          step_df <- data.frame(Step=1,
                                Query=Query,
                                Status='Failed',
                                error_msg = as.character(status),
                                stringsAsFactors=FALSE)
          df <- step_df			
        }
      }
      return(df)
    }
  }
  else print("Provide both connection name, query and prefix!")
}

