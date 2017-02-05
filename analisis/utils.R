load <- function(con=con,name){
  "Función que descarga si y sólo si no existe un archivo 
    Si no existe, descarga y guarda el archivo."
  #Check if exists
  if (file.exists(paste0("./",name,".csv"))){
    data <- read_csv(paste0("./",name,".csv"))}
  else{
    #Si no existe la descargamos
    system(paste0("rm", name,".xls"))
    download.file(con,paste0("./",name,".xls"),mode="wb") 
    system(paste0("ssconvert ./",name,".xls ./",name,".csv"))
    data <- read_csv(paste0("./",name,".csv"),skip = 30)
    write.csv(data, paste0("./",name,".csv"),row.names = FALSE)
    }
  return(data)
}


clean <- function(db){
  dic <- read_csv("./dir163_dic.csv")
  colnames(db) <- dic$name

  db<-remove_empty_rows(db)  
  return(db)
}

remove_empty_rows <- function(db) {
  db <- db %>% filter(Reduce(`+`, lapply(., is.na)) != ncol(.))
  return(db)
}



getGeoDetails <- function(address){   
  #use the gecode function to query google servers
  geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  #now extract the bits that we need from the returned list
  answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  
  #if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  
  #return Na's if we didn't get a match:
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  #else, extract what we need from the Google server reply into a dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  return(answer)
}
