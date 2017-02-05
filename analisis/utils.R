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
