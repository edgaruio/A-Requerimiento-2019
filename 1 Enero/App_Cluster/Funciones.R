# Funcion para limpiar y Tokenizacion

limpiar_tokenizar <- function(texto){ 
  # El orden de la limpieza no es arbitrario 
  # Se convierte todo el texto a minúsculas 
  nuevo_texto <- tolower(texto) 
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio) 
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "") 
  # Eliminación de signos de puntuación 
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ") 
  # Eliminación de números 
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ") 
  # Eliminación de espacios en blanco múltiples 
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ") 
  # Tokenización por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]] 
  # Eliminación de tokens con una longitud < 2 
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1}) 
  return(nuevo_texto) }

Limpiar.Cadenas <-function(x, espacios=T){
  x<-gsub("\\.", "_",tolower(x))
  x<-gsub("([\\W])\\1+","\\1",iconv(x,to="ASCII//TRANSLIT"), perl=T)
  if(!espacios){
    x<-gsub("\\s","",iconv(x,to="ASCII//TRANSLIT"), perl=T)
  } else {
    x
  }
}