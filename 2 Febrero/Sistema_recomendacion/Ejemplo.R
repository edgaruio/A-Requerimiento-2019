# Ejemplo

library(recommenderlab)
data(MovieLense)

# Seleccionamos mas de 50 valoraciones por fila y por columna
rating_movies <- MovieLense[rowCounts(MovieLense)>50,
                            colCounts(MovieLense)>50]

# Algunos casos
head(rating_movies@data,10)
tail(rating_movies@data,10)

# Primer usuario con sus valoraciones
rating_movies@data[1,]

# Todas las valoraciones a Toy Store
rating_movies@data[,1]


