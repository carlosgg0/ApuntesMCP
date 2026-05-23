library(igraph)

# La misma información de la sección anterior, ahora como grafo bipartito
interacciones <- data.frame(
  usuario = c("Ana",   "Ana",   "Beto",   "Beto",   "Beto",  "Carla", "Carla",  "Carla",
              "David", "David", "Elena",  "Elena"),
  item    = c("Matrix","StarWars","Matrix","StarWars","Inception","Titanic","Amelie","Matrix",
              "StarWars","Inception","Titanic","Amelie"),
  # Interacción positiva = rating >= 4
  positiva = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE,
               TRUE, TRUE, FALSE, TRUE)
) |> filter(positiva)

# Creamos el grafo bipartito
g_bip <- graph_from_data_frame(interacciones[, c("usuario","item")], directed = FALSE)

# CLAVE: añadir el atributo 'type' para identificar las dos particiones
V(g_bip)$type <- V(g_bip)$name %in% interacciones$usuario

cbind(V(g_bip)$name, V(g_bip)$type)

cat("¿Es bipartito?", is_bipartite(g_bip), "\n")