install.packages("igraph")

library(igraph)

ciudades <- c("Málaga", "Madrid", "Barcelona", "Valencia", "Sevilla", "Alicante", "Asturias")

distancias <- data.frame(
  from = c("Málaga", "Málaga", "Málaga", "Málaga", "Málaga", "Málaga",
           "Madrid", "Madrid", "Madrid", "Madrid", "Madrid", 
           "Barcelona", "Barcelona", "Barcelona", "Barcelona", 
           "Valencia", "Valencia", "Valencia", 
           "Sevilla", "Sevilla",
           "Alicante"),
  to = c("Madrid", "Barcelona", "Valencia", "Sevilla", "Alicante", "Asturias",
         "Barcelona", "Valencia", "Sevilla", "Alicante", "Asturias", 
         "Valencia", "Sevilla", "Alicante", "Asturias", 
         "Sevilla", "Alicante", "Asturias", 
         "Alicante", "Asturias", 
         "Asturias"),
  peso = c(529, 996, 618, 206, 472, 974,
           626, 360, 530, 424, 446, 
           349, 994, 527, 889,
           656, 170, 804,
           595, 779,
           871)
)

#Impresion de datos
print(ciudades)

print(distancias)

g <- graph_from_data_frame(distancias, directed = FALSE)
E(g)$weight <- distancias$peso
V(g)$name <- ciudades


is_hamiltonian_path <- function(graph, path) {
  n <- length(V(graph))
  if (length(path) != n) return(FALSE)
  
  for (i in 1:(n - 1)) {
    if (!are_adjacent(graph, path[i], path[i + 1])) return(FALSE) 
  }
  return(TRUE)
}

find_hamiltonian_paths <- function(graph, current_path) {
  if (length(current_path) == length(V(graph))){
    return(list(current_path))
  }
  paths <- list()
  for (neighbor in neighbors(graph, tail(current_path, n=1))) {
    if(!(neighbor %in% current_path)){
      new_path <- c(current_path, neighbor)
      paths <- c(paths, find_hamiltonian_paths(graph, new_path))
    }
  }
  return(paths)
}

calculate_path_distance <- function(graph, path) {
  total_distance <- 0
  
  for (i in 1:(length(path) - 1)) {
    edge_id <- get.edge.ids(graph, c(path[i], path[i + 1]))
    total_distance <- total_distance + E(graph)[edge_id]$weight
  }
  return(total_distance)
}

#Busqueda de cada vertice
hamiltonian_paths <- list()
for (start_vertex in V(g)) {
  hamiltonian_paths <- c(hamiltonian_paths, find_hamiltonian_paths(g, c(start_vertex)))
}

unique_hamiltonian_paths <- Filter(function(path) is_hamiltonian_path(g, path), hamiltonian_paths)

is_unique_path <- function(path, path_list) {
  for (p in path_list) {
    if (all(path == p)) {
      return(FALSE)
    }
  }
  return(TRUE)
}

unique_hamiltonian_paths <- list()
for (path in hamiltonian_paths) {
  if (is_unique_path(path, unique_hamiltonian_paths)) {
    unique_hamiltonian_paths <- c(unique_hamiltonian_paths, list(path))
  }
}


print(unique_hamiltonian_paths)


path_distances <- sapply(unique_hamiltonian_paths, function(path) calculate_path_distance(g, path))


if (length(path_distances) > 0) {
  min_distance <- min(path_distances)
  min_distance_paths <- unique_hamiltonian_paths[path_distances == min_distance]
} else {
  min_distance_paths <- list()
}


if (length(min_distance_paths) > 0) {
  print("Se encontraron caminos Hamiltonianos con la menor distancia:")
  print(min_distance_paths)
  
  min_distance_path <- min_distance_paths[[1]]
  min_distance_value <- calculate_path_distance(g, min_distance_path)
  
  print(paste("La suma de las distancias del menor camino Hamiltoniano es:", min_distance_value, "km"))
  
  E(g)$color <- "green"
  E(g)$width <- 1
  
  for (i in 1:(length(min_distance_path) - 1)) {
    edge_id <- get.edge.ids(g, c(min_distance_path[i], min_distance_path[i + 1]))
    E(g)[edge_id]$color <- "red"
    E(g)[edge_id]$width <- 2
  }
  
  V(g)$color <- ifelse(V(g)$name %in% min_distance_path, "red", "yellow")
  
  plot(g, vertex.label = V(g)$name, vertex.size = 30, vertex.color = V(g)$color,
       edge.color = E(g)$color, edge.width = E(g)$width,
       edge.label = E(g)$weight,
       main = "Camino hamiltoniano con la menor distancia del caso:
       Problema del Viaje por la Geografía Española")
} else {
  print("No se encontraron caminos Hamiltonianos")
}