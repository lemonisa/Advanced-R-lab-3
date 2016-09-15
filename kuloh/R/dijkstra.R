#' @title Dijkstra Algorithm
#'
#' @description
#' An algorithm to find the shortest path from a inital node to all other nodes.
#' Takes two inputs two find the shortest path from the initial node
#'
#' @param graph is a dataframe
#' @param init_node is the initial node
#' @return Shortest path from Initial Node
#'
#' @references \url{https://en.wikipedia.org/wiki/Dijkstras_algorithm}
#'
#' @export
dijkstra <- function(graph, init_node){
  if((!is.data.frame(graph) & dim(graph)[2]==3) | !is.numeric(init_node)) {
    stop("Insert a correct dataframe or a numeric value as initial node")
  } else {
    # Initial setup
    init_node <- init_node
    unique_nodes <- unique(graph[,"v1"])

    init_matrix <- matrix(Inf,
                          nrow = nrow(unique(graph["v1"])),
                          ncol = nrow(unique(graph["v1"])))

    for(i in 1:nrow(graph["v1"])){
      init_matrix[graph["v2"][i,],graph["v1"][i,]] <- graph["w"][i,]
    }

    # Creating the distance vectors
    distance <- rep(Inf,times = length(unique_nodes))

    distance[init_node] <- 0

    while(length(unique_nodes) != 0){
      idx <- which.min(distance[unique_nodes])
      u <- unique_nodes[idx]

      for(neighbour in unique_nodes){
        alt <- distance[u] + init_matrix[u,neighbour]
        if(alt < distance[neighbour]){
          distance[neighbour] <- alt
        }
      }
      unique_nodes <- unique_nodes[-idx]
    }
  return(distance)
  }
}
