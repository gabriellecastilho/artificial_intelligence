library(WheresCroc)

myFunction <- function(moveInfo, readings, positions, edges, probs){

  waterholes = 40
  
  # Creating transition matrix for new game
  if (moveInfo$mem$status %in% c(0,1)) {
    
    transitionMatrix = diag(waterholes)
    
    for (node in 1:waterholes){
      neighbors = c(edges[which(edges[,1] == node), 2], edges[which(edges[,2] == node), 1], node)
      for (neighbor in neighbors)
        transitionMatrix[node, neighbor] = 1 / length(neighbors)
    }
    
    moveInfo$mem$transitionMatrix <- transitionMatrix
    moveInfo$mem$probability <- rep(1, times = waterholes)
  }

  # Calculating emissions for Hidden Markov Model
  meanEmissions = list(probs[[1]][,1], probs[[2]][,1], probs[[3]][,1])
  stdEmissions = list(probs[[1]][,2], probs[[2]][,2], probs[[3]][,2])
  
  salinity = dnorm(readings[1], meanEmissions[[1]], stdEmissions[[1]])
  phosphate = dnorm(readings[2], meanEmissions[[2]], stdEmissions[[2]])
  nitrogen = dnorm(readings[3], meanEmissions[[3]], stdEmissions[[3]])
  
  emissions = rep(1, times = waterholes)
  for (node in 1:waterholes)
    emissions[node] = (salinity[node] * phosphate[node] * nitrogen[node]) / sum(salinity * phosphate * nitrogen)
  
  # Calculating and updating probability
  probability = (moveInfo$mem$probability %*% moveInfo$mem$transitionMatrix) * emissions
  
  if (!is.na(positions[1]))
    if (positions[1] > 0) probability[positions[1]] = 0 else probability[-positions[1]] = 1
  if (!is.na(positions[2]))
    if (positions[2] > 0) probability[positions[2]] = 0 else probability[-positions[2]] = 1
  
  moveInfo$mem$probability <- probability / sum(probability)
  
  # Searching for best node to move
  moveInfo$moves <- search(which.max(probability), positions, edges)
  moveInfo$mem$status <- 2
  
  return(moveInfo)
}

search <- function(goal, positions, edges) {

  frontier = list(list(position = positions[3], path = c()))
  visited = c(positions[3])
  
  #Case when current position is the goal
  if (positions[3] == goal)
    return(c(0,0))
  
  while (length(frontier) > 0){
    
    currentNode = frontier[[1]]
    
    # Checking if goal was achieved
    if (currentNode$position == goal && length(currentNode$path) <= 2)
        return(c(currentNode$path[1], 0))
    else if (currentNode$position == goal)
        return(c(currentNode$path[1], currentNode$path[2]))
    
    # Transferring node from the frontier to the visited list
    visited = c(visited, c(frontier[[1]]$position))
    frontier = frontier[-1]
    
    # Listing all neighbors to move next and adding to frontier
    neighbors = c(edges[which(edges[,1] == currentNode$position), 2],
                  edges[which(edges[,2] == currentNode$position), 1], currentNode$position)
    
    for (neighbor in neighbors){
      if (neighbor %in% visited)
        next
      frontier = append(frontier, list(list(position = neighbor, path = c(currentNode$path, neighbor))))
    }
  }
}


