# A* implementation for Assignment 1, Artificial Intelligence, UU, HT2023

# Obs.: The original functions were changed very little
# New function created for the A* algorithm

library("DeliveryMan")

myFunction <- function(trafficMatrix, carInfo, packageMatrix) {
  
  if(carInfo$load == 0) {
    goal <- nextPickup(trafficMatrix, carInfo, packageMatrix)
    carInfo$mem$goal <- list(x = goal[1], y = goal[2])
  } else {
    goal <- packageMatrix[carInfo$load, c(3,4)]
    carInfo$mem$goal <- list(x = goal[1], y = goal[2])
  }

  carInfo$nextMove <- aStar(trafficMatrix, carInfo, packageMatrix)
  return(carInfo)
}

nextPickup <- function(trafficMatrix, carInfo, packageMatrix) {
  distanceVector = abs(packageMatrix[,1] - carInfo$x) + abs(packageMatrix[,2] - carInfo$y)
  distanceVector[packageMatrix[,5] != 0] = Inf
  return(packageMatrix[which.min(distanceVector), c(1,2)])
}

nextMove <- function(neighborInfo, carInfo){
  if(carInfo$x < neighborInfo$x)
    return(6)
  else if (carInfo$x > neighborInfo$x)
    return(4)
  else if (carInfo$y < neighborInfo$y)
    return(8)
  else if (carInfo$y > neighborInfo$y)
    return(2)
  else
    return(5)
}

aStar <- function(trafficMatrix, carInfo, packageMatrix) {
  
  frontier <- list()
  visited <- list()
  
  # Calculating costs and adding starting node to the frontier
  g <- 0 
  h <- abs(carInfo$x - carInfo$mem$goal$x) + abs(carInfo$y - carInfo$mem$goal$y)
  
  frontier[paste(carInfo$x,carInfo$y)] <- list(list(x = carInfo$x, y = carInfo$y,
                                                    f = g + h, g = g, h = h,
                                                    path = list(c(carInfo$x,carInfo$y))))
  
  # Performing A* calculations for all neighbors
  while(length(frontier) > 0){
    
    # Selecting node to expand from frontier
    currentInfo <- frontier[[which.min(lapply(frontier, function(getF)getF$f))]]
    
    # Transferring selected node from the frontier to the visited list
    visited <- append(visited, paste(currentInfo$x,currentInfo$y))
    frontier[paste(currentInfo$x,currentInfo$y)] <- NULL
    
    # Listing all neighbors to move next
    neighbors <- list()
    neighbors["up"] <- list(c(currentInfo$x, currentInfo$y + 1)) #up
    neighbors["down"] <- list(c(currentInfo$x, currentInfo$y - 1)) #down
    neighbors["right"] <- list(c(currentInfo$x + 1, currentInfo$y)) #right
    neighbors["left"] <- list(c(currentInfo$x - 1, currentInfo$y)) #left
    
    for(neighbor in 1:length(neighbors)){
      neighborInfo <- list(x = neighbors[[neighbor]][1], y = neighbors[[neighbor]][2])
      
      # Case when neighbor is out of matrix bounds
      if(neighborInfo$x < 1 || neighborInfo$x > 10 || neighborInfo$y < 1 || neighborInfo$y > 10)
        next
      
      # Case when neighbor was already visited
      if(paste(neighborInfo$x,neighborInfo$y) %in% visited)
        next
      
      # Calculating costs and appending info to neighbor
      if (names(neighbors)[neighbor] == "up")
        g <- currentInfo$g + trafficMatrix$vroads[currentInfo$x, currentInfo$y]
      else if (names(neighbors)[neighbor] == "down")
        g <- currentInfo$g + trafficMatrix$vroads[currentInfo$x, currentInfo$y - 1]
      else if (names(neighbors)[neighbor] == "right")
        g <- currentInfo$g + trafficMatrix$hroads[currentInfo$x, currentInfo$y]
      else if (names(neighbors)[neighbor] == "left")
        g <- currentInfo$g + trafficMatrix$hroads[currentInfo$x - 1, currentInfo$y]
      
      h <- abs(neighborInfo$x - carInfo$mem$goal$x) + abs(neighborInfo$y - carInfo$mem$goal$y)
      neighborInfo <- append(neighborInfo, list(f = g + h, g = g, h = h))
 
      # Checking if goal was achieved
      if(neighborInfo$h == 0){
        neighborInfo <- append(neighborInfo, list(path = append(currentInfo$path,
                                                                list(c(neighborInfo$x,neighborInfo$y)))))
        path <- unlist(neighborInfo$path[[2]])
        return(nextMove(list(x = path[1], y = path[2]), carInfo))
      }
      
      # Case when node is already in the frontier with a better cost g
      if(paste(neighborInfo$x,neighborInfo$y) %in% names(frontier))
        if(frontier[[paste(neighborInfo$x,neighborInfo$y)]]$g < g)
          next
      
      # Adding neighbor to the frontier
      frontier[paste(neighborInfo$x,neighborInfo$y)] <- list(list(x = neighborInfo$x, y = neighborInfo$y,
                                                                  f = g + h, g = g, h = h,
                                                                  path = append(currentInfo$path,
                                                                                list(c(neighborInfo$x, neighborInfo$y)))))
    }
  }
  return(nextMove(carInfo, carInfo))
}

