library(WheresCroc)

myFunction <- function(moveInfo, readings, positions, edges, probs){
  # move_info:  A list of information for the move. This has two fields: moves and mem
  # readings: A vector giving the salinity, phosphate and nitrogen reading from Croc sensors at his current location.
  # positions: A vector giving the positions of the two tourists (elements 1 and 2) and yourself (element 3).
  # edges: two column matrix giving the edges paths between waterholes (edges) present (the numbers are from and to numbers for the waterholes)
  # probs: standard deviation of readings for salinity, phosphate and nitrogen respectively at each waterhole
  
  print(readings)
  sal = readings[1]
  pho = readings[2]
  nit = readings[3]
  
  matches = list(sal = c(), pho = c(), nit = c())
  for (i in 1:3) {
    for (j in 1:nrow(probs[[1]])){
      if (readings[i] > probs[[i]][j, 1]-probs[[i]][j, 2] & readings[i] < probs[[i]][j, 1]+probs[[i]][j, 2]){
        matches[[i]] = append(matches[[i]], j)
      }
    }
  }
  print(probs)
  print(matches)
  print(edges)
  
  
}

path_finder <- function(goal, position, visited){
  visited = append(visited, position)
  neighbors <- c()
  
}