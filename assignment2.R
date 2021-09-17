library(WheresCroc)

myFunction <- function(moveInfo, readings, positions, edges, probs){
  # move_info:  A list of information for the move. This has two fields: moves and mem
  # readings: A vector giving the salinity, phosphate and nitrogen reading from Croc sensors at his current location.
  # positions: A vector giving the positions of the two tourists (elements 1 and 2) and yourself (element 3).
  # edges: two column matrix giving the edges paths between waterholes (edges) present (the numbers are from and to numbers for the waterholes)
  # probs: standard deviation of readings for salinity, phosphate and nitrogen respectively at each waterhole
  
  if (moveInfo$mem$status == 1) {
    moveInfo$mem$status = 0
    moveInfo$mem[[3]] = list(rep(c(1/40), each = 40))
  }
  
  # Create transition matrix, path matrix and F0
  if (length(moveInfo$mem) == 1) {
    # Create transition matrix
    trans_mat = matrix(0, 40, 40)
    for (node in 1:40){
      neighbor = c()
      for (i in 1:2){
        for (j in 1:nrow(edges)){
          if (edges[j, i] == node) {
            neighbor = append(neighbor, edges[j,-i])
          }
        }
      }
      prob = 1/(length(neighbor)+1)
      for (i in 1:length(neighbor)){
        trans_mat[node, neighbor[i]] = prob
      }
      trans_mat[node, node] = prob
    }
    moveInfo$mem[[2]] = trans_mat
    
    # Create F0
    moveInfo$mem[[3]] = list(rep(c(1/40), each = 40))
    
    # Create path matrix
    moveInfo$mem[[4]] = list()
    for (i in 1:40){
      moveInfo$mem[[4]][[i]] = rep(list(NA), each = 40)
    }
  }
  
  
  Ot_mat = emission_finder(readings, probs)
  T_mat = moveInfo$mem[[2]]
  F_prev = moveInfo$mem[[3]][[length(moveInfo$mem[[3]])]]
  
  # If previous move was searched
  if (length(moveInfo$moves) != 0) {
    if (moveInfo$moves[2] == 0){
      F_prev[positions[3]] = 0
    }
  }
  
  F_vec = F_prev %*% T_mat %*% Ot_mat
  
  for (i in 1:2){
    if (is.na(positions[[i]])){
      break
    } else if (positions[[i]] < 0){
      F_vec[-positions[[i]]] = 1
    } else {
      F_vec[positions[[i]]] = 0
    }
  }
  
  moveInfo$mem[[3]][[length(moveInfo$mem[[3]])+1]] = F_vec
  goal = which.max(F_vec)
  
  path = NA
  
  if (length(moveInfo$mem[[4]][[positions[[3]]]][[goal]])==1){
    path = path_finder(goal, positions[[3]], edges)
    moveInfo$mem[[4]][[positions[[3]]]][[goal]] = path
  } else {
    path = moveInfo$mem[[4]][[positions[[3]]]][[goal]]
  }
  
  if (length(path) == 2){
    moveInfo$moves = c(0, 0)
  } else {
    moveInfo$moves = path[2:3]
  }
  return (moveInfo)
}


path_finder <- function(goal, position, edges){
  frontier = list()
  visited = c()
  path = c()
  expand = list()
  first_pos = list(pos=position, path=c())
  frontier = append(frontier, list(first_pos))
  while (length(frontier) != 0){
    expand = frontier[[1]]
    visited = append(visited, expand$pos)
    frontier = frontier[-1]
    if (expand$pos == goal) {
      expand$path = append(expand$path, goal)
      expand$path = append(expand$path, 0)
      return (expand$path)
    } else {
      for (i in 1:2){
        for (j in 1:nrow(edges)){
          if (edges[j, i] == expand$pos & !(edges[j, -i] %in% visited)){
            neighbor = list(pos=edges[j,-i], path=append(expand$path, expand$pos))
            frontier = append(frontier, list(neighbor))
          }
        }
      }
    }
  }
}


emission_finder <- function(readings, probs) {
  emi_mat = matrix(0, 40, 40)
  for (i in 1:40){
    i_value = dnorm(readings[1], probs$salinity[i,1], probs$salinity[i,2])*dnorm(readings[2], 
              probs$phosphate[i,1], probs$phosphate[i,2])*dnorm(readings[3], probs$nitrogen[i,1], probs$nitrogen[i,2])
    emi_mat[i,i] = i_value
  }
  return (emi_mat)
}