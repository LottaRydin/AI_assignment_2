library(WheresCroc)

myFunction <- function(moveInfo, readings, positions, edges, probs){
  # move_info:  A list of information for the move. This has two fields: moves and mem
  # readings: A vector giving the salinity, phosphate and nitrogen reading from Croc sensors at his current location.
  # positions: A vector giving the positions of the two tourists (elements 1 and 2) and yourself (element 3).
  # edges: two column matrix giving the edges paths between waterholes (edges) present (the numbers are from and to numbers for the waterholes)
  # probs: standard deviation of readings for salinity, phosphate and nitrogen respectively at each waterhole
  
  print(readings)
  
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
  print(path_finder(15, positions[3], c(), edges, c()))
  print('mem')
  print(length(moveInfo$mem))
  
  if (length(moveInfo$mem) == 1) {
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
    print(trans_mat)
    moveInfo$mem[[2]] = trans_mat
  }
  print('mem')
  print(moveInfo$mem)
  
  
}

path_finder <- function(goal, position, visited, edges, path){
  frontier = list()
  visited = c()
  expand = list()
  first_pos = list(pos=position, path=c())
  frontier = append(frontier, list(first_pos))
  while (length(frontier) != 0){
    expand = frontier[[1]]
    visited = append(visited, expand$pos)
    frontier = frontier[-1]
    if (expand$pos == goal) {
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
  
  
  # print(cat('position', position))
  # path = append(path, position)
  # neighbors <- c()
  # visited = append(visited, position)
  # for (i in 1:2){
  #   for (j in 1:nrow(edges)){
  #     if (edges[j, i] == position){
  #       neighbors = append(neighbors, edges[j,-i])
  #     }
  #   }
  # }
  # for (i in 1:length(neighbors)){
  #   if (!(neighbors[i] %in% visited)) {
  #     if (neighbors[i] == goal){
  #       return (path)
  #     } else {
  #       return (path_finder(goal, neighbors[i], visited, edges, path))
  #     }
  #   }
  # }
}