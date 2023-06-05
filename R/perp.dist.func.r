
# This function calculates the perp distance from monkey to the line given distance of monkey from cami
# input = observer position at each time step, monkey position, distance between them

perp.funct = function(cam,monks){
  
  #objects
  # cam = camwalk[i,]
  # monks = monkeys.pos
  
  
  # at each position of observer (X) and monkeys (Y), with distance (mcd), calculate the perp distance
  #between monkey and transect
  if (length(as.vector(monks)) > 2){ # if matrix
    perp.dist = rep(NA, nrow(monks))
    for ( i in 1:nrow(monks)){
      #i=28
      X = cam         #camwalk
      Y = monks[i,]   #monkey.pos
      a =  abs( X[1] - Y[1] )
      perp.dist[i] = a
    }
  } else { # in vector
    X = cam
    Y = monks
    a =  abs( X[1] - Y[1] )
    perp.dist = a
  }
  
  # return distance
  return( perp.dist )
}


