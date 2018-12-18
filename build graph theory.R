###################################
##  SOLVES HAMILTONIAN CIRCUITS  ##
###################################

##################################
##  NEAREST NEIGHBOR ALGORITHM  ##
##################################

nearest.neighbor <- function(vertex, connections){
  ##  connections matrix
  pull.connections <- connections
  ##  Initializes the "stack" as the vertex where you want to start 
  ##    The vertex is randomly chosen by the builder function
  stack <- c(vertex)
  ##  Initializes the total weight of the path as 0
  total.weight <- 0
  
  # do this code while the stack is not empty
  while (length(stack) != 0){
    
    ## node is the last element on the stack
    node <- stack[length(stack)]  ##  pop last element on stack 
    ##  neighborhood of the node based off the connections matrix
    neighborhood <- pull.connections[node,]  
    ##  sorts the neighbors from smallest weight to largest
    neighborhood <- sort(neighborhood)
    
    ##  This will help find which neighbor we should go to
    ##    It will be used as an index for the neighborhood
    best <- 0
    
    ##  Checks for the best neighbor
    for (neighbor in neighborhood){
      ##  Need to increase the "best" choice index
      best <- best + 1
      ##  This piece of code sees in we've already visited the neighbor we're looking at
      ##    If we HAVE been to the current "best" neighbor, it WON'T run
      ##    If we HAVEN'T been there before and it's the "best" neighbor, it WILL run
      if (match(names(neighborhood[best]), stack, nomatch = FALSE) == 0){
        ##  Appends the neighbor to stack
        stack <- c(stack, names(neighborhood[best]))  
        ##  Calculates the total weight of path by adding the "best" neighbor's weight
        total.weight <- total.weight + unname(neighborhood[best]) 
        ##  reassign weight value in the matrix so it is not used again
        pull.connections[node,names(neighborhood[best])] <- pull.connections[names(neighborhood[best]),node] <- 10000 
        ##  Breaks out of the for loop because we don't need to check the other neighbors
        break
        
      }
    }
    ##  Checks to see if all of the Vertices have been visited
    ##    If they have been visited, we have a solution
    if (match("A", stack, nomatch = FALSE) != 0 && match("B", stack, nomatch = FALSE) != 0 && match("C", stack, nomatch = FALSE) != 0 && match("D", stack, nomatch = FALSE) != 0){
      ##  Adjust the total weight to include going back to the starting vertex
      total.weight <- total.weight + pull.connections[stack[length(stack)],vertex]
      ##  Need to add the starting vertex to the stack
      stack <- c(stack, vertex)
      ##  The solution list that contains the starting vertex, final path, and total weight
      solution <- list("starting.vertex" = vertex, "path" = stack, "total.weight" = total.weight, "connections" = connections)
      ##  Returns the solution list
      return (solution)
    }
  }
}

##############################
##  SORTED EDGES ALGORITHM  ##
##############################

sorted.edges <- function(vertices, connections){
  
  ##  Pulls the connections matrix
  connections <- connections
  
  ##  Saves original connections matrix
  original.connections <- connections
  
  ##  IGNORE - HERE FOR ERROR CHECKING
  # print("CONNECTIONS")
  # print(connections)
  # print("ORIGINAL CONNECTIONS")
  # print(original.connections)
  
  #####JON'S##########################################################
  
  ##  Vector of the vertices
  letter <- vertices
  
  ##  Counts some stuff
  counter <- 1
  
  ##  Empty vector of the first vertex we like
  MyOutputFirst<-NULL
  ##  Empty vector of the second vertex we like
  MyOutputLast<-NULL
  
  ##  Have to move 3 times so you can visit every vertex
  for(stuff in 1:(nrow(connections)-1))
  {
    big.number = 10000
    for(i in 1:nrow(connections))
    {
      for(j in 1:ncol(connections))
      {
        ##  The degree of the ith element is equal to how many times it appears
        ##    Can't have a degree three according to rules
        check.degree.i=sum(MyOutputFirst==letter[i])+sum(MyOutputLast==letter[i])
        ##  Checks degree of jth element
        check.degree.j=sum(MyOutputFirst==letter[j])+sum(MyOutputLast==letter[j])
        
        ##  These names won't seem intuitive when the vertices aren't ABCD
        ##    Checks the degree of each vertex
        check.degree.A=sum(MyOutputFirst==letter[1])+sum(MyOutputLast==letter[1])
        check.degree.B=sum(MyOutputFirst==letter[2])+sum(MyOutputLast==letter[2])
        check.degree.C=sum(MyOutputFirst==letter[3])+sum(MyOutputLast==letter[3])
        check.degree.D=sum(MyOutputFirst==letter[4])+sum(MyOutputLast==letter[4])
        
        ##  If the degrees of vertex i and j are less than 2, their edge weight is
        ##    less than the big number, and one of them has a degree less than 1 OR
        ##    all of the degrees of the vertices are the same, then we like this edge
        if ((check.degree.i<2)&(check.degree.j<2) & (connections[i,j] < big.number) 
            & ((check.degree.i<1|check.degree.j<1) 
               | (check.degree.A == check.degree.B & check.degree.B == check.degree.C & check.degree.C == check.degree.D)))
        {
          ##  readjust the big number in case another i j pair becomes more optimal
          big.number <- connections[i,j]
          ##  first vertex of the edge is the i vertex
          first=letter[i]
          ##  last vertex of the edge is the j vertex
          last=letter[j]
        }
      }
    }
    ##  Appends the first vertex of the edge to the OutputFirst list in the counter position
    MyOutputFirst[counter]=first
    ##  Appends the last vertex of the edge to the OutputLast list in the counter position
    MyOutputLast[counter]=last
    ##  Increases the counter
    counter=counter+1
    ##  Changes the edge weight so it's no longer optimal
    connections[first,last]=10000
    connections[last,first]=10000
  }
  
  ##  GETS THE LAST LEGAL MOVE EVEN IF IT'S NOT OPTIMAL
  big.number = 10000
  for(i in 1:nrow(connections))
  {
    for(j in 1:ncol(connections))
    { 
      ##  Checks the ith degree and jth degree
      check.degree.i <- sum(MyOutputFirst==letter[i]) + sum(MyOutputLast==letter[i])
      check.degree.j <- sum(MyOutputFirst==letter[j]) + sum(MyOutputLast==letter[j])
      ##  Runs if the edge of i and j is the best
      if ((check.degree.i < 2)&(check.degree.j <2 ) & (connections[i,j] < big.number))
      {
        ##  reevaluates big number
        big.number <- connections[i,j]
        ##  first vertex of the edge
        first=letter[i]
        ##  second vertex of the edge
        last=letter[j]
      }
    }
  }
  ##  Appends the first vertex edge to the OutputFirst list
  MyOutputFirst[counter]=first
  ##  Appends the last vertex edge to the OutputLast list
  MyOutputLast[counter]=last
  ##  Readjusts the counter
  counter=counter+1
  ##  Adjusts the edge weight so that it is no longer optimal
  connections[first,last]=10000
  connections[last,first]=10000
  
  ##  IGNORE - HERE FOR ERROR CHECKING
  # print("NEW CONNECTIONS")
  # print(connections)
  # 
  # print(MyOutputFirst)
  # print(MyOutputLast)
  
  #####MY CODE##############################################################
  
  ##  Stack is initialized as the first vertex we liked and the one it's connected to
  ##    (because sorted edges picks the smallest edge weight first so these are its friends)
  stack <- c(MyOutputFirst[1],MyOutputLast[1])
  
  ##  The total weight is initialized as the edge weight of the first move
  total.weight <- original.connections[MyOutputFirst[1],MyOutputLast[1]]
  
  ##  IGNORE - HERE FOR ERROR CHECKING
  # print("STACK")
  # print(stack)
  # print("TOTAL WEIGHT")
  # print(total.weight)
  
  ##  This code will execute while the stack is NOT empty
  while (length(stack)!=0){
    ##  Pulls the last vertex on the stack
    pull <- stack[length(stack)]
    
    ##  Checks if the vertex pulled is in the OutPutLast
    ##  If it is there, this code will run
    if (match(pull, MyOutputLast, nomatch = FALSE) != 0)
    {
      ##  Gets the indices of where the "pulled" vertex is in OutPutLast
      ##    May be in more than one place so the index.last is a vector of indices
      index.last <- which(MyOutputLast == pull)
      ##  Checks each index found
      for (i in 1:length(index.last))
      {
        ##  If the OutputFirst it matches is not in the stack, we go there
        if (match(MyOutputFirst[index.last[i]], stack, nomatch = FALSE) == 0)
        {
          ##  Appends the new vertex to the stack
          stack <- c(stack, MyOutputFirst[index.last[i]])
          ##  Readjusts the total weight with the new vertex
          total.weight <- total.weight + original.connections[pull, MyOutputFirst[index.last[i]]]
          
          ##  IGNORE - HERE FOR ERROR CHECKING
          # print("stack and weight going up")
          # print(stack)
          # print(total.weight)
        }
      }
    }
    
    ##  Checks if the element pulled is in the OutPutFirst
    ##  If it is there, this code will run
    if (match(pull, MyOutputFirst, nomatch = FALSE) != 0)
    {
      ##  Gets the indices of where the "pulled" vertex is in OutPutFirst
      index.first <- which(MyOutputFirst == pull)
      ##  Checks each index found
      for (i in 1:length(index.first))
      {
        ##  If the OutputLast it matches is not in the stack, we go there
        if (match(MyOutputLast[index.first[i]], stack, nomatch = FALSE) == 0)
        {
          ##  Appends the new vertex to the stack
          stack <- c(stack, MyOutputLast[index.first[i]])
          ##  Readjusts the total weight with the new vertex
          total.weight <- total.weight + original.connections[pull, MyOutputLast[index.first[i]]]
          
          ##  IGNORE - HERE FOR ERROR CHECKING
          # print("stack and weight going down")
          # print(stack)
          # print(total.weight)
        }
      }
    }
    
    ##  If all the vertices have been visited, we have our solution
    if (match("A", stack, nomatch = FALSE) != 0 & match("B", stack, nomatch = FALSE) != 0 & match("C", stack, nomatch = FALSE) != 0 & match("D", stack, nomatch = FALSE) != 0)
    {
      ##  Need to append the starting vertex to the stack so we can make a circuit
      stack <- c(stack, stack[1])
      ##  Recalculate the total weight for going back to the start
      total.weight <- total.weight + original.connections[stack[length(stack)-1], stack[length(stack)]]
      ##  Solution list that contains the final path, total weight, and original connections matrix
      solution <- list("path" = stack, "total.weight" = total.weight, "connections" = original.connections)
      return(solution)
      
    }
  }
}

################################
##  TRIANGULAR GRAPH BUILDER  ##
################################

build.trianglething <- function(max.weight, method){
  
  ##  Vector of weights
  weights <- c(1:max.weight)
  ##  There are only 6 edges 
  n.edges <- 6
  ##  Sample different weights to create the edges
  edges <- sample(weights, n.edges)
  
  ##  There's only 4 vertices
  n.vertices <- n.edges - 2
  ##  Vertices are initialized as ABCD
  vertices <- LETTERS[1:n.vertices]
  ##  Initial vertex sampled for nearest neighbor algorithm
  ##    Choses random vertex for where to start path
  vertex <- sample(vertices, 1)
  
  ########################
  ##  BUILD THE MATRIX  ##
  ########################
  
  ##  Connections matrix first initialized as one filled with 10000
  connections <- matrix(10000, nrow = n.vertices, ncol = n.vertices)
  ##  The row names are the vertices
  rownames(connections) <- vertices
  ##  As are the column names
  colnames(connections) <- vertices
  
  ##  Assigns the edge weights in the matrix
  position <- 0
  for (i in 1:(nrow(connections)-1)){
    for (j in (i+1):ncol(connections)){
      position <- position + 1
      connections[i,j] <- connections[j,i] <- edges[position]
    }
  }
  
  ########################
  ##  BUILD THE PICTURE ##
  ########################
  
  ##  Initializes plot (has no axes)
  plot(1, type="n", xlab="", ylab="", xlim=c(0, 11), ylim=c(0, 11), axes=FALSE, ann=FALSE)
  #grid()
  
  ##  Adds the shape of the triangular graph
  #AB
  segments(1,2,5,9, lwd =3)
  #AC
  segments(5,9,9,2, lwd =3)
  #AD
  segments(5,4,5,9, lwd =3)
  #BC
  segments(1,2,9,2, lwd =3)
  #BD
  segments(1,2,5,4, lwd =3)
  #CD
  segments(5,4,9,2, lwd =3)
  
  # plot vertex points
  points(x=c(1,9,5,5), y=c(2,2,4,9), cex = 5, pch = 21, lwd = 3, col="black", bg = "white")
  
  # plot edge points for labeling
  points(x=c(3,7,5,5,3,7), y=c(5.5,5.5,6,2,3,3), cex = 5, pch = 19, col="white")
  
  label.edges = edges
  label.vertices = vertices
  # label edges
  text(x=c(3,7,5,5,3,7), y=c(5.5,5.5,6,2,3,3),label.edges, col = "black", cex = 1.5)
  # label vertices
  text(x=c(4,1,9,6),y=c(9,3.5,3.5,4.5), label.vertices, col = "black", cex = 2)
  
  
  ###############
  ##  METHODS  ##
  ###############
  
  ## nearest neighbor
  if (method == "nna"){
    print(nearest.neighbor(vertex, connections))
  }
  ## repeated nearest neighbor
  else if (method == "rnna"){
    totw.min <- vector()
    for (element in vertices){
      solution <- nearest.neighbor(vertex = element, connections)
      print(solution)
      totw.min <- c(totw.min, solution$total.weight)
    }
    print("Minimum total weight possible using the nearest neighbor algorithm:")
    print(min(totw.min))
  }
  ## sorted edges
  else if (method == "sea"){
    print(sorted.edges(vertices, connections))
  }
  
}

####################
##  TEST BUILDER  ##
####################

##  chooses random seed 
auto <- sample(100:999, 1) #0
#auto <- auto + 1
print(auto)

##  BUILDS TRIANGULAR GRAPH BASED OFF CHOSEN METHOD
##  Nearest Neighbor Algorithm
set.seed(auto)
build.trianglething(max.weight = 15, method = "nna")
##  Repeated Nearest Neighbor Algorithm
set.seed(auto)
build.trianglething(max.weight = 15, method = "rnna")
##  Sorted Edges Algorithm
set.seed(auto)
build.trianglething(max.weight = 15, method = "sea")

