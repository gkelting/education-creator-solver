#####################################
##  APPORTIONMENT SOLVER/BUILDER?  ##
#####################################

#########################
##  HAMILTON'S METHOD  ##
#########################

hamilton <- function(pop.top, div, n.seats, n.places){
  
  ##  divisor passed from apportionment builder
  div <- div
  ##  number of seats passed from apportionment builder
  n.seats <- n.seats
  ##  number of places needing seats apportioned passed from apportionment builder
  n.places <- n.places
  
  ## determine initial seat placements
  for (i in 1:n.places){
    ## calculate quota for each place
    quota <- pop.top[i, "Population"] / div
    ## append quotas to the quota column in the population matrix
    pop.top[i,"Quota"] <- quota
    ##  the floor of the quotas is the lower quota
    low.quota <- floor(quota)
    ##  append the lower quotas to the initial seat allocation column in the matrix
    pop.top[i,"Initial"] <- low.quota
  }
  
  ## compute the sum of the lower quotas and append it to the matrix
  pop.top["Total","Initial"] <- sum(pop.top[c(1:n.places),"Initial"])
  
  ## final number of seats is first initialized as the initial seats allocated
  pop.top[,"Final"] <- pop.top[,"Initial"] 
  
  ## supplemental column to determine which places receive extra seats
  for (i in 1:n.places){
    ## for each of the places, takes the decimals of the quotas and appends them to the supplemental column
    pop.top[i,"(Extra for Hamilton)"] <- pop.top[i,"Quota"] - floor(pop.top[i,"Quota"]) 
  }
  
  ## add one member to each consequent max number of decimals if haven't reached n.seats yet
  while (pop.top["Total","Final"] < n.seats){
    ##  index number of the max decimal in the supplemental column
    position <- which.max(pop.top[,"(Extra for Hamilton)"])
    ##  add an extra seat to the final number of seats for the place in max decimal position
    pop.top[position, "Final"] <- pop.top[position, "Final"] + 1
    ##  recalculate the final number of seats that have been apportioned
    pop.top["Total","Final"] <- sum(pop.top[c(1:n.places), "Final"])
    ##  so that the max decimal is not used again, redefine it as -1
    ##  this way the next max decimal is used
    pop.top[position, "(Extra for Hamilton)"] <- -1
    
  }
  
  ## information from the population matrix wanted to keep when presenting the final answer
  keep <- c("Population", "Quota", "Initial", "Final")
  ## returns the matrix with the kept information
  return (pop.top[, keep])
  
  ## TO SEE FULL MATRIX:
  ## STEP 1: COMMENT OUT THE KEEP AND RETURN STATEMENTS STATED PREVIOUSLY
  ## STEP 2: UNCOMMENT NEXT LINE
  #return(pop.top)
}

##########################
##  JEFFERSON'S METHOD  ##
##########################

jefferson <- function(pop.top, div, n.seats, n.places, mod.div.list){
  
  # standard divisor passed from apportionment builder
  std.divisor <- div
  # number of seats passed from apportionment builder
  n.seats <- n.seats
  # number of places passed from apportionment builder
  n.places <- n.places
  
  ## determine initial seat placements
  for (i in 1:n.places){
    ## calculate quota for each place
    quota <- pop.top[i, "Population"] / std.divisor
    ## append quotas to the quota column in the population matrix
    pop.top[i,"Quota"] <- quota
    ##  the floor of the quotas is the lower quota
    low.quota <- floor(quota)
    ##  append the lower quotas to the initial seat allocation column in the matrix
    pop.top[i,"Initial"] <- low.quota
  }
  
  ## sum of lower quotas
  pop.top["Total","Initial"] <- sum(pop.top[c(1:n.places),"Initial"])
  
  ## re-evaluate divisor
  if (pop.top["Total","Initial"] < n.seats || pop.top["Total","Initial"] > n.seats){
    ## method used if there weren't enough seats allocated
    if (pop.top["Total","Initial"] < n.seats){
      ## reduce the divisor
      ## no particular reason that it's 0.95, it just makes the divisor smaller
      modified.divisor <- std.divisor*0.95
      mod.div.list <- c(mod.div.list, modified.divisor)
      ## recalculates the seat allocation using the new divisor
      jefferson(pop.top, div = modified.divisor, n.seats, n.places, mod.div.list)
    }
    ## method used if there were too many seats allocated
    else{
      ## increase divisor
      ## no particular reason that it's 1.05, it just makes the divisor bigger
      modified.divisor <- std.divisor*1.05
      mod.div.list <- c(mod.div.list, modified.divisor)
      ## recalculates the seat allocation using the new divisor
      jefferson(pop.top, div = modified.divisor, n.seats, n.places, mod.div.list)
    }
  }
  ## will execute once the divisor no longer needs adjusted
  else{
    ## information from the population matrix wanted to keep when presenting the final answer
    keep <- c("Population", "Quota", "Initial")
    solution <- list("New Apportionment Table" = pop.top[,keep], "New Divisor Used" = mod.div.list[length(mod.div.list)])
    return(solution)
    
    ## TO SEE FULL MATRIX:
    ## STEP 1: COMMENT OUT THE KEEP AND RETURN STATEMENTS STATED PREVIOUSLY
    ## STEP 2: UNCOMMENT NEXT LINE
    #return(pop.top)
  }
  
}

##  RETURNS ORIGINAL JEFFERSON SOLUTION
original.jefferson <- function(pop.top, div, n.seats, n.places){
  
  # standard divisor passed from apportionment builder
  std.divisor <- div
  # number of seats passed from apportionment builder
  n.seats <- n.seats
  # number of places passed from apportionment builder
  n.places <- n.places
  
  ## determine initial seat placements
  for (i in 1:n.places){
    ## calculate quota for each place
    quota <- pop.top[i, "Population"] / std.divisor
    ## append quotas to the quota column in the population matrix
    pop.top[i,"Quota"] <- quota
    ##  the floor of the quotas is the lower quota
    low.quota <- floor(quota)
    ##  append the lower quotas to the initial seat allocation column in the matrix
    pop.top[i,"Initial"] <- low.quota
  }
  
  ## sum of lower quotas
  pop.top["Total","Initial"] <- sum(pop.top[c(1:n.places),"Initial"])
  
  ## returns the number of representatives without recalculating divisor
  keep <- c("Population", "Quota", "Initial")
  ## returns the matrix with the kept information
  return (pop.top[, keep])
}

##  THIS HELPS IN DETERMINING THE RANGE OF DIVISORS
##  USES JEFFERSON METHOD WITHOUT CHANGING DIVISOR
test.jefferson <- function(pop.top, div, n.seats, n.places){
  
  # standard divisor passed from apportionment builder
  std.divisor <- div
  # number of seats passed from apportionment builder
  n.seats <- n.seats
  # number of places passed from apportionment builder
  n.places <- n.places
  
  ## determine initial seat placements
  for (i in 1:n.places){
    ## calculate quota for each place
    quota <- pop.top[i, "Population"] / std.divisor
    ## append quotas to the quota column in the population matrix
    pop.top[i,"Quota"] <- quota
    ##  the floor of the quotas is the lower quota
    low.quota <- floor(quota)
    ##  append the lower quotas to the initial seat allocation column in the matrix
    pop.top[i,"Initial"] <- low.quota
  }
  
  ## sum of lower quotas
  pop.top["Total","Initial"] <- sum(pop.top[c(1:n.places),"Initial"])
  
  ## returns the number of representatives without recalculating divisor
  representatives <- pop.top["Total","Initial"]
  return (representatives)
  
  
}

##  THIS DETERMINES THE RANGE OF THE DIVISORS FOR JEFFERSON
range.jefferson <- function(pop.top, div, n.seats, n.places){
  
  ##  initial divisor passed from apportionment builder
  div <- div
  ##  number of seats passed from apportionment builder
  n.seats <- n.seats
  ##  number of places passed from apportionment builder
  n.places <- n.places
  
  ##  potential.divisors is a vector to hold all divisors that have been/are being tested
  potential.divisors <- c(div)
  ##  this is for help with the while loop because I haven't quite mastered them in R
  help <- 5
  ##  initialize the procedure as "it's fine" because the initial divisor may not need to be changed
  procedure <- "it's fine"
  
  while (help > 1){
    ##  Using the last element in the potential.divisors vector, if the number of seats allocated
    ##  is LESS than the number of seats needed, then REDUCE the divisor
    if (test.jefferson(pop.top, div = potential.divisors[length(potential.divisors)], n.seats, n.places) < n.seats){
      ##  this redefines the procedure
      ##  "reduce" because the divisor needs reduced
      procedure <- "reduce"
      ##  systematically check for the next divisor by testing the number one less than the one just used
      ##  EG divisor = 500 but doesn't work
      ##    then the next divisor tried is 499
      try <- potential.divisors[length(potential.divisors)] - 1
      ##  appends the divisor we're trying to potential.divisors
      potential.divisors <- c(potential.divisors, try)
      ##  use test.jefferson to compute the total number of seats allocated with the new divisor
      test.jefferson(pop.top, div = try, n.seats, n.places)
    }
    ##  Using the last element in the potential.divisors vector, if the number of seats allocated
    ##  is MORE than the number of seats needed, then INCREASE the divisor
    else if (test.jefferson(pop.top, div = potential.divisors[length(potential.divisors)], n.seats, n.places) > n.seats){
      ##  this redefines the procedure
      ##  "increase" because the divisor needs increased
      procedure <- "increase"
      ##  systematically check for the next divisor by testing the number one more than the one just used
      ##  EG divisor = 500 but doesn't work
      ##    then the next divisor tried is 501
      try <- potential.divisors[length(potential.divisors)] + 1
      ##  appends the divisor we're trying to potential.divisors
      potential.divisors <- c(potential.divisors, try)
      ##  use test.jefferson to compute the total number of seats allocated with the new divisor
      test.jefferson(pop.top, div = try, n.seats, n.places)
    }
    ##  If the divisor last tried gives the correct number of seats, do this
    else{
      ##  Pass the procedure used previously
      procedure <- procedure
      ##  The range of divisors starts at the last divisor used
      begin <- potential.divisors[length(potential.divisors)]
      ##  append the beginning divisor to the vector range.divisors
      range.divisors <- c(begin)
      
      while (help > 1){
        
        ##  If we were reducing the divisor previously, we need to continue reducing it 
        ##  until we get to a divisor that no longer works
        if (procedure == "reduce"){
          ##  systematically check for the next divisor by testing the number one less than the one just used
          try <- potential.divisors[length(potential.divisors)] - 1
          ##  appends the divisor we're trying to potential.divisors
          potential.divisors <- c(potential.divisors, try)
          ##  use test.jefferson to compute the total number of seats allocated with the new divisor
          test.jefferson(pop.top, div = try, n.seats, n.places)
          
          ##  If the last divisor used no longer works and gives us too many seats, then the range 
          ##  of divisors stops at the second to last element (because that one worked)
          if (test.jefferson(pop.top, div = try, n.seats, n.places) > n.seats){
            ##  The range of divisors ends at the second to last divisor used
            end <- potential.divisors[length(potential.divisors)-1]
            ##  append the ending divisor to the vector range.divisors
            range.divisors <- c(range.divisors, end)
            ##  sort the range.divisors because the reduce method starts 
            ##  at a bigger number and ends at a smaller one
            range.divisors <- sort(range.divisors)
            ##  print the title "Range of Divisors"
            print ("Range of Divisors")
            ##  return range.divisors
            return (range.divisors)
          }
        }
        
        ##  If we were increasing the divisor previously, we need to continue increasing it 
        ##  until we get to a divisor that no longer works
        else if (procedure == "increase"){
          ##  systematically check for the next divisor by testing the number one more than the one just used
          try <- potential.divisors[length(potential.divisors)] + 1
          ##  appends the divisor we're trying to potential.divisors
          potential.divisors <- c(potential.divisors, try)
          ##  use test.jefferson to compute the total number of seats allocated with the new divisor
          test.jefferson(pop.top, div = try, n.seats, n.places)
          
          ##  If the last divisor used no longer works and gives us too few seats, then the range 
          ##  of divisors stops at the second to last element (because that one worked)
          if (test.jefferson(pop.top, div = try, n.seats, n.places) < n.seats){
            ##  The range of divisors ends at the second to last divisor used
            end <- potential.divisors[length(potential.divisors)-1]
            ##  append the ending divisor to the vector range.divisors
            range.divisors <- c(range.divisors, end)
            ##  print the title "Range of Divisors"
            print ("Range of Divisors")
            ##  return range.divisors
            return (range.divisors)
          }
        }
        ## If the initial divisor is perfectly fine, then return "it's fine"
        else{
          return (procedure)
        }
      }
    }
  }
}

##############################
##  HUNTINGTON HILL METHOD  ##
##############################

huntington.hill <- function(pop.top, div, n.seats, n.places, mod.div.list){
  
  ## standard divisor passed from apportion builder
  std.divisor <- div
  ## number of seats passed from apportion builder
  n.seats <- n.seats
  ## number of places passed from apportion builder
  n.places <- n.places
  
  ## compute quotas and geometric means
  for (i in 1:n.places){
    
    ##  determine the quotas for each place and append to population matrix
    quota <- pop.top[i, "Population"] / std.divisor
    pop.top[i,"Quota"] <- quota
    
    ##  compute lower quota for each place and append to population matrix
    low.quota <- floor(quota)
    pop.top[i,"Lower.Quota"] <- low.quota
    
    ##  compute geometric mean for each place and append to population matrix
    geometric.mean <- sqrt(low.quota * (low.quota + 1))
    pop.top[i,"Geometric.Mean"] <- geometric.mean
    
    ##  If the quota is greater than the geometric mean, the ceiling of the quota is used
    ##  for the initial number of seats
    if (pop.top[i,"Quota"] > pop.top[i,"Geometric.Mean"]){
      up.quota <- ceiling(quota)
      pop.top[i,"Initial"] <- up.quota
    }
    ##  else the floor of the quota is used for the initial number of seats
    else{
      pop.top[i,"Initial"] <- low.quota
    }
  }
  
  ## sum of new quotas
  pop.top["Total","Initial"] <- sum(pop.top[c(1:n.places),"Initial"])
  
  ## re-evaluate divisor
  if (pop.top["Total","Initial"] < n.seats || pop.top["Total","Initial"] > n.seats){
    ## method used if there weren't enough seats allocated
    if (pop.top["Total","Initial"] < n.seats){
      ## reduce the divisor
      ## no particular reason that it's 0.95, it just makes the divisor smaller
      modified.divisor <- std.divisor*0.95
      mod.div.list <- c(mod.div.list, modified.divisor)
      ## recalculates the seat allocation using the new divisor
      huntington.hill(pop.top, div = modified.divisor, n.seats, n.places, mod.div.list)
    }
    ## method used if there were too many seats allocated
    else{
      ## increase divisor
      ## no particular reason that it's 1.05, it just makes the divisor bigger
      modified.divisor <- std.divisor*1.05
      mod.div.list <- c(mod.div.list, modified.divisor)
      ## recalculates the seat allocation using the new divisor
      huntington.hill(pop.top, div = modified.divisor, n.seats, n.places, mod.div.list)
    }
  }
  ## will execute once the divisor no longer needs adjusted
  else{
    ## information from the population matrix wanted to keep when presenting the final answer
    keep <- c("Population", "Quota", "Lower.Quota", "Geometric.Mean", "Initial")
    ## returns the matrix with the kept information
    #return (pop.top[, keep])
    
    solution <- list("New Apportionment Table" = pop.top[,keep], "New Divisor Used" = mod.div.list[length(mod.div.list)])
    return(solution)
    
    ## TO SEE FULL MATRIX:
    ## STEP 1: COMMENT OUT THE KEEP AND RETURN STATEMENTS STATED PREVIOUSLY
    ## STEP 2: UNCOMMENT NEXT LINE
    #return(pop.top)
  }
}

##  RETURNS ORIGINAL HUNTINGTON HILL SOLUTION
original.huntingtonhill <- function(pop.top, div, n.seats, n.places){
  
  ## standard divisor passed from apportion builder
  std.divisor <- div
  ## number of seats passed from apportion builder
  n.seats <- n.seats
  ## number of places passed from apportion builder
  n.places <- n.places
  
  ## compute quotas and geometric means
  for (i in 1:n.places){
    
    ##  determine the quotas for each place and append to population matrix
    quota <- pop.top[i, "Population"] / std.divisor
    pop.top[i,"Quota"] <- quota
    
    ##  compute lower quota for each place and append to population matrix
    low.quota <- floor(quota)
    pop.top[i,"Lower.Quota"] <- low.quota
    
    ##  compute geometric mean for each place and append to population matrix
    geometric.mean <- sqrt(low.quota * (low.quota + 1))
    pop.top[i,"Geometric.Mean"] <- geometric.mean
    
    ##  If the quota is greater than the geometric mean, the ceiling of the quota is used
    ##  for the initial number of seats
    if (pop.top[i,"Quota"] > pop.top[i,"Geometric.Mean"]){
      up.quota <- ceiling(quota)
      pop.top[i,"Initial"] <- up.quota
    }
    ##  else the floor of the quota is used for the initial number of seats
    else{
      pop.top[i,"Initial"] <- low.quota
    }
  }
  
  ## sum of new quotas
  pop.top["Total","Initial"] <- sum(pop.top[c(1:n.places),"Initial"])
  
  keep <- c("Population", "Quota", "Lower.Quota", "Geometric.Mean", "Initial")
  ## returns the matrix with the kept information
  return (pop.top[, keep])
  
}

##  THIS HELPS IN DETERMINING THE RANGE OF DIVISORS
##  USES HUNTINGTON HILL METHOD WITHOUT CHANGING DIVISOR
test.huntingtonhill <- function(pop.top, div, n.seats, n.places){
  
  ## standard divisor passed from apportion builder
  std.divisor <- div
  ## number of seats passed from apportion builder
  n.seats <- n.seats
  ## number of places passed from apportion builder
  n.places <- n.places
  
  ## compute quotas and geometric means
  for (i in 1:n.places){
    
    ##  determine the quotas for each place and append to population matrix
    quota <- pop.top[i, "Population"] / std.divisor
    pop.top[i,"Quota"] <- quota
    
    ##  compute lower quota for each place and append to population matrix
    low.quota <- floor(quota)
    pop.top[i,"Lower.Quota"] <- low.quota
    
    ##  compute geometric mean for each place and append to population matrix
    geometric.mean <- sqrt(low.quota * (low.quota + 1))
    pop.top[i,"Geometric.Mean"] <- geometric.mean
    
    ##  If the quota is greater than the geometric mean, the ceiling of the quota is used
    ##  for the initial number of seats
    if (pop.top[i,"Quota"] > pop.top[i,"Geometric.Mean"]){
      up.quota <- ceiling(quota)
      pop.top[i,"Initial"] <- up.quota
    }
    ##  else the floor of the quota is used for the initial number of seats
    else{
      pop.top[i,"Initial"] <- low.quota
    }
  }
  
  ## sum of new quotas
  pop.top["Total","Initial"] <- sum(pop.top[c(1:n.places),"Initial"])
  
  ## returns the number of representatives without recalculating divisor
  representatives <- pop.top["Total","Initial"]
  
  return (representatives)
  
}

##  THIS DETERMINES THE RANGE OF THE DIVISORS FOR HUNTINGTON HILL
range.huntingtonhill <- function(pop.top, div, n.seats, n.places){
  
  ##  initial divisor passed from apportionment builder
  div <- div
  ##  number of seats passed from apportionment builder
  n.seats <- n.seats
  ##  number of places passed from apportionment builder
  n.places <- n.places
  
  ##  potential.divisors is a vector to hold all divisors that have been/are being tested
  potential.divisors <- c(div)
  ##  this is for help with the while loop because I haven't quite mastered them in R
  help <- 5
  ##  initialize the procedure as "it's fine" because the initial divisor may not need to be changed
  procedure <- "it's fine"
  
  while (help > 1){
    ##  Using the last element in the potential.divisors vector, if the number of seats allocated
    ##  is LESS than the number of seats needed, then REDUCE the divisor
    if (test.huntingtonhill(pop.top, div = potential.divisors[length(potential.divisors)], n.seats, n.places) < n.seats){
      ##  this redefines the procedure
      ##  "reduce" because the divisor needs reduced
      procedure <- "reduce"
      ##  systematically check for the next divisor by testing the number one less than the one just used
      ##  EG divisor = 500 but doesn't work
      ##    then the next divisor tried is 499
      try <- potential.divisors[length(potential.divisors)] - 1
      ##  appends the divisor we're trying to potential.divisors
      potential.divisors <- c(potential.divisors, try)
      ##  use test.huntingtonhill to compute the total number of seats allocated with the new divisor
      test.huntingtonhill(pop.top, div = try, n.seats, n.places)
    }
    ##  Using the last element in the potential.divisors vector, if the number of seats allocated
    ##  is MORE than the number of seats needed, then INCREASE the divisor
    else if (test.huntingtonhill(pop.top, div = potential.divisors[length(potential.divisors)], n.seats, n.places) > n.seats){
      ##  this redefines the procedure
      ##  "increase" because the divisor needs increased
      procedure <- "increase"
      ##  systematically check for the next divisor by testing the number one more than the one just used
      ##  EG divisor = 500 but doesn't work
      ##    then the next divisor tried is 501
      try <- potential.divisors[length(potential.divisors)] + 1
      ##  appends the divisor we're trying to potential.divisors
      potential.divisors <- c(potential.divisors, try)
      ##  use test.huntingtonhill to compute the total number of seats allocated with the new divisor
      test.huntingtonhill(pop.top, div = try, n.seats, n.places)
    }
    ##  If the divisor last tried gives the correct number of seats, do this
    else{
      ##  Pass the procedure used previously
      procedure <- procedure
      ##  The range of divisors starts at the last divisor used
      begin <- potential.divisors[length(potential.divisors)]
      ##  append the beginning divisor to the vector range.divisors
      range.divisors <- c(begin)
      
      while (help > 1){
        
        ##  If we were reducing the divisor previously, we need to continue reducing it 
        ##  until we get to a divisor that no longer works
        if (procedure == "reduce"){
          ##  systematically check for the next divisor by testing the number one less than the one just used
          try <- potential.divisors[length(potential.divisors)] - 1
          ##  appends the divisor we're trying to potential.divisors
          potential.divisors <- c(potential.divisors, try)
          ##  use test.huntingtonhill to compute the total number of seats allocated with the new divisor
          test.huntingtonhill(pop.top, div = try, n.seats, n.places)
          
          ##  If the last divisor used no longer works and gives us too many seats, then the range 
          ##  of divisors stops at the second to last element (because that one worked)
          if (test.huntingtonhill(pop.top, div = try, n.seats, n.places) > n.seats){
            ##  The range of divisors ends at the second to last divisor used
            end <- potential.divisors[length(potential.divisors)-1]
            ##  append the ending divisor to the vector range.divisors
            range.divisors <- c(range.divisors, end)
            ##  sort the range.divisors because the reduce method starts 
            ##  at a bigger number and ends at a smaller one
            range.divisors <- sort(range.divisors)
            ##  print the title "Range of Divisors"
            print ("Range of Divisors")
            ##  return range.divisors
            return (range.divisors)
          }
        }
        
        ##  If we were increasing the divisor previously, we need to continue increasing it 
        ##  until we get to a divisor that no longer works
        else if (procedure == "increase"){
          ##  systematically check for the next divisor by testing the number one more than the one just used
          try <- potential.divisors[length(potential.divisors)] + 1
          ##  appends the divisor we're trying to potential.divisors
          potential.divisors <- c(potential.divisors, try)
          ##  use test.huntingtonhill to compute the total number of seats allocated with the new divisor
          test.huntingtonhill(pop.top, div = try, n.seats, n.places)
          
          ##  If the last divisor used no longer works and gives us too few seats, then the range 
          ##  of divisors stops at the second to last element (because that one worked)
          if (test.huntingtonhill(pop.top, div = try, n.seats, n.places) < n.seats){
            ##  The range of divisors ends at the second to last divisor used
            end <- potential.divisors[length(potential.divisors)-1]
            ##  append the ending divisor to the vector range.divisors
            range.divisors <- c(range.divisors, end)
            ##  print the title "Range of Divisors"
            print ("Range of Divisors")
            ##  return range.divisors
            return (range.divisors)
          }
        }
        ## If the initial divisor is perfectly fine, then return "it's fine"
        else{
          return (procedure)
        }
      }
    }
  }
}

#############################
##  APPORTIONMENT BUILDER  ##
#############################

## YOU HAVE TO INPUT THE NUMBER OF PLACES THAT NEED SEATS AND THE METHOD USED
build.apportionment <- function(n.places, method){
  
  ##  randomly select a number between 50 and 100 to be the number of seats needing allocated
  n.seats <- sample(50:100, 1)
  ##  allowed to choose the number of places need seats
  ##  passes chosen value of places to use
  n.places <- n.places
  
  ##  Give your places a name!
  stuff.names <- LETTERS[1:n.places]
  
  ##  WHAT TO DO IF YOU DON'T LIKE THE NAMES AS LETTERS OF THE ALPHABET
  ##  STEP 1: COMMENT OUT STUFF.NAMES FROM ABOVE
  ##  STEP 2: UNCOMMENT NEXT LINE AND FILL IN THE NAMES (MAKE SURE YOU HAVE AS MANY NAMES AS PLACES YOU WANT)
  
  ## append "Total" to you stuff.names so that we can name the row information in our matrix
  places.name <- c(stuff.names, "Total")
  
  ## create the population matrix
  pop.top <- matrix(nrow = n.places + 1, ncol = 7)
  rownames(pop.top) <- places.name
  colnames(pop.top) <- c("Population", "Quota", "Lower.Quota", "Geometric.Mean", "Initial", "Final", "(Extra for Hamilton)")
  
  ##  don't want wonky population values to deal with
  nice.numbers <- seq(from = 25000, to = 100000, by = 500)
  ##  Take a sample of the nice.numbers to use as population values
  populations <- sample(nice.numbers, n.places)
  ##  append those population values to the population matrix
  for (i in 1:n.places){
    pop.top[i,"Population"] <- populations[i]
  }
  
  ##  calculate the total population
  pop.top["Total","Population"] <- sum(pop.top[c(1:n.places),"Population"])
  
  ##  Calculate the initial divisor
  total.pop <- pop.top["Total","Population"]
  divisor <- total.pop / n.seats
  
  print("Number of Seats")
  print(n.seats)
  print("Divisor")
  print(divisor)
  
  mod.div.list <- vector()
  
  ##  compute the apportionment using Hamilton's method
  if (method == "hamilton"){
    print(hamilton(pop.top, div = divisor, n.seats = n.seats, n.places = n.places))
  }
  ##  compute the apportionment using Jefferson's method
  else if (method == "jefferson"){
    print("Original Apportionment Table")
    print(original.jefferson(pop.top, div = divisor, n.seats = n.seats, n.places = n.places))
    
    print(jefferson(pop.top, div = divisor, n.seats = n.seats, n.places = n.places, mod.div.list = mod.div.list))
    
    print(range.jefferson(pop.top, div = divisor, n.seats = n.seats, n.places = n.places))
  }
  ##  compute the apportionment using Huntington Hill's method
  else if (method == "huntington.hill"){
    print("Original Apportionment Table")
    print(original.huntingtonhill(pop.top, div = divisor, n.seats = n.seats, n.places = n.places))
    
    print(huntington.hill(pop.top, div = divisor, n.seats = n.seats, n.places = n.places, mod.div.list = mod.div.list))
    
    print(range.huntingtonhill(pop.top, div = divisor, n.seats = n.seats, n.places = n.places))
  }
  
}

##  automatically generates a seed number
##  However, seed numbers can be input manually
##  Comment out sample(1:999, 1) and type in the desired seed number
##  It will look like this:   auto <- 123#sample(1:999, 1)
auto <- 3969#sample(1:999, 1) #0
#auto <- auto + 1
print(auto)

## Solves the apportionment problem using each method
set.seed(auto)
build.apportionment(n.places = 5, method = "hamilton")
set.seed(auto)
build.apportionment(n.places = 5, method = "jefferson")
set.seed(auto)
build.apportionment(n.places = 5, method = "huntington.hill")
