# appended functions to get probabilites 

sub.size <- function(x, y, z){
  length(mmlws$sex[mmlws$sex == x 
                   & mmlws$age == y 
                   & mmlws$companion == z])
}

prop.vec <- function(n, p, x, y){
  x.count <- rep(x, round(n*p))
  y.count <- rep(y, round(n*(1-p)))
  return((sample(append(x.count, y.count)))[1:n])
}

# the data frame 
# from loopy

sex <- c("boy", "girl")
age <- c("<3", ">3")
companion <- c("yes", "no")

mmlws <- data.frame(rep(NA, 100))

mmlws$sex <- append(rep("girl", 50), rep("boy", 50))

mmlws$rep.NA..100. <- NULL

mmlws$age <- sample(append(rep("<3", 50), rep(">3", 50)), replace = FALSE)

mmlws$companion <- sample(append(rep("yes", 50), rep("no", 50)), 
                          replace = FALSE)

mmlws$outcome <- rep(NA, 100)

# the function
pop.out <- function(x, y, z, p){
  for(i in 1:length(mmlws$sex)){
    if(mmlws$sex[i] == x){
      if(mmlws$age[i] == y){
        if(mmlws$companion[i] == z){
          mmlws$outcome[i] <- prop.vec(sub.size(x, y, z), p, "yes", "no")[h]
          print(prop.vector(subset.size(x, y, z), p, "yes", "no"))
          h <- h + 1
        }
      } 
    } 
  }
}      # unfortunately, as soon as it is a function it doesn't run properly...
       #  not sure why this is the case, as the only thing that is changing 
       #  is that it's a function. Anyways, running it separately by hand, 
       #  is currently the best option

# to initialize the arguments
h <- 1
i <- 1
x <- sex[1]         # this combination is boy, <3, yes
y <- age[1]
z <- companion[1]
p <- 0.6            # currently the proportion is for yes

# changing the values for x, y, z (the arguments) and reinitializing the indeces
#  as well as changing the probability (if wanted)

# there are 8 combinations
# after each combination, run the for loop EVERY TIME
for(i in 1:length(mmlws$sex)){
  if(mmlws$sex[i] == x){
    if(mmlws$age[i] == y){
      if(mmlws$companion[i] == z){
        mmlws$outcome[i] <- prop.vec(sub.size(x, y, z), p, "yes", "no")[h]
        print(prop.vector(subset.size(x, y, z), p, "yes", "no"))
        h <- h + 1
      }
    } 
  } 
}

# from above
# boy, <3, yes (first)
h <- 1
i <- 1
x <- sex[1]         # this combination is boy, <3, yes
y <- age[1]
z <- companion[1]
p <- 0.8

# boy, >3, yes (second)
h <- 1
i <- 1
x <- sex[1]         # this combination is boy, >3, yes
y <- age[2]
z <- companion[1]
p <- 0.7

# boy, <3, no (third)
h <- 1
i <- 1
x <- sex[1]         # this combination is boy, <3, no
y <- age[1]
z <- companion[2]
p <- 0.8 

# boy, >3, no (fourth)
h <- 1
i <- 1
x <- sex[1]         # this combination is boy, >3, no
y <- age[2]
z <- companion[2]
p <- 0.9

# girl, <3, yes (fifth)
h <- 1
i <- 1
x <- sex[2]         # this combination is girl, <3, yes
y <- age[1]
z <- companion[1]
p <- 0.4

# girl, >3, yes (sixth)
h <- 1
i <- 1
x <- sex[2]         # this combination is girl, >3, yes
y <- age[2]
z <- companion[1]
p <- 0.2

# girl, <3, no (seventh)
h <- 1
i <- 1
x <- sex[2]         # this combination is girl, <3, no
y <- age[1]
z <- companion[2]
p <- 0.4

# girl, >3, no (eight)
h <- 1
i <- 1
x <- sex[2]         # this combination is girl, >3, no
y <- age[2]
z <- companion[2]
p <- 0.2



# now lets look at the data frame
mmlws

# and the outcome is filled!!!







