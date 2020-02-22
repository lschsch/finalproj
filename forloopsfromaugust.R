# load in the BN learn library

library("bnlearn")

#-------------------simulating marshmallow dataset------------------------------

# variable names for our nodes
# each variable will have two factors
# sex: girl or boy
# age: <3 or >3
# companion in room: yes or no
# Eats marshmallow before 10 min: yes or no


# below we will make an empty dataframe
marshmallows <- data.frame(rep(NA, 100))

# now we can fill in our columns one by one
marshmallows$sex <- append(rep("girl", 50), rep("boy", 50))

# lets take out the NA column
marshmallows$rep.NA..100. <- NULL

# now we want to build in new columns one by one
# lets build in the age column, we want in total for about half of the children
# to be older than three and half to be less than three
marshmallows$age <- sample(append(rep("<3", 50), rep(">3", 50)), replace = FALSE)

# lets add wheteher or not they have a companion
marshmallows$companion <- sample(append(rep("yes", 50), rep("no", 50)), replace = FALSE)


# finally, we want to add in the conditional probability
# we will make an if statement for all of the possible outcomes
# this will be a conditional probability and will determine whether
# the child eats the marshmallow before 10 minutes or not
# when we make our network, we can check that the conditional
# probabilities match up with what we tell R 

# we make the empty column for outcome which we will populate with "yes" or "no"
# to indicate if the ate their marshmallow before 10 minutes
marshmallows$outcome <- rep(NA, 100)

# lets include conditional probability
   # girls
   # companion
outcome.vec <- sample(c("yes","no"), 100, prob=c(0.7, 0.3), rep=T)  # No
outcome.vec2 <- sample(c("yes","no"), 100, prob=c(0.3, 0.7), rep=T) # Yes
  # if older than 3
outcome.vec3 <- sample(c("yes","no"), 100, prob=c(0.75, 0.25), rep=T)  # No
outome.vec4 <- sample(c("yes","no"), 100, prob=c(0.25, 0.75), rep=T)   # Yes
   # if boy    ### ALL OF THESE SHOULD BE BASED ON ACTUAL PROBABILITIES
   # age younger
outcome.vec5 <- sample(c("yes","no"), 100, prob=c(0.7, 0.3), rep=T)
outome.vec6 <- sample(c("yes","no"), 100, prob=c(0.3, 0.7), rep=T)
   # older (same as girl)
outcome.vec7 <- sample(c("yes","no"), 100, prob=c(0.85, 0.15), rep=T)
outcome.vec8 <- sample(c("yes","no"), 100, prob=c(0.15, 0.85), rep=T)
# older than three
outcome.vec9 <- sample(c("yes","no"), 100, prob=c(0.75, 0.25), rep=T)
outome.vec10 <- sample(c("yes","no"), 100, prob=c(0.25, 0.75), rep=T)



# from https://stats.stackexchange.com/questions/15141/how-to-create-a-dataset-with-conditional-probability
diseaserate <- 3/1000
symptomrate <- 5/1000
symptomgivendisease <- 0.3

status  <- sample(c("SYDY", "SNDY", "SYDN", "SNDN"), 1000, 
                  prob=c(diseaserate * symptomgivendisease,
                         diseaserate * (1-symptomgivendisease),
                         symptomrate - diseaserate * symptomgivendisease,
                         1 - symptomrate - diseaserate * (1-symptomgivendisease)),
                  rep=TRUE)
symptom <- status %in% c("SYDY","SYDN")
disease <- status %in% c("SYDY","SNDY")

# now to make this work for what we're doing
marshmallow <- marshmallows

for(i in 1:length(marshmallow$sex)){
  if(marshmallow$sex[i] == "girl"){
    if(marshmallow$age[i] == ">3"){
      if(marshmallow$companion[i] == "no"){
        marshmallow$outcome <-  outcome.vec
      }
    }
  }
}




# in this case, we just want for R to leave the value alone if it is not specified
# therefore, we only need if statements and no else statements
# this works also and is much more concise
# not to self: else statements are just added after each curly bracket
# also recall that else statements/commands are inside {}
for(i in 1:length(marshmallows$sex)){
  if(marshmallows$sex[i] == "girl"){
    if(marshmallows$age[i] == "<3"){
      if(marshmallows$companion[i] =="no"){
        marshmallows$outcome[i] <- outcome.vec[i]
      }
    } 
  } 
}

# for all of them      # COULD YOU DO EACH IN ONE IF STATEMENT
for(i in 1:length(marshmallows$sex)){
  if(marshmallows$sex[i] == "girl"){
    if(marshmallows$age[i] == "<3"){
      if(marshmallows$companion[i] =="no"){
        marshmallows$outcome[i] <- outcome.vec[i]
        else{
          marshmallows$outcome[i] <- outcome.vec2[i]
        }
      }
      else if(marshmallows$age[i] == ">3"){
        if(marshmallows$companion[i] =="no"){
          marshmallows$outcome[i] <- outcome.vec3[i]
          else{
            marshmallows$outcome[i] <- outcome.vec4[i]
          }
        }  
      }
    }
  }
    else if(marshmallows$sex[i] == "boy"){
      if(marshmallows$age[i] == "<3"){
        if(marshmallows$companion[i] =="no"){
          marshmallows$outcome[i] <- outcome.vec5[i]
          else{
            marshmallows$outcome[i] <- outcome.vec6[i]
          }
        }
        else if(marshmallows$age[i] == ">3"){
          if(marshmallows$companion[i] =="no"){
            marshmallows$outcome[i] <- outcome.vec7[i]
            else{
              marshmallows$outcome[i] <- outcome.vec8[i]
            }
          }  
        }
      }
    }
  } 
}
## DOESN'T WORK

# vectors for new attempt
outcome.vect1 <- sample(c("yes","no"), 100, prob=c(0.5, 0.5), rep=T)

# setting index 
j <- 1
## lets try with less if statements
for(j in 1:length(marshmallows$sex)){
  if(all(marshmallows$sex[j] == "girls", marshmallows$age[j] == "<3", 
     marshmallows$companion[j] == "yes")){
    marshmallows$outcome[j] <- outcome.vect1[j]
  }
  else if(all(marshmallows$sex[j] == "girls", marshmallows$age[j] == "<3", 
          marshmallows$companion[j] == "no")){
    marshmallows$outcome[j] <- outcome.vect1[j]
  }
  else if(all(marshmallows$sex[j] == "girls", marshmallows$age[j] == ">3", 
    marshmallows$companion[j] == "yes")){
    marshmallows$outcome[j] <- outcome.vect1[j]
  }
  else if(all(marshmallows$sex[j] == "girls", marshmallows$age[j] == ">3", 
          marshmallows$companion[j] == "no")){
    marshmallows$outcome[j] <-  outcome.vect1[j]
  }
  else if(all(marshmallows$sex[j] == "boys", marshmallows$age[j] == "<3", 
          marshmallows$companion[j] == "yes")){
    marshmallows$outcome[j] <- outcome.vect1[j]
  }
  else if(all(marshmallows$sex[j] == "boys", marshmallows$age[j] == "<3", 
          marshmallows$companion[j] == "no")){
    marshmallow$outcome[j] <- outcome.vect1[j]
  }
  else if(all(marshmallows$sex[j] == "boys", marshmallows$age[j] == ">3", 
          marshmallows$companion[j] == "yes")){
    marshmallows$outcome[j] <- outcome.vect1[j]
  }
  else if(all(marshmallows$sex[j] == "boys", marshmallows$age[j] == ">3", 
          marshmallows$companion[j] == "no")){
    marshmallows$outcome[j] <- outcome.vect1[j]
  }
}


# build in our own conditional probabilities
# use if statements to tell R how to construct the network for each column
# start with one and use if statements to fill in the next column


samp <- mean(rnorm(100, mean = 0.6, sd = 0.2))

test <- append(c(rep("yes", floor(100*samp))), c(rep("no", ceiling((100 - 100*samp)))))    
ranodmized_test <- sample(test)



#--------------------creating a simple BN structure by hand---------------------


test <- empty.graph(c("sex", "age", "companion", "eats"))
plot(test)


# we now want to add in an arc set
# we need to put the arcs into a matrix in order to do this
edges <- matrix(c("sex", "eats",
                  "age", "eats",
                  "companion", "age",
                  "companion", "eats"),
                byrow = TRUE, ncol = 2,
                dimnames = list(NULL, c("from", "to")))

# below we assign our edges to the graph
arcs(test) <- edges

# plot it to see that it did what we wanted
# note that it seems as though R by default makes the graph directed
# makes sense since we are in the BN package which is by defn directed graphs
plot(test)

# we want to create the conditional probability table by hand now
# we need to store the states of each node into individual objects

sex <- c("boy", "girl")
age <- c("<3", ">3")
companion <- c("yes", "no")
