# now, copying what we did for smoking but using our data frame w cp for 
#  marshmallows (called mmlws)

# from makingdata



# load in the "bnlearn" library for bayesian networs
library("bnlearn")

# instead of using a pre-loaded dataset, use own dataset
# assign data set to new object (also to make sure we don't accidentally 
#  change it)
marsh <- mmlws

# lets take a look at the data
marsh
head(marsh)
tail(marsh)


# to learn the structure of the marsh data frame and load it into an object 
#  using the hill climbing algorithm
bn_marsh <- hc(marsh)  
# this gives error message
## Error in check.data(x) : the data set contains NaN/NA values.
# this error does not make sense as the data set smoking also had NaN values, so 
#  either we have to tell it to expect them or it sees NA values, though there
#  are none

# lets check what smoking was
str(smoking)
## 'data.frame':	1841 obs. of  6 variables:
## $ Smoking : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
## $ M. Work : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
## $ P. Work : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
## $ Pressure: Factor w/ 2 levels "<140",">140": 1 1 1 1 1 1 1 1 1 1 ...
## $ Proteins: Factor w/ 2 levels "<3",">3": 1 1 1 1 1 1 1 1 1 1 ...
#  $ Family  : Factor w/ 2 levels "neg","pos": 1 1 1 1 1 1 1 1 1 1 ...

# now lets check structure of marsh
str(marsh)
## 'data.frame':	100 obs. of  4 variables:
## $ sex      : chr  "girl" "girl" "girl" "girl" ...
## $ age      : chr  ">3" ">3" ">3" "<3" ...
## $ companion: chr  "no" "no" "yes" "yes" ...
## $ outcome  : chr  "yes" "no" "yes" "no" ...

# so the difference is that in marsh, the columns aren't Factors with 2 levels

  # from https://stackoverflow.com/questions/33180058/coerce-multiple-columns-to-factors-at-once
  #  could convert each column into factors separately with
  #  data$A = as.factor(data$A)

  # make a vector with the different column names
  cols <- c("sex", "age", "companion", "outcome")

  # turn the columns into factors (self assign levels i think?)
  marsh[cols] <- lapply(marsh[cols], factor)  # as.factor() could also be used

  # to check if they are now factors
  sapply(marsh, class)

# back to my own 

# could also check using structure
str(marsh)

# now lets go back to where we were
bn_marsh <- hc(marsh)               # THIS MAY BE THE SAME ISSUE WE HAD WITH THE
                                    # BOOKDOWN PAGE STUFF!!!!
# to plot the bn 
plot(bn_marsh)

# apparently the only edge is from sex to outcome? Maybe that's the only 
#  condition the algorithm found? 
#  Maybe should expand to be 1000 rather than 100 entries? Would be easy now

# not gonna remove any edges... we already only have one...

# storing all the conditional probability tables 
fit_bn <- bn.fit(bn_marsh, data = marsh)

# lets look at the likelihood of eating the marshmallow given that it's a boy
cpquery(fit_bn, event = (outcome == 'yes'), evidence = (sex == 'boy'))
## 0.3920455
# so this says that the probability of eating the marshmallow if a boy is
#   about 0.4

# what about for girl
cpquery(fit_bn, event = (outcome == 'yes'), evidence = (sex == 'girl'))
## 0.1407826
# so this says that the probability of eating the marshmallow if a girl is 
#   about 0.14, which makes sense that it's a lot lower based on the 
#   probabilities used when building the data frame mmlws


















