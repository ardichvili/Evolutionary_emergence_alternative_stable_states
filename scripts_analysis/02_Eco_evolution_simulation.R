## This script contains a temporal simulation of the adaptive dynamics of macrophytes
## The code was originally made by Avril Weinbach then modified by Alice Ardichvili 

## Necessary packages to run the code --------------------------------------------------------------------------------------
library(deSolve) # to use the ode function

## Parameters names and their values --------------------------------------------------------------------------------------------

#ecological dynamics parameters
b = 1
p = 1
u = 3.6
w = 0.3

a0= 0.01
Nu0 = 5
W0 = 1
q0 = 0.005

zb = 10
h = 0.1
r = 0.5
l = 0.05

# simmulation time parameters
time=0
timemax=50000000

# mutation process parameters
popmin=1e-2
probmut=1e-4 
ampmut=0.05
maxpop=50 #this setting only exists for time constraints, removing it will yields the same result but after longer simulation time

#initialisation
popini = 10
traitini = 1



## Ecological dynamics ----------------------------------------------------------------------------------------------------------

dMdt=function(time,pop,trait){
  # The function returns the growth rate of the N macrophyte populations (in a vector dim N)
  # time is a time vector added to use the ode function from package deSolve
  # pop is a vector (dim N) containing the density of the macrophyte populations
  # trait is a vector (dim N) containing corresponding trait values, here depth in the lake
  # this function requires the use of the functions shading and nutrient.limitation
  with(as.list(c(pop, trait)),{
    n <- nutrient.content(trait) / (1 + sum(nutrient.retention(trait) * pop ))
    dM <- r * pop * (n/(n+h)) * (1/(1 + turbidity(trait) + apply(shading(trait)*pop, 2,sum))) - l * pop 
    list(dM)
  }) # end with(as.list...) if several species.
}

## Explicit expression of the parameters functions ------------------------------------------------------------------------------

shading <- function(trait){
  #returns (matrix dim NxN) the shading coefficients that the colum pop experiences from the row pop. 
  #trait is a vector of trait (dim N)
  dim = length(trait)
  a <- matrix(ncol = dim, nrow = dim)
  for (i in c(1:dim)){
    for (j in c(1:dim)){
      a[i,j] <- (2*a0*exp(b*(trait[j]-trait[i])))/(1+exp(b*(trait[j]-trait[i])))
    }
  }
  return(a)
}

nutrient.content <- function(z){
  #returns (numeric) the nutrient content at a given depth z 
  Nu <- (Nu0/(u*sqrt(2*pi))) * exp(-(zb-z)^2/(2*u^2))
  return(Nu)
}

turbidity <- function(z){
  #returns (numeric) the background turbidity at a given depth z 
  W <- W0 * exp(w*z) - W0
  return(W)
}

nutrient.retention <- function(z){
  #returns (numeric) the background turbidity at a given depth z 
  q <- q0 * exp(p*z)
  return(q)
}

## Evolutionary dynamics, Mutation process -------------------------------------------------------------------------------------------------------------
# Identification of mutations
mutid=function(pop){
  # gives the position of the mutating populations, depedning on the proba of mutation probmut (previously defined),
  # and the size of the populations pop
  probvec=probmut*pop;
  randvec=c(runif(length(pop)));
  mutvec=randvec<probvec;
  return(which(mutvec));
};

# Drawing new traits
newtraits=function(mut,trait){
  # compute a new vector of trait with the mutations added
  # mut is a vector indicating which populations are leading to mutant
  # trait is the vector of trait values
  temptraits=as.vector(rnorm(length(mut),mean=trait[mut],sd=ampmut))
  temptraits[which(temptraits>zb)] = zb # remove values that are above the surface or below the bottom of the lake 
  temptraits[which(temptraits<0)] = 0
  trait=c(trait,temptraits); # add the new traits to the existing trait vector
  return(trait)
};


## Introduction of the mutant
intropop=function(pop,mut){
  # pop is the vector containing the population densities
  # mut is a vector indicating which population are leading to mutants
  #returns a new vector of population densities 
  div=length(mut);
  newpop=rep(popmin, div)
  pop=c(pop,newpop)
  return(pop)
};


## Extinctions of populations that are too small

# Keep only species whose population is large enough
survpop=function(pop){
  # compute a vector containing only the surviving populations
  # pop is the vector containing the pollinators population densities
  # popmin is the minimum population size, previously defined
  as.vector(pop[pop>popmin])
};

# Keep only the traits corresponding to the surviving populations
survtrait=function(pop,trait){
  # compute a vector containing only the traits surviving populations
  # pop is the vector containing the population densities
  # trait is a vector containing the macrophyte's position in the lake
  # popmin is the minimum population size, previously defined
  as.vector(trait[pop>popmin])
}

## Main function  -----------------------------------------------------------------------------------------------------

ecoevodyn=function(time, pop, trait, tab){
  while(time<timemax){
    # population dynamics 
    popvar <- as.matrix(ode(y = pop, times = c(time, time+1), func = dMdt, parms = trait ))# this gives the present pop value,
    # and the pop density at the next time step
    # the new population variation is 
    newpop <- as.vector(popvar[2,2:dim(popvar)[2]])
    # newpop <- dMdt(c(1),pop,trait)[[1]] + pop
    
    # extinctions
    if(length(trait)>=1){
      trait=survtrait(newpop,trait);
      pop=survpop(newpop);
    };
    
    
    # mutations
    if(length(trait)<maxpop){
      vecmut=as.vector(mutid(pop));
      if(length(vecmut)>0){
        trait=newtraits(vecmut,trait);
        pop=intropop(pop,vecmut);
      };
    };

    
    
    # fill the time vector with repetition of the present time value
    addtime=rep(time+1, length(pop))
    time=time+1;
    
    # Saving once in a while 
    skip = timemax/1000  
    newtab=cbind(addtime, pop, trait)
    if(time%%skip==0){
      write.table(newtab,file=filename,append=TRUE, sep = ";", row.names =FALSE,col.names=FALSE)
    }
  };
}

## Main part of the program --------------------------------------------------------------------------------------------

filename = paste0("../data/simulation_","p",p, "b",b,"u",u,"w",w,"timemax",timemax,"popmin",popmin,"probmut",probmut,"ampmut",ampmut, "traitini", traitini,".csv")

timevec=c(time)

# initial populations
pop=c(popini)

#initial trait of the populations
trait = c(traitini)

# store the initial values in a table
tab=cbind(timevec, pop, trait)

write.table(tab,file=filename,append=FALSE,row.names=FALSE,col.names=TRUE, sep = ";")

ecoevodyn(time,pop,trait,tab)


