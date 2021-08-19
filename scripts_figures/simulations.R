#this script plots the eco-evolutionary simulations

list.files("../data/", full.names = T)

plot.sim<-function(pathname){
  plot.data = read.csv(pathname, sep=";", header=TRUE)
  plot(plot.data$timevec, plot.data$trait, pch=20, cex=0.1,col="black", ylim=c(10,0),xlab="time", ylab="z, deph in the lake (m)")
}

figA2 =  "../data/simulation_p0b0u3w0.3timemax5e+06popmin0.01probmut1e-04ampmut0.05traitini2.4.csv" 
figB2 =  "../data/simulation_p0b1u4.8w0.1timemax5e+07popmin0.01probmut1e-04ampmut0.05maxpop50.csv"
figC2 =  "../data/simulation_p0.5b1u4.5w0.15timemax5e+07popmin0.01probmut1e-04ampmut0.05traitini1.csv"

plot.sim(figA2)
plot.sim(figB2)
plot.sim(figC2)