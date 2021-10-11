#this script plots the eco-evolutionary simulations

list.files("../data/", full.names = T)

plot.sim<-function(pathname){
  plot.data = read.csv(pathname, sep=";")
  plot(plot.data$timevec, plot.data$trait, pch=20, cex=0.1,col="black",xlab="time", ylim=c(10,0), ylab="z, deph in the lake (m)")
}

fig3A2 =  "../data/simulation_p0b0u3w0.3.csv" 
fig3B2 =  "../data/simulation_p0b1u4.8w0.1.csv"
fig3C2 =  "../data/simulation_p0.5b1u4.5w0.15.csv"

fig4A = "../data/simulation_p0.5b1u4.5w0.2.csv" 
fig4B =  "../data/simulation_p0.5b1u4.5w0.15.csv"
fig4C = "../data/simulation_p0.5b1u4.5w0.1.csv"

plot.sim(fig3A2)
plot.sim(fig3B2)
plot.sim(fig3C2)


plot.sim(fig4A)
plot.sim(fig4B)
plot.sim(fig4C)
