# This scripts creates the bifurcation plots of Figure 4 

par(mfrow=c(2,2))

co1 = "black"
co2 = "grey"
labx = expression("Nu"[0]*", total nutrient concentration (mg/dm"^2*")")
laby = "Population density (g/L)"

plot.perturbation <-function(pathname){
  pop = read.csv(pathname, comment.char = "#")
  plot(pop$Nu0[which(pop$stable=="True")], pop$M1[which(pop$stable=="True")], col=co1, lwd=3 )
  points(pop$Nu0[which(pop$stable=="True")], pop$M2[which(pop$stable=="True")], col=co2, lwd=3 )
  #its also possible to add the unstable equilibria by adding 
  #[which(pop$stable=="False" && pop$M1>0)] in the appropriate range
  }


plot.perturbation("../data/perturbation_scenario3_u45w02.txt")
plot.perturbation("../data/perturbation_scenario3_u45w015.txt")
plot.perturbation("../data/perturbation_scenario3_u45w01t750.txt")
plot.perturbation("../data/perturbation_scenario3_u45w01t1370.txt")
