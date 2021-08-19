#Scripts used to make the E3-diagrams
#Requires output from the Mathematica file "Model analysis"

#Graphical choices------------------------------------------------------------------------------
#Colors chosen each strategies
un.col = "black" #unfeasible
br.col = "red" #color for branching
css.col = "black" #color for CSS
rep.col = "black" #color for repellor
ge.col = "black" #color for garden of Eden

lty.css = 1 #line type for CSS
lty.br = 2 #line type for branching
lty.rep = 3 #line type for repellor
lty.ge = 3 #line type for garden of eden

#Base functions---------------------------------------------------------------------------------------

make.E3.diagram <- function(result, parameter, title){
  "creates the base 2D diagram 
  result : the dataset 
  parameter : the parameter with respect to which is drawn the E3 plot
  title : the title given to the figure"
  plot(NULL, xlim=c(min(parameter),max(parameter)), ylim=c(10,0), xlab="parameter", ylab="z*, selected strategy (m)", main=title)
  lines(parameter[which(result$type=="CSS"|result$type=="No ES")], result$ES[which(result$type=="CSS"|result$type=="No ES")], col=css.col, lty=lty.css)
  lines(parameter[which(result$type=="Branching point")], result$ES[which(result$type=="Branching point")], col="white", lwd=10)
  lines(parameter[which(result$type=="Branching point")], result$ES[which(result$type=="Branching point")], col=br.col, lty=lty.br)
  lines(parameter[which(result$type=="Repellor")], result$ES[which(result$type=="Repellor")], col=rep.col, lty=lty.rep)
  lines(parameter[which(result$type=="Garden of Eden")], result$ES[which(result$type=="Garden of Eden")], col=ge.col, lty=lty.ge)

  polygon(c(parameter[which(result$zmin>0)],min(parameter)), c(result$zmin[which(result$zmin>0)],0), col=un.col)
  if(length(parameter[which(result$zmax<10)])>0){ 
  polygon(c(parameter[which(result$zmax<10)],max(parameter),min(parameter)),c(result$zmax[which(result$zmax<10)],10,10), col=un.col)
    }
  }

#b=0;p=0, w=0.3 ----------------------------------------------------------------------------------------------
result <- read.csv("../data/evolution_scenario1.txt", header=TRUE, comment.char="#")
colnames(result) <- c("u","w","zmin","zmax", "ES", "type")
result <-result[which(result$w==0.3),]
make.E3.diagram(result,result$u,"sc1 _ u ")
polygon(c(1,2.08,2.12,1),c(0,6,10,10), col=1)
points(3, result$ES[which(result$u==3)], col="purple", pch=20)

#b=1;p=0, w=0.1 ----------------------------------------------------------------------------------------------
result <- read.csv("../data/evolution_scenario2.txt", header=TRUE, comment.char="#")
colnames(result) <- c("u","w","zmin","zmax", "ES", "type")
result <-result[which(result$w==0.1),]
make.E3.diagram(result,result$u,"sc2 _ u ")
points(4.8, result$ES[which(result$u==4.8)], col="purple", pch=20)


#b1;p=0.5, w=0.15 ------------------------------------------------------------------------------------------
result <- read.csv("../data/evolution_scenario3.txt", header=TRUE, comment.char="#")
colnames(result) <- c("u","w","zmin","zmax", "ES", "type")
result <-result[which(result$w==0.15),]

make.E3.diagram(result,result$u,"sc3 _ u ")
points(4.5, result$ES[which(result$u==4.5)], col="purple", pch=20)
