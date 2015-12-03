
png("/Users/gregorymatthews/snowTest1small.png",h=13,w=19,units="in",res=100)
par(mfrow=c(5,8))
par(mar=c(0,0,0,0))
for (greg in 1:40){print(greg)
plot(0,0,frame.plot=F,xlim=c(-10,10),ylim=c(-10,10),col="white",asp=1,xaxt='n',yaxt='n',xlab="",ylab="")

th <- runif(1,0,2*pi)

lwd<-runif(1,5,10)
x<-c(0,5)
y<-c(0,5)

for (theta in th+seq(0,2*pi,length=7)[-1]){
xTemp<-t(matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),ncol=2,byrow=TRUE)%*%t(cbind(x,y)))[,1]
yTemp<-t(matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),ncol=2,byrow=TRUE)%*%t(cbind(x,y)))[,2]
points(xTemp,yTemp,type="l",lwd=lwd,col=rgb(0,0,1,.1))
#polygon(c(xTemp),c(yTemp),col=rgb(0,0,1,0.05),bor=rgb(0,0,1,0.05))
}

yy<-runif(1,6,8)
x<-c(5,5)
y<-c(5,yy)

for (theta in th+seq(0,2*pi,length=7)[-1]){
  xTemp<-t(matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),ncol=2,byrow=TRUE)%*%t(cbind(x,y)))[,1]
  yTemp<-t(matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),ncol=2,byrow=TRUE)%*%t(cbind(x,y)))[,2]
 # points(xTemp,yTemp,type="l",lwd=lwd,col=rgb(0,0,1,.1))
  #polygon(c(xTemp),c(yTemp),col=rgb(0,0,1,0.05),bor=rgb(0,0,1,0.05))
}


x<-c(5,yy)
y<-c(5,5)

for (theta in th+seq(0,2*pi,length=7)[-1]){
  xTemp<-t(matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),ncol=2,byrow=TRUE)%*%t(cbind(x,y)))[,1]
  yTemp<-t(matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),ncol=2,byrow=TRUE)%*%t(cbind(x,y)))[,2]
 # points(xTemp,yTemp,type="l",lwd=lwd,col=rgb(0,0,1,.1))
  #polygon(c(xTemp),c(yTemp),col=rgb(0,0,1,0.05),bor=rgb(0,0,1,0.05))
}


yy<-runif(1,5,7)
  x<-c(4,4)
  y<-c(4,yy)
  
  for (theta in th+seq(0,2*pi,length=7)[-1]){
    xTemp<-t(matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),ncol=2,byrow=TRUE)%*%t(cbind(x,y)))[,1]
    yTemp<-t(matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),ncol=2,byrow=TRUE)%*%t(cbind(x,y)))[,2]
    points(xTemp,yTemp,type="l",lwd=lwd,col=rgb(0,0,1,.1))
    #polygon(c(xTemp),c(yTemp),col=rgb(0,0,1,0.05),bor=rgb(0,0,1,0.05))
  }


x<-c(4,yy)
y<-c(4,4)

for (theta in th+seq(0,2*pi,length=7)[-1]){
  xTemp<-t(matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),ncol=2,byrow=TRUE)%*%t(cbind(x,y)))[,1]
  yTemp<-t(matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),ncol=2,byrow=TRUE)%*%t(cbind(x,y)))[,2]
  points(xTemp,yTemp,type="l",lwd=lwd,col=rgb(0,0,1,.1))
  #polygon(c(xTemp),c(yTemp),col=rgb(0,0,1,0.05),bor=rgb(0,0,1,0.05))
}

  
yy<-runif(1,3,6)
x<-c(3,yy)
y<-c(3,3)

for (theta in th+seq(0,2*pi,length=7)[-1]){
  xTemp<-t(matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),ncol=2,byrow=TRUE)%*%t(cbind(x,y)))[,1]
  yTemp<-t(matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),ncol=2,byrow=TRUE)%*%t(cbind(x,y)))[,2]
  points(xTemp,yTemp,type="l",lwd=lwd,col=rgb(0,0,1,.1))
  #polygon(c(xTemp),c(yTemp),col=rgb(0,0,1,0.05),bor=rgb(0,0,1,0.05))
}


x<-c(3,3)
y<-c(3,yy)

for (theta in th+seq(0,2*pi,length=7)[-1]){
  xTemp<-t(matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),ncol=2,byrow=TRUE)%*%t(cbind(x,y)))[,1]
  yTemp<-t(matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),ncol=2,byrow=TRUE)%*%t(cbind(x,y)))[,2]
  points(xTemp,yTemp,type="l",lwd=lwd,col=rgb(0,0,1,.1))
  #polygon(c(xTemp),c(yTemp),col=rgb(0,0,1,0.05),bor=rgb(0,0,1,0.05))
}

for (q in 1:2){
w<-runif(1,1,3)
h<-runif(1,0,2)
x<-c(0,w,w,h,0)
y<-c(0,h,w,w,0)
for (theta in th+seq(0,2*pi,length=7)[-1]){
  xTemp<-t(matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),ncol=2,byrow=TRUE)%*%t(cbind(x,y)))[,1]
  yTemp<-t(matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),ncol=2,byrow=TRUE)%*%t(cbind(x,y)))[,2]
  #points(xTemp,yTemp,type="l",lwd=3)
  polygon(c(xTemp),c(yTemp),col=rgb(0,0,1,.1),bor=rgb(0,0,1,.1))
}

w<-runif(1,1,3)
h<-runif(1,0,2)
x<-c(0,w,w,h,0)
y<-c(0,h,w,w,0)
for (theta in th+pi/6+seq(0,2*pi,length=7)[-1]){
  xTemp<-t(matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),ncol=2,byrow=TRUE)%*%t(cbind(x,y)))[,1]
  yTemp<-t(matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),ncol=2,byrow=TRUE)%*%t(cbind(x,y)))[,2]
  #points(xTemp,yTemp,type="l",lwd=3)
  polygon(c(xTemp),c(yTemp),col=rgb(0,0,1,.1),bor=rgb(0,0,1,.1))
}
}

for (q in 1:2){
w<-runif(1,1,4)
h<-runif(1,1,4)
x<-c(3,w,w,h,3)
y<-c(3,h,w,w,3)
#x<-c(3,5,7,2,3)
#y<-c(3,2,7,5,3)
for (theta in th+seq(0,2*pi,length=7)[-1]){
  xTemp<-t(matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),ncol=2,byrow=TRUE)%*%t(cbind(x,y)))[,1]
  yTemp<-t(matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),ncol=2,byrow=TRUE)%*%t(cbind(x,y)))[,2]
  #points(xTemp,yTemp,type="l",lwd=3)
  polygon(c(xTemp),c(yTemp),col=rgb(0,0,1,.25),bor=rgb(0,0,1,.25))
}
#text(0,0,"@STATSINTHEWILD",cex=1,col=rgb(.5,.5,.5,.9))
}


}


dev.off()
