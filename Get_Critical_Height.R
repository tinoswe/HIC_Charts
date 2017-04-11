heights <- c(656.0, 
             923.0,
             1195.0,
             1451.0)
HICs <- c(300.2,
          676.4,
          1220.8,
          1911.5)

point_label <- "I"

df <- data.frame(h=heights,
                 hic=HICs)

outFile <- paste("229709_Grafici/Punto_", point_label, "_Altezza_Critica.PDF",
                 sep="")

pdf(outFile,8,6)


par(mgp=c(3,0.45,0), 
    tcl=-0.2, 
    mar=c(3.0,5.3,1.1,2.1))
plot(1e-3*df$h,
     df$hic,
     pch=19,
     col="black",
     cex=1.2,
     xlim=c(0.5,1.65),
     ylim=c(250,2200),
     xlab="h [m]",
     ylab="HIC",
     xaxs = "i",
     ann=FALSE,
     axes=FALSE,
     #title=plot_title,
     type="n")

#panel.first = c(abline(h=seq(0,2500,by=250), v=seq(0,3,by=.25), col="gray", lty=3)))
#title(paste("Posizione:", as.character(lvl)))
axis(side=1, 
     at=seq(0,3,by=0.25),
     lwd=1, 
     cex.axis=1.2)
axis(side=2, 
     at=seq(0, 2500, by=250),
     lwd=1, 
     cex.axis=1.2,
     las=1)
mtext(side=1,
      text="h [m]",
      line=1.7,
      cex=1.2)
mtext(side=2,
      text="HIC",
      line=3.3,
      cex=1.2)

hs <- 1e-3*df$h
hics <- df$hic
fit_1 <- lm(hics ~ poly(hs,2,raw=T),
            weights = 0.3*hics/100.)

if (coef(fit_1)[3] < 0) {
  fit_1 <- lm(hics ~ poly(hs,1,raw=T),
              weights = 0.3*hics/100.)
}

x <- seq(.5, 
         3,
         length.out = 250)
y <- predict(fit_1,
             data.frame(hs=x))
lines(x,
      y,
      col="grey")

points(1e-3*df$h,
       df$hic,
       pch=19,
       col="black")

#ok up to here

A <- as.numeric(fit_1$coefficients[1])
B <- as.numeric(fit_1$coefficients[2])
C <- as.numeric(fit_1$coefficients[3])
#curve(A + B*x + C*(x*x), 
#      1.75, 
#      2.5,
#      add=TRUE,
#      col="red")

s1 <- Re(polyroot(c(A-1000,
                    B,
                    C))[1])


yp <- predict(fit_1,
             data.frame(hs=s1))
points(s1,
       yp,
       pch=15,
       col="firebrick")
segments(s1,0,s1,yp,
         col="firebrick",
         lty=3,
         lwd=2)
segments(0,yp,s1,yp,
         col="firebrick",
         lty=3,
         lwd=2)
xtext <- s1
ytext <- yp-20
text(xtext,
     ytext,
     pos=4,
     paste("HIC=1000, h=", round(s1,3),"m",
           sep=""))


plot_title <- paste("Punto ",
                    point_label,
                    ": altezza critica=",
                    trunc(s1*10^1)/10^1,
                    "m",
                    sep="")
title(plot_title,
      line=-1)
dev.off()



