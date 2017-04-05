myFile <- file.choose()
fName <- tail(strsplit(myFile,"/")[[1]],n=1)
fPath <- sub(paste("/",fName,sep=""), "", myFile)

#myFile
#fName
#fPath
HIC_tmin <- 5127.3*1e-3
HIC_tmax <- 5132.38*1e-3
HICmax <- 1188.8
HICinterval <- 1e3*(HIC_tmax - HIC_tmin)


fPos <- strsplit(fName,"_")[[1]][1]
fH <-  strsplit(strsplit(fName,"_")[[1]][2],"[.]")[[1]][1]

data <- read.delim(myFile, 
                   header = FALSE,
                   skip = 23)

t  <- data$V1 
g  <- data$V2 
t <- as.numeric(sub(",", ".", sub(".", "", t, fixed=TRUE), fixed=TRUE))
g <- as.numeric(sub(",", ".", sub(".", "", g, fixed=TRUE), fixed=TRUE))

df <- data.frame(t=t,
                 g=g)
plot(df$t,
     df$g,
     type="l")

t_start <- 4.5 # this is for cutting first peak
plot(df[t>t_start,]$t,
     df[t>t_start,]$g,
     type="l")

g_max <- max(df[t>t_start,]$g)
t_max <- df[t>t_start,]$t[which(df[t>t_start,]$g == g_max)]

t_impact_start <- t_max - 0.01 
t_impact_stop <- t_max + 0.011


plot(df[t>t_impact_start & t<t_impact_stop ,]$t,
     df[t>t_impact_start & t<t_impact_stop ,]$g,
     type="l")

#generate PDF
outFile <- paste(fPos, "_", fH, ".", "PDF",
                 sep="")

pdf(outFile,8,6)
p_title <- paste("Punto ",
                 fPos,
                 ", h=",
                 fH,
                 "mm",
                 sep="")

par(mgp=c(30,0.45,0), 
    tcl=-0.4, 
    mar=c(3.0,3.3,1.1,1.1))

plot(df[t>t_impact_start & t<t_impact_stop ,]$t,
     df[t>t_impact_start & t<t_impact_stop ,]$g,
     type="l",
     lwd=4,
     col="firebrick",
     ann=FALSE,
     cex=1.5,
     #xlim=c(min(df[t>t_impact_start & t<t_impact_stop ,]$t),
     #        max(df[t>t_impact_start & t<t_impact_stop ,]$t)),
     ylim=c(0,200),
     axes=FALSE,
     yaxs="i")
 
axis(1,
     pos=0,
     at=round(seq(min(df[t>t_impact_start & t<t_impact_stop ,]$t),
            max(df[t>t_impact_start & t<t_impact_stop ,]$t),
            by=0.005),3),
     lwd=1, 
     cex.axis=1.1,
     tck=-0.01)
axis(2,
    at=seq(0,
           250,
           by=25),
    lwd=1, 
    cex.axis=1.1,
    tck=-0.01,
    las=0)
mtext(side=1,
      text="Tempo [ms]",
      line=1.7,
      cex=1.2)
mtext(side=2,
      text="Accelerazione [g]",
      line=2,
      cex=1.2)

  
  
#abline(v=HIC_tmin, 
#       col="red",
#       lty=2,
#       lwd=2)
#abline(v=HIC_tmax, 
#       col="red",
#       lty=2,
#       lwd=2)

# par(new=TRUE)
# x_grid <- seq(from=5, to=20, by=5)
# y_grid <- seq(from=25, to=200, by=25)
# segments(x_grid, 0,
#          x_grid, 300,
#          col="lightgray",
#          lty=2)
# segments(0, y_grid, 
#          20, y_grid,
#          col="lightgray",
#          lty=2)
# 

# par(new=TRUE)
cord.x <- c(HIC_tmin, 
            #df_impact[df_impact$time > HIC_tmin & df_impact$time < HIC_tmax, ]$time,
            df[t > t_impact_start & t < t_impact_stop & t > HIC_tmin & t < HIC_tmax,]$t,
            HIC_tmax)
cord.y <- c(0,
            df[t > t_impact_start & t < t_impact_stop & t > HIC_tmin & t < HIC_tmax,]$g,
            0)
#col2rgb("salmon")
polycol<-rgb(red=250,
            green=128,
            blue=114,
            alpha=20,
            maxColorValue = 255)
polygon(cord.x,
        cord.y,
        col=polycol,
        border="firebrick")
# 
# par(new=TRUE)
# lines(df_impact$time,
#       df_impact$g,
#       #type="l",
#       lwd=4,
#       col="firebrick",
#       cex=1.5
#       #xlim=c(0,20)
# )

legend("topleft",
       horiz="FALSE",
       c(paste("HIC = ", 
               round(HICmax,1)), 
         paste("dt = ", 
               round(HICinterval,4), "ms"), 
         paste("max(a) = ", 
               round(max(df[t>t_impact_start & t<t_impact_stop ,]$g),1),"g")),
       cex=1.2,
       bty="n")
title(p_title)
dev.off()

