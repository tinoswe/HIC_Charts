myFile <- file.choose()
fName <- tail(strsplit(myFile,"/")[[1]],n=1)
fPath <- sub(paste("/",fName,sep=""), "", myFile)


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

t_start <- 5.5 # this is for cutting first peak
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