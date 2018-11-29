install.packages('rgl')

library(rgl)

data(volcano)
y <- 2 * volcano # Exaggerate the relief
x <- 10 * (1:nrow(y)) # 10 meter spacing (S to N)
z <- 10 * (1:ncol(y)) # 10 meter spacing (E to W)
ylim <- range(y)
ylen <- ylim[2] - ylim[1] + 1
colorlut <- terrain.colors(ylen) # height color lookup table
col <- colorlut[ y-ylim[1]+1 ] # assign colors to heights
rgl.open()
rgl.surface(x, z, y, color=col, back="lines") 
