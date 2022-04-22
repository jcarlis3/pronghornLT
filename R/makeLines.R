



library(raster)
library(spatstat)
# library(spcosa)

        # # Solve for approx spacing (using bounding box)
        # bbox <- c(sf::st_bbox(sPoly))
        # ewSide <- abs(bbox[1]-bbox[3])
        # nsSide <- abs(bbox[2]-bbox[4])
        # 
        # sideLen <- nsSide #mean(c(ewSide, nsSide))
        # minLines <- ceiling(targlen/sideLen)
        # approxSpace <- sideLen/minLines


#-----Inputs
datDir <- "W:\\My Drive\\Projects\\Pronghorn_ltds"
sPoly <- sf::st_read(file.path(datDir, "Rattlesnake_HU.shp"))
xPoly <- sf::st_read(file.path(datDir, "Rattlesnake_toRemove.shp"))
totalLengthKm <- 1000
bearing <- 90
outFile <- file.path(datDir, "test_out.shp")

AA <- sPoly
space <- 25*1000
#edge <- 20
targLen <- totalLengthKm*1000
orien <- bearing

evalOffset <- 0
evalMinLength <- 2000


#-------------------------------------------------------------------------------
#-----Functions

# Define a function to generate lines (modified from spatstat.geom::rlinegrid())
genLines <- function (angle = 90, spacing = 0.1, win = owin(), offset, minLength) {
  # TODO validate that offset is less than spacing
  win <- as.owin(win)
  width <- diff(win$xrange)
  height <- diff(win$yrange)
  rmax <- sqrt(width^2 + height^2)/2
  xmid <- mean(win$xrange)
  ymid <- mean(win$yrange)
  # u <- runif(1, min = 0, max = spacing) - rmax
  u <- offset - rmax
  if (u >= rmax) 
    return(psp(numeric(0), numeric(0), numeric(0), numeric(0), 
               window = win, check = FALSE))
  p <- seq(from = u, to = rmax, by = spacing)
  q <- sqrt(rmax^2 - p^2)
  theta <- pi * ((angle - 90)/180)
  co <- cos(theta)
  si <- sin(theta)
  X <- psp(x0 = xmid + p * co + q * si, y0 = ymid + p * si - 
             q * co, x1 = xmid + p * co - q * si, y1 = ymid + p * 
             si + q * co, window = owin(xmid + c(-1, 1) * rmax, ymid + 
                                          c(-1, 1) * rmax), check = FALSE)
  X <- X[win]$ends
  xlengths <- eval(expression(sqrt((x1 - x0)^2 + (y1 - y0)^2)), envir = X)
  subX <- X[xlengths >= minLength,]
  return(subX)
}


evalLines <- function(spacing, angle, win, targLen, offset, minLength){
    tlines <- genLines(
      angle = orien,
      spacing = spacing,
      win = aawin,
      offset = offset,
      minLength = minLength
    )
    tlengths <- eval(expression(sqrt((x1 - x0)^2 + (y1 - y0)^2)), envir = tlines)
    totLen <- sum(tlengths)
    err <- abs(targlen-totLen)
  print(paste0(spacing, ":", err))
  return(err)
}


#-------------------------------------------------------------------------------
#-----Processes

optimSpace <- optimize(f = evalLines,
               # interval = c(500, 50000),
               lower = 500,
               upper = 50000,
               # tol = 0.1,
               angle = orien,
               win = aawin,
               minLength = evalMinLength,
               targLen = targLen,
               offset = 3000)


keepLines <- genLines(
  angle = orien,
  spacing = optimSpace$minimum,
  win = aawin,
  offset = evalOffset,
  minLength = evalMinLength)


tlist <- list()
for(k in 1:nrow(keepLines)){
  t2 <- cbind(c(keepLines[k,1],keepLines[k,3]),c(keepLines[k,2],keepLines[k,4]))
  t3 <- Line(t2)
  t4 <- Lines(t3, ID = k)
  tlist <- append(tlist,t4)
}
jlines <- SpatialLines(tlist)
crs(jlines) <- AA@proj4string

nlines <- length(jlines)
llen <- sapply(1:nlines, function(x) LineLength(jlines[x]@lines[[1]]@Lines[[1]]@coords))
print(nlines)
print(sum(llen))
plot(AA)
plot(jlines, add =T)

mapview(sPoly) + mapview(jlines)


