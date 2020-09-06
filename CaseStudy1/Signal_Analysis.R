

# t = "Time stamp (Milliseconds) since 12:00am, January 1, 1970"
# Id = router MAC address
# Pos = router location
# Degree = Direction scanning device measured in degrees that was carried by the researcher
# MAC = MAC address of either the router, or scanning device combined with corresponding values for signal strength (dBm), the mode in which it was operating(adhoc scanner = 1, access router = 3), and its corresponding channel frequency.
# Signal = Received Signal Strength in DbM

# read in the data as raw
data <- readLines("F:/SMU/DS7333/Project1/offline.final.trace.txt")

# Identify comments - they start with a # sign - 5,312
sum(substr(data, 1, 1) == "#")

# How many lines are in the file? - 151,392
length(data)

# Remove comments and use ;= as the delimiter
tokens = strsplit(data[4], "[;=,]")[[1]]


# Aiming to include one location per line, so further manipulating the file
tmp = matrix(tokens[ - (1:10) ], ncol = 4, byrow = TRUE)
mat = cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow = nrow(tmp), ncol = 6, byrow = TRUE), tmp)

# Build the above into a single function
processLine = function(x)
{
  tokens = strsplit(x, "[;=,]")[[1]]
  if (length(tokens) == 10)
    return(NULL)
  
  tmp = matrix(tokens[ - (1:10)], ncol = 4, byrow = TRUE)
  cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow = nrow(tmp),ncol = 6, byrow = TRUE), tmp)
}

# Run the processline function across the entire data set - we are left with 10 columns and 1,181,628 rows after transformation
lines = data[ substr(data, 1, 1) != "#" ]
tmp = lapply(lines, processLine)
offline = as.data.frame(do.call("rbind", tmp),
                        stringsAsFactors = FALSE)
dim(offline)

# Assign column names
names(offline) = c("time", "scanMac", "posX", "posY", "posZ", "orientation", "mac", "signal", "channel", "type")

# Convert the numeric values to numerics
numVars = c("time", "posX", "posY", "posZ",
            "orientation", "signal")
offline[ numVars ] = lapply(offline[ numVars ], as.numeric)

# Remove non-access point devices (since 1= adhoc and 3 = access point) - 978,443 rows and 9 rows
offline = offline[ offline$type == "3", ]
offline = offline[ , "type" != names(offline) ]
dim(offline)

# Convert time to a workable format
offline$rawTime = offline$time
offline$time = offline$time/1000
class(offline$time) = c("POSIXt", "POSIXct")

# Review data summary
summary(offline[, numVars])

# Further cleaning the data, posZ and ScanMac are all 0 values for all rows - remove
offline = offline[ , !(names(offline) %in% c("scanMac", "posZ"))]

# Plot the values from the orientation field
plot(ecdf(offline$orientation))

# Round the orientation so they are more inline with the intended analysis
roundOrientation = function(angles) {
  refs = seq(0, by = 45, length = 9)
  q = sapply(angles, function(o) which.min(abs(o - refs)))
  c(refs[1:8], 0)[q]
}
offline$angle = roundOrientation(offline$orientation)

# Plot angles
with(offline, boxplot(orientation ~ angle,
                      xlab = "nearest 45 degree angle",
                      ylab = "orientation"))

# Count number of unique MAC and channels - row counts
c(length(unique(offline$mac)), length(unique(offline$channel)))
table(offline$mac)

# Limiting the number of access points down to 6 because these were all on the same floor, after running EDA
subMacs = c('00:0f:a3:39:e1:c0', '00:14:bf:b1:97:90', '00:14:bf:3b:c7:c6',
            '00:14:bf:b1:97:81', '00:14:bf:b1:97:8a', '00:14:bf:b1:97:8d')
subMacs2 = c('00:0f:a3:39:dd:cd', '00:14:bf:b1:97:90', '00:14:bf:3b:c7:c6',
             '00:14:bf:b1:97:81', '00:14:bf:b1:97:8a', '00:14:bf:b1:97:8d')
subMacsall = names(sort(table(offline$mac), decreasing = TRUE))[1:7]

# Review the number of x,y variables - and returns total possible combinations
offline = offline[ offline$mac %in% subMacsall, ]
locDF = with(offline,
             by(offline, list(posX, posY), function(x) x))
locDF = locDF[ !sapply(locDF, is.null) ]
locCounts = sapply(locDF,
                   function(df)
                     c(df[1, c("posX", "posY")], count = nrow(df)))
dim(locCounts)
locCounts[ , 1:8]

# This takes all of the above and makes a function that combines all of it
processLine = function(x)
{
  tokens = strsplit(x, "[;=,]")[[1]]
  if (length(tokens) == 10)
    return(NULL)
  tmp = matrix(tokens[ - (1:10) ], ncol= 4, byrow = TRUE)
  cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow(tmp), 6,
               byrow = TRUE), tmp)
}


roundOrientation = function(angles) {
  refs = seq(0, by = 45, length  = 9)
  q = sapply(angles, function(o) which.min(abs(o - refs)))
  c(refs[1:8], 0)[q]
}

readData =
  function(filename, subMacs )
  {
    txt = readLines(filename)
    lines = txt[ substr(txt, 1, 1) != "#" ]
    tmp = lapply(lines, processLine)
    offline = as.data.frame(do.call("rbind", tmp),
                            stringsAsFactors= FALSE)
    names(offline) = c("time", "scanMac",
                       "posX", "posY", "posZ", "orientation",
                       "mac", "signal", "channel", "type")
    # keep only signals from access points
    offline = offline[ offline$type == "3", ]
    # drop scanMac, posZ, channel, and type - no info in them
    dropVars = c("scanMac", "posZ", "channel", "type")
    offline = offline[ , !( names(offline) %in% dropVars ) ]
    # drop more unwanted access points
    offline = offline[ offline$mac %in% subMacs, ]
    # convert numeric values
    numVars = c("time", "posX", "posY", "orientation", "signal")
    offline[ numVars ] = lapply(offline[ numVars ], as.numeric)
    # convert time to POSIX
    offline$rawTime = offline$time
    offline$time = offline$time/1000
    class(offline$time) = c("POSIXt", "POSIXct")
    # round orientations to nearest 45
    offline$angle = roundOrientation(offline$orientation)
    return(offline)
  }

# Leverage the combined function to create the various recordsets based on the OFFLINE MAC ID selections named before
offline <- readData(filename = "F:/SMU/DS7333/Project1/offline.final.trace.txt" , subMacs=subMacs)  # With C0 without CD
offline2 <- readData(filename = "F:/SMU/DS7333/Project1/offline.final.trace.txt" , subMacs=subMacs2)  # With CD without C0
offlineall <- readData(filename = "F:/SMU/DS7333/Project1/offline.final.trace.txt" , subMacs=subMacsall)  # With both CD and C0
offline$posXY = paste(offline$posX, offline$posY, sep = "-")

# Leverage the combined function to create the various ONLINE recordsets based on the MACID selections named before
online <- readData(filename = "F:/SMU/DS7333/Project1/online.final.trace.txt", subMacs=subMacs)  # With C0 without CD
online2 <- readData(filename = "F:/SMU/DS7333/Project1/online.final.trace.txt", subMacs=subMacs2)  # With CD without C0
onlineall <- readData(filename = "F:/SMU/DS7333/Project1/online.final.trace.txt" , subMacs=subMacsall)  # With both CD and C0
online$posXY = paste(online$posX, online$posY, sep = "-")
online2$posXY = paste

#### Signal Strength Analysis
Using the above cleaned data, we want to explore the relationship between signal strength and distance from observation to access point. We want to answer questions like: Is the signal strength consistent across distances? Is it significantly impacted by the other variables we collected?
  
  The boxplot below shows the variance of the signal strength with multiple combinations of position and orientation. We also output a density plot to observe the variance of these combinations.  As expected, there is a mixture of normal and skewed distributions.

The boxplots in this figure represent signals for one location, which is in the upper left corner of the floor plan, i.e., x = 2 and y = 12. These boxes are organized by access point and the angle of the hand-held device. The dependence of signal strength on angle is evident at several of the access points, e.g., 00:14:bf:97:90 in the final panel of the figure.

```{r BoxPlots, eval=TRUE, echo=FALSE}
library(lattice)
bwplot(signal ~ factor(angle) | mac, data = offline,
       subset = posX == 2 & posY == 12,
       layout = c(2,3))
```

The density curves shown here are density plots for signal strength which represent each of the access point x, y angle combinations. We wanted to see the impact of orientation on signal strength at various points on the map with all the routers involved. We initially didn't think that orientation would play a major role, but further analysis proved otherwise.

```{r DensityPlots, eval=TRUE, echo=FALSE}
macs = unique(offline$mac)
orientations = unique(offline$angle)
library("RColorBrewer")
colors = brewer.pal(name = "Blues", n = 9)
par(mfrow = c(2, 3))
for (i in 1:6) {
  routerData = offline[offline$mac == macs[i],]
  X = 23
  Y = 7
  
  positionData = routerData[routerData$posX == X & routerData$posY == Y,]
  first = TRUE
  for (k in 1:8) {
    signalAtOrientation = positionData[positionData$angle == orientations[k], c("signal")]
    if (length(signalAtOrientation) <= 2) {
      next
    }
    if (first) {
      plot(density(signalAtOrientation), ylim = c(0, 0.6), xlim = c(-90, -30), 
           col = colors[k], main = paste("(", X, ", ", Y, "): ", macs[i], sep = ""),
           xlab = "Signal Strength (dB)")
      first = FALSE
    }
    else {
      lines(density(signalAtOrientation), col = colors[k])
    }
  }
  lines(density(positionData[,c("signal")]), col = "red")
}
legend(legend = c(orientations, "Combined"), x = -90, y = 0.5, col = c(colors, "red"), lty = c(1, 1), lwd = c(2.5, 2.5))
```

This plot shows the density curves for various orientations at the point (23, 7) near the center of the map for all six routers. We can clearly see that changing orientation can make up to a 10dB difference or so in the peaks of the density curves. This means that our distance formula would have to take into account, not only signal strength differences, but also orientation differences.

Next, we want take a closer look at summary statistics broken down by each router location. First, we will create a couple of new variables that contain all combinations of the x and y coordinates of the scanning device and another variable that contains every combination of posXY, angle, and access point MAC address. Then we calcultate summary statistics for each of those.

```{r SummaryPerRouter, eval=TRUE, echo=FALSE}
offline$posXY = paste(offline$posX, offline$posY, sep = "-")
byLocAngleAP = with(offline,
                    by(offline, list(posXY, angle, mac),
                       function(x) x))
signalSummary = 
  lapply(byLocAngleAP, 
         function(oneLoc) { 
           ans = oneLoc[1,] 
           ans$medSignal = median(oneLoc$signal) 
           ans$avgSignal = mean(oneLoc$signal) 
           ans$num = length(oneLoc$signal)
           ans$sdSignal = sd(oneLoc$signal)
           ans$iqrSignal = IQR(oneLoc$signal)
           ans
         })
offlineSummary = do.call("rbind", signalSummary)
offline2$posXY = paste(offline2$posX, offline2$posY, sep = "-")
byLocAngleAP2 = with(offline2,
                     by(offline2, list(posXY, angle, mac),
                        function(x) x))
signalSummary2 = 
  lapply(byLocAngleAP2, 
         function(oneLoc) { 
           ans = oneLoc[1,] 
           ans$medSignal = median(oneLoc$signal) 
           ans$avgSignal = mean(oneLoc$signal) 
           ans$num = length(oneLoc$signal)
           ans$sdSignal = sd(oneLoc$signal)
           ans$iqrSignal = IQR(oneLoc$signal)
           ans
         })
offlineSummary2 = do.call("rbind", signalSummary2)
offlineall$posXY = paste(offlineall$posX, offlineall$posY, sep = "-")
byLocAngleAPall = with(offlineall,
                       by(offlineall, list(posXY, angle, mac),
                          function(x) x))
signalSummaryall = 
  lapply(byLocAngleAPall, 
         function(oneLoc) { 
           ans = oneLoc[1,] 
           ans$medSignal = median(oneLoc$signal) 
           ans$avgSignal = mean(oneLoc$signal) 
           ans$num = length(oneLoc$signal)
           ans$sdSignal = sd(oneLoc$signal)
           ans$iqrSignal = IQR(oneLoc$signal)
           ans
         })
offlineSummaryall = do.call("rbind", signalSummaryall)
```

We want to look at a box plot which compares how standard deviation of signal strength varies with mean signal strength for each location/angle/access point combination.

```{r BoxPlots2, eval=TRUE, echo=FALSE}
## Box plots for mean signal strength
breaks = seq(-90, -30, by = 5)
bwplot(sdSignal ~ cut(avgSignal, breaks = breaks),
       data = offlineSummaryall,
       xlab = "Mean Signal", ylab = "SD Signal")
```

A closer look at the variance between signal strength and position show something interesting, which is that strong signals have a higher variance while weaker signals have a lower variance.

```{r SignalStrength, eval=TRUE, echo=FALSE}
with(offlineSummaryall,
     smoothScatter((avgSignal - medSignal) ~ num,
                   xlab = "Number of Observations", 
                   ylab = "mean - median"))
abline(h = 0, col = "#984ea3", lwd = 2)
lo.obj = 
  with(offlineSummaryall,
       loess(diff ~ num, 
             data = data.frame(diff = (avgSignal - medSignal),
                               num = num)))
lo.obj.pr = predict(lo.obj, newdata = data.frame(num = (70:120)))
lines(x = 70:120, y = lo.obj.pr, col = "#4daf4a", lwd = 2)
```

Returning to the question of the two MAC addresses that seem to be from the same access point, we can now create a heatmap representing signal strength for the access points in question to compare with the building floor plan in Figure 1. In the figure below, each row represents an access point and indeed, both appear to be located at the same position. There is a notable difference however. The top row, representing MAC address 00:0f:a3:39:e1:c0, shows a slightly stringer signal as well as a stronger corridor effect, implying that wall had a stronger effect on this action point than the bottom row. These differences support the hypothesis that the bottom row, representing MAC address 00:0f:a3:39:dd:cd, may be on a different floor.

```{r ComparingAP, eval=TRUE, echo=FALSE}
surfaceSS = function(data, mac, angle = 45) {
  require(fields)
  oneAPAngle = data[ data$mac == mac & data$angle == angle, ]
  smoothSS = Tps(oneAPAngle[, c("posX","posY")], 
                 oneAPAngle$avgSignal)
  vizSmooth = predictSurface(smoothSS)
  plot.surface(vizSmooth, type = "C", 
               xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  points(oneAPAngle$posX, oneAPAngle$posY, pch=19, cex = 0.5) 
}
```
```{r Heatmap, eval=TRUE, echo=FALSE}
parCur = par(mfrow = c(2,2), mar = rep(1, 4))
mapply(surfaceSS, mac = subMacsall[ rep(c(1, 2), each = 2) ], 
       angle = rep(c(0, 135), 2),
       data = list(data = offlineSummaryall))

par(parCur)
```


## K-Nearest Neighbors Analysis
We will now use K-Nearest Neighbors to determine location from access point signal strength. K-NN works by measuring the distance between a new point and it's neighboring points. The k closest neighboring points to the newest point are used to vote on the value for the new point.

The first step we are going to do is to prepare test data. The online data has duplicate observations for time, position, mac, and angle. Similar to the training data set above, the signal strengths will need to be averaged for situations where these tuples of data occur, resulting in a single series of signal data for a given position, MAC address, and angle.

```{r DeDupe, eval=TRUE, echo=FALSE}
summary(online)
tabonlineXYA = table(online$posXY, online$angle)
tabonlineXYA[1:6, ]
```

It appears that signal strengths were only recorded at one angle for each test location in this data. To account for this, we will reorganize the data to contain a column for each access point containing the average signal strength. This step will make it easier to compute distance between the six signal strength vectors. By doing this, we provide the average signal strength at each location as summary.  Notice 12 columns are present in our summary, including the concatenated X-Y values, X and Y values in their separate columns, orientation, angle, and the seven access points. 

```{r CombineSingalStrength, eval=TRUE, echo=FALSE}
keepVars = c("posXY", "posX","posY", "orientation", "angle") 
byLoc = with(online, 
             by(online, list(posXY), 
                function(x) { 
                  ans = x[1, keepVars] 
                  avgSS = tapply(x$signal, x$mac, mean) 
                  y = matrix(avgSS, nrow = 1, ncol = 6, 
                             dimnames = list(ans$posXY, names(avgSS))) 
                  cbind(ans, y) 
                }))
onlineSummary = do.call("rbind", byLoc)
head(onlineSummary)
```
```{r CombineSingalStrength2, eval=TRUE, echo=FALSE}
byLoc = with(online2, 
             by(online2, list(posXY), 
                function(x) { 
                  ans = x[1, keepVars] 
                  avgSS = tapply(x$signal, x$mac, mean) 
                  y = matrix(avgSS, nrow = 1, ncol = 6, 
                             dimnames = list(ans$posXY, names(avgSS))) 
                  cbind(ans, y) 
                }))
onlineSummary2 = do.call("rbind", byLoc)
```
```{r CombineSingalStrengthall, eval=TRUE, echo=FALSE}
byLoc = with(online_allmacs, 
             by(online_allmacs, list(posXY), 
                function(x) { 
                  ans = x[1, keepVars] 
                  avgSS = tapply(x$signal, x$mac, mean) 
                  y = matrix(avgSS, nrow = 1, ncol = 7, 
                             dimnames = list(ans$posXY, names(avgSS))) 
                  cbind(ans, y) 
                }))
onlineSummaryall = do.call("rbind", byLoc)
```

We discovered in our analysis that orientation does affect signal strength. Our objective is to find offline data points that share similar orientations to our new location points. Since all observations were recorded in 45 degree increments, this becomes as easy as specifying the number of neighboring angles to include from the offline dataset. For even numbers, this means selecting even multiples of 45 degrees on each side of a test observation's orientation angle. For odd numbers, it means selecting offline data with angles that match the new observation's rounded orientation as well as those that flank the new observation's angle. In the case where only one orientation is desired, offline data with angles matching the new observations will be selected only. Below we write a function that creates data structure aggregating the values from these angles.

```{r reShapeSS, eval=TRUE, echo=FALSE}
AP = matrix( c( 7.5, 6.3, 2.5, -.8, 12.8, -2.8,  
               1, 14, 33.5, 9.3,  33.5, 2.8),
            ncol = 2, byrow = TRUE,
            dimnames = list(subMacs, c("x", "y") ))
reshapeSS = function(data, varSignal = "signal", 
                     keepVars = c("posXY", "posX","posY")) {
  byLocation =
    with(data, by(data, list(posXY), 
                  function(x) {
                    ans = x[1, keepVars]
                    avgSS = tapply(x[ , varSignal ], x$mac, mean)
                    y = matrix(avgSS, nrow = 1, ncol = 6,
                               dimnames = list(ans$posXY,
                                               names(avgSS)))
                    cbind(ans, y)
                  }))
  newDataSS = do.call("rbind", byLocation)
  return(newDataSS)
}
```

```{r selectTrain, eval=TRUE, echo=FALSE}
selectTrain = function(angleNewObs, signals = NULL, m = 1){
  # m is the number of angles to keep between 1 and 5
  refs = seq(0, by = 45, length  = 8)
  nearestAngle = roundOrientation(angleNewObs)
  
  if (m %% 2 == 1) 
    angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
  else {
    m = m + 1
    angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
    if (sign(angleNewObs - nearestAngle) > -1) 
      angles = angles[ -1 ]
    else 
      angles = angles[ -m ]
  }
  angles = angles + nearestAngle
  angles[angles < 0] = angles[ angles < 0 ] + 360
  angles[angles > 360] = angles[ angles > 360 ] - 360
  angles = sort(angles)
  
  offlineSubset = signals[ signals$angle %in% angles, ]
  reshapeSS(offlineSubset, varSignal = "avgSignal")
}
```

Next, we want to create a function that will find the nearest neighbor to a given point. We name this function as findNN.

```{r FindNeighbor, eval=TRUE, echo=FALSE}
findNN = function(newSignal, trainSubset) { 
  diffs = apply(trainSubset[, 4:9], 1, 
                function(x) x - newSignal) 
  dists = apply(diffs, 2, function(x) sqrt(sum(x^2))) 
  closest = order(dists) 
  return(trainSubset[closest, 1:3]) 
}
```

We can now use the selectTrain() and findNN() functions and embed into a wrapper function predXY() wrapper function in the code below which will use KNN to predict location. 

```{r KNN, eval=TRUE, echo=FALSE}
predXY = function(newSignals, newAngles, trainData,
                  numAngles = 1, k = 3){
  closeXY = list(length = nrow(newSignals))
  
  for (i in 1:nrow(newSignals)) {
    trainSS = selectTrain(newAngles[i], trainData, m = numAngles)
    closeXY[[i]] = findNN(newSignal = as.numeric(newSignals[i, ]), trainSS)
  }
  
  estXY = lapply(closeXY,
                 function(x) sapply(x[ ,2:3],
                                    function(x) mean(x[1:k])))
  estXY = do.call("rbind", estXY)
  return(estXY)
}
```

We can test this function using three angles and both k=3 nearest neighbors and k=1 nearest neighbor. Furthermore, model fit is assessed by mapping actual and predicted locations with lines connecting the two points for each respective new observation. This map is first drawn for the 3-NN predictions below.

```{r ThreeNNFit, eval=TRUE, echo=FALSE}
estXYk3 = predXY(newSignals = onlineSummary[ , 6:11], 
                 newAngles = onlineSummary[ , 4], 
                 offlineSummary, numAngles = 3, k = 3)
estXYk1 = predXY(newSignals = onlineSummary[ , 6:11], 
                 newAngles = onlineSummary[ , 4], 
                 offlineSummary, numAngles = 3, k = 1)
```

```{r ThreeNNFit, eval=TRUE, echo=FALSE}
estXYk3_2 = predXY(newSignals = onlineSummary2[ , 6:11], 
                 newAngles = onlineSummary2[ , 4], 
                 offlineSummary2, numAngles = 3, k = 3)
estXYk1_2 = predXY(newSignals = onlineSummary2[ , 6:11], 
                 newAngles = onlineSummary[ , 4], 
                 offlineSummary2, numAngles = 3, k = 1)
```

```{r FloorErrorMap, eval = TRUE, echo = False}
floorErrorMap = function(estXY, actualXY, trainPoints = NULL, AP = NULL){
  
    plot(0, 0, xlim = c(0, 35), ylim = c(-3, 15), type = "n",
         xlab = "", ylab = "", axes = FALSE)
    box()
    if ( !is.null(AP) ) points(AP, pch = 15)
    if ( !is.null(trainPoints) )
      points(trainPoints, pch = 19, col="grey", cex = 0.6)
    
    points(x = actualXY[, 1], y = actualXY[, 2], 
           pch = 19, cex = 0.8 )
    points(x = estXY[, 1], y = estXY[, 2], 
           pch = 8, cex = 0.8 )
    segments(x0 = estXY[, 1], y0 = estXY[, 2],
             x1 = actualXY[, 1], y1 = actualXY[ , 2],
             lwd = 2, col = "red")
}
```
```{r PlotFloorError, eval = TRUE, echo = FALSE}
trainPoints = offlineSummary[ offlineSummary$angle == 0 & 
                              offlineSummary$mac == "00:0f:a3:39:e1:c0" ,
                        c("posX", "posY")]
oldPar = par(mar = c(1, 1, 1, 1))
```

Furthermore, model fit is assessed by mapping actual and predicted locations with lines connecting the two points for each respective new observation. This map is first drawn for the 3-NN predictions below followed by the 1-NN predictions.

```{r ThreeNNMap, eval=TRUE, echo=FALSE}
floorErrorMap(estXYk3, onlineSummary[ , c("posX","posY")], 
              trainPoints = trainPoints, AP = AP)
floorErrorMap(estXYk1, onlineSummary[ , c("posX","posY")], 
              trainPoints = trainPoints, AP = AP)
```

Now we can calculate the error for each of these test sets and compare them to see if using one or three neighbors produces more accurate results. The first pair of results uses the first of the two questionable MAC addresses and the second pair uses the second.

```{r CalcError, eval=TRUE, echo=FALSE}
calcError =
  function(estXY, actualXY)
    sum(rowSums((estXY - actualXY)^2))
actualXY = onlineSummary[ , c("posX", "posY")]
sapply(list(estXYk1, estXYk3), calcError, actualXY)
```

```{r CalcError2, eval=TRUE, echo=FALSE}
calcError =
  function(estXY_2, actualXY_2)
    sum(rowSums((estXY_2 - actualXY_2)^2))
actualXY_2 = onlineSummary2[ , c("posX", "posY")]
sapply(list(estXYk1_2, estXYk3_2), calcError, actualXY_2)
```

We see that k=3 nearest neighbors produces a lower error rate than using k=1 nearest neighbor for both sets of access points.
In order to determine the optimal value for k and avoid overfitting, we will use v-fold cross validation. A v value of 11 is selected since we have 166 different offline locations, allocating 15 locations to each fold.

```{r VCrossFold, eval=TRUE, echo=FALSE, warning = FALSE}
v = 11
permuteLocs = sample(unique(offlineSummary$posXY))
permuteLocs = matrix(permuteLocs, ncol = v, 
                     nrow = floor(length(permuteLocs)/v))
```


```{r head, eval=TRUE, echo=FALSE}
onlineFold = subset(offlineSummary, posXY %in% permuteLocs[ , 1])
head(onlineFold)
onlineFold.ul <- nrow(unique(onlineFold[,2:3]))
```

Previously we structured and summarized the offline data into six signal strength columns, one for each access point.  We will do the same with our cross-validated test data. However, because it is easier to structure the test data in its complete form from offline data which is then divided into our desired folds, we now need to modify the reshapeSS() function as follows.

```{r RestructureData, eval=TRUE, echo=FALSE}
seed = 101
reshapeSS = function(data, varSignal = "signal", 
                     keepVars = c("posXY", "posX","posY"),
                     sampleAngle = FALSE, 
                     refs = seq(0, 315, by = 45)) {
    
  set.seed(seed = seed)
    
  byLocation =
    with(data, by(data, list(posXY), 
                  function(x) {
                    if (sampleAngle) { # conditional statement added for cross-validation
                      x = x[x$angle == sample(refs, size = 1), ]}
                    ans = x[1, keepVars]
                    avgSS = tapply(x[ , varSignal ], x$mac, mean)
                    y = matrix(avgSS, nrow = 1, ncol = 6,
                               dimnames = list(ans$posXY,
                                               names(avgSS)))
                    cbind(ans, y)
                  }))
  newDataSS = do.call("rbind", byLocation)
  return(newDataSS)
  
keepVars = c("posXY", "posX","posY", "orientation", "angle")
}
```


```{r CVSummary, eval=TRUE, echo=FALSE}
onlineCVSummary = reshapeSS(offline, keepVars = keepVars,sampleAngle = TRUE)
onlineCVSummary2 = reshapeSS(offline2, keepVars = keepVars,sampleAngle = TRUE)
```

With all the above functions in place now we are ready to find the appropriate k based on our data. We will step through different values for k to find the optimal number of nearest neighbors. We will test values between one and twenty and calculate the error rate for each of them.

```{r TestKValues, eval=TRUE, echo=FALSE}
K = 20
err = rep(0, K)
for (j in 1:v) {
  onlineFold = subset(onlineCVSummary, 
                      posXY %in% permuteLocs[ , j])
  offlineFold = subset(offlineSummary,
                       posXY %in% permuteLocs[ , -j])
  actualFold = onlineFold[ , c("posX", "posY")]
  for (k in 1:K) {
    estFold = predXY(newSignals = onlineFold[ , 6:11],
                     newAngles = onlineFold[ , 4], 
                     offlineFold, numAngles = 3, k = k)
    err[k] = err[k] + calcError(estFold, actualFold)
  }
}
```

The below figure plots the sum of fold error values with respect to each k count.

```{r PlotFoldError, eval=TRUE, echo=FALSE}
plot(y = err, x = (1:K),  type = "l", lwd= 2,
     ylim = c(1200, 2100),
     xlab = "Number of Neighbors",
     ylab = "Sum of Square Errors")
rmseMin = min(err)
kMin = which(err == rmseMin)[1]
segments(x0 = 0, x1 = kMin, y0 = rmseMin, col = gray(0.4), 
         lty = 2, lwd = 2)
segments(x0 = kMin, x1 = kMin, y0 = 1100,  y1 = rmseMin, 
         col = grey(0.4), lty = 2, lwd = 2)
mtext(kMin, side = 1, line = 1, at = kMin, col = grey(0.4))
text(x = kMin - 2, y = rmseMin + 40, 
     label = as.character(round(rmseMin)), col = grey(0.4))
```

Based on our analysis, k=6 produces the least amount of error. With our optimal k=5 defined, we may proceed with estimation to calculate final Sum of Squared Errors.

```{r CalcSSE, eval=TRUE, echo=FALSE}
estXYk5 = predXY(newSignals = onlineSummary[ , 6:11], 
                 newAngles = onlineSummary[ , 4], 
                 offlineSummary, numAngles = 3, k = kMin)
SSE.k5 <- calcError(estXYk5, actualXY)
SSE.k5
estXYk5_2 = predXY(newSignals = onlineSummary2[ , 6:11], 
                 newAngles = onlineSummary2[ , 4], 
                 offlineSummary2, numAngles = 3, k = kMin)
SSE.k5_2 <- calcError(estXYk5_2, actualXY_2)
SSE.k5_2
```

Based on these results, with k=5, the error when including 00:0f:a3:39:e1:c0 is `r SSE.k5` and when instead choosing 00:0f:a3:39:dd:cd it is `r SSE.k5_2`. These values are both less than our previous reviews of k=1 and K=3 where errors were `r SSW.k3`, `r SSW.k3_2` and `r SSE.k1`, `r SSW.k1_2` respectively. As expected, choosing an optimized value for k results in the most accurate model. It is with k=5 and choosing the access point at MAC address 00:0f:a3:39:dd:cd that error is minimized at `r SSE.k5_2` and that we will use to compare against our weighted distance KNN model.
#### KNN Distance Weighting

To further improve the accuracy of predicting location, we up-weight stronger signals relative to weaker signals. This allows us to give strong, closer signals a larger impact on determining the location than those that are weak and further away. We use the weighting fraction below to achieve this result, where we use the signal strength as an estimator for distance.

$$\frac{1/d}{\sum_{i=1}^k 1/d_i}$$

The findNN() function below has been modified from the previous version to also output the distance metrics utilized in determining the "closest" k points. Additionally, the predXY() function has been modified to compute weights with the formula above and distance outputs from the findNN() output. These weights are multiplied against each k nearest observation, respectively, then summed to compute a weighted estimation by distance. These new calculations may allow locations of closer distances to be more impactful than those further away, instead of equal weighting amongst the k points.

```{r CombineFunctions4, eval=TRUE, echo=FALSE}
#Modify findNN to also output the distances
findNN = function(newSignal, trainSubset) {
  diffs = apply(trainSubset[ , 4:9], 1, 
                function(x) x - newSignal)
  dists = apply(diffs, 2, function(x) sqrt(sum(x^2)) )
  closest = order(dists)
  return(list(trainSubset[closest, 1:3 ],dists[order(dists)]))
}
#Modify findNN to utilize distance output for estimation efforts
predXY = function(newSignals, newAngles, trainData, 
                  numAngles = 1, k = 3){
  
  closeXY = list(length = nrow(newSignals))
  closeDist = list(length = nrow(newSignals))
  
  for (i in 1:nrow(newSignals)) {
    trainSS = selectTrain(newAngles[i], trainData, m = numAngles)
    fnnResult =
    findNN(newSignal = as.numeric(newSignals[i, ]), trainSS)
  
    closeXY[[i]] = fnnResult[[1]]
    closeDist[[i]] = fnnResult[[2]]
  }
  
  
  distWeight = list(length = length(closeDist))
  
  for (i in 1:length(closeDist)){
    distW = list(length = k)
    for (j in 1:k){
      distW[j] = (1/closeDist[[i]][j])/sum(1/closeDist[[i]][1:k])
    }
     
    distWeight[[i]] =  distW
  }
  estXYDetails = list(length=length(closeXY))
  
  for(i in 1:length(closeXY)){
    estXYDetails[[i]] = as.matrix(closeXY[[i]][1:k,2:3]) * unlist(distWeight[[i]])
  }
  
  estXY = lapply(estXYDetails,
                 function(x) apply(x, 2,
                                   function(x) sum(x)))
  
  estXY = do.call("rbind", estXY)
  return(estXY)
}
```

```{r RunFunctions4, eval=TRUE, echo=FALSE}
set.seed(seed = seed)
K = 20
err = rep(0, K)
for (j in 1:v) {
  onlineFold = subset(onlineCVSummary2, 
                      posXY %in% permuteLocs[ , j])
  offlineFold = subset(offlineSummary2,
                       posXY %in% permuteLocs[ , -j])
  actualFold = onlineFold[ , c("posX", "posY")]
  for (k in 1:K) {
    estFold = predXY(newSignals = onlineFold[ , 6:11],
                     newAngles = onlineFold[ , 4], 
                     offlineFold, numAngles = 3, k = k)
    err[k] = err[k] + calcError(estFold, actualFold)
  }
}
```

We will perform similar steps and perform v-fold tests on one to twenty k nearest neighbor estimations to find the optimal value of K with weighted samples.

```{r VFoldTests, eval=TRUE, echo=FALSE}
set.seed(seed = seed)
oldPar = par(mar = c(4, 5, 1, 1))
plot(y = err, x = (1:K),  type = "l", lwd= 2,
     ylim = c(1000, 2100),
     xlab = "Number of Neighbors",
     ylab = "Sum of Square Errors")
rmseMin = min(err)
kMin2 = which(err == rmseMin)[1]
segments(x0 = 0, x1 = kMin2, y0 = rmseMin, col = gray(0.4), 
         lty = 2, lwd = 2)
segments(x0 = kMin2, x1 = kMin2, y0 = 900,  y1 = rmseMin, 
         col = grey(0.4), lty = 2, lwd = 2)
mtext(kMin2, side = 1, line = 1, at = kMin2, col = grey(0.4))
text(x = kMin2 - 2, y = rmseMin + 40, 
     label = as.character(round(rmseMin)), col = grey(0.4))
```

Plotting error values for 20 distinct k test iterations, we see that k=8 produces optimal results.

```{r PlotErrors, eval=TRUE, echo=FALSE}
set.seed(seed = seed)
estXYk8 = predXY(newSignals = onlineSummary2[ , 6:11], 
                 newAngles = onlineSummary2[ , 4], 
                 offlineSummary2, numAngles = 3, k =kMin2)
SSE.k8 <- calcError(estXYk8, actualXY_2)
SSE.k8
```

```{r ErrorMap, eval=TRUE, echo=FALSE}
floorErrorMap(estXYk8, onlineSummary[ , c("posX","posY")], 
              trainPoints = trainPoints, AP = AP)
```
