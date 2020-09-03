

# t = "Time stamp (Milliseconds) since 12:00am, January 1, 1970"
# Id = router MAC address
# Pos = router location
# Degree = Direction scanning device measured in degrees that was carried by the researcher
# MAC = MAC address of either the router, or scanning device combined with corresponding values for signal strength (dBm), the mode in which it was operating(adhoc scanner = 1, access router = 3), and its corresponding channel frequency.
# Signal = Received Signal Strength in DbM

# Objective: Our job is to predict the XY positions for the online data set from the offline data set

# Define the MAC IDs of the relevant routers/access points
fileLoc = "F:/SMU/DS7333/Project1/offline.final.trace.txt"
macsC0 = c('00:0f:a3:39:e1:c0', '00:14:bf:b1:97:90', '00:14:bf:3b:c7:c6','00:14:bf:b1:97:81', '00:14:bf:b1:97:8a', '00:14:bf:b1:97:8d') #C0
macsCD = c('00:0f:a3:39:dd:cd', '00:14:bf:b1:97:90', '00:14:bf:3b:c7:c6','00:14:bf:b1:97:81', '00:14:bf:b1:97:8a', '00:14:bf:b1:97:8d') #CD
macsC0_CD = c('00:0f:a3:39:e1:c0', '00:0f:a3:39:dd:cd', '00:14:bf:b1:97:90','00:14:bf:3b:c7:c6', '00:14:bf:b1:97:8a', '00:14:bf:b1:97:8d') # C0 & CD
cols01 = c("posXY", "posX","posY", "orientation", "angle") 
cols02 = c("posXY", "posX","posY")
cols03 = "signal"
cols04 = c("posX", "posY")
cols05 = "avgSignal"
cols06 = c("time", "scanMac","posX", "posY", "posZ", "orientation","mac", "signal", "channel", "type")
cols07 = c("scanMac", "posZ", "channel", "type")
cols08 = c("time", "posX", "posY", "orientation", "signal")
cols09 = c("POSIXt", "POSIXct")
cols10 = c("posXY", "posX","posY", "orientation", "angle")
commentValue = "[;=,]"
commentValue2 = "#"
sepValue = "-"
accessPoint = "3"
seed = 125

# This takes all of the above and makes a function that combines all of it
processLine = function(x)
{ tokens = strsplit(x, commentValue)[[1]]
  if (length(tokens) == 10) return(NULL)
  tmp = matrix(tokens[ - (1:10) ], ncol= 4, byrow = TRUE)
  cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow(tmp), 6,
               byrow = TRUE), tmp)}

roundOrientation = function(angles) {
  refs = seq(0, by = 45, length  = 9)
  q = sapply(angles, function(o) which.min(abs(o - refs)))
  c(refs[1:8], 0)[q]}

readData =
  function(filename, macs )
  {txt = readLines(filename)
    lines = txt[ substr(txt, 1, 1) != commentValue2 ]
    tmp = lapply(lines, processLine)
    offline = as.data.frame(do.call("rbind", tmp),
                            stringsAsFactors= FALSE)
    names(offline) = cols06
    offline = offline[ offline$type == accessPoint, ]
    dropVars = cols07
    offline = offline[ , !( names(offline) %in% dropVars ) ]
    offline = offline[ offline$mac %in% macs, ]
    numVars = cols08
    offline[ numVars ] = lapply(offline[ numVars ], as.numeric)
    offline$rawTime = offline$time
    offline$time = offline$time/1000
    class(offline$time) = cols09
    # round orientations to nearest 45
    offline$angle = roundOrientation(offline$orientation)
    return(offline)}

# With C0 without CD
offline <- readData(filename = fileLoc , macs=macsC0)   # Change file location
offline$posXY = paste(offline$posX, offline$posY, sep = sepValue)

byLocAngleAP = with(offline, by(offline, list(posXY, angle, mac),function(x) x))

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

# With CD without C0
offline2 <- readData(filename = fileLoc , macs=macsCD)  # Change file location
offline2$posXY = paste(offline2$posX, offline2$posY, sep = sepValue)

byLocAngleAP2 = with(offline2, by(offline2, list(posXY, angle, mac), function(x) x))

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

# With C0 and CD
offlineall <- readData(filename = fileLoc , macs=macsC0_CD)   # Change file location
offlineall$posXY = paste(offlineall$posX, offlineall$posY, sep = sepValue)

byLocAngleAPall = with(offlineall, by(offlineall, list(posXY, angle, mac),function(x) x))

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

# With C0 without CD
online <- readData(filename = fileLoc, macs=macsC0)  
online$posXY = paste(online$posX, online$posY, sep = sepValue)

Loc = with(online, by(online, list(posXY), function(x) { 
  ans = x[1, cols01] 
  avgSS = tapply(x$signal, x$mac, mean) 
  y = matrix(avgSS, nrow = 1, ncol = 6, dimnames = list(ans$posXY, names(avgSS))) 
  cbind(ans, y) }))
onlineSummary = do.call("rbind", Loc)

# With CD without C0
online2 <- readData(filename = fileLoc, macs=macsCD)  
online2$posXY = paste(online2$posX, online2$posY, sep = sepValue)

Loc = with(online2, by(online2, list(posXY), function(x) {
  ans = x[1, cols01] 
  avgSS = tapply(x$signal, x$mac, mean) 
  y = matrix(avgSS, nrow = 1, ncol = 6, 
             dimnames = list(ans$posXY, names(avgSS))) 
  cbind(ans, y) }))
onlineSummary2 = do.call("rbind", Loc)

# With both CD and C0
onlineall <- readData(filename = fileLoc , macs=macsC0_CD)  
onlineall$posXY = paste(onlineall$posX, onlineall$posY, sep = sepValue)

Loc = with(onlineall, by(onlineall, list(posXY), function(x) {
  ans = x[1, cols01] 
  avgSS = tapply(x$signal, x$mac, mean) 
  y = matrix(avgSS, nrow = 1, ncol = 6, 
             dimnames = list(ans$posXY, names(avgSS))) 
  cbind(ans, y) }))
onlineSummaryAll = do.call("rbind", Loc)





#### EDA





## K-Nearest Neighbors Analysis



### Angle orientation adjustments before KNN
adjustData = function(data, varSignal = cols03, cols = cols02) {
  set01 = with(data, by(data, list(posXY), function(x) {
  ans = x[1, cols]
  avgSS = tapply(x[ , varSignal ], x$mac, mean)
  y = matrix(avgSS, nrow = 1, ncol = 6,
  dimnames = list(ans$posXY,names(avgSS)))
  cbind(ans, y)}))
  adjustDataRes = do.call("rbind", set01)
return(adjustDataRes)}

### Angle orientation adjustments before KNN - odd and evens
adjustAngle = function(adjustAngleData, signals = NULL, m = 1){
  refs = seq(0, by = 45, length  = 8)
  roundedAngle = roundOrientation(adjustAngleData)
  
  if (m %% 2 == 1) angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
  else {m = m + 1
    angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
    if (sign(adjustAngleData - roundedAngle) > -1) 
      angles = angles[ -1 ]
    else 
      angles = angles[ -m ]}
  angles = angles + roundedAngle
  angles[angles < 0] = angles[ angles < 0 ] + 360
  angles[angles > 360] = angles[ angles > 360 ] - 360
  angles = sort(angles)
  offlineSubset = signals[ signals$angle %in% angles, ]
  adjustData(offlineSubset, varSignal = cols05)}

### Neighbor location
locKNNpoints = function(signalData, trainSubset) { 
  diffs = apply(trainSubset[, 4:9], 1, function(x) x - signalData) 
  dists = apply(diffs, 2, function(x) sqrt(sum(x^2))) 
  closest = order(dists) 
  return(trainSubset[closest, 1:3])}

### Location prediction
knnPredict = function(signalData, newAngles, trainData,numAngles = 1, k = 3){closeXY = list(length = nrow(signalData))
  for (i in 1:nrow(signalData)) {
    trainSS = adjustAngle(newAngles[i], trainData, m = numAngles)
    closeXY[[i]] = locKNNpoints(signalData = as.numeric(signalData[i, ]), trainSS)}
    estXY = lapply(closeXY,function(x) sapply(x[ ,2:3],function(x) mean(x[1:k])))
    estXY = do.call("rbind", estXY)
  return(estXY)}

### C0 KNN Scoring
onlineLocs_C0 = onlineSummary[ , cols04]
### K = 1 - baseline
predLocs1 = knnPredict(signalData = onlineSummary[ , 6:11], newAngles = onlineSummary[ , 4], offlineSummary, numAngles = 3, k = 1)
C0error1 = function(predLocs1, onlineLocs_C0) sum(rowSums((predLocs1 - onlineLocs_C0)^2)) 
sapply(list(predLocs1), C0error1, onlineLocs_C0)
### K = 3
predLocs3 = knnPredict(signalData = onlineSummary[ , 6:11], newAngles = onlineSummary[ , 4], offlineSummary, numAngles = 3, k = 3)
C0error3 = function(predLocs3, onlineLocs_C0) sum(rowSums((predLocs3 - onlineLocs_C0)^2)) 
sapply(list(predLocs3), C0error3, onlineLocs_C0)
### K = 5
predLocs5 = knnPredict(signalData = onlineSummary[ , 6:11], newAngles = onlineSummary[ , 4], offlineSummary, numAngles = 3, k = 5)
C0error5 = function(predLocs5, onlineLocs_C0) sum(rowSums((predLocs5 - onlineLocs_C0)^2)) 
sapply(list(predLocs5), C0error5, onlineLocs_C0)
### K = 7
predLocs7 = knnPredict(signalData = onlineSummary[ , 6:11], newAngles = onlineSummary[ , 4], offlineSummary, numAngles = 3, k = 7)
C0error7 = function(predLocs7, onlineLocs_C0) sum(rowSums((predLocs7 - onlineLocs_C0)^2)) 
sapply(list(predLocs7), C0error7, onlineLocs_C0)
### K = 10
predLocs10 = knnPredict(signalData = onlineSummary[ , 6:11], newAngles = onlineSummary[ , 4], offlineSummary, numAngles = 3, k = 10)
C0error10 = function(predLocs10, onlineLocs_C0) sum(rowSums((predLocs10 - onlineLocs_C0)^2)) 
sapply(list(predLocs10), C0error10, onlineLocs_C0)
# Optimal K = 7 for C0 - 273.6

### CD KNN Scoring
onlineLocs_CD = onlineSummary2[ , cols04]
### K = 1 - baseline
predLocsCD_1 = knnPredict(signalData = onlineSummary2[ , 6:11], newAngles = onlineSummary2[ , 4], offlineSummary2, numAngles = 3, k = 1)
CDerror2_1 = function(predLocsCD_1, onlineLocs_CD) sum(rowSums((predLocsCD_1 - onlineLocs_CD)^2))
sapply(list(predLocsCD_1), CDerror2_1, onlineLocs_CD)
### K = 3
predLocsCD_3 = knnPredict(signalData = onlineSummary2[ , 6:11], newAngles = onlineSummary2[ , 4], offlineSummary2, numAngles = 3, k = 3)
CDerror2_3 = function(predLocsCD_3, onlineLocs_CD) sum(rowSums((predLocsCD_3 - onlineLocs_CD)^2))
sapply(list(predLocsCD_3), CDerror2_3, onlineLocs_CD)
### K = 5
predLocsCD_5 = knnPredict(signalData = onlineSummary2[ , 6:11], newAngles = onlineSummary2[ , 4], offlineSummary2, numAngles = 3, k = 5)
CDerror2_5 = function(predLocsCD_5, onlineLocs_CD) sum(rowSums((predLocsCD_5 - onlineLocs_CD)^2))
sapply(list(predLocsCD_5), CDerror2_5, onlineLocs_CD)
### K = 7
predLocsCD_7 = knnPredict(signalData = onlineSummary2[ , 6:11], newAngles = onlineSummary2[ , 4], offlineSummary2, numAngles = 3, k = 7)
CDerror2_7 = function(predLocsCD_7, onlineLocs_CD) sum(rowSums((predLocsCD_7 - onlineLocs_CD)^2))
sapply(list(predLocsCD_7), CDerror2_7, onlineLocs_CD)
### K = 10
predLocsCD_10 = knnPredict(signalData = onlineSummary2[ , 6:11], newAngles = onlineSummary2[ , 4], offlineSummary2, numAngles = 3, k = 10)
CDerror2_10 = function(predLocsCD_10, onlineLocs_CD) sum(rowSums((predLocsCD_10 - onlineLocs_CD)^2))
sapply(list(predLocsCD_10), CDerror2_10, onlineLocs_CD)
### Optimal K = 5 for CD - 249.9



#### KNN Distance Weighting

### To further improve the accuracy of predicting location, we up-weight stronger signals relative to weaker signals. 
### This allows us to give strong, closer signals a larger impact on determining the location than those that are weak and further away. 
### We use the weighting fraction below to achieve this result, where we use the signal strength as an estimator for distance.

### The locKNNpoints() function below has been modified from the previous version to also output the distance metrics utilized in 
### determining the "closest" k points. Additionally, the knnPredict() function has been modified to compute weights with the formula 
### above and distance outputs from the locKNNpoints() output. These weights are multiplied against each k nearest observation, 
### respectively, then summed to compute a weighted estimation by distance. These new calculations may allow locations of closer 
### distances to be more impactful than those further away, instead of equal weighting amongst the k points.
#Modify findNN to also output the distances


v = 11
permuteLocs = sample(unique(offlineSummary$posXY))
permuteLocs = matrix(permuteLocs, ncol = v, 
                     nrow = floor(length(permuteLocs)/v))



onlineFold = subset(offlineSummary, posXY %in% permuteLocs[ , 1])
head(onlineFold)
onlineFold.ul <- nrow(unique(onlineFold[,2:3]))


### Previously we structured and summarized the offline data into six signal strength columns, one for each access point.  
### We will do the same with our cross-validated test data. However, because it is easier to structure the test data in its complete 
### form from offline data which is then divided into our desired folds, we now need to modify the reshapeSS() function as follows.

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



KeepVars = c("posXY", "posX","posY", "orientation", "angle")

onlineCVSummary = reshapeSS(offline, keepVars = keepVars,sampleAngle = TRUE)
onlineCVSummary2 = reshapeSS(offline2, keepVars = keepVars,sampleAngle = TRUE)







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


###  will perform similar steps and perform v-fold tests on one to twenty k nearest neighbor estimations to find the optimal value of K with 
### weighted samples.

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


### Plotting error values for 20 distinct k test iterations, we see that k=8 produces optimal results.


set.seed(seed = seed)
estXYk8 = predXY(newSignals = onlineSummary2[ , 6:11], 
                 newAngles = onlineSummary2[ , 4], 
                 offlineSummary2, numAngles = 3, k =kMin2)
SSE.k8 <- calcError(estXYk8, actualXY_2)
SSE.k8



floorErrorMap(estXYk8, onlineSummary[ , c("posX","posY")], 
              trainPoints = trainPoints, AP = AP)


