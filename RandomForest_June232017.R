# This code applies random forest algorithm to classify aerial imagery into pre-defined classes
#####################################################################
install.packages("sp") 
install.packages("rgdal")
install.packages("raster")
install.packages("randomForest")
install.packages("caret")
install.packages("e1071")

setwd('') 

# read raster
rgbRaster = stack("resampled.tif")
plot(rgbRaster)

# replace band names
names(rgbRaster) <- c(paste0("B", 1:4, coll = "")) 

# display RGB raster
plotRGB(rgbRaster)

### read shapefile with training data and store the name "class" column
trainData = readOGR(dsn="~/classification/",layer="trainings4pnt") 
responseCol <- "Class"

# extract training pixels
dfAll = data.frame(matrix(vector(), nrow = 0, ncol = length(names(rgbRaster)) + 1)) # dfAll - storage
#  for pixel values in the training areas for every band along with corresponding land cover class id  
for (i in 1:length(unique(trainData[[responseCol]]))){                          
  category <- unique(trainData[[responseCol]])[i]
  categorymap <- trainData[trainData[[responseCol]] == category,]
  dataSet <- extract(rgbRaster, categorymap)
  dataSet <- dataSet[!unlist(lapply(dataSet, is.null))]
  dataSet <- lapply(dataSet, function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})
  df <- do.call("rbind", dataSet)
  dfAll <- rbind(dfAll, df)
}

#Subset data generating 1000 random samples
nsamples <- 1000
sdfAll <- subset(dfAll[sample(1:nrow(dfAll), nsamples), ])

### Define and fit RandomForest model
# specify the model as a formula with the dependent variable 
modFit_rf <- train(as.factor(class) ~ B2 + B3 + B4, method = "rf", data = sdfAll)
# (i.e., the land cover types ids) encoded as factors

# create cluster object to speed up computation
beginCluster()
preds_rf <- clusterR(rgbRaster, raster::predict, args = list(model = modFit_rf))
endCluster()

output = preds_rf

plot(preds_rf)

bf <- writeRaster(preds_rf, filename="bragg_rf.tif", overwrite=TRUE)
plot(bf)