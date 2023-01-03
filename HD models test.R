library(lmfor)
library(brnn)

# Tilia platyphyllos height data
data_height <- read.csv("data_height_TIPL.csv")

plot(data_height$DBH, data_height$height)

data_height <- data_height[!is.na(data_height$height),]

# Curtis level = 1
model <- ImputeHeights(d = data_height$DBH, h = data_height$height, plot=data_height$ID, modelName="curtis", 
                       nranp = 2, varf = TRUE, addResidual = FALSE, makeplot=TRUE, level = 1,
                       start=NA, bh=1.3, control=list(maxIter = 1500),random=NA)

# Curtis level = 0
model <- ImputeHeights(d = data_height$DBH, h = data_height$height, plot=data_height$ID, modelName="curtis", 
                       nranp = 2, varf = TRUE, addResidual = FALSE, makeplot=TRUE, level = 0,
                       start=NA, bh=1.3, control=list(maxIter = 1500),random=NA)

model <- brnn(height ~ DBH, data = data_height)
data_height$h_predicted <- predict(model)

# predicted vs. observed
plot(data_height$h_predicted, data_height$height)

# HD curve 
df_curve <- data.frame(DBH = seq(10, max(data_height$DBH)))
df_curve$h_predicted <- predict(model, df_curve)

plot(df_curve$DBH, df_curve$h_predicted, type = "l")






# Naslund level = 1  
model <- ImputeHeights(d = data_height$DBH, h = data_height$height, plot=data_height$ID, modelName="naslund", 
                       nranp = 2, varf = TRUE, addResidual = FALSE, makeplot=TRUE, level = 1,
                       start=NA, bh=1.3, control=list(maxIter = 1500),random=NA)

# Naslund level = 0  
model <- ImputeHeights(d = data_height$DBH, h = data_height$height, plot=data_height$ID, modelName="naslund", 
                       nranp = 2, varf = TRUE, addResidual = FALSE, makeplot=TRUE, level = 0,
                       start=NA, bh=1.3, control=list(maxIter = 1500),random=NA)

