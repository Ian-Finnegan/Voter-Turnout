
library(GWmodel)
library(car)
library(gridExtra)

data(DubVoter) # the data

plot(Dub.voter@data[,4:11],pch=16,cex=0.75) # correlation plots of variables

cor(Dub.voter@data[,4:11]) # correlation values

# bandwith determination
invisible(capture.output(bw.gwr.1 <- bw.gwr(GenEl2004 ~ DiffAdd + LARent + SC1 + Unempl + LowEduc + Age18_24 + Age25_44 + Age45_64, data = Dub.voter, approach = "AICc",kernel = "bisquare", adaptive = TRUE)))

# collinearity model
gwr.coll.data <- gwr.collin.diagno(GenEl2004 ~ DiffAdd + LARent + SC1 + Unempl + LowEduc + Age18_24 + Age25_44 + Age45_64, data = Dub.voter, bw = bw.gwr.1, kernel = "bisquare", adaptive = TRUE) 

# DiffAdd and Age25_44 variable collinearty
grid.arrange(
spplot(gwr.coll.data$SDF,"Corr_DiffAdd.Age25_44",key.space = "right", main=list(label="DiffAdd and Age25_44 \n Correlations")), 
spplot(gwr.coll.data$SDF,"DiffAdd_VIF",key.space = "right", main=list(label="VIFs for DiffAdd \n")), 
spplot(gwr.coll.data$SDF,"Age25_44_VIF",key.space = "right", main=list(label="VIFs for Age25_44 \n")),
ncol = 3)

# LARent and Unempl variable collinearty
grid.arrange(
spplot(gwr.coll.data$SDF,"Corr_LARent.Unempl",key.space = "right", main=list(label="LARent and Unempl \n Correlations")), 
spplot(gwr.coll.data$SDF,"LARent_VIF",key.space = "right", main=list(label="VIFs for LARent \n")), 
spplot(gwr.coll.data$SDF,"Unempl_VIF",key.space = "right", main=list(label="VIFs for Unempl \n")),
ncol = 3)

data(DubVoter) # the data

Data.scaled <- scale(as.matrix(Dub.voter@data[,5:11])) # scaling date

Coords <- as.matrix(cbind(Dub.voter$X,Dub.voter$Y)) # coordinate map
Data.scaled.spdf <- SpatialPointsDataFrame(Coords, as.data.frame(Data.scaled)) # spatial data frame

# bandwidth detemination
invisible(capture.output(bw.gwpca.1 <-bw.gwpca(Data.scaled.spdf,vars = colnames(Data.scaled.spdf@data), k=3, adaptive=TRUE)))

# gwpca model
gwpca.1 <- gwpca(Data.scaled.spdf, vars = colnames(Data.scaled.spdf@data), bw=bw.gwpca.1, k=ncol(Data.scaled), adaptive=TRUE)
gwpca.1

# proportion function 
prop.var <- function(gwpca.obj, n.components) {return((rowSums(gwpca.obj$var[,1:n.components])/rowSums(gwpca.obj$var))*100)}

Dub.voter$var.gwpca <- prop.var(gwpca.1,2) # adding proportion to data

# plotting pca comp 1 and 2 values
spplot(Dub.voter,"var.gwpca",key.space = "right", cuts=7, main=list(label="GW PCA Comp 1 and 2")) 

data(DubVoter) # the data

# bandwith determination
invisible(capture.output(bw.gwr.2 <- bw.gwr(GenEl2004 ~ LARent + SC1 + Unempl + LowEduc + Age18_24 + Age25_44 + Age45_64, data = Dub.voter, approach = "AICc",kernel = "bisquare", adaptive = TRUE)))

# basic gwr model
bgwr.res <- gwr.basic(GenEl2004 ~ LARent + SC1 + Unempl + LowEduc + Age18_24 + Age25_44 + Age45_64, data = Dub.voter, bw = bw.gwr.2, kernel = "bisquare", adaptive = TRUE)
bgwr.res

# monte carlo determination of p-values
bgwr.mc <- montecarlo.gwr(GenEl2004 ~ LARent + SC1 + Unempl + LowEduc + Age18_24 + Age25_44 + Age45_64, data = Dub.voter, bw = bw.gwr.2, kernel = "bisquare", adaptive = TRUE)

# mixed gwr model
mgwr.res <- gwr.mixed(GenEl2004 ~ LARent + SC1 + Unempl + LowEduc + Age18_24 + Age25_44 + Age45_64, data = Dub.voter, bw = bw.gwr.2, fixed.vars = c("LARent", "LowEduc", "Age18_24", "Age25_44", "Age45_64"), intercept.fixed = TRUE, kernel = "bisquare", adaptive = TRUE)
mgwr.res 

# Basic vs Mixed GWR for unempl 
grid.arrange(
spplot(bgwr.res$SDF, "Unempl", key.space = "right", main = "Basic GWR for \n Unempl Estimates"),
spplot(mgwr.res$SDF, "Unempl_L", key.space = "right", main = "Mixed GWR \n Unempl Estimates"),
ncol = 2)
