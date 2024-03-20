# Run PCA using vegan and ape


# 1. Load libraries and data ----
# Install and load the following packages
library(tidyverse)
library(ggpubr)
library(forcats)
library(FactoMineR)
library(factoextra)
library(corrplot)


# Read in data
skins = read.csv(file = "C:/Users/alexs/Documents/pca/skins_data_for_pca.csv", dec = ".", sep = ",")
#skins = read.csv(file = "skins_data_for_pca.csv", dec = ".", sep = ",")

# 2. Explore dataset and make PCA plot
# choose active -aka numeric- columns
#reorder data frame so ID comes first & remove redundant columns
colnames(skins)
skins_long <- skins[,c(1,2,3,8,9,4,5,6,7)]
colnames(skins_long)

# create dataframes with and without relevant variables 
skins.active <- skins_long[6:9] # only numerical values
skins.active <- na.omit(skins.active, na.action = "omit")
skins.inactive <- na.omit(skins_long, na.action = "omit")

# get variables factor map
skins.pca <- PCA(skins.active, graph = TRUE)

# generate scree plot - what is relative importance of each dimension?
fviz_eig(skins.pca, addlabels = TRUE, ylim = c(0, 50)) + 
  ggtitle("c)") +
  theme_classic(base_size = 16) 

var <- get_pca_var(skins.pca)

# Coordinates
head(var$coord)

# Cos2: quality on the factor map
head(var$cos2)

# Contributions to the principal components
head(var$contrib)

# Coordinates of variables
head(var$coord, 4)

# Examine dimensions 1 and 2
fviz_pca_var(skins.pca, col.var = "black") 

#correlation plot - weighting of variable by dimension
(correlation_subplot <- corrplot(var$cos2,tl.col = 'black',is.corr=TRUE)) + ggtitle("d)")  

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(skins.pca, choice = "var", axes = 1:2)

# Color by cos2 values: quality on the factor map
fviz_pca_var(skins.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

# Change the transparency by cos2 values
fviz_pca_var(skins.pca, alpha.var = "cos2")
head(var$contrib, 4)

# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)
set.seed(333) # because there are 333 observations
skins.km <- kmeans(var$coord, centers = 3, nstart = 25)
grp <- as.factor(skins.km$cluster)

# Color variables by groups
fviz_pca_var(skins.pca, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster",
             repel = TRUE)

#look at individual contributions
ind <- get_pca_ind(skins.pca)
ind

# Coordinates of individuals
head(ind$coord)

# Quality of individuals
head(ind$cos2)

# Contributions of individuals
head(ind$contrib)
fviz_pca_ind(skins.pca)
fviz_pca_ind(skins.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_pca_ind(skins.pca, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)

#contribution of individuals
fviz_cos2(skins.pca, choice = "ind")

# Total contribution on PC1 and PC2
fviz_contrib(skins.pca, choice = "ind", axes = 1:2)

# Create a random continuous variable of length 333
# Same length as the number of active individuals in the PCA
set.seed(333)
my.cont.var <- rnorm(333)

# Color individuals by the continuous variable
fviz_pca_ind(skins.pca, col.ind = my.cont.var,
             gradient.cols = NULL,
             legend.title = "Cont.Var")

# 2. Plot by country and year ----

#colour by groups
head(skins.active, 8)

# PCA - cluster by COUNTRY?
fviz_pca_ind(skins.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = skins.inactive$Country , # color by groups
             addEllipses = TRUE, ellipse.type = "confidence",# Concentration ellipses,
             legend.title = "Groups")


# PCA - cluster by YEAR?
fviz_pca_ind(skins.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = as.factor(skins.inactive$Year) , # color by groups
             addEllipses = TRUE, ellipse.type = "confidence",# Concentration ellipses,
             legend.title = "Groups")


# nice looking plot below - feel free to substitue country for year (fill.ind = as.factor(skins.inactive$Country))
(simple_biplot_country <- fviz_pca_biplot(skins.pca, 
                                 # Individuals
                                 geom.ind = "point",
                                 fill.ind = skins.inactive$Country, col.ind = "black",
                                 pointshape = 21, pointsize = 2,
                                 addEllipses = TRUE, ellipse.type = "confidence",
                                 ellipse.alpha = 0.5,
                                 # Variables  
                                 repel = TRUE))

# another way of plotting
ind.p <- fviz_pca_biplot(skins.pca, geom = "point", col.ind = skins.inactive$Country)
ggpubr::ggpar(ind.p,
              title = "Principal Component Analysis",
              fill.ind = skins.inactive$Plot, col.ind = "black",
              subtitle = "Skins Characteristics",
              addEllipses = TRUE, ellipse.type = "confidence",
              xlab = "PC1", ylab = "PC2", 
              legend.title = "Location", legend.position = "top",
              ggtheme = theme_linedraw())

# 3. Biplot PC1and2 vs PC2and3 ----
(biplotdim12 <- fviz_pca_biplot(skins.pca, axes = c(1,2),
                                # Individuals
                                geom.ind = "point",
                                fill.ind = skins.inactive$Country, col.var="black",
                                pointshape = 20,  alpha = 0.2,
                                pointsize = skins.inactive$Year ,
                                addEllipses = TRUE, ellipse.type = "confidence",
                                ellipse.alpha = 0.6,
                                repel = TRUE))

(biplotdim12 <- fviz_pca_biplot(skins.pca, axes = c(2,3),
                                # Individuals
                                geom.ind = "point",
                                fill.ind = skins.inactive$Country, col.var="black",
                                pointshape = 21, 
                                pointsize = skins.inactive$Year ,
                                alpha = 0.4,
                                addEllipses = TRUE, ellipse.type = "confidence",
                                ellipse.alpha =  0.6,
                                repel = TRUE))                                                                            
                                                                                             

