
# 4/6/2020

# Hierarchical Clustering of Clackamas Water Customers #

# This is my clean version of the first analysis.
# Paste into Markdown when done.

####----------> Rationale for sharing this project

# I chose to share this project because it seemed to have a lot of elements
# Collaboration with multiple stakeholders, data collection, messy data,
# data from multiple sources,Geocoding using Google API, exploratory anaylses, machine learning (segmentation),
# Use of an additional analysis platform (ArcGIS Pro), and Mapping. The final result
# helped inform the "Save it for the Salmon" and ongoing "Clackamas Watershed Resiliency Project"

# Segmentation, more specifically psychographic and behavioral, is common in the power
# sector but not water. I thought this would be a novel contribution. I was
# curious how water customer attitudes around water conservation and climate change manifested
# among customers with the intutition that there are different customer "archetypes".
# Knowing about these archetypes can help focus messaging and this project was developed
# as the starting point for an ongoing social marketing campaign around watershed conservation
# and protection.

# Questions:

#1. Do water customer attitudes show attitudinal clustering around climate change, water conservation
#   trust, and place attitudes.

#2. Do these attitudinal clusters exhibit spatial clustering within the water service area?



#####################################1. Load Packages

# Explain all the packages

library(tidyverse)  # Set of useful core packages
library(Hmisc)      # Misc utility package
library(mice)       # For imputation
library(cluster)    # Clustering algorithms
library(factoextra) # Extract and visualize the output of multivariate data analyses
library(psych)      # General purpose toolbox for personality, psychometric theory and experimental psychology
library(NbClust)    # Provides 30 indexes for determining the optimal number of clusters in a data set
library(clValid)    # Statistical and biological validation of clustering results
library(fpc)        # Flexible procedures for clustering - Various methods for clustering and cluster validation
library(DataExplorer) # Easy exploratory data analysis
library(corrplot)   # Makes pretty correlation plots

#####################################2. Exploratory Data Analysis (EDA)

#Load and Inspect Data

#load("./water_cust_seg_coord_FINAL_MASTER.RData")

# Added the file with the addresses removed and changed what I originally
# pointed to watercust. Everything else should be the same.

load(file = "watercust_masterdata.rda")



watercust<-watercust_masterdata


#Initially inspect data
glimpse(watercust)


# Visualize variables in dataset
plot_str(watercust)



#Explore missing values

plot_missing(watercust)


plot_density(watercust)

## Reduce dataset to variables of interest and convert Likert scale variables to factor
## Keep attitudinal, demographic and respondent ID to join back to original dataset

#Change ID to key, it will make joining easier later

watercust <- watercust %>% 
  rename(key = ID) %>% 
  rename(Educ = Education1) %>% 
  rename(Politics = Political.Attitudes1) %>% 
  rename(Climate_imp = CCImpactInd_rc) %>% 
  rename(Age = Age1) 

watercust_dem<- watercust %>% 
                select(key,Age,Educ,Politics,inc_val,Climate_imp, 
                       pro_water_con,Trust_Ind,Place_Attitudes)

glimpse(watercust_dem)


#####################################3. Make a Correlogram

# Make a correlogram to visualize correlaions

watercust_dem_cor<-watercust_dem %>% 
  select(-key)

cordta<-watercust_dem_cor
cor1 <- cor(cordta, use="pairwise.complete.obs", method="pearson")

corrplot(cor1)

# Compute the p-value of correlations
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(watercust_dem_cor)
head(p.mat[, 1:5])

#Customize Correlogram

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
watercust_cor<-corrplot(cor1, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

# I think this corrplot tells the best story
watercust_cor


#########################################4. Make Clusters and Visualize

# Subset data for clustering. Make sure 1 dataset has unique key and other does not
## So it can be joined back to the spatial dataset later.

watercust_clus_wkey<- watercust_dem %>% 
  select(key,Climate_imp,pro_water_con,Trust_Ind,Place_Attitudes)

watercust_clus_nokey <- watercust_dem %>% 
  select(Climate_imp,pro_water_con,Trust_Ind,Place_Attitudes)

## Remove Missing Variables

watercust_clus_wkey <- watercust_clus_wkey[complete.cases(watercust_clus_wkey), ]
watercust_clus_nokey <- watercust_clus_nokey[complete.cases(watercust_clus_nokey), ]

## Scale prior to clustering

watercust_clus_nokey_sc <- scale(watercust_clus_nokey)

## Determining optimal clusters using elbow method

library(cluster)    # clustering algorithms
library(factoextra)

fviz_nbclust(watercust_clus_nokey_sc , FUN = hcut, method = "wss")

# Method is used for determining the optimal number of clusters. "wss" is
# total within sum of squares


## Average Silhouette Method

fviz_nbclust(watercust_clus_nokey_sc, FUN = hcut, method = "silhouette")

##------> Note: Both the elbow method and average silhouette width show "2" as the optimal cluster solution


# Test optimal number of clusters
library("NbClust")
set.seed(42)
res.nbclust = NbClust(watercust_clus_nokey_sc, distance = "euclidean",
                      min.nc = 2, max.nc = 10,
                      method = "ward.D", index = "all")
factoextra::fviz_nbclust(res.nbclust)+theme_minimal()

##-----> Note: Uses a variety of indices (e.g. Too many to mention - See documentation here https://www.rdocumentation.org/packages/NbClust/versions/3.0/topics/NbClust)

### Optimal number of clusters is 2.

# Hierarchical clustering
hc.res <- eclust(watercust_clus_nokey_sc, "hclust", k = 2, hc_metric = "euclidean", 
                 hc_method = "ward.D", graph = FALSE)


# Validating the best clustering algorithm. This is incredibly useful.

library(clValid)
watercust_clus_valid = clValid(watercust_clus_nokey_sc, nClust = 2:6,
                               clMethods = c("hierarchical","kmeans","pam"),
                               validation = "internal")
summary(watercust_clus_valid)


#Validating Clusters #
library(fpc)
dd <- dist(watercust_clus_nokey_sc, method ="euclidean")

# Statistics for hierarchical clustering
hc_stats <- cluster.stats(dd,  hc.res$cluster)
# (HCLUST) within clusters sum of squares
hc_stats$within.cluster.ss
hc_stats$clus.avg.silwidths

# Visualize Dendrogram

sp = fviz_dend(hc.res, show_labels = FALSE,
               palette = "Set1",main = "Cluster Dendrogram", theme(plot.title = element_text(hjust = 0.5)),cex = 0.75, as.ggplot = TRUE)

sp+ scale_color_manual(values=c("#FC4E07", "#2E9FDF"))

##Plot cluster solution
# Visualize HC clusters
sp2 <- fviz_cluster(hc.res, geom = "point", ellipse.type = "norm",
                    palette = c("#FC4E07", "#2E9FDF"), ggtheme = theme_minimal())
print(sp2)


# Cut tree into 2 groups and attach to original data

sub_grp <- cutree(hc.res, k = 2)
clusters <- sub_grp
clusters<-as.data.frame(clusters)

# Join clusters back to original data. Select only relevant variables. Rename variables with unintelligible names.

watercust_clusters<- watercust_clus_wkey %>% 
  bind_cols(clusters) %>% 
  left_join(watercust,watercust_clusters, by="key") %>% 
  select(key,Age,Educ,Politics,inc_val,Climate_imp.y,pro_water_con.y,Trust_Ind.y,Place_Attitudes.y,clusters,lon,lat) %>% 
  rename(Respondent = key)
names(watercust_clusters)<- gsub(".y", "", names(watercust_clusters))                    

# Save out the data for future analyses

save(watercust_clusters, file = "watercust_clusters.RData")


####################5. Validate Clusters with MANOVA

## Subset cluster output

man_dta<-watercust_clusters %>% 
  select(clusters,Climate_imp,pro_water_con,Trust_Ind,Place_Attitudes)

####---> MANOVA

# We are looking to see if there is a significant difference
# in means between the two groups.

#library(MVN)  # Not sure if I need this
#library(goftest)# Not sure if I need
#library(nlme) # Not sure if I need this
library(MASS)

set.seed(42)

man_dta$clusters = as.factor(man_dta$clusters)
Y = as.matrix(cbind(man_dta$Climate_imp, man_dta$pro_water_con, man_dta$Trust_Ind,man_dta$Place_Attitudes))
c = as.vector(man_dta$clusters)


m = manova(Y~c)
summary(m , test = "Pillai")
summary(m , test = "Wilks")
summary(m , test = "Hotelling-Lawley")
summary(m , test = "Roy")

# We can infer from the tests that the clusters are indeed statistically different from one another.


#########################6. Interpret Clusters

# Now it's time to make some summary tables to interpret the clusters.

summary(watercust_clusters)

# Consider separating into two datasets then combining in the end.

watercust_clust1 <- watercust_clusters %>% 
  filter(clusters ==1)

watercust_clust2 <- watercust_clusters %>% 
  filter(clusters ==2)

#####
##### Begin by looking at cluster 1

# Age
summary(watercust_clust1$Age)


#Income
summary(watercust_clust1$inc_val)


#Education

clust1_ed<-watercust_clust1%>% filter(!is.na(Educ))
clust1_ed <- clust1_ed%>% group_by(Educ) %>% summarise(n = n()) %>% ungroup() %>% 
  mutate(total = sum(n), share = n/total) %>% 
  filter(Educ >= 4) 

bachelor_or_higher1<-sum(clust1_ed$share)

summary(clust1_ed)
print(bachelor_or_higher1)

# Politics

watercust_clust1<-watercust_clust1 %>% filter(!is.na(Politics))

#ELSE <- TRUE
politics <- watercust_clust1%>% mutate(PoliticsSummary = 
                                         case_when( Politics >= 0 &
                                                      Politics < 2 ~ "Liberal",
                                                    Politics >= 2 & 
                                                      Politics < 3 ~ "Neutral",
                                                    Politics >= 4 ~ "Conservative")) %>% 
  dplyr::select(Politics, PoliticsSummary)
polp1 <- politics %>% group_by(PoliticsSummary) %>% summarise(n = n()) %>% ungroup() %>% 
  mutate(total = sum(n), share = n/total)
print(polp1)

# Climate Change Impact Beliefs

watercust_clust1<-watercust_clust1 %>% filter(!is.na(Climate_imp))

#ELSE <- TRUE
climate1<- watercust_clust1 %>% mutate(pnwSummary1 = 
                                         case_when( Climate_imp >= 0 &
                                                      Climate_imp < 2 ~ "Low",
                                                    Climate_imp >= 2 & 
                                                      Climate_imp< 3 ~ "Medium",
                                                    Climate_imp >= 3 ~ "High")) %>% 
  dplyr::select(Climate_imp, pnwSummary1)
pnwp1 <- climate1  %>% dplyr::group_by(pnwSummary1) %>% summarise(n = n()) %>% ungroup() %>% 
  mutate(total = sum(n), share = n/total)

print(pnwp1)

# Pro Water Conservation


watercust_clust1<-watercust_clust1 %>% filter(!is.na(pro_water_con))

clust1_prowat<- watercust_clust1 %>% mutate(pro_water_conSummary1 = 
                                              case_when( pro_water_con >= 0 &
                                                           pro_water_con < 2 ~ "Low",
                                                         pro_water_con >= 2 & 
                                                           pro_water_con < 3 ~ "Medium",
                                                         pro_water_con >= 3 ~ "High")) %>% 
  dplyr::select(pro_water_con, pro_water_conSummary1)
prowatp1 <- clust1_prowat  %>% dplyr::group_by(pro_water_conSummary1) %>% summarise(n = n()) %>% ungroup() %>% 
  mutate(total = sum(n), share = n/total)

print(prowatp1)

# Trust

watercust_clust1<-watercust_clust1 %>% filter(!is.na(Trust_Ind))

#ELSE <- TRUE
clust1_trust<- watercust_clust1 %>% mutate(Trust_IndexSummary1 = 
                                             case_when( Trust_Ind >= 0 &
                                                          Trust_Ind < 2 ~ "Low",
                                                        Trust_Ind >= 2 & 
                                                          Trust_Ind < 3 ~ "Medium",
                                                        Trust_Ind >= 3 ~ "High")) %>% 
  dplyr::select(Trust_Ind, Trust_IndexSummary1)
trustp1 <- clust1_trust  %>% dplyr::group_by(Trust_IndexSummary1) %>% summarise(n = n()) %>% ungroup() %>% 
  mutate(total = sum(n), share = n/total)

print(trustp1)

# Place Attitudes

watercust_clust1<-watercust_clust1 %>% filter(!is.na(Place_Attitudes))

#ELSE <- TRUE
pl_attitudes <- watercust_clust1 %>% mutate(AttitudeSummary1 = 
                                              case_when( Place_Attitudes >= 0 &
                                                           Place_Attitudes < 2 ~ "Low",
                                                         Place_Attitudes >= 2 & 
                                                           Place_Attitudes < 3 ~ "Medium",
                                                         Place_Attitudes >= 4 ~ "High")) %>% 
  dplyr::select(Place_Attitudes, AttitudeSummary1)
attp1 <- pl_attitudes %>% group_by(AttitudeSummary1) %>% summarise(n = n()) %>% ungroup() %>% 
  mutate(total = sum(n), share = n/total)

print(attp1)


######
###### Cluster 2

# Age
summary(watercust_clust2$Age)


#Income
summary(watercust_clust2$inc_val)


#Education

clust2_ed<-watercust_clust2%>% filter(!is.na(Educ))
clust2_ed <- clust2_ed%>% group_by(Educ) %>% summarise(n = n()) %>% ungroup() %>% 
  mutate(total = sum(n), share = n/total) %>% 
  filter(Educ >= 4) 

bachelor_or_higher2<-sum(clust2_ed$share)

summary(clust2_ed)
print(bachelor_or_higher2)

# Politics

watercust_clust2<-watercust_clust2 %>% filter(!is.na(Politics))

#ELSE <- TRUE
politics <- watercust_clust2%>% mutate(PoliticsSummary2 = 
                                         case_when( Politics >= 0 &
                                                      Politics < 2 ~ "Liberal",
                                                    Politics >= 2 & 
                                                      Politics < 3 ~ "Neutral",
                                                    Politics >= 4 ~ "Conservative")) %>% 
  dplyr::select(Politics, PoliticsSummary2)
polp2 <- politics %>% group_by(PoliticsSummary2) %>% summarise(n = n()) %>% ungroup() %>% 
  mutate(total = sum(n), share = n/total)
print(polp2)

# Climate Change Impact Beliefs

watercust_clust2<-watercust_clust2 %>% filter(!is.na(Climate_imp))

#ELSE <- TRUE
climate2<- watercust_clust2 %>% mutate(pnwSummary2 = 
                                         case_when( Climate_imp >= 0 &
                                                      Climate_imp < 2 ~ "Low",
                                                    Climate_imp >= 2 & 
                                                      Climate_imp< 3 ~ "Medium",
                                                    Climate_imp >= 3 ~ "High")) %>% 
  dplyr::select(Climate_imp, pnwSummary2)
pnwp2 <- climate2  %>% dplyr::group_by(pnwSummary2) %>% summarise(n = n()) %>% ungroup() %>% 
  mutate(total = sum(n), share = n/total)

print(pnwp2)

# Pro Water Conservation


watercust_clust2<-watercust_clust2 %>% filter(!is.na(pro_water_con))

clust2_prowat<- watercust_clust2 %>% mutate(pro_water_conSummary2 = 
                                              case_when( pro_water_con >= 0 &
                                                           pro_water_con < 2 ~ "Low",
                                                         pro_water_con >= 2 & 
                                                           pro_water_con < 3 ~ "Medium",
                                                         pro_water_con >= 3 ~ "High")) %>% 
  dplyr::select(pro_water_con, pro_water_conSummary2)
prowatp2 <- clust2_prowat  %>% dplyr::group_by(pro_water_conSummary2) %>% summarise(n = n()) %>% ungroup() %>% 
  mutate(total = sum(n), share = n/total)

print(prowatp2)

# Trust

watercust_clust2<-watercust_clust2 %>% filter(!is.na(Trust_Ind))

#ELSE <- TRUE
clust2_trust<- watercust_clust2 %>% mutate(Trust_IndexSummary2 = 
                                             case_when( Trust_Ind >= 0 &
                                                          Trust_Ind < 2 ~ "Low",
                                                        Trust_Ind >= 2 & 
                                                          Trust_Ind < 3 ~ "Medium",
                                                        Trust_Ind >= 3 ~ "High")) %>% 
  dplyr::select(Trust_Ind, Trust_IndexSummary2)
trustp2 <- clust2_trust  %>% dplyr::group_by(Trust_IndexSummary2) %>% summarise(n = n()) %>% ungroup() %>% 
  mutate(total = sum(n), share = n/total)

print(trustp2)

# Place Attitudes

watercust_clust2<-watercust_clust2 %>% filter(!is.na(Place_Attitudes))

#ELSE <- TRUE
pl_attitudes <- watercust_clust2 %>% mutate(AttitudeSummary2 = 
                                              case_when( Place_Attitudes >= 0 &
                                                           Place_Attitudes < 2 ~ "Low",
                                                         Place_Attitudes >= 2 & 
                                                           Place_Attitudes < 3 ~ "Medium",
                                                         Place_Attitudes >= 4 ~ "High")) %>% 
  dplyr::select(Place_Attitudes, AttitudeSummary2)
attp2 <- pl_attitudes %>% group_by(AttitudeSummary2) %>% summarise(n = n()) %>% ungroup() %>% 
  mutate(total = sum(n), share = n/total)

print(attp2)

# Combine into table and interpret customer types


water_customer_types<-tribble(~Variable, ~Cluster1,~Cluster2,"MedAge",62,57,"MedIncome",87500,112500,
                              "Bachelor_or_Higher_perc",54,68,"Conservative_perc",11,3,"Liberal_perc",27,66,
                              "Neutral_perc",35,16,"Strong_Climate_Beliefs_perc",11,65,
                              "Medium_Climate_Beliefs_perc",72,34,"Weak_Climate_Beliefs_perc",17,1,
                              "Strong_Water_Conserv_perc",17,53,"Medium_Water_Conserv_perc",54,47,
                              "Weak_Water_Conserv_perc",29,0,"High_Trust_perc",2,0,"Medium_Trust_perc",26,67,
                              "Low_Trust_perc",72,31,"Medium_Place_Attachment_perc",55,42,
                              "Weak_Place_Attachment_perc",56,27)

#View results

print(water_customer_types)

# Print a prettier table for Markdown. Makes it more readable
library(knitr)
library(kableExtra)

water_customer_types %>%
  knitr::kable("html", align = 'clc', caption = 'Water Customer Types') %>%
  kable_styling(full_width = F, position = "center")

####-------> Interpret Tables

# We can see some pretty big differences betweeen the customer types
# begin to emerge. Demographic differences show customer type 2 to be 
# Slightly younger, wealthier, and better educated.
# Customer type 2 is also more liberal with stronger beliefs
# about climate change and water conservation.
# I think a very interesting point is Trust( Trust in institutions to manager water)
# Cluster 1 has 72% low trust versus 31% low trust. These are pretty stark differences.

# For segmentation to be useful it must be desciptive. Therefore
# "clusters" and "customer types" will now get more descriptive names

# Cluster 1 -----> Water wasting climate skeptics

# Cluster 2 -----> Water conscious green progressives

# The names may seem silly or judgemental but they serve to make a point 
# about the differences between the groups and paint a picture.



#7. Perform HotSpot Analysis

## Pull in data and convert to Spatial Points Polygon
library(sp)
library(sf)
library(rgdal)

load("./watercust_clusters.RData")

watercust_sp<-watercust_clusters %>% 
  dplyr::select(Respondent,clusters,lon,lat)
#Now filter out by cluster

clus1_sp<-watercust_sp %>% 
  filter(clusters==1)

clus2_sp<-watercust_sp %>% 
  filter(clusters==2)

#Convert to spatial object


coordinates(clus1_sp) <- c("lon", "lat")
coordinates(clus2_sp) <- c("lon", "lat")

# Examine the object
class(clus1_sp)
class(clus2_sp)
# Assign the projection or check if it is assigned
is.projected(clus1_sp)
is.projected(clus2_sp)

# Not projected but we knew that
proj4string(clus1_sp) <- CRS("+init=epsg:4326") # this is WGS84
proj4string(clus2_sp) <- CRS("+init=epsg:4326") # this is WGS84


# Export shapefiles for maniuplation in ArcGIS Pro

# Note: Layer already exists and I don't want to duplicate so 
# I have nullified for demonstration purposes

#writeOGR(clus1_sp, ".", "clus1", driver = "ESRI Shapefile")
#writeOGR(clus2_sp, ".", "clus2", driver = "ESRI Shapefile")

##### Perform Hot Spot Analysis in ArcGIS Pro



#8. Show Hot Spot Map

library(leaflet)
library(leaflet.extras)
library(htmltools)



#Read in shapefiles generated in ArcGIS Pro

clus1htspt_poly<-readOGR(".","clus1_htspt")
clus2htspt_poly<-readOGR(".","clus2_htspt")

class(clus1htspt_poly)


# This is our base map
map<- leaflet() %>% 
  addProviderTiles("CartoDB")

# Let's save out one for base to making plotting easier later.

base_map <- map

# Create a new variable only showing areas of 99% Confidence 
# of Hot Spot (High spatial clustering). Creating a factor variable will
# make visualization much easier

clus1htspt_poly$level<-clus1htspt_poly$Gi_Bin
clus2htspt_poly$level<-clus2htspt_poly$Gi_Bin

# Make a color pallette
pal1 <- colorFactor(palette = c("red"), 
                    levels = c( "3"))
pal2 <- colorFactor(palette = c("blue"), 
                    levels = c( "3"))





customertype1map = addPolygons(base_map, 
                          data = clus1htspt_poly,
                          stroke = FALSE, 
                          smoothFactor = 0.2, 
                          opacity = 1,
                          color = ~pal1(clus1htspt_poly$level)) %>% 
                  addLegend("bottomright", colors= c("red"), 
                  labels=c("Water Wasting Climate Skeptics")) %>% 
  
                  addMeasure() %>% 
                  addResetMapButton()




# Now make cluster 2

customertype2map = addPolygons(base_map, 
                          data = clus2htspt_poly,
                          stroke = FALSE, 
                          smoothFactor = 0.2, 
                          opacity = 1,
                          color = ~pal2(clus2htspt_poly$level)) %>% 
                  addLegend("bottomright", colors= c("blue"), 
                  labels=c("Water conscious green progressives")) %>% 
  
                  addMeasure() %>% 
                  addResetMapButton()


# So below is the final product for the mapping exercise.


customertype1map
customertype2map

## Lessons from this project went on to inform the CRWP "Save it for the Salmon"
### Along with informing the ongoing Clackamas Basin Resiliency Project.
#### Maps can be used to target neighborhoods for messaging and signage when 
#### resources are scarce.