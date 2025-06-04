library(readxl)
library(xUCINET)
library(sna)

## Loading the Network Data
pb <- apply(as.matrix(read_excel("Responses_Final.xlsx", sheet="problem_solving"))[,-1], 2, as.numeric)
ad <- apply(as.matrix(read_excel("Responses_Final.xlsx", sheet="advice_seeking"))[,-1], 2, as.numeric)
sc <- apply(as.matrix(read_excel("Responses_Final.xlsx", sheet="socialization"))[,-1], 2, as.numeric)
en <- apply(as.matrix(read_excel("Responses_Final.xlsx", sheet="receiving_energy"))[,-1], 2, as.numeric)
le <- apply(as.matrix(read_excel("Responses_Final.xlsx", sheet="leadership_emergence"))[,-1], 2, as.numeric)

## Column name from anonymized to 1:32
colnames(pb) <- 1:nrow(pb)
colnames(ad) <- 1:nrow(ad)
colnames(sc) <- 1:nrow(sc)
colnames(en) <- 1:nrow(en)
# colnames(ad_s) <- 1:32
colnames(le) <- 1:nrow(le)

rownames(pb) <- 1:nrow(pb)
rownames(ad) <- 1:nrow(ad)
rownames(sc) <- 1:nrow(sc)
rownames(en) <- 1:nrow(en)
# rownames(ad_s) <- 1:32
rownames(le) <- 1:nrow(le)

# Make Attribute Data
at <- read_excel("Responses_Final.xlsx", sheet="Attribute")
at <- at[, (ncol(at)-2):ncol(at)]
at$response <- ifelse(at$Growth_Mindset == 0 & at$Emotional_Intelligence == 0, 0, 1)

# Centering
at$EI_cent <- scale(at$Emotional_Intelligence)[,1]
at$GM_cent <- scale(at$Growth_Mindset)[,1]
at$le_cent <- scale(at$le)[,1]

# Add this vector as a new column to the 'at' dataframe
at$le <- colSums(le, na.rm = TRUE)
at$pb <- colSums(pb, na.rm = TRUE)
at$ad <- colSums(ad, na.rm = TRUE)
at$sc <- colSums(sc, na.rm = TRUE)
at$en <- colSums(en, na.rm = TRUE)

## Yule's Q: I chose Yule's Q than EI since Female:Male = 19:13 (60%:40%)
at$YulesQ_pb <- xEgoAlterSimilarityCat(pb, at$Gender)[,11]
at$YulesQ_ad <- xEgoAlterSimilarityCat(ad, at$Gender)[,11]
at$YulesQ_sc <- xEgoAlterSimilarityCat(sc, at$Gender)[,11]
at$YulesQ_en <- xEgoAlterSimilarityCat(en, at$Gender)[,11]

at <- as.data.frame(at)
# Gender Matrix
gender_matrix <- outer(at$Gender, at$Gender, function(x, y) as.integer(x == y))
diag(gender_matrix) <- 0


Zachry <-xCreateProject(GeneralDescription="Zachry Junior level Students Network Data",
                              NetworkName="pb",
                              NETFILE=pb,
                              FileType="Robject",
                              InFormatType="AdjMat",
                              NetworkDescription="Problem Solving Network",
                              Mode=c("People"),
                              Directed=TRUE, 
                              Loops=FALSE,
                              Values="Binary",
                              Class="matrix",
                              References="No references")


Zachry <- xAddAttributesToProject(ProjectName = Zachry,
                                  ATTFILE1 = at,
                                  FileType = "Robject",
                                  Mode = c("People"),
                                  AttributesDescription = colnames(at))
                                  
Zachry <- xAddToProject(ProjectName = Zachry,
                                  NetworkName = "ad",
                                  NETFILE = ad,
                                  FileType = "Robject",
                                  InFormatType = "AdjMat",
                                  NetworkDescription = "Advice Seeking Network",
                                  Mode = c("People"),
                                  Directed = TRUE, 
                                  Loops = FALSE,
                                  Values = "Binary",
                                  Class = "matrix")
Zachry <- xAddToProject(ProjectName = Zachry,
                                  NetworkName = "sc",
                                  NETFILE = sc,
                                  FileType = "Robject",
                                  InFormatType = "AdjMat",
                                  NetworkDescription = "Socialization Network",
                                  Mode = c("People"),
                                  Directed = TRUE, 
                                  Loops = FALSE,
                                  Values = "Binary",
                                  Class = "matrix")
Zachry <- xAddToProject(ProjectName = Zachry,
                                  NetworkName = "en",
                                  NETFILE = en,
                                  FileType = "Robject",
                                  InFormatType = "AdjMat",
                                  NetworkDescription = "Receiving Energy Network",
                                  Mode = c("People"),
                                  Directed = TRUE, 
                                  Loops = FALSE,
                                  Values = "Binary",
                                  Class = "matrix")
Zachry <- xAddToProject(ProjectName = Zachry,
                                  NetworkName = "le",
                                  NETFILE = le,
                                  FileType = "Robject",
                                  InFormatType = "AdjMat",
                                  NetworkDescription = "Leadership Emergence Network",
                                  Mode = c("People"),
                                  Directed = TRUE, 
                                  Loops = FALSE,
                                  Values = "Binary",
                                  Class = "matrix")

le <- Zachry$le
summary(ergm(le ~ edges + mutual + edgecov(ad_ergm) + nodefactor("gender", levels = 2) + nodematch("gender")))
d