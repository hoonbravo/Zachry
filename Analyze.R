{
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(xUCINET)
library(sna)
library(igraph)
library(ggplot2)
library(sna)
library(mediation)
library(lm.beta)
library(xlsx)
library(ergm)
}

# Remove all objects from the workspace
# rm(list = ls())
{
# Convert the data frames to numeric matrices, excluding the first column
pb <- apply(as.matrix(read_excel("Responses_Final.xlsx", sheet="problem_solving"))[,-1], 2, as.numeric)
ad <- apply(as.matrix(read_excel("Responses_Final.xlsx", sheet="advice_seeking"))[,-1], 2, as.numeric)
sc <- apply(as.matrix(read_excel("Responses_Final.xlsx", sheet="socialization"))[,-1], 2, as.numeric)
en <- apply(as.matrix(read_excel("Responses_Final.xlsx", sheet="receiving_energy"))[,-1], 2, as.numeric)
le <- apply(as.matrix(read_excel("Responses_Final.xlsx", sheet="leadership_emergence"))[,-1], 2, as.numeric)

# Make Attribute Data
at <- read_excel("Responses_Final.xlsx", sheet="Attribute")
at <- at[, (ncol(at)-2):ncol(at)]
at$response <- ifelse(at$Growth_Mindset == 0 & at$Emotional_Intelligence == 0, 0, 1)

# at$EI_cent <- scale(at$Emotional_Intelligence)[,1]
# at$GM_cent <- scale(at$Growth_Mindset)[,1]

## advice sending: original data from Qualtrics
# ad_s <- (apply(as.matrix(read_excel("Responses_Final.xlsx", sheet="advice_seeking"))[,-1], 2, as.numeric))

## Removing non-response data
{
keep_idx <- which(at$response == 1)

at_filtered <- at[keep_idx, ]
pb_filtered <- pb[keep_idx, keep_idx]
ad_filtered <- ad[keep_idx, keep_idx]
sc_filtered <- sc[keep_idx, keep_idx]
en_filtered <- en[keep_idx, keep_idx]
le_filtered <- le[keep_idx, keep_idx]

pb <- pb_filtered
ad <- ad_filtered
sc <- sc_filtered
en <- en_filtered
le <- le_filtered
at <- at_filtered

rm(ad_filtered, at_filtered, en_filtered, le_filtered, pb_filtered, sc_filtered)
}

# # Fill NA with non response data
# {
# na_idx <- which(at$response == 0)

# pb[na_idx, ] <- NA
# pb[, na_idx] <- NA
# ad[na_idx, ] <- NA
# ad[, na_idx] <- NA
# sc[na_idx, ] <- NA
# sc[, na_idx] <- NA
# en[na_idx, ] <- NA
# en[, na_idx] <- NA
# le[na_idx, ] <- NA
# le[, na_idx] <- NA
# }
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

# Sum each column of the 'le' dataframe --> leadership emergence
at$le <- colSums(le, na.rm = TRUE)
at$pb <- colSums(pb, na.rm = TRUE)
at$ad <- colSums(ad, na.rm = TRUE)
at$sc <- colSums(sc, na.rm = TRUE)
at$en <- colSums(en, na.rm = TRUE)
# at$ad_s <- colSums(ad_s, na.rm = TRUE)

## Yule's Q: I chose Yule's Q than EI since Female:Male = 19:13 (60%:40%)
at$YulesQ_pb <- xEgoAlterSimilarityCat(pb, at$Gender)[,11]
at$YulesQ_ad <- xEgoAlterSimilarityCat(ad, at$Gender)[,11]
at$YulesQ_sc <- xEgoAlterSimilarityCat(sc, at$Gender)[,11]
at$YulesQ_en <- xEgoAlterSimilarityCat(en, at$Gender)[,11]
# xEgoAlterSimilarityCat(xTranspose(pb), at$Gender) ## to check whether transpose
# EI_pb <- xEgoAlterSimilarityCat(pb, at$Gender)[,8]

## BETWEENNESS CENTRALITY (before Centering)
at$pb_bw_b <- xBetweennessCentrality(pb)[,2]
at$ad_bw_b <- xBetweennessCentrality(ad)[,2]
at$sc_bw_b <- xBetweennessCentrality(sc)[,2]
at$en_bw_b <- xBetweennessCentrality(en)[,2]
# at$ad_s_bw_b <- xBetweennessCentrality(ad_s)[,2]

## EIGENVECTOR CENTALITY
at$pb_ei_b <- xEigenvectorCentrality(pb)[,2]
at$ad_ei_b <- xEigenvectorCentrality(ad)[,2]
at$sc_ei_b <- xEigenvectorCentrality(sc)[,2]
at$en_ei_b <- xEigenvectorCentrality(en)[,2]
# at$ad_s_ei_b <- xEigenvectorCentrality(ad_s)[,2]

## CLOSENESS CENTALITY
at$pb_cl_b <- xClosenessCentrality(pb)[,2]
at$ad_cl_b <- xClosenessCentrality(ad)[,2]
at$sc_cl_b <- xClosenessCentrality(sc)[,2]
at$en_cl_b <- xClosenessCentrality(en)[,2]
# at$ad_s_cl_b <- xClosenessCentrality(ad_s)[,2]

## Centering
at$pb_bw <- scale(at$pb_bw_b)[,1]
at$ad_bw <- scale(at$ad_bw_b)[,1]
at$sc_bw <- scale(at$sc_bw_b)[,1]
at$en_bw <- scale(at$en_bw_b)[,1]

at$pb_ei <- scale(at$pb_ei_b)[,1]
at$ad_ei <- scale(at$ad_ei_b)[,1]
at$sc_ei <- scale(at$sc_ei_b)[,1]
at$en_ei <- scale(at$en_ei_b)[,1]
# at$ad_s_ei <- scale(at$ad_s_ei_b)[,1]

at$pb_cl <- scale(at$pb_cl_b)[,1]
at$ad_cl <- scale(at$ad_cl_b)[,1]
at$sc_cl <- scale(at$sc_cl_b)[,1]
at$en_cl <- scale(at$en_cl_b)[,1]
# at$ad_s_cl <- scale(at$ad_s_cl_b)[,1]

at$le_cent <- scale(at$le)[,1]

# Gender, GM, EI Matrice
gender_matrix <- outer(at$Gender, at$Gender, function(x, y) as.integer(x == y))
diag(gender_matrix) <- 0
GM_diff <- outer(at$Growth_Mindset, at$Growth_Mindset, FUN = function(i, j) i - j)  # i = sender, j = receiver
EI_diff <- outer(at$Emotional_Intelligence, at$Emotional_Intelligence, FUN = function(i, j) i - j)  # i = sender, j = receiver
}

# For ERGM
{
le_ergm <-as.network(le, directed = TRUE)
le_ergm %v% "gender" <-at$Gender
le_ergm %v% "GM" <- at$Growth_Mindset
le_ergm %v% "EI" <- at$Emotional_Intelligence

pb_ergm <-as.network(pb, directed = TRUE)
pb_ergm %v% "gender" <-at$Gender
pb_ergm %v% "GM" <- at$Growth_Mindset
pb_ergm %v% "EI" <- at$Emotional_Intelligence

ad_ergm <-as.network(ad, directed = TRUE)
ad_ergm %v% "gender" <-at$Gender
ad_ergm %v% "GM" <- at$Growth_Mindset
ad_ergm %v% "EI" <- at$Emotional_Intelligence

sc_ergm <-as.network(sc, directed = TRUE)
sc_ergm %v% "gender" <-at$Gender
sc_ergm %v% "GM" <- at$Growth_Mindset
sc_ergm %v% "EI" <- at$Emotional_Intelligence

en_ergm <-as.network(en, directed = TRUE)
en_ergm %v% "gender" <-at$Gender
en_ergm %v% "GM" <- at$Growth_Mindset
en_ergm %v% "EI" <- at$Emotional_Intelligence
}


## ERGM
summary(ergm(le_ergm ~ edges + 
              # mutual + nodefactor("gender", levels = 2) + nodematch("gender") +
              edgecov(GM_diff) + nodeicov("GM") + 
              edgecov(pb_ergm) + edgecov(ad_ergm) + edgecov(sc_ergm) + edgecov(en_ergm) +
              gwidegree(0.8, fixed = TRUE) +
              dgwesp(type="OTP", decay = .5, fixed = TRUE) + # Transitivity: a-->b, b-->c, a-->c
              dgwesp(type="ITP", decay = .5, fixed = TRUE) + # Cyclicality: a-->b, b-->c, c-->a
              edgecov(EI_diff) + nodeicov("EI")
              # nodeicov("GM") + nodeocov("GM") + # https://www.rdocumentation.org/packages/ergm/versions/4.8.1/topics/nodecov-ergmTerm
              # nodeicov("EI") + nodeocov("EI") + # https://www.rdocumentation.org/packages/ergm/versions/4.8.1/topics/nodecov-ergmTerm
              ))

summary(ergm(pb_ergm ~ edges + mutual +
              edgecov(GM_diff) + edgecov(EI_diff) +
              gwidegree(0.8, fixed = TRUE) +
              dgwesp(type="OTP", decay = .5, fixed = TRUE) + # Transitivity: a-->b, b-->c, a-->c
              dgwesp(type="ITP", decay = .5, fixed = TRUE) + # Cyclicality: a-->b, b-->c, c-->a
              nodeicov("GM") + nodeicov("EI") +
              nodefactor("gender", levels = 2) + nodematch("gender")))

summary(ergm(ad_ergm ~ edges + mutual +
              edgecov(GM_diff) + edgecov(EI_diff) +
              gwidegree(0.8, fixed = TRUE) +
              dgwesp(type="OTP", decay = .5, fixed = TRUE) + # Transitivity: a-->b, b-->c, a-->c
              dgwesp(type="ITP", decay = .5, fixed = TRUE) + # Cyclicality: a-->b, b-->c, c-->a
              nodeicov("GM") + nodeicov("EI") +
              nodefactor("gender", levels = 2) + nodematch("gender")))

summary(ergm(sc_ergm ~ edges + mutual +
              edgecov(GM_diff) + edgecov(EI_diff) +
              # gwidegree(0.8, fixed = TRUE) + 
              # dgwesp(type="OTP", decay = .5, fixed = TRUE) + # Transitivity: a-->b, b-->c, a-->c
              # dgwesp(type="ITP", decay = .5, fixed = TRUE) + # Cyclicality: a-->b, b-->c, c-->a
              nodeicov("GM") + nodeicov("EI") +
              nodefactor("gender", levels = 2) + nodematch("gender")))

summary(ergm(en_ergm ~ edges + mutual +
              edgecov(GM_diff) + edgecov(EI_diff) +
              # gwidegree(0.8, fixed = TRUE) + 
              # dgwesp(type="OTP", decay = .5, fixed = TRUE) + # Transitivity: a-->b, b-->c, a-->c
              # dgwesp(type="ITP", decay = .5, fixed = TRUE) + # Cyclicality: a-->b, b-->c, c-->a
              nodeicov("GM") + nodeicov("EI") +
              nodefactor("gender", levels = 2) + nodematch("gender")))

# Descriptive
mean(at$le) #7
sd(at$le) #4.8
xDensity(le) #.22
xReciprocity(le) #.20 low

mean(at$ad) #6
sd(at$ad) #3.7
xDensity(ad) # .2
xReciprocity(ad) # .32

mean(at$pb) #6
sd(at$pb) #3.7
xDensity(pb) #.2
xReciprocity(pb) #.30

mean(at$sc) #13
sd(at$sc) #3.3
xDensity(sc) #.45 high density and reciprocity
xReciprocity(sc) #.60

mean(at$en) #10
sd(at$en) #3.6
xDensity(en) #.32
xReciprocity(en) #.47

# Visualization
par(mar = c(0, 0, 0, 0)) # Set margins to zero for better visualization
gplot(ad,
       gmode = "digraph", #directed
       displaylabels = TRUE,
       label.cex = 0.8,
       label.pos = 5, #center of the node
       vertex.col = 3-at$response, #color by response, green = no response
       edge.col = "gray",
       vertex.cex = at$le * .18, #node size
       edge.lwd = 1, #line width
)
gplot(pb,
       gmode = "digraph", #directed
       displaylabels = TRUE,
       label.cex = 0.8,
       label.pos = 5, #center of the node
       vertex.col = 3-at$response, #color by response, green = no response
       edge.col = "gray",
       vertex.cex = at$le * .18, #node size
       edge.lwd = 1, #line width
)
gplot(sc,
       gmode = "digraph", #directed
       displaylabels = TRUE,
       label.cex = 0.8,
       label.pos = 5, #center of the node
       vertex.col = 3-at$response, #color by response, green = no response
       edge.col = "gray",
       vertex.cex = at$le * .18, #node size
       edge.lwd = 1, #line width
)
gplot(en,
       gmode = "digraph", #directed
       displaylabels = TRUE,
       label.cex = 0.8,
       label.pos = 5, #center of the node
       vertex.col = 3-at$response, #color by response, green = no response
       edge.col = "gray",
       vertex.cex = at$le * .18, #node size
       edge.lwd = 1, #line width
)
gplot(en,
       gmode = "digraph", #directed
       displaylabels = TRUE,
       label.cex = 0.8,
       label.pos = 5, #center of the node
       vertex.col = 3-at$response, #color by response, green = no response
       edge.col = "gray",
       vertex.cex = at$en * .18, #node size
       edge.lwd = 1, #line width
)



## PREVIOUS CODES TO REFER
######################################

# # Correlation between Yule's Q and LE
# cor.test(at$YulesQ_pb[!is.nan(at$YulesQ_pb)], at$le[!is.nan(at$YulesQ_pb)])
# cor.test(at$YulesQ_ad[!is.nan(at$YulesQ_ad)], at$le[!is.nan(at$YulesQ_ad)])
# cor.test(at$YulesQ_sc[!is.nan(at$YulesQ_sc)], at$le[!is.nan(at$YulesQ_sc)])
# cor.test(at$YulesQ_en[!is.nan(at$YulesQ_en)], at$le[!is.nan(at$YulesQ_en)])
# xQAPCorrelation(gender_matrix, le, NPERM = 1000, Directed = TRUE)[7]
# xQAPCorrelation(gender_matrix, pb, NPERM = 1000, Directed = TRUE)[7]
# xQAPCorrelation(gender_matrix, ad, NPERM = 1000, Directed = TRUE)[7]
# xQAPCorrelation(gender_matrix, sc, NPERM = 1000, Directed = TRUE)[7]
# xQAPCorrelation(gender_matrix, en, NPERM = 1000, Directed = TRUE)[7]

# xQAPCorrelation(pb, le, NPERM = 1000, Directed = TRUE)[7]
# xQAPCorrelation(xStructuralEquivalence(pb), le, NPERM = 1000, Directed = TRUE)[7]
# xQAPCorrelation(pb, le, NPERM = 1000, Directed = TRUE)$ClassicSign_2tailed

# ## QAP
xQAPCorrelation(pb, le, NPERM = 1000, Directed = TRUE)[7]
xQAPCorrelation(ad, le, NPERM = 1000, Directed = TRUE)[7]
xQAPCorrelation(sc, le, NPERM = 1000, Directed = TRUE)[7]
xQAPCorrelation(en, le, NPERM = 1000, Directed = TRUE)[7]
xQAPCorrelation(ad, pb, NPERM = 1000, Directed = TRUE)[7]
xQAPCorrelation(sc, pb, NPERM = 1000, Directed = TRUE)[7]
xQAPCorrelation(en, pb, NPERM = 1000, Directed = TRUE)[7]
xQAPCorrelation(sc, ad, NPERM = 1000, Directed = TRUE)[7]
xQAPCorrelation(en, ad, NPERM = 1000, Directed = TRUE)[7]
xQAPCorrelation(en, sc, NPERM = 1000, Directed = TRUE)[7]

xQAPCorrelation(gender_matrix, le, NPERM = 1000, Directed = TRUE)[7]
xQAPCorrelation(gender_matrix, pb, NPERM = 1000, Directed = TRUE)[7]
xQAPCorrelation(gender_matrix, ad, NPERM = 1000, Directed = TRUE)[7]
xQAPCorrelation(gender_matrix, sc, NPERM = 1000, Directed = TRUE)[7]
xQAPCorrelation(gender_matrix, en, NPERM = 1000, Directed = TRUE)[7]

xQAPCorrelation(GM_diff, le, NPERM = 1000, Directed = TRUE)[7]
xQAPCorrelation(GM_diff, pb, NPERM = 1000, Directed = TRUE)[7]
xQAPCorrelation(GM_diff, ad, NPERM = 1000, Directed = TRUE)[7]
xQAPCorrelation(GM_diff, sc, NPERM = 1000, Directed = TRUE)[7]
xQAPCorrelation(GM_diff, en, NPERM = 1000, Directed = TRUE)[7]

xQAPCorrelation(EI_diff, le, NPERM = 1000, Directed = TRUE)[7]
xQAPCorrelation(EI_diff, pb, NPERM = 1000, Directed = TRUE)[7]
xQAPCorrelation(EI_diff, ad, NPERM = 1000, Directed = TRUE)[7]
xQAPCorrelation(EI_diff, sc, NPERM = 1000, Directed = TRUE)[7]
xQAPCorrelation(EI_diff, en, NPERM = 1000, Directed = TRUE)[7]
# xQAPCorrelation(ad_s, le, NPERM = 1000, Directed = TRUE)
xQAPRegression(le, list(pb, ad, sc, en, gender_matrix, GM_diff, EI_diff), NPERM = 1000)
xQAPLogisticRegression(le, list(pb, ad, sc, en), NPERM = 1000)

# # function not exists: xQAPLogisticRegression(le, list(pb, ad_s, sc, en), NPERM = 1000)

# ## H1 (EI -> LE)
# summary(lm(le_cent ~ EI_cent, data = at))

# ## H2 (EI --> Centrality)
# summary(lm(pb_bw ~ EI_cent, data = at))
# summary(lm(ad_bw ~ EI_cent, data = at))
# summary(lm(sc_bw ~ EI_cent, data = at))
# summary(lm(en_bw ~ EI_cent, data = at))

# summary(lm(pb_ei ~ EI_cent, data = at))
# summary(lm(ad_ei ~ EI_cent, data = at))
# summary(lm(sc_ei ~ EI_cent, data = at))
# summary(lm(en_ei ~ EI_cent, data = at))
# summary(lm(ad_s_ei ~ EI_cent, data = at))

# summary(lm(pb_cl ~ EI_cent, data = at))
# summary(lm(ad_cl ~ EI_cent, data = at))
# summary(lm(sc_cl ~ EI_cent, data = at))
# summary(lm(en_cl ~ EI_cent, data = at))
# summary(lm(ad_s_cl ~ EI_cent, data = at))

# ## H3 (Centrality --> LE)
# # Betweenness Centrality Regressions
# summary(lm(le_cent ~ pb_bw, data = at))
# summary(lm(le_cent ~ ad_bw, data = at))
# summary(lm(le_cent ~ sc_bw, data = at))
# summary(lm(le_cent ~ en_bw, data = at))
# summary(lm(le_cent ~ pb_bw + ad_bw + sc_bw + en_bw, data = at))

# # Eigenvector Centrality Regressions
# summary(lm(le_cent ~ pb_ei, data = at))
# summary(lm(le_cent ~ ad_ei, data = at))
# summary(lm(le_cent ~ sc_ei, data = at))
# summary(lm(le_cent ~ en_ei, data = at))
# summary(lm(le_cent ~ ad_s_ei, data = at))
# summary(lm(le_cent ~ pb_ei + ad_ei + sc_ei + en_ei + ad_s_ei, data = at))

# # Closeness Centrality Regressions
# summary(lm(le_cent ~ pb_cl, data = at))
# summary(lm(le_cent ~ ad_cl, data = at))
# summary(lm(le_cent ~ sc_cl, data = at))
# summary(lm(le_cent ~ en_cl, data = at))
# summary(lm(le_cent ~ ad_s_cl, data = at))
# summary(lm(le_cent ~ pb_cl + ad_cl + sc_cl + en_cl + ad_s_cl, data = at))

# ## H4 (Mediation)
# process(data=at,y="le_cent",x="EI_cent",m="pb_bw",model=4)
# process(data=at,y="le_cent",x="EI_cent",m="ad_bw",model=4)
# process(data=at,y="le_cent",x="EI_cent",m="sc_bw",model=4)
# process(data=at,y="le_cent",x="EI_cent",m="en_bw",model=4)
# process(data=at,y="le_cent",x="EI_cent",m="ad_bw",model=4, cov = "Gender")

# process(data=at,y="le_cent",x="EI_cent",m="pb_ei",model=4)
# process(data=at,y="le_cent",x="EI_cent",m="ad_ei",model=4)
# process(data=at,y="le_cent",x="EI_cent",m="sc_ei",model=4)
# process(data=at,y="le_cent",x="EI_cent",m="en_ei",model=4)
# process(data=at,y="le_cent",x="EI_cent",m="ad_s_ei",model=4)
# process(data=at,y="le_cent",x="EI_cent",m="ad_ei",model=4,  cov = "Gender")

# process(data=at,y="le_cent",x="EI_cent",m="pb_cl",model=4)
# process(data=at,y="le_cent",x="EI_cent",m="ad_cl",model=4)
# process(data=at,y="le_cent",x="EI_cent",m="sc_cl",model=4)
# process(data=at,y="le_cent",x="EI_cent",m="en_cl",model=4)
# process(data=at,y="le_cent",x="EI_cent",m="ad_s_cl",model=4)
# process(data=at,y="le_cent",x="EI_cent",m="ad_cl",model=4, cov = "Gender")

# ### Additional Functions from the Book
# ## Ch.8 == Node-level
# xTieComposition(ad) ## invalid
# xValuedTieComposition(ad) ## non-valued
# IQV_gender <- xAlterCompositionCat(ad,at$Gender)[,10]
# Yule_Q <- xEgoAlterSimilarityCat(ad,at$Gender)[,11]

# ## Leadership Emergence and Gender
# summary(lm(at$le ~ IQV_gender))
# summary(lm(at$le ~ Yule_Q))

# xStructuralHoles(ad) ## not for directed
# xegonetstructure(ad) ## no function

# ## Ch. 9 == Centralities
# ## Ch. 10 == Group-level
# xDensity(ad)
# xReciprocity(ad)
# xTransitivity(ad)
# xCyclicality(ad)

# xComponents(ad)
# xConnectedness(ad)
# xCompactness(ad)
# xCentralization(ad) ## matrix?

# ## Ch. 11 == Subgroups
# xcluques(ad) ## no function
# xCliquesOverlap(ad)
# xCliquesMembership(ad)
# cl <- xCliquesCoMembership(ad)
# xGirvanNewman(ad)
# xFastGreedy(ad)
# xWalkTrap(ad)
# xLouvainMethod(ad)
# xLabelPropagation(ad)

# xGirvanNewman(le)
# xFastGreedy(le)
# xWalkTrap(le)
# xLouvainMethod(le)
# xLabelPropagation(le)

# ## Ch 12 == Equivalence
# se <- xStructuralEquivalence(ad)
# xHierarchicalClustering(ad, Input="Similarities")
# xBlockmodelOptimizing(pb,NOG=2,BlockTypes=c("nul","com"),Options="Binary")

# xCorePeriphery(ad)
# write.xlsx(at,"at.xlsx")
# ## Ch. 13 == Two-mode

# ## Ch. 14 == Inferential Statistics
# ### QAP, autoregression
# ## QAP Correlation, p.285
# # xQAPCorrelation(Padgett_FlorentineFamilies$Marriage, Padgett_FlorentineFamilies$Business)
# # Padgett_FlorentineFamilies$Marriage
# # PFM_Degree<-sna::degree(Padgett_FlorentineFamilies$Marriage)
