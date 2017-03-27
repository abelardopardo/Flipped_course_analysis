##############################################################
## USE LCA TO GROUP STUDENTS BASED ON THE PATHS THEY FOLLOWED 
## THROUGHOUT THE COURSE (WEEKS 2-13)
##############################################################

clusters.w213 <- read.csv(file = "Intermediate_files/original_weekly_clusters_w2_to_w13_(feb2016).csv")  

f.fullcourse <- cbind(cl.w2, cl.w3, cl.w4, cl.w5, cl.w6, cl.w7, cl.w8, 
                      cl.w9, cl.w10, cl.w11, cl.w12, cl.w13) ~ 1

eval.metrics <- data.frame()
require(poLCA)
set.seed(3003)
for(i in 3:7) {
  lc <- poLCA(f.fullcourse, data = clusters.w213, nrep = 50, nclass = i, verbose = F, na.rm = F)
  metrics <- c(i, lc$aic, lc$bic, lc$llik, lc$Chisq)
  eval.metrics <- as.data.frame( rbind(eval.metrics, metrics) )
}
colnames(eval.metrics) <- c('nclass', 'AIC', 'BIC', 'LogLike', 'ChiSquare')
require(knitr)
kable(x = eval.metrics, format = "rst")

require(poLCA)
set.seed(3003)
## examine the model with 5 classes
# lc5.fullcourse <- poLCA(f.fullcourse, data = clusters.w213, nrep = 50, 
#                         nclass = 5, graphs = T, na.rm = F)
# # order the latent classes based on their size ($P gives the size each latent class)
# probs.start <- poLCA.reorder(lc5.fullcourse$probs.start, order(lc5.fullcourse$P,decreasing=T))
# lc5.fullcourse <- poLCA(f.fullcourse, data = clusters.w213, graphs = T, 
#                         nclass = 5, probs.start = probs.start, na.rm = F)

## examine the model with 4 classes
# lc4.fullcourse <- poLCA(f.fullcourse, data = clusters.w213, nrep = 50, 
#                         nclass = 4, graphs = T, na.rm = F)
# probs.start <- poLCA.reorder(lc4.fullcourse$probs.start, order(lc4.fullcourse$P,decreasing=T))
# lc4.fullcourse <- poLCA(f.fullcourse, data = clusters.w213, graphs = T, 
#                         nclass = 4, probs.start = probs.start, na.rm = F)

## examine the model with 6 classes
lc6.fullcourse <- poLCA(f.fullcourse, data = clusters.w213, nrep = 50, 
                        nclass = 6, na.rm = F)
probs.start <- poLCA.reorder(lc6.fullcourse$probs.start, order(lc6.fullcourse$P,decreasing=T))
lc6.fullcourse <- poLCA(f.fullcourse, data = clusters.w213, graphs = T, 
                        nclass = 6, probs.start = probs.start, na.rm = F)


## add the predicted class to each observation
clusters.w213.pred <- clusters.w213
# clusters.w213.pred$lca5 <- as.factor(lc5.fullcourse$predclass)
# clusters.w213.pred$lca4 <- as.factor(lc4.fullcourse$predclass)
clusters.w213.pred$lca6 <- as.factor(lc6.fullcourse$predclass)
str(clusters.w213.pred)

## write the features + the LCA class values to a file
write.csv(x = clusters.w213.pred, file = "results/lca_w2_to_w13_6classes(April2016).csv", 
          row.names = F, quote = F)


###########################################################
## TRACE STUDENTS' STRATEGIES (WEEKLY CLUSTERS) THROUGHT ## 
## THE OBTAINED LCA-BASED TRAJECTORIES                   ##
###########################################################

weekly.strategies <- read.csv(file = "results/lca_w2_to_w13_6classes(April2016).csv")
str(weekly.strategies)
weekly.strategies$cl.w2 <- factor(weekly.strategies$cl.w2,
                                  levels = c(1:5), labels = c('A','D','E','B1','C1'))
weekly.strategies$cl.w3 <- factor(weekly.strategies$cl.w3,
                                  levels = c(1:4), labels = c('A','E','D','B2'))
weekly.strategies$cl.w4 <- factor(weekly.strategies$cl.w4,
                                  levels = c(1:5), labels = c('E','C2','B1','A','B2'))
weekly.strategies$cl.w5 <- factor(weekly.strategies$cl.w5,
                                  levels = c(1:4), labels = c('D','B1','E','A'))
weekly.strategies$cl.w6 <- factor(weekly.strategies$cl.w6,
                                  levels = c(1:5), labels = c('C2','C1','E','D','A'))
weekly.strategies$cl.w7 <- factor(weekly.strategies$cl.w7,
                                  levels = c(1:4), labels = c('E','B1','D','C1'))
weekly.strategies$cl.w8 <- factor(weekly.strategies$cl.w8,
                                  levels = c(1:5), labels = c('B1','E','B2','A','D'))
weekly.strategies$cl.w9 <- factor(weekly.strategies$cl.w9,
                                  levels = c(1:4), labels = c('D','A','E','B1'))
weekly.strategies$cl.w10 <- factor(weekly.strategies$cl.w10,
                                  levels = c(1:5), labels = c('B1','A','D','B2','E'))
weekly.strategies$cl.w11 <- factor(weekly.strategies$cl.w11,
                                   levels = c(1:4), labels = c('D','B1','F','E'))
weekly.strategies$cl.w12 <- factor(weekly.strategies$cl.w12,
                                   levels = c(1:4), labels = c('E','F','B1','B2'))
weekly.strategies$cl.w13 <- factor(weekly.strategies$cl.w13,
                                   levels = c(1:5), labels = c('A','E','D','C1','C2'))
weekly.strategies$lca6 <- factor(weekly.strategies$lca6)
str(weekly.strategies)


## select students who had strategy F in weeks 11 and 12
stud.F.w11 <- weekly.strategies$user_id[is.na(weekly.strategies$cl.w11) == F & 
                                          weekly.strategies$cl.w11 == 'F']
stud.F.w12 <- weekly.strategies$user_id[is.na(weekly.strategies$cl.w12) == F & 
                                          weekly.strategies$cl.w12 == 'F']
## check how many students had strategy F in both weeks (11 and 12)
length(intersect(stud.F.w11, stud.F.w12))
## 24 students 

## find the distribution of strategies in weeks 10 and 13 for those students
## who had strategy F in week 11
strat.w9to13.studF.in.w11 <- subset(weekly.strategies, user_id %in% stud.F.w11, 
                                      select = c(cl.w9, cl.w10, cl.w12, cl.w13))
summary(strat.w9to13.studF.in.w11)

## find the distribution of strategies in weeks 10 and 13 for those students
## who had strategy F in week 12
strat.w9to13.studF.in.w12 <- subset(weekly.strategies, user_id %in% stud.F.w12, 
                                    select = c(cl.w9, cl.w10, cl.w11, cl.w13))
summary(strat.w9to13.studF.in.w12)

## distribution of trajectories for students who had strategy F in week 11  
table(subset(weekly.strategies, user_id %in% stud.F.w11, select = lca6))

## distribution of trajectories for students who had strategy F in week 11  
summary(subset(weekly.strategies, user_id %in% stud.F.w12, select = lca6))
