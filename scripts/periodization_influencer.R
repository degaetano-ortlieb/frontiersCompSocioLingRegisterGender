##Script for Periodization 
#Author: Stefania Degaetano-Ortlieb 
#Date: 2018
#Note: builds on Rscript of Peter Fankhauser on comparing corpora
###### Fankhauser et al. 2014, Exploring and Visualizing Variation in Language Resources when publishing smth based on this script
###### to compute distance measures and perform t-test on pairs of corpora represented
###### as (smoothed) unigram language models



#Prepare classes for KLD calculation
#Stefania Degaetano-Ortlieb 30.01.2018
#adapted 08.05.2018, Stefania Degaetano-Ortlieb

#Read in file 
#Input format: Tab delimited matrix with features as columns and text-ids as rows
d4kld <- read.table(choose.files(), sep = "\t", header = T, as.is = T)
#read in data cc_F_gentry_lex_forMatrix_new.freq
#cc_F_gentry_po3_forMatrix_notoutlier.csv

#d4kld[d4kld[-c(0),c(1,3:ncol(d4kld))]] <- as.numeric(as.character(d4kld[-c(0),c(1,3:ncol(d4kld))]))
#d4kld2 <- as.numeric(as.character(d4kld[-c(0),c(1,3:ncol(d4kld))]))
#normalize outliers, i.e. detect statistical outliers and replace them with either the mean or media
#head(d4kld[3,3:ncol(d4kld)])
#clean from absurd datapoints
library(dplyr)
library(tidyverse) 
#df <- select(d4kld, -contains("xx"))
#df <- select(d4kld, -contains("UNC"))
#d4kld <- df
#head(df)
#rownames(df) <- d4kld$id

#Change outlier to mean/median
library(outliers)
#nooutlier <- rm.outlier(df[,3:ncol(df)], fill=T)
#nooutlier <- rm.outlier(df[,3:ncol(df)], fill=T)
#nooutlier <- rm.outlier(d4kld[,3:ncol(d4kld)], fill=T)

#OBC
nooutlier <- rm.outlier(d4kld[,3:ncol(d4kld)], fill=T)
#head(nooutlier)
#head(d4kld)
library(tibble)

#for pos3grams
#Female
#nooutlier <- add_column(nooutlier, year=df$year, .before=1) 
#nooutlier <- add_column(nooutlier, textid=df$textid, .before=1) 
nooutlier <- add_column(nooutlier, gender=d4kld$gender, .before=1) 
nooutlier <- add_column(nooutlier, year=d4kld$year, .before=1) 

#nooutlier <- add_column(nooutlier, textid=d4kld$textid, .before=1) 
#head(d4kld$)
#Male
#nooutlier <- add_column(nooutlier, text_id_year=d4kld$text_id_year, .before=1) 

#for lemma
#nooutlier <- add_column(nooutlier, year=d4kld$year, .before=1) 
#nooutlier <- add_column(nooutlier, id=d4kld$textid, .before=1) 
#nooutlier <- add_column(nooutlier, no=d4kld$no, .before=2) 

#nooutlier <- cbind(d4kld$year, nooutlier)
#nooutlier <- cbind(d4kld$id, nooutlier)
#nooutlier$id <- d4kld$id
#nooutlier$year <- d4kld$year
#head(nooutlier)
#Error
#Error in (function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE,  : 
#                      arguments imply differing number of rows: 419, 418
#Solution:
#You got the error because you don't have the same number of outlier bar variable.
#To correct it you have 2 options :
#put the option fill = TRUE :the mean is placed instead of outlier and not removed
#Remove the oulier by yourself:
# get a list of outlier index for each variable
#ll <- apply(datalog,2,function(x) which(x == outlier(x)))

#write out datafile
#path1 <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\data\\obc\\grammar\\"

#write.table(nooutlier,paste(path1,"cc_F_gentry_morph_forMatrix_notoutlier.csv", sep=""), sep=";", row.names=F, quote=F)
#write.table(nooutlier,paste(path1,"obc_F_pos3_notoutlier.csv", sep=""), sep=";", row.names=F, quote=F)
#write.table(nooutlier,paste(path1,"obc_F_pos3_notoutlier.csv", sep=""), sep=";", row.names=F, quote=F)

#head(nooutlier)
d4kld <- nooutlier

#d4kld <- read.table(choose.files(), sep = "\t", header = T)
#For Lemma in LATECh paper
#rsc37_5yearsperiods1740-1771_forKLD_lemma_new
#For 3pos in Latech paper
#rsc37_1700-1849_3pos_year_min5_noavs.csv

#head(d4kld)
#d4kld <- cbind("id"=1:nrow(d4kld), d4kld)

#d4kld <- subset(d4kld, select=-c(textid))
##docs2 <- subset(docs2, select=-c(id))
#d4kld <- rbind(c(1:ncol(d4kld),d4kld))
#row <- c(1:length(docs))
#Define parameters for periodization
#gap <- 1 #gap for sliding window
#span <- 20  #period range of comparison
#firstp <- 1710 #starting year of FEMALE GENTRY
#range <- 1800 - firstp #range of period to be analyzed
#period <- range/gap  #period to be analyzed

#for small data points
gap <- 1 #gap for sliding window
span <- 20  #period range of comparison
firstp <- 1740 #starting year of FEMALE GENTRY
range <- 1800 - firstp #range of period to be analyzed
period <- range/gap  #period to be analyzed

#Loop over the period to be analyzed
 for(i in 1:period){
   #for all models but the contemporary one
   #cs <- firstp #gap start year
   #ce <- firstp +gap-1 #gap end year 
   #pres <- cs-1 - span #pre-period start
   #pree <- pres +span  #pre-period end
   #poste <- ce+1+span  #post-period start
   #posts <- poste-span #post-period end
   #p <- paste("p",firstp, sep = "") #name for column

   #for a contemporary model
   cs <- firstp #gap start year
   #ce <- firstp +gap-1 #gap end year 
   pres <- cs-10  #pre-period start
   pree <- pres +19  #pre-period end
   #poste <- ce+1+span  #post-period start
   #posts <- poste-span #post-period end
   p <- paste("p",firstp, sep = "") #name for column
   
   #for more classes
   #modeling pre females vs. post males
   #if pre females diverge more but increasingly are more similar to 
   #their post males, then they somehow drive a change
   #contemporary females and males (change to uppercase for Letter corpus)
   d4kld[[p]][d4kld$year>=pres & d4kld$year<=pree & d4kld$gender=="F"]<- "pre"   #insert column and define pre period
   #d4kld[[p]][d4kld$year>=cs & d4kld$year<=ce]<- "gap"       #define gap
   d4kld[[p]][d4kld$year>=pres & d4kld$year<=pree & d4kld$gender=="M"]<- "post" #define post period
   
   #females
   #d4kld[[p]][d4kld$year>=pres & d4kld$year<=pree & d4kld$gender=="f"]<- "pre"   #insert column and define pre period
   #d4kld[[p]][d4kld$year>=cs & d4kld$year<=ce & d4kld$gender=="f"]<- "gap"       #define gap
   #d4kld[[p]][d4kld$year>=posts & d4kld$year<=poste & d4kld$gender=="f"]<- "post" #define post period
  #males
   #d4kld[[p]][d4kld$year>=pres & d4kld$year<=pree & d4kld$gender=="m"]<- "pre"   #insert column and define pre period
   #d4kld[[p]][d4kld$year>=cs & d4kld$year<=ce & d4kld$gender=="m"]<- "gap"       #define gap
   #d4kld[[p]][d4kld$year>=posts & d4kld$year<=poste & d4kld$gender=="m"]<- "post" #define post period
  #pastF vs futureM
   #d4kld[[p]][d4kld$year>=pres & d4kld$year<=pree & d4kld$gender=="f"]<- "pre"   #insert column and define pre period
   #d4kld[[p]][d4kld$year>=cs & d4kld$year<=ce ]<- "gap"       #define gap
   #d4kld[[p]][d4kld$year>=posts & d4kld$year<=poste & d4kld$gender=="m"]<- "post" #define post period
   #futureF vs pastM
   #d4kld[[p]][d4kld$year>=pres & d4kld$year<=pree & d4kld$gender=="m"]<- "pre"   #insert column and define pre period
   #d4kld[[p]][d4kld$year>=cs & d4kld$year<=ce ]<- "gap"       #define gap
   #d4kld[[p]][d4kld$year>=posts & d4kld$year<=poste & d4kld$gender=="f"]<- "post" #define post period
   
   #MMONTAGU influencer?
   #d4kld[[p]][d4kld$year>=pres & d4kld$year<=pree & d4kld$gender=="MMONTAGU"]<- "pre"   #insert column and define pre period
   #d4kld[[p]][d4kld$year>=cs & d4kld$year<=ce ]<- "gap"       #define gap
   #d4kld[[p]][d4kld$year>=posts & d4kld$year<=poste & d4kld$gender=="F"]<- "post" #define post period
   
   
   #baseline
   #d4kld[[p]][d4kld$year>=pres & d4kld$year<=pree]<- "pre"   #insert column and define pre period
   #d4kld[[p]][d4kld$year>=cs & d4kld$year<=ce]<- "gap"       #define gap
   #d4kld[[p]][d4kld$year>=posts & d4kld$year<=poste]<- "post" #define post period
   
   firstp <- firstp +gap #slide over the time line based on gap
   
 }

#d4kld <- subset(d4kld, select=-c(rb3,rdo))

#Set path for output files
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\analyses\\dynamics-of-change\\pos3\\data_latech2018\\pos3\\results\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\analyses\\dynamics-of-change\\pos3\\data_latech2018\\word\\results\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\dissemination\\B1_journal2018\\analysis\\periodization\\data\\lemma\\results\\5year_50span\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\andrew_piper\\analysis\\periodization\\lemma_scitex\\data_compling\\1year_5span\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\andrew_piper\\analysis\\periodization\\coha\\1year_5span\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\pos\\1year_20span_female_nooutlier_morph_noRoyalty\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\obc\\lexis\\1year_20span_female_nooutlier_lexis\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\obc\\grammar\\1year_20span_female_nooutlier_pos3\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\obc\\lexis\\1year_20span_female_nooutlier_wholeperiod\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\pos\\10year_20span_female_nooutlier_pos3_noRoyalty\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\pos\\1year_20span_male_nooutlier_pos3_noRoyalty\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\pos\\all-1y_20s\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\obc\\lexis\\1y20s_MvsM\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\lexis\\1y20s_pastMvsfutureM\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\pos\\all\\futureM_pastM-1y_20s\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\obc\\grammar\\1y20s_MpastvsMfuture\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\obc\\lexis\\1y20s_baseline\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\lexis\\1y20s_baseline\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\obc\\grammar\\1y20s_baseline\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\lexis\\1y20s_pastFvsfutureM-new\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\pos\\all\\1y20s_baseline\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\pos\\all\\1y20s_pastFvsfutureALL\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\data\\obc\\lexis\\test_sameyear\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\data\\obc\\grammar\\test_sameyear\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\data\\pos\\all\\test_sameyear\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\morph\\sameyear20-1-new\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\morph\\FvsF20-1\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\morph\\MvsM20-1\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\morph\\pastFvsfutureM20-1\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\morph\\futureFvspastM20-1\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\obc\\morph\\sameyear20-1\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\obc\\morph\\FvsF20-1\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\obc\\morph\\MvsM20-1\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\obc\\morph\\pastFvsfutureM20-1\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\obc\\morph\\futureFvspastM20-1\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\obc\\morph\\baseline\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\morph\\1year_20span_baseline_nooutlier_new\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\pos\\all\\20year_contemp\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\lexis\\20year_contemp\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\morph\\20year_contemp\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\obc\\morph\\20year_contemp\\"
#path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\obc\\lexis\\20year_contemp\\"
path <- "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\obc\\grammar\\20year_contemp\\"

dir.create(paste(path, sep=""))

#Output file for KLD calculation
#write.table(d4kld, "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Corpora\\new_JSTOR-\\txt_1GRAMS\\merge_rs2\\data_periodization.csv", sep=";", row.names = F, quote = F)
#write.table(d4kld, "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\analyses\\dynamics-of-change\\pos3\\data_latech2018\\word\\rsc37_lemma_forKLD_periods_19Mai_5min_10g_20p_1725-1850.csv", sep=";", row.names = F, quote = F)
#write.table(d4kld,"C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\dissemination\\B1_journal2018\\analysis\\periodization\\data\\lemma\\data_periodization_year_5y30s.csv", sep=";", row.names=F, quote=F)
#write.table(d4kld,"C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\andrew_piper\\analysis\\periodization\\lemma_scitex\\data_compling\\data_periodization_B1_1y5s.csv", sep=";", row.names=F, quote=F)
#write.table(d4kld,paste(path,"data_periodization_lexis_1y20p_maleANDfemale.csv", sep=""), sep=";", row.names=F, quote=F)

#write.table(d4kld,"C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\dissemination\\B1_journal2018\\analysis\\periodization\\data\\pos\\data_periodization_year_10y20s.csv", sep=";", row.names=F, quote=F)
# ESU 2015, Leipzig
# Workshop: Comparing Corpora
# Peter Fankhauser
# compute distance measures and perform t-test on pairs of corpora represented
# as (smoothed) unigram language models

library(tm) # for constructing (sparse) document term matrices
library(slam) # for computing with sparse matrices
library(BSDA) # for t-test based on sample means and standard deviations

#read CQP tabulate
#choose file

#Tab-delimited input format
#docs <- read.table(file.choose(), header=T, fill=T, row.names=NULL, sep="\t")

#Semicolon-delimited input format
#docs <- read.table(file.choose(), header=T, fill=T, row.names=NULL, sep=";")

#Use document prepared for KLD
docs <- d4kld

#Set again starting year
firstp <- 1740



#dir.create(paste(path,"kldsum\\", sep=""))



#################### Preprocessing #########################


#Define columns with metadata (e.g. text-id is metadata) 
#CHANGE
#Last columns are selected based on amount of periods to be analyzed
docs.meta <- docs[,c(1:2,(ncol(docs)-period+1):ncol(docs))] #select from docs (your file) column 1 to 4
head(docs.meta)
#Define columns with data (not metadata)
#first two columns are textid and year of publication
docs.dtm <- as.simple_triplet_matrix(docs[,3:(ncol(docs)-period)]) 




####### end of preprocessing #######

for(i in 1:period){
  
  p <- paste("p",firstp, sep = "")
  
  ######## Define groupindex, i.e. column with class that has to be compared 
  groupindex <- as.factor(docs[[p]])
  
  ######## Define classes (selectors) to be compared  ############
  #CHANGE
  # selectors for subcorpora
  #Comparison is always based on two corpora only
  sel1 <- "pre"
  sel2 <- "post"
  
  ######## Define ling. level of comparison (used for output file)
  #llevel <- "N" #nouns
  #llevel <- "words" #words
  llevel <- "pos3" #POS trigrams

  ######## Define Outputfile location
  #CHANGE to your directory
  #pos3
  file <- paste(path,firstp,sel1,sel2,llevel,".csv", sep = "")
  #lemma
  #file <- paste("C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\analyses\\dynamics-of-change\\lemma_new\\results\\sall",firstp,sel1,sel2,"_sp.csv", sep = "")
  
  ##Prepare for smoothing
  # sum up (absolute) frequencies for each class combination
  docs.group.freq <-rollup(docs.dtm,1L,groupindex,na.rm=T,fun=sum)
  
  # sum up all frequencies  (for smoothing)
  docs.all.freq <-rollup(docs.dtm,1L,fun=sum)
  
  ##### Smoothing to allow comparison of different vocabulary size of (sub)corpora
  # calculate smoothed term probabilities
  # simple smoothing with 0.005 of background corpus
  # see Fankhauser et al. 2014, Exploring and Visualizing Variation in Language Resources
  # and Zhai and Lafferty 2004, A Study of Smoothing Methods for Language Models Applied to Information Retrieval
  # for evalution of smoothing methods with different lambdas
  lambda<-0.995
  
  docs.group.prob <-as.matrix(docs.group.freq/row_sums(docs.group.freq))
  docs.all.prob <-as.matrix(docs.all.freq/row_sums(docs.all.freq))
  
  #smooth probabilities to be comparable if corpora have different vocab size
  docs.group.smoothprob<-sweep(lambda*docs.group.prob,2,as.vector((1-lambda)*docs.all.prob),"+")
  
  ####### Definition of divergence measures ###########
  
  
  # Kullback-Leibler Divergence
  
  kld <- function(x,y) {
    if (y==0 && x > 0) {
      NaN
    }
    else if (x==0) {
      0
    }
    else {
      x*log2(x/y) # kld1 -> D_kl(femaleH||femaleL) = p(femaleH) * log2(p(femaleH)/p(femaleL))
      # kld2 -> D_kl(femaleL||femaleH) = p(femaleL) * log2(p(femaleL)/p(femaleH))
    }
  }
  
  # Jensen-Shannon Divergence
  
  js <- function(x,y) {
    m <- (x+y)/2
    0.5*(kld(x,m)+kld(y,m))
  }
  
  # Jensen-Shannon Divergence ... individual terms
  
  js.left <- function(x,y) {
    m <- (x+y)/2
    kld(x,m)
  }
  
  js.right <- function(x,y) {
    m <- (x+y)/2
    kld(y,m)
  }
  
  # Chisquare
  
  chisquare <- function(o11,o12,o21,o22) {
    sum <- o11+o12+o21+o22
    e11 <- (o11+o12)*(o11+o21)/sum
    e12 <- (o11+o12)*(o12+o22)/sum
    e21 <- (o21+o22)*(o11+o21)/sum
    e22 <- (o21+o22)*(o12+o22)/sum
    (o11-e11)^2/e11+(o12-e12)^2/e12+(o21-e21)^2/e21+(o22-e22)^2/e22
  }
  # apply all divergence measures to a pair of corpora selected by sel1 and sel2
  compute_divergences <- function (sel1,sel2) {
    #smoothed probabilities
    prob1<-docs.group.smoothprob[sel1,,drop=FALSE]
    prob2<-docs.group.smoothprob[sel2,,drop=FALSE]
    #Apply KLD
    kld1 <- mapply(kld,prob1,prob2)
    kld2 <- mapply(kld,prob2,prob1)
    #Apply Jensen-Shannon
    js <- mapply(js,prob1,prob2)
    js.left <- mapply(js.left,prob1,prob2)
    js.right <- mapply(js.right,prob1,prob2)
    #Group frequencies
    freq1 <- as.matrix(docs.group.freq[sel1,])
    freq2 <- as.matrix(docs.group.freq[sel2,])
    #Sum frequencies
    sum1 <- sum(freq1)
    sum2 <- sum(freq2)
    #Rest frequencies 
    nfreq1 <- sum(freq1)-freq1
    nfreq2 <- sum(freq2)-freq2
    #Apply chisquare test
    chisquare <- mapply(chisquare,freq1,freq2,nfreq1,nfreq2)
    pchi <- 1.0-pchisq(chisquare,1)
    #Bind results
    res <- rbind(kld1,kld2,prob1,prob2,js,js.left,js.right,freq1,freq2,chisquare,pchi)
    #Define row and column names
    rownames(res) <- c("kld1","kld2","p1","p2","js","js1","js2","f1","f2","chi","pchi")
    colnames(res) <- colnames(prob1)
    #Build data frame
    data.frame(t(res))
  }
  
  
  #Compute divergences for the selected subcorpora
  divergences <- compute_divergences(sel1,sel2)
  #sorted
  sdivergences <- divergences[order(-divergences$kld2),]
  
  
  
  ###### Significance Testing ######
  
  # T-test
  
  # sparse variants of mean and standard deviation
  # x vector of values, nx number of 0 entries
  
  #Mean
  sparsemean <- function(x,nx) {
    n <- nx+length(x)
    if (n<1) {
      NA
    }
    else {
      sum(x)/n
    }
  }
  
  #Standard deviation
  sparsesd <- function(x,nx) {
    n <- nx+length(x)
    if (n<2) {
      NA
    }
    else {
      sum <- sum(x)
      sqrsum <- sum(x^2)
      sqrt((sqrsum-sum^2/n)/(n-1))
    }
  }
  
  # normalize frequencies by document length (unsmoothed probabilities)
  docs.dtm.norm <- docs.dtm/row_sums(docs.dtm)
  # means
  docs.group.prob.mean<-as.matrix(rollup(docs.dtm.norm,1L,groupindex,EXPAND="sparse",FUN=sparsemean))
  # standard deviations
  docs.group.prob.sd<-as.matrix(rollup(docs.dtm.norm,1L,groupindex,EXPAND="sparse",FUN=sparsesd))
  # number of documents per group
  docs.group.count<-table(groupindex)
  
  tsum.test.wrap <- function (m1,m2,s1,s2,n1,n2) {
    ttest<-tsum.test(m1, s.x=s1, n.x=n1, m2, s.y = s2,
                     n.y = n2, alternative = "two.sided", mu = 0, var.equal = FALSE,
                     conf.level = 0.95)
    ttest$p.value
  }
  
  compute_pvalues <- function(sel1,sel2) {
    means1 <- docs.group.prob.mean[sel1,,drop="FALSE"]
    means2 <- docs.group.prob.mean[sel2,,drop="FALSE"]
    sds1 <- docs.group.prob.sd[sel1,,drop="FALSE"]
    sds2 <- docs.group.prob.sd[sel2,,drop="FALSE"]
    ns1 <- as.vector(docs.group.count[sel1])
    ns2 <- as.vector(docs.group.count[sel2])
    res<-t(mapply(tsum.test.wrap,means1,means2,sds1,sds2,ns1,ns2))
    colnames(res)<-colnames(means1)
    rownames(res)<-c("pvalue")
    t(res)
  }
  
  pvalues <- compute_pvalues(sel1,sel2)
  
  # Combine all and sort/filter by various aspects
  all <- cbind(divergences,pvalues)
  all <- all[!is.nan(all$pvalue),]
  
  #Overall KLD
  kld1S <- sum(all$kld1)
  kld1S #D_kl(femaleH||femaleL)_1700 = 1.27
  
  kld2S <- sum(all$kld2)
  kld2S #D_kl(femaleL||femaleH)_1700 = 1.28
  
  #Difference
  klddiff <- kld1S - kld2S
  klddiff
  okld <- cbind(firstp,kld1S,kld2S,klddiff)

  kldfile <- paste(path,sel1,sel2,llevel,"kldsum.csv", sep = "")
  write.table(okld,kldfile, append = T, row.names=T, sep=";")
  
  #Significant in terms of relative frequency 
  sall <- all[order(-all$kld1),]
  head(sall)
  # filter everything with pvalue > 0.05
  sall <- sall[sall$pvalue<0.05,]
  #chisig <- sall[sall$pchi>0.05,]
  
  
  #WRITE table into directory
  #CHANGE path to your directory!
  #Write out only features showing significant difference 
  write.table(cbind(id=row.names(sall), as.data.frame(sall)), file, row.names=F, sep="\t")
  #write.table(sall, file, sep="\t")
  #Write out all features
  #write.table(all, file, sep=";")
  
  firstp <- firstp +gap #slide over the time line based on gap
}

firstp <- 1710
path = "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\lexis\\1y20s_pastFvsfutureF\\"

#multmerge function to merge data from different files by same column
multmerge = function(mypath){
  filenames=list.files(mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
  Reduce(function(x,y) {merge(x,y, by.x="id", by.y="id", all=T)}, datalist)
}


#POST vs. PRE periods -> increasing change
direction <- "prepost"
dir.create(paste(path,"mergemePOST-",direction, sep=""))
#dir.create(paste(path,"mergemePRE-",direction, sep=""))

for(i in 1:period){
  
  #pos3
  #file <- paste("C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\analyses\\dynamics-of-change\\pos3\\results\\signF\\sall",period,"prepost_NNconf.csv", sep = "")
  #outf <- paste("C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\analyses\\dynamics-of-change\\pos3\\results\\signF\\mergeme_PREvsPOST_NNconf\\sall",period,"prepost_typF.csv", sep = "")
  
  #Define file names/directories
  #file <- paste(path,firstp,direction,llevel,".csv", sep = "")
  file <- paste(path,firstp,direction,llevel,".csv", sep = "")
  
  outf <- paste(path,"mergemePOST-",direction,"\\",firstp,direction,"_typF.csv", sep = "")
  # outf <- paste(path,"mergemePRE-",direction,"\\",firstp,direction,"_typF.csv", sep = "")
  
    #lemma
  #file <- paste("C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\analyses\\dynamics-of-change\\lemma_new\\results\\sall",period,"prepostsp.csv", sep = "")
  #outf <- paste("C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\analyses\\dynamics-of-change\\lemma_new\\results\\mergeme_sp_POSTvsPRE\\sall",period,"prepost_typF.csv", sep = "")
  
  #PRE vs CORE
  d <- read.table(file, sep = "\t", header = T)
  head(d)
  #by kld1 or kld2
  d <- subset(d, select=c("id", "kld2"))
  write.csv(d, outf, row.names = F)
  firstp <- firstp +gap
}

firstp <- 1710
 vcolnames <- c(firstp)
 #change to period-1
 for(i in 1:91){
   firstp <- firstp + gap
   vcolnames <- append(vcolnames, firstp)
 }

 path = "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\lexis\\1y20s_futureFvspastM\\"
 #lexis\\1year_20span_female_nooutlier_new_noUNC_noRoyalty\\"
data = multmerge(paste(path,"mergemePOST-",direction, sep=""))  
# data = multmerge(paste(path,"mergemePRE-",direction, sep=""))  
 
#lemma
colnames(data) <- c("", vcolnames)
#head(data)

write.table(data, paste(path,"mergemePOST-",direction,"\\data",direction,".csv", sep=""), row.names = F, sep=";")

#PRE
firstp <- 1710
dir.create(paste(path,"mergemePRE-",direction, sep=""))

for(i in 1:period){
  
  #pos3
  #file <- paste("C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\analyses\\dynamics-of-change\\pos3\\results\\signF\\sall",period,"prepost_NNconf.csv", sep = "")
  #outf <- paste("C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\analyses\\dynamics-of-change\\pos3\\results\\signF\\mergeme_PREvsPOST_NNconf\\sall",period,"prepost_typF.csv", sep = "")
  
  #Define file names/directories
  #file <- paste(path,firstp,direction,llevel,".csv", sep = "")
  file <- paste(path,firstp,direction,llevel,".csv", sep = "")
  
  #outf <- paste(path,"mergemePOST-",direction,"\\",firstp,direction,"_typF.csv", sep = "")
   outf <- paste(path,"mergemePRE-",direction,"\\",firstp,direction,"_typF.csv", sep = "")
  
  #lemma
  #file <- paste("C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\analyses\\dynamics-of-change\\lemma_new\\results\\sall",period,"prepostsp.csv", sep = "")
  #outf <- paste("C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\analyses\\dynamics-of-change\\lemma_new\\results\\mergeme_sp_POSTvsPRE\\sall",period,"prepost_typF.csv", sep = "")
  
  #PRE vs CORE
  d <- read.table(file, sep = "\t", header = T)
  head(d)
  #by kld1 or kld2
  d <- subset(d, select=c("id", "kld1"))
  write.csv(d, outf, row.names = F)
  firstp <- firstp +gap
}


firstp <- 1740
vcolnames <- c(firstp)
#change to period-1
for(i in 1:91){
  firstp <- firstp + gap
  vcolnames <- append(vcolnames, firstp)
}

path = "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\collaborations\\tanja\\Paper_CompSocioLing_Frontiers\\results\\lexis\\1y20s_futureFvspastM\\"
#lexis\\1year_20span_female_nooutlier_new_noUNC_noRoyalty\\"
#data = multmerge(paste(path,"mergemePOST-",direction, sep=""))  
data = multmerge(paste(path,"mergemePRE-",direction, sep=""))  

#lemma
colnames(data) <- c("", vcolnames)
#head(data)

write.table(data, paste(path,"mergemePRE-",direction,"\\data",direction,".csv", sep=""), row.names = F, sep=";")


##############
install.packages("plotly")
library(plotly)

dplot <- read.table(choose.files(), sep = ";", header = T)
tx - group_by(dplot, )
library(dplyr)


trace_0 <- rnorm(100, mean = 5)
trace_1 <- rnorm(100, mean = 0)
trace_2 <- rnorm(100, mean = -5)
x <- c(1:100)
data <- data.frame(x, trace_0, trace_1, trace_2)

p <- plot_ly(data, x = ~x) %>%
  add_trace(y = ~trace_0, name = 'trace 0',mode = 'lines') %>%
  add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers') %>%
  add_trace(y = ~trace_2, name = 'trace 2', mode = 'markers')


data <- data.frame(x, trace_0, trace_1, trace_2)
p <- plot_ly(dplot, x = "X", y = "the", type= 'scatter', mode= 'lines+markers')


add_trace(y = dplot[,2], name="the", type= 'scatter', mode= 'lines+markers')
  
add_trace( y= dplot[,4], name='x', mode='markers')
  add_trace( y= dplot[,5], name='x', mode='markers')
  layout(title = "KLD",
         xaxis = list(title = "Time"),
         yaxis = list (title = "KLD values"))
  plot_ly(txhousing, x = ~date, y = ~median) %>%
    add_lines(color = ~city, colors = "black", alpha = 0.2)
############
#POST vs. PRE periods -> decreasing change
direction <- "post2pre"
for(i in 1:period){
  
  #pos3
  #file <- paste("C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\analyses\\dynamics-of-change\\pos3\\results\\signF\\sall",period,"prepost_NNconf.csv", sep = "")
  #outf <- paste("C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\analyses\\dynamics-of-change\\pos3\\results\\signF\\mergeme_PREvsPOST_NNconf\\sall",period,"prepost_typF.csv", sep = "")
  
  #Define file names/directories
  file <- paste(path,firstp,direction,"_NNconf.csv", sep = "")
  outf <- paste(path,firstp,direction,"_typF.csv", sep = "")
  #lemma
  #file <- paste("C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\analyses\\dynamics-of-change\\lemma_new\\results\\sall",period,"prepostsp.csv", sep = "")
  #outf <- paste("C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\analyses\\dynamics-of-change\\lemma_new\\results\\mergeme_sp_POSTvsPRE\\sall",period,"prepost_typF.csv", sep = "")
  
  #PRE vs CORE
  d <- read.table(file, sep = ";", header = T)
  head(d)
  #by kld1 or kld2
  d <- subset(d, select=c(X, kld2))
  write.csv(d, outf, row.names = F)
}



#lemma
data = multmerge("C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\analyses\\dynamics-of-change\\lemma_new\\results\\mergeme_sp_PREvsPOST")
colnames(data) <- c("", periods)
head(data)
write.table(data, "C:\\Users\\Stefania\\Documents\\Stefania\\Dokumente\\Uni\\Saarbruecken\\SFB\\analyses\\dynamics-of-change\\lemma_new\\results\\mergeme_sp_POSTvsPRE\\data_POSTvsPRE_sptest.csv", row.names = F, sep=";")


