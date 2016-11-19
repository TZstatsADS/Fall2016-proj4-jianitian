setwd("/Users/jianitian/Desktop/Project4")

library(cluster)

##read data
load("lyr.RData")
### delete number 2 3 and 6:30 column
lyr <- lyr[,-c(2,3,6:30)]

source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
library(rhdf5)

###########################cluster according to the analysis features##########################

########## loop to get all the songs features#######
feature.list <- list()

barstart_avgtime <- list()
barstart_sdtime <- list()
barstart_length <- list()
barstart_time <- list()
bar_confidence_mean <- list()

beatstart_avgtime <- list()
beatstart_sdtime <- list()
beatstart_length <- list()
beatstart_time <- list()
beat_confidence_mean <-list()

sectioncenter <- list()
sectionmean <- list()
sectionsd <- list()
section_length <- list()
section_confidence_mean <-list()

segment_confidence_mean <-list()
segment_length <- list()
segment_avgleng <- list()

loudness_quantile <- list()
loudness_range <- list()
loudness_mean <- list()
loudness_sd <- list()
loudness_trend <- list()
loudness_loud_timeline <- list()
loudness_timeline <- list()

pitches <- list()
pitch_mean <- list()
pitch_quantile <- list()

timber <- list()
timbre_mean <- list()
timbre_quantile <-list()

tatum_confidence_mean <- list()
tatum_avgtime<-list()
tatum_sdtime <- list()

filenames<-list.files(path = "/Users/jianitian/Desktop/Project4/data",pattern = ".h5",all.files = TRUE,
           full.names = TRUE,recursive = TRUE, include.dirs = TRUE)

### firstly I choose 350 songs as test data, the rest 2000 songs as train data
set.seed(100)
test_files_names <- sample(filenames,350,replace = FALSE,prob = NULL)
test_files_names <- list.files(path = "/Users/jianitian/Desktop/Project4/TestSongFile100",pattern = ".h5",all.files = TRUE,
                               full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
train_filenames <- filenames[!filenames %in% test_files_names]

### get features 
for (i in 1:length(train_filenames)){
  feature.list [[i]]<- h5read(train_filenames[[i]],"/analysis")
  thisfile <- feature.list[[i]]
  
  ### The difference between each bar and beats are almost the same
  ### we can take average time between each bar and beats 
  ### as features to represent the rhythm of a song;
  ### additionally, the length of the bar and beats can represent the length of a song 
  ### that is also a feature of a song
  
  ### Also, if I want to put all the features together, I neet to give them a weight
  ### Let their related confidence average be their weight

  barstart_length[[i]] <- length(thisfile$bars_start)
  barstart_avgtime[[i]]<-mean(diff(thisfile$bars_start))
  barstart_sdtime[[i]] <- sd(diff(thisfile$bars_start))
  barstart_time[[i]] <- thisfile$bars_start[1]
  bar_confidence_mean[[i]]<-mean(thisfile$bars_confidence)
  
  beatstart_length[[i]] <- length(thisfile$beats_start)
  beatstart_avgtime[[i]] <- mean(diff(thisfile$beats_start))
  beatstart_sdtime[[i]] <- sd(diff(thisfile$beats_start))
  beatstart_time[[i]] <- thisfile$beats_start[1]
  beat_confidence_mean[[i]]<-mean(thisfile$beats_confidence)
  
  ### After analyzing the section part, I decided to divided each songs into three part using kmeans method
  ### To use the cluster means and clustering vector as the features for section part
  section_avglength <- diff(thisfile$sections_start)
  if(length(section_avglength)>3){
  k<-kmeans(section_avglength,3)
  sectioncenter[[i]] <- k$centers
  #section_cluster[[i]] <- k$cluster
  }
  else
  {sectioncenter[[i]] <- section_avglength}
  section_length[[i]] <- length(section_avglength)
  sectionmean[[i]] <- mean(section_avglength)
  sectionsd[[i]]<-sd(section_avglength)
  section_confidence_mean[[i]]<-mean(thisfile$sections_confidence)
  
  segment_confidence_mean[[i]] <- mean(thisfile$segments_confidence)
  segment_length[[i]] <- length(thisfile$segments_start)
  segment_avgleng[[i]] <- mean(diff(thisfile$segments_start))
  
  loudness_quantile[[i]] <- quantile(thisfile$segments_loudness_max)
  loudness_range[[i]] <- max(thisfile$segments_loudness_max)-min(thisfile$segments_loudness_max)
  loudness_mean[[i]] <- mean(thisfile$segments_loudness_max)
  loudness_sd[[i]] <- sd(thisfile$segments_loudness_max)
  
  index_time <-as.integer(seq(1,length(thisfile$segments_loudness_max), length.out = 20))
  loudness_trend[[i]] <-thisfile$segments_loudness_max[index_time]
  loudness_loud_timeline[[i]] <- thisfile$segments_loudness_start[index_time]
  loudness_timeline[[i]] <- thisfile$segments_start[index_time]
  
  pitches[[i]] <- thisfile$segments_pitches[,index_time]
  pitch_mean[[i]] <- rowMeans(thisfile$segments_pitches)
  pitch_quantile[[i]] <- apply(thisfile$segments_pitches, 1, quantile, probs = c(0.05, 0.9),  na.rm = TRUE)
  
  timber[[i]] <- thisfile$segments_timbre[,index_time]
  timbre_mean[[i]] <- rowMeans(thisfile$segments_timbre)
  timbre_quantile[[i]] <- apply(thisfile$segments_timbre, 1, quantile, probs = c(0.05, 0.9),  na.rm = TRUE)
  
  tatum_confidence_mean[[i]] <- mean(thisfile$tatums_confidence)
  tatum_avgtime[[i]] <- mean(diff(thisfile$tatums_start))
  tatum_sdtime[[i]] <- sd(diff(thisfile$tatums_start))
}

index_train <- match(train_filenames,filenames)
file.names<-list.files(path = "/Users/jianitian/Desktop/Project4/data",pattern = ".h5",all.files = TRUE,
                      full.names = FALSE,recursive = TRUE, include.dirs = FALSE)
file.names.train <- file.names[index_train]
file.names.train.s <- gsub(".*/", "", file.names.train)
file.names.train.short <- strsplit(file.names.train.s,".",fixed = TRUE)[[1]][1]

file.names.train.s <-gsub(".*", "", file.names.train.s,fixed = TRUE)
file.names.test <- file.names[-index_train]


sec_center_mat <- matrix(nrow = 2000,ncol = 3)
for(i in 1:2000){
    for (j in 1:3) {
      sec_center_mat[i,j] <- sectioncenter[[i]][j]
    }
}
sec_center_mat[is.na(sec_center_mat)] <- 0

quantilemat_loudness<-matrix(nrow = 2000,ncol = 5)
for (i in 1:2000) {
  for (j in 1:5) {
    quantilemat_loudness[i,j] <- loudness_quantile[[i]][j]
  }
}

featuremat_loudness_trend_mat <- matrix(nrow = 2000,ncol = 20)
featuremat_loudness_timeline <- matrix(nrow = 2000,ncol = 20)
featuremat_timeline <- matrix(nrow = 2000,ncol = 20)
for (i in 1:2000) {
  for (j in 1:20) {
    featuremat_loudness_trend_mat[i,j]<-loudness_trend[[i]][j]
    featuremat_loudness_timeline[i,j] <- loudness_loud_timeline[[i]][j]
    featuremat_timeline[i,j] <- loudness_timeline[[i]][j]
  }
}

pitches_mat <- matrix(nrow = 2000,ncol = 240)
for (i in 1:2000) {
  mat <- pitches[[i]]
  thismat <- matrix(t(mat),nrow = 1,byrow = TRUE)
  for (j in 1:240) {
    pitches_mat[i,j] <- as.numeric(thismat[j])
  }
}

quantilemat_pitch<-matrix(nrow = 2000,ncol = 24)
for (i in 1:2000) {
  thismat<-pitch_quantile[[i]]
  matr <- matrix(t(thismat),nrow = 1,byrow = TRUE)
  for (j in 1:24) {
    quantilemat_pitch[i,j] <- matr[j]
  }
}

pitch_mean_mat <- matrix(nrow = 2000,ncol = 12)
for (i in 1:2000) {
  for (j in 1:12) {
    pitch_mean_mat[i,j] <- pitch_mean[[i]][j]
  }
}


timbre_mat <- matrix(nrow = 2000,ncol = 240)
for (i in 1:2000) {
  mat <- timber[[i]]
  thismat <- matrix(t(mat),nrow = 1,byrow = TRUE)
  for (j in 1:240) {
    timbre_mat[i,j] <- as.numeric(thismat[j])
  }
}

timbre_mean_mat <- matrix(nrow = 2000,ncol = 12)
for (i in 1:2000) {
  for (j in 1:12) {
    timbre_mean_mat[i,j] <- timbre_mean[[i]][j]
  }
}

quantilemat_timbre<-matrix(nrow = 2000,ncol = 24)
for (i in 1:2000) {
  thismat<-timbre_quantile[[i]]
  matr <- matrix(t(thismat),nrow = 1,byrow = TRUE)
  for (j in 1:24) {
    quantilemat_timbre[i,j] <- matr[j]
  }
}

### The features are as follows
featuremat_bar<-cbind(as.numeric(barstart_length),as.numeric(barstart_avgtime),as.numeric(barstart_sdtime),as.numeric(barstart_time))
featuremat_beat <-cbind(as.numeric(beatstart_length),as.numeric(beatstart_avgtime),as.numeric(beatstart_sdtime),as.numeric(beatstart_time))
featuremat_section <- cbind(sec_center_mat,as.numeric(sectionmean),as.numeric(sectionsd),as.numeric(section_length))
featuremat_segment <- cbind(as.numeric(segment_length),as.numeric(segment_avgleng))
featuremat_loudness_statis <- cbind(quantilemat_loudness,as.numeric(loudness_range),as.numeric(loudness_mean),as.numeric(loudness_sd))
feature_timbre_statistics <-cbind(timbre_mean_mat,quantilemat_timbre)
feature_tatum <-cbind(as.numeric(tatum_avgtime),as.numeric(tatum_sdtime))
pitch_feature_statitcs <- cbind(quantilemat_pitch,pitch_mean_mat)

###And another five matrix
# The dimention of featuremat_loudness_trend_mat is 2000*20
# The dimention of featuremat_loudness_timeline is 2000*20
# The dimention of featuremat_timeline is 2000*20
# The dimention of pitches_mat is 2000*240
# The dimention of timbre_mat is 2000*240

### Using whole features and do the pca

### I use their mean confidence as their relative weight

feature.total <- cbind(featuremat_bar,featuremat_beat,featuremat_section,featuremat_segment,featuremat_loudness_statis,
                       feature_timbre_statistics,pitch_feature_statitcs,feature_tatum)

for (i in 1:dim(feature.total)[2]) {
  index<-which(is.na(feature.total[,i]))
  feature.total[index,i]<-mean(na.omit(feature.total)[,i])
}

featuredf<-as.data.frame(feature.total)
featuredf$songnames <- file.names.train


### The dim of featuredf is 2000*99

confidence <- cbind(unlist(bar_confidence_mean),unlist(beat_confidence_mean),unlist(section_confidence_mean),
                    unlist(segment_confidence_mean),unlist(tatum_confidence_mean))
for (i in 1:5) {
  confidence[,i][which(is.na(confidence[,i]))] <- means[i]
}

new_matrix <-featuredf
for(i in 1:4){
  new_matrix[,i] <- featuredf[,i]*confidence[,1]
}
for (i in 5:8) {
  new_matrix[,i] <- featuredf[,i]*confidence[,2]
}
for (i in 9:14) {
  new_matrix[,i] <- featuredf[,i]*confidence[,3]
}
for (i in 15:96) {
  new_matrix[,i] <- featuredf[,i]*confidence[,4]
}
for (i in 97:98) {
  new_matrix[,i] <- featuredf[,i]*confidence[,5]
}

#new_matrix <- new_matrix[,-41]

### get statistics from features
mea <- function(x){mean(x,na.rm=TRUE)}
med <- function(x){median(x,na.rm = TRUE)}
mads <- apply(new_matrix[,1:98],2,mea)
medians <- apply(new_matrix[,1:98],2,med)

### replace NA with their means if numeric

scaled_featuremat <- scale(new_matrix[,1:98],center=medians,scale=mads)
pca1 <- prcomp(scaled_featuremat)

### 12 vecotors can explain 80.1% cumulative proportion of information of statistic features
newfeature_mat <- as.matrix(new_matrix[,1:98]) %*% pca1$rotation[,1:12]

### time series features
### delete number 21 and 41 column as their related variance are 0
time_series_feature <- cbind(featuremat_loudness_trend_mat,featuremat_loudness_timeline,featuremat_timeline,pitches_mat,timbre_mat)
which(is.na(time_series_feature))
time_series_feature <- time_series_feature[,-c(21,41)]
medians1 <- apply(time_series_feature,2,med)
mads1 <- apply(time_series_feature,2,mad)

scaled_time_feature <- scale(time_series_feature,center = medians1, scale=mads1)
pca2 <- prcomp(scaled_time_feature)
summary(pca2)
### nine variables can explain 99.34% information
plot(pca2)
newfeature_time_mat <- time_series_feature %*% pca2$rotation[,1:2]
newfeature.total <- cbind(newfeature_mat,newfeature_time_mat)



### Naive method to do clustering, comparing their performance about clustering results

medians2<-apply(newfeature.total,2,med)
mads2 <- apply(newfeature.total,2,mad)
scaled.feature.total <- scale(newfeature.total,center = medians2,scale = mads2)
featuremat.dist <- dist(scaled.feature.total)
feature.hclust <- hclust(featuremat.dist)
plot(feature.hclust,k=20,border="red",main='clustering of features')
groups.20 = cutree(feature.hclust,20)
clust.centroid = function(i, dat, groups.20) {
  ind = (clusters == i)
  colMeans(dat[ind,])
}

sapply(unique(groups.20), clust.centroid, scaled.feature.total, groups.20)
table(groups.20)
s<-sapply(unique(groups.20),function(g)featuredf$songnames[groups.20 == g])
feature.pam <- pam(featuremat.dist,20)
names(feature.pam)
table(groups.20,feature.pam$clustering)
plot(feature.pam)

for (i in 1:20) {

  A<-gsub(".*/", "", s[[i]])
  selected_var<-vector()
  for (j in 1:length(A)) {
    selected_var[j]<-strsplit(A,".",fixed = TRUE)[[j]][1]
  }
  select_mat <- train_lyr[train_lyr$`dat2$track_id`==selected_var,]
}

rect.hclust(feature.hclust,k=5,border = 2)
identify(feature.hclust,N=20,MAXCLUSTER = 20)
### kmeans methods ####
k.means.fit <- kmeans(scaled_featuremat, 20)
attributes(k.means.fit)
k.means.fit$cluster



##### topic modeling method

####install libraries
devtools::install_github("cpsievert/LDAvisData")
install.packages("LDAvis")
install.packages("lda")
library(LDAvisData)
data(reviews, package = "LDAvisData")
library(NLP)
library(tm)
library(lda)
library(LDAvis)

train_lyr <- lyr[index_train,]
test_lyr <- lyr[-index_train,]

documents<-list()
word_index<-list()
for (i in 1:dim(train_lyr)[1]) {
  this.row <- train_lyr[i,]
  word_index[[i]] <- which(this.row!=0)
  this.n<-length(word_index[[i]])
  this.mat <- matrix(nrow = 2,ncol = this.n-1)
  for (j in 1:this.n-1){
    this.mat[1,j] <- as.integer((word_index[[i]][2:this.n]-1)[j]-1)
    this.mat[2,j] <- as.integer((as.numeric(this.row[word_index[[i]]])[2:this.n])[j])
  }
  documents[[i]] <- as.matrix(this.mat)
}

save(documents,file = "document.RData")
vocab <- colnames(train_lyr)[2:dim(train_lyr)[2]]

# MCMC and model tuning parameters:
# I use validation try to find a better tune, the larger G, the more exactly the result will be 
# the larger alpha and eta, the more even the topic will be

K <- 18
G <- 5000
alpha <- 0.1
eta <- 0.1


# Fit the model:

set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # time is 6.67 min in my laptop

save(fit,file = "fit.RData")
topics<-fit$topics
topic1<-which(topics[1,]!=0,arr.ind = T, useNames = T)
topic_rank_result <- matrix(nrow = 18,ncol = 4973)
for (i in 1:18) {
  topic_rank_result[i,] <- rank(-topics[i,],ties.method ="min")
}
save(topic_rank_result,file = "topic_rank_result.RData")
unique(fit$assignment[[2]])

topic_mat <- matrix(nrow = 2000,ncol = 18)
percentages_mat <- matrix(nrow = 2000, ncol=18)
colnames(topic_mat) <- 0:17
colnames(percentages_mat) <- 0:17
for (i in 1:2000) {
  for (j in 1:18) {
    thismat <- t(as.matrix(table(fit$assignment[i])))
    these_var<-as.numeric(colnames(thismat))+1
    topic_mat[i,these_var]<-as.numeric(thismat)
    n<-18-length(these_var)
    topic_mat[i,-these_var]<-rep(0,n)
    percentages_mat[i,] <- topic_mat[i,]/rowSums(topic_mat)[i]
  }
}

# Another lad method to do clusting, the effect is not good enough
# fit1 <- lda.cvb0(documents, K=K, vocab = vocab, num.iterations = G, alpha = alpha, eta = eta, trace = 0L)


### Generalized linear regression to do the motinomial regression
X=solve(t(newfeature.total)%*%newfeature.total,t(newfeature.total)%*%percentages_mat)
save(X,file = "corelationmt.RData")

### find only one topic for each song
songs.topic <- vector()
for (i in 1:2000) {
  thismat <- t(as.matrix(table(fit$assignment[i])))
  ind <- which.max(thismat)
  songs.topic[i] <- colnames(thismat)[ind]
}
length(songs.topic)
table(songs.topic)
songs_topic <- as.factor(songs.topic)

### Now I want to use regression to find the relationship between features and songs' topics
library(foreign)
library(nnet)
feature.topic <- as.data.frame(cbind(newfeature.total,songs_topic))
colnames(feature.topic)<-c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11","PC12","PC13","PC14")
test <- multinom(songs_topic ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14, data = feature.topic)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors



predict(test, newdata = dses)



#### Test precess
test_100_dir.h5 <- "/Users/jianitian/Desktop/Project4/TestSongFile100"
test_100_files.list <- paste0("/Users/jianitian/Desktop/Project4/TestSongFile100/testsong",1:100)
test_100_files.list <- paste0(test_100_files.list,".h5")
test_100_files_names<- gsub(".*/", "", test_100_files.list)
test.100.files.names <-list()
for (i in 1:100) {
  test.100.files.names[[i]] <- strsplit(test_100_files_names[i],".",fixed = TRUE)[[1]][1]
}
test.100.names <- unlist(test.100.files.names)

#### how to get the test data features
feature.list <- list()
barstart_avgtime <- list()
barstart_sdtime <- list()
barstart_length <- list()
barstart_time <- list()
bar_confidence_mean <- list()

beatstart_avgtime <- list()
beatstart_sdtime <- list()
beatstart_length <- list()
beatstart_time <- list()
beat_confidence_mean <-list()

sectioncenter <- list()
sectionmean <- list()
sectionsd <- list()
section_length <- list()
section_confidence_mean <-list()

segment_confidence_mean <-list()
segment_length <- list()
segment_avgleng <- list()

loudness_quantile <- list()
loudness_range <- list()
loudness_mean <- list()
loudness_sd <- list()
loudness_trend <- list()
loudness_loud_timeline <- list()
loudness_timeline <- list()

pitches <- list()
pitch_mean <- list()
pitch_quantile <- list()

timber <- list()
timbre_mean <- list()
timbre_quantile <-list()

tatum_confidence_mean <- list()
tatum_avgtime<-list()
tatum_sdtime <- list()

for (i in 1:length(test_100_files.list)){
  feature.list [[i]]<- h5read(test_files_names[[i]],"/analysis")
  thisfile <- feature.list[[i]]
  
  barstart_length[[i]] <- length(thisfile$bars_start)
  barstart_avgtime[[i]]<-mean(diff(thisfile$bars_start))
  barstart_sdtime[[i]] <- sd(diff(thisfile$bars_start))
  barstart_time[[i]] <- thisfile$bars_start[1]
  bar_confidence_mean[[i]]<-mean(thisfile$bars_confidence)
  
  beatstart_length[[i]] <- length(thisfile$beats_start)
  beatstart_avgtime[[i]] <- mean(diff(thisfile$beats_start))
  beatstart_sdtime[[i]] <- sd(diff(thisfile$beats_start))
  beatstart_time[[i]] <- thisfile$beats_start[1]
  beat_confidence_mean[[i]]<-mean(thisfile$beats_confidence)
  
  
  section_avglength <- diff(thisfile$sections_start)
  if(length(section_avglength)>3){
    k<-kmeans(section_avglength,3)
    sectioncenter[[i]] <- k$centers
    #section_cluster[[i]] <- k$cluster
  }
  else
  {sectioncenter[[i]] <- section_avglength}
  section_length[[i]] <- length(section_avglength)
  sectionmean[[i]] <- mean(section_avglength)
  sectionsd[[i]]<-sd(section_avglength)
  section_confidence_mean[[i]]<-mean(thisfile$sections_confidence)
  
  segment_confidence_mean[[i]] <- mean(thisfile$segments_confidence)
  segment_length[[i]] <- length(thisfile$segments_start)
  segment_avgleng[[i]] <- mean(diff(thisfile$segments_start))
  
  loudness_quantile[[i]] <- quantile(thisfile$segments_loudness_max)
  loudness_range[[i]] <- max(thisfile$segments_loudness_max)-min(thisfile$segments_loudness_max)
  loudness_mean[[i]] <- mean(thisfile$segments_loudness_max)
  loudness_sd[[i]] <- sd(thisfile$segments_loudness_max)
  
  index_time <-as.integer(seq(1,length(thisfile$segments_loudness_max), length.out = 20))
  loudness_trend[[i]] <-thisfile$segments_loudness_max[index_time]
  loudness_loud_timeline[[i]] <- thisfile$segments_loudness_start[index_time]
  loudness_timeline[[i]] <- thisfile$segments_start[index_time]
  
  pitches[[i]] <- thisfile$segments_pitches[,index_time]
  pitch_mean[[i]] <- rowMeans(thisfile$segments_pitches)
  pitch_quantile[[i]] <- apply(thisfile$segments_pitches, 1, quantile, probs = c(0.05, 0.9),  na.rm = TRUE)
  
  timber[[i]] <- thisfile$segments_timbre[,index_time]
  timbre_mean[[i]] <- rowMeans(thisfile$segments_timbre)
  timbre_quantile[[i]] <- apply(thisfile$segments_timbre, 1, quantile, probs = c(0.05, 0.9),  na.rm = TRUE)
  
  tatum_confidence_mean[[i]] <- mean(thisfile$tatums_confidence)
  tatum_avgtime[[i]] <- mean(diff(thisfile$tatums_start))
  tatum_sdtime[[i]] <- sd(diff(thisfile$tatums_start))
}

file.names.test <- test_files_names


sec_center_mat <- matrix(nrow = length(file.names.test),ncol = 3)
for(i in 1:length(file.names.test)){
  for (j in 1:3) {
    sec_center_mat[i,j] <- sectioncenter[[i]][j]
  }
}
sec_center_mat[is.na(sec_center_mat)] <- 0

quantilemat_loudness<-matrix(nrow = length(file.names.test),ncol = 5)
for (i in 1:length(file.names.test)) {
  for (j in 1:5) {
    quantilemat_loudness[i,j] <- loudness_quantile[[i]][j]
  }
}

featuremat_loudness_trend_mat <- matrix(nrow = length(file.names.test),ncol = 20)
featuremat_loudness_timeline <- matrix(nrow = length(file.names.test),ncol = 20)
featuremat_timeline <- matrix(nrow = length(file.names.test),ncol = 20)
for (i in 1:length(file.names.test)) {
  for (j in 1:20) {
    featuremat_loudness_trend_mat[i,j]<-loudness_trend[[i]][j]
    featuremat_loudness_timeline[i,j] <- loudness_loud_timeline[[i]][j]
    featuremat_timeline[i,j] <- loudness_timeline[[i]][j]
  }
}

pitches_mat <- matrix(nrow = length(file.names.test),ncol = 240)
for (i in 1:length(file.names.test)) {
  mat <- pitches[[i]]
  thismat <- matrix(t(mat),nrow = 1,byrow = TRUE)
  for (j in 1:240) {
    pitches_mat[i,j] <- as.numeric(thismat[j])
  }
}

quantilemat_pitch<-matrix(nrow = length(file.names.test),ncol = 24)
for (i in 1:length(file.names.test)) {
  thismat<-pitch_quantile[[i]]
  matr <- matrix(t(thismat),nrow = 1,byrow = TRUE)
  for (j in 1:24) {
    quantilemat_pitch[i,j] <- matr[j]
  }
}

pitch_mean_mat <- matrix(nrow = length(file.names.test),ncol = 12)
for (i in 1:length(file.names.test)) {
  for (j in 1:12) {
    pitch_mean_mat[i,j] <- pitch_mean[[i]][j]
  }
}


timbre_mat <- matrix(nrow = length(file.names.test),ncol = 240)
for (i in 1:length(file.names.test)) {
  mat <- timber[[i]]
  thismat <- matrix(t(mat),nrow = 1,byrow = TRUE)
  for (j in 1:240) {
    timbre_mat[i,j] <- as.numeric(thismat[j])
  }
}

timbre_mean_mat <- matrix(nrow = length(file.names.test),ncol = 12)
for (i in 1:length(file.names.test)) {
  for (j in 1:12) {
    timbre_mean_mat[i,j] <- timbre_mean[[i]][j]
  }
}

quantilemat_timbre<-matrix(nrow = length(file.names.test),ncol = 24)
for (i in 1:length(file.names.test)) {
  thismat<-timbre_quantile[[i]]
  matr <- matrix(t(thismat),nrow = 1,byrow = TRUE)
  for (j in 1:24) {
    quantilemat_timbre[i,j] <- matr[j]
  }
}

### The features are as follows
featuremat_bar<-cbind(as.numeric(barstart_length),as.numeric(barstart_avgtime),as.numeric(barstart_sdtime),as.numeric(barstart_time))
featuremat_beat <-cbind(as.numeric(beatstart_length),as.numeric(beatstart_avgtime),as.numeric(beatstart_sdtime),as.numeric(beatstart_time))
featuremat_section <- cbind(sec_center_mat,as.numeric(sectionmean),as.numeric(sectionsd),as.numeric(section_length))
featuremat_segment <- cbind(as.numeric(segment_length),as.numeric(segment_avgleng))
featuremat_loudness_statis <- cbind(quantilemat_loudness,as.numeric(loudness_range),as.numeric(loudness_mean),as.numeric(loudness_sd))
feature_timbre_statistics <-cbind(timbre_mean_mat,quantilemat_timbre)
feature_tatum <-cbind(as.numeric(tatum_avgtime),as.numeric(tatum_sdtime))
pitch_feature_statitcs <- cbind(quantilemat_pitch,pitch_mean_mat)

###And another five matrix
# The dimention of featuremat_loudness_trend_mat is 350*20
# The dimention of featuremat_loudness_timeline is 350*20
# The dimention of featuremat_timeline is 350*20
# The dimention of pitches_mat is 350*240
# The dimention of timbre_mat is 350*240

### I use their mean confidence as their relative weight

feature.total <- cbind(featuremat_bar,featuremat_beat,featuremat_section,featuremat_segment,featuremat_loudness_statis,
                       feature_timbre_statistics,pitch_feature_statitcs,feature_tatum)

for (i in 1:dim(feature.total)[2]) {
  index<-which(is.na(feature.total[,i]))
  feature.total[index,i]<-mean(na.omit(feature.total)[,i])
}

featuredf<-as.data.frame(feature.total)
featuredf$songnames <- file.names.test

save(featuredf,file = "test_feature.RData")

### The dim of featuredf is 350*99


confidence <- cbind(unlist(bar_confidence_mean),unlist(beat_confidence_mean),unlist(section_confidence_mean),
                    unlist(segment_confidence_mean),unlist(tatum_confidence_mean))
for (i in 1:5) {
  confidence[,i][which(is.na(confidence[,i]))] <- means[i]
}

new_matrix <-featuredf
for(i in 1:4){
  new_matrix[,i] <- featuredf[,i]*confidence[,1]
}
for (i in 5:8) {
  new_matrix[,i] <- featuredf[,i]*confidence[,2]
}
for (i in 9:14) {
  new_matrix[,i] <- featuredf[,i]*confidence[,3]
}
for (i in 15:96) {
  new_matrix[,i] <- featuredf[,i]*confidence[,4]
}
for (i in 97:98) {
  new_matrix[,i] <- featuredf[,i]*confidence[,5]
}
#new_matrix <- new_matrix[,-41]
### get statistics from features
mea <- function(x){mean(x,na.rm=TRUE)}
med <- function(x){median(x,na.rm = TRUE)}
#means <- apply(featuredf[,1:98],2,mea)
mads <- apply(new_matrix[,1:98],2,mad)
medians <- apply(new_matrix[,1:98],2,median)
means <- apply(confidence,2,mea)
### replace NA with their means if numeric
for (i in 1:5) {
  confidence[,i][which(is.na(confidence[,i]))] <- means[i]
}
scaled_featuremat <- scale(new_matrix[,1:98],center=medians,scale=mads)


### 12 vecotors can explain 80.1% cumulative proportion of information of statistic features
newfeature_mat <- as.matrix(new_matrix[,1:98]) %*% pca1$rotation[,1:12]

### time series features
### delete number 21 and 41 column as their related variance are 0
time_series_feature <- cbind(featuremat_loudness_trend_mat,featuremat_loudness_timeline,featuremat_timeline,pitches_mat,timbre_mat)
which(is.na(time_series_feature))
time_series_feature <- time_series_feature[,-c(21,41)]
medians1 <- apply(time_series_feature,2,med)
mads1 <- apply(time_series_feature,2,mad)

scaled_time_feature <- scale(time_series_feature,center = medians1, scale=mads1)

### nine variables can explain 99.34% information
### Only use one dimention of y
newfeature_time_mat <- time_series_feature %*% pca2$rotation[,1:2]
newfeature.total <- cbind(newfeature_mat,newfeature_time_mat)
colnames(newfeature.total)<-c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11","PC12","PC13","PC14")

for (i in 1:100) {
  rank(total.test.mat[i,])
}

predictionresult <- predict(test, newdata = newfeature.total)
predictionresult <- as.numeric(predictionresult)

test_predicion_result <- matrix(nrow = length(file.names.test),ncol = dim(fit$topics)[2])
for (i in 1:length(predictionresult)) {
  test_predicion_result[i,] <- as.numeric(topic_rank_result[predictionresult[i],])
}
colnames(test_predicion_result)<-colnames(lyr[,2:4974])

num <- 1:100
test1<-data.frame(num,test_predicion_result[,1:4973])
dat.track_id <- test.100.names
bspot <- which(colnames(test1)=="X.qué")
test2<-data.frame(test1[,1],dat.track_id,test1[,bspot:ncol(test1)])
row_2_3 <- matrix(rep(4987,200),nrow = 100,ncol = 2)
test3 <-cbind(test2[,1:2],row_2_3)
col_6_30 <- matrix(rep(4987,2500),nrow = 100,ncol = 25)
test4<-cbind(test3,test2[,3:4],col_6_30,test2[,5:ncol(test2)])
write.csv(test4,file = "test_submission.csv")
save(test4,file = "test_prediction_result.RData")
save(percentages_mat,file = "percentage.RData")

### find only one topic for each song
songs.topic <- vector()
for (i in 1:2000) {
  thismat <- t(as.matrix(table(fit$assignment[i])))
  ind <- which.max(thismat)
  songs.topic[i] <- colnames(thismat)[ind]
}
length(songs.topic)
table(songs.topic)
songs_topic <- as.factor(songs.topic)


topic_mat_test <- matrix(nrow = 100,ncol = 18)
percentages_mat_test <- matrix(nrow = 2000, ncol=18)
colnames(topic_mat_test) <- 0:17
colnames(percentages_mat_test) <- 0:17
for (i in 1:100) {
  for (j in 1:18) {
    thismat <- t(as.matrix(table(fit$assignment[i])))
    these_var<-as.numeric(colnames(thismat))+1
    topic_mat[i,these_var]<-as.numeric(thismat)
    n<-18-length(these_var)
    topic_mat[i,-these_var]<-rep(0,n)
    percentages_mat[i,] <- topic_mat[i,]/rowSums(topic_mat)[i]
  }
}

### Generalized linear regression method

test_topic_mat <- newfeature.total %*% X
total.test.mat <- test_topic_mat %*% topics
### Evaluation 
evaluation <- vector()
for (i in 1 : 350){
  i<-1
  r_bar <- sum(test_predicion_result[i,]) / 4973
  evaluation[i] <-sum(test_predicion_result[1,which(test_lyr[i,] > 0)]) / length(which(test_lyr[i,] > 0))/r_bar
}

mean(evaluation)
