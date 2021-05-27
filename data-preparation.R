library(R2jags)

# load data file from relevant experiment - here seen for exp1 
exp1Raw <- read.csv("data/experiment1.csv")

# Load prepared/defaul stimulus vector  
# these are node activations corresponding to stimulus labels in data file
stim_vector <- read.csv("data/stim_vector.csv",header = FALSE,sep=",")
stim_vector <- as.matrix(stim_vector)


# each cue/stimulus is an activation node - 0-1
n_nodes <- 10
n_subs <- 123 #there was 123 subjects in experiment 1 
n_trials <- 80 

exp1 = as.data.frame(t(exp1Raw)) #re-orient data
stimNo = seq(1,length(exp1Raw),1) # use this for modulus calc.....

# ....to extract only matrix of sequences & responses - every 3rd number
stim_seq = exp1[stimNo%%3 == 2,] #the sequences - extract only seqences - every 3rd item
x <- exp1[stimNo%%3==0,] #the responses - extract only responses - every 3rd item

# trim experiment number info & take only training trials
stim_seq = stim_seq[3:length(stim_seq[,1]),]
x <- x[2:length(x[,1]),]

# make empty arrays to populate 
stim <- array(0,c(n_trials,n_nodes,n_subs)) #three dimensional 
R <- array(0,c(n_trials,n_subs))
r <- array(0,c(n_trials,n_subs))

#testing 
test_stim <- array(0,c(n_nodes,48,n_subs)) #not using this anyways
test_r <- array(0,c(48,n_subs)) #not using this anyways

# loop through all subjects
for (s in 1:length(stim_seq[1,])) {
  
  #extract sequence of trials for subject s
  seq <- as.numeric(as.character(unlist(stim_seq[,s])))
  
  # extract responses for subject s
  x_s <- as.numeric(as.character(unlist(x[,s])))
  
  # make a data frame of default ordered stimulus node activations, 
  # we want to make a list of which stimulus occurs on which trial, which is what we get in the stim variable in the end stim[,,s]
  # responses for s, and sequence of trials 
  df <- cbind(stim_vector,x_s,seq)
  
  # These dfs are the blocks:: 
  #order the first block (df1), second block (df2), etc
  df1 <- df[1:16,]
  df1 <- df1[order(df1[1:16,13]),]
  
  df2 <- df[17:32,]
  df2 <- df2[order(df2[1:16,13]),]
  
  df3 <- df[33:48,]
  df3 <- df3[order(df3[1:16,13]),]
  
  df4 <- df[49:64,]
  df4 <- df4[order(df4[1:16,13]),]
  
  df5 <- df[65:80,]
  df5 <- df5[order(df5[1:16,13]),]
  
  df6 <-df[81:128,] #### ERROR ::: subscript out of bounds 
  df6 <- df6[order(df6[1:48,13]),] #### ERROR ::: subscript out of bounds 
  
  # populate arrays with ordered stimulus node activations (stim), 
  # correct responses (R), and subject responses, for subject s
  stim[,,s] <- rbind(df1[,1:10],df2[,1:10],df3[,1:10],
                     df4[,1:10],df5[,1:10]) ## now we know what stimulus occurs on which trial
  
  R[,s] <- c(df1[,11],df2[,11],df3[,11],
             df4[,11],df5[,11])
  
  r[,s] <- c(df1[,12],df2[,12],df3[,12],
             df4[,12],df5[,12])
  
  test_stim[,,s] <- df6[,1:10]
  test_r[,s] <- df6[,12]
  
}

#to see the structure for only subject 1:
#stim <- stim[,,1] #  three dimensions initially. this makes stim only two dimensions
#R <- R[,1]
#r <- r[,1]

#test_stim <- test_stim[,,1]
#test_r <- test_r[,1]
