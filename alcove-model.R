#libraries
install.packages("pacman")
pacman::p_load(extraDistr, igraph, polspline, pscl, here,rjags,R2jags,jagsUI,tidyverse,tidybayes)

#theme for plots; increasing text size
theme_set(theme_minimal(base_size=8))

#simulation variables
n_trials <- 80
n_inputs <- 10
n_outputs <- 4

id <- "exp1_totalAQ" ## change this to AQ, switching, or social

## ------- running model ------ ##

stim <- stim[,,exclude==FALSE]
R <- R[,exclude==FALSE]
r <- r[,exclude==FALSE]
AQ <- AQ[exclude==FALSE]
switching <- switching[exclude==FALSE]
social <- social[exclude==FALSE]

#standardize AQ scores
AQ <- (AQ-mean(AQ))/sd(AQ)
#standardize switching scores
switching <- (switching-mean(switching))/sd(switching)
#standardize social scores
social <- (social-mean(social))/sd(social)

n_subs <- length(AQ)

start_time = Sys.time()
#change here between AQ, switching or social 
data <- list("stim","R","AQ","n_subs","r") #AQ - data inputted into jags #"nagents","n_subs"
params <- c("beta_lambda0","beta_gamma0","beta_lambdaAQ","beta_gammaAQ","gamma","lambda","theta","beta_theta0","beta_thetaAQ") #parameters we'll track in jags

## Doing inference/parameter recovery using the jags code
samples <- jags.parallel(data, inits=NULL,params,
                         model.file="theta_alcove.txt",
                         n.chains=3, n.iter=10000, n.burnin=2000, n.thin=2)
end_time <- Sys.time()
end_time-start_time
#beep(sound = 1)

#mean posterior 
mean(samples$BUGSoutput$sims.list$beta_thetaSocial)

#results reporting
print(samples, digits=3)
samples1 <- as.mcmc(samples)
traceplot(samples1)

#save files
filename = paste("results/", id, "exp1_totalAQ", ".RData",sep="")
filename
save.image(file = "results/exp1_totalAQ.RData") ##change here to attention shifting etc 

#inferred AQ 
beta_lambda0_infer <- samples$BUGSoutput$sims.list$beta_lambda0
beta_lambdaAQ_infer <- samples$BUGSoutput$sims.list$beta_lambdaAQ
beta_gamma0_infer <- samples$BUGSoutput$sims.list$beta_gamma0
beta_gammaAQ_infer <- samples$BUGSoutput$sims.list$beta_gammaAQ
beta_theta0_infer <- samples$BUGSoutput$sims.list$beta_theta0
beta_thetaAQ_infer <- samples$BUGSoutput$sims.list$beta_thetaAQ
lambda_infer <- samples$BUGSoutput$sims.list$lambda
gamma_infer <- samples$BUGSoutput$sims.list$gamma
theta_infer <- samples$BUGSoutput$sims.list$theta

#inferred switching
beta_lambda0_infer <- samples$BUGSoutput$sims.list$beta_lambda0
beta_lambdaShifting_infer <- samples$BUGSoutput$sims.list$beta_lambdaShifting
beta_gamma0_infer <- samples$BUGSoutput$sims.list$beta_gamma0
beta_gammaShifting_infer <- samples$BUGSoutput$sims.list$beta_gammaShifting
beta_theta0_infer <- samples$BUGSoutput$sims.list$beta_theta0
beta_thetaShifting_infer <- samples$BUGSoutput$sims.list$beta_thetaShifting
lambda_infer <- samples$BUGSoutput$sims.list$lambda
gamma_infer <- samples$BUGSoutput$sims.list$gamma
theta_infer <- samples$BUGSoutput$sims.list$theta

#inferred social
beta_lambda0_infer <- samples$BUGSoutput$sims.list$beta_lambda0
beta_lambdaSocial_infer <- samples$BUGSoutput$sims.list$beta_lambdaSocial
beta_gamma0_infer <- samples$BUGSoutput$sims.list$beta_gamma0
beta_gammaSocial_infer <- samples$BUGSoutput$sims.list$beta_gammaSocial
beta_theta0_infer <- samples$BUGSoutput$sims.list$beta_theta0
beta_thetaSocial_infer <- samples$BUGSoutput$sims.list$beta_thetaSocial
lambda_infer <- samples$BUGSoutput$sims.list$lambda
gamma_infer <- samples$BUGSoutput$sims.list$gamma
theta_infer <- samples$BUGSoutput$sims.list$theta


##############################################
############### plots  #######################
##############################################


#saving posterior plots - png.
make_pwetty_plot <- function(var, title){
  tibble(x=var) %>% 
    ggplot(aes(x)) +
    stat_halfeye(fill = "lightsteelblue", color="darkblue") +
    ggtitle(title) +
    labs(y = "Density") + theme(axis.title.x = element_blank())
  
  filename <- paste0("fig/exp4t/socialskills/", title, ".png")
  ggsave(filename)
}

## save posterior plots for exp 1 - total AQ 
vars <- list(beta_lambda0_infer, beta_gamma0_infer,beta_lambdaAQ_infer, beta_gammaAQ_infer,beta_theta0_infer,beta_thetaAQ_infer)
titles <- c("Associative Learning Intercept", "Attentional Learning Intercept", "Associative Learning - Total AQ Score","Attentional Learning - Total AQ Score", "Response Noise Intercept","Response Noise - Total AQ score")
map2(vars, titles, make_pwetty_plot)

## save posterior plots for exp 1 - Attention shifting 
vars <- list(beta_lambda0_infer, beta_gamma0_infer, beta_lambdaShifting_infer, beta_gammaShifting_infer,beta_theta0_infer,beta_thetaShifting_infer)
titles <- c("Associative Learning Intercept", "Attentional Learning Intercept", "Associative Learning - Attention Shifting Subscale Score ","Attentional Learning - Attention Shifting Subscale Score","Response Noise Intercept","Response Noise - Attention Shifting")
map2(vars, titles, make_pwetty_plot)

## save posterior plots for exp 1 - Social Skills 
vars <- list(beta_lambda0_infer, beta_gamma0_infer,beta_lambdaSocial_infer, beta_gammaSocial_infer,beta_theta0_infer,beta_thetaSocial_infer)
titles <- c("Associative Learning Intercept", "Attentional Learning Intercept", "Associative Learning - Social Skills","Attentional Learning - Social Skills","Response Noise Intercept","Response Noise - Social Skills")
map2(vars, titles, make_pwetty_plot)



## bayes factor 
prior <- dnorm(0,0,(1/sqrt(.1)))
fit.posterior <- logspline(samples$BUGSoutput$sims.list$beta_thetaSocial)
posterior <- dlogspline(0,fit.posterior) #this gives the pdf at point delta = 0 
BF <- prior/posterior
BF

prior <- dnorm(0,0,(1/sqrt(.1)))
fit.posterior <- logspline(samples$BUGSoutput$sims.list$beta_thetaSocial)
posterior <- dlogspline(0,fit.posterior) #this gives the pdf at point delta = 0 
BF <- posterior/prior
BF


