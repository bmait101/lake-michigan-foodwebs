#-----------------------------------------------#
# Bayesian L-W regression model (Froese et al. 2014)
# Modified from Fred Keppeller
#----------------------------------------------#

# Requires list of fish species to run analysis on 

# Libraries
pacman::p_load(here, tidyverse, rfishbase, R2jags)

# Prep -----

# List of our species
xref_spp <- read.csv(here("data", "spp_xref.csv")) |> 
  mutate(common_name = str_to_lower(common_name))

List_fish <- xref_spp |> select(Species=sci_name) |> as.data.frame()

# Extract species & bodyshape columns from morphology database (fishbase)  
shape_fish <- morphology()[,c(2,13)]

# Extract length-weight relationships available from fishbase
# data_LL <- length_length()
# data_LL <- subset(data_LL, Species %in% List_fish$Species)
data_LW <- length_weight()
data_LW <- subset(data_LW, Type=="TL")  # Use only TL
# data_LW <- subset(data_LW, Type=="TL" | is.na(Type))

# Add shape to the LWR table and convert to factor
data_LW <- merge(data_LW, shape_fish, by="Species")
colnames(data_LW)[39]<- "Bshape"
data_LW$Bshape<-factor(data_LW$Bshape)

# Extract genus names
Genus <- data.frame(
  do.call(
    'rbind', 
    strsplit(as.character(data_LW$Species),' ',fixed=TRUE)
    )
  )

data_LW$Genus<-Genus[,1]
data_LW$Ep_specific<-Genus[,2]

# Define species scores (CoeffDetermination) according to Froese et al. 2014
for (i in 1:nrow(data_LW)){
  if (is.na(data_LW$CoeffDetermination[i])==FALSE){
    data_LW$Score[i]= data_LW$CoeffDetermination[i]
  } else if (is.na(data_LW$LengthMin[i])==FALSE & 
             is.na(data_LW$LengthMax[i])==FALSE){
    data_LW$Score[i]=0.7
  } else {
    data_LW$Score[i]=0.5
  }}

# Family code - add to LWR data
data <- species()[,c(2,11)]
data_LW <- merge(data_LW, data, by="Species")
data_LW$Species <- gsub(" ", "_", data_LW$Species)

# Organize fish species table
# List_fish <- data.frame(Species=List_fish)
Genus <- data.frame(
  do.call(
    'rbind', strsplit(as.character(List_fish$Species), ' ', fixed=TRUE)
    )
  )

List_fish$Genus<-Genus[,1]
List_fish$Ep_specific<-Genus[,2]
List_fish <- merge(List_fish, shape_fish, by="Species")
colnames(List_fish)[4]<- "Bshape"      
List_fish <- 
  List_fish |> 
  mutate(Bshape = case_when(
    Ep_specific == "thompsonii" ~ "elongated", 
    Ep_specific == "bairdii" ~ "elongated", 
    Ep_specific == "hudsonius" ~ "fusiform / normal", 
    Ep_specific == "diaphanus" ~ "fusiform / normal", 
    TRUE ~ Bshape))
# List_fish <- List_fish |> slice(-18)

#-------------------------#
# BAYESIAN ANALYSIS ----
#-------------------------#

RESULTS <- matrix(nrow=nrow(List_fish),ncol=15)
colnames(RESULTS)<-c(
  "Target Species",
  "Number of target species observations",
  "Number of related species",
  "Number of related species observations",	
  "Body shape", "mean loga Related species",	
  "sd loga Related Species",
  "mean b Related species",	
  "sd b Related species",
  "mean loga Target species",	
  "sd loga Target species",	
  "mean b Target species",	
  "sd b Target species",
  "95% HDI of a",	
  "95% HDI of b"
  )

# i = 7
pdf(here("out", "models", "lw", "Post_param.pdf"), width=10)
par(mfrow=c(1,2))
for (i in 1:nrow(List_fish)) {
  Genus = as.character(List_fish$Genus[i])  
  Species = as.character(List_fish$Ep_specific[i])
  Data = data_LW
  DataGS = Data[Data$Genus == Genus & Data$Ep_specific == Species,] # select data for Species
  Family = species(as.character(List_fish$Species[i]))$FamCode
  DataFam = subset(Data,FamCode.y == Family)
  Bshape = as.character(List_fish$Bshape[i]) # one of: "eel-like", "elongated", "fusiform / normal", "short & deep"
  
  # if (Bshape == "eel-like") { # eel-like prior for log(a) and b
  #   ELL<-subset(data_LW,Bshape=="eel-like")
  #   prior_sd_log10a<- sd(log10(ELL$a))
  #   prior_tau_log10a = 1/prior_sd_log10a^2
  #   prior_mean_b<- mean(ELL$b)
  #   prior_sd_b<- sd(ELL$b)
  #   prior_tau_b <- 1/prior_sd_b^2
  #   }
  if(Bshape == "elongated") { # elongate prior for log(a) and b
    ELONGATED<-subset(data_LW,Bshape=="elongated")
    prior_mean_log10a<- mean(log10(ELONGATED$a))
    prior_sd_log10a<- sd(log10(ELONGATED$a))
    prior_tau_log10a = 1/prior_sd_log10a^2
    prior_mean_b<- mean(ELONGATED$b)
    prior_sd_b<- sd(ELONGATED$b)
    prior_tau_b <- 1/prior_sd_b^2
    }
  if(Bshape == "fusiform / normal") { # fusiform prior for log(a) and b
    FUSIFORM<-subset(data_LW,Bshape=="fusiform / normal")
    prior_mean_log10a<- mean(log10(FUSIFORM$a))
    prior_sd_log10a<- sd(log10(FUSIFORM$a))
    prior_tau_log10a = 1/prior_sd_log10a^2
    prior_mean_b<- mean(FUSIFORM$b)
    prior_sd_b<- sd(FUSIFORM$b)
    prior_tau_b <- 1/prior_sd_b^2
    }
  if(Bshape == "short and / or deep") { # short & deep prior for log(a) and b
    SHORT_DEEP<-subset(data_LW,Bshape=="short and / or deep")
    prior_mean_log10a<- mean(log10(SHORT_DEEP$a))
    prior_sd_log10a<- sd(log10(SHORT_DEEP$a))
    prior_tau_log10a = 1/prior_sd_log10a^2
    prior_mean_b<- mean(SHORT_DEEP$b)
    prior_sd_b<- sd(SHORT_DEEP$b)
    prior_tau_b <- 1/prior_sd_b^2
    }
  if(nrow(DataFam)==0 & nrow(DataGS)==0 ){
    RESULTS[i,1]<- paste(Genus,Species,sep=" ") 
    RESULTS[i,2]<-0
    RESULTS[i,3]<-0
    RESULTS[i,4]<-0
    RESULTS[i,10]<-prior_mean_log10a
    RESULTS[i,11]<-prior_sd_log10a
    RESULTS[i,12]<-prior_mean_b
    RESULTS[i,13]<-prior_sd_b
  } 
  else{
    
    ### Define data ----
    
    Keep = which(DataFam$Score>0) # exclude studies with zero Score or with other body shapes
    wts = DataFam$Score[Keep]  # Un-normalized weights (so that Cov is comparable among analyses)
    a = DataFam$a[Keep] # vector with estimates of parameter 'a' from selected studies
    b = DataFam$b[Keep] # vector with estimates of parameter 'b' from selected studies
    GenSpec = paste(DataFam$Genus[Keep],DataFam$Species[Keep]) # get combinations of names
    TargetSpec = paste(Genus, Species)
    
    # Relabel GenSpec so that TargetSpec = 1
    OtherSpecies = unique(GenSpec[GenSpec != TargetSpec])
    GenusSpecies = factor(GenSpec, levels=c(TargetSpec, OtherSpecies))
    Nspecies = nlevels(GenusSpecies) # number of species
    Nobs = length(a) # number of observations
    
    # Priors for measurement error (= sigma) based on 5150 studies for 1821 species
    # given here as shape mu and rate r, for gamma distribution
    SD_rObs_log10a = 6520
    SD_muObs_log10a = 25076 
    SD_rObs_b = 6808
    SD_muObs_b = 37001
    
    # Priors for process error (between species variabilit, = sigma) based on 5150 studies for 1821 species
    SD_rGS_log10a = 1372
    SD_muGS_log10a = 7933
    SD_rGS_b = 572
    SD_muGS_b = 6498
    
    ### Define JAGS model  ----
    
    Model = "
    model {               
    
    #### Process model -- effects of taxonomy
    
    # Given the priors distribution and likelihood distributions (new data), 
    # estimatr normal posterior distributions for log10a, b, 
    # and for the process (measurement?) error (=between species variability sigmaGS):
    
    abTrue[1] ~ dnorm(prior_mean_log10a,prior_tau_log10a)   # given the data and the prior, get normal posterior distribution for log10a
    abTrue[2] ~ dnorm(prior_mean_b,prior_tau_b)             # given the data and the prior, get normal posterior distribution for b
    sigmaGSlog10a ~ dgamma(SD_rGS_log10a, SD_muGS_log10a)   # given the data and the prior, establish posterior distribution for measurement error in log10a  
    sigmaGSb ~ dgamma(SD_rGS_b, SD_muGS_b)                  # given the data and the prior, establish posterior distribution for measurement error in b
    
    # Given the posterior distributions and the process (measurement?) errors,
    # establish for every species the expected witin-species 
    # parameter distributions; no correlation (roGS) between species:
    
    roGS <- 0 
    tauGenusSpecies[1] <- pow(sigmaGSlog10a,-2)
    tauGenusSpecies[2] <- pow(sigmaGSb,-2)
    for(k in 1:Nspecies){
    abGenusSpecies[k,1] ~ dnorm(abTrue[1],tauGenusSpecies[1]) 
    abGenusSpecies[k,2] ~ dnorm(abTrue[2],tauGenusSpecies[2]) 
    }
    
    ### Observation model  
    ## Errors 
    
    # given the data and the priors, establish distributions  	
    # for the observation errors sigmaObs 	
    
    sigmaObslog10a ~ dgamma( SD_rObs_log10a, SD_muObs_log10a) 
    sigmaObsb ~ dgamma( SD_rObs_b, SD_muObs_b) 	
    
    # create inverse covariance matrix, with negative parameter correlation roObs
    roObs ~ dunif(-0.99,0)  # uniform prior for negative correlation between log10a and b
    CovObs[1,1] <- pow(sigmaObslog10a,2)  
    CovObs[2,2] <- pow(sigmaObsb,2) 
    CovObs[1,2] <- roObs * sigmaObslog10a * sigmaObsb 
    CovObs[2,1] <- CovObs[1,2]
    TauObs[1:2,1:2] <- inverse(CovObs[1:2,1:2]) 
    
    ## likelihood function
    
    # given the data, the priors and the covariance, 
    # create multivariate likelihood distributions for log10(a) and b 
    
    for(i in 1:N){
    TauObsI[i,1:2,1:2] <- TauObs[1:2,1:2] * pow(Weights[i],2)   # weighted precision
    ab[i,1:2] ~ dmnorm(abGenusSpecies[GenusSpecies[i],1:2],TauObsI[i,1:2,1:2])   
    }
    }
    "
    
    # Write JAGS model 
    File <- glue::glue(here("out", "models", "lw"), "/model_", List_fish$Species[i], sep="")
    cat(Model, file=glue::glue(File,"_dmnorm.bug",sep=""))
    
    ### JAGS settings ----
    
    Nchains = 3	# number of MCMC chains to be used in JAGS
    Nburnin = 1e4 # number of burn-in iterations, to be discarded; 1e4 = 10000 iterations for burn-in
    Niter = 3e4 # number of iterations after burn-in; 3e4 = 30000 iterations
    Nthin = 1e1 # subset of iterations to be used for analysis; 1e1 = every 10th iteration 
    
    ### Run JAGS -----
    
    # define data to be passed on in DataJags; 
    # determine parameters to be returned in Param2Save; 
    # call JAGS with function Jags()
    DataJags = list(ab=cbind(log10(a),b), N=Nobs, Weights=wts, Nspecies=Nspecies, GenusSpecies=GenusSpecies,
                    prior_mean_b=prior_mean_b, prior_tau_b=prior_tau_b, 
                    prior_mean_log10a=prior_mean_log10a, prior_tau_log10a=prior_tau_log10a, 
                    SD_rObs_log10a=SD_rObs_log10a, SD_muObs_log10a=SD_muObs_log10a,  
                    SD_rObs_b=SD_rObs_b, SD_muObs_b=SD_muObs_b, 
                    SD_rGS_log10a=SD_rGS_log10a, SD_muGS_log10a=SD_muGS_log10a,
                    SD_rGS_b=SD_rGS_b, SD_muGS_b=SD_muGS_b)
    Params2Save = c("abTrue","abGenusSpecies","sigmaGSlog10a","sigmaGSb","sigmaObslog10a","sigmaObsb","roObs")
    Jags <- jags(model.file=glue::glue(File,"_dmnorm.bug",sep=""), working.directory=NULL, data=DataJags, 
                 parameters.to.save=Params2Save, n.chains=Nchains, n.thin=Nthin, n.iter=Niter, n.burnin=Nburnin)
    Jags$BUGSoutput # contains the results from the JAGS run
    
    # Analyze output for the relatives ----
    abTrue <- Jags$BUGSoutput$sims.list$abTrue
    R_mean_log10a  <- mean(abTrue[,1]) # true mean of log10(a)
    R_sd_log10a    <- sd(abTrue[,1])   # true SE of log10(a)
    R_mean_b       <- mean(abTrue[,2])         # true mean of b
    R_sd_b         <- sd(abTrue[,2])           # true SE of b
    
    # Analyze output for the target species ----
    abGenusSpecies <- Jags$BUGSoutput$sims.list$abGenusSpecies
    mean_log10a  <- mean(abGenusSpecies[,1,1]) # true mean of log10(a) for the first species= target species
    sd_log10a    <- sd(abGenusSpecies[,1,1])   # true SE of log10(a)
    mean_b       <- mean(abGenusSpecies[,1,2])         # true mean of b
    sd_b         <- sd(abGenusSpecies[,1,2])           # true SE of b
    mean_sigma_log10a <- mean(Jags$BUGSoutput$sims.list$sigmaObslog10a) # measurement error of log10(a)
    sd_sigma_log10a <- apply(Jags$BUGSoutput$sims.list$sigmaObslog10a, 2, sd)
    mean_sigma_b    <- mean(Jags$BUGSoutput$sims.list$sigmaObsb) # measurement error of b
    sd_sigma_b		<- apply(Jags$BUGSoutput$sims.list$sigmaObsb, 2, sd)
    ro_ab        <- mean(Jags$BUGSoutput$sims.list$roObs) # measurement correlation of log10(a),b
    
    # Parameters to be save ----
    RESULTS[i,1]<- TargetSpec
    RESULTS[i,2]<-length(GenSpec[GenSpec==TargetSpec])
    RESULTS[i,3]<-Nspecies - 1
    RESULTS[i,4]<-length(a) - length(GenSpec[GenSpec==TargetSpec])
    RESULTS[i,5]<- Bshape
    RESULTS[i,6]<- format(R_mean_log10a,digits=3)
    RESULTS[i,7]<- format(R_sd_log10a,digits=3)
    RESULTS[i,8]<- format(R_mean_b,digits=3)
    RESULTS[i,9]<- format(R_sd_b,digits=3)
    RESULTS[i,10]<- format(mean_log10a,digits=3)
    RESULTS[i,11]<- format(sd_log10a,digits=3)
    RESULTS[i,12]<- format(mean_b,digits=3)
    RESULTS[i,13]<- format(sd_b,digits=3)
    RESULTS[i,14]<- paste(format(quantile(abGenusSpecies[,1,1],prob=0.025),digits=3),"/",
                          format(quantile(abGenusSpecies[,1,1],prob=0.975),digits=3),sep=" ")
    RESULTS[i,15]<-paste(format(quantile(abGenusSpecies[,1,2],prob=0.025),digits=3),"/",
                         format(quantile(abGenusSpecies[,1,2],prob=0.975),digits=3),sep=" ")
    
    # Posterior and prior density of log10(a) ----
    x <- seq((prior_mean_log10a - 4 * prior_sd_log10a),(prior_mean_log10a + 4 * prior_sd_log10a),0.001)
    curve(
      dnorm(x, mean=mean_log10a, sd=sd_log10a), 
      from = prior_mean_log10a-4*prior_sd_log10a, 
      to = prior_mean_log10a+4*prior_sd_log10a, las=1, main="", 
      xlab=expression("log"[10]*"("~italic(a)~")"), 
      ylab= "Density", frame=F) 
    curve(
      dnorm(x, mean=prior_mean_log10a, sd=prior_sd_log10a), 
      lty=2, add=T) 
    if (nrow(DataGS)!=0){
      for (j in 1:nrow(DataGS)){
        points(log10(DataGS$a[j]), 0,pch=16,col="black")
      }}
    # DataFam2<-DataFam[DataFam$Ep_specific!=Species,]
    # if (nrow(DataFam)!=0 & DataFam$Ep_specific!=Species){
    #   for (k in 1:nrow(DataFam2)){
    #     points(log10(DataFam2$a[k]), 0)
    #   }} 
    label<-List_fish$Species[i]
    title (bquote(italic(.(label))~"- Log a"))
    
    # Posterior and prior density of b ----
    x <- seq(prior_mean_b - 4 * prior_sd_b, prior_mean_b + 4 * prior_sd_b, 0.001)
    curve(dnorm(x, mean = mean_b, sd = sd_b), from = prior_mean_b - 4 * prior_sd_b, 
          to = prior_mean_b + 4 * prior_sd_b, las=1, main="",
          xlab=expression(~italic(b)), ylab="Density", frame=F)
    curve(dnorm(x, mean = prior_mean_b, sd = prior_sd_b), lty=2, add=T)
    if (nrow(DataGS)!=0){
      for (j in 1:nrow(DataGS)){
        points(DataGS$b[j], 0,pch=16,col="black")
      }}
    # DataFam2<-DataFam[DataFam$Species!=Species,]
    # if (nrow(DataFam2)!=0 & DataFam$Species!=Species){
    #   for (k in 1:nrow(DataFam2)){
    #     points(DataFam2$b[k], 0)
    #   }} 
    label<-List_fish$Species[i]
    title (bquote(italic(.(label))~'- b'))
  }# END MODEL
  } # END ALL
dev.off()

RESULTS
write.table (RESULTS, file = here("out", "models", "lw", "results_L_W_Bayes_v2.txt"))
# RESULTS <- read.table (file = here("out", "models", "lw", "results_L_W_Bayes.txt"))

df_lw_params <- RESULTS |> 
  as_tibble() |> 
  slice(-33) |>  # remove duplicate brown trout record
  select(
    sci_name = Target.Species, 
    log_a = mean.loga.Target.species, 
    b = mean.b.Target.species
    )

write_rds(df_lw_params, here("out", "tbls", "body_mass_params.rds"))


