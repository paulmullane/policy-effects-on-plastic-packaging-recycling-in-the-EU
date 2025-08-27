#power sims and type I sims for europe

#read in packages----
library(readxl)
library(lmtest)

#load in data and select countries----
europe <- read_excel("recycling full.xlsx")
europe$year <- as.double(europe$year)
netherlands <- europe[,c('year','netherlands')]
germany <- europe[, c('year', 'germany')]
portugal <- europe[, c('year', 'portugal')]
spain <- europe[, c('year', 'spain')]
denmark <- europe[, c('year', 'denmark')]
belgium <- europe[,c('year', 'belgium')]
greece <- europe[, c('year', 'greece')]
france <- europe[, c('year', 'france')]
italy <- europe[, c('year', 'italy')]
luxembourg <- europe[, c('year', 'luxembourg')]
austria <- europe[, c('year', 'austria')]
finland <- europe[, c('year', 'finland')]
sweden <- europe[, c('year', 'sweden')]

imports <- read_excel("import full.xlsx")
exports <- read_excel("export full.xlsx")

#imports
netherlands$import <- imports$netherlands/10000000000
germany$import <- imports$germany/10000000000
portugal$import <- imports$portugal/10000000000
spain$import <- imports$spain/10000000000
denmark$import <- imports$denmark/10000000000
belgium$import <- imports$belgium/10000000000
greece$import <- imports$greece/10000000000
france$import <- imports$france/10000000000
italy$import <- imports$italy/10000000000
luxembourg$import <- imports$luxembourg/10000000000
austria$import <- imports$austria/10000000000
finland$import <- imports$finland/10000000000
sweden$import <- imports$sweden/10000000000
ireland$import <- imports$ireland/10000000000

#exports
netherlands$export <- exports$netherlands/10000000000
germany$export <- exports$germany/10000000000
portugal$export <- exports$portugal/10000000000
spain$export <- exports$spain/10000000000
denmark$export <- exports$denmark/10000000000
belgium$export <- exports$belgium/10000000000
greece$export <- exports$greece/10000000000
france$export <- exports$france/10000000000
italy$export <- exports$italy/10000000000
luxembourg$export <- exports$luxembourg/10000000000
austria$export <- exports$austria/10000000000
finland$export <- exports$finland/10000000000
sweden$export <- exports$sweden/10000000000
ireland$export <- exports$ireland/10000000000


#necessary simulation functions----
simulate_its_data <- function(npre, npost, slope_pre, level_change, slope_change, sigma, imports, exports){
  #This function simulates the data & intervention change 
  ntotal <- npre+npost
  time <- 1:ntotal
  intervention <- c(rep(0, npre), rep(1, npost))
  time_post <- c(rep(0, npre), 1:npost)
  
  #expected values without noise
  mu <- slope_pre*time+level_change*intervention+slope_change*time_post
  #pre intervention trend+ level change from intervention + slope change from intervention
  
  #add noise
  e <- rnorm(ntotal, mean=0, sd=sigma)
  
  y <- mu+e
  
  data.frame(y=y, time=time, intervention=intervention, time_after=time_post, imports=imports, exports=exports)
}

run_simulation <- function(npre, npost, slope_pre, level_change, slope_change, sigma, imports, exports){
  #This function runs the simulations
  sim_data <- simulate_its_data(npre, npost, slope_pre, level_change, slope_change, sigma, imports, exports)
  sim_data$net <- sim_data$imports-sim_data$exports
  
  model <- lm(y~time+intervention+time_after+net, data=sim_data)
  
  #p-values
  coefs <- summary(model)$coefficients
  level_p <- coefs["intervention", "Pr(>|t|)"]
  slope_p <- coefs["time_after", "Pr(>|t|)"]
  
  return(c(level_p=level_p, slope_p=slope_p))
}

################################################################################
#Type I error simulations------
#netherlands----
netherlands_npre <- nrow(netherlands[netherlands$year<2005,])
netherlands_npost <- nrow(netherlands[netherlands$year>=2005,])
netherlands_data <- subset(netherlands, year<2005)
netherlands_model <- lm(netherlands~year, data=netherlands_data)
acf(residuals(netherlands_model))
pacf(residuals(netherlands_model))
netherlands_slope_pre <- coef(netherlands_model)[["year"]]
netherlands_sigma <- sd(residuals(netherlands_model))

set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- 0
  level_change <- 0  
  
  #Run simulation
  p_vals <- run_simulation(netherlands_npre, netherlands_npost, 
                           netherlands_slope_pre, level_change, slope_change, 
                           netherlands_sigma, netherlands$import, 
                           netherlands$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Type I error:", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")


#germany----
germany_npre <- nrow(germany[germany$year<2005,])
germany_npost <- nrow(germany[germany$year>=2005,])
germany_data <- subset(germany, year<2005)
germany_model <- lm(germany~year, data=germany_data)
acf(residuals(germany_model))  
pacf(residuals(germany_model))
germany_slope_pre <- coef(germany_model)[["year"]]
germany_sigma <- sd(residuals(germany_model))

set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- 0 
  level_change <- 0 
  
  #Run simulation
  p_vals <- run_simulation(germany_npre, germany_npost, germany_slope_pre, 
                           level_change, slope_change, germany_sigma, 
                           germany$import, germany$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Type I error:", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")


#portugal----
portugal_npre <- nrow(portugal[portugal$year<2007,])
portugal_npost <- nrow(portugal[portugal$year>=2007,])
portugal_data <- subset(portugal, year<2007)
portugal_model <- lm(portugal~year, data=portugal_data)
acf(residuals(portugal_model))  
pacf(residuals(portugal_model))
portugal_slope_pre <- coef(portugal_model)[["year"]]
portugal_sigma <- sd(residuals(portugal_model))

set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- 0 
  level_change <- 0 
  
  #Run simulation
  p_vals <- run_simulation(portugal_npre, portugal_npost, portugal_slope_pre, 
                           level_change, slope_change, portugal_sigma, 
                           portugal$import, portugal$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Type I error:", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")


#spain----
spain_npre <- nrow(spain[spain$year<2006,])
spain_npost <- nrow(spain[spain$year>=2006,])
spain_data <- subset(spain, year<2006)
spain_model <- lm(spain~year, data=spain_data)
acf(residuals(spain_model))  
pacf(residuals(spain_model))
spain_slope_pre <- coef(spain_model)[["year"]]
spain_sigma <- sd(residuals(spain_model))

set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- 0 
  level_change <- 0 
  
  #Run simulation
  p_vals <- run_simulation(spain_npre, spain_npost, spain_slope_pre, 
                           level_change, slope_change, spain_sigma, 
                           spain$import, spain$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Type I error:", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")


#denmark----
denmark_npre <- nrow(denmark[denmark$year<2005,])
denmark_npost <- nrow(denmark[denmark$year>=2005,])
denmark_data <- subset(denmark, year<2005)
denmark_model <- lm(denmark~year, data=denmark_data)
acf(residuals(denmark_model))  
pacf(residuals(denmark_model))
denmark_slope_pre <- coef(denmark_model)[["year"]]
denmark_sigma <- sd(residuals(denmark_model))

set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- 0 
  level_change <- 0 
  
  #Run simulation
  p_vals <- run_simulation(denmark_npre, denmark_npost, denmark_slope_pre, 
                           level_change, slope_change, denmark_sigma, 
                           denmark$import, denmark$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Type I error:", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")


#belgium----
belgium_npre <- nrow(belgium[belgium$year<2005,])
belgium_npost <- nrow(belgium[belgium$year>=2005,])
belgium_data <- subset(belgium, year<2005)
belgium_model <- lm(belgium~year, data=belgium_data)
acf(residuals(belgium_model)) #no 
pacf(residuals(belgium_model))
belgium_slope_pre <- coef(belgium_model)[["year"]]
belgium_sigma <- sd(residuals(belgium_model))

set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- 0 
  level_change <- 0 
  
  #Run simulation
  p_vals <- run_simulation(belgium_npre, belgium_npost, belgium_slope_pre, 
                           level_change, slope_change, belgium_sigma, 
                           belgium$import, belgium$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Type I error:", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")


#france----
france_npre <- nrow(france[france$year<2005,])
france_npost <- nrow(france[france$year>=2005,])
france_data <- subset(france, year<2005)
france_model <- lm(france~year, data=france_data)
acf(residuals(france_model)) #no 
pacf(residuals(france_model))
france_slope_pre <- coef(france_model)[["year"]]
france_sigma <- sd(residuals(france_model))

set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- 0 
  level_change <- 0 
  
  #Run simulation
  p_vals <- run_simulation(france_npre, france_npost, france_slope_pre, 
                           level_change, slope_change, france_sigma, 
                           france$import, france$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Type I error:", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")


#italy----
italy_npre <- nrow(italy[italy$year<2006,])
italy_npost <- nrow(italy[italy$year>=2006,])
italy_data <- subset(italy, year<2006)
italy_model <- lm(italy~year, data=italy_data)
acf(residuals(italy_model)) #no 
pacf(residuals(italy_model))
italy_slope_pre <- coef(italy_model)[["year"]]
italy_sigma <- sd(residuals(italy_model))

set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- 0 
  level_change <- 0 
  
  #Run simulation
  p_vals <- run_simulation(italy_npre, italy_npost, italy_slope_pre, 
                           level_change, slope_change, italy_sigma, 
                           italy$import, italy$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Type I error:", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")


#luxembourg----
luxembourg_npre <- nrow(luxembourg[luxembourg$year<2006,])
luxembourg_npost <- nrow(luxembourg[luxembourg$year>=2006,])
luxembourg_data <- subset(luxembourg, year<2006)
luxembourg_model <- lm(luxembourg~year, data=luxembourg_data)
acf(residuals(luxembourg_model)) #no 
pacf(residuals(luxembourg_model))
luxembourg_slope_pre <- coef(luxembourg_model)[["year"]]
luxembourg_sigma <- sd(residuals(luxembourg_model))

set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- 0 
  level_change <- 0 
  
  #Run simulation
  p_vals <- run_simulation(luxembourg_npre, luxembourg_npost, luxembourg_slope_pre, 
                           level_change, slope_change, luxembourg_sigma, 
                           luxembourg$import, luxembourg$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Type I error:", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")


#austria----
austria_npre <- nrow(austria[austria$year<2007,])
austria_npost <- nrow(austria[austria$year>=2007,])
austria_data <- subset(austria, year<2007)
austria_model <- lm(austria~year, data=austria_data)
acf(residuals(austria_model)) #no 
pacf(residuals(austria_model))
austria_slope_pre <- coef(austria_model)[["year"]]
austria_sigma <- sd(residuals(austria_model))

set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- 0 
  level_change <- 0 
  
  #Run simulation
  p_vals <- run_simulation(austria_npre, austria_npost, austria_slope_pre, 
                           level_change, slope_change, austria_sigma, 
                           austria$import, austria$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Type I error:", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")


#finland----
finland_npre <- nrow(finland[finland$year<2005,])
finland_npost <- nrow(finland[finland$year>=2005,])
finland_data <- subset(finland, year<2005)
finland_model <- lm(finland~year, data=finland_data)
acf(residuals(finland_model)) #no 
pacf(residuals(finland_model))
finland_slope_pre <- coef(finland_model)[["year"]]
finland_sigma <- sd(residuals(finland_model))

set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- 0 
  level_change <- 0 
  
  #Run simulation
  p_vals <- run_simulation(finland_npre, finland_npost, finland_slope_pre, 
                           level_change, slope_change, finland_sigma, 
                           finland$import, finland$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Type I error:", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")


#sweden----
sweden_npre <- nrow(sweden[sweden$year<2005,])
sweden_npost <- nrow(sweden[sweden$year>=2005,])
sweden_data <- subset(sweden, year<2005)
sweden_model <- lm(sweden~year, data=sweden_data)
acf(residuals(sweden_model))  
pacf(residuals(sweden_model))
sweden_slope_pre <- coef(sweden_model)[["year"]]
sweden_sigma <- sd(residuals(sweden_model))

set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- 0 
  level_change <- 0 
  
  #Run simulation
  p_vals <- run_simulation(sweden_npre, sweden_npost, sweden_slope_pre, 
                           level_change, slope_change, sweden_sigma, 
                           sweden$import, sweden$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Type I error:", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")

#ireland----
ireland_npre <- nrow(ireland[ireland$year<2007,])
ireland_npost <- nrow(ireland[ireland$year>=2007,])
ireland_data <- subset(ireland, year<2007)
ireland_model <- lm(ireland~year, data=ireland_data)
acf(residuals(ireland_model))  
pacf(residuals(ireland_model))
ireland_slope_pre <- coef(ireland_model)[["year"]]
ireland_sigma <- sd(residuals(ireland_model))

set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- 0 
  level_change <- 0 
  
  #Run simulation
  p_vals <- run_simulation(ireland_npre, ireland_npost, ireland_slope_pre, 
                           level_change, slope_change, ireland_sigma, 
                           ireland$import, ireland$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Type I error:", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")


################################################################################
#Power simulations-----
#netherlands----
set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- rnorm(1, 1.5, 1.5)
  level_change <- rnorm(1, 6, 3)
  
  #Run simulation
  p_vals <- run_simulation(netherlands_npre, netherlands_npost, 
                           netherlands_slope_pre, level_change, slope_change, 
                           netherlands_sigma, netherlands$import, 
                           netherlands$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Power (either change):", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")


#germany----
set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- rnorm(1, 1.5, 1.5)
  level_change <- rnorm(1, 6, 3)
  
  #Run simulation
  p_vals <- run_simulation(germany_npre, germany_npost, germany_slope_pre, 
                           level_change, slope_change, germany_sigma, 
                           germany$import, germany$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Power (either change):", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")


#portugal----
set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- rnorm(1, 1.5, 1.5)
  level_change <- rnorm(1, 6, 3)
  
  #Run simulation
  p_vals <- run_simulation(portugal_npre, portugal_npost, portugal_slope_pre, 
                           level_change, slope_change, portugal_sigma, 
                           portugal$import, portugal$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Power (either change):", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")


#spain----
set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- rnorm(1, 1.5, 1.5)
  level_change <- rnorm(1, 6, 3)
  
  #Run simulation
  p_vals <- run_simulation(spain_npre, spain_npost, spain_slope_pre, 
                           level_change, slope_change, spain_sigma, 
                           spain$import, spain$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Power (either change):", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")


#denmark----
set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- rnorm(1, 1.5, 1.5)
  level_change <- rnorm(1, 6, 3)
  
  #Run simulation
  p_vals <- run_simulation(denmark_npre, denmark_npost, denmark_slope_pre, 
                           level_change, slope_change, denmark_sigma, 
                           denmark$import, denmark$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Power (either change):", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")


#belgium----
set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- rnorm(1, 1.5, 1.5)
  level_change <- rnorm(1, 6, 3)
  
  #Run simulation
  p_vals <- run_simulation(belgium_npre, belgium_npost, belgium_slope_pre, 
                           level_change, slope_change, belgium_sigma, 
                           belgium$import, belgium$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Power (either change):", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")


#france----
set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- rnorm(1, 1.5, 1.5)
  level_change <- rnorm(1, 6, 3)
  
  #Run simulation
  p_vals <- run_simulation(france_npre, france_npost, france_slope_pre, 
                           level_change, slope_change, france_sigma, 
                           france$import, france$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Power (either change):", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")


#italy----
set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- rnorm(1, 1.5, 1.5)
  level_change <- rnorm(1, 6, 3)
  
  #Run simulation
  p_vals <- run_simulation(italy_npre, italy_npost, italy_slope_pre, 
                           level_change, slope_change, italy_sigma, 
                           italy$import, italy$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Power (either change):", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")


#luxembourg----
set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- rnorm(1, 1.5, 1.5)
  level_change <- rnorm(1, 6, 3)
  
  #Run simulation
  p_vals <- run_simulation(luxembourg_npre, luxembourg_npost, luxembourg_slope_pre, 
                           level_change, slope_change, luxembourg_sigma, 
                           luxembourg$import, luxembourg$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Power (either change):", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")


#austria----
set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- rnorm(1, 1.5, 1.5)
  level_change <- rnorm(1, 6, 3)
  
  #Run simulation
  p_vals <- run_simulation(austria_npre, austria_npost, austria_slope_pre, 
                           level_change, slope_change, austria_sigma, 
                           austria$import, austria$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Power (either change):", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")


#finland----
set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- rnorm(1, 1.5, 1.5)
  level_change <- rnorm(1, 6, 3)
  
  #Run simulation
  p_vals <- run_simulation(finland_npre, finland_npost, finland_slope_pre, 
                           level_change, slope_change, finland_sigma, 
                           finland$import, finland$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Power (either change):", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")


#sweden----
set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- rnorm(1, 1.5, 1.5)
  level_change <- rnorm(1, 6, 3)
  
  #Run simulation
  p_vals <- run_simulation(sweden_npre, sweden_npost, sweden_slope_pre, 
                           level_change, slope_change, sweden_sigma, 
                           sweden$import, sweden$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Power (either change):", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")

#ireland----
set.seed(20229798)
sim_data <- data.frame(seed=abs(round(rnorm(10000)*10000)), p_val_level=NA,
                       p_val_slope=NA,slope_change=NA, level_change=NA)
for (i in 1:nrow(sim_data)){
  set.seed(sim_data[i, "seed"])
  
  #sample from European parameters
  slope_change <- rnorm(1, 1.5, 1.5)
  level_change <- rnorm(1, 6, 3)
  
  #Run simulation
  p_vals <- run_simulation(ireland_npre, ireland_npost, ireland_slope_pre, 
                           level_change, slope_change, ireland_sigma, 
                           ireland$import, ireland$export)
  
  sim_data[i, "p_val_level"] <- p_vals["level_p"]
  sim_data[i, "p_val_slope"] <- p_vals["slope_p"]
  sim_data[i, "slope_change"] <- slope_change
  sim_data[i, "level_change"] <- level_change
}

power_either <- mean((sim_data$p_val_level<0.025) | (sim_data$p_val_slope<0.025))
ci_either <- binom.test(sum((sim_data$p_val_level<0.025)|(sim_data$p_val_slope<0.025)), nrow(sim_data))$conf.int
cat("Power (either change):", round(power_either, 5), "[", round(ci_either[1], 5), ",", round(ci_either[2], 5), "]", "\n")
