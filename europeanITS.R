#Loading in necessary packages----
library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())
options(scipen=999) #setting scientific notation


#load in data and select countries with full data----
#PPW recycling rates
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
ireland <- europe[, c('year', 'ireland')]

imports <- read_excel("import full.xlsx")
exports <- read_excel("export full.xlsx")
generation <- read_excel("generation full.xlsx")
generation <- generation %>%
  mutate(across(everything(), as.double))

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

#calculate net imports
netherlands$net <- netherlands$import-netherlands$export
germany$net <- germany$import-germany$export
portugal$net <- portugal$import-portugal$export
spain$net <- spain$spain-ireland$export
denmark$net <- denmark$import-denmark$export
belgium$net <- belgium$import-belgium$export
greece$net <- greece$import-greece$export
france$net <- france$import-france$export
italy$net <- italy$import-italy$export
luxembourg$net <- luxembourg$import-luxembourg$export
austria$net <- austria$import-austria$export
finland$net <- finland$import-finland$export
sweden$net <- sweden$import-sweden$export
ireland$net <- ireland$import-ireland$export


#The netherlands (2005)-----
netherlands$time <- netherlands$year-1999
netherlands$intervention <- ifelse(netherlands$year>=2005, 1, 0)
netherlands$time_after <- ifelse(netherlands$year>=2005, netherlands$year-2005, 0)

netherlands_its <- lm(netherlands~time+intervention+time_after+net, 
                      data=netherlands)
summary(netherlands_its)
confint(netherlands_its, level=0.95)

plot(y=netherlands$netherlands, x=netherlands$year, main='Netherlands', 
     xlab='Year', ylab='%', type='b')
lines(y=fitted(netherlands_its), x=netherlands$year, type='b', col='red')
abline(v=2005, col='red')

plot(y=residuals(netherlands_its), fitted(netherlands_its))

#germany(2005)----
germany$time <- germany$year-1999
germany$intervention <- ifelse(germany$year>=2005, 1, 0)
germany$time_after <- ifelse(germany$year>=2005, germany$year-2005, 0)

germany_its <- lm(germany~time+intervention+time_after+net, data=germany)
summary(germany_its)
confint(germany_its, level=0.95)

plot(y=germany$germany, x=germany$year, main='Germany', xlab='Year', ylab='%',
     type='b')
lines(y=fitted(germany_its), x=germany$year, col='red', type='b')
abline(v=2005, col='red')

plot(y=residuals(germany_its), fitted(germany_its))

#portugal (2007)----
portugal$time <- portugal$year-1999
portugal$intervention <- ifelse(portugal$year>=2007, 1, 0)
portugal$time_after <- ifelse(portugal$year>=2007, portugal$year-2007, 0)

portugal_its <- lm(portugal~time+intervention+time_after+net, data=portugal)
summary(portugal_its)
confint(portugal_its)


plot(y=portugal$portugal, x=portugal$year, main='Portugal', xlab='Year', 
     ylab='%', type='b')
lines(y=fitted(portugal_its), x=portugal$year, type='b', col='red')
abline(v=2007, col='red')

plot(y=residuals(portugal_its), fitted(portugal_its))

#spain(2006)----
spain$time <- spain$year-1999
spain$intervention <- ifelse(spain$year>=2006, 1, 0)
spain$time_after <- ifelse(spain$year>=2006, spain$year-2006, 0)

spain_its <- lm(spain~time+intervention+time_after+net, data=spain)
summary(spain_its)
confint(spain_its)

plot(y=spain$spain, x=spain$year, main='Spain', xlab='Year', ylab='%', type='b')
lines(y=fitted(spain_its), x=spain$year, type='b', col='red')
abline(v=2006,col='red')

plot(y=residuals(spain_its), fitted(spain_its))

#Denmark (2005)----
denmark$time <- denmark$year-1999
denmark$intervention <- ifelse(denmark$year>=2005, 1, 0)
denmark$time_after <- ifelse(denmark$year>=2005, denmark$year-2005, 0)

denmark_its <- lm(denmark~time+intervention+time_after+net, data=denmark)
summary(denmark_its)
confint(denmark_its, level=0.95)

plot(y=denmark$denmark, x=denmark$year, main='Denmark', xlab='Year', ylab='%',
     type='b')
lines(y=fitted(denmark_its), x=denmark$year, col='red', type='b')
abline(v=2005, col='red')

plot(y=residuals(denmark_its), fitted(denmark_its))

#belgium (2005)----
belgium$time <- belgium$year-1999
belgium$intervention <- ifelse(belgium$year>=2005, 1, 0)
belgium$time_after <- ifelse(belgium$year>=2005, belgium$year-2005, 0)

belgium_its <- lm(belgium~time+intervention+time_after+net, data=belgium)
summary(belgium_its)
confint(belgium_its, level=0.95)

plot(y=belgium$belgium, x=belgium$year, main='Belgium', xlab='Year', ylab='%',
     type='b')
lines(y=fitted(belgium_its), x=belgium$year, col='red', type='b')
abline(v=2005, col='red')

plot(y=residuals(belgium_its), fitted(belgium_its))

#France(2005)----
france$time <- france$year-1999
france$intervention <- ifelse(france$year>=2005, 1, 0)
france$time_after <- ifelse(france$year>=2005, france$year-2005, 0)

france_its <- lm(france~time+intervention+time_after+net, data=france)
summary(france_its)
confint(france_its, level=0.95)

plot(y=france$france, x=france$year, main='France', xlab='Year', ylab='%', 
     type='b')
abline(v=2005, col='red')
lines(y=fitted(france_its), x=france$year, col='red', type='b')

plot(y=residuals(france_its), fitted(france_its))

#Italy(2006)----
italy$time <- italy$year-1999
italy$intervention <- ifelse(italy$year>=2006, 1, 0)
italy$time_after <- ifelse(italy$year>=2006, italy$year-2006, 0)

italy_its <- lm(italy~time+intervention+time_after+net, data=italy)
summary(italy_its)
confint(italy_its, level=0.95)

plot(y=italy$italy, x=italy$year, main='Italy', xlab='Year', ylab='%', 
     type='b')
abline(v=2006, col='red')
lines(y=fitted(italy_its), x=italy$year, col='red', type='b')

plot(y=residuals(italy_its), fitted(italy_its))

#luxembourg (2006)----
luxembourg$time <- luxembourg$year-1999
luxembourg$intervention <- ifelse(luxembourg$year>=2006, 1, 0)
luxembourg$time_after <- ifelse(luxembourg$year>=2006, luxembourg$year-2005, 0)

luxembourg_its <- lm(luxembourg~time+intervention+time_after+net, 
                     data=luxembourg)
summary(luxembourg_its)
confint(luxembourg_its, level=0.95)

plot(y=luxembourg$luxembourg, x=luxembourg$year, main='luxembourg', xlab='Year', 
     ylab='%', type='b')
abline(v=2006, col='red')
lines(y=fitted(luxembourg_its), x=luxembourg$year, col='red', type='b')

plot(y=residuals(luxembourg_its), fitted(luxembourg_its))

#austria (2007)----
austria$time <- austria$year-1999
austria$intervention <- ifelse(austria$year>=2007, 1, 0)
austria$time_after <- ifelse(austria$year>=2007, austria$year-2005, 0)

austria_its <- lm(austria~time+intervention+time_after+net, data=austria)
summary(austria_its)
confint(austria_its, level=0.95)

plot(y=austria$austria, x=austria$year, main='austria', xlab='Year', ylab='%', 
     type='b')
abline(v=2007, col='red')
lines(y=fitted(austria_its), x=austria$year, col='red', type='b')

plot(y=residuals(austria_its), fitted(austria_its))

#finland (2005)----
finland$time <- finland$year-1999
finland$intervention <- ifelse(finland$year>=2005, 1, 0)
finland$time_after <- ifelse(finland$year>=2005, finland$year-2005, 0)

finland_its <- lm(finland~time+intervention+time_after+net, data=finland)
summary(finland_its)
confint(finland_its, level=0.95)

plot(y=finland$finland, x=finland$year, main='finland', xlab='Year', ylab='%', 
     type='b')
abline(v=2005, col='red')
lines(y=fitted(finland_its), x=finland$year, col='red', type='b')

plot(y=residuals(finland_its), fitted(finland_its))

#sweden (2005)----
sweden$time <- sweden$year-1999
sweden$intervention <- ifelse(sweden$year>=2005, 1, 0)
sweden$time_after <- ifelse(sweden$year>=2005, sweden$year-2005, 0)

sweden_its <- lm(sweden~time+intervention+time_after+net, data=sweden)
summary(sweden_its)
confint(sweden_its, level=0.95)

plot(y=sweden$sweden, x=sweden$year, main='sweden', xlab='Year', ylab='%', 
     type='b')
abline(v=2005, col='red')
lines(y=fitted(sweden_its), x=sweden$year, col='red', type='b')

plot(y=residuals(sweden_its), fitted(sweden_its))

#ireland(2007)-----
ireland$time <- ireland$year-1999
ireland$intervention <- ifelse(ireland$year>=2007, 1, 0)
ireland$time_after <- ifelse(ireland$year>=2007, ireland$year-2007, 0)

ireland_its <- lm(ireland~time+intervention+time_after+net, data=ireland)
summary(ireland_its)
confint(ireland_its, level=0.95)

plot(y=ireland$ireland, x=ireland$year, main='Irish PPW Recycling Rate', xlab='Year', ylab='%', 
     type='b')
abline(v=2007, col='red')
lines(y=fitted(ireland_its), x=ireland$year, col='red', type='b')


plot(y=residuals(ireland_its), fitted(ireland_its))


################################################################################
#volume trend analysis----
#the netherlands----
netherlands$generated <- generation$netherlands
netherlands$recycled <- netherlands$generated*(netherlands$netherlands/100)

pre_netherlands <- netherlands[netherlands$year<2005, ]
post_netherlands <- netherlands[netherlands$year>=2005, ]
post_netherlands$time <- post_netherlands$time-min(post_netherlands$time)#makes sure the post time starts on 0

rec_pre_model_netherlands <- lm(recycled~time, data=pre_netherlands)
rec_post_model_netherlands <- lm(recycled~time, data=post_netherlands)
summary(rec_pre_model_netherlands)
summary(rec_post_model_netherlands)

plot(y=netherlands$recycled, x=netherlands$year, xlab='Year', 
     ylab='Volume (Tonnes)', main='Netherlands Recycling', type='b')
lines(y=fitted(rec_pre_model_netherlands), x=pre_netherlands$year, col='red', 
      lwd=2, lty=5)
lines(y=fitted(rec_post_model_netherlands), x=post_netherlands$year, col='red', 
      lwd=2, lty=5)
abline(v=2005, lty=2)

vol_pre_model_netherlands <- lm(generated~time, data=pre_netherlands)
vol_post_model_netherlands <- lm(generated~time, data=post_netherlands)
summary(vol_pre_model_netherlands)
summary(vol_post_model_netherlands)

plot(y=netherlands$generated, x=netherlands$year, xlab='Year',
     ylab='Volume (Tonnes)', main= 'Netherlands Generated', type='b')
lines(y=fitted(vol_pre_model_netherlands), x=pre_netherlands$year, col='red', 
      lwd=2, lty=5)
lines(y=fitted(vol_post_model_netherlands), x=post_netherlands$year, col='red', 
      lwd=2, lty=5)
abline(v=2005, lty=2)

#germany----
germany$generated <- generation$germany
germany$recycled <- germany$generated*(germany$germany/100)

pre_germany <- germany[germany$year<2005, ]
post_germany <- germany[germany$year>=2005, ]
post_germany$time <- post_germany$time-min(post_germany$time)

rec_pre_model_germany <- lm(recycled~year, data=pre_germany)
rec_post_model_germany <- lm(recycled~year, data=post_germany)
summary(rec_pre_model_germany)
summary(rec_post_model_germany)

plot(y=germany$recycled, x=germany$year, type='b', xlab='Year',
     ylab='Volume (tonnes)', main='Germany Recycling')
lines(y=fitted(rec_pre_model_germany), x=pre_germany$year, col='red', lwd=2, 
      lty=5)
lines(y=fitted(rec_post_model_germany), x=post_germany$year, col='red', lwd=2, 
      lty=5)
abline(v=2005, lty=2)

vol_pre_model_germany <- lm(generated~year, data=pre_germany)
vol_post_model_germany <- lm(generated~year, data=post_germany)
summary(vol_pre_model_germany)
summary(vol_post_model_germany)

plot(y=germany$generated, x=germany$year, type='b', xlab='Year',
     ylab='Volume (tonnes)', main='Germany Generation')
lines(y=fitted(vol_pre_model_germany), x=pre_germany$year, col='red', lwd=2, 
      lty=5)
lines(y=fitted(vol_post_model_germany), x=post_germany$year, col='red', lwd=2, 
      lty=5)
abline(v=2005, lty=2)

#Portugal ----
portugal$generated <- generation$portugal
portugal$recycled <- portugal$generated*(portugal$portugal/100)

pre_portugal <- portugal[portugal$year<2007, ]
post_portugal <- portugal[portugal$year>=2007, ]
post_portugal$time <- post_portugal$time-min(post_portugal$time)

rec_pre_portugal_model <- lm(recycled~time, data=pre_portugal)
rec_post_portugal_model <- lm(recycled~time, data=post_portugal)
summary(rec_pre_portugal_model)
summary(rec_post_portugal_model)

plot(y=portugal$recycled, x=portugal$year, type='b', xlab='Year',
     ylab='Volume (tonnes)', main='Portugal Recycling')
lines(y=fitted(rec_pre_portugal_model), x=pre_portugal$year, col='red', lwd=2, 
      lty=5)
lines(y=fitted(rec_post_portugal_model), x=post_portugal$year, col='red', lwd=2, 
      lty=5)
abline(v=2007, lty=2)

vol_pre_model_portugal <- lm(generated~year, data=pre_portugal)
vol_post_model_portugal <- lm(generated~year, data=post_portugal)
summary(vol_pre_model_portugal)
summary(vol_post_model_portugal)

plot(y=portugal$generated, x=portugal$year, type='b', xlab='Year',
     ylab='Volume (tonnes)', main='Portugal Generation')
lines(y=fitted(vol_pre_model_portugal), x=pre_portugal$year, col='red', lwd=2, 
      lty=5)
lines(y=fitted(vol_post_model_portugal), x=post_portugal$year, col='red', lwd=2, 
      lty=5)
abline(v=2007, lty=2)

#spain----
spain$generated <- generation$spain
spain$recycled <- spain$generated*(spain$spain/100)

pre_spain <- spain[spain$year<2006, ]
post_spain <- spain[spain$year>=2006, ]
post_spain$time <- post_spain$time-min(post_spain$time)

rec_pre_spain_model <- lm(recycled~time, data=pre_spain)
rec_post_spain_model <- lm(recycled~time, data=post_spain)
summary(rec_pre_spain_model)
summary(rec_post_spain_model)

plot(y=spain$recycled, x=spain$year, type='b', xlab='Year', 
     ylab='Volume (tonnes)', main='Spain Recycling')
lines(y=fitted(rec_pre_spain_model), x=pre_spain$year, col='red', lwd=2, 
      lty=5)
lines(y=fitted(rec_post_spain_model), x=post_spain$year, col='red', lwd=2, 
      lty=5)
abline(v=2006, lty=2)

vol_pre_spain_model <- lm(generated~time, data=pre_spain)
vol_post_spain_model <- lm(generated~time, data=post_spain)
summary(vol_pre_spain_model)
summary(vol_post_spain_model)

plot(y=spain$generated, x=spain$year, type='b', xlab='Year', 
     ylab='Volume (tonnes)', main='Spain Generated')
lines(y=fitted(vol_pre_spain_model), x=pre_spain$year, col='red', lwd=2, 
      lty=5)
lines(y=fitted(vol_post_spain_model), x=post_spain$year, col='red', lwd=2, 
      lty=5)
abline(v=2006, lty=2)

#denmark----
denmark$generated <- generation$denmark
denmark$recycled <- denmark$generated*(denmark$denmark/100)

pre_denmark <- denmark[denmark$year<2005, ]
post_denmark <- denmark[denmark$year>=2005, ]
post_denmark$time <- post_denmark$time-min(post_denmark$time)

rec_pre_denmark_model <- lm(recycled~year, data=pre_denmark)
rec_post_denmark_model <- lm(recycled~year, data=post_denmark)
summary(rec_pre_denmark_model)
summary(rec_post_denmark_model)

plot(y=denmark$recycled, x=denmark$year, type='b', xlab='Year', 
     ylab='Volume (tonnes)', main='Denmark Recycling')
lines(y=fitted(rec_pre_denmark_model), x=pre_denmark$year, col='red', lwd=2, 
      lty=5)
lines(y=fitted(rec_post_denmark_model), x=post_denmark$year, col='red', lwd=2, 
      lty=5)
abline(v=2005, lty=2)

vol_pre_denmark_model <- lm(generated~year, data=pre_denmark)
vol_post_denmark_model <- lm(generated~year, data=post_denmark)
summary(vol_pre_denmark_model)
summary(vol_post_denmark_model)

plot(y=denmark$generated, x=denmark$year, type='b', xlab='Year', 
     ylab='Volume (tonnes)', main='Denmark Generated')
lines(y=fitted(vol_pre_denmark_model), x=pre_denmark$year, col='red', lwd=2, 
      lty=5)
lines(y=fitted(vol_post_denmark_model), x=post_denmark$year, col='red', lwd=2, 
      lty=5)
abline(v=2005, lty=2)

#belgium----
belgium$generated <- generation$belgium
belgium$recycled <- belgium$generated*(belgium$belgium/100)

pre_belgium <- belgium[belgium$year<2005, ]
post_belgium <- belgium[belgium$year>=2005, ]
post_belgium$time <- post_belgium$time-min(post_belgium$time)

rec_pre_belgium_model <- lm(recycled~year, data=pre_belgium)
rec_post_belgium_model <- lm(recycled~year, data=post_belgium)
summary(rec_pre_belgium_model)
summary(rec_post_belgium_model)

plot(y=belgium$recycled, x=belgium$year, type='b', xlab='Year', 
     ylab='Volume (tonnes)', main='Belgium Recycling')
lines(y=fitted(rec_pre_belgium_model), x=pre_belgium$year, col='red', lwd=2, 
      lty=5)
lines(y=fitted(rec_post_belgium_model), x=post_belgium$year, col='red', lwd=2, 
      lty=5)
abline(v=2005, lty=2)

vol_pre_belgium_model <- lm(generated~year, data=pre_belgium)
vol_post_belgium_model <- lm(generated~year, data=post_belgium)
summary(vol_pre_belgium_model)
summary(vol_post_belgium_model)

plot(y=belgium$generated, x=belgium$year, type='b', xlab='Year', 
     ylab='Volume (tonnes)', main='Belgium Generated')
lines(y=fitted(vol_pre_belgium_model), x=pre_belgium$year, col='red', lwd=2, 
      lty=5)
lines(y=fitted(vol_post_belgium_model), x=post_belgium$year, col='red', lwd=2, 
      lty=5)
abline(v=2005, lty=2)

#france----
france$generated <- generation$france
france$recycled <- france$generated*(france$france/100)

pre_france <- france[france$year<2005, ]
post_france <- france[france$year>=2005, ]
post_france$time <- post_france$time-min(post_france$time)

rec_pre_france_model <- lm(recycled~year, data=pre_france)
rec_post_france_model <- lm(recycled~year, data=post_france)
summary(rec_pre_france_model)
summary(rec_post_france_model)

plot(y=france$recycled, x=france$year, type='b', xlab='Year', 
     ylab='Volume (tonnes)', main='France Recycling')
lines(y=fitted(rec_pre_france_model), x=pre_france$year, col='red', lwd=2, 
      lty=5)
lines(y=fitted(rec_post_france_model), x=post_france$year, col='red', lwd=2, 
      lty=5)
abline(v=2005, lty=2)

vol_pre_france_model <- lm(generated~year, data=pre_france)
vol_post_france_model <- lm(generated~year, data=post_france)
summary(vol_pre_france_model)
summary(vol_post_france_model)

plot(y=france$generated, x=france$year, type='b', xlab='Year', 
     ylab='Volume (tonnes)', main='France Generated')
lines(y=fitted(vol_pre_france_model), x=pre_france$year, col='red', lwd=2, 
      lty=5)
lines(y=fitted(vol_post_france_model), x=post_france$year, col='red', lwd=2, 
      lty=5)
abline(v=2005, lty=2)

#italy----
italy$generated <- generation$italy
italy$recycled <- italy$generated*(italy$italy/100)

pre_italy <- italy[italy$year<2006, ]
post_italy <- italy[italy$year>=2006, ]
post_italy$time <- post_italy$time-min(post_italy$time)

rec_pre_model_italy <- lm(recycled~year, data=pre_italy)
rec_post_model_italy <- lm(recycled~year, data=post_italy)
summary(rec_pre_model_italy)
summary(rec_post_model_italy)

plot(y=italy$recycled, x=italy$year, type='b', xlab='Year',
     ylab='Volume (tonnes)', main='Italy Recycling')
lines(y=fitted(rec_pre_model_italy), x=pre_italy$year, col='red', lwd=2, 
      lty=5)
lines(y=fitted(rec_post_model_italy), x=post_italy$year, col='red', lwd=2, 
      lty=5)
abline(v=2006, lty=2)

vol_pre_model_italy <- lm(generated~year, data=pre_italy)
vol_post_model_italy <- lm(generated~year, data=post_italy)
summary(vol_pre_model_italy)
summary(vol_post_model_italy)

plot(y=italy$generated, x=italy$year, type='b', xlab='Year',
     ylab='Volume (tonnes)', main='Italy Generation')
lines(y=fitted(vol_pre_model_italy), x=pre_italy$year, col='red', lwd=2, 
      lty=5)
lines(y=fitted(vol_post_model_italy), x=post_italy$year, col='red', lwd=2, 
      lty=5)
abline(v=2006, lty=2)

#austria----
austria$generated <- generation$austria
austria$recycled <- austria$generated*(austria$austria/100)

pre_austria <- austria[austria$year<2007, ]
post_austria <- austria[austria$year>=2007, ]
post_austria$time <- post_austria$time-min(post_austria$time)

rec_pre_model_austria <- lm(recycled~year, data=pre_austria)
rec_post_model_austria <- lm(recycled~year, data=post_austria)
summary(rec_pre_model_austria)
summary(rec_post_model_austria)

plot(y=austria$recycled, x=austria$year, type='b', xlab='Year',
     ylab='Volume (tonnes)', main='Austria Recycling')
lines(y=fitted(rec_pre_model_austria), x=pre_austria$year, col='red', lwd=2, 
      lty=5)
lines(y=fitted(rec_post_model_austria), x=post_austria$year, col='red', lwd=2, 
      lty=5)
abline(v=2007, lty=2)

vol_pre_model_austria <- lm(generated~year, data=pre_austria)
vol_post_model_austria <- lm(generated~year, data=post_austria)
summary(vol_pre_model_austria)
summary(vol_post_model_austria)

plot(y=austria$generated, x=austria$year, type='b', xlab='Year',
     ylab='Volume (tonnes)', main='Austria Generation')
lines(y=fitted(vol_pre_model_austria), x=pre_austria$year, col='red', lwd=2, 
      lty=5)
lines(y=fitted(vol_post_model_austria), x=post_austria$year, col='red', lwd=2, 
      lty=5)
abline(v=2007, lty=2)

#finland----
finland$generated <- generation$finland
finland$recycled <- finland$generated*(finland$finland/100)

pre_finland <- finland[finland$year<2005, ]
post_finland <- finland[finland$year>=2005, ]
post_finland$time <- post_finland$time-min(post_finland$time)

rec_pre_model_finland <- lm(recycled~year, data=pre_finland)
rec_post_model_finland <- lm(recycled~year, data=post_finland)
summary(rec_pre_model_finland)
summary(rec_post_model_finland)

plot(y=finland$recycled, x=finland$year, type='b', xlab='Year',
     ylab='Volume (tonnes)', main='Finland Recycling')
lines(y=fitted(rec_pre_model_finland), x=pre_finland$year, col='red', lwd=2, 
      lty=5)
lines(y=fitted(rec_post_model_finland), x=post_finland$year, col='red', lwd=2, 
      lty=5)
abline(v=2005, lty=2)

vol_pre_model_finland <- lm(generated~year, data=pre_finland)
vol_post_model_finland <- lm(generated~year, data=post_finland)
summary(vol_pre_model_finland)
summary(vol_post_model_finland)

plot(y=finland$generated, x=finland$year, type='b', xlab='Year',
     ylab='Volume (tonnes)', main='Finland Generation')
lines(y=fitted(vol_pre_model_finland), x=pre_finland$year, col='red', lwd=2, 
      lty=5)
lines(y=fitted(vol_post_model_finland), x=post_finland$year, col='red', lwd=2,
      lty=5)
abline(v=2005, lty=2)

#sweden----
sweden$generated <- generation$sweden
sweden$recycled <- sweden$generated*(sweden$sweden/100)

pre_sweden <- sweden[sweden$year<2005, ]
post_sweden <- sweden[sweden$year>=2005, ]
post_sweden$time <- post_sweden$time-min(post_sweden$time)

rec_pre_model_sweden <- lm(recycled~year, data=pre_sweden)
rec_post_model_sweden <- lm(recycled~year, data=post_sweden)
summary(rec_pre_model_sweden)
summary(rec_post_model_sweden)

plot(y=sweden$recycled, x=sweden$year, type='b', xlab='Year',
     ylab='Volume (tonnes)', main='sweden Recycling')
lines(y=fitted(rec_pre_model_sweden), x=pre_sweden$year, col='red', lwd=2, 
      lty=5)
lines(y=fitted(rec_post_model_sweden), x=post_sweden$year, col='red', lwd=2, 
      lty=5)
abline(v=2005, lty=2)

vol_pre_model_sweden <- lm(generated~year, data=pre_sweden)
vol_post_model_sweden <- lm(generated~year, data=post_sweden)
summary(vol_pre_model_sweden)
summary(vol_post_model_sweden)

plot(y=sweden$generated, x=sweden$year, type='b', xlab='Year',
     ylab='Volume (tonnes)', main='Sweden Generation')
lines(y=fitted(vol_pre_model_sweden), x=pre_sweden$year, col='red', lwd=2, 
      lty=5)
lines(y=fitted(vol_post_model_sweden), x=post_sweden$year, col='red', lwd=2, 
      lty=5)
abline(v=2005, lty=2)

#ireland----
ireland$generated <- generation$ireland
ireland$recycled <- ireland$generated*(ireland$ireland/100)

pre_ireland <- ireland[ireland$year<2007,]
post_ireland <- ireland[ireland$year>=2007, ]
post_ireland$time <- post_ireland$time-(min(post_ireland$time)) #makes sure the post time starts on 0

rec_pre_model_ireland <- lm(recycled~time, data=pre_ireland)
rec_post_model_ireland <- lm(recycled~time, data=post_ireland)
summary(rec_pre_model_ireland)
summary(rec_post_model_ireland)

plot(y=ireland$recycled, x=ireland$year, type='b', xlab='Year',
     ylab='Volume (tonnes)', main='Ireland Recycling')
lines(y=fitted(rec_pre_model_ireland), x=pre_ireland$year, col='red', lwd=2, 
      lty=5)
lines(y=fitted(rec_post_model_ireland), x=post_ireland$year, col='red', lwd=2, 
      lty=5)
abline(v=2007, lty=2)

vol_pre_model_ireland <- lm(generated~time, data=pre_ireland)
vol_post_model_ireland <- lm(generated~time, data=post_ireland)
summary(vol_pre_model_ireland)
summary(vol_post_model_ireland)

plot(y=ireland$generated, x=ireland$year, type='b', xlab='Year',
     ylab='Volume (tonnes)', main='Ireland Generation')
lines(y=fitted(vol_pre_model_ireland), x=pre_ireland$year, col='red', lwd=2, 
      lty=5)
lines(y=fitted(vol_post_model_ireland), x=post_ireland$year, col='red', lwd=2, 
      lty=5)
abline(v=2007, lty=2)



################################################################################
#Plots for paper----

#combining the VTA plots----
pdf("volume_trend_panels.pdf", width = 12, height = 16)
par(mfrow = c(7, 2),
    mar = c(4, 5, 2, 1),
    cex.main = 2,
    cex.lab = 1.5,
    cex.axis = 1.5)
plot(y=netherlands$recycled, x=netherlands$year, xlab='Year', ylab='Tonnes', main='Netherlands Recycling', type='b')
lines(y=fitted(rec_pre_model_netherlands), x=pre_netherlands$year, col='red', lwd=2, lty=5)
lines(y=fitted(rec_post_model_netherlands), x=post_netherlands$year, col='red', lwd=2, lty=5)
abline(v=2005, lty=2)

plot(y=netherlands$generated, x=netherlands$year, xlab='Year',ylab='Tonnes', main= 'Netherlands Generation', type='b')
lines(y=fitted(vol_pre_model_netherlands), x=pre_netherlands$year, col='red', lwd=2, lty=5)
lines(y=fitted(vol_post_model_netherlands), x=post_netherlands$year, col='red', lwd=2, lty=5)
abline(v=2005, lty=2)


plot(y=germany$recycled, x=germany$year, type='b', xlab='Year',ylab='Tonnes', main='Germany Recycling')
lines(y=fitted(rec_pre_model_germany), x=pre_germany$year, col='red', lwd=2, lty=5)
lines(y=fitted(rec_post_model_germany), x=post_germany$year, col='red', lwd=2, lty=5)
abline(v=2005, lty=2)

plot(y=germany$generated, x=germany$year, type='b', xlab='Year',ylab='Tonnes', main='Germany Generation')
lines(y=fitted(vol_pre_model_germany), x=pre_germany$year, col='red', lwd=2, lty=5)
lines(y=fitted(vol_post_model_germany), x=post_germany$year, col='red', lwd=2, lty=5)
abline(v=2005, lty=2)


plot(y=spain$recycled, x=spain$year, type='b', xlab='Year', ylab='Tonnes', main='Spain Recycling')
lines(y=fitted(rec_pre_spain_model), x=pre_spain$year, col='red', lwd=2, lty=5)
lines(y=fitted(rec_post_spain_model), x=post_spain$year, col='red', lwd=2, lty=5)
abline(v=2006, lty=2)

plot(y=spain$generated, x=spain$year, type='b', xlab='Year', ylab='Tonnes', main='Spain Generation')
lines(y=fitted(vol_pre_spain_model), x=pre_spain$year, col='red', lwd=2, lty=5)
lines(y=fitted(vol_post_spain_model), x=post_spain$year, col='red', lwd=2, lty=5)
abline(v=2006, lty=2)


plot(y=belgium$recycled, x=belgium$year, type='b', xlab='Year', ylab='Tonnes', main='Belgium Recycling')
lines(y=fitted(rec_pre_belgium_model), x=pre_belgium$year, col='red', lwd=2, lty=5)
lines(y=fitted(rec_post_belgium_model), x=post_belgium$year, col='red', lwd=2, lty=5)
abline(v=2005, lty=2)

plot(y=belgium$generated, x=belgium$year, type='b', xlab='Year', ylab='Tonnes', main='Belgium Generation')
lines(y=fitted(vol_pre_belgium_model), x=pre_belgium$year, col='red', lwd=2, lty=5)
lines(y=fitted(vol_post_belgium_model), x=post_belgium$year, col='red', lwd=2, lty=5)
abline(v=2005, lty=2)


plot(y=france$recycled, x=france$year, type='b', xlab='Year', ylab='Tonnes', main='France Recycling')
lines(y=fitted(rec_pre_france_model), x=pre_france$year, col='red', lwd=2, lty=5)
lines(y=fitted(rec_post_france_model), x=post_france$year, col='red', lwd=2, lty=5)
abline(v=2005, lty=2)

plot(y=france$generated, x=france$year, type='b', xlab='Year', ylab='Tonnes', main='France Generation')
lines(y=fitted(vol_pre_france_model), x=pre_france$year, col='red', lwd=2, lty=5)
lines(y=fitted(vol_post_france_model), x=post_france$year, col='red', lwd=2, lty=5)
abline(v=2005, lty=2)

plot(y=austria$recycled, x=austria$year, type='b', xlab='Year',ylab='Tonnes', main='Austria Recycling')
lines(y=fitted(rec_pre_model_austria), x=pre_austria$year, col='red', lwd=2, lty=5)
lines(y=fitted(rec_post_model_austria), x=post_austria$year, col='red', lwd=2, lty=5)
abline(v=2007, lty=2)

plot(y=austria$generated, x=austria$year, type='b', xlab='Year',ylab='Tonnes', main='Austria Generation')
lines(y=fitted(vol_pre_model_austria), x=pre_austria$year, col='red', lwd=2, lty=5)
lines(y=fitted(vol_post_model_austria), x=post_austria$year, col='red', lwd=2, lty=5)
abline(v=2007, lty=2)


plot(y=ireland$recycled, x=ireland$year, type='b', xlab='Year',ylab='Tonnes', main='Ireland Recycling')
lines(y=fitted(rec_pre_model_ireland), x=pre_ireland$year, col='red', lwd=2, lty=5)
lines(y=fitted(rec_post_model_ireland), x=post_ireland$year, col='red', lwd=2, lty=5)
abline(v=2007, lty=2)

plot(y=ireland$generated, x=ireland$year, type='b', xlab='Year',ylab='Tonnes', main='Ireland Generation')
lines(y=fitted(vol_pre_model_ireland), x=pre_ireland$year, col='red', lwd=2, lty=5)
lines(y=fitted(vol_post_model_ireland), x=post_ireland$year, col='red', lwd=2, lty=5)
abline(v=2007, lty=2)
dev.off()

#combining the recycling rate plots----
pdf("rates.pdf", width = 12, height = 16)
par(mfrow = c(7, 2),
    mar = c(4, 5, 2, 1),
    cex.main = 2,
    cex.lab = 1.5,
    cex.axis = 1.5)
plot(y=netherlands$netherlands, x=netherlands$year, main='The Netherlands', 
     xlab='Year', ylab='%', type='b')
abline(v=2005, col='red')

plot(y=germany$germany, x=germany$year, main='Germany', xlab='Year', ylab='%',
     type='b')
abline(v=2005, col='red')

plot(y=portugal$portugal, x=portugal$year, main='Portugal', xlab='Year', 
     ylab='%', type='b')
abline(v=2007, col='red')

plot(y=spain$spain, x=spain$year, main='Spain', xlab='Year', ylab='%', type='b')
abline(v=2006,col='red')

plot(y=denmark$denmark, x=denmark$year, main='Denmark', xlab='Year', ylab='%',
     type='b')
abline(v=2005, col='red')

plot(y=belgium$belgium, x=belgium$year, main='Belgium', xlab='Year', ylab='%',
     type='b')
abline(v=2005, col='red')

plot(y=france$france, x=france$year, main='France', xlab='Year', ylab='%', 
     type='b')
abline(v=2005, col='red')

plot(y=italy$italy, x=italy$year, main='Italy', xlab='Year', ylab='%', 
     type='b')
abline(v=2006, col='red')

plot(y=luxembourg$luxembourg, x=luxembourg$year, main='Luxembourg', xlab='Year', 
     ylab='%', type='b')
abline(v=2006, col='red')

plot(y=austria$austria, x=austria$year, main='Austria', xlab='Year', ylab='%', 
     type='b')
abline(v=2007, col='red')

plot(y=finland$finland, x=finland$year, main='Finland', xlab='Year', ylab='%', 
     type='b')
abline(v=2005, col='red')

plot(y=sweden$sweden, x=sweden$year, main='Sweden', xlab='Year', ylab='%', 
     type='b')
abline(v=2005, col='red')

plot(y=ireland$ireland, x=ireland$year, main='Ireland', xlab='Year', ylab='%', 
     type='b')
abline(v=2007, col='red')

dev.off()

