#Copied from ITER BY Christoph Hanck, Martin Arnold, Alexander Gerber, and Martin Schmelzer
library(AER)
data(Fatalities)
# define the fatality rate
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000
# subset the data
Fatalities1982 <- subset(Fatalities, year == "1982")
Fatalities1988 <- subset(Fatalities, year == "1988")
plot(Fatalities1982$fatal_rate, Fatalities1982$beertax)
plot(Fatalities1988$fatal_rate, Fatalities1988$beertax)
# estimate simple regression models using 1982 and 1988 data
fatal1982_mod <- lm(fatal_rate ~ beertax, data = Fatalities1982)
fatal1988_mod <- lm(fatal_rate ~ beertax, data = Fatalities1988)
coeftest(fatal1982_mod, vcov. = vcovHC, type = "HC1")

# compute the differences
diff_fatal_rate <- Fatalities1988$fatal_rate - Fatalities1982$fatal_rate
diff_beertax <- Fatalities1988$beertax - Fatalities1982$beertax
# estimate a regression using differenced data
fatal_diff_mod <- lm(diff_fatal_rate ~ diff_beertax)
coeftest(fatal_diff_mod, vcov = vcovHC, type = "HC1")

# plot the differenced data
plot(x = diff_beertax,
     y = diff_fatal_rate,
     xlab = "Change in beer tax (in 1988 dollars)",
     ylab = "Change in fatality rate (fatalities per 10000)",
     main = "Changes in Traffic Fatality Rates and Beer Taxes in 1982-1988",
     xlim = c(-0.6, 0.6),
     ylim = c(-1.5, 1),
     pch = 20,
     col = "steelblue")
# add the regression line to plot
abline(fatal_diff_mod, lwd = 1.5)

install.packages("plm")
library("plm")

#We can simply use the function lm() to obtain an estimate of 1.
fatal_fe_lm_mod <- lm(fatal_rate ~ beertax + state - 1, data = Fatalities)
fatal_fe_lm_mod

# obtain demeaned data
Fatalities_demeaned <- with(Fatalities,
                            data.frame(fatal_rate = fatal_rate - ave(fatal_rate, state),
                                       beertax = beertax - ave(beertax, state)))
# estimate the regression
summary(lm(fatal_rate ~ beertax - 1, data = Fatalities_demeaned))

# estimate the fixed effects regression with plm()
fatal_fe_mod <- plm(fatal_rate ~ beertax,
                    data = Fatalities,
                    index = c("state", "year"),
                    model = "within")

# print summary using robust standard errors
coeftest(fatal_fe_mod, vcov. = vcovHC, type = "HC1")
 
##time fixed effects

fatal_tfe_lm_mod <- lm(fatal_rate ~ beertax + year - 1, data = Fatalities)
fatal_tfe_lm_mod
# print summary using robust standard errors
coeftest(fatal_tfe_mod, vcov. = vcovHC, type = "HC1")

##time and state fixed effects both

fatal_stfe_lm_mod <- lm(fatal_rate ~ beertax + state+year - 1, data = Fatalities)
fatal_stfe_lm_mod
# print summary using robust standard errors
coeftest(fatal_stfe_mod, vcov. = vcovHC, type = "HC1")

# via plm()
fatal_tefe_mod <- plm(fatal_rate ~ beertax,
                      data = Fatalities,
                      index = c("state", "year"),
                      model = "within",
                      effect = "twoways")
coeftest(fatal_tefe_mod, vcov = vcovHC, type = "HC1")