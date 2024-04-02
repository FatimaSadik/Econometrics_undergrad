library(PoEdata)
library(AER)
library("plm")
data("nls_panel")
pooled<-lm(lwage~educ+exper+exper2+tenure+tenure2+black+south+union,data=nls_panel)
coeftest(pooled, vcov. = vcovHC, type = "HC1")
data("nls_panel10")
fixedeffect<-plm(lwage ~ exper+exper2+tenure+tenure2+union,data=nls_panel,index = c("id", "year"),
                    model = "within")
coeftest(fixedeffect, vcov. = vcovHC, type = "HC1")
randomeffect<-plm(lwage~educ+exper+exper2+tenure+tenure2+black+south+union,data=nls_panel,index = c("id", "year"),
                  model = "random")
coeftest(randomeffect, vcov. = vcovHC, type = "HC1")
# Perform the Hausman test
hausman_test <- phtest(fixedeffect, randomeffect)
print(hausman_test)
