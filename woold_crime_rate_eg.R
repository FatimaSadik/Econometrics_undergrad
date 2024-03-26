library("wooldridge")
data("crime2")
#simple cross_section of crime rate on unemployment
model1<-lm(crmrte~unem, data=crime2)
coeftest(model1, vcov. = vcovHC, type = "HC1")

#heterogeniety bias

model2<-lm(crmrte~unem+d87, data=crime2)
summary(model2)

#using pooled OLS on the two years has not substantially changed anything from using a single cross section.
# compute the differences
crime87<-subset(crime2,d87=="1")
crime82<-subset(crime2,d87=="0")
diff_crime_rate <- crime87$crmrte - crime82$crmrte
diff_unem <-crime87$unem - crime82$unem
diff_model<-lm(diff_crime_rate~diff_unem)
summary(diff_model)
