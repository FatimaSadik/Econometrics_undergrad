#C2


#faminc    cigtax    cigprice  bwght     fatheduc  motheduc  parity    male     
#white     cigs      lbwght    bwghtlbs  packs     lfaminc   

#Obs:  1388

#1. faminc                   1988 family income, $1000s
#2. cigtax                   cig. tax in home state, 1988
#3. cigprice                 cig. price in home state, 1988
#4. bwght                    birth weight, ounces
#5. fatheduc                 father's yrs of educ
#6. motheduc                 mother's yrs of educ
#7. parity                   birth order of child
#8. male                     =1 if male child
#9. white                    =1 if white
#10. cigs                     cigs smked per day while preg
#11. lbwght                   log of bwght
#12. bwghtlbs                 birth weight, pounds
#13. packs                    packs smked per day while preg
#14. lfaminc                  log(faminc)

library(wooldridge)
data("bwght")
n_women <- nrow(bwght)                   # total sample size
n_smokers <- sum(bwght$cigs > 0, na.rm = TRUE)  # women who smoked (cigs > 0)

c(total_women = n_women, smokers = n_smokers)
print(list(total_women = n_women, smokers = n_smokers))

avg_cigs <- mean(bwght$cigs, na.rm = TRUE)
median_cigs <- median(bwght$cigs, na.rm = TRUE)

c(mean = avg_cigs, median = median_cigs)


avg_cigs_smokers <- mean(bwght$cigs[bwght$cigs > 0], na.rm = TRUE)
avg_cigs_smokers

avg_fatheduc <- mean(bwght$fatheduc, na.rm = TRUE)
n_fatheduc <- sum(!is.na(bwght$fatheduc))

c(mean_fatheduc = avg_fatheduc, n_used = n_fatheduc)
avg_faminc <- mean(bwght$faminc, na.rm = TRUE)
sd_faminc  <- sd(bwght$faminc, na.rm = TRUE)

c(mean_income = avg_faminc, sd_income = sd_faminc)


