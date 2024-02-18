**Simulation exercise in stata**
//the program will generate data and then run ols regression
program reg_sim, eclass
drop _all
set obs 500
gen x=rnormal()
gen y=1+3*x+rnormal(0,1)
regress y x
end

simulate _b _se, reps(1000): reg_sim
// using the reg_sim program 1000 times coefficients and s.e are collected
histogram _b_x, bin(10) frequency
//the hist for b1 will be centered around 3 and for b0 will be centered around at 1
histogram _b_cons, bin(10) frequency
