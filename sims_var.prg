'close @all
wfopen "sims_data"

'Selects a page in an open workfile where all the instructions will be applied from this point on: 
      pageselect   Sims
'trace the impact of shock to monetary policy, e.g. interest rate hike

'Transforms the variables that are included in the EVIEWS WORKFILE

genr ly = log(y)
genr lxr = log(xr)
genr lm = log(m1)
genr lp = log(cpi)
genr lcp = log(cmp)

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''''''''''''''''''''''''''''SIMS''''''''''''''''''''''''''''''''''''''''''

''Sets the sample period, estimates the VAR, generates the impulse responses (impulse), creates a graph with them (freeze) and transforms the graph (addtext)
'variables are federal funds rate (ff), M1(lm), CPI(lp) and Industrial production (ly)
'the policy varaible is ff
'First run
Smpl 1958:04 1991:02
var sims0.ls(nw) 1 14  ff  lm lp ly   
freeze(fig10sims) sims0.impulse(48,m)  ff  lm lp ly  @  ff  lm lp ly 
'format the output
fig10sims.addtext(t, font(40), [+/ b]) Fig 10 C. Sims, Interpreting…

show fig10sims

' looking at the first column of the impulse responses, the Fed Funds rate increases, then the quantity of money falls, output quickly falls, and prices go up a little and start decreasing after 33 months.
'the 'price puzzle' Sims observed was After a shock to the interest rate (monetary policy variable), prices actually went up. This is against traditional economic intuition which says that interest rates and inflation have a negative relationship.

'the VAR could be misspecified
'Second run, including commodity prices and exchange rate
Smpl 1958:04 1991:02
'exchange rate (lxr) and commodity prices (lcp)
var sims1.ls(nw) 1 14  ff  lxr lcp  lm lp ly   
freeze(fig5sims) sims1.impulse(48,m)  ff  lxr lcp  lm lp ly  @  ff  lxr lcp  lm lp ly 
'format the output
fig5sims.addtext(t, font(40), [+/ b]) Fig 5 C. Sims, Interpreting…
fig5sims.options size(4.7,3.2)  indenth(0.16) 
fig5sims.area(l)

show fig5sims
'as ff increases exchange rate appreciates, commodity prices decline, then money supply declines, gdp falls and prices barely change and then decline after 18 months

