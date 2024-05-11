load VAR_DATA.wf1
pageselect AUS
smpl 1994q1 2014q4
var aus_var.ls 1 4 lgdp_gap infl mpr rer_gap
aus_var.results
aus_var.impulse(0,20) lgdp_gap infl mpr rer_gap @   lgdp_gap infl mpr rer_gap
