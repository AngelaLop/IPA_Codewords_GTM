*Joint Treatments
*No Controls
local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
foreach y in $`empower' {  
	qui reg `y' privacy, vce(cluster village_id)
	qui lincom privacy
    qui replace pval=r(p) if _n == `s'
	
        
    local s = `s'+1
	
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, cluster size and number of control & tment, icc, direction lower
foreach y in `indexlist' {
	qui reg `y' privacy ,vce(cluster village_id)
	qui est sto n_`y'
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 
	gen conmean = r(mean)
	qui sum `y'
	gen sd = r(sd)
	qui sum `y' 
	gen code_key = r(N)
	power twomeans `=conmean', n(`=code_key') power(0.8) sd(`=sd') direction(lower)
	qui estadd scalar mdepri = r(delta)
    drop conmean code_key sd
}	

local i = 1
local j = 7

    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' privacy, vce(cluster village_id) 
        qui est sto n_`y'
		qui sum `y' if tment ==0
		qui estadd scalar cmean = r(mean)
		qui sum `y' if tment==0 
		gen conmean = r(mean)
		qui sum `y'
		gen sd = r(sd)
		qui sum `y' 
		gen code_key = r(N)
		power twomeans `=conmean', n(`=code_key') power(0.8) sd(`=sd') direction(lower)
		*qui estadd scalar mdepri = r(delta)
		
        drop conmean code_key sd
		matrix code = J(1,1,.)
		matrix code[1,1] = qval[`i']
		mat colnames code = privacy
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
 

*create 2x1 matrices to append to each regression s.t there is an fdr q for each treatment 

esttab n_empowerment_0 n_empowerment_1 n_empowerment_2 n_empowerment_3 n_empowerment_4 n_empowerment_5 n_empowerment_6 ///
using "$tables/new_results_nc_joint.tex", ///
cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) keep(privacy) ///
starlevels(* 0.1 ** 0.05 *** 0.01) compress booktabs label collabels(none) ///
nonumbers noobs nogaps nonotes addnotes("Notes: Each column shows the coefficient of regressing each answer related to women empowerment" "on increased privacy (Pooled codenames and keystroke treatment)." "Clustered Standard Errors at the village level in parentheses. Index is the average of all answers." "Stars denote statistical significance at 1,5, and 10\% levels based on unadjusted p-values." "FDR-q values in brackets. FDR-q values indicate the probability of false positives among significant tests.") ///
scalars( "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt( 3 0 3 ) ///
replace

est clear 
drop pval qval

*Controls
local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
foreach y in $`empower' {  
	qui reg `y' privacy $`covariates' , vce(cluster village_id)
	qui lincom privacy
    qui replace pval=r(p) if _n == `s'
	
        
    local s = `s'+1
	
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, cluster size and number of control & tment, icc, direction lower
foreach y in `indexlist' {
	qui reg `y' privacy $`covariates' ,vce(cluster village_id)
	qui est sto n_`y'
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 
	gen conmean = r(mean)
	qui sum `y'
	gen sd = r(sd)
	qui sum `y' 
	gen con_code = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	*qui estadd scalar mdepri = r(delta)
    drop conmean con_code sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' privacy $`covariates' , vce(cluster village_id) 
        qui est sto n_`y'
		qui sum `y' if tment ==0
		qui estadd scalar cmean = r(mean)
		qui sum `y' if tment==0 
		gen conmean = r(mean)
		qui sum `y'
		gen sd = r(sd)
		qui sum `y' 
		gen con_code = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		*qui estadd scalar mdepri = r(delta)
        drop conmean con_code  sd
		matrix code = J(1,1,.)
		matrix code[1,1] = qval[`i']
		mat colnames code = privacy
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
 

*create 2x1 matrices to append to each regression s.t there is an fdr q for each treatment 

esttab n_empowerment_0 n_empowerment_1 n_empowerment_2 n_empowerment_3 n_empowerment_4 n_empowerment_5 n_empowerment_6 ///
using "$tables/new_results_wc_joint.tex", ///
cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) keep(privacy) ///
starlevels(* 0.1 ** 0.05 *** 0.01) compress booktabs label collabels(none) ///
nonumbers noobs nogaps nonotes addnotes("Notes: Each column shows the coefficient of regressing each answer related to women empowerment" "on increased privacy (Pooled codenames and keystroke treatment)." "Clustered Standard Errors at the village level in parentheses. Index is the average of all answers." "All regressions include control variables." "Control variables include age, marital status, log of land size, women education level, partner education level," "household head, land ownership, number of (children, family members, adult men, adult women), and Digitagro participation." "Stars denote statistical significance at 1,5, and 10\% levels based on unadjusted p-values." "FDR-q values in brackets. FDR-q values indicate the probability of false positives among significant tests") ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3 ) ///
replace

est clear 
drop pval qval



*Separate Treatments
*No Controls
local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard, vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, cluster size and number of control & tment, icc, direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard ,vce(cluster village_id)
	qui est sto n_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 
	gen conmean = r(mean)
	qui sum `y'
	gen sd = r(sd)
	qui sum `y' if tment!=2
	gen con_code = r(N)
	qui sum `y' if tment!=1
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard, vce(cluster village_id) 
        qui est sto n_`y'
		qui sum `y' if tment ==0
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 
		gen conmean = r(mean)
		qui sum `y'
		gen sd = r(sd)
		qui sum `y' if tment!=2
		gen con_code = r(N)
		qui sum `y' if tment!=1
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		*qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		*qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
 

*create 2x1 matrices to append to each regression s.t there is an fdr q for each treatment 

esttab n_empowerment_0 n_empowerment_1 n_empowerment_2 n_empowerment_3 n_empowerment_4 n_empowerment_5 n_empowerment_6 ///
using "$tables/new_results_nc.tex", ///
cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) keep(codewords keyboard) ///
starlevels(* 0.1 ** 0.05 *** 0.01) compress booktabs label collabels(none) ///
nonumbers noobs nogaps nonotes addnotes("Notes: Each column shows the coefficient of regressing each answer related to women empowerment" "on increased privacy (codenames or keystroke treatment)." "Clustered Standard Errors at the village level in parentheses. Index is the average of all answers." "Stars denote statistical significance at 1,5, and 10\% levels based on unadjusted p-values." "FDR-q values in brackets. FDR-q values indicate the probability of false positives among significant tests.") ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 0 3 3) ///
replace

est clear 
drop pval qval

*Controls
local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard $`covariates', vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, cluster size and number of control & tment, icc, direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' ,vce(cluster village_id)
	qui est sto n_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 
	gen conmean = r(mean)
	qui sum `y'
	gen sd = r(sd)
	qui sum `y' if tment!=2
	gen con_code = r(N)
	qui sum `y' if tment!=1
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	*qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	*qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard $`covariates', vce(cluster village_id) 
        qui est sto n_`y'
		qui sum `y' if tment ==0
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 
		gen conmean = r(mean)
		qui sum `y'
		gen sd = r(sd)
		qui sum `y' if tment!=2
		gen con_code = r(N)
		qui sum `y' if tment!=1
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		*qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		*qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
 

*create 2x1 matrices to append to each regression s.t there is an fdr q for each treatment 

esttab n_empowerment_0 n_empowerment_1 n_empowerment_2 n_empowerment_3 n_empowerment_4 n_empowerment_5 n_empowerment_6 ///
using "$tables/new_results_wc.tex", ///
cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) keep(codewords keyboard) ///
starlevels(* 0.1 ** 0.05 *** 0.01) compress booktabs label collabels(none) ///
nonumbers noobs nogaps nonotes addnotes("Notes: Each column shows the coefficient of regressing each answer related to women empowerment" "on increased privacy (codenames or keystroke treatment)." "Clustered Standard Errors at the village level in parentheses. Index is the average of all answers." "All regressions include control variables." "Control variables include age, marital status, log of land size, women education level, partner education level," "household head, land ownership, number of (children, family members, adult men, adult women), and Digitagro participation" "Stars denote statistical significance at 1,5, and 10\% levels based on unadjusted p-values" "FDR-q values in brackets. FDR-q values indicate the probability of false positives among significant tests") ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 0 3 3) ///
replace

est clear
drop pval qval


*Fixed Effects
*No Controls
local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	xtset muninum
	qui xtreg `y' codewords keyboard ,fe nonest vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, cluster size and number of control & tment, icc, direction lower
foreach y in `indexlist' {
	xtset muninum
	qui xtreg `y' codewords keyboard ,fe nonest vce(cluster village_id)
	qui est sto n_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 
	gen conmean = r(mean)
	qui sum `y'
	gen sd = r(sd)
	qui sum `y' if tment!=2
	gen con_code = r(N)
	qui sum `y' if tment!=1
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	*qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	*qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
		xtset muninum
        qui xtreg `y' codewords keyboard ,fe nonest vce(cluster village_id)
        qui est sto n_`y'
		qui sum `y' if tment ==0
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 
		gen conmean = r(mean)
		qui sum `y'
		gen sd = r(sd)
		qui sum `y' if tment!=2
		gen con_code = r(N)
		qui sum `y' if tment!=1
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		*qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		*qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
 

*create 2x1 matrices to append to each regression s.t there is an fdr q for each treatment 

esttab n_empowerment_0 n_empowerment_1 n_empowerment_2 n_empowerment_3 n_empowerment_4 n_empowerment_5 n_empowerment_6 ///
using "$tables/new_results_nc_fe.tex", ///
cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) keep(codewords keyboard) ///
starlevels(* 0.1 ** 0.05 *** 0.01) compress booktabs label collabels(none) ///
nonumbers noobs nogaps nonotes addnotes("Notes: Each column shows the coefficient of regressing each answer related to women empowerment" "on increased privacy (codenames or keystroke treatment)." "Clustered Standard Errors at the village level in parentheses." "All regressions include controls for fixed effects in municipality" "Index is the average of all answers." "Stars denote statistical significance at 1,5, and 10\% levels based on unadjusted p-values." "FDR-q values in brackets. FDR-q values indicate the probability of false positives among significant tests." "The MDE is reported for each outcome") ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 0 3 3) ///
replace

est clear 
drop pval qval 
*Controls

local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	xtset muninum
	qui xtreg `y' codewords keyboard $`covariates' ,fe nonest vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, cluster size and number of control & tment, icc, direction lower
foreach y in `indexlist' {
	xtset muninum
	qui xtreg `y' codewords keyboard $`covariates',fe nonest vce(cluster village_id)
	qui est sto n_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 
	gen conmean = r(mean)
	qui sum `y'
	gen sd = r(sd)
	qui sum `y' if tment!=2
	gen con_code = r(N)
	qui sum `y' if tment!=1
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	*qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	*qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
		xtset muninum
        qui xtreg `y' codewords keyboard $`covariates',fe nonest vce(cluster village_id)
        qui est sto n_`y'
		qui sum `y' if tment ==0
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 
		gen conmean = r(mean)
		qui sum `y'
		gen sd = r(sd)
		qui sum `y' if tment!=2
		gen con_code = r(N)
		qui sum `y' if tment!=1
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		*qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		*qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
 

*create 2x1 matrices to append to each regression s.t there is an fdr q for each treatment 

esttab n_empowerment_0 n_empowerment_1 n_empowerment_2 n_empowerment_3 n_empowerment_4 n_empowerment_5 n_empowerment_6 ///
using "$tables/new_results_wc_fe.tex", ///
cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) keep(codewords keyboard) ///
starlevels(* 0.1 ** 0.05 *** 0.01) compress booktabs label collabels(none) ///
nonumbers noobs nogaps nonotes addnotes("Notes: Each column shows the coefficient of regressing each answer related to women empowerment" "on increased privacy (codenames or keystroke treatment)." "Clustered Standard Errors at the village level in parentheses. Index is the average of all answers." "All regressions include fixed effects for municipality." "Control variables include age, marital status, log of land size, women education level, partner education level," "household head, land ownership, number of (children, family members, adult men, adult women), and Digitagro participation." "Stars denote statistical significance at 1,5, and 10\% levels based on unadjusted p-values" "FDR-q values in brackets. FDR-q values indicate the probability of false positives among significant tests.") ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 0 3 3) ///
replace

est clear 
drop pval qval


*Education
*No Controls
*1st panel(No Primary)

local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard if home_12 <3, vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, cluster size and number of control & tment, icc, direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard if home_12 <3 ,vce(cluster village_id)
	qui est sto n_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & home_12 <3
	gen conmean = r(mean)
	qui sum `y' if home_12 <3
	gen sd = r(sd)
	qui sum `y' if tment!=2 & home_12 <3
	gen con_code = r(N)
	qui sum `y' if tment!=1 & home_12 <3
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	*qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	*qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard if home_12 <3, vce(cluster village_id) 
        qui est sto n_`y'
		qui sum `y' if tment ==0 & home_12 <3
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & home_12 <3
		gen conmean = r(mean)
		qui sum `y' if home_12 <3
		gen sd = r(sd)
		qui sum `y' if tment!=2 & home_12 <3
		gen con_code = r(N)
		qui sum `y' if tment!=1 & home_12 <3
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		*qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		*qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

*2nd panel (Primary)

local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard if home_12 >2, vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, cluster size and number of control & tment, icc, direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard if home_12 >2 ,vce(cluster village_id)
	qui est sto c_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & home_12 >2
	gen conmean = r(mean)
	qui sum `y' if home_12 >2
	gen sd = r(sd)
	qui sum `y' if tment!=2 & home_12 >2
	gen con_code = r(N)
	qui sum `y' if tment!=1 & home_12 >2
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	*qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	*qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard if home_12 >2, vce(cluster village_id) 
        qui est sto c_`y'
		qui sum `y' if tment ==0 & home_12 >2
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & home_12 >2
		gen conmean = r(mean)
		qui sum `y' if home_12 <3
		gen sd = r(sd)
		qui sum `y' if tment!=2 & home_12 >2
		gen con_code = r(N)
		qui sum `y' if tment!=1 & home_12 >2
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		*qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		*qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

esttab n_empowerment_0 n_empowerment_1 n_empowerment_2 n_empowerment_3 n_empowerment_4 n_empowerment_5 n_empowerment_6 ///
using "$tables/new_results_nc_ed.tex", ///
 prehead("\begin{tabular}{l*{8}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel A: No Primary}} \\\\[-1ex]") ///
 cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
 fragment collabels(none) ///
 mtitles("Index" "Allowed to go out" "Does not ask permission to buy goods" "Visit Friends" "Asset Purchasing" "Relatives Care" "Child Care" ) ///
 nonum keep (codewords keyboard) label ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 0 3 3) ///
replace


esttab c_empowerment_0 c_empowerment_1 c_empowerment_2 c_empowerment_3 c_empowerment_4 c_empowerment_5 c_empowerment_6 ///
using "$tables/new_results_nc_ed.tex", ///
posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel B: Primary}} \\\\[-1ex]") ///
cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
fragment collabels(none) ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 0 3 3)  ///
prefoot("\hline" ) ///
postfoot("\hline \\  \multicolumn{8}{l}{\tiny Notes: Each column shows the coefficient of regressing each answer related to women empowerment" "on increased privacy (codenames or keystroke treatment).} \\ \multicolumn{8}{l}{\tiny Clustered Standard Errors at the village level in parentheses. Index is the average of all answers.}\\  \multicolumn{8}{l}{\tiny Panel A includes women who have not finished primary education. Panel B includes women who have finished primary education. } \\ \multicolumn{8}{l}{\tiny Stars denote statistical significance at 1,5, and 10\% levels based on unadjusted p-values. FDR-q values in brackets.} \\ \multicolumn{8}{l}{\tiny FDR-q values indicate the probability of false positives among significant tests.} \\ \multicolumn{8}{l}{\tiny The last row reports the p-value to check if there is a statistically significant difference between treatments. } \\  \hline\hline \end{tabular}") label ///


est clear

*Controls

local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard $`covariates' if home_12 <3, vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, cluster size and number of control & tment, icc, direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if home_12 <3 ,vce(cluster village_id)
	qui est sto n_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & home_12 <3
	gen conmean = r(mean)
	qui sum `y' if home_12 <3
	gen sd = r(sd)
	qui sum `y' if tment!=2 & home_12 <3
	gen con_code = r(N)
	qui sum `y' if tment!=1 & home_12 <3
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	*qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	*qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard $`covariates' if home_12 <3, vce(cluster village_id) 
        qui est sto n_`y'
		qui sum `y' if tment ==0 & home_12 <3
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & home_12 <3
		gen conmean = r(mean)
		qui sum `y' if home_12 <3
		gen sd = r(sd)
		qui sum `y' if tment!=2 & home_12 <3
		gen con_code = r(N)
		qui sum `y' if tment!=1 & home_12 <3
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		*qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		*qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

*2nd panel (Primary)

local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard if home_12 >2, vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, sample size, power,  direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if home_12 >2 ,vce(cluster village_id)
	qui est sto c_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & home_12 >2
	gen conmean = r(mean)
	qui sum `y' if home_12 >2
	gen sd = r(sd)
	qui sum `y' if tment!=2 & home_12 >2
	gen con_code = r(N)
	qui sum `y' if tment!=1 & home_12 >2
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	*qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	*qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard $`covariates' if home_12 >2, vce(cluster village_id) 
        qui est sto c_`y'
		qui sum `y' if tment ==0 & home_12 >2
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & home_12 >2
		gen conmean = r(mean)
		qui sum `y' if home_12 <3
		gen sd = r(sd)
		qui sum `y' if tment!=2 & home_12 >2
		gen con_code = r(N)
		qui sum `y' if tment!=1 & home_12 >2
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		*qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		*qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

esttab n_empowerment_0 n_empowerment_1 n_empowerment_2 n_empowerment_3 n_empowerment_4 n_empowerment_5 n_empowerment_6 ///
using "$tables/new_results_wc_ed.tex", ///
 prehead("\begin{tabular}{l*{8}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel A: No Primary}} \\\\[-1ex]") ///
 cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
 fragment collabels(none) ///
 mtitles("Index" "Allowed to go out" "Does not ask permission to buy goods" "Visit Friends" "Asset Purchasing" "Relatives Care" "Child Care" ) ///
 nonum keep (codewords keyboard) label ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 0 3 3) ///
replace


esttab c_empowerment_0 c_empowerment_1 c_empowerment_2 c_empowerment_3 c_empowerment_4 c_empowerment_5 c_empowerment_6 ///
using "$tables/new_results_wc_ed.tex", ///
posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel B: Primary}} \\\\[-1ex]") ///
cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
fragment collabels(none) ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 0 3 3)  ///
prefoot("\hline" ) ///
postfoot("\hline \\ \multicolumn{8}{l}{\tiny Notes: Each column shows the coefficient of regressing each answer related to women empowerment" "on increased privacy (codenames or keystroke treatment).} \\ \multicolumn{8}{l}{\tiny Clustered Standard Errors at the village level in parentheses. Index is the average of all answers.} \\ \multicolumn{8}{l}{\tiny All regressions include control variables. Control variables include age, marital status, log of land size, women education level, partner education level, } \\ \multicolumn{8}{l}{\tiny household head, land ownership, number of (children, family members, adult men, adult women), and Digitagro participation.} \\  \multicolumn{8}{l}{\tiny Panel A includes women who have not finished primary education. Panel B includes women who have finished primary education. } \\ \multicolumn{8}{l}{\tiny Stars denote statistical significance at 1,5, and 10\% levels based on unadjusted p-values. FDR-q values in brackets.} \\ \multicolumn{8}{l}{\tiny FDR-q values indicate the probability of false positives among significant tests.} \\ \multicolumn{8}{l}{\tiny The last row reports the p-value to check if there is a statistically significant difference between treatments. } \\  \hline\hline \end{tabular}") label ///


est clear


*Marital Status

*No Controls
*1st panel(Single)

local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard if single==1, vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, number of control & tment, power, direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard if single==1 ,vce(cluster village_id)
	qui est sto n_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & single==1
	gen conmean = r(mean)
	qui sum `y' if single==1 
	gen sd = r(sd)
	qui sum `y' if tment!=2 & single==1
	gen con_code = r(N)
	qui sum `y' if tment!=1 & single==1
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	*qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	*qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard if single==1, vce(cluster village_id) 
        qui est sto n_`y'
		qui sum `y' if tment ==0 & single==1
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & single==1
		gen conmean = r(mean)
		qui sum `y' if single==1 
		gen sd = r(sd)
		qui sum `y' if tment!=2 & single==1
		gen con_code = r(N)
		qui sum `y' if tment!=1 & single==1
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		*qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		*qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

*2nd panel (Married)

local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard if single==0, vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, cluster size and number of control & tment, icc, direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard if single==0 ,vce(cluster village_id)
	qui est sto c_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & single==0
	gen conmean = r(mean)
	qui sum `y' if single==0
	gen sd = r(sd)
	qui sum `y' if tment!=2 & single==0
	gen con_code = r(N)
	qui sum `y' if tment!=1 & single==0
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	*qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	*qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard if single==0, vce(cluster village_id) 
        qui est sto c_`y'
		qui sum `y' if tment ==0 & single==0
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & single==0
		gen conmean = r(mean)
		qui sum `y' if single==0
		gen sd = r(sd)
		qui sum `y' if tment!=2 & single==0
		gen con_code = r(N)
		qui sum `y' if tment!=1 & single==0
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		*qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		*qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval
/*
esttab n_empowerment_0 n_empowerment_1 n_empowerment_2 n_empowerment_3 n_empowerment_4 n_empowerment_5 n_empowerment_6 ///
using "$tables/new_results_nc_marital.tex", ///
 prehead("\begin{tabular}{l*{8}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel A: Single}} \\\\[-1ex]") ///
 cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
 fragment collabels(none) ///
 mtitles("Index" "Allowed to go out" "Does not ask permission to buy goods" "Visit Friends" "Asset Purchasing" "Relatives Care" "Child Care" ) ///
 nonum keep (codewords keyboard) label ///
scalars("mdecode MDE Codewords" "mdekey MDE Keystroke" "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 3 3 0 3 3) ///
replace


esttab c_empowerment_0 c_empowerment_1 c_empowerment_2 c_empowerment_3 c_empowerment_4 c_empowerment_5 c_empowerment_6 ///
using "$tables/new_results_nc_marital.tex", ///
posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel B: Married}} \\\\[-1ex]") ///
cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
fragment collabels(none) ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("mdecode MDE Codewords" "mdekey MDE Keystroke" "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt( 3 3 3 0 3 3)  ///
prefoot("\hline" ) ///
postfoot("\hline \\ \multicolumn{8}{l}{\tiny Notes: Each column shows the coefficient of regressing each answer related to women empowerment" "on increased privacy (codenames or keystroke treatment).} \\ \multicolumn{8}{l}{\tiny Clustered Standard Errors at the village level in parentheses. Index is the average of all answers.}\\  \multicolumn{8}{l}{\tiny Panel A includes women who are single. Panel B includes women who are married. } \\ \multicolumn{8}{l}{\tiny Stars denote statistical significance at 1,5, and 10\% levels based on unadjusted p-values. FDR-q values in brackets.} \\ \multicolumn{8}{l}{\tiny FDR-q values indicate the probability of false positives among significant tests. The MDE is reported for each outcome.} \\ \multicolumn{8}{l}{\tiny The MDE stands for minimum detectable effect which is the smallest effect that, if true," "has an X\% chance of } \\ \multicolumn{8}{l}{\tiny producing an impact estimate that is statistically significant at the Y level. } \\ \multicolumn{8}{l}{\tiny The last row reports the p-value to check if there is a statistically significant difference between treatments. } \\  \hline\hline \end{tabular}") label ///


est clear

*Controls
*1st panel(Single)
local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard $`covariates' if single==1 , vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, cluster size and number of control & tment, icc, direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if single==1  ,vce(cluster village_id)
	qui est sto n_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & single==1 
	gen conmean = r(mean)
	qui sum `y' if single==1 
	gen sd = r(sd)
	qui sum `y' if tment!=2 & single==1 
	gen con_code = r(N)
	qui sum `y' if tment!=1 & single==1 
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard $`covariates' if single==1 , vce(cluster village_id) 
        qui est sto n_`y'
		qui sum `y' if tment ==0 & single==1 
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & single==1 
		gen conmean = r(mean)
		qui sum `y' if single==1 
		gen sd = r(sd)
		qui sum `y' if tment!=2 & single==1 
		gen con_code = r(N)
		qui sum `y' if tment!=1 & single==1 
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

*2nd panel (Married)

local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard if single==0, vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, sample size, power,  direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if single==0 ,vce(cluster village_id)
	qui est sto c_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & single==0
	gen conmean = r(mean)
	qui sum `y' if single==0
	gen sd = r(sd)
	qui sum `y' if tment!=2 & single==0
	gen con_code = r(N)
	qui sum `y' if tment!=1 & single==0
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard $`covariates' if single==0, vce(cluster village_id) 
        qui est sto c_`y'
		qui sum `y' if tment ==0 & single==0
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & single==0
		gen conmean = r(mean)
		qui sum `y' if single==0
		gen sd = r(sd)
		qui sum `y' if tment!=2 & single==0
		gen con_code = r(N)
		qui sum `y' if tment!=1 & single==0
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

esttab n_empowerment_0 n_empowerment_1 n_empowerment_2 n_empowerment_3 n_empowerment_4 n_empowerment_5 n_empowerment_6 ///
using "$tables/new_results_wc_marital.tex", ///
 prehead("\begin{tabular}{l*{8}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel A: Single}} \\\\[-1ex]") ///
 cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
 fragment collabels(none) ///
 mtitles("Index" "Allowed to go out" "Does not ask permission to buy goods" "Visit Friends" "Asset Purchasing" "Relatives Care" "Child Care" ) ///
 nonum keep (codewords keyboard) label ///
scalars("mdecode MDE Codewords" "mdekey MDE Keystroke" "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 3 3 0 3 3) ///
replace


esttab c_empowerment_0 c_empowerment_1 c_empowerment_2 c_empowerment_3 c_empowerment_4 c_empowerment_5 c_empowerment_6 ///
using "$tables/new_results_wc_marital.tex", ///
posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel B: Married}} \\\\[-1ex]") ///
cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
fragment collabels(none) ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("mdecode MDE Codewords" "mdekey MDE Keystroke" "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 3 3 0 3 3)  ///
prefoot("\hline" ) ///
postfoot("\hline \\ \multicolumn{8}{l}{\tiny Notes: Each column shows the coefficient of regressing each answer related to women empowerment" "on increased privacy (codenames or keystroke treatment).} \\ \multicolumn{8}{l}{\tiny Clustered Standard Errors at the village_id level in parentheses. Index is the average of all answers.} \\ \multicolumn{8}{l}{\tiny All regressions include control variables. Control variables include age, marital status, log of land size, women education level, partner education level, } \\ \multicolumn{8}{l}{\tiny household head, land ownership, number of (children, family members, adult men, adult women), and Digitagro participation.} \\  \multicolumn{8}{l}{\tiny Panel A includes women who are single. Panel B includes women who are married. } \\ \multicolumn{8}{l}{\tiny Stars denote statistical significance at 1,5, and 10\% levels based on unadjusted p-values. FDR-q values in brackets.} \\ \multicolumn{8}{l}{\tiny FDR-q values indicate the probability of false positives among significant tests. The MDE is reported for each outcome.} \\ \multicolumn{8}{l}{\tiny The MDE stands for minimum detectable effect which is the smallest effect that, if true," "has an X\% chance of } \\ \multicolumn{8}{l}{\tiny producing an impact estimate that is statistically significant at the Y level. } \\ \multicolumn{8}{l}{\tiny The last row reports the p-value to check if there is a statistically significant difference between treatments. } \\  \hline\hline \end{tabular}") label ///


est clear
*/
*Vulnerability

*No Controls
*1st panel(Low Vulnerability)

local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard if vulnerability <= -.0505324, vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, number of control & tment, power, direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard if vulnerability <= -.0505324 ,vce(cluster village_id)
	qui est sto n_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & vulnerability <= -.0505324
	gen conmean = r(mean)
	qui sum `y' if vulnerability <= -.0505324 
	gen sd = r(sd)
	qui sum `y' if tment!=2 & vulnerability <= -.0505324
	gen con_code = r(N)
	qui sum `y' if tment!=1 & vulnerability <= -.0505324
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	*qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	*qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard if vulnerability <= -.0505324, vce(cluster village_id) 
        qui est sto n_`y'
		qui sum `y' if tment ==0 & vulnerability <= -.0505324
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & vulnerability <= -.0505324
		gen conmean = r(mean)
		qui sum `y' if vulnerability <= -.0505324
		gen sd = r(sd)
		qui sum `y' if tment!=2 & vulnerability <= -.0505324
		gen con_code = r(N)
		qui sum `y' if tment!=1 & vulnerability <= -.0505324
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		*qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		*qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

*2nd panel (High Vulnerability)

local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard if vulnerability > -.0505324, vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, cluster size and number of control & tment, icc, direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard if vulnerability > -.0505324 ,vce(cluster village_id)
	qui est sto c_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & vulnerability > -.0505324 
	gen conmean = r(mean)
	qui sum `y' if vulnerability > -.0505324 
	gen sd = r(sd)
	qui sum `y' if tment!=2 & vulnerability > -.0505324 
	gen con_code = r(N)
	qui sum `y' if tment!=1 & vulnerability > -.0505324 
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	*qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	*qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard if vulnerability > -.0505324 , vce(cluster village_id) 
        qui est sto c_`y'
		qui sum `y' if tment ==0 & vulnerability > -.0505324 
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & vulnerability > -.0505324 
		gen conmean = r(mean)
		qui sum `y' if vulnerability > -.0505324 
		gen sd = r(sd)
		qui sum `y' if tment!=2 & vulnerability > -.0505324 
		gen con_code = r(N)
		qui sum `y' if tment!=1 & vulnerability > -.0505324 
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		*qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		*qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

esttab n_empowerment_0 n_empowerment_1 n_empowerment_2 n_empowerment_3 n_empowerment_4 n_empowerment_5 n_empowerment_6 ///
using "$tables/new_results_nc_vulnerability.tex", ///
 prehead("\begin{tabular}{l*{8}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel A: Low Vulnerability}} \\\\[-1ex]") ///
 cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
 fragment collabels(none) ///
 mtitles("Index" "Allowed to go out" "Does not ask permission to buy goods" "Visit Friends" "Asset Purchasing" "Relatives Care" "Child Care" ) ///
 nonum keep (codewords keyboard) label ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 0 3 3) ///
replace


esttab c_empowerment_0 c_empowerment_1 c_empowerment_2 c_empowerment_3 c_empowerment_4 c_empowerment_5 c_empowerment_6 ///
using "$tables/new_results_nc_vulnerability.tex", ///
posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel B: High Vulnerability}} \\\\[-1ex]") ///
cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
fragment collabels(none) ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 0 3 3)  ///
prefoot("\hline" ) ///
postfoot("\hline \\ \multicolumn{8}{l}{\tiny Notes: Each column shows the coefficient of regressing each answer related to women empowerment" "on increased privacy (codenames or keystroke treatment).} \\ \multicolumn{8}{l}{\tiny Clustered Standard Errors at the village level in parentheses. Index is the average of all answers.} \\  \multicolumn{8}{l}{\tiny Vulnerability was determined by dependency ratio (Children and Older than 65 to members 15-64), who is the household head, land ownership, and education and separating groups by the median} \\ \multicolumn{8}{l}{\tiny Panel A includes women who are have low vulnerability. Panel B includes women who have high vulnerability. } \\ \multicolumn{8}{l}{\tiny Stars denote statistical significance at 1,5, and 10\% levels based on unadjusted p-values. FDR-q values in brackets.} \\ \multicolumn{8}{l}{\tiny FDR-q values indicate the probability of false positives among significant tests.} \\ \multicolumn{8}{l}{\tiny The last row reports the p-value to check if there is a statistically significant difference between treatments. } \\  \hline\hline \end{tabular}") label ///


est clear





*------------------------------------------------------------------------------





*Controls
*1st panel(Low Vulnerability)
local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard $`covariates' if vulnerability <= -.0505324 , vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, cluster size and number of control & tment, icc, direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if vulnerability <= -.0505324  ,vce(cluster village_id)
	qui est sto n_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & vulnerability <= -.0505324
	gen conmean = r(mean)
	qui sum `y' if vulnerability <= -.0505324 
	gen sd = r(sd)
	qui sum `y' if tment!=2 & vulnerability <= -.0505324 
	gen con_code = r(N)
	qui sum `y' if tment!=1 & vulnerability <= -.0505324
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	*qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	*qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard $`covariates' if vulnerability <= -.0505324, vce(cluster village_id) 
        qui est sto n_`y'
		qui sum `y' if tment ==0 & vulnerability <= -.0505324
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & vulnerability <= -.0505324
		gen conmean = r(mean)
		qui sum `y' if vulnerability <= -.0505324
		gen sd = r(sd)
		qui sum `y' if tment!=2 & vulnerability <= -.0505324 
		gen con_code = r(N)
		qui sum `y' if tment!=1 & vulnerability <= -.0505324
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		*qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		*qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

*2nd panel (High Vulnerability)

local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard if vulnerability > -.0505324, vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, sample size, power,  direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if vulnerability > -.0505324,vce(cluster village_id)
	qui est sto c_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & vulnerability > -.0505324
	gen conmean = r(mean)
	qui sum `y' if vulnerability > -.0505324
	gen sd = r(sd)
	qui sum `y' if tment!=2 & vulnerability > -.0505324
	gen con_code = r(N)
	qui sum `y' if tment!=1 & vulnerability > -.0505324
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	*qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	*qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard $`covariates' if vulnerability > -.0505324, vce(cluster village_id) 
        qui est sto c_`y'
		qui sum `y' if tment ==0 & vulnerability > -.0505324
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & vulnerability > -.0505324
		gen conmean = r(mean)
		qui sum `y' if vulnerability > -.0505324
		gen sd = r(sd)
		qui sum `y' if tment!=2 & vulnerability > -.0505324
		gen con_code = r(N)
		qui sum `y' if tment!=1 & vulnerability > -.0505324
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		*qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		*qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

esttab n_empowerment_0 n_empowerment_1 n_empowerment_2 n_empowerment_3 n_empowerment_4 n_empowerment_5 n_empowerment_6 ///
using "$tables/new_results_wc_vulnerability.tex", ///
 prehead("\begin{tabular}{l*{8}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel A: Low Vulnerability}} \\\\[-1ex]") ///
 cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
 fragment collabels(none) ///
 mtitles("Index" "Allowed to go out" "Does not ask permission to buy goods" "Visit Friends" "Asset Purchasing" "Relatives Care" "Child Care" ) ///
 nonum keep (codewords keyboard) label ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 0 3 3) ///
replace


esttab c_empowerment_0 c_empowerment_1 c_empowerment_2 c_empowerment_3 c_empowerment_4 c_empowerment_5 c_empowerment_6 ///
using "$tables/new_results_wc_vulnerability.tex", ///
posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel B: High Vulnerability}} \\\\[-1ex]") ///
cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
fragment collabels(none) ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 0 3 3)  ///
prefoot("\hline" ) ///
postfoot("\hline \\ \multicolumn{8}{l}{\tiny Notes: Each column shows the coefficient of regressing each answer related to women empowerment" "on increased privacy (codenames or keystroke treatment).} \\ \multicolumn{8}{l}{\tiny Clustered Standard Errors at the village level in parentheses. Index is the average of all answers.} \\ \multicolumn{8}{l}{\tiny Vulnerability was determined by dependency ratio (Children and Older than 65 to members 15-64), who is the household head, land ownership, and education and separating groups by the median} \\ \multicolumn{8}{l}{\tiny Panel A includes women who are have low vulnerability. Panel B includes women who have high vulnerability. } \\ \multicolumn{8}{l}{\tiny All regressions include control variables. Control variables include age, marital status, log of land size, women education level, partner education level, } \\ \multicolumn{8}{l}{\tiny household head, land ownership, number of (children, family members, adult men, adult women), and Digitagro participation.} \\  \multicolumn{8}{l}{\tiny Stars denote statistical significance at 1,5, and 10\% levels based on unadjusted p-values. FDR-q values in brackets.} \\ \multicolumn{8}{l}{\tiny FDR-q values indicate the probability of false positives among significant tests. The MDE is reported for each outcome.} \\ \multicolumn{8}{l}{\tiny The last row reports the p-value to check if there is a statistically significant difference between treatments. } \\  \hline\hline \end{tabular}") label ///


est clear

*Land Size

*No Controls
*1st panel(Smaller Land)

local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard if farm_2 <= 4, vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, number of control & tment, power, direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard if farm_2 <= 4,vce(cluster village_id)
	qui est sto n_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & farm_2 <= 4
	gen conmean = r(mean)
	qui sum `y' if farm_2 <= 4
	gen sd = r(sd)
	qui sum `y' if tment!=2 & farm_2 <= 4
	gen con_code = r(N)
	qui sum `y' if tment!=1 & farm_2 <= 4
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	*qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	*qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard if farm_2 <= 4, vce(cluster village_id) 
        qui est sto n_`y'
		qui sum `y' if tment ==0 & farm_2 <= 4
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & farm_2 <= 4
		gen conmean = r(mean)
		qui sum `y' if farm_2 <= 4
		gen sd = r(sd)
		qui sum `y' if tment!=2 & farm_2 <= 4
		gen con_code = r(N)
		qui sum `y' if tment!=1 & farm_2 <= 4
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		*qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		*qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

*2nd panel (Bigger Land)

local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard if farm_2 > 4, vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, cluster size and number of control & tment, icc, direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard if farm_2 > 4 ,vce(cluster village_id)
	qui est sto c_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & farm_2 > 4
	gen conmean = r(mean)
	qui sum `y' if farm_2 > 4
	gen sd = r(sd)
	qui sum `y' if tment!=2 & farm_2 > 4 
	gen con_code = r(N)
	qui sum `y' if tment!=1 & farm_2 > 4
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	*qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	*qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard if farm_2 > 4 , vce(cluster village_id) 
        qui est sto c_`y'
		qui sum `y' if tment ==0 & farm_2 > 4
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & farm_2 > 4
		gen conmean = r(mean)
		qui sum `y' if farm_2 > 4 
		gen sd = r(sd)
		qui sum `y' if tment!=2 & farm_2 > 4 
		gen con_code = r(N)
		qui sum `y' if tment!=1 & farm_2 > 4
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		*qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		*qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval
/*
esttab n_empowerment_0 n_empowerment_1 n_empowerment_2 n_empowerment_3 n_empowerment_4 n_empowerment_5 n_empowerment_6 ///
using "$tables/new_results_nc_land.tex", ///
 prehead("\begin{tabular}{l*{8}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel A: Smaller Land}} \\\\[-1ex]") ///
 cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
 fragment collabels(none) ///
 mtitles("Index" "Allowed to go out" "Does not ask permission to buy goods" "Visit Friends" "Asset Purchasing" "Relatives Care" "Child Care" ) ///
 nonum keep (codewords keyboard) label ///
scalars("mdecode MDE Codewords" "mdekey MDE Keystroke" "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 3 3 0 3 3) ///
replace


esttab c_empowerment_0 c_empowerment_1 c_empowerment_2 c_empowerment_3 c_empowerment_4 c_empowerment_5 c_empowerment_6 ///
using "$tables/new_results_nc_land.tex", ///
posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel B: Bigger Land}} \\\\[-1ex]") ///
cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
fragment collabels(none) ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("mdecode MDE Codewords" "mdekey MDE Keystroke" "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt( 3 3 3 0 3 3)  ///
prefoot("\hline" ) ///
postfoot("\hline \\ \multicolumn{8}{l}{\tiny Notes: Each column shows the coefficient of regressing each answer related to women empowerment" "on increased privacy (codenames or keystroke treatment).} \\ \multicolumn{8}{l}{\tiny Clustered Standard Errors at the village level in parentheses. Index is the average of all answers.} \\  \multicolumn{8}{l}{\tiny Land Size was determined by separating groups by the median} \\ \multicolumn{8}{l}{\tiny Panel A includes women who are have a smaller piece of land. Panel B includes women who have a bigger piece of land. } \\ \multicolumn{8}{l}{\tiny Stars denote statistical significance at 1,5, and 10\% levels based on unadjusted p-values. FDR-q values in brackets.} \\ \multicolumn{8}{l}{\tiny FDR-q values indicate the probability of false positives among significant tests. The MDE is reported for each outcome.} \\ \multicolumn{8}{l}{\tiny The MDE stands for minimum detectable effect which is the smallest effect that, if true," "has an X\% chance of } \\ \multicolumn{8}{l}{\tiny producing an impact estimate that is statistically significant at the Y level. } \\ \multicolumn{8}{l}{\tiny The last row reports the p-value to check if there is a statistically significant difference between treatments. } \\  \hline\hline \end{tabular}") label ///


est clear

*Controls
*1st panel(Bigger Land)
local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard $`covariates' if farm_2 <= 4 , vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, cluster size and number of control & tment, icc, direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if farm_2 <= 4  ,vce(cluster village_id)
	qui est sto n_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & farm_2 <= 4
	gen conmean = r(mean)
	qui sum `y' if farm_2 <= 4
	gen sd = r(sd)
	qui sum `y' if tment!=2 & farm_2 <= 4
	gen con_code = r(N)
	qui sum `y' if tment!=1 & farm_2 <= 4
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard $`covariates' if farm_2 <= 4, vce(cluster village_id) 
        qui est sto n_`y'
		qui sum `y' if tment ==0 & farm_2 <= 4
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & farm_2 <= 4
		gen conmean = r(mean)
		qui sum `y' if farm_2 <= 4
		gen sd = r(sd)
		qui sum `y' if tment!=2 & farm_2 <= 4
		gen con_code = r(N)
		qui sum `y' if tment!=1 & farm_2 <= 4
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

*2nd panel (Bigger Land)

local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard if farm_2 > 4, vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, sample size, power,  direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if farm_2 > 4, vce(cluster village_id)
	qui est sto c_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & farm_2 > 4
	gen conmean = r(mean)
	qui sum `y' if farm_2 > 4
	gen sd = r(sd)
	qui sum `y' if tment!=2 & farm_2 > 4
	gen con_code = r(N)
	qui sum `y' if tment!=1 & farm_2 > 4
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard $`covariates' if farm_2 > 4, vce(cluster village_id) 
        qui est sto c_`y'
		qui sum `y' if tment ==0 & farm_2 > 4
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & farm_2 > 4
		gen conmean = r(mean)
		qui sum `y' if farm_2 > 4
		gen sd = r(sd)
		qui sum `y' if tment!=2 & farm_2 > 4
		gen con_code = r(N)
		qui sum `y' if tment!=1 & farm_2 > 4
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

esttab n_empowerment_0 n_empowerment_1 n_empowerment_2 n_empowerment_3 n_empowerment_4 n_empowerment_5 n_empowerment_6 ///
using "$tables/new_results_wc_land.tex", ///
 prehead("\begin{tabular}{l*{8}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel A: Smaller Land}} \\\\[-1ex]") ///
 cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
 fragment collabels(none) ///
 mtitles("Index" "Allowed to go out" "Does not ask permission to buy goods" "Visit Friends" "Asset Purchasing" "Relatives Care" "Child Care" ) ///
 nonum keep (codewords keyboard) label ///
scalars("mdecode MDE Codewords" "mdekey MDE Keystroke" "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 3 3 0 3 3) ///
replace


esttab c_empowerment_0 c_empowerment_1 c_empowerment_2 c_empowerment_3 c_empowerment_4 c_empowerment_5 c_empowerment_6 ///
using "$tables/new_results_wc_land.tex", ///
posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel B: Bigger Land}} \\\\[-1ex]") ///
cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
fragment collabels(none) ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("mdecode MDE Codewords" "mdekey MDE Keystroke" "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 3 3 0 3 3)  ///
prefoot("\hline" ) ///
postfoot("\hline \\ \multicolumn{8}{l}{\tiny Notes: Each column shows the coefficient of regressing each answer related to women empowerment" "on increased privacy (codenames or keystroke treatment).} \\ \multicolumn{8}{l}{\tiny Clustered Standard Errors at the village_id level in parentheses. Index is the average of all answers.} \\  \multicolumn{8}{l}{\tiny Land Size was determined by separating groups by the median} \\ \multicolumn{8}{l}{\tiny Panel A includes women who are have a smaller piece of land. Panel B includes women who have a bigger piece of land. } \\ \multicolumn{8}{l}{\tiny All regressions include control variables. Control variables include age, marital status, log of land size, women education level, partner education level, } \\ \multicolumn{8}{l}{\tiny household head, land ownership, number of (children, family members, adult men, adult women), and Digitagro participation.} \\  \multicolumn{8}{l}{\tiny Stars denote statistical significance at 1,5, and 10\% levels based on unadjusted p-values. FDR-q values in brackets.} \\ \multicolumn{8}{l}{\tiny FDR-q values indicate the probability of false positives among significant tests. The MDE is reported for each outcome.} \\ \multicolumn{8}{l}{\tiny The MDE stands for minimum detectable effect which is the smallest effect that, if true," "has an X\% chance of } \\ \multicolumn{8}{l}{\tiny producing an impact estimate that is statistically significant at the Y level. } \\ \multicolumn{8}{l}{\tiny The last row reports the p-value to check if there is a statistically significant difference between treatments. } \\  \hline\hline \end{tabular}")  label ///


est clear
*/
*Noise

*No Controls
*1st panel(No Noise)

local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard if enum_evaluation2==0, vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, number of control & tment, power, direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard if enum_evaluation2==0, vce(cluster village_id)
	qui est sto n_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & enum_evaluation2==0
	gen conmean = r(mean)
	qui sum `y' if enum_evaluation2==0
	gen sd = r(sd)
	qui sum `y' if tment!=2 & enum_evaluation2==0
	gen con_code = r(N)
	qui sum `y' if tment!=1 & enum_evaluation2==0
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	*qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	*qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard if enum_evaluation2==0, vce(cluster village_id) 
        qui est sto n_`y'
		qui sum `y' if tment ==0 & enum_evaluation2==0
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & enum_evaluation2==0
		gen conmean = r(mean)
		qui sum `y' if enum_evaluation2==0
		gen sd = r(sd)
		qui sum `y' if tment!=2 & enum_evaluation2==0
		gen con_code = r(N)
		qui sum `y' if tment!=1 & enum_evaluation2==0
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		*qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		*qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

*2nd panel (Noise)

local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard if enum_evaluation2==1, vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, cluster size and number of control & tment, icc, direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard if enum_evaluation2==1 ,vce(cluster village_id)
	qui est sto c_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & enum_evaluation2==1
	gen conmean = r(mean)
	qui sum `y' if enum_evaluation2==1
	gen sd = r(sd)
	qui sum `y' if tment!=2 & enum_evaluation2==1
	gen con_code = r(N)
	qui sum `y' if tment!=1 & enum_evaluation2==1
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	*qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	*qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard if enum_evaluation2==1 , vce(cluster village_id) 
        qui est sto c_`y'
		qui sum `y' if tment ==0 & enum_evaluation2==1
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & enum_evaluation2==1
		gen conmean = r(mean)
		qui sum `y' if enum_evaluation2==1
		gen sd = r(sd)
		qui sum `y' if tment!=2 & enum_evaluation2==1
		gen con_code = r(N)
		qui sum `y' if tment!=1 & enum_evaluation2==1
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		*qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		*qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

esttab n_empowerment_0 n_empowerment_1 n_empowerment_2 n_empowerment_3 n_empowerment_4 n_empowerment_5 n_empowerment_6 ///
using "$tables/new_results_nc_noise.tex", ///
 prehead("\begin{tabular}{l*{8}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel A: No Noise}} \\\\[-1ex]") ///
 cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
 fragment collabels(none) ///
 mtitles("Index" "Allowed to go out" "Does not ask permission to buy goods" "Visit Friends" "Asset Purchasing" "Relatives Care" "Child Care" ) ///
 nonum keep (codewords keyboard) label ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 0 3 3) ///
replace


esttab c_empowerment_0 c_empowerment_1 c_empowerment_2 c_empowerment_3 c_empowerment_4 c_empowerment_5 c_empowerment_6 ///
using "$tables/new_results_nc_noise.tex", ///
posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel B: Noise}} \\\\[-1ex]") ///
cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
fragment collabels(none) ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 0 3 3)  ///
prefoot("\hline" ) ///
postfoot("\hline \\ \multicolumn{8}{l}{\tiny Notes: Each column shows the coefficient of regressing each answer related to women empowerment" "on increased privacy (codenames or keystroke treatment).} \\ \multicolumn{8}{l}{\tiny Clustered Standard Errors at the village level in parentheses. Index is the average of all answers.} \\ \multicolumn{8}{l}{\tiny Noise in the background was determined by the interviewer whenever background noises were noticed.} \\ \multicolumn{8}{l}{\tiny Panel A includes interviews where no background noise was detected. Panel B includes interviews where background noise was detected. } \\  \multicolumn{8}{l}{\tiny Stars denote statistical significance at 1,5, and 10\% levels based on unadjusted p-values. FDR-q values in brackets.} \\ \multicolumn{8}{l}{\tiny FDR-q values indicate the probability of false positives among significant tests.} \\ \multicolumn{8}{l}{\tiny The last row reports the p-value to check if there is a statistically significant difference between treatments. } \\  \hline\hline \end{tabular}")  label ///


est clear

*Controls
*1st panel(No Noise)
local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard $`covariates' if enum_evaluation2==0 , vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, cluster size and number of control & tment, icc, direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if enum_evaluation2==0  ,vce(cluster village_id)
	qui est sto n_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & enum_evaluation2==0
	gen conmean = r(mean)
	qui sum `y' if enum_evaluation2==0
	gen sd = r(sd)
	qui sum `y' if tment!=2 & enum_evaluation2==0
	gen con_code = r(N)
	qui sum `y' if tment!=1 & enum_evaluation2==0
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard $`covariates' if enum_evaluation2==0, vce(cluster village_id) 
        qui est sto n_`y'
		qui sum `y' if tment ==0 & enum_evaluation2==0
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & enum_evaluation2==0
		gen conmean = r(mean)
		qui sum `y' if enum_evaluation2==0
		gen sd = r(sd)
		qui sum `y' if tment!=2 & enum_evaluation2==0
		gen con_code = r(N)
		qui sum `y' if tment!=1 & enum_evaluation2==0
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		*qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		*qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

*2nd panel (Noise)

local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard if enum_evaluation2==1, vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, sample size, power,  direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if enum_evaluation2==1, vce(cluster village_id)
	qui est sto c_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & enum_evaluation2==1
	gen conmean = r(mean)
	qui sum `y' if enum_evaluation2==1
	gen sd = r(sd)
	qui sum `y' if tment!=2 & enum_evaluation2==1
	gen con_code = r(N)
	qui sum `y' if tment!=1 & enum_evaluation2==1
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	*qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	*qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard $`covariates' if enum_evaluation2==1, vce(cluster village_id) 
        qui est sto c_`y'
		qui sum `y' if tment ==0 & enum_evaluation2==1
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & enum_evaluation2==1
		gen conmean = r(mean)
		qui sum `y' if enum_evaluation2==1
		gen sd = r(sd)
		qui sum `y' if tment!=2 & enum_evaluation2==1
		gen con_code = r(N)
		qui sum `y' if tment!=1 & enum_evaluation2==1
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		*qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		*qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

esttab n_empowerment_0 n_empowerment_1 n_empowerment_2 n_empowerment_3 n_empowerment_4 n_empowerment_5 n_empowerment_6 ///
using "$tables/new_results_wc_noise.tex", ///
 prehead("\begin{tabular}{l*{8}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel A: Noise}} \\\\[-1ex]") ///
 cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
 fragment collabels(none) ///
 mtitles("Index" "Allowed to go out" "Does not ask permission to buy goods" "Visit Friends" "Asset Purchasing" "Relatives Care" "Child Care" ) ///
 nonum keep (codewords keyboard) label ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 0 3 3) ///
replace


esttab c_empowerment_0 c_empowerment_1 c_empowerment_2 c_empowerment_3 c_empowerment_4 c_empowerment_5 c_empowerment_6 ///
using "$tables/new_results_wc_noise.tex", ///
posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel B: Noise}} \\\\[-1ex]") ///
cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
fragment collabels(none) ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 0 3 3)  ///
prefoot("\hline" ) ///
postfoot("\hline \\ \multicolumn{8}{l}{\tiny Notes: Each column shows the coefficient of regressing each answer related to women empowerment" "on increased privacy (codenames or keystroke treatment).} \\ \multicolumn{8}{l}{\tiny Clustered Standard Errors at the village level in parentheses. Index is the average of all answers.} \\\multicolumn{8}{l}{\tiny Noise in the background was determined by the interviewer whenever background noises were noticed.} \\ \multicolumn{8}{l}{\tiny Panel A includes interviews where no background noise was detected. Panel B includes interviews where background noise was detected. } \\ \multicolumn{8}{l}{\tiny All regressions include control variables. Control variables include age, marital status, log of land size, women education level, partner education level, } \\ \multicolumn{8}{l}{\tiny household head, land ownership, number of (children, family members, adult men, adult women), and Digitagro participation.} \\  \multicolumn{8}{l}{\tiny Stars denote statistical significance at 1,5, and 10\% levels based on unadjusted p-values. FDR-q values in brackets.} \\ \multicolumn{8}{l}{\tiny FDR-q values indicate the probability of false positives among significant tests.} \\ \multicolumn{8}{l}{\tiny The last row reports the p-value to check if there is a statistically significant difference between treatments. } \\  \hline\hline \end{tabular}") label ///


est clear

*Assessment
/*

*No Controls
*1st panel(Comfortable)

local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard if enum_evaluation1<2, vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, number of control & tment, power, direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard if enum_evaluation1<2,vce(cluster village_id)
	qui est sto n_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & enum_evaluation1<2
	gen conmean = r(mean)
	qui sum `y' if enum_evaluation1<2
	gen sd = r(sd)
	qui sum `y' if tment!=2 & enum_evaluation1<2
	gen con_code = r(N)
	qui sum `y' if tment!=1 & enum_evaluation1<2
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard if enum_evaluation1<2, vce(cluster village_id) 
        qui est sto n_`y'
		qui sum `y' if tment ==0 & enum_evaluation1<2
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & enum_evaluation1<2
		gen conmean = r(mean)
		qui sum `y' if enum_evaluation1<2
		gen sd = r(sd)
		qui sum `y' if tment!=2 & enum_evaluation1<2
		gen con_code = r(N)
		qui sum `y' if tment!=1 & enum_evaluation1<2
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

*2nd panel (Uncomfortable)

local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard if enum_evaluation1>1 , vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, cluster size and number of control & tment, icc, direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard if enum_evaluation1>1  ,vce(cluster village_id)
	qui est sto c_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & enum_evaluation1>1 
	gen conmean = r(mean)
	qui sum `y' if enum_evaluation1>1 
	gen sd = r(sd)
	qui sum `y' if tment!=2 & enum_evaluation1>1 
	gen con_code = r(N)
	qui sum `y' if tment!=1 & enum_evaluation1>1 
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard if enum_evaluation1>1 , vce(cluster village_id) 
        qui est sto c_`y'
		qui sum `y' if tment ==0 & enum_evaluation1>1 
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & enum_evaluation1>1 
		gen conmean = r(mean)
		qui sum `y' if enum_evaluation1>1 
		gen sd = r(sd)
		qui sum `y' if tment!=2 & enum_evaluation1>1 
		gen con_code = r(N)
		qui sum `y' if tment!=1 & enum_evaluation1>1 
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

esttab n_empowerment_0 n_empowerment_1 n_empowerment_2 n_empowerment_3 n_empowerment_4 n_empowerment_5 n_empowerment_6 ///
using "$tables/new_results_nc_assess.tex", ///
 prehead("\begin{tabular}{l*{8}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel A: Comfortable}} \\\\[-1ex]") ///
 cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
 fragment collabels(none) ///
 mtitles("Index" "Allowed to go out" "Does not ask permission to buy goods" "Visit Friends" "Asset Purchasing" "Relatives Care" "Child Care" ) ///
 nonum keep (codewords keyboard) label ///
scalars("mdecode MDE Codewords" "mdekey MDE Keystroke" "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 3 3 0 3 3) ///
replace


esttab c_empowerment_0 c_empowerment_1 c_empowerment_2 c_empowerment_3 c_empowerment_4 c_empowerment_5 c_empowerment_6 ///
using "$tables/new_results_nc_assess.tex", ///
posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel B: Uncomfortable}} \\\\[-1ex]") ///
cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
fragment collabels(none) ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("mdecode MDE Codewords" "mdekey MDE Keystroke" "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt( 3 3 3 0 3 3)  ///
prefoot("\hline" ) ///
postfoot("\hline \\ \multicolumn{8}{l}{\tiny Notes: Each column shows the coefficient of regressing each answer related to women empowerment" "on increased privacy (codenames or keystroke treatment).} \\ \multicolumn{8}{l}{\tiny Clustered Standard Errors at the village level in parentheses. Index is the average of all answers.} \\ \multicolumn{8}{l}{\tiny Comfortability was determined by the interviewer assessment.} \\ \multicolumn{8}{l}{\tiny Panel A includes interviews where the interviewee was comfortable. Panel B includes interviews where the interviewee was uncomfortable. } \\  \multicolumn{8}{l}{\tiny Stars denote statistical significance at 1,5, and 10\% levels based on unadjusted p-values. FDR-q values in brackets.} \\ \multicolumn{8}{l}{\tiny FDR-q values indicate the probability of false positives among significant tests. The MDE is reported for each outcome.} \\ \multicolumn{8}{l}{\tiny The MDE stands for minimum detectable effect which is the smallest effect that, if true," "has an X\% chance of } \\ \multicolumn{8}{l}{\tiny producing an impact estimate that is statistically significant at the Y level. } \\ \multicolumn{8}{l}{\tiny The last row reports the p-value to check if there is a statistically significant difference between treatments. } \\  \hline\hline \end{tabular}") label ///


est clear

*Controls
*1st panel(Comfortable)
local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard $`covariates' if enum_evaluation1<2 , vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, cluster size and number of control & tment, icc, direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if enum_evaluation1<2  ,vce(cluster village_id)
	qui est sto n_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & enum_evaluation1<2
	gen conmean = r(mean)
	qui sum `y' if enum_evaluation1<2
	gen sd = r(sd)
	qui sum `y' if tment!=2 & enum_evaluation1<2
	gen con_code = r(N)
	qui sum `y' if tment!=1 & enum_evaluation1<2
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard $`covariates' if enum_evaluation1<2, vce(cluster village_id) 
        qui est sto n_`y'
		qui sum `y' if tment ==0 & enum_evaluation1<2
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & enum_evaluation1<2
		gen conmean = r(mean)
		qui sum `y' if enum_evaluation1<2
		gen sd = r(sd)
		qui sum `y' if tment!=2 & enum_evaluation1<2
		gen con_code = r(N)
		qui sum `y' if tment!=1 & enum_evaluation1<2
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

*2nd panel (Uncomfortable)

local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard if enum_evaluation1>1, vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, sample size, power,  direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if enum_evaluation1>1, vce(cluster village_id)
	qui est sto c_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & enum_evaluation1>1
	gen conmean = r(mean)
	qui sum `y' if enum_evaluation1>1
	gen sd = r(sd)
	qui sum `y' if tment!=2 & enum_evaluation1>1
	gen con_code = r(N)
	qui sum `y' if tment!=1 & enum_evaluation1>1
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard $`covariates' if enum_evaluation1>1, vce(cluster village_id) 
        qui est sto c_`y'
		qui sum `y' if tment ==0 & enum_evaluation1>1
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & enum_evaluation1>1
		gen conmean = r(mean)
		qui sum `y' if enum_evaluation1>1
		gen sd = r(sd)
		qui sum `y' if tment!=2 & enum_evaluation1>1
		gen con_code = r(N)
		qui sum `y' if tment!=1 & enum_evaluation1>1
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

esttab n_empowerment_0 n_empowerment_1 n_empowerment_2 n_empowerment_3 n_empowerment_4 n_empowerment_5 n_empowerment_6 ///
using "$tables/new_results_wc_assess.tex", ///
 prehead("\begin{tabular}{l*{8}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel A: Comfortable}} \\\\[-1ex]") ///
 cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
 fragment collabels(none) ///
 mtitles("Index" "Allowed to go out" "Does not ask permission to buy goods" "Visit Friends" "Asset Purchasing" "Relatives Care" "Child Care" ) ///
 nonum keep (codewords keyboard) label ///
scalars("mdecode MDE Codewords" "mdekey MDE Keystroke" "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 3 3 0 3 3) ///
replace


esttab c_empowerment_0 c_empowerment_1 c_empowerment_2 c_empowerment_3 c_empowerment_4 c_empowerment_5 c_empowerment_6 ///
using "$tables/new_results_wc_assess.tex", ///
posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel B: Uncomfortable}} \\\\[-1ex]") ///
cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
fragment collabels(none) ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("mdecode MDE Codewords" "mdekey MDE Keystroke" "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 3 3 0 3 3)  ///
prefoot("\hline" ) ///
postfoot("\hline \\ \multicolumn{8}{l}{\tiny Notes: Each column shows the coefficient of regressing each answer related to women empowerment" "on increased privacy (codenames or keystroke treatment).} \\ \multicolumn{8}{l}{\tiny Clustered Standard Errors at the village level in parentheses. Index is the average of all answers.} \\\multicolumn{8}{l}{\tiny Comfortability was determined by the interviewer assessment.} \\ \multicolumn{8}{l}{\tiny Panel A includes interviews where the interviewee was comfortable. Panel B includes interviews where the interviewee was uncomfortable. } \\ \multicolumn{8}{l}{\tiny All regressions include control variables. Control variables include age, marital status, log of land size, women education level, partner education level, } \\ \multicolumn{8}{l}{\tiny household head, land ownership, number of (children, family members, adult men, adult women), and Digitagro participation.} \\  \multicolumn{8}{l}{\tiny Stars denote statistical significance at 1,5, and 10\% levels based on unadjusted p-values. FDR-q values in brackets.} \\ \multicolumn{8}{l}{\tiny FDR-q values indicate the probability of false positives among significant tests. The MDE is reported for each outcome.} \\ \multicolumn{8}{l}{\tiny The MDE stands for minimum detectable effect which is the smallest effect that, if true," "has an X\% chance of } \\ \multicolumn{8}{l}{\tiny producing an impact estimate that is statistically significant at the Y level. } \\ \multicolumn{8}{l}{\tiny The last row reports the p-value to check if there is a statistically significant difference between treatments. } \\  \hline\hline \end{tabular}")label ///


est clear

*Household Head

*No Controls
*1st panel(Interviewee is HH head)

local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard if head_self==1, vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, number of control & tment, power, direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard if head_self==1,vce(cluster village_id)
	qui est sto n_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & head_self==1
	gen conmean = r(mean)
	qui sum `y' if head_self==1
	gen sd = r(sd)
	qui sum `y' if tment!=2 & head_self==1
	gen con_code = r(N)
	qui sum `y' if tment!=1 & head_self==1
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard if head_self==1, vce(cluster village_id) 
        qui est sto n_`y'
		qui sum `y' if tment ==0 & head_self==1
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & head_self==1
		gen conmean = r(mean)
		qui sum `y' if head_self==1
		gen sd = r(sd)
		qui sum `y' if tment!=2 & head_self==1
		gen con_code = r(N)
		qui sum `y' if tment!=1 & head_self==1
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

*2nd panel (Other is HH head)

local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard if head_self==0 , vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, cluster size and number of control & tment, icc, direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard if head_self==0  ,vce(cluster village_id)
	qui est sto c_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & head_self==0
	gen conmean = r(mean)
	qui sum `y' if head_self==0
	gen sd = r(sd)
	qui sum `y' if tment!=2 & head_self==0
	gen con_code = r(N)
	qui sum `y' if tment!=1 & head_self==0 
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard if head_self==0 , vce(cluster village_id) 
        qui est sto c_`y'
		qui sum `y' if tment ==0 & head_self==0
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & head_self==0 
		gen conmean = r(mean)
		qui sum `y' if head_self==0
		gen sd = r(sd)
		qui sum `y' if tment!=2 & head_self==0
		gen con_code = r(N)
		qui sum `y' if tment!=1 & head_self==0 
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

esttab n_empowerment_0 n_empowerment_1 n_empowerment_2 n_empowerment_3 n_empowerment_4 n_empowerment_5 n_empowerment_6 ///
using "$tables/new_results_nc_hhead.tex", ///
 prehead("\begin{tabular}{l*{8}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel A: Interviewee is HH Head}} \\\\[-1ex]") ///
 cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
 fragment collabels(none) ///
 mtitles("Index" "Allowed to go out" "Does not ask permission to buy goods" "Visit Friends" "Asset Purchasing" "Relatives Care" "Child Care" ) ///
 nonum keep (codewords keyboard) label ///
scalars("mdecode MDE Codewords" "mdekey MDE Keystroke" "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 3 3 0 3 3) ///
replace


esttab c_empowerment_0 c_empowerment_1 c_empowerment_2 c_empowerment_3 c_empowerment_4 c_empowerment_5 c_empowerment_6 ///
using "$tables/new_results_nc_hhead.tex", ///
posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel B: Other is HH Head}} \\\\[-1ex]") ///
cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
fragment collabels(none) ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("mdecode MDE Codewords" "mdekey MDE Keystroke" "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt( 3 3 3 0 3 3)  ///
prefoot("\hline" ) ///
postfoot("\hline \\ \multicolumn{8}{l}{\tiny Notes: Each column shows the coefficient of regressing each answer related to women empowerment" "on increased privacy (codenames or keystroke treatment).} \\ \multicolumn{8}{l}{\tiny Clustered Standard Errors at the village level in parentheses. Index is the average of all answers.} \\\multicolumn{8}{l}{\tiny Who is the household head was determined during the interview.} \\ \multicolumn{8}{l}{\tiny Panel A includes women who are the head of the household. Panel B includes women who are not the head of the household. } \\  \multicolumn{8}{l}{\tiny Stars denote statistical significance at 1,5, and 10\% levels based on unadjusted p-values. FDR-q values in brackets.} \\ \multicolumn{8}{l}{\tiny FDR-q values indicate the probability of false positives among significant tests. The MDE is reported for each outcome.} \\ \multicolumn{8}{l}{\tiny The MDE stands for minimum detectable effect which is the smallest effect that, if true," "has an X\% chance of } \\ \multicolumn{8}{l}{\tiny producing an impact estimate that is statistically significant at the Y level. } \\ \multicolumn{8}{l}{\tiny The last row reports the p-value to check if there is a statistically significant difference between treatments. } \\  \hline\hline \end{tabular}") label ///


est clear

*Controls
*1st panel(Interviewee is HH Head)
local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard $`covariates' if head_self==1 , vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, cluster size and number of control & tment, icc, direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if head_self==1  ,vce(cluster village_id)
	qui est sto n_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & head_self==1
	gen conmean = r(mean)
	qui sum `y' if head_self==1
	gen sd = r(sd)
	qui sum `y' if tment!=2 & head_self==1
	gen con_code = r(N)
	qui sum `y' if tment!=1 & head_self==1
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard $`covariates' if head_self==1, vce(cluster village_id) 
        qui est sto n_`y'
		qui sum `y' if tment ==0 & head_self==1
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & head_self==1
		gen conmean = r(mean)
		qui sum `y' if head_self==1
		gen sd = r(sd)
		qui sum `y' if tment!=2 & head_self==1
		gen con_code = r(N)
		qui sum `y' if tment!=1 & head_self==1
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

*2nd panel (Other is HH head)

local indexlist empowerment_0
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*1st step: run regressions, obtain pvalues, then fdr-q values
gen long pval=.
local s = 1
local f = 7
foreach y in $`empower' {  
	qui reg `y' codewords keyboard if head_self==0, vce(cluster village_id)
	qui lincom codewords
    qui replace pval=r(p) if _n == `s'
	qui lincom keyboard
	qui replace pval=r(p) if _n == `f'
        
    local s = `s'+1
	local f = `f'+1
}
qqvalue pval, method(simes) qvalue(qval)

*2nd step: get coefficients for table
*For MDE we need mean of control, sd, sample size, power,  direction lower
foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if head_self==0, vce(cluster village_id)
	qui est sto c_`y'
	qui lincom codewords - keyboard
	qui estadd scalar pdiff = r(p)
	qui sum `y'
	estadd scalar cmean =r(mean)
	qui sum `y' if tment==0 & head_self==0
	gen conmean = r(mean)
	qui sum `y' if head_self==0
	gen sd = r(sd)
	qui sum `y' if tment!=2 & head_self==0
	gen con_code = r(N)
	qui sum `y' if tment!=1 & head_self==0
	gen con_key = r(N)
	power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
	qui estadd scalar mdecode = r(delta)
	power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
	qui estadd scalar mdekey = r(delta)
    drop conmean con_code con_key sd
}	

local i = 1
local j = 7
	
    *est clear  
    * Within the group of variables 
    foreach y in $`empower' {
        local num : word count $`y'
		disp "words:" `num'
        * OLS
        qui reg `y' codewords keyboard $`covariates' if head_self==0, vce(cluster village_id) 
        qui est sto c_`y'
		qui sum `y' if tment ==0 & head_self==0
		qui estadd scalar cmean = r(mean)
        qui lincom codewords - keyboard 
		qui estadd scalar pdiff = r(p)
		qui sum `y' if tment==0 & head_self==0
		gen conmean = r(mean)
		qui sum `y' if head_self==0
		gen sd = r(sd)
		qui sum `y' if tment!=2 & head_self==0
		gen con_code = r(N)
		qui sum `y' if tment!=1 & head_self==0
		gen con_key = r(N)
		power twomeans `=conmean', n(`=con_code') power(0.8) sd(`=sd') direction(lower)
		qui estadd scalar mdecode = r(delta)
		power twomeans `=conmean', n(`=con_key') sd(`=sd') power(0.8) direction(lower)
		qui estadd scalar mdekey = r(delta)
        drop conmean con_code con_key sd
		matrix code = J(1,2,.)
		matrix code[1,1] = qval[`i']
		matrix code[1,2] = qval[`j']
		mat colnames code = codewords keyboard
		mat li code
		estadd matrix code
		mat drop code
		local ++i
		local ++j
    }
drop pval qval

esttab n_empowerment_0 n_empowerment_1 n_empowerment_2 n_empowerment_3 n_empowerment_4 n_empowerment_5 n_empowerment_6 ///
using "$tables/new_results_wc_hhead.tex", ///
 prehead("\begin{tabular}{l*{8}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel A: Interviewee is HH Head}} \\\\[-1ex]") ///
 cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
 fragment collabels(none) ///
 mtitles("Index" "Allowed to go out" "Does not ask permission to buy goods" "Visit Friends" "Asset Purchasing" "Relatives Care" "Child Care" ) ///
 nonum keep (codewords keyboard) label ///
scalars("mdecode MDE Codewords" "mdekey MDE Keystroke" "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 3 3 0 3 3) ///
replace


esttab c_empowerment_0 c_empowerment_1 c_empowerment_2 c_empowerment_3 c_empowerment_4 c_empowerment_5 c_empowerment_6 ///
using "$tables/new_results_wc_hhead.tex", ///
posthead("\hline \\ \multicolumn{8}{c}{\textbf{Panel B: Other is HH Head}} \\\\[-1ex]") ///
cells(b(star fmt(3)) se(par fmt(3)) code(par([ ]) fmt(3))) starlevels(* 0.1 ** 0.05 *** 0.01) ///
fragment collabels(none) ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("mdecode MDE Codewords" "mdekey MDE Keystroke" "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared" "pdiff Pval Diff. between treatment") sfmt(3 3 3 0 3 3)  ///
prefoot("\hline" ) ///
postfoot("\hline \\ \multicolumn{8}{l}{\tiny Notes: Each column shows the coefficient of regressing each answer related to women empowerment" "on increased privacy (codenames or keystroke treatment).} \\ \multicolumn{8}{l}{\tiny Clustered Standard Errors at the village level in parentheses. Index is the average of all answers.} \\\multicolumn{8}{l}{\tiny Who is the household head was determined during the interview.} \\ \multicolumn{8}{l}{\tiny Panel A includes women who are the head of the household. Panel B includes women who are not the head of the household. } \\ \multicolumn{8}{l}{\tiny All regressions include control variables. Control variables include age, marital status, log of land size, women education level, partner education level, } \\ \multicolumn{8}{l}{\tiny household head, land ownership, number of (children, family members, adult men, adult women), and Digitagro participation.} \\  \multicolumn{8}{l}{\tiny Stars denote statistical significance at 1,5, and 10\% levels based on unadjusted p-values. FDR-q values in brackets.} \\ \multicolumn{8}{l}{\tiny FDR-q values indicate the probability of false positives among significant tests. The MDE is reported for each outcome.} \\ \multicolumn{8}{l}{\tiny The MDE stands for minimum detectable effect which is the smallest effect that, if true," "has an X\% chance of } \\ \multicolumn{8}{l}{\tiny producing an impact estimate that is statistically significant at the Y level. } \\ \multicolumn{8}{l}{\tiny The last row reports the p-value to check if there is a statistically significant difference between treatments. } \\  \hline\hline \end{tabular}")  label ///



est clear

*/


