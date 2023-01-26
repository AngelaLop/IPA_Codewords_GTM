*===============================================================================
*Do File for Appendix Tables
*===============================================================================

*===============================================================================
*Results Table No Controls
*===============================================================================

*Create local variable lists to utilise in regressions
* List for Indexes
* List for SE Answers
* List for Covariates
local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

* Indexes
foreach y in `indexlist' {
	qui reg `y' codewords keyboard ,vce(cluster village_id)
	qui est sto n_`y'
	qui sum `y' if tment ==0
	qui estadd scalar cmean = r(mean)
}

esttab n_avg n_standard n_index_Q1Q5 ///
using "$tables/index_nc_h.tex", ///
star("*" 0.10 "**" 0.05 "***" 0.01) b(3) se(3) par keep(codewords keyboard) ///
compress booktabs label ///
nonumbers noobs nogaps addnotes("Notes: Clustered Standard Errors at the village level in parentheses." "Average Index takes the average of all outcomes, Standardized Index takes the average of the standardized outcomes." "Index from Q1-Q5 takes the average of the questions 1 to 5." ) ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3) ///
replace

est clear

*Controls

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

* Indexes
foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' ,vce(cluster village_id)
	qui est sto n_`y'
	qui sum `y' if tment ==0
	qui estadd scalar cmean = r(mean)
}

esttab n_avg n_standard n_index_Q1Q5 ///
using "$tables/index_wc_h.tex", ///
star("*" 0.10 "**" 0.05 "***" 0.01) b(3) se(3) par keep(codewords keyboard) ///
compress booktabs label ///
nonumbers noobs nogaps addnotes("Notes: Clustered Standard Errors at the village level in parentheses." "Average Index takes the average of all outcomes, Standardized Index takes the average of the standardized outcomes." "Index from Q1-Q5 takes the average of the questions 1 to 5." "Control variables include age, marital status, log of land size," "education of interviewee, education of HH head, Number of children in household, number of women in household," "number of men in household" ) ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3) ///
replace

est clear

*Fixed effects
*No Controls
local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4


* Indexes
foreach y in `indexlist' {
	xtset muninum
	qui xtreg `y' codewords keyboard ,fe nonest vce(cluster village_id)
	qui est sto n_`y'
	qui sum `y' if tment ==0
	qui estadd scalar cmean = r(mean)
}
esttab n_avg n_standard n_index_Q1Q5 ///
using "$tables/index_nc_fe.tex", ///
star("*" 0.10 "**" 0.05 "***" 0.01) b(3) se(3) par keep(codewords keyboard) ///
compress booktabs label ///
nonumbers noobs nogaps addnotes("Notes: Clustered Standard Errors at the village level in parentheses." "Average Index takes the average of all outcomes, Standardized Index takes the average of the standardized outcomes." "Index from Q1-Q5 takes the average of the questions 1 to 5."   ) ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3) ///
replace

est clear

*Controls
local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4


* Indexes
foreach y in `indexlist' {
	xtset muninum
	qui xtreg `y' codewords keyboard $`covariates',fe nonest vce(cluster village_id)
	qui est sto n_`y'
	qui sum `y' if tment ==0
	qui estadd scalar cmean = r(mean)
}
esttab n_avg n_standard n_index_Q1Q5 ///
using "$tables/index_wc_fe.tex", ///
star("*" 0.10 "**" 0.05 "***" 0.01) b(3) se(3) par keep(codewords keyboard) ///
compress booktabs label ///
nonumbers noobs nogaps addnotes("Notes: Clustered Standard Errors at the village level in parentheses." "Average Index takes the average of all outcomes, Standardized Index takes the average of the standardized outcomes." "Index from Q1-Q5 takes the average of the questions 1 to 5." "Control variables include age, marital status, log of land size," "education of interviewee, education of HH head, Number of children in household, number of women in household," "number of men in household" ) ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3) ///
replace

est clear

/*
*Education
* No Controls 

*1st panel

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*No Primary
* Indexes

foreach y in `indexlist' {
	qui reg `y' codewords keyboard if home_12 <3,vce(cluster village_id)
	qui est sto n_`y'
	qui sum `y' if tment ==0 & home_12 <3
	qui estadd scalar cmean = r(mean)
}


*2nd panel (Finished Primary)

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*No Primary
* Indexes
foreach y in `indexlist' {
	qui reg `y' codewords keyboard if home_12>2,vce(cluster village_id)
	qui est sto c_`y'
	qui sum `y' if tment ==0 & home_12 >2
	qui estadd scalar cmean = r(mean)
}

*top panel

*esttab n_avg n_standard n_index_Q1Q5  ///
*using "$tables/index_nc_primary.tex", ///
 *prehead("\begin{tabular}{l*{4}{c}} \hline\hline") ///
 *posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel A: No Primary}} \\\\[-1ex]") ///
 *b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
 *mtitles("Average Index" "Standardized Index" "Index from Q1-Q5" ) ///
 *nonum keep (codewords keyboard) label ///
*scalars( "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3) ///
*replace


*esttab c_avg c_standard c_index_Q1Q5 ///
*using "$tables/index_nc_primary.tex", ///
*posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel B: Primary}} \\\\[-1ex]") ///
*b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
*append ///
*nonum nomti nolines keep (codewords keyboard) ///
*scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3)  ///
*notes addnotes("Notes: Clustered Standard Errors at the village level in parentheses." "Average Index takes the average of all outcomes, Standardized Index takes the average of the standardized outcomes." "Index from Q1-Q5 takes the average of the questions 1 to 5."  ) ///
*prefoot("\hline") ///
*postfoot("\hline\hline \end{tabular}") label ///


*est clear
*Controls

*1st panel

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*No Primary
* Indexes

foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if home_12 <3,vce(cluster village_id)
	qui est sto n_`y'
	qui sum `y' if tment ==0 & home_12 <3
	qui estadd scalar cmean = r(mean)
}


*2nd panel (Finished Primary)

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*No Primary
* Indexes
foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if home_12>2,vce(cluster village)
	qui est sto c_`y'
	qui sum `y' if tment ==0 & home_12 >2
	qui estadd scalar cmean = r(mean)
}

*top panel

esttab n_avg n_standard n_index_Q1Q5  ///
using "$tables/index_wc_primary.tex", ///
 prehead("\begin{tabular}{l*{4}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel A: No Primary}} \\\\[-1ex]") ///
 b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
 mtitles("Average Index" "Standardized Index" "Index from Q1-Q5" ) ///
 nonum keep (codewords keyboard) label ///
scalars( "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3) ///
replace


esttab c_avg c_standard c_index_Q1Q5 ///
using "$tables/index_wc_primary.tex", ///
posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel B: Primary}} \\\\[-1ex]") ///
b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3)  ///
notes addnotes("Notes: Clustered Standard Errors at the village level in parentheses." "Average Index takes the average of all outcomes, Standardized Index takes the average of the standardized outcomes." "Index from Q1-Q5 takes the average of the questions 1 to 5." "Control variables include age, marital status, log of land size," "education of interviewee, education of HH head, Number of children in household, number of women in household," "number of men in household" ) ///
prefoot("\hline") ///
postfoot("\hline\hline \end{tabular}") label ///

*Marital
* No Controls 

*1st panel

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*Single
* Indexes

foreach y in `indexlist' {
	qui reg `y' codewords keyboard if single==1,vce(cluster village_id)
	qui est sto n_`y'
	qui sum `y' if tment ==0 & single==1
	qui estadd scalar cmean = r(mean)
}


*2nd panel 

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*Married
* Indexes
foreach y in `indexlist' {
	qui reg `y' codewords keyboard if single==0,vce(cluster village)
	qui est sto c_`y'
	qui sum `y' if tment ==0 & single==0
	qui estadd scalar cmean = r(mean)
}

*top panel

esttab n_avg n_standard n_index_Q1Q5  ///
using "$tables/index_nc_marital.tex", ///
 prehead("\begin{tabular}{l*{4}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel A: Single}} \\\\[-1ex]") ///
 b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
 mtitles("Average Index" "Standardized Index" "Index from Q1-Q5" ) ///
 nonum keep (codewords keyboard) label ///
scalars( "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3) ///
replace


esttab c_avg c_standard c_index_Q1Q5 ///
using "$tables/index_nc_marital.tex", ///
posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel B: Married}} \\\\[-1ex]") ///
b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3)  ///
notes addnotes("Notes: Clustered Standard Errors at the village_id level in parentheses." "Average Index takes the average of all outcomes, Standardized Index takes the average of the standardized outcomes." "Index from Q1-Q5 takes the average of the questions 1 to 5." ) ///
prefoot("\hline") ///
postfoot("\hline\hline \end{tabular}") label ///


est clear

*Controls


*1st panel

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*Single
* Indexes

foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if single==1,vce(cluster village)
	qui est sto n_`y'
	qui sum `y' if tment ==0 & single==1
	qui estadd scalar cmean = r(mean)
}


*2nd panel 

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*Married
* Indexes
foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if single==0,vce(cluster village_id)
	qui est sto c_`y'
	qui sum `y' if tment ==0 & single==0
	qui estadd scalar cmean = r(mean)
}

*top panel

esttab n_avg n_standard n_index_Q1Q5  ///
using "$tables/index_wc_marital.tex", ///
 prehead("\begin{tabular}{l*{4}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel A: Single}} \\\\[-1ex]") ///
 b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
 mtitles("Average Index" "Standardized Index" "Index from Q1-Q5" ) ///
 nonum keep (codewords keyboard) label ///
scalars( "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3) ///
replace


esttab c_avg c_standard c_index_Q1Q5 ///
using "$tables/index_wc_marital.tex", ///
posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel B: Married}} \\\\[-1ex]") ///
b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3)  ///
notes addnotes("Notes: Clustered Standard Errors at the village level in parentheses." "Average Index takes the average of all outcomes, Standardized Index takes the average of the standardized outcomes." "Index from Q1-Q5 takes the average of the questions 1 to 5." "Control variables include age, marital status, log of land size," "education of interviewee, education of HH head, Number of children in household, number of women in household," "number of men in household"  ) ///
prefoot("\hline") ///
postfoot("\hline\hline \end{tabular}") label ///


est clear


*Vulnerability

* No Controls 

*1st panel

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*Single
* Indexes

foreach y in `indexlist' {
	qui reg `y' codewords keyboard if vulnerability <= -.0505324 ,vce(cluster village_id)
	qui est sto n_`y'
	qui sum `y' if tment ==0 & vulnerability <= -.0505324 
	qui estadd scalar cmean = r(mean)
}


*2nd panel 

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*Married
* Indexes
foreach y in `indexlist' {
	qui reg `y' codewords keyboard if vulnerability > -.0505324 ,vce(cluster village_id)
	qui est sto c_`y'
	qui sum `y' if tment ==0 & vulnerability > -.0505324 
	qui estadd scalar cmean = r(mean)
}

*top panel
esttab n_avg n_standard n_index_Q1Q5  ///
using "$tables/index_nc_vulnerability.tex", ///
 prehead("\begin{tabular}{l*{4}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel A: Low Vulnerability}} \\\\[-1ex]") ///
 b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
 mtitles("Average Index" "Standardized Index" "Index from Q1-Q5" ) ///
 nonum keep (codewords keyboard) label ///
scalars( "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3) ///
replace


esttab c_avg c_standard c_index_Q1Q5 ///
using "$tables/index_nc_vulnerability.tex", ///
posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel B: High Vulnerability}} \\\\[-1ex]") ///
b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3)  ///
notes addnotes("Notes: Clustered Standard Errors at the village level in parentheses." "Average Index takes the average of all outcomes, Standardized Index takes the average of the standardized outcomes." "Index from Q1-Q5 takes the average of the questions 1 to 5." ) ///
prefoot("\hline") ///
postfoot("\hline\hline \end{tabular}") label ///


est clear

*Controls 

*1st panel

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*Single
* Indexes

foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if vulnerability <= -.0505324 ,vce(cluster village_id)
	qui est sto n_`y'
	qui sum `y' if tment ==0 & vulnerability <= -.0505324 
	qui estadd scalar cmean = r(mean)
}


*2nd panel 

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*Married
* Indexes
foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if vulnerability > -.0505324,vce(cluster village)
	qui est sto c_`y'
	qui sum `y' if tment ==0 & vulnerability > -.0505324
	qui estadd scalar cmean = r(mean)
}

*top panel

esttab n_avg n_standard n_index_Q1Q5  ///
using "$tables/index_wc_vulnerability.tex", ///
 prehead("\begin{tabular}{l*{4}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel A: Low Vulnerability}} \\\\[-1ex]") ///
 b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
 mtitles("Average Index" "Standardized Index" "Index from Q1-Q5" ) ///
 nonum keep (codewords keyboard) label ///
scalars( "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3) ///
replace


esttab c_avg c_standard c_index_Q1Q5 ///
using "$tables/index_wc_vulnerability.tex", ///
posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel B: High Vulnerability}} \\\\[-1ex]") ///
b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3)  ///
notes addnotes("Notes: Clustered Standard Errors at the village level in parentheses." "Average Index takes the average of all outcomes, Standardized Index takes the average of the standardized outcomes." "Index from Q1-Q5 takes the average of the questions 1 to 5." "Control variables include age, marital status, log of land size," "education of interviewee, education of HH head, Number of children in household, number of women in household," "number of men in household" ) ///
prefoot("\hline") ///
postfoot("\hline\hline \end{tabular}") label ///


est clear

*Land Size


* No Controls 

*1st panel

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

* Indexes

foreach y in `indexlist' {
	qui reg `y' codewords keyboard if farm_2 <= 4 ,vce(cluster village_id)
	qui est sto n_`y'
	qui sum `y' if tment ==0 & farm_2 <= 4
	qui estadd scalar cmean = r(mean)
}


*2nd panel 

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*Married
* Indexes
foreach y in `indexlist' {
	qui reg `y' codewords keyboard if farm_2 >4,vce(cluster village_id)
	qui est sto c_`y'
	qui sum `y' if tment ==0 & farm_2 >4
	qui estadd scalar cmean = r(mean)
}

*top panel

esttab n_avg n_standard n_index_Q1Q5  ///
using "$tables/index_nc_land.tex", ///
 prehead("\begin{tabular}{l*{4}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel A: Smaller Land}} \\\\[-1ex]") ///
 b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
 mtitles("Average Index" "Standardized Index" "Index from Q1-Q5" ) ///
 nonum keep (codewords keyboard) label ///
scalars( "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3) ///
replace


esttab c_avg c_standard c_index_Q1Q5 ///
using "$tables/index_nc_land.tex", ///
posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel B: Bigger Land}} \\\\[-1ex]") ///
b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3)  ///
notes addnotes("Notes: Clustered Standard Errors at the village level in parentheses." "Average Index takes the average of all outcomes, Standardized Index takes the average of the standardized outcomes." "Index from Q1-Q5 takes the average of the questions 1 to 5." ) ///
prefoot("\hline") ///
postfoot("\hline\hline \end{tabular}") label ///


est clear

*Controls 

*1st panel

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*Single
* Indexes

foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if farm_2 <= 4,vce(cluster village_id)
	qui est sto n_`y'
	qui sum `y' if tment ==0 & farm_2 <= 4
	qui estadd scalar cmean = r(mean)
}


*2nd panel 

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*Married
* Indexes
foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if farm_2 >4,vce(cluster village_id)
	qui est sto c_`y'
	qui sum `y' if tment ==0 & farm_2 >4
	qui estadd scalar cmean = r(mean)
}

*top panel

esttab n_avg n_standard n_index_Q1Q5  ///
using "$tables/index_wc_land.tex", ///
 prehead("\begin{tabular}{l*{4}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel A: Smaller Land}} \\\\[-1ex]") ///
 b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
 mtitles("Average Index" "Standardized Index" "Index from Q1-Q5" ) ///
 nonum keep (codewords keyboard) label ///
scalars( "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3) ///
replace


esttab c_avg c_standard c_index_Q1Q5 ///
using "$tables/index_wc_land.tex", ///
posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel B: Bigger Land}} \\\\[-1ex]") ///
b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3)  ///
notes addnotes("Notes: Clustered Standard Errors at the village level in parentheses." "Average Index takes the average of all outcomes, Standardized Index takes the average of the standardized outcomes." "Index from Q1-Q5 takes the average of the questions 1 to 5." "Control variables include age, marital status, log of land size," "education of interviewee, education of HH head, Number of children in household, number of women in household," "number of men in household"  ) ///
prefoot("\hline") ///
postfoot("\hline\hline \end{tabular}") label ///


est clear

*Noise 
* No Controls 

*1st panel

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

* Indexes

foreach y in `indexlist' {
	qui reg `y' codewords keyboard if enum_evaluation2==0 ,vce(cluster village_id)
	qui est sto n_`y'
	qui sum `y' if tment ==0 & enum_evaluation2==0
	qui estadd scalar cmean = r(mean)
}


*2nd panel 

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*Married
* Indexes
foreach y in `indexlist' {
	qui reg `y' codewords keyboard if enum_evaluation2==1, vce(cluster village_id)
	qui est sto c_`y'
	qui sum `y' if tment ==0 & enum_evaluation2==1
	qui estadd scalar cmean = r(mean)
}

*top panel

esttab n_avg n_standard n_index_Q1Q5  ///
using "$tables/index_nc_noise.tex", ///
 prehead("\begin{tabular}{l*{4}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel A: No Noise}} \\\\[-1ex]") ///
 b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
 mtitles("Average Index" "Standardized Index" "Index from Q1-Q5" ) ///
 nonum keep (codewords keyboard) label ///
scalars( "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3) ///
replace


esttab c_avg c_standard c_index_Q1Q5 ///
using "$tables/index_nc_noise.tex", ///
posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel B: Noise}} \\\\[-1ex]") ///
b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3)  ///
notes addnotes("Notes: Clustered Standard Errors at the village level in parentheses." "Average Index takes the average of all outcomes, Standardized Index takes the average of the standardized outcomes." "Index from Q1-Q5 takes the average of the questions 1 to 5." ) ///
prefoot("\hline") ///
postfoot("\hline\hline \end{tabular}") label ///


est clear

*Controls 

*1st panel

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*Single
* Indexes

foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if enum_evaluation2==0, vce(cluster village_id)
	qui est sto n_`y'
	qui sum `y' if tment ==0 & enum_evaluation2==0
	qui estadd scalar cmean = r(mean)
}


*2nd panel 

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*Married
* Indexes
foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if enum_evaluation2==1, vce(cluster village_id)
	qui est sto c_`y'
	qui sum `y' if tment ==0 & enum_evaluation2==1
	qui estadd scalar cmean = r(mean)
}

*top panel

esttab n_avg n_standard n_index_Q1Q5  ///
using "$tables/index_wc_noise.tex", ///
 prehead("\begin{tabular}{l*{4}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel A: Noise}} \\\\[-1ex]") ///
 b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
 mtitles("Average Index" "Standardized Index" "Index from Q1-Q5" ) ///
 nonum keep (codewords keyboard) label ///
scalars( "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3) ///
replace


esttab c_avg c_standard c_index_Q1Q5 ///
using "$tables/index_wc_noise.tex", ///
posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel B: Noise}} \\\\[-1ex]") ///
b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3)  ///
notes addnotes("Notes: Clustered Standard Errors at the village level in parentheses." "Average Index takes the average of all outcomes, Standardized Index takes the average of the standardized outcomes." "Index from Q1-Q5 takes the average of the questions 1 to 5." "Control variables include age, marital status, log of land size," "education of interviewee, education of HH head, Number of children in household, number of women in household," "number of men in household"  ) ///
prefoot("\hline") ///
postfoot("\hline\hline \end{tabular}") label ///


est clear

*Assessment

* No Controls 

*1st panel

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

* Indexes

foreach y in `indexlist' {
	qui reg `y' codewords keyboard if enum_evaluation1<2 ,vce(cluster village_id)
	qui est sto n_`y'
	qui sum `y' if tment ==0 & enum_evaluation1<2
	qui estadd scalar cmean = r(mean)
}


*2nd panel 

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4


* Indexes
foreach y in `indexlist' {
	qui reg `y' codewords keyboard if enum_evaluation1>1, vce(cluster village_id)
	qui est sto c_`y'
	qui sum `y' if tment ==0 & enum_evaluation1>1
	qui estadd scalar cmean = r(mean)
}

*top panel

esttab n_avg n_standard n_index_Q1Q5  ///
using "$tables/index_nc_assess.tex", ///
 prehead("\begin{tabular}{l*{4}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel A: Comfortable}} \\\\[-1ex]") ///
 b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
 mtitles("Average Index" "Standardized Index" "Index from Q1-Q5" ) ///
 nonum keep (codewords keyboard) label ///
scalars( "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3) ///
replace


esttab c_avg c_standard c_index_Q1Q5 ///
using "$tables/index_nc_assess.tex", ///
posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel B: Uncomfortable}} \\\\[-1ex]") ///
b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3)  ///
notes addnotes("Notes: Clustered Standard Errors at the village level in parentheses." "Average Index takes the average of all outcomes, Standardized Index takes the average of the standardized outcomes." "Index from Q1-Q5 takes the average of the questions 1 to 5."  ) ///
prefoot("\hline") ///
postfoot("\hline\hline \end{tabular}") label ///


est clear

*Controls 

*1st panel

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

* Indexes

foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if enum_evaluation1<2, vce(cluster village_id)
	qui est sto n_`y'
	qui sum `y' if tment ==0 & enum_evaluation1<2
	qui estadd scalar cmean = r(mean)
}


*2nd panel 

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4


* Indexes
foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if enum_evaluation1>1, vce(cluster village_id)
	qui est sto c_`y'
	qui sum `y' if tment ==0 & enum_evaluation1>1
	qui estadd scalar cmean = r(mean)
}

*top panel

esttab n_avg n_standard n_index_Q1Q5  ///
using "$tables/index_wc_assess.tex", ///
 prehead("\begin{tabular}{l*{4}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel A: Comfortable}} \\\\[-1ex]") ///
 b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
 mtitles("Average Index" "Standardized Index" "Index from Q1-Q5" ) ///
 nonum keep (codewords keyboard) label ///
scalars( "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3) ///
replace


esttab c_avg c_standard c_index_Q1Q5 ///
using "$tables/index_wc_assess.tex", ///
posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel B: Uncomfortable}} \\\\[-1ex]") ///
b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3)  ///
notes addnotes("Notes: Clustered Standard Errors at the village level in parentheses." "Average Index takes the average of all outcomes, Standardized Index takes the average of the standardized outcomes." "Index from Q1-Q5 takes the average of the questions 1 to 5." "Control variables include age, marital status, log of land size," "education of interviewee, education of HH head, Number of children in household, number of women in household," "number of men in household" ) ///
prefoot("\hline") ///
postfoot("\hline\hline \end{tabular}") label ///


est clear

*HH Head

* No Controls 

*1st panel

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

* Indexes

foreach y in `indexlist' {
	qui reg `y' codewords keyboard if head_self==1 ,vce(cluster village_id)
	qui est sto n_`y'
	qui sum `y' if tment ==0 & head_self==1
	qui estadd scalar cmean = r(mean)
}


*2nd panel 

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

* Indexes
foreach y in `indexlist' {
	qui reg `y' codewords keyboard if head_self==0, vce(cluster village_id)
	qui est sto c_`y'
	qui sum `y' if tment ==0 & head_self==0
	qui estadd scalar cmean = r(mean)
}

*top panel

esttab n_avg n_standard n_index_Q1Q5  ///
using "$tables/index_nc_hhead.tex", ///
 prehead("\begin{tabular}{l*{4}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel A: Interviewee is HH Head}} \\\\[-1ex]") ///
 b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
 mtitles("Average Index" "Standardized Index" "Index from Q1-Q5" ) ///
 nonum keep (codewords keyboard) label ///
scalars( "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3) ///
replace


esttab c_avg c_standard c_index_Q1Q5 ///
using "$tables/index_nc_hhead.tex", ///
posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel B: Other is HH Head}} \\\\[-1ex]") ///
b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3)  ///
notes addnotes("Notes: Clustered Standard Errors at the village level in parentheses." "Average Index takes the average of all outcomes, Standardized Index takes the average of the standardized outcomes." "Index from Q1-Q5 takes the average of the questions 1 to 5."  ) ///
prefoot("\hline") ///
postfoot("\hline\hline \end{tabular}") label ///


est clear

*Controls 

*1st panel

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*Single
* Indexes

foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if head_self==1, vce(cluster village_id)
	qui est sto n_`y'
	qui sum `y' if tment ==0 & head_self==1
	qui estadd scalar cmean = r(mean)
}


*2nd panel 

local indexlist avg standard index_Q1Q5
local empower index empowerment_1 empowerment_2 empowerment_3 empowerment_4 empowerment_5 empowerment_6
local covariates index edad_nacimiento aplica_mam single ln_land primary_w primary_head head_self head_partner home_2 home_3 home_4 home_5 digitagro_baseline farm_4

*Married
* Indexes
foreach y in `indexlist' {
	qui reg `y' codewords keyboard $`covariates' if head_self==0, vce(cluster village_id)
	qui est sto c_`y'
	qui sum `y' if tment ==0 & head_self==0
	qui estadd scalar cmean = r(mean)
}

*top panel

esttab n_avg n_standard n_index_Q1Q5  ///
using "$tables/index_wc_hhead.tex", ///
 prehead("\begin{tabular}{l*{4}{c}} \hline\hline") ///
 posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel A: Interviewee is HH Head}} \\\\[-1ex]") ///
 b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
 mtitles("Average Index" "Standardized Index" "Index from Q1-Q5" ) ///
 nonum keep (codewords keyboard) label ///
scalars( "cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3) ///
replace


esttab c_avg c_standard c_index_Q1Q5 ///
using "$tables/index_wc_hhead.tex", ///
posthead("\hline \\ \multicolumn{4}{c}{\textbf{Panel B: Other is HH Head}} \\\\[-1ex]") ///
b(3) se(3) star("*" 0.10 "**" 0.05 "***" 0.01)  fragment ///
append ///
nonum nomti nolines keep (codewords keyboard) ///
scalars("cmean Control Mean" "N Obs." "r2_a Adjusted R-squared") sfmt(3 0 3)  ///
notes addnotes("Notes: Clustered Standard Errors at the village_id level in parentheses." "Average Index takes the average of all outcomes, Standardized Index takes the average of the standardized outcomes." "Index from Q1-Q5 takes the average of the questions 1 to 5." "Control variables include age, marital status, log of land size," "education of interviewee, education of HH head, Number of children in household, number of women in household," "number of men in household" ) ///
prefoot("\hline") ///
postfoot("\hline\hline \end{tabular}") label ///


est clear
*/