********************************************************************************
* Preamble, globals, and import
********************************************************************************
clear all

global import "/Users/trevorincerti/gitrepos/corruption_meta/data/"
global export "/Users/trevorincerti/gitrepos/corruption_meta/figs/"

********************************************************************************
* Define categories: Breitenstein
********************************************************************************
use "${import}/choosing_crook_clean.dta", replace

* Create categories
decode nperformance, gen(economy)
decode nqualities, gen(experience)
decode samep, gen(copartisan)

* Create categories
gen category = .
replace category = 1 if corrupt == 1 & economy == "bad" & experience == "low" & copartisan == "Different party"
replace category = 2 if corrupt == 1 & economy == "bad" & experience == "high" & copartisan == "Different party"
replace category = 3 if corrupt == 1 & economy == "bad" & experience == "low" & copartisan == "Same party"
replace category = 4 if corrupt == 1 & economy == "bad" & experience == "high" & copartisan == "Same party"

replace category = 5 if corrupt == 1 & economy == "good" & experience == "low" & copartisan == "Different party"
replace category = 6 if corrupt == 1 & economy == "good" & experience == "high" & copartisan == "Different party"
replace category = 7 if corrupt == 1 & economy == "good" & experience == "low" & copartisan == "Same party"
replace category = 8 if corrupt == 1 & economy == "good" & experience == "high" & copartisan == "Same party"

* Name categories
label define category ////
		1 `" "Poor economy" "Low experience" "Different party" "' ////
		2 `" "Poor economy" "High experience" "Different party" "' ////
		3 `" "Poor economy" "Low experience" "Copartisan" "' ////
		4 `" "Poor economy" "High experience" "Copartisan" "' ////
		5 `" "Strong economy" "Low experience" "Different party" "' ////
		6 `" "Strong economy" "High experience" "Different party" "' ////
		7 `" "Strong economy" "Low experience" "Copartisan" "' ////
		8 `" "Strong economy" "High experience" "Copartisan" "' ////

label val category category

********************************************************************************
* Calculate marginal effects
********************************************************************************
reg Y i.category, cl(id)
margins category

********************************************************************************
* Plot marginal effects
********************************************************************************
* Sort results
matrix plot = r(table)'

matsort plot 1 "up"

matrix plot = plot'

* Create plot
mylabels 0(25)100, myscale(@/100) local(myla) 

coefplot (matrix(plot[1,])), ci((plot[5,] plot[6,])) ///
	vertical scheme(s1color) ylabel(`myla') ///
	ytitle("Probability of voting for corrupt candidate (%)" " ") ///
	xtitle(" " "Candidate profile") ///
	yline(.5, lstyle(grid)) ///
	xlabel(, labsize(small)) ///
	xsize(6.6) ysize(3) ///
	color(midblue) ciopts(lcolor(gs10))

* Export plot
graph export "${export}/b_margins.pdf", replace

********************************************************************************
* Repeat analyis with clean alternative only
******************************************************************************** 
gen alternative = "clean" if corrupt == 1 & corrupt[_n+1] == 0 & candidate == 1 & candidate[_n+1] == 2
replace alternative = "clean" if corrupt == 0 & corrupt[_n-1] == 1 & candidate == 2 & candidate[_n-1] == 1
replace alternative = "clean" if corrupt == 0 & corrupt[_n+1] == 1 & candidate == 1 & candidate[_n+1] == 2
replace alternative = "clean" if corrupt == 1 & corrupt[_n-1] == 0 & candidate == 2 & candidate[_n-1] == 1
keep if alternative == "clean"

* Calculate predicted probabilities
reg Y i.category, cl(id)
margins category

* Sort results
matrix plot = r(table)'

matsort plot 1 "up"

matrix plot = plot'

* Create plot
mylabels 0(25)100, myscale(@/100) local(myla) 

coefplot (matrix(plot[1,])), ci((plot[5,] plot[6,])) ///
	vertical scheme(s1color) ylabel(`myla') ///
	ytitle("Probability of voting for corrupt candidate (%)" " ") ///
	xtitle(" " "Candidate profile") ///
	yline(.5, lstyle(grid)) ///
	xlabel(, labsize(small)) ///
	xsize(6.6) ysize(3) ///
	color(midblue) ciopts(lcolor(gs10))

* Export plot
graph export "${export}/b_margins_clean.pdf", replace


********************************************************************************
* Define categories: Franchino and Zucchini
********************************************************************************
use "${import}/franchino_zucchini.dta", replace

* Create left vs. right categorization
gen left = 0
replace left = 1 if left_right <= 4

gen right = 0
replace right = 1 if left_right >= 6

* Drop NA values
drop if missing(Y)

* Pool corruption into one treatment
gen corrupt = 0
replace corrupt = 1 if corruption == 2 | corruption == 3

* Create categories
decode taxspend, gen(tax)
decode samesex, gen(same)

* Create categories
gen category = .
replace category = 1 if corrupt == 1 & tax == "Cut taxes" & same == "No rights"
replace category = 2 if corrupt == 1 & tax == "More social services" & same == "No rights"
replace category = 3 if corrupt == 1 & tax == "Maintain level of provision" & same == "No rights"

replace category = 4 if corrupt == 1 & tax == "Cut taxes" & same == "Some rights"
replace category = 5 if corrupt == 1 & tax == "More social services" & same == "Some rights"
replace category = 6 if corrupt == 1 & tax == "Maintain level of provision" & same == "Some rights"

replace category = 7 if corrupt == 1 & tax == "Cut taxes" & same == "Same rights"
replace category = 8 if corrupt == 1 & tax == "More social services" & same == "Same rights"
replace category = 9 if corrupt == 1 & tax == "Maintain level of provision" & same == "Same rights"

* Name categories
label define category ////
		1 `" "Cut taxes" "No rights" "' ////
		2 `" "More services" "No rights" "' ////
		3 `" "Status quo" "No rights" "' ////	
		4 `" "Cut taxes" "Some rights" "' ////
		5 `" "More services" "Some rights" "' ////
		6 `" "Status quo" "Some rights" "' ////	
		7 `" "Cut taxes" "Same rights" "' ////
		8 `" "More services" "Same rights" "' ////
		9 `" "Status quo" "Same rights" "' ////

label val category category

********************************************************************************
* Calculate marginal effects
********************************************************************************
foreach i in left right {
preserve

keep if `i' == 1

reg Y i.category, cl(IDContatto)
margins category

********************************************************************************
* Plot marginal effects
********************************************************************************
* Sort results
matrix plot = r(table)'

matsort plot 1 "up"

matrix plot = plot'

* Create plot
mylabels 0(25)100, myscale(@/100) local(myla) 

coefplot (matrix(plot[1,])), ci((plot[5,] plot[6,])) ///
	vertical scheme(s1color) ylabel(`myla') ///
	ytitle("Probability of voting for corrupt candidate (%)" " ") ///
	xtitle(" " "Policy platform") ///
	yline(.5, lstyle(grid)) ///
	xlabel(, labsize(medsmall)) ///
	xsize(6.6) ysize(3) ///
	color(midblue) ciopts(lcolor(gs10))

* Export plot
graph export "${export}/fz_margins_`i'.pdf", replace

restore
}

********************************************************************************
* Repeat analyis with clean alternative only
******************************************************************************** 
gen alternative = "clean" if corrupt == 1 & corrupt[_n+1] == 0 & n_vote == n_vote[_n+1]
replace alternative = "clean" if corrupt == 0 & corrupt[_n-1] == 1 & n_vote == n_vote[_n-1]
replace alternative = "clean" if corrupt == 0 & corrupt[_n+1] == 1 & n_vote == n_vote[_n+1]
replace alternative = "clean" if corrupt == 1 & corrupt[_n-1] == 0 & n_vote == n_vote[_n-1]
keep if alternative == "clean"

* Calculate predicted probabilies
foreach i in left right {
preserve

keep if `i' == 1

reg Y i.category, cl(IDContatto)
margins category


* Sort results
matrix plot = r(table)'

matsort plot 1 "up"

matrix plot = plot'

* Create plot
mylabels 0(25)100, myscale(@/100) local(myla) 

coefplot (matrix(plot[1,])), ci((plot[5,] plot[6,])) ///
	vertical scheme(s1color) ylabel(`myla') ///
	ytitle("Probability of voting for corrupt candidate (%)" " ") ///
	xtitle(" " "Policy platform") ///
	yline(.5, lstyle(grid)) ///
	xlabel(, labsize(medsmall)) ///
	xsize(6.6) ysize(3) ///
	color(midblue) ciopts(lcolor(gs10))

* Export plot
graph export "${export}/fz_margins_`i'_clean.pdf", replace

restore
}





