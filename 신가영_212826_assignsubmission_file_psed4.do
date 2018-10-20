
/* psed_h4.do */

/* Panel Survey of Employment for the Disabled */
/* Assignment 4 : Stepwise Regression with Interaction terms */


clear
log using psed4, replace t 

set more off

use "C:\Users\user\Desktop/psed4_nf.dta", clear


xtset pid gubun  /* declare this data set as a panel data */


foreach var of varlist _all {
	label var `var' ""
}



/********************************/
/*    Variables of interest     */
/********************************/

local inc_v g003001 g005001 h102001 h104001 

/* log transformation for skewed distribution of income variables */

foreach i in `inc_v' { 
	gen ln`i'=1+`i'
	replace ln`i'=ln(ln`i')  
	}
	
	
/********************************/
/*    Labeling & Formatting     */
/********************************/

label variable pid "Personal ID"
label variable gubun "Survey year"
label variable p "Response"
label variable area "Region"
label variable gender "Gender"
label variable grade6 "Grade of Disability"
label variable school "Education Level"
label variable g001001 "Earned Income"
label variable g003001 "Earned Income Amount"
label variable g005001 "Not-earned Income Amount"
label variable lng003001 "Log_Earned Income Amount"
label variable lng005001 "Log_Not-earned Income Amount"
label variable emp3 "Employed Status"
label variable f001001 "Health"
label variable c060001 "Ability_Computer"
label variable c060002 "Ability_English"
label variable c060003 "Ability_Social"
label variable c061001 "Task Preference"
label variable h102001 "Earned Income Amount_Household"
label variable h104001 "Not-earned Income Amount_Household"
label variable h109001 "Expense_Household"
label variable dq002100 "Marital Status"
label variable f012010 "Perceived SES"
label variable f011001 "Being discriminated in everday life"

label def ab 1"Poor" 2"Below Average" 3"Average" 4"Above Average" 5"Outstanding"
label def emp_3 1"Employed" 2"Unemployed" 3"Not-economically Active"
label def grd_2 1"Mild" 2"Severe"
label def satis 1"Highly Dissatisfied" 2"Dissatisfied" 3"Neutral" 4"satisfied" 5"Highly satisfied"
label def sex 1"men" 2"women"

label value emp3 emp_3
label value c060001 ab
label value c060002 ab
label value c060003 ab
label value grade2 grd_2
label value gender sex



/****************************************************/
/*          Stepwise Regression Analysis            */
/****************************************************/

local ln_inc_v lng003001 lng005001

/* outlier check */

su g003001 g005001 h102001 h104001 h113002 gender school emp3 f001001 dq002100 grade6 age

recode g003001 (9999999=.) 
recode g003001 (9999998=.)


/* correlation check */

corr g003001 g005001 h102001 h104001 h113002 gender school emp3 f001001 dq002100 grade6 age
	

* model - Factors affecting a perceived SES *
	
xtreg f012010 `ln_inc_v' 
	outreg2 using dis_ses.xls,replace label title("Factors affecting perceived SES")
xtreg f012010 `ln_inc_v' h113002 
	outreg2 using dis_ses.xls,append label e(r2_o r2_b r2_w)
xtreg f012010 `ln_inc_v' h113002 i.emp3
	outreg2 using dis_ses.xls,append label e(r2_o r2_b r2_w)
xtreg f012010 `ln_inc_v' h113002 i.emp3 f001001
	outreg2 using dis_ses.xls,append label e(r2_o r2_b r2_w)
xtreg f012010 `ln_inc_v' h113002 i.emp3 f001001 dq002100
	outreg2 using dis_ses.xls,append label e(r2_o r2_b r2_w)
xtreg f012010 `ln_inc_v' h113002 i.emp3 f001001 dq002100 grade6
	outreg2 using dis_ses.xls,append label e(r2_o r2_b r2_w)
xtreg f012010 `ln_inc_v' h113002 i.emp3 f001001 dq002100 grade6 school
	outreg2 using dis_ses.xls,append label e(r2_o r2_b r2_w)
xtreg f012010 `ln_inc_v' h113002 i.emp3 f001001 dq002100 grade6 school age
	outreg2 using dis_ses.xls,append label e(r2_o r2_b r2_w)
xtreg f012010 `ln_inc_v' h113002 i.emp3 f001001 dq002100 grade6 school age i.gender
	outreg2 using dis_ses.xls,append label e(r2_o r2_b r2_w)
xtreg f012010 `ln_inc_v' h113002 i.emp3 f001001 dq002100 grade6 school age i.gender f011001
	outreg2 using dis_ses.xls,append label e(r2_o r2_b r2_w)
	

/****************************************************************/
/*          Stepwise Regression with interaction term           */
/****************************************************************/


xtreg f012010 `ln_inc_v' h113002 i.emp3 f001001 dq002100 grade6 school age i.gender f011001 c.f011001#c.school
	outreg2 using dis_ses.xls,append label e(r2_o r2_b r2_w)
	
		
* comparing with fixed effect model *
	
xtreg f012010 `ln_inc_v' h113002 i.emp3 f001001 dq002100 grade6 school age i.gender c.f011001#c.school, fe
	outreg2 using dis_ses.xls,append label
		

* output_doc format *

xtreg f012010 `ln_inc_v' h113002 i.emp3 f001001 dq002100 grade6 school age i.gender f011001 c.f011001#c.school
	outreg2 using dis_ses.doc,replace label e(r2_o r2_b r2_w) title("Factors affecting perceived SES")

		

/************************************/
/*      Other model of interest     */
/************************************/


recode d001001 (2=0) (1=1)
bysort pid: egen emp_g = sum(d001001)

label variable emp_g "empservice_gov"   /* How many times have you been in emp_services provided by governments? */


* model test *

reg emp_g `ln_inc_v' h113002 i.emp3 f001001 dq002100 i.grade6 school age i.gender f011001 c060001 c060002 c060003

	
	
	





