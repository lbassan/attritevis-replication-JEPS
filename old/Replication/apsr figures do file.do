

label define treatment 1 "1.  treat 1 90/90" 2 "2. treat 2A retro nuke" 3 "3. treat 2B retro conv" 4 "4. 3A" 5 "5. 3B" 6 "6. 3C" 7 "7. 3D" 8 "8. 4A" 9 "9. 4B" 10 "10. treat 5A 90/45" 11 "11. treat 5B 90/70" 12 "12. 6A" 13 "13. 6B" 14 "14. 7A" 15 "15. 7B" 16 "16. 8A" 17 "17. 8B" 18 "18. 8C" 98 "98. Skipped" 99 "99. Not Asked", replace

*** Figure 1A

gen q3_t1_convpref_dummy = q3_t1
recode q3_t1_convpref_dummy 1=1 2=1 3=0 4=0
gen q3_t1_nukepref_dummy = q3_t1
recode q3_t1_nukepref_dummy  1=0 2=0 3=1 4=1
mean  q3_t1_convpref_dummy q3_t1_nukepref_dummy [pweight=weight]
test  q3_t1_nukepref_dummy= q3_t1_convpref_dummy

*** Figure 1B

gen q2a_t1_approve_dummy  =  q2a_t1
recode q2a_t1_approve_dummy  1=0 2=0 3=0 4=1 5=1 6=1
gen q2a_t1_disapprove_dummy  = q2a_t1
recode q2a_t1_disapprove_dummy  1=1 2=1 3=1 4=0 5=0 6=0

mean  q2a_t1_approve_dummy q2a_t1_disapprove_dummy [pweight=weight]
test  q2a_t1_approve_dummy= q2a_t1_disapprove_dummy

*** Figure 2
gen q2a_t2ab_dummy = .
replace q2a_t2ab_dummy = q2A_t2A if treatment==2
replace q2a_t2ab_dummy = q2a_t2B if treatment==3
recode q2a_t2ab_dummy 1=0 2=0 3=0 4=1 5=1 6=1
mean  q2a_t2ab_dummy, over( treatment), [pweight=weight]
test [ q2a_t2ab_dummy]_subpop_1= [ q2a_t2ab_dummy]_subpop_2

gen q2b_t2ab_dummy = .
replace q2b_t2ab_dummy = q2b_t2A if treatment==2
replace q2b_t2ab_dummy = q2b_t2B if treatment==3
recode q2b_t2ab_dummy 1=1 2=1 3=1 4=0 5=0 6=0
mean  q2b_t2ab_dummy, over( treatment), [pweight=weight]
test [ q2b_t2ab_dummy]_subpop_1= [ q2b_t2ab_dummy]_subpop_2

***Figure 3

gen q3_treats1_5a_5b_dummy = .
replace q3_treats1_5a_5b_dummy = q3_t1 if treatment==1
replace q3_treats1_5a_5b_dummy = q3_t5A if treatment==10
replace q3_treats1_5a_5b_dummy = q3_t5B if treatment==11
recode q3_treats1_5a_5b_dummy  1=0 2=0 3=1 4=1
mean  q3_treats1_5a_5b_dummy, over(  treatment ), [pweight=weight]
test [  q3_treats1_5a_5b_dummy]_subpop_1=[  q3_treats1_5a_5b_dummy]_subpop_2
test [  q3_treats1_5a_5b_dummy]_subpop_2=[  q3_treats1_5a_5b_dummy]_subpop_3
test [  q3_treats1_5a_5b_dummy]_subpop_1=[  q3_treats1_5a_5b_dummy]_subpop_3

gen q2a_treats1_5a_5b_dummy = .
replace q2a_treats1_5a_5b_dummy = q2a_t1 if treatment==1
replace q2a_treats1_5a_5b_dummy = q2a_t5A if treatment==10
replace q2a_treats1_5a_5b_dummy = q2a_t5B if treatment==11
recode q2a_treats1_5a_5b_dummy  1=0 2=0 3=0 4=1 5=1 6=1
mean  q2a_treats1_5a_5b_dummy, over(  treatment ), [pweight=weight]
test [  q2a_treats1_5a_5b_dummy]_subpop_1=[  q2a_treats1_5a_5b_dummy]_subpop_2
test [  q2a_treats1_5a_5b_dummy]_subpop_2=[  q2a_treats1_5a_5b_dummy]_subpop_3
test [  q2a_treats1_5a_5b_dummy]_subpop_1=[  q2a_treats1_5a_5b_dummy]_subpop_3

***Figure 4
svyset [pweight=weight]

gen q5b_t1_withnuke = q5b_t1
label define conv_pref_reasons_withnuke 1 "1. civ fatalities" 2 "2. morally wrong" 3 "3. set precedent" 4 "4. damage reputation" 5 "5. insufficient mil adv" 6 "6. uncivilized" 7 "7. prefer nuclear", replace
label values q5b_t1_withnuke conv_pref_reasons_withnuke

replace q5b_t1_withnuke = 7 if (q3_t1==3 | q3_t1==4)
svy: tab   q5b_t1_withnuke , obs se ci stubwidth(100)

gen q5b_t5A_withnuke = q5b_t5A
label values q5b_t5A_withnuke conv_pref_reasons_withnuke
replace q5b_t5A_withnuke = 7 if (q3_t5A==3 | q3_t5A==4)
svy: tab q5b_t5A_withnuke , obs se ci stubwidth(100)

gen q5b_t5B_withnuke = q5b_t5B
label values q5b_t5B_withnuke conv_pref_reasons_withnuke
replace q5b_t5B_withnuke = 7 if (q3_t5B==3 | q3_t5B==4)
svy: tab q5b_t5B_withnuke , obs se ci stubwidth(100)


***Table 1

gen college_grad = educ
recode college_grad 1=0 2=0 3=0 4=0 5=1 6=1
gen republican = pid7_fixed
recode republican 1=0 2=0 3=0 4=0 5=1 6=1 7=1 8=0 .=0
gen political_interest = q8
label values political_interest q8
gen male = gender
recode male 1=1 2=0
** recode "prefer not to say" income to mean
gen income_no_missing = income
recode income_no_missing 15=8 

logit q3_treats1_5a_5b_dummy college_grad republican political_interest income_no_missing birthyr male [pweight=weight]

