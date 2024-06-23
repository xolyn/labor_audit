
*********************************************************************************
******run these stata codes before runing the Python codes for this CP

**************clean OSH management systems

replace QCleaned="does the factory have a valid business license?" if QCleaned=="has the factory submitted a notice to the chief inspector before starting operations?" & Country=="Bangladesh"
//one item asked in different ways in Bangladesh

replace CPCleaned="osh management systems" if QCleaned=="mechanisms/committee for cooperation on osh"


replace QCleaned=Q if Q=="does the workplace have a functioning hiv/aids committee?"
//2503; rectify coding error in Cambodia

replace QCleaned="mechanisms/committee for cooperation on osh" if Q=="has the employer developed mechanisms to ensure cooperation between workers and management on osh matters?"
//2503 changed, Cambodia

replace disclosed=1 if QCleaned=="mechanisms/committee for cooperation on osh" 
//3087 changed; right contribute distribution


replace QCleaned="does the factory have a written osh policy?" if QCleaned=="is there an adequate osh policy that is signed by top management?" & Country=="Cambodia"

replace QCleaned="has the employer performed an assessment of general occupational safety and health issues in the factory?" if Q=="does the employer conduct risk assessment?"

replace QCleaned="has the employer performed an assessment of general occupational safety and health issues in the factory?" if Q=="does the employer conduct regular hazards and risk assessment?"

replace QCleaned="does the factory have a written osh policy?" if regexm(Q,"does the employer have written plans for osh programs?|does the employer have a written policy and plans for osh programs?")

 duplicates tag QuestID if Country=="Jordan" & year==2015 & QCleaned=="does the factory have a written osh policy?",gen(JDdup)

 drop if index==470616
  drop if index==470687
 //two identical questions in Jordan in 2015
 
drop JDdup

replace QCleaned="does the employer investigate work-related accidents and indicate the technical recommendations necessary to prevent them?" if regexm(Q,"does the employer investigate work-related accidents and indicate the technical recommendations necessary to prevent them?|does the employer record and investigate work-related accidents and illnesses, and indicate the technical recommendations necessary to prevent them?")

replace QCleaned="has legal construction permits/structural safety certificate" if Q=="does the employer have legally required construction/building permits?"

replace QCleaned="has legal construction permits/structural safety certificate" if Q=="does the employer ensure the building is safe and maintain legally required permits

replace QCleaned="does the employer record work-related accidents?" if Q=="does the employer record work-related accidents and diseases and submit the record to ofatma on a monthly basis?"
//161 changes, asked differently in Haiti


replace QCleaned="has legal construction permits/structural safety certificate"
if QCleaned=="does the factory have a valid business license?" & Country=="Jordan"



*********************************************************************************
*****run these  in full to clean some CPs

*****************************************************************************
*****************overtime wages CP 

replace CPCleaned="overtime wages" if CP=="premium pay"

replace QCleaned="Correct pay for work on public holidays" if regexm(QCleaned,"does the employer pay all workers correctly for work on public holidays?|does the employer pay workers 100% more than their normal wage for all overtime hours worked on national holidays?|does the employer pay workers 150% of their normal wage for overtime worked on weekly rest days and public holidays?|does the employer pay all workers correctly for work on public holidays?")

replace QCleaned="Correct pay for work on public holidays" if Q=="does the employer pay workers 50% above the normal wage when workers work regular hours on legally mandated holidays?"
//161 changes, one item in Haiti grouped under "premium pay"


replace QCleaned="Correct pay for overtime at night" if regexm(Q,"does the employer pay workers correctly for all overtime hours worked at night?|does the employer pay workers double their normal wage for all overtime hours worked at night?")

replace QCleaned="Correct pay for overtime at weekly rest days" if regexm(Q,"does the employer pay workers correctly for all overtime hours worked on weekly rest days?|does the employer pay workers double their normal wage for all hours worked on weekly rest days?|does the employer pay workers 100% more than their normal wage for all overtime hours worked on weekly rest days?")

replace QCleaned="Correct pay for overtime at weekly rest days" if Q=="does the employer pay workers 50% above the normal wage when workers work regular hours on weekly rest days?"
//161 changed. combine 1 in Haiti with others

replace QCleaned="Correct pay for overtime at weekly rest days" if regexm(Q,"does the employer pay workers correctly when they take compensatory days off after working on weekly rest days?")
//584 changed; Combine one item in Bangladesh with those in other countries



replace QCleaned="Correct pay for overtime at weekly rest days" if regexm(Q,"does the employer provide meals and drinks of at least 1,400 calories to workers working overtime for 3 hours or more?|does the employer provide meals and drinks of at least 1,400 calories to workers working overtime for 4 hours or more?")
//834 changed, combine one item in Indonesia with those in Cambodia and Jordan


replace QCleaned="Correct pay for work on public holidays" if QCleaned=="does the employer pay piece rate workers correctly for all overtime hours worked on regular work days?" 
//584 changed, combine one item in Balgdesh with those in other countries


drop if regexm(Q,"does the employer pay workers 100% above the normal wage for all overtime hours worked at night?|does the employer pay workers 100% above the normal wage for overtime hours worked on legally mandated holidays?|does the employer pay workers 100% above the normal wage for overtime hours worked on weekly rest days?|does the employer comply with national law regarding payment for regular working hours worked at night?") & Country=="Haiti"
//320 deleted; 4 items only asked in 2015-2018 in Haiti


drop if Q=="does the factory incorporate all required allowances and additional payments into the calculation of wage-based benefits (e.g. social insurance payments, overtime, paid leave, etc.)?"
//1555 deleted; 1 item only in Vietnam and not asked in 2015


drop if Q=="does the employer pay at least 5% higher than the normal applicable wage level for workers who perform hazardous and dangerous work?"
//1775 deleted; 1 unique item only asked in Vietnam


drop if Q=="does the employer pay all workers the correct meal allowance or give them a reasonable free meal when they work overtime?" & Country=="Cambodia"
//2503 deleted; one item only asked in Cambodia


drop if QCleaned=="Correct pay for overtime at night" 
//4157 deleted; one item only in Cambodia, Nicaragua, and Vietnam

drop if QCleaned=="does the employer pay workers correctly for regular working hours worked at night?"
//only asked in Cambodia and Vietnam


********************************************************************************
**********clean Prison Labour CP

replace QCleaned="prison labor consent and same treatment" if regexm(Q,"if prison labour is used, do the prisoners receive similar treatment to non-prison workers working in the factory?|if prison labour is used, have the prisoners freely consented to do the work; do they receive similar treatment to non-prison workers; and is the work carried out under the supervision and control of a public authority?|if prison labour is used, have the prisoners freely consented to do the work?")


drop if Q=="if prison labour is used, is the work carried out under the supervision and control of a public authority?" & Country=="Indonesia"
//833 deleted; similar item only in Indonesia 


drop if Q=="if prison labour is used, have the prisoners freely consented to do the work?" & Country=="Indonesia"
//833 deleted; similar item only in Indonesia 













********I have run these on 17 June.
**********no need to run them on 21 June


*****************************************************************************
*****************************************************************************
****clean Strikes CP
replace QCleaned="Employer punished workers for striking" if (regexm(Q,"punished any workers for participating in a strike"))

replace QCleaned="Employer prevented workers from striking" if (regexm(Q,"prevent any workers from participating in a strike|prevent workers from participating in a strike"))

replace QCleaned="Employer called police to break up strike" if (regexm(Q,"break up a peaceful strike"))

replace QCleaned="Employer replaced striking workers" if (regexm(Q,"hired new workers to replace striking workers|failed to reinstate all eligible workers"))


********************clean Collective Bargaining CP
replace QCleaned="Employer refused to bargain in good faith" if regexm(Q,"refuse to bargain in good faith")


replace QCleaned="Employer consult with workers/union" if regexm(Q,"consult with")

replace QCleaned="CBA at least as favorable as law" if (regexm(Q,"at least as favourable|at least as favorable|more favorable terms"))

replace QCleaned="Employer inform workers CBA" if (regexm(Q,"inform workers about|publically available to all workers|workers informed about|provide workers access to the collective bargaining agreement|prevent workers from accessing"))

replace QCleaned=Q if Q=="Does the employer properly inform workers about wage payments and deductions?"

replace QCleaned=Q if Q=="Does the employer inform workers about overtime at least 2 hours in advance?"


**********clean Union Operations CP

 replace QCleaned="Workers are free to meet" if regexm(Q,"workers free to meet without management present")

  replace QCleaned="Union has access to workers" if regexm(Q,"union representatives have access to workers|union representatives have access to the workers")
  
  
 replace QCleaned="Employer requires workers to join union" if regexm(Q,"employer require workers to join a union") 

 
  
 replace QCleaned="Employer deducts union dues as requested" if (regexm(Q,"union dues")) 
 
 
 

replace QCleaned="Employer punishes workers for union activity" if (regexm(Q,"employer punish workers for joining a union or engaging in union activities")) 


replace QCleaned="Employer fires workers for union activity" if (regexm(Q,"employer terminated any worker|employer terminated workers")) 

replace QCleaned="Employer disuades workers from unions" if (regexm(Q,"employer provide incentives to workers to keep them from joining a union or engaging in union activities")) 
    //4674 entries

	
replace QCleaned="Employer threatens union members" if (regexm(Q,"employer threaten, intimidate, or harass workers who join a union or engage in union activities")) 

	
replace QCleaned="Employer fires union official" if (regexm(Q,"employer terminated a union official")) 
replace QCleaned="Employer fires union official" if (regexm(Q,"punished or changed the conditions of service of a union official")) //Bangladesh only
replace QCleaned="Employer fires union official" if (regexm(Q,"terminated the contract, transferred to a new work, or dismissed a union official")) //27 entries in Vietnam only


replace QCleaned="Union membership/activity influences hiring" if (regexm(Q," job applicant's union membership or union activities a factor in hiring decisions|the employer consider a job applicant's union membership or union activities when hiring")) 
//Is a job applicant's union membership or union activities a factor in hiring decisions?
//Does the employer consider a job applicant's union membership or union activities when hiring?


replace QCleaned="Union membership/activity influences hiring" if regexm(Q,"a job applicant's union membership or union activities a factor during hiring decisions?") 

replace QCleaned="Employer provides facility for union" if (regexm(Q," employer provide the trade union with the necessary facilities|employer provide unions with access to bulletin boards|employer provide the Collective Bargaining Agent with an office|employer provide a place to post the names|employer provide the trade union with the necessary facilities and time")) 

replace QCleaned="Employer provides facility for union" if (regexm(Q," employer allow workers to carry out trade union activities")) 



replace QCleaned="Interfere union" if Q=="Has the employer tried to interfere with, manipulate, or control the union(s)?" & Country=="Vietnam"




replace CPcleaned="Freedom to Associate" if QCleaned=="Freedom to form/join union"

replace CPcleaned="Freedom to Associate" if QCleaned=="Employer requires workers to join union"


drop if regexm(Q, "employer refuse to bargain collectively with union federations")
//deleted 7 items only asked in Haiti and Nicaragua



drop if regexm(Q, "by negotiating directly with individual workers")
//deleted 7 items "employer try to undermine the union(s) by negotiating directly with individual workers", only asked in Haiti and Nicaragua

drop if regexm(Q, "employer limited the issues that can be negotiated")
//deleted 7 items only asked in Haiti and Nicaragua

drop if regexm(Q, "grassroots level union in the factory involved in the bargaining process at the enterprise level")
// deleted 1802 items, only asked in Vietnam


drop if regexm(Q, "collective agreement in force been approved by more than 50% of workers")
// deleted 1802 items, only asked in Vietnam


drop if regexm(Q, "the employer interfere with workers or unions when they draw up their constitutions and rules, hold elections, or organize their activities, administration or finances?") & Country=="Cambodia"
// 1153 only in Cambodia





 drop if (regexm(Q,"employer use blacklists"))
   //7 entries, only asked in Haiti and Nicaragua
   
 
 drop if (regexm(Q,"employer tried to promote the formation"))
   //Has the employer tried to promote the formation of a workers' organization to compete against existing union(s)?
   //7 entries, only asked in Haiti and Nicaragua

 drop if (regexm(Q,"employer not renewed a worker's employment contract"))
   //7 entries, only asked in Haiti and Nicaragua
   //Has the employer not renewed a worker's employment contract due to the worker's union membership or activities?

   drop if (regexm(Q,"employer provide paid time off to workers"))
   //Jordan only, 99 entries, overlapp with another similar item on union facility
   

   drop if (regexm(Q,"employer provide trade union officers paid time off to participate in training"))
   //Bangladesh only 584; overlap with 

   
     drop if (regexm(Q,"freely form and join federations and confederations of their choice"))
   // Can the union(s) freely form and join federations and confederations of their choice
   //3552 entries; not in Bangladesh and Vietnam

 drop if (regexm(Q,"If there is more than one union")) 
   //3569 entries only 
   //If there is more than one union, does the employer treat them as stipulated by national law?
   
   drop if regexm(Q,"Is the employer involved in union")
   
   
   
   drop if QCleaned=="Disciplinary measures"
   //161 only in Haiti
   
      drop if QCleaned=="Minimum wages for non-permanent workers"

	 //851 only in Indonesia
	 
	 
	 replace disclosed=1  if QCleaned=="Implement CBA"
	 
	 
	 replace QCleaned=Q if Q=="Does the employer properly inform workers about wage payments and deductions? [differentiation question]"
	 
	 
	 drop if QCleaned=="Employment contract for all"
	 //973 only in Indonesia and Nicaragua
	 
	 
	 replace disclosed=0 if QCleaned=="Does the factory have a functioning Participation Committee?"
	 
	 replace disclosed=1 if regexm(QCleaned,"Mechanisms/committee for cooperation")
	 
	 replace QCleaned="Mechanisms/committee for cooperation on OSH" if Q=="Does the factory have a functioning Safety Committee?" 
   