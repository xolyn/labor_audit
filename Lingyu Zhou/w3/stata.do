drop if regexm(Q,"the employer terminated workers for joining a union or engaging in union activities?") & Country=="Haiti"
//5 obsers deleted; 
//1 item in Haiti that repeates meaning for "terminate workers for union activity"

drop if regexm(Q,"workers forced to work overtime in order to earn minimum wage") & Country=="Haiti"
//5 obsers deleted// 1 item only asked in Haiti

drop if regexm(Q,"Are eligible workers paid correctly for maternity leave") & Country=="Haiti"
//19 deleted, one extra item asking the same questions in Haiti

drop if regexm(Q,"workers freely form a union?") & Country=="Haiti"
//5 obs deleted, in Haiti, asked in different way

drop if regexm(Q,"Does the employer force workers to work overtime beyond legal limits by threatening dismissal or other action that would reduce their future income?") & Country=="Nicaragua"
//2 in 2015

drop if regexm(Q,"employer subject any workers under age 18 to the unconditional worst forms of child labour") & Country=="Haiti"
//35, 1 item asked in Haiti only in 2015 and early 2016

drop if regexm(Q,"in advance in order to obtain the authorization before the dismissal of a pregnant worker?") & Country=="Haiti" 
//28 deleted, 1 newly gender descriminate item asked since 2020

drop if regexm(Q,"Has the employer taken steps to enable workers who become disabled for whatever reason to retain their work") & Country=="Jordan"
//99, asked in Jordan only since late 2020

drop if regexm(Q,"Is disability a factor in the employer's decisions regarding termination or retirement?") & Country=="Jordan"
//99, asked in Jordan only since late 2020

drop if regexm(Q,"Is there harassment of workers on the basis of disability?") & Country=="Jordan"
//99, asked in Jordan only since late 2020

drop if regexm(Q,"Is disability a factor in decisions regarding opportunities for promotion or access to training?") & Country=="Jordan"
//99, asked in Jordan only since late 2020

drop if regexm(Q,"Have all accommodations required by national law been made for physically disabled persons?") & Country=="Jordan"
//99, asked in Jordan only since late 2020
 
drop if regexm(Q,"Can workers who owe debts for recruitment fees to a third party freely leave their jobs?") & Country=="Haiti"
//5 asked in Haiti only in early 2015

drop if regexm(Q,"Can workers who owe debts for recruitment fees to the employer freely leave their jobs?") & Country=="Haiti"
//5 asked in Haiti only in early 2015

drop if regexm(Q,"Has the employer ensured that the private employment agency does not use bonded labour?") & Country=="Haiti"
//5 asked in Haiti only in early 2015

drop if regexm(Q,"Does the employer provide non-cash benefits that make workers so indebted to the employer that they are unable to leave the job?") & Country=="Haiti"
//5 asked in Haiti only in early 2015

drop if regexm(Q,"Does the employer provide non-cash benefits that make workers so indebted to the employer that they are unable to leave the job?") & Country=="Nicaragua"
//2 asked in Nicaragua only in early 2015

drop if regexm(Q,"Can workers who owe debts for recruitment fees to a third party freely leave their jobs?") & Country=="Nicaragua"
//2 asked in Nicaragua only in early 2015

drop if regexm(Q,"Can workers who owe  debts for recruitment fees to the employer freely leave their jobs?") & Country=="Nicaragua"
//2 asked in Nicaragua only in early 2015

drop if regexm(Q,"If prison labour is used, do the prisoners receive similar treatment to non-prison workers working in the factory?") & Country=="Haiti"
//5 asked in Haiti only in early 2015

drop if regexm(Q,"If prison labour is used, do the prisoners receive the same treatment as non-prison workers working in the factory?") & Country=="Haiti"

drop if regexm(Q,"If prison labour is used, is the work carried out under the supervision and control of a public authority?") & Country=="Haiti"
//5 asked in Haiti only in early 2015

drop if regexm(Q,"In case of using prison labor, have inmates given their free consent?") & Country=="Nicaragua"
//2 asked in Nicaragua only in early 2015

drop if regexm(Q,"If prison labour is used, is the work carried out under the supervision and control of a public authority?") & Country=="Nicaragua"
//2 asked in Nicaragua only in early 2015

drop if regexm(Q,"Does the employer refuse to bargain collectively with union federations and confederations?") & Country=="Haiti"
//5 asked in Haiti only in early 2015

drop if regexm(Q,"Has the employer limited the issues that can be negotiated?") & Country=="Haiti"
//5 asked in Haiti only in early 2015

drop if regexm(Q,"by negotiating directly with individual workers") & Country=="Haiti"
//5 asked in Haiti only in early 2015

drop if regexm(Q,"Does the employer refuse to bargain collectively with union federations and confederations?") & Country=="Nicaragua"
//2 asked in Nicaragua only in early 2015

drop if regexm(Q,"Has the employer limited the issues that can be negotiated?") & Country=="Nicaragua"
//2 asked in Nicaragua only in early 2015

drop if regexm(Q,"by negotiating directly with individual workers") & Country=="Nicaragua"
//2 asked in Nicaragua only in early 2015

drop if regexm(Q,"Has the employer failed to reinstate all eligible workers after a strike?") & Country=="Nicaragua"
//2 asked in Nicaragua only in early 2015

drop if regexm(Q,"Has the employer failed to reinstate all eligible workers after a strike?") & Country=="Haiti"
//5 asked in Haiti only in early 2015

drop if regexm(Q,"Does the employer provide paid time off to workers who serve on the Union Labour Committee?") & Country=="Jordan"
//99 deleted, 1 item asked in Jordan only since late 2020

drop if regexm(Q,"Does the employer use blacklists to ensure that union members or union officials are not employed?") & Country=="Haiti"
//5 asked in Haiti only in early 2015

drop if regexm(Q,"Has the employer tried to promote the formation of a workers' organization to compete against existing") & Country=="Haiti"
//5 asked in Haiti only in early 2015

drop if regexm(Q,"Does the employer use blacklists to ensure that union members or union officials are not employed?") & Country=="Nicaragua"
//2 asked in Haiti only in early 2015

drop if regexm(Q,"Has the employer tried to promote the formation of a workers' organization to compete against existing") & Country=="Nicaragua"
//2 asked in Haiti only in early 2015

drop if regexm(Q,"Is senior management serving on the union executive committee?") & Country=="Vietnam"
//474 sked in Vietnam only in 2015, 2016

drop if regexm(Q,"Does the employer restrict workers' freedom to use their wages as they choose?") & Country=="Nicaragua"
//2 asked in Haiti only in early 2015

drop if regexm(Q,"Does the employer restrict workers' freedom to use their wages as they choose?") & Country=="Haiti"
//5 asked in Haiti only in early 2015

drop if regexm(Q,"Does the employer collect the required social insurance contribution to ONA from all workers?") & Country=="Haiti"
//5 asked in Haiti only in early 2015

drop if regexm(Q,"Does the employer collect the required contribution to  OFATMA from all workers?") & Country=="Haiti"

//5 asked in Haiti only in early 2015

drop if regexm(Q,"Does the employer comply with other wage payments?") & Country=="Nicaragua"
//2. only asked in 2015

drop if regexm(Q,"Does the employer deduct 6.25% from all workers' wages for contributions to social security?") & Country=="Nicaragua"
//2. only asked in 2015

drop if regexm(Q,"Does the employer pay the legally required employer contributions to social, health and unemployment insurance funds on time?") & Country=="Vietnam"
//474. only asked in 2015, 2016

drop if regexm(Q,"Are workers provided with transportation to and from the workplace?") & Country=="Jordan"
//275, not asked in 2018, 2019

drop if regexm(Q,"Does the employer pay workers 50% above the normal wage for overtime hours worked on legally mandated holidays?") & Country=="Haiti"
//81. 1 item asked since 2018

drop if regexm(Q,"Does the employer pay workers 50% above the normal wage for overtime hours worked on weekly rest days?") & Country=="Haiti"

//81. 1 item asked since 2018

drop if regexm(Q,"Does the employer pay workers correctly for all overtime hours worked on public holidays and weekly rest days?") & Country=="Indonesia"
//769. not asked in most of 2015 (only 50 in 2015)

drop if regexm(Q,"Does the employer pay workers correctly for paternity leave?") & Country=="Jordan"
//99. only asked since 2020

drop if regexm(Q,"Are workers paid at least every 15 days?") & Country=="Haiti"
//5 asked in Haiti only in early 2015

drop if regexm(Q,"Are wages paid at the workplace?") & Country=="Haiti"
//5 asked in Haiti only in early 2015

drop if regexm(Q,"Are wages paid on working days?") & Country=="Haiti"
//5 asked in Haiti only in early 2015

drop if regexm(Q,"Are wages paid at the workplace?") & Country=="Nicaragua"
//2 asked in Haiti only in early 2015

drop if regexm(Q,"Are wages paid on working days?") & Country=="Nicaragua"
//2 asked in Haiti only in early 2015

drop if regexm(Q,"Are wages paid directly to workers?") & Country=="Nicaragua"
//2 asked in Haiti only in early 2015

drop if regexm(Q,"Are workers' full wages paid in the manner required?") & Country=="Nicaragua"
//2 asked in Haiti only in early 2015

drop if regexm(Q,"Does the factory adequately compensate workers when sent home due to suspicion of COVID-19 symptoms?") & Country=="Haiti"
//19 delted. only on Covid sympton.


drop if regexm(Q,"Does the employer pay at least the applicable legal minimum wage for ordinary hours of work to temporary workers?") & Country=="Vietnam"
//1775, not asked in many months in 2021

drop if regexm(Q,"Do regular weekly working hours exceed 48 hours?") & Country=="Vietnam"
//247, only asked in 2015

drop if regexm(Q,"Do regular weekly working hours exceed 48 hours?") & Country=="Haiti"
//5 de

drop if regexm(Q,"Does the employer obtain authorization from the Department of Labour before working at night?") & Country=="Haiti"
//98, only asked in 2016-2019

drop if regexm(Q,"Does the employer provide the required breaks for pregnant women?") & Country=="Haiti"
//54, only asked since 2019

drop if regexm(Q,"he employer comply with notice") & Country=="Vietnam" & CP=="Overtime"
//123, only asked in 2015

drop if regexm(Q,"the employer provide workers at least 3 days off for paternity leave?") & Country=="Jordan"
//99 only asked since 2020

drop if regexm(Q,"Are workers able to take time off for annual leave and not forced to accept payment for it instead?") & Country=="Vietnam"
//247, only asked in 2015

drop if regexm(Q,"Has the employer communicated and disseminated laws and regulations on prevention and control of sexual harassment at the workplace to workers?") & Country=="Vietnam"
//27. only in 2021

drop if regexm(Q,"Did the employer resolve grievances and disputes in compliance with legal requirements?") & Country=="Nicaragua"
//only 2 items

drop if regexm(Q,"Have you found non-compliance with legal requirements for compensation, contracts, OSH, systems, and/or working time pertaining to non-production workers and/or sub-contracted workers?") & Country=="Vietnam"
//76; only asked in 2017

drop if regexm(Q,"Have you found non-compliance with legal requirements for compensation, contracts, OSH, and/or working time pertaining to non-production workers and/or sub-contracted workers?") & Country=="Vietnam"
//1252; only asked since 2017

drop if regexm(Q,"Have you found non-compliance with legal requirements for compensation, contracts, OSH and/or working time pertaining to non-production workers and/or sub-contracted workers?") & Country=="Haiti"
//117; only asked since 2017

drop if regexm(Q,"Do the migrant workers working outside of free zones have valid work permits?") & Country=="Haiti"
//5; only in 2015

drop if regexm(Q,"Does the employer comply with limits on the use of fixed term contracts?") & Country=="Haiti"
//5; only in 2015

drop if regexm(Q,"Does the employer comply with requirements concerning homeworkers?") & Country=="Haiti"
//5; only in 2015

drop if regexm(Q,"Does the employer comply with requirements concerning sub-contracted workers at the workplace?") & Country=="Haiti"
//5; only in 2015

drop if regexm(Q,"Have you found non-compliance with legal requirements for compensation, contracts, OSH, systems, and/or working time pertaining to non-production workers and/or sub-contracted workers?") & Country=="Jordan"
//343, only asked since 2017

drop if regexm(Q,"Have you found non-compliance with legal requirements for compensation, contracts, OSH, and/or working time pertaining to non-production workers who are directly employed by the factory?") & Country=="Indonesia"
//480, only asked since 2017

drop if regexm(Q,"Is the trial period for unlimited duration contracts limited to 30 days?") & Country=="Nicaragua"
//2; in 2015 only

drop if regexm(Q,"Does the recruitment process for migrant workers comply with legal requirements?") & Country=="Nicaragua"
//2; in 2015 only

drop if regexm(Q,"Does the employer comply with requirements concerning Sub-contracted workers at the workplace?") & Country=="Nicaragua"
//2; in 2015 only

drop if regexm(Q,"Does the employer comply with legal requirements limiting the number of consecutive fixed-term contracts?") & Country=="Nicaragua"
//2; in 2015 only

drop if regexm(Q,"Have you found non-compliance with legal requirements for compensation, contracts, OSH and/or working time pertaining to non-production workers and/or sub-contracted workers?") & Country=="Nicaragua"
//only since 2017

drop if regexm(Q,"Has the employer complied with any orders to reinstate or compensate workers who were found to be unjustly terminated?") & Country=="Vietnam"
//247; only in 2015/16

drop if regexm(Q,"Does the employer pay judicially ordered damages for wrongful termination?") & Country=="Haiti"
//5; only in 2015

drop if regexm(Q,"Does the employer comply with legal requirements before reducing the size of the workforce due to changes in operations?") & Country=="Haiti"
//5; only in 2015

drop if regexm(Q,"Do workers have an opportunity to defend themselves before they are terminated based on their conduct or performance?") & Country=="Haiti"
//5; only in 2015

drop if regexm(Q,"Does the employer notify the labour ministry when suspending operations due to lack of materials, force majeure, or accident resulting in an immediate work stoppage?") & Country=="Haiti"
//5; only in 2015

drop if regexm(Q,"Do workers who resign or are terminated receive the accumulated thirteenth month payment?") & Country=="Nicaragua"
//2; only in 2015

drop if regexm(Q,"Does the employer compensate workers for unused paid annual leave when they resign or are terminated?") & Country=="Nicaragua"
//2; only in 2015

drop if regexm(Q,"Does the employer comply with legal requirements when reducing the work force due to changes in operations?") & Country=="Nicaragua"
//2; only in 2015

drop if regexm(Q,"the employer informed and prepare") & Country=="Haiti" & CP=="Emergency Preparedness"
//60; only since 2019

drop if regexm(Q,"Does the employer require workers to comply with OSH requirements?") & Country=="Haiti" 
//66
drop if regexm(Q,"Does the factory have a written OSH policy?") & Country=="Haiti" 
//55

drop if regexm(Q,"Is there an adequate OSH Policy that is signed by top management?") & Country=="Haiti" 
//46

drop if regexm(Q,"Does the employer has a written emergency preparedness procedure?") & Country=="Indonesia" 
//64 in 2015

drop if regexm(Q,"Does OSH policies and procedures are effectively communicated and implemented?") & Country=="Indonesia" 
//64 in 2015

drop if regexm(Q,"Does the factory investigate, monitor and measure OSH issues to identify root causes and make necessary adjustments to prevent re-occurrence?") & Country=="Indonesia" 
//64 in 2015 onl

drop if regexm(Q,"Does the employer record work-related illnesses?") & Country=="Nicaragua" 
//2 in 2015

drop if regexm(Q,"Does the employer develop the Document on Working Conditions and Environment?") & Country=="Vietnam" 
//247 in 2015 and early 2016

drop if regexm(Q,"Do female workers receive periodical gynecology health checks every 6 months?") & Country=="Vietnam" 
//721 in 2015-2017

drop if regexm(Q,"Do workers who are not exposed to work-related hazards receive annual medical checks?") & Country=="Vietnam" 
//555 in 2015/6

drop if regexm(Q,"Does the employer comply with requirements regarding workers' mental health?") & Country=="Jordan" 
//90; since 2020

drop if regexm(Q,"Do workers who are exposed to work-related hazards receive annual free health checks?") & Country=="Nicaragua" 
//2 

drop if regexm(Q,"Does the accommodation have adequate cooking and storage facilities?") & Country=="Vietnam" 
//816;until 2017

drop if regexm(Q,"Does the accommodation have lighting of at least 50 lux?") & Country=="Vietnam" 
//816;until 2017

drop if regexm(Q,"Does the accommodation provide each worker with at least 75 liters of safe water per day?") & Country=="Vietnam" 
//550;until 2017

drop if regexm(Q,"Has the accommodation been built with noise-proof materials?") & Country=="Vietnam" 
//550;until 2017

drop if regexm(Q,"Does the employer provide accommodation for workers in the Free Trade Zone?") & Country=="Nicaragua" 
//2

drop if regexm(Q,"Are materials, tools, switches, and controls within easy reach of workers?") & Country=="Vietnam"
//474 in 2015/16
 
drop if regexm(Q,"Are materials, tools, switches, and controls within easy reach of workers?") & Country=="Haiti"
//5

drop if regexm(Q,"Are standing workers properly accommodated?") & Country=="Haiti"
//5

drop if regexm(Q,"Are there sufficient measures in place to avoid heavy lifting by workers?") & Country=="Haiti"
//5

drop if regexm(Q,"Are workers effectively trained to use machines and equipment safely?") & Country=="Haiti"
//5

drop if regexm(Q,"body temperature upon entry and keep a registry for all persons who present temperature of 38C and above") & Country=="Haiti"
//19 on Covid

drop if regexm(Q,"the factory provide training and/or awareness of the measures adopted to prevent the risks of exposure to COVID-19 to all staff") & Country=="Haiti"
//19 on Covid

drop if regexm(Q,"the factory take appropriate measures to respect the physical and social distancing required?") & Country=="Haiti"
//19 on Covid

drop if regexm(Q,"Are materials, tools, switches, and controls within easy reach of workers?") & Country=="Nicaragua"
//2

drop if regexm(Q,"Are standing workers properly accommodated?") & Country=="Nicaragua"
//2

drop if regexm(Q,"Are there sufficient measures in place to avoid heavy lifting by workers?") & Country=="Nicaragua"
//2

drop if regexm(Q,"Are workers effectively trained to use machines and equipment safely?") & Country=="Nicaragua"
//2

drop if regexm(Q,"Are workers punished if they remove themselves from work situations that they believe present an imminent and serious danger to life or health?") & Country=="Nicaragua"
//2

drop if regexm(Q,"Has the employer taken legally required measures to protect workers from falls from heights?") & Country=="Nicaragua"
//53

drop if regexm(Q,"Is the temperature in the workplace acceptable?") & Country=="Haiti"
//5

drop if regexm(Q,"Is the temperature in the workplace acceptable?") & Country=="Nicaragua"
//2

