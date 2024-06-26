########################################################################
#		Apply DiD methods to labor audits data
#		Yuequan
#		240623
########################################################################



# preparation
	## install R packages
	# install.packages("")

	## load R packages
	lib <- c("haven","tidyverse","plm","did","modelsummary")
	lapply(lib,require,character.only=TRUE)

	## clear up space
	rm(list=ls())


# sink("DID_FE_240623.txt")
# read in clean data
	dt <- read_dta("June25audit variables for analysis.dta")

	dt2 <- dt %>%
		mutate(
			fid = fct(as.character(FactoryAssessedID)),
			AssesmentDate = dmy(AssesmentDate),
			ym = floor_date(AssesmentDate, unit="month"),
			T1 = if_else(
				Country%in%c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua") & AssesmentDate>=ymd(20170701),
				"1","0"
				) %>% fct(levels=c("0","1")), # uniform treatment
			T2 = case_when(
				Country=="Vietnam" 		& AssesmentDate>=ymd(20160601)	~ "1",
				Country=="Jordan" 		& AssesmentDate>=ymd(20161101)	~ "1",
				Country=="Indonesia" 	& AssesmentDate>=ymd(20170101)	~ "1",
				Country=="Haiti" 			& AssesmentDate>=ymd(20170701)	~ "1",
				Country=="Nicaragua" 	& AssesmentDate>=ymd(20180101)	~ "1",
				TRUE 																									~ "0"
				) %>% fct(levels=c("0","1")) # staggered treatment
			)
	# with(dt2, table(T1_ym, after17jul, exclude=NULL)) # "T1_ym"=="after17jul" except for 5 missing
	# str(dt2$T2)

	dt3 <- dt %>%
		mutate(
			fid = as.integer(FactoryAssessedID),
			AssesmentDate = dmy(AssesmentDate),
			y = format(AssesmentDate, "%Y") %>% as.integer,
			ym = format(AssesmentDate, "%Y%m") %>% as.integer,
			T1 = if_else(
				Country%in%c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua"),
				2017,0
				), # uniform treatment
			T1_ym = if_else(
				Country%in%c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua"),
				201707,0
				), # uniform treatment
			T2 = case_when(
				Country=="Vietnam" 			~ 2016, # AssesmentDate>=ymd(20160601)
				Country=="Jordan" 			~ 2016, # AssesmentDate>=ymd(20161101)
				Country=="Indonesia" 		~ 2017, # AssesmentDate>=ymd(20170101)
				Country=="Haiti" 				~ 2017, # AssesmentDate>=ymd(20170701)
				Country=="Nicaragua" 		~ 2018, # AssesmentDate>=ymd(20180101)
				TRUE 										~ 0
				), # staggered treatment
			T2_ym = case_when(
				Country=="Vietnam" 			~ 201606, # AssesmentDate>=ymd(20160601)
				Country=="Jordan" 			~ 201611, # AssesmentDate>=ymd(20161101)
				Country=="Indonesia" 		~ 201701, # AssesmentDate>=ymd(20170101)
				Country=="Haiti" 				~ 201707, # AssesmentDate>=ymd(20170701)
				Country=="Nicaragua" 		~ 201801, # AssesmentDate>=ymd(20180101)
				TRUE 										~ 0
				) # staggered treatment
			)
	# with(dt3, table(y, T1, exclude=NULL))
	# str(dt3$ym)



# DiD by Callaway and Sant'Anna (2021)
## reported
	m_r <- att_gt(
		yname = "reportedcompl",
		idname = "fid",
		tname = "ym",
		gname = "T2_ym",
		xformla = NULL,
		data = dt3,
		panel = TRUE,
		allow_unbalanced_panel = TRUE,
		control_group = "nevertreated", #"notyettreated"
		clustervars = NULL,
		est_method = "dr"
		)

	summary(m_r)
	ggdid(m_r) + theme(axis.text.y=element_text(size=10), axis.text.x=element_text(angle=90, vjust=0.5, hjust=1, size=10))

	agg.simple <- aggte(m_r, type="simple", na.rm=TRUE)
	summary(agg.simple)

	agg.gs <- aggte(m_r, type="group", na.rm=TRUE)
	summary(agg.gs)
	ggdid(agg.gs) + scale_y_discrete(labels=c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua"))
	ggsave("DID_GT_group.png")

	agg.es <- aggte(m_r, type="dynamic", na.rm=TRUE)
	summary(agg.es)
	ggdid(agg.es) + scale_x_continuous(expand=c(0.01,0), n.breaks=10)
	ggsave("DID_GT_dynamic.png")

	agg.cs <- aggte(m_r, type="calendar", na.rm=TRUE)
	summary(agg.cs)
	ggdid(agg.cs) + scale_x_continuous(expand=c(0.01,0), guide=guide_axis(angle=90), n.breaks=10)
	ggsave("DID_GT_calendar.png")



# DiD by two-way fixed effects panel models
## reported
	m_r_1 <- plm(reportedcompl ~ T1 + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_r_1, vcov=function(x) vcovHC(x, method="ar"))

	m_r_2 <- plm(reportedcompl ~ T2 + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_r_2, vcov=function(x) vcovHC(x, method="ar"))


## similar
	m_s_1 <- plm(similarCPcompl ~ T1 + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_s_1, vcov=function(x) vcovHC(x, method="ar"))

	m_s_2 <- plm(similarCPcompl ~ T2 + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_s_2, vcov=function(x) vcovHC(x, method="ar"))


## distant
	m_d_1 <- plm(distantCPcompl ~ T1 + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_d_1, vcov=function(x) vcovHC(x, method="ar"))

	m_d_2 <- plm(distantCPcompl ~ T2 + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_d_2, vcov=function(x) vcovHC(x, method="ar"))
# sink()

## coef plots
	m <- list(
		"reported/uniform"=m_r_1, "similar/uniform"=m_s_1, "distant/uniform"=m_d_1, 
		"reported/staggered"=m_r_2, "similar/staggered"=m_s_2, "distant/staggered"=m_d_2
		)
	modelplot(
		models=m,
		coef_map=c(
			"T21","T11"
			)
		) + geom_vline(xintercept=0) + scale_colour_brewer(palette="Set1") + theme_bw() + labs(title="DiD by FE panel models")
	# ggsave("DID_FE_240623.png")