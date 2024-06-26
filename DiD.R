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
			AssesmentDate = dmy(AssesmentDate),
			fid = fct(as.character(FactoryAssessedID)),
			ym = floor_date(AssesmentDate, unit="month"),
			T1 = if_else(
				Country%in%c("Indonesia","Vietnam","Jordan","Nicaragua","Haiti") & AssesmentDate>=ymd(20170701),
				"1","0"
				) %>% fct(levels=c("0","1")), # uniform treatment
			T2 = case_when(
				is.na(AssesmentDate)																	~ NA_character_, # to be removed later
				Country=="Indonesia" 	& AssesmentDate>=ymd(20170101)	~ "1",
				Country=="Vietnam" 		& AssesmentDate>=ymd(20160601)	~ "1",
				Country=="Jordan" 		& AssesmentDate>=ymd(20161101)	~ "1",
				Country=="Nicaragua" 	& AssesmentDate>=ymd(20180101)	~ "1",
				Country=="Haiti" 			& AssesmentDate>=ymd(20170701)	~ "1",
				TRUE 																									~ "0"
				) %>% fct(levels=c("0","1")) # staggered treatment
			)
	# with(dt2, table(T1_ym, after17jul, exclude=NULL)) # "T1_ym"=="after17jul" except for 5 missing
	# str(dt2$T2)



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



# DiD by Callaway and Sant'Anna (2021)
