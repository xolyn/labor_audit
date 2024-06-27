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


# sink("DiD_GT_240627.txt")
# read in clean data
	dt <- read_dta("June26audit variables for analysis.dta")

	dt2 <- dt %>%
		mutate(
			fid = fct(as.character(FactoryAssessedID)),
			AssesmentDate = dmy(AssesmentDate),
			ym = floor_date(AssesmentDate, unit="month"),
			b1i = if_else(buyer1FTindex<0, NA, buyer1FTindex),
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
	# names(dt2)
	# with(dt2, table(buyer1FTindex, exclude=NULL))

	dt3 <- dt %>%
		mutate(
			fid = as.integer(FactoryAssessedID),
			AssesmentDate = dmy(AssesmentDate),	y = year(AssesmentDate), m = month(AssesmentDate),
			ym = format(AssesmentDate, "%Y%m") %>% as.integer,
			ym2 = str_c(y, ceiling(m/2)) %>% as.integer,
			yq = str_c(y, quarter(AssesmentDate)) %>% as.integer,
			T1 = if_else(
				Country%in%c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua"),
				2017,0
				),
			T1_ym = if_else(
				Country%in%c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua"),
				201707,0
				),
			T1_ym2 = if_else(
				Country%in%c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua"),
				20174,0
				),
			T1_yq = if_else(
				Country%in%c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua"),
				20173,0
				),
			T2 = case_when(
				Country=="Vietnam" 			~ 2016, # AssesmentDate>=ymd(20160601)
				Country=="Jordan" 			~ 2016, # AssesmentDate>=ymd(20161101)
				Country=="Indonesia" 		~ 2017, # AssesmentDate>=ymd(20170101)
				Country=="Haiti" 				~ 2017, # AssesmentDate>=ymd(20170701)
				Country=="Nicaragua" 		~ 2018, # AssesmentDate>=ymd(20180101)
				TRUE 										~ 0
				),
			T2_ym = case_when(
				Country=="Vietnam" 			~ 201606, # AssesmentDate>=ymd(20160601)
				Country=="Jordan" 			~ 201611, # AssesmentDate>=ymd(20161101)
				Country=="Indonesia" 		~ 201701, # AssesmentDate>=ymd(20170101)
				Country=="Haiti" 				~ 201707, # AssesmentDate>=ymd(20170701)
				Country=="Nicaragua" 		~ 201801, # AssesmentDate>=ymd(20180101)
				TRUE 										~ 0
				),
			T2_ym2 = case_when(
				Country=="Vietnam" 			~ 20163, # AssesmentDate>=ymd(20160601)
				Country=="Jordan" 			~ 20166, # AssesmentDate>=ymd(20161101)
				Country=="Indonesia" 		~ 20171, # AssesmentDate>=ymd(20170101)
				Country=="Haiti" 				~ 20174, # AssesmentDate>=ymd(20170701)
				Country=="Nicaragua" 		~ 20181, # AssesmentDate>=ymd(20180101)
				TRUE 										~ 0
				),
			T2_yq = case_when(
				Country=="Vietnam" 			~ 20162, # AssesmentDate>=ymd(20160601)
				Country=="Jordan" 			~ 20164, # AssesmentDate>=ymd(20161101)
				Country=="Indonesia" 		~ 20171, # AssesmentDate>=ymd(20170101)
				Country=="Haiti" 				~ 20173, # AssesmentDate>=ymd(20170701)
				Country=="Nicaragua" 		~ 20181, # AssesmentDate>=ymd(20180101)
				TRUE 										~ 0
				)
			)
	# str(dt3$ym)
	# with(dt3, table(ym2, exclude=NULL))
	# subset(dt3, subset=!is.na(Country)) %>% summarize(n=n(), .by=c(Country, ym)) %>% pivot_wider(names_from=Country, values_from=n) %>% print(n=Inf)



# DiD by Callaway and Sant'Anna (2021)
## group-time ATEs for each of three outcomes
	dvs <- c("reportedcompl","similarCPcompl","distantCPcompl")

	m_gt <- lapply(dvs, function(dv) {
		att_gt(
			yname = dv,
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
		})


## reported
	m_r_t <- aggte(m_gt[[1]], type="dynamic", na.rm=TRUE)
	summary(m_r_t)
	ggdid(m_r_t) + scale_x_continuous(expand=c(0.01,0), n.breaks=20) + labs(title="Reported", x="Exposure Periods")
	# ggsave("DiD_reported_dynamic.png")

	m_r_g <- aggte(m_gt[[1]], type="group", na.rm=TRUE)
	summary(m_r_g)
	ggdid(m_r_g) + scale_y_discrete(labels=c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua")) + labs(title="Reported", y="Countries")
	# ggsave("DiD_reported_group.png")


## similar
	m_s_t <- aggte(m_gt[[2]], type="dynamic", na.rm=TRUE)
	summary(m_s_t)
	ggdid(m_s_t) + scale_x_continuous(expand=c(0.01,0), n.breaks=20) + labs(title="Similar", x="Exposure Periods")
	# ggsave("DiD_similar_dynamic.png")

	m_s_g <- aggte(m_gt[[2]], type="group", na.rm=TRUE)
	summary(m_s_g)
	ggdid(m_s_g) + scale_y_discrete(labels=c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua")) + labs(title="Similar", y="Countries")
	# ggsave("DiD_similar_group.png")


## distant
	m_d_t <- aggte(m_gt[[3]], type="dynamic", na.rm=TRUE)
	summary(m_d_t)
	ggdid(m_d_t) + scale_x_continuous(expand=c(0.01,0), n.breaks=20) + labs(title="Distant", x="Exposure Periods")
	# ggsave("DiD_distant_dynamic.png")

	m_d_g <- aggte(m_gt[[3]], type="group", na.rm=TRUE)
	summary(m_d_g)
	ggdid(m_d_g) + scale_y_discrete(labels=c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua")) + labs(title="Distant", y="Countries")
	# ggsave("DiD_distant_group.png")
# sink()

## TESTING CODES
	m_r <- att_gt(
		yname = "reportedcompl",
		idname = "fid",
		tname = "ym2",
		gname = "T2_ym2",
		xformla = ~union+mng,
		data = dt3,
		panel = TRUE,
		allow_unbalanced_panel = TRUE,
		control_group = "nevertreated", #"notyettreated"
		clustervars = NULL,
		est_method = "ipw"
		)

	summary(m_r)
	ggdid(m_r) + theme(axis.text.y=element_text(size=10), axis.text.x=element_text(angle=90, vjust=0.5, hjust=1, size=10))

	agg.es <- aggte(m_r, type="dynamic", na.rm=TRUE)
	summary(agg.es)
	ggdid(agg.es) + scale_x_continuous(expand=c(0.01,0), n.breaks=10)
	# ggsave("DID_GT_dynamic.png")

	# agg.cs <- aggte(m_r, type="calendar", na.rm=TRUE)
	# summary(agg.cs)
	# ggdid(agg.cs) + scale_x_continuous(expand=c(0.01,0), guide=guide_axis(angle=90), n.breaks=10)
	# ggsave("DID_GT_calendar.png")

	agg.gs <- aggte(m_r, type="group", na.rm=TRUE)
	summary(agg.gs)
	ggdid(agg.gs) + scale_y_discrete(labels=c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua"))
	# ggsave("DID_GT_group.png")

	# agg.simple <- aggte(m_r, type="simple", na.rm=TRUE)
	# summary(agg.simple)



# DiD by two-way fixed effects panel models
## reported
	m_r_1 <- plm(reportedcompl ~ T1 + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_r_1, vcov=function(x) vcovHC(x, method="ar"))

	m_r_2 <- plm(reportedcompl ~ T2 + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_r_2, vcov=function(x) vcovHC(x, method="ar"))

	m_r_2u <- plm(reportedcompl ~ T2*union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_r_2u, vcov=function(x) vcovHC(x, method="ar"))
	m_r_2m <- plm(reportedcompl ~ T2*mng + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_r_2m, vcov=function(x) vcovHC(x, method="ar"))
	m_r_2b <- plm(reportedcompl ~ T2*b1i + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_r_2b, vcov=function(x) vcovHC(x, method="ar"))


## similar
	m_s_1 <- plm(similarCPcompl ~ T1 + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_s_1, vcov=function(x) vcovHC(x, method="ar"))

	m_s_2 <- plm(similarCPcompl ~ T2 + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_s_2, vcov=function(x) vcovHC(x, method="ar"))

	m_s_2u <- plm(similarCPcompl ~ T2*union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_s_2u, vcov=function(x) vcovHC(x, method="ar"))
	m_s_2m <- plm(similarCPcompl ~ T2*mng + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_s_2m, vcov=function(x) vcovHC(x, method="ar"))
	m_s_2b <- plm(similarCPcompl ~ T2*b1i + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_s_2b, vcov=function(x) vcovHC(x, method="ar"))


## distant
	m_d_1 <- plm(distantCPcompl ~ T1 + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_d_1, vcov=function(x) vcovHC(x, method="ar"))

	m_d_2 <- plm(distantCPcompl ~ T2 + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_d_2, vcov=function(x) vcovHC(x, method="ar"))

	m_d_2u <- plm(distantCPcompl ~ T2*union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_d_2u, vcov=function(x) vcovHC(x, method="ar"))
	m_d_2m <- plm(distantCPcompl ~ T2*mng + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_d_2m, vcov=function(x) vcovHC(x, method="ar"))
	m_d_2b <- plm(distantCPcompl ~ T2*b1i + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_d_2b, vcov=function(x) vcovHC(x, method="ar"))


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