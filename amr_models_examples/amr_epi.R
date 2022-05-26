## TITLE: EXAMPLE CODE FOR AMR ANALYSIS ON CHICKENS AND HUMANS IN ECUADOR
## AUTHOR: HAYDEN HEDMAN
## AIM: SUMMARY OF A FEW HIERARCHICAL MODEL EXAMPLES USED DURING DISSERTATION RESEARCH AT UNVIERSITY OF MICHIGAN
## MORE INFO: www.haydenhedman.com

## LOAD PACKAGES
require(lme4)
require(lmerTest)
require(gee)

## LOAD DUMMY DATA
df <- read.csv("amr_epi_dummy_data.csv",header=T)

## AMR EXPOSURE BY DISTANCE TO HOUSEHOLD FARMING BROILER CHICKENS IN PREVIOUS 60 DAYS
rep.mem.blaCTX <- glmer(CTX~ dist_60day + (1|sample_period) + (1|community) + (1|id), data=df, family=binomial)

## REPEATED MEASURES OF CHICKENS AND HUMANS AMR LEVELS OVER TIME
mf1 <- formula(resistance~period_2 + period_3+(1|community)+(1|id))
glm1 <- glmer(mf1, data=s_drug, family=binomial)



## EXAMPLE GEE FUNCTION FOR AMR COMPARISONS BETWEEN VILLAGES AND SAMPLE PERIODS 
GEE_bin_pc <- function(drug) {
  s_drug <- subset(cr, drug_code==drug)
  s_drug$vil_period <- NULL
  s_drug$vil_period[(s_drug$village==1)&(s_drug$sample_period==1)] <- 2
  s_drug$vil_period[(s_drug$village==1)&(s_drug$sample_period==0)] <- 0
  s_drug$vil_period[(s_drug$village==0)&(s_drug$sample_period==0)] <- 1
  
  mf1 <- formula(resistance~as.factor(vil_period))
  gee1 <- geeglm(mf1, data=s_drug, id=id_household, family=binomial)
  
  cc <- coef(summary(gee1))
  citab <- with(as.data.frame(cc),
                cbind(lwr=Estimate-1.96*Std.err,
                      upr=Estimate+1.96*Std.err))
  rownames(citab) <- rownames(cc)
  cbind(cc,citab)
}
GEE_bin_pc("AM")

