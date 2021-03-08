# this script takes an incomplete subset of senior data from a .csv file, cleans it, computes factor scores, and prepares it for analysis
# additionally, this script is being used to teach and so occasionally blocks of code will be included but commented out
# in order to demonstrate in class how to explore and analyze the data

# NOTE from Spring 2020: This main body of this script was used in the spring 2019 course. Minor modifications have been made
# for spring 2020. I am using this shell so that students in the class can see different idiosyncratic ways people have 
# for writing their R code

require(tidyverse)
require(psych)


getwd()
wd_path <- "G:/My Drive/AK Faculty/Teaching/ENGE 5714 - Quantitative Analysis Spring 2020/ENGE 5714 Quantitative Analysis - Shared materials/Week 8 - Comparing two means (t-tests)"
setwd(wd_path)
list.files()


# load file seniorsurvey.csv into R
seniorSurvey.df <- read_csv(file = "seniorsurvey.csv")

# after loading, it is always nice to just see how things loaded in -- functions like str() and describe() are nice.
# for example, if we use describe(), we can see the following (some variables have been deleted):
#
#  		vars    n  mean    sd median trimmed   mad min max range  skew
#	What.is.your.PRIMARY.MAJOR.                          1 1849 31.79 20.43     29   31.25 28.17   1  70    69  0.18
# 	Internship..field.experience..co.op..or.practicum    2 1121  1.00  0.00      1    1.00  0.00   1   1     0   NaN
#	ParticipateServiceL                                  3  489  1.00  0.00      1    1.00  0.00   1   1     0   NaN
#	ParticipateCService                                  4 1296  1.00  0.00      1    1.00  0.00   1   1     0   NaN
#	ParticipateStudyAbroadSemester                       5  142  1.00  0.00      1    1.00  0.00   1   1     0   NaN
#	SJ1                                                  6 1733  2.32  1.03      2    2.25  1.48   1   5     4  0.46
#	SJ2                                                  7 1732  2.08  0.96      2    1.98  1.48   1   5     4  0.64
#	SJ3                                                  8 1731  2.77  0.88      3    2.79  1.48   1   5     4 -0.03
#	SJ4                                                  9 1726  2.27  1.01      2    2.20  1.48   1   5     4  0.52
#	SJ5                                                 10 1728  3.27  0.92      3    3.27  1.48   1   5     4 -0.19
#	SJ6                                                 11 1719  3.50  0.83      4    3.54  1.48   1   5     4 -0.40
#	SJ7                                                 12 1719  4.01  0.79      4    4.07  0.00   1   5     4 -0.75
#	SJ8                                                 13 1719  4.15  0.83      4    4.23  1.48   1   5     4 -0.86
#	DA1                                                 14 1719  2.23  0.93      2    2.15  1.48   1   5     4  0.57
#	DA2                                                 15 1719  2.86  0.95      3    2.89  1.48   1   5     4 -0.09
#	DA3                                                 16 1720  1.97  0.81      2    1.90  0.00   1   5     4  0.70
#	DA4                                                 17 1721  4.20  0.72      4    4.27  1.48   1   5     4 -0.66
#	DA5                                                 18 1721  4.06  0.81      4    4.11  1.48   1   5     4 -0.59

#                                                  kurtosis   se
#	What.is.your.PRIMARY.MAJOR.                          -1.33 0.48
#	Internship..field.experience..co.op..or.practicum      NaN 0.00
#	ParticipateServiceL                                    NaN 0.00
#	ParticipateCService                                    NaN 0.00
#	ParticipateStudyAbroadSemester                         NaN 0.00
#	SJ1                                                  -0.51 0.02
#	SJ2                                                  -0.16 0.02
#	SJ3                                                  -0.14 0.02
#	SJ4                                                  -0.40 0.02
#	SJ5                                                  -0.03 0.02
#	SJ6                                                   0.15 0.02
#	SJ7                                                   1.05 0.02
#	SJ8                                                   0.69 0.02
#	DA1                                                  -0.11 0.02
#	DA2                                                  -0.41 0.02
#	DA3                                                   0.31 0.02
#	DA4                                                   0.54 0.02
#	DA5                                                   0.16 0.02



# Upon examining this, I notice a few things:
#	Primary Major variable is all messed up.  I won't fix it in this script, but basically there is a numeric code needed (e.g., 13 = underwater basket weaving)
#	Columns 2-5 have lots of missing values (note the small N's) -- this means that this was asked via checkbox so (1) is true and missing is not missing but False
#	SJ1-8 and DA1-5 all look essentially ok -- about the same N (some survey fatigue or skips) but all values in range (1-5)

# Now, we know that SJ and DA are scales from the literature and we want to compute scale scores for those.  Typically for attitude scales like these
# we just report means across the items.  So, we will use the "psych" package to use a built in function to help us with this.  If you have not used psych
# yet, be sure it is installed using the command install.packages("psych") -- you need only do this once and then in subsequent uses you only need
# require(psych) to tell R to look in that package for the functions you will be using.

require(psych)

# subset out only the SJ and DA items in their own dataframe and then use tools in the psych package to compute scale means
seniorSurveyScales.df <- seniorSurvey.df[6:18]

# use my.keys() function from psych package to key-in how the scales are built (mapping items to scales, use - for reverse scored items)
my.keys <- make.keys(seniorSurveyScales.df, list(SJCa=c(-1,-2,-3,-4),SJCh=c(5,6,7),DA=c(-9,-10,-11,12,13)))

# use scoreItems function to score each respondent on the three scales of interest SJCa, SJCh, and DA -- the default here in scoreItems is to
# takes the mean of the items (not additive though that is sometimes used) and also, it imputes missing values instead of dropping cases 
# the scoreItems function calculates many things.  At this stage, all we really want are the scores, so I include a line to only extract that info
my.scales <- scoreItems(my.keys, seniorSurveyScales.df)	
my.scores <- my.scales$scores

# now, if you view the first few rows of the my.scores vector using the header -- head() command -- it looks like we expect:
#  head(my.scores)
#     SJCa     SJCh  DA
# [1,] 2.75 3.000000 3.2
# [2,] 3.75 3.333333 4.2
# [3,] 3.00 3.000000 3.0
# [4,] 2.25 4.333333 3.6
# [5,] 3.00 3.333333 3.4
# [6,] 4.50 4.333333 3.4

# now, lets build a clean dataframe to prep for analysis - by clean in this case I mean that we have replaced item scores from the scales with
# their means and also that we have fixed the NAs that don't belong (for participation variables, in this dataset, the NAs should be 0s)
mydf <- data.frame(seniorSurvey.df[1:5],my.scores, seniorSurvey.df[19:25])

mydf$ParticipateServiceL[is.na(mydf$ParticipateServiceL)] <- 0 
mydf$ParticipateCService[is.na(mydf$ParticipateCService)] <- 0 
mydf$ParticipateStudyAbroadSemester[is.na(mydf$ParticipateStudyAbroadSemester)] <- 0 
# mydf$ParticipateInternCoop...[is.na(mydf$ParticipateInternCoop...)] <- 0 ------- this variable read in cumbersomely named and I don't care about it right now so I'll skip

# At this point, I'm primed to do some analysis

# Let's investigate correlations
# What seems most obvious would just be to run cor() but, as we found out in class,
# this can cause us to run full speed ahead without considering assumptions
mycorrelations <- mydf %>% select(SJCa,SJCh,DA) %>% cor()
print(mycorrelations)

# Ok, so, it is important that we note that this ran correlations but R doesn't
# know that this was sample data and therefore that we are interested in 
# statistical significance (or not) of these results AND that our data may need
# another method (e.g., non-parametric).  cor() does have a way to run spearman
# instead
my.spearman.correlations <- mydf %>% select(SJCa,SJCh,DA) %>% cor(method="spearman")
print(my.spearman.correlations)

# if i need p values though, i need to change to something else -- corr.test
myresults <- corr.test(mydf$SJCa,mydf$DA)
# and then I can pull out results from this list or print it.  I'll do both 
print(myresults,short=FALSE)
myresults$r # correlation coefficient
myresults$p # p-value

# visually, we should be able to see this on a scatterplot.  I'm going to use qplot
# which stands for quickplot from within ggplot.  It is useful and quicker for simple
# plotting than building up ggplot (though from the same package)
# i need to jitter my points (take geom="jitter" out if you want to see why)
qplot(SJCa,DA,data=mydf,geom="jitter")

# other functions we used today in class were describe() and also the q-q plot creation
# to investigate normality assumption copying syntax from the Field, Miles, & Field book

newmodel <- t.test(DA ~ ParticipateStudyAbroadSemester, data=mydf, paired= FALSE)
newmodel

#Interpret- since 0 is not in the confidence interval and p-value is small, then the means are more than likely not the same. 
#for the t-etst- look at the p-value and then the decide if it is statistifically significant then look at the effect size that has its own package

library(effsize)
cohen.d(mydf$DA ~ mydf$ParticipateStudyAbroadSemester)

library(ggplot2)
ggplot(mydf, aes(x=as.factor(ParticipateStudyAbroadSemester), y=DA)) + 
  geom_boxplot(fill="slateblue", alpha=0.2)

#https://www.r-graph-gallery.com/boxplot/ 
#use ~ if you are grouping by data in the same column. gather() puts data that you are grouping by in the same column
#use , if you are groupuing by data in a different column. spread() can get you there