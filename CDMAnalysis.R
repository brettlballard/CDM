#!/usr/bin/env Rscript
#Above line allows code to be run using ./CDMAnalysis.R in terminal

#Libraries and what they are used for commented next to them
library(dplyr)#as_tibble and many other dataframe manipulation shortcuts
library(data.table)#setnames function
library(reshape2)#melt function
library(insight)#print_color function
library(argparser)#anything parser related
library(GDINA)#anything CDM related
library(CDM)#anything CDM related

#Adding argument parsers so that I can vary the scoring analysis from the command line
parser <- arg_parser('Options for varying the run of the IRT scoring analysis')
parser <- add_argument(parser, "--test", help = 'test being used, options are FCI, FMCE, or FMCE-Th: default is FCI',nargs='*',default='FCI')
parser <- add_argument(parser, "--qmat", help = 'Q-Matrix being used, options are JS or PF: default is PF',nargs='*',default='PF')
#
#JS = John Suggested Q-matrix from mirt papers on FCI and FMCE
#JF = John Final Q-matrix taking in suggested changes from Q-matrix validation
#BS = Brett Suggested Q-matrix
#BF = Brett Final Q-matrix taking in suggested changes from Q-matrix validation
#PF = Paper Final Q-matrix
#PE = Paper Expert Q-matrix
#
parser <- add_argument(parser, "--post", help = 'TRUE if posttest data is being used for test: default is TRUE',nargs='*',default=TRUE)
parser <- add_argument(parser, "--items", help = 'items chosen for subscale in single run mode if subsetting',nargs='*',default=c('All'))
arg <- parse_args(parser)

#Redefining variable from argparser
if (arg$post){
	tt <- 'post'
}else {
	tt <- 'pre'
}

#Collect data
print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!Running ',arg$test,' ',toupper(tt),'TEST ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
df <- read.csv(paste0(arg$test,'-',tt,'.csv'))
data <- as_tibble(df)
QMdf <- read.csv(paste0('QMatrices/QM-',arg$test,'-',arg$qmat,'.csv'))

#Defining Items based on argparser
arg$items <- strsplit(arg$items,',')[[1]]
if ('All' %in% arg$items){
	Item <- colnames(data)
}else {
	Item <- paste0('Item',arg$items)
}
nitems <- length(Item)

#Printing out the full tibble and Q-Matrix so one can see column names and data types
print_color('============================================================================\n','bold')
print_color('========================Data Set With Selected Items========================\n','bold')
print_color('============================================================================\n','bold')
data <- data %>%
	select(all_of(Item))
data$Raw.Score <- apply(data[,Item],1,sum)
print(data)
npart <- nrow(data)

print_color('============================================================================\n','bold')
print_color('========================Data Set With Selected Items========================\n','bold')
print_color('============================================================================\n','bold')
print(QMdf)

#Converting to matrix for analyses below
QMdf <- QMdf %>% select(-'Items')
QMatrix <- as.matrix(QMdf)

##############################################################################################################
###############################################CDM ANALYSIS###################################################
##############################################################################################################
print_color('============================================================================\n','bgreen')
print_color('================================DINA ANALYSIS===============================\n','bgreen')
print_color('============================================================================\n','bgreen')

print_color('=======================DINA ANALYSIS FROM CDM PACKAGE=======================\n','bcyan')
DINAcdm <- gdina(data = data[,Item], q.matrix = QMatrix, rule = 'DINA')
summary(DINAcdm)

print_color('======================DINA ANALYSIS FROM GDINA PACKAGE======================\n','bcyan')
DINAgdina <- GDINA(dat = data[,Item], Q = QMatrix, model = 'DINA')
summary(DINAgdina)
