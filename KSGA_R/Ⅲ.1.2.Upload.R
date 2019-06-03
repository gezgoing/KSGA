### Upload DB
library(RODBC)

### Data 읽기
sales<-read.csv('../KSGA_BI/PRF_SALES.csv',header=T, as.is=T)
str(sales)
names(sales)[1]<-'YM'
sales$YM<-as.character(sales$YM)

cost<-read.csv('../KSGA_BI/PRF_COST.csv',header=T, as.is=T)
str(cost)
names(cost)[1]<-'YM'
cost$YM<-as.character(cost$YM)

prc<-read.csv('../KSGA_BI/PRF_PRC.csv',header=T, as.is=T)
str(prc)
names(prc)[1]<-'ITM_CD'

org<-read.csv('../KSGA_BI/PRF_ORG.csv',header=T, as.is=T)
str(org)
names(org)[1]<-'BU'

CustSM<-read.csv('../KSGA_BI/PRF_CUST_SM.csv',header=T, as.is=T)
str(CustSM)
names(CustSM)[1]<-'GRP_CD'


#############################
# CREATE FOR THE FIRST TIME #
#############################
ch<-odbcConnect("jkahn",uid="jkahn",pwd="****")
system.time(sqlSave(ch, sales, tablename = "TB_PRF_SALES", rownames=F))
system.time(sqlSave(ch, cost, tablename = "TB_PRF_COST", rownames=F))
system.time(sqlSave(ch, prc, tablename = "TB_PRF_PRC", rownames=F))
system.time(sqlSave(ch, org, tablename = "TB_PRF_ORG", rownames=F))
system.time(sqlSave(ch, CustSM, tablename = "TB_PRF_CUST_SM", rownames=F))
close(ch)

##############################
# APPEND DATA to Existing DB #
##############################
ch<-odbcConnect("jkahn",uid="jkahn",pwd="****")
system.time(sqlSave(ch, sales, tablename = "TB_PRF_SALES", append=T, rownames=F))
close(ch)

#####################
# Read DATA from DB #
#####################
ch<-odbcConnect("jkahn",uid="jkahn",pwd="****")
system.time(sales_f <- sqlFetch(ch, sqtable = "TB_PRF_SALES", rownames=F))
close(ch)

head(sales_f)

#######################################
# DELETE DATA FROM THE EXISTING TABLE #
#######################################
ch<-odbcConnect("jkahn",uid="jkahn",pwd="****")
sql_del<-paste("delete from TB_PRF_SALES where YM=",YM,sep="")
sqlQuery(ch, sql_del, errors = TRUE)
close(ch)

##############
# DROP TABLE #
##############
ch<-odbcConnect("jkahn",uid="jkahn",pwd="****")
sql_drop<-"drop table TB_PRF_PRC"
sqlQuery(ch, sql_drop, errors = TRUE)
close(ch)

# Single and double quotes delimit character constants. They can be used interchangeably but double quotes are preferred (and character constants are printed using double quotes), <<<so single quotes are normally only used to delimit character constants containing double quotes>>>. 

# \n newline 
# \r carriage return 
# \t tab 
# \b backspace 
# \a alert (bell) 
# \f form feed 
# \v vertical tab 
# \\ backslash \ 
# \' ASCII apostrophe ' 
# \" ASCII quotation mark " 
# \` ASCII grave accent (backtick) ` 
# \nnn character with given octal code (1, 2 or 3 digits) 
# \xnn character with given hex code (1 or 2 hex digits) 
# \unnnn Unicode character with given code (1--4 hex digits) 
# \Unnnnnnnn Unicode character with given code (1--8 hex digits) 
