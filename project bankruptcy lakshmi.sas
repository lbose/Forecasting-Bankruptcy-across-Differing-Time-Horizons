/*Bankruptcy model for market variables*/
/* Lakshmi Palaparambil Dinesh*/
/*11/3/2012*/


/*Set a library for the project*/
libname ld 'C:\Users\Lakshmi\Documents\Fall_2012-2013\FinancialEngineering\Project_Files';

/*Connect to wrds*/
%let wrds=wrds.wharton.upenn.edu 4016;
options comamid=TCP remote=WRDS;
signon username=_prompt_;
rsubmit;

libname compx '/wrds/comp/sasdata/naa';
libname compn '/wrds/comp/sasdata/naa/company';
libname sm '/wrds/crsp/sasdata/a_stock';
libname sp '/wrds/crsp/sasdata/a_indexes';

/*Delete the old file that has the same name(usually not necessary)*/ 
proc datasets library=work;
   delete acct;
run;

*Select variables for all bankrupted firms in manufacture industry in year range (1990, 2010);
*create a SAS dataset named 'bankrupt' in the WRDS server;

*select accouting variables for all bankrupted firms in manufacture industry in year range (1990, 2010);

proc sql;
create table acct as
select 
a.cusip, a.gvkey, a.fyear, a.datadate,  
a.AT, a.RE, a.WCAP, a.DLTT, a.DLC, a.EBIT, a.SALE,
a.csho, a.prcc_f, a.CEQ, ni, act,
b.dldte, b.dlrsn, b.sic
from compx.funda a 
inner join compn.company b 
on (a.gvkey=b.gvkey)
where input(sic, best12.) >= 2000
and input(sic, best12.) < 4000
and fyear >= 1990 and fyear <=2010
and datafmt = 'STD';
quit;

proc download data=acct out=ld.acct;
run;


*download individual stock return;
data return; set sm.msf
(keep=cusip permno date RET prc SHROUT);
where 1990<=year(date)<2011;
run;

proc download data=return out=ld.return; 
run; 

*download sp500 index return;
data sp500; set sp.msp500;
if year(caldt) >= 1990 and year(caldt)<2011;
keep caldt ewretd ewretx spindx sprtrn totcnt totval vwretd vwretx;
run;

proc download data=sp500 out=ld.sp500; 
run; 

endrsubmit;
signoff;

/*Calculate Accounting variables for 1990-2005*/
data ld.acct;
set ld.acct;
if fyear>=1990 and fyear<=2005;
cusip = substr(cusip, 1, 8);
*Altman's variables;
WCbTA= WCAP/AT;
REbTA = re/at;
EBITbTA = EBIT/AT;
TL = sum(0.5*DLTT, DLC);
MEbTL= csho * abs(prcc_f) /TL;
SbTA = sale/at;
*Zmi's varialbes;
NIbTA = ni/at;
TLbTA = TL/at;
CAbCL =  act/dlc;
if dlrsn IN('02','03') then bky = 1; else bky = 0;
run; 
/*Run PROC MEANS on accounting variables and identify outliers*/
proc means data=ld.acct sum n mean p1 p5 p10 p25 p50 p75 p95 p99 min max   ;
var WCbTA REbTA EBITbTA TL MEbTL SbTA NIbTA TLbTA CAbCL ;
output out=percent n=n mean=mean p1=p1 p5=p5 p10=p10 p25=p25 p50=p50 p75=p75 p95=p95 p99=p99;
run;

data ld.acct; set ld.acct;
if WCbTA < 0 then WCbTA=0;
if WCbTA > 1 then WCbTA = 1;
if REbTA < 0 then REbTA=0;
if REbTA > 1 then REbTA=1;
if EBITbTA < 0 then EBITbTA=0;
if EBITbTA > 1 then EBITbTA=1;
if TL < 0 then TL=0;
if TL > 6500 then TL=6500;
if MEbTL < 0 then MEbTL=0;
if MEbTL > 5200 then MEbTL=5200;
if SbTA < 0 then SbTA=0;
if SbTA > 5 then SbTA=5;
if NIbTA < 0 then NIbTA=0;
if NIbTA > 1 then NIbTA=1;
if TLbTA < 0 then TLbTA=0;
if TLbTA > 5 then TLbTA=5;
if CAbCL < 0 then CAbCL=0;
if CAbCL > 3500 then CAbCL=3500;
run;
/*Frequency of bankruptcy*/
proc freq data =ld.acct;
tables bky;
run;
/*Test of Ya's code*/
data ld.acct_test;
set ld.acct;
sum=sum(WCbTA,REbTA,EBITbTA,TL,MEbTL,SbTA,NIbTA,TLbTA,CAbCL);
run;

/*proc sort data=ld.acct_test;by cusip fyear descending sum;run;
 
proc sort data=ld.acct_test nodupkey;by cusip fyear;run;
  
run;
 
proc sort data=temp;by cusip fyear descending sum;run;
 
proc sort data=temp nodupkey;by cusip fyear;run;*/
 
 

/*Taking means at a cusip level to remove missing values*/
proc means data = ld.acct mean median noprint;
class cusip;
var WCbTA REbTA EBITbTA TL MEbTL SbTA NIbTA TLbTA CAbCL;
output out=ld.meandata mean=mean1 mean2 mean3 mean4 mean5 mean6 mean7 mean8 mean9 ;
run;

proc sql; create table acct as
select a.*, b.*
from ld.acct as a
left join ld.meandata as b
on a.cusip=b.cusip;
quit;
/*Impute missing values using means at a CUSIP level*/
data acct; set acct;
if WCbTA =. then WCbTA=mean1;
if REbTA = . then REbTA=mean2;
if EBITbTA = . then EBITbTA=mean3;
if TL = . then TL=mean4;
if MEbTL = . then MEbTL=mean5;
if SbTA = . then SbTA=mean6;
if NIbTA = . then NIbTA=mean7;
if TLbTA = . then TLbTA=mean8;
if CAbCL = . then CAbCL=mean9;
drop mean1 mean2 mean3 mean4 mean5 mean6 mean7 mean8 mean9;
run;
/*Running PROC MEANS to get summary statistics*/
proc means data = acct;
var WCbTA REbTA EBITbTA TL MEbTL SbTA NIbTA TLbTA CAbCL;
run;
/*select only relevant years*/
data acct_1990_2002;
set ld.acct;
where fyear>=1990 and fyear<=2002;
run;

/*Splitting the values where the year is not the latest year and hence company is not considered bankrupt*/
proc sql; create table temp1 as
select a.* from acct_1990_2002 as a 
where fyear ne (select max(fyear) from acct_1990_2002 as b where a.cusip=b.cusip);
quit;
/*Splitting the values where year is max year and hence we get the year of bankruptcy*/
proc sql; create table temp2 as
select a.* from acct_1990_2002 as a 
where fyear = (select max(fyear) from acct_1990_2002 as b where a.cusip=b.cusip);
quit;
/*Setting the latest bankrupt companies as 1*/
data temp3; set temp1;
if bky=1 then bky=0;
if bky=0 then bky=0;
run;

data acct_dev_1990_2002;
set temp3 temp2;
run;
/*Get summary stats for this data*/
proc sort data=acct_dev_1990_2002;
by bky;
run;
proc means data = acct_dev_1990_2002;
var WCbTA REbTA EBITbTA TL MEbTL SbTA NIbTA TLbTA CAbCL;
by bky;
run;

/* Sort data by fyear and gkey*/
/*proc sort data = acct;
by fyear gvkey;
run;
/*Choose the latest observation for the bankrupt companies*/
/*proc sql;
create table temp1 as
select a.*
from acct as a
where bky =1
group by gvkey
having max(a.fyear) = a.fyear;
quit;
run;

/* Choose all observations for non-bankrupt companies*/
/*proc sql;
create table temp2 as
select a.*
from acct as a
where bky =0;
group by gvkey;
run;
/* Merge tables for panel data accounting variables 1990-2005*/
/*data ld.acct_dev;
set temp1 temp2;
run;
/* Find the number of bankrupt and non-bankrupt companies yearly*/
proc freq data= acct_dev_1990_2002;
tables fyear*bky;
run;
/*Merging market data for S&P and individual stock returns based on date for 1990-2005*/
proc sql;
create table mkt_1990_2002 as
select A.*, B.* from ld.return as A, ld.Sp500 as B
/*where A.date = B.caldt*/
where 1990<=year(date)<=2004
and  1990<=year(caldt)<=2004
and A.date = B.caldt;
quit;
run;

*calculate log relative size;
data mkt_1990_2002;
set  mkt_1990_2002;
equity = abs(prc)*SHROUT; 
rsize = log(equity/totval);
*inpute missing return using index return;
if ret = . then ret = sprtrn;
if ret = .C then ret = sprtrn;
if ret = .B then ret = sprtrn;
year = 	year(date);
run;
/*calculate excess return*/
data mkt_1990_2002;
set  mkt_1990_2002;
er=ret-sprtrn;
run;
/*choose distinct companies*/
/*proc sql;
create table ld.temp  as
select distinct CUSIP, year from ld.mkt_1990_2002; quit;
run;
/*Merge the market and accounting variables*/
/*proc sql;
create table ld.temp  as
select A.*, B.*
from ld.mkt_1990_2002 as A, ld.acct_dev_1990_2002 as B
where  A.CUSIP = B.CUSIP;
quit;
run;
/* Merge market and accounting variables*/
/*proc sql;
create table ld.temp1  as
select A.*, B.*
from ld.mkt_1990_2002 as A
left join ld.acct_dev_2002 as B 
on  A.CUSIP = B.CUSIP
and A.year = B.fyear;
quit;
run;

proc sort data= ld.mkt_1990_2002;
by CUSIP year;
run;
proc sort data= ld.acct_dev_1990_2002;
by CUSIP year;
run;
/**prepare data to calculate sigma and excess return in R;
data ld.temp2;
length CUSIP $ 10;
merge ld.mkt(IN=IN1) ld.acct_dev(IN=IN2) ;
by CUSIP year;
IF IN1=1 AND IN2=1;
run;
/*calculate sigma*/
proc sort data= mkt_1990_2002;
by descending CUSIP descending year;
run;

proc reg data=mkt_1990_2002 noprint;
by descending CUSIP descending year;
model ret=sprtrn;
output out=b r=resid; run;

proc sql;
create table sigma_1990_2002 as
select CUSIP, year, std(resid) as sigma from b
group by CUSIP, year;
quit;

*average rsize and er in a year;
proc sql;
create table rsize_er_1990_2002 as
select CUSIP, year, avg(rsize) as rsize, avg(er) as er, avg(prc) as price from mkt_1990_2002
group by CUSIP, year;
quit;
/*Adding sigma to the rsize and er table*/
proc sql;
create table mktvar_1990_2002 as
select a.*, b.sigma
from rsize_er_1990_2002 as a, sigma_1990_2002 as b
where a.CUSIP=b.CUSIP and a.year=b.year;
quit;
/*Sorting accounting data*/
proc sort data= acct_dev_1990_2002;
by CUSIP fyear;
run;

/*Getting the final data for 1990-2005*/
proc sql;
create table ld.finaldata_1990_2002 as
select a.fyear, a.CUSIP, a.bky, WCbTA, REbTA, EBITbTA, MEbTL, SbTA, NIbTA, TLbTA, CAbCL, a.dlrsn,
b.rsize, b.er, c.sigma
from acct_dev_1990_2002 as a, rsize_er_1990_2002 as b, mktvar_1990_2002 as c
where a.cusip = b.cusip and b.cusip = c.cusip and a.fyear = b.year and b.year = c.year;
quit;

/**calculate bankrupt year ;
proc sql;
create table ld.bankrupyear_1990_2002 as
select cusip, max(fyear) as byear, count(fyear) as county
from ld.finaldata_1990_2002
group by cusip;
run;

proc sql;
create table ld.finaldata_1990_2002 as
select *
from ld.finaldata_1990_2002 as a ,  ld.bankrupyear_1990_2002 as b
where a.cusip = b.cusip;
run;

data ld.finaldata_1990_2002;
set ld.finaldata_1990_2002;
if byear = fyear and dlrsn in ("02", "03") then  y = 1 ;
else y = 0;
cusipn = input(cusip, 8.0);
/*logage = log(county);*/
/*run;*/

   /*proc sort data=ld.finaldata_1990_2002;
      by cusipn fyear;
   run;*/
/*Checking the number of bankrupt companies in the data set*/
proc freq data = ld.finaldata_1990_2002;
tables fyear*bky;
run;

/*proc means data = ld.finaldata_1990_2002;
class y;
var WCbTA REbTA EBITbTA  MEbTL  SbTA  NIbTA  TLbTA CAbCL  rsize er sigma;
run;

/*Running a logistic regression model on the data from 1990-2005*/
proc logistic data = ld.finaldata_1990_2002 descending
outmodel = ld_logit plots = all plots(MAXPOINTS=none);
model bky =  WCbTA REbTA EBITbTA  MEbTL  SbTA  NIbTA  TLbTA CAbCL  rsize er sigma / rsquare ctable;
output out =out p=p;
score data = ld.finaldata_validation out=valpred outroc=vroc;
roc; roccontrast;
run;
/*Gain and lift chart*/
%inc 'C:\Users\Lakshmi\Documents\Fall_2012-2013\FinancialEngineering\Assignments\HW2\fusion_41683_6_gainlift.sas.txt';
%GainLift(data=out, response = bky, event =1,p=p,out=ld.rankout_1990_2002,groups=10)
  
/*Summary Statistics*/
/*libname ld 'C:\Users\Lakshmi\Documents\Fall_2012-2013\FinancialEngineering\Project_Files';*/
/*proc sort data = ld.finaldata_1990_2002;
by bky;
proc means data = ld.finaldata_1990_2002;
var WCbTA REbTA EBITbTA  MEbTL  SbTA  NIbTA  TLbTA CAbCL  rsize er sigma;
by bky;
title 1990-2005;
run;
proc sort data = ld.finaldata_1990_2002;
by bky;
proc means data = ld.finaldata_1990_2002;
var WCbTA REbTA EBITbTA  MEbTL  SbTA  NIbTA  TLbTA CAbCL  rsize er sigma;
by bky;
title 1990-2005;
run;
proc sort data = ld.finaldata_1990_2002;
by bky;
proc means data = ld.finaldata_1990_2002;
var WCbTA REbTA EBITbTA  MEbTL  SbTA  NIbTA  TLbTA CAbCL  rsize er sigma;
by bky;
title 1990-2003;
run;
proc sort data = ld.finaldata_1990_2001;
by bky;
proc means data = ld.finaldata_1990_2001;
var WCbTA REbTA EBITbTA  MEbTL  SbTA  NIbTA  TLbTA CAbCL  rsize er sigma;
by bky;
title 1990-2001;
run;
proc sort data = ld.finaldata_1990_1999;
by bky;
proc means data = ld.finaldata_1990_1999;
var WCbTA REbTA EBITbTA  MEbTL  SbTA  NIbTA  TLbTA CAbCL  rsize er sigma;
by bky;
title 1990-1999;
run;
proc sort data = ld.finaldata_1990_2002;
by bky;

proc means data = ld.finaldata_1990_2002;
var WCbTA REbTA EBITbTA  MEbTL  SbTA  NIbTA  TLbTA CAbCL  rsize er sigma;
by bky;
title 1990-2004;
run;
proc sort data = ld.finaldata_validation;
by bky;
proc means data = ld.finaldata_validation;
var WCbTA REbTA EBITbTA  MEbTL  SbTA  NIbTA  TLbTA CAbCL  rsize er sigma;
by bky;
title validation;
run;
/**lag 2 and lag 5;
data temp;
set shumway.finaldata;
if county >1;
run;
proc panel data=temp;
      id cusipn fyear;
      lag WCbTA(2) REbTA(2) EBITbTA(2) MEbTL(2) SbTA(2) NIbTA(2) TLbTA(2) CAbCL(2) rsize(2) er(2) sigma(2) logage(2)/ out=shumway.lag2;
run;
proc panel data=temp;
      id cusipn fyear;
      lag WCbTA(5) REbTA(5) EBITbTA(5) MEbTL(5) SbTA(5) NIbTA(5) TLbTA(5) CAbCL(5) rsize(5) er(5) sigma(5) logage(5)/ out=shumway.lag5;
run;






















proc freq data=bankrupt_val;
table bky; run;


proc means data=bankrupt_val sum n mean p1 p5 p10 p25 p50 p75 p95 p99 min max   ;
var WCbTA REbTA EBITbTA TL MEbTL SbTA NIbTA TLbTA CAbCL ;
output out=percent n=n mean=mean p1=p1 p5=p5 p10=p10 p25=p25 p50=p50 p75=p75 p95=p95 p99=p99;
run;



data bankrupt1; set bankrupt_val;
if WCbTA < 0 then WCbTA=0;
if WCbTA > 1 then WCbTA = 1;
if REbTA < 0 then REbTA=0;
if REbTA > 1 then REbTA=1;
if EBITbTA < 0 then EBITbTA=0;
if EBITbTA > 1 then EBITbTA=1;
if TL < 0 then TL=0;
if TL > 10000 then TL=10000;
if MEbTL < 0 then MEbTL=0;
if MEbTL > 10000 then MEbTL=10000;
if SbTA < 0 then SbTA=0;
if SbTA > 5 then SbTA=5;
if NIbTA < 0 then NIbTA=0;
if NIbTA > 1 then NIbTA=1;
if TLbTA < 0 then TLbTA=0;
if TLbTA > 5 then TLbTA=5;
if CAbCL < 0 then CAbCL=0;
if CAbCL > 5000 then CAbCL=5000;
run;

proc means data = bankrupt1 mean median noprint;
class gvkey;
var x1 x2 x3 x4 x5;
output out=meandata mean=mean1 mean2 mean3 mean4 mean5 median=median1 median2 median3 median4 median5;
run;

proc sql; create table bankrupt2 as
select a.*, b.*
from bankrupt1 as a
left join meandata as b
on a.gvkey=b.gvkey;
quit;

data bankrupt3; set bankrupt2;
if x1 = . then x1=mean1;
if x2 = . then x2=mean2;
if x3 = . then x3=mean3;
if x4 = . then x4=mean4;
if x5 = . then x5=mean5;
run;

proc sql; create table temp1 as
select a.* from bankrupt3 as a 
where fyear ne (select max(fyear) from bankrupt3 as b where a.gvkey=b.gvkey);
quit;

proc sql; create table temp2 as
select a.* from bankrupt3 as a 
where fyear = (select max(fyear) from bankrupt3 as b where a.gvkey=b.gvkey);
quit;

data temp3; set temp1;
if bky=1 then bky=0;
if bky=0 then bky=0;
run;

data sb.bky_dev;
set temp3 temp2;
drop mean1 mean2 mean3 mean4 mean5 median1 median2 median3 median4 median5;
run;

data sb.bky_val;
set temp3 temp2;
drop mean1 mean2 mean3 mean4 mean5 median1 median2 median3 median4 median5;
run;

proc sql; create table sb.bky_dev_cs as
select a.* from sb.bky_dev as a 
where fyear in (select max(fyear) from sb.bky_dev as b where a.gvkey=b.gvkey);
quit;
proc sql; create table sb.bky_val_cs as
select a.* from sb.bky_val as a 
where fyear in (select max(fyear) from sb.bky_val as b where a.gvkey=b.gvkey);
quit;

proc freq data=sb.bky_val ;
tables fyear*bky / norow nocol nopercent;
run;

/***Panel data analysis - keeping all years for all firms *****/
/****Keep the proportion of bankrupt and non-bankrupt same in train and test sample ****/

/*
proc means data=sb.bky_val mean std min max;
class bky;
var x1 x2 x3 x4 x5;
output out=percent mean=mean std=std min=min max=max;
run;

proc kde data=sb.bky_dev;
univar x1 x2 x3 x4 x5;
run;


proc logistic data=sb.bky_dev descending outest=betas covout outmodel=bkymodel plots=all;
model bky = x1 x2 x3 x4 x5 /lackfit rsq technique=newton outroc=roc clparm=wald clodds=wald ctable nodummyprint nologscale nocheck;
output out=pred resdev=resdev reschi=reschi h=hat p=phat lower=lcl upper=ucl predprob=crossvalidate;
score data=sb.bky_dev out=predict_bkydev fitstat outroc=roctrain;
score data=sb.bky_val out=predict_bkyval fitstat outroc=roctest;
run;

proc rank data=predict_bkydev out=predict_bky groups=10 descending; var p_1; ranks rank1; run;
proc means data=predict_bky; class rank1; var bky; run; 

proc rank data=predict_bkyval out=predict_bky groups=10 descending; var p_1; ranks rank1; run;
proc means data=predict_bky; class rank1; var bky; run;  

/***Cross-section data analysis - lagging end year by 2 years for all firms *****/
/*
proc means data=sb.bky_val_cs mean std min max;
class bky;
var x1 x2 x3 x4 x5;
output out=percent mean=mean std=std min=min max=max;
run;

proc logistic data=sb.bky_dev_cs descending outest=betas covout outmodel=bkymodel plots=all;
model bky = x1 x2 x3 x4 x5 /lackfit rsq technique=newton outroc=roc clparm=wald clodds=wald ctable nodummyprint nologscale nocheck;
output out=pred resdev=resdev reschi=reschi h=hat p=phat lower=lcl upper=ucl predprob=crossvalidate;
score data=sb.bky_dev_cs out=predict_bkytrain fitstat outroc=roctrain;
score data=sb.bky_val_cs out=predict_bkytest fitstat outroc=roctest;
run;

proc rank data=predict_bkytrain out=predict_bky groups=10 descending; var p_1; ranks rank1; run;
proc means data=predict_bky; class rank1; var bky; run; 

proc rank data=predict_bkytest out=predict_bky groups=10 descending; var p_1; ranks rank1; run;
proc means data=predict_bky; class rank1; var bky; run;  

proc discrim data = sb.bky_dev_cs testdata = sb.bky_val_cs out=pred testout=pred1;
var x1 x2 x3 x4 x5;
class Bky;
run;
proc rank data=pred out=predict_bky groups=10 descending; var _1; ranks rank1; run;
proc means data=predict_bky; class rank1; var bky; run; 

proc rank data=pred1 out=predict_bky groups=10 descending; var _1; ranks rank1; run;
proc means data=predict_bky; class rank1; var bky; run; 




data rocfile; set roctrain; run;

%MACRO ROC;
DATA ROC;
     SET ROCFILE;  * A  D  C  B ;
	 MISS = (_FALPOS_ + _FALNEG_)/(_POS_ + _NEG_ + _FALPOS_ + _FALNEG_); 	 
RUN;

PROC SORT DATA = ROC OUT = M; BY MISS;RUN;
DATA MISSRATE;
     SET M;
	 IF _N_ > 5 THEN DELETE;
PROC PRINT DATA = MISSRATE;
RUN;



PROC GPLOT DATA = ROC;
           PLOT _SENSIT_*_1MSPEC_;
		   SYMBOL V = DOT, I = NONE;
RUN;
QUIT;
%MEND;

%ROC; run;

%LET PCUT = 0.5;

data miss; set pred;
	 if phat > &PCUT then yhat = 1; else yhat = 0;
	 if bky = yhat then miss = 0; else miss = 1;
RUN;

proc freq data=miss; tables miss; run;


