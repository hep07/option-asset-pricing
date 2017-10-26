* This sample program is to:
  1. retrieves trade data from the TAQ files on the WRDS cloud server
     for stocks whose tickers are included in "tic" for period between
     "begyr" and "endyr"
  2. download the data files to local machine, and 
  3. convert the results to CSV files;

* Each ticker is stored in a separate file;

* Author: Charles Tang (ct284@columbia.edu)
  Date:   June 1, 2017;

* This program can run on a PC or the CBS Grid.  
  To submit the program on the CBS Grid, use the following command:
  sas --grid_mem=25G "-set begyr 1997 -set endyr 1997 -set begmm 10 -set endmm 10 -set begdd 30 -set enddd 30 -set threadID 1 -memsize 12g -sortsize 5g taq_grid_all_tickers_by_day_taq.sas";



%let threadID = %sysget(threadID);


* this unique job ID is for creating the tic name so that we should not have locker availability issue nay more;
%let jobID = &SYSJOBID;



*********************************************************************
* Users must specify requests by modify the "%let" statements below;*
*;  %let tic=option_&jobID;                  /* Tickers requested     */  *
*;  %let datayear=%sysget(datayr);                   /* Begin year            */  *
*;  %let datamonth=%sysget(datamt);                   /* End year              */  *
*;  %let wrdsid=hep07;                      /* WRDS User ID          */  *
*;  %let wrdspswd={SAS002}C4A1204849B6E8673F0E00530C05B3F9;                    /* WRDS Encoded Password */  *
*;  %let store=./tickdata_bash_&threadID;     /* For CBS Grid   */  *
*;* %let store=C:\Users\owner\Desktop\TAQ;    /* For PC         */  *
*********************************************************************;
  



* Noxwait is not needed if you run this program on the CBS Grid;
options nocenter ps=max ls=233 compress=yes macrogen symbolgen mprint;* noxwait;

%global tic begyr endyr store wrdsid wrdspswd;

* Create storage folder on local machine;
x mkdir &store;
libname store "&store";

* Make connection to WRDS cloud server;
%let wrds=wrds-cloud.wharton.upenn.edu 4016;options comamid=TCP remote=WRDS;signon username=&wrdsid password="&wrdspswd";

%syslput wrdsid=&wrdsid;
%syslput tic=&tic;

%syslput threadID=&threadID;
%syslput jobID=&jobID;
%syslput datayear=&datayear;
%syslput datamonth=&datamonth;


* Define work environment on WRDS;
rsubmit wrds; 

    options nocenter ps=max ls=133 compress=yes macrogen symbolgen mprint; 
    
    x mkdir /scratch/columbia/&wrdsid;
    x mkdir /scratch/columbia/&wrdsid/&jobID;
    
    libname wrdsf "/scratch/columbia/&wrdsid/&jobID";
 
* rsubmit wrds;

%macro get(yy=,mm=);




x rm -r /scratch/columbia/&wrdsid/&jobID;
x mkdir /scratch/columbia/&wrdsid/&jobID;


proc append base=wrdsf.&tic
 
data=OPTIONM.opprcd&yy
     ( keep=SECID DATE EXDATE CP_FLAG STRIKE_PRICE VOLUME OPEN_INTEREST BEST_BID BEST_OFFER IMPL_VOLATILITY
       where=( MONTH(date)=&mm ) 
     ); 
run;


proc download inlib=wrdsf outlib=store;run;

x rm -r /scratch/columbia/&wrdsid/&jobID;

%mend;

* Execute the macro;
%get(yy=&datayear,mm=&datamonth);run;

endrsubmit;





* On local machine, convert files to CSV;
proc export data=store.&tic
      outfile="&store/WIP_&datayear._&datamonth..csv"
      dbms=csv
replace;
run;

x mv &store/WIP_&datayear._&datamonth..csv &store/&datayear._&datamonth..csv;

* remove all the sas files downloaded to save space, index, data etc..;
x rm &store/&tic..*;

signoff;

* Done!;
