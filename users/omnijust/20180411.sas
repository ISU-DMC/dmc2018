/*Read in the datasets.*/
proc import file="c:\users\omnijust\desktop\dmc\items.csv" out=items dbms=csv replace;
run;

proc import file="c:\users\omnijust\desktop\dmc\prices.csv" out=prices_1 dbms=csv replace;
run;
data prices_1;
	set prices_1;
	drop size;
	obs+1;
run;
data prices_2;
	infile 'c:\users\omnijust\desktop\dmc\prices.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2;
	informat pid best32. ;
	informat size $30. ;
	format pid best32. ;
	format size $30. ;
	input pid size;
	obs+1;
run;
data prices;
	merge prices_2 prices_1 ;
	by obs;
	format pid best32. size $30.;
	drop size1 obs;
run;

data train;
	infile 'c:\users\omnijust\desktop\dmc\train.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2;
	informat date mmddyy10. ;
	informat pid best32. ;
	informat size $20. ;
	informat units best32. ;
	format date mmddyy10. ;
	format pid best12. ;
	format size $20. ;
	format units best12. ;
	input date pid size $ units;
	if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
run;

/*perhaps need to check the levels of size*/
proc freq data=items;
	tables size / out=freq_items;
run;
proc freq data=prices;
	tables size / out=freq_prices;
run;
/*results show that the two datasets have different levels of size*/

/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/**/*/*/*/*/*/*/*/*/*/*/*/*/*/*/
/*/*/*/*/*/*/*/*/*/*PREPROCESSING of TRAIN*/*/*/*/*/*/*/*/*/*/
/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/**/*/*/*/*/*/*/*/*/*/*/*/*/*/*/

/*train is so muddy, we have duplicated observations for the same (PID, size)*/
proc sort data=train;
	by pid size;
run;

/*one possible way is to sum those observations as long as they are in the same (date, pid, size)*/

proc freq data=train;
	table units;
run;
/*minimal units is 1*/

/*better to keep the data, instead of delete them*/
data train;
	set train;
	if missing(units) then units=0;
run;
proc sql;
	create tatble train_nodup as
	select date, pid, size, sum(units) as units from train
	group by date, pid, size
	order by pid, date, size;

proc sort data=train_nodup;
	by pid size;
run;

proc transpose data = train_nodup out = train_wide (drop = _NAME_);
	by pid size;
	var units;
	id date;
run;
proc export data=train_wide outfile="C:\Users\omnijust\Desktop\DMC\train_wide.xlsx" dbms=excel replace;run;

/*till now, train has been mostly clean*/
/*move on to the other two*/

proc sql;
	create table combined
	as select * from items as a full join prices as b
	on a.pid=b.pid and a.size=b.size;

/*looking at data set COMBINED, check for missing values*/
data temp;
	set prices;
	if _10_1_2017=18.99 then output;
run;
proc sort data=combined out=combined_sort_by_10012017;
	by _10_1_2017;
run;
data temp1;
	set combined;
	if _10_1_2017=18.99 then output;
run;
/*see if there is any missing PID and SIZE*/
proc sort data=temp;
	by pid size;
run;
proc sort data=temp1;
	by pid size;
run;
/*these two datasets are not the same, indicating (PID, SIZE) is changing over time*/

/*PROLBLEM IS WE HAVE NO INFORMATION ABOUT THESE NEW ITEMS*/
/*first distinguish the new items*/

/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/**/*/*/*/*/*/*/*/*/*/*/*/*/*/*/
/*/*/*/*/*/*/*/*/*/*PREPROCESSING of PRICES*/*/*/*/*/*/*/*/*/;
/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/**/*/*/*/*/*/*/*/*/*/*/*/*/*/*/;
proc iml;
	use prices;
	read all var _NUM_ into x;
	close prices;
 
	rowMiss = countmiss(x, "ROW"); /* returns 0,1,2 or 3 for each row */
	print rowmiss;
	create nmiss_row var {rowmiss};
	append;
	close nmiss_row;
quit;
proc freq data=nmiss_row;
	table rowmiss;
run;
/*from this we see the missing structure is even messier than expected*/
/*see what's happening with the observations with missing values*/
data miss_see;
	set prices;
	if missing(_10_1_2017) then output;
run;
/*rows 405, 431, 436,...*/
/*(18839,S), (22273,46 2/3), and (12340,L)*/
data see_items;
	set items;
	if pid in (18839,22273,12340) then output;
run;
/*(12340,L) already exists in the items, it's no new*/
/*hard to tell whether that is a new item*/
/*hard to tell the missing price is due to no entry or no trading*/
/*we can just assume there is no reason and carry on the analysis*/

/*So my questions are:
1. isit appropriate to rearrange the size?
2. how do we know the items are new?*/

/*create some new variables:
1. age
2. stock vs sell*/

/*age is in unit of days, the obseced date-starting date*/
