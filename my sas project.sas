/* define date format  */
options datestyle=dmy;

filename retail "/folders/myfolders/Online Retail.csv";

proc import file= 'retail' dbms=csv out= online_retail replace;
run;

/* data step to input file in sas */
data online_retail;
infile retail dlm=',' dsd missover firstobs=2;
input Invoice_No :$10.
Product_Id :$10.
Product_Name :$100.
Quantity
Invoice_DateTime	
Rate	
Customer_ID :$10.
Country :$50.
;
informat Invoice_DateTime anydtdtm.;
format Invoice_DateTime datetime.;
run;

/* dsd means do not consider 2 consecutive delimiter as one,consider as missing value */
/*missover means do not jump to next line when the last value is a missing value */
/* firstobs points starting point,here 2 means start from 2nd row */

/* data step to create the transaction table and seperate date and time */
data online_transaction;
set online_retail;
format Invoice_Date date9. Invoice_Time time5.;
keep Invoice_no Product_ID Customer_Id Quantity Rate Invoice_Date Invoice_Time;
Invoice_Date = datepart(Invoice_DateTime);
Invoice_Time = timepart(Invoice_DateTime);
run;

/* proc code to create table for customer information */
proc sql;
create table Customer_Detail as
select distinct(customer_id), country 
from online_retail;
quit;

/* since the customer data is ambiguos we will not be creating a dimension table for customer */
proc datasets lib=work;
delete customer_detail;
run;

/* recreating transaction table with country info */
data online_transaction;
set online_retail;
format Invoice_Date date9. Invoice_Time time5.;
keep Invoice_no Product_ID Customer_Id Quantity Rate Country Invoice_Date Invoice_Time;
Invoice_Date = datepart(Invoice_DateTime);
Invoice_Time = timepart(Invoice_DateTime);
run;

/*proc code to create table for product information  */
proc sql;
create table Product_Detail as
select distinct(Product_id), Product_Name
from online_retail;
quit;

/* fix data issue in product table */
proc sort data=product_detail;
by product_id;
run;
data product_detail_1 error_table;
set product_detail;
by product_id;
length_of_name=length(product_name);
if length_of_name>10 or (length_of_name<=10 and first.product_id=last.product_id) 
then output product_detail_1;
else output error_table;
run;

proc print data=error_table;
run;

/* review the error product names */
proc sql;
create table error_table_summary as
select distinct(product_name) from error_table;
quit;

/* find other error records */
data product_detail_1;
set product_detail_1;
product_id=upcase(product_id);
run;

proc sort data=product_detail_1 out=product_detail_2 dupout=possible_error nodupkey;
by product_id;
run;

/* make the final table as the product fact table */
data product_detail(drop=length_of_name);
set product_detail_2;
run;

/*-----------------------------------/* fact table created */----------------------------------/*

/* summarise the quantity of the products on the country */

proc sql;
create table report1 as 
select sum(Quantity) as total_quantity, product_id, country from online_transaction
group by product_id,country;
quit;

/* Get the product with maximum quantity in each country  */

proc sql;
create table max_quantity_table_1 as
select country,product_id,total_quantity
from report1
group by country
having total_quantity = max(total_quantity);
run;

proc sort data= max_quantity_table_1;
by product_id;
run;

/* merge with product table to get product name  */
 
data maximum_product_country_wise;
merge max_quantity_table_1 (in=a) product_detail(in=b);
by product_id;
if a;
run;
/* if a; means matching and non-matching of first table and matching of second table  */
/* for merge 'by' some variable the conditon is the tables should be sorted according to that variable */


/* get the product with min quantity in each country */
proc sql;
create table minimum_product_country_wise_1 as
select Country,product_id,total_quantity
from report1
group by country
having total_quantity=min(total_quantity);
quit;

proc sort data=minimum_product_country_wise_1;
by product_id;
run;

/* merge with product table to get the product names */
data minimum_product_country_wise;
merge minimum_product_country_wise_1 (in=a) product_detail(in=b);
by product_id;
if a;
run;

/* create table with total amount, year and month information */
data online_transaction_1;
set online_transaction;
if quantity=. then quantity=0;
if rate=. then rate=0;
Total_Amount=quantity*rate;
Year_of_Sale = year(Invoice_Date);
Month_Of_Sale = Month(Invoice_Date);
run;

/*  EXTRA */
/* report to show the  month-wise revenue */
proc sql;
create table Month_wise_Revenue as
select Year_Of_Sale, Month_Of_Sale, sum(total_Amount) as Total_Revenue
from online_transaction_1
group by year_of_sale,month_of_sale;
quit;

/* report to show the month-wise revenue for each country */
proc sql;
create table Country_Month_wise_Revenue as
select Year_Of_Sale, Month_Of_Sale, Country, sum(total_Amount) as Total_Revenue
from online_transaction_1
group by year_of_sale,month_of_sale,country;
quit;

proc sort data=country_month_wise_revenue;
by descending total_revenue country;
run;

/* -----------------------completed first reporting------------------ */

proc contents data=online_transaction_1;
run;

/* get the total number of visits by each customer as per the number of bills */
proc sql;
create table report_3 as
select Customer_Id, count(Invoice_no) as Number_Of_Visits
from online_transaction_1
group by customer_Id
having customer_id <>''
order by number_of_visits desc;
run;

/* arrange the report to show the customer with most visit on the top */
proc sort data=report_3;
by descending number_of_visits;
run;

/* get the total number of customer from the table */
proc sql noprint;
select count(*) into :total_cust from report_3;
quit;

%put Total Customers = &total_cust;

/* create a table to group the customers in 3 parts and assign them their groups */
data most_loyal;
set report_3 ;
total_cust=&total_cust.;
cut_off_value=total_cust/3;
if _n_ <=cut_off_value then group=1;
else if _n_ >cut_off_value and _n_ <=(cut_off_value*2) then group=2;
else group=3;
run;

proc sort data=most_loyal;
by descending group;
run;

/*----------------------------------------------------------------------------*/

/* extract data for the last 2 months only */

data report_4;
set online_transaction_1;
where year_of_sale=2011 and month_of_sale in (11,12);
run;


/* get the count of visits for each customer based on the number of invoices */
proc sql;
create table report_4_1 as
select Customer_Id, count(Invoice_no) as Number_Of_Visits
from report_4
group by customer_Id
having customer_id <>'';
run;

/* arrange the report to show the customer with most visit on the top */
proc sort data=report_4_1;
by descending number_of_visits;
run;

/* get the total number of customer from the table */
proc sql;
select count(*) into :total_cust from report_4_1;
quit;

%put Total Customers = &total_cust;

/* create a table to group the customers in 3 parts and assign them their groups */
data most_frequent;
set report_4_1;
total_cust=&total_cust.;
cut_off_value=total_cust/3;
if _n_ <=cut_off_value then group=1;
else if _n_ >cut_off_value and _n_ <=(cut_off_value*2) then group=2;
else group=3;
run;

/*--------------------------------------------------------------------------  */

/* select the date of reporting */
%let date_of_reporting='08DEC2011'd;
%put &date_of_reporting.;

/* get the value of the weekday */
data _null_;
date_of_reporting=&date_of_reporting;
day_of_reporting = put(date_of_reporting,weekdate9.);
call symput('day_of_reporting',day_of_reporting);
run;
%put &day_of_reporting;

/* create the table with the data from the said week */
data report_5(keep=invoice_date total_amount start_date end_date reduce date_of_reporting);
set online_transaction_1;
format date_of_reporting date9. start_date date9. end_date date9.;
date_of_reporting=&date_of_reporting;
if &day_of_reporting=Sunday then do; reduce=-7; end;
if &day_of_reporting=Monday then do; reduce=-8;end;
if &day_of_reporting=Tuesday then do; reduce=-9; end;
if &day_of_reporting=Wednesday then do; reduce=-10; end;
if &day_of_reporting=Thursday then do; reduce=-11; end;
if &day_of_reporting=Friday then do; reduce=-12; end;
if &day_of_reporting=Saturday then do; reduce=-13; end;

start_date = intnx('Day',date_of_reporting,reduce);
end_date= intnx('Day',start_date,6);

if Invoice_Date >=start_date and Invoice_Date <=end_date;

run;

/* create the report to get the revenue for each day */
proc sql ;
create table weekday_revenue as
select Invoice_date, sum(total_amount) as total_revenue
from report_5
group by Invoice_Date
order by total_revenue desc;
quit;

proc sql OUTOBS=1;
create table weekday_revenue as
select Invoice_date, sum(total_amount) as total_revenue
from report_5
group by Invoice_Date
order by total_revenue desc;
quit;