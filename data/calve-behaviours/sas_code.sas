
options formdlim='-';

proc import datafile="U:\2013_1\RA\pinkeye\naty\final_red2.csv" out=mydata   replace;
run;


data mydata2;
set mydata;
ofs=log(time_obs);
run;

  
/*Model with a negative binomial distribution*/
/*total number of combining pain variables (head shaking, head rubbing, head scratchig)*/
ods pdf file='All_models.pdf';
proc glimmix data=mydata2;
class  ID Trial_Day Trial Treatment_2 ;
model tn_oc_pain = Trial_Day | Treatment_2/ dist =nb offset = ofs;
random ID(Treatment_2) Trial;
lsmeans Treatment_2*Trial_Day/cl ilink adjust=Tukey ;
run; 


proc import datafile="U:\2013_1\RA\pinkeye\naty\final.red.cont2.csv" out=contdat   replace;
run;


data contdat2;
set contdat;
l_td_lying_rf=log(td_lying_rf);
ofs=log(time_obs);
run;



/*Sickness Hypotheses: Natural standing*/

proc glimmix data=contdat2  ;
class  ID Trial_Day Trial Treatment_2 ;
model  l_td_lying_rf = Trial_Day | Treatment_2 / dist =n offset = ofs ;
random ID(Treatment_2) Trial;
lsmeans Treatment_2*Trial_Day / cl pdiff adjust=Tukey ;
output out=p;
run; 




data contdat4;
set contdat2;
l_td_Feeding=log(td_Feeding);
ofs=log(time_obs);
run;

/* Feeding*/


proc glimmix data=contdat4 ;
class  ID Trial_Day Trial Treatment_2 ;
model  l_td_Feeding= Trial_Day | Treatment_2 / dist = n offset = ofs;
random ID(Treatment_2) Trial ;
lsmeans Treatment_2*Trial_Day / cl pdiff ilink adjust=Tukey ;
run; 


data contdat5;
set contdat2;
l_td_Standing1 =log(td_Standing1);
ofs=log(time_obs);
run;

/* Standing 1 */
proc glimmix data=contdat5 ;
class  ID Trial_Day Trial Treatment_2 ;
model  l_td_Standing1 = Trial_Day | Treatment_2 / dist = n offset = ofs;
random ID(Treatment_2) Trial ;
lsmeans Treatment_2*Trial_Day / cl pdiff adjust=Tukey ;
run; 



/*Lying 1, trial as fixed */
data contdat6;
set contdat2;
l_td_Lying1 =log(td_Lying1);
ofs=log(time_obs);
run;

proc glimmix data=contdat6 ;
class  ID Trial_Day Trial Treatment_2 ;
model  l_td_Lying1 = Trial_Day | Treatment_2 Trial/ dist = n offset = ofs;
random ID(Treatment_2) ;
lsmeans Treatment_2*Trial_Day / cl pdiff adjust=Tukey ;
run; 



/*Head rubbing, trial as fixed */

data contdat7;
set contdat;
l_td_head_rub =log(td_head_rub);
ofs=log(time_obs_rub);
run;


proc glimmix data=contdat7;
class  ID Trial_Day Trial Treatment_2 ;
model  l_td_head_rub = Trial_Day | Treatment_2 Trial / dist = n offset = ofs;
random ID(Treatment_2)  ;
lsmeans Treatment_2*Trial_Day / cl pdiff adjust=Tukey ;
run; 

ods pdf close;





