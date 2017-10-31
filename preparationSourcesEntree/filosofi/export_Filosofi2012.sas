/*Source utilisée : source fiscale  FILOSOFI2012 */
options mprint mlogic notes;

libname filosofi "X:\HAB-FILOSOFI-PROD\BASES12\N_infra" access=readonly; 


data filo;
set filosofi.N_revdisp12  (keep = x	y reg16	depcom	nbpers_D1 nbpers_D10 nbpersm i_pauvre60	nivviem	oprdec revdecm revdispm taillem typmenr);
where x>0; /*on ne conserve que les menages geolocalises*/

/*definition des variables*/
pop = nbpersm;
men = 1 ;
if typmenr = "1" then  m_mono = 1 ; else m_mono = 0;
if (typmenr = "1" and i_pauvre60 = 1) then  m_mono_pa = 1 ; else m_mono_pa = 0;
if typmenr = "2" then m_cossenf = 1; else m_cossenf = 0;
if typmenr = "3" then m_coenf = 1; else m_coenf = 0;
if typmenr = "5" then m_fseul = 1; else m_fseul = 0;
if typmenr = "6" then m_hseul = 1; else m_hseul = 0;
if taillem ="5" then m_5p = 1; else m_5p = 0;
if ( taillem ="5" and i_pauvre60 = 1) then m_5p_pa = 1; else m_5p_pa = 0;
if oprdec = "1" then m_rev_sal = 1; else m_rev_sal = 0;
if oprdec = "2" then m_rev_cho = 1; else m_rev_cho = 0;
if oprdec = "3" then m_rev_nsa = 1; else m_rev_nsa = 0;
if oprdec = "4" then m_rev_ret = 1; else m_rev_ret = 0;
pop_pa = nbpersm  * i_pauvre60;
pop_d1 = nbpers_D1;
pop_d10 = nbpers_D10;

run;

/*calcul des quintiles*/
PROC UNIVARIATE DATA=filo;
VAR nivviem ;
WEIGHT nbpersm;
WHERE revdispm>=0;
OUTPUT OUT=quintiles pctlpts= 0 to 100 by 20  pctlpre=P;
run;

proc transpose data = quintiles out=test;
run;

data _NULL_;
set test;
CALL SYMPUT(COMPRESS("quintile"||_N_ -1),col1);
run;

%put &quintile1;
%put &quintile2;
%put &quintile3;
%put &quintile4;
%put &quintile5;

/*calcul de la population appartenant a chaque quintile */
data filo;
set filo;
if nivviem>=0 and nivviem <= &quintile1. then i_quintile1 = 1; else i_quintile1=0;
if nivviem>&quintile1.  and nivviem <= &quintile2. then i_quintile2 = 1; else i_quintile2=0;
if nivviem>&quintile2.  and nivviem <= &quintile3. then i_quintile3 = 1; else i_quintile3=0;
if nivviem>&quintile3.  and nivviem <= &quintile4. then i_quintile4 = 1; else i_quintile4=0;
if nivviem>&quintile4.  then i_quintile5 = 1; else i_quintile5=0;
pop_q1 = nbpersm * i_quintile1;
pop_q2 = nbpersm * i_quintile2;
pop_q3 = nbpersm * i_quintile3;
pop_q4 = nbpersm * i_quintile4;
pop_q5 = nbpersm * i_quintile5;
run;


/*base finale*/
data filo_def;
set filo (keep = x y depcom REG16 pop men m_mono m_mono_pa	m_cossenf	m_coenf	m_fseul	m_hseul	
				m_5p m_5p_pa m_rev_sal m_rev_cho m_rev_nsa m_rev_ret pop_pa	nivviem	revdecm	revdispm	
				pop_d1	pop_d10	pop_q1	pop_q2	pop_q3	pop_q4	pop_q5);
run;


 
proc contents data=filo_def short;
run;
