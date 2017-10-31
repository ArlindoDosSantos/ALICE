/********************************************************************/
/* CAF */
options mprint mlogic notes;
options nofmterr;

libname coffrCaf "X:\HAB-Infracommunal\CAF\2015" access=readonly; 

data caf2015;
	set coffrCaf.Frecinseev2b  (keep = AAHVERS APLVERS CATBEN DTNAIRES MTDEREVB MTPFVERS MTPRERUC NBENLEFA NBUC NUMCOMDO PERSCOUV PRESCONJ RSAACT RSASOCL RSAVERS RUCDERRE TPPOPRUC NUMCOCI GEOXL2E GEOYL2E QUALXY TPACCUM);	                                   
run;

PROC EXPORT DATA= WORK.Caf2015 
            OUTFILE= "V:\PSAR-AU\ADS\CAF\cnaf2015.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

/********************************************************************/
/* pop filosofi 2013 */
options mprint mlogic notes;

libname filosofi "X:\HAB-FILOSOFI-PROD\BASES13\N_infra" access=readonly; 

data filo2013Pop;
	set filosofi.N_revdec13  (keep = nbpersm depcom x y nbpersm occtypR);
run;

PROC EXPORT DATA= WORK.filo2013Pop 
            OUTFILE= "V:\PSAR-AU\ADS\CAF\filosofi2013Occupation.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
