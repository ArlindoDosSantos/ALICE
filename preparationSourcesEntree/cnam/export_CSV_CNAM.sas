/* Exportation des données CNAM du format SAS vers cvs */
options mprint mlogic notes;
options nofmterr;

libname cofCNAM "X:\HAB-Infracommunal\CNAM\2016\6-retour_partenaire" access=readonly; 

data cnam2016;
	set cofCNAM.adr_don_cnam_2016  (keep = nais sexe x93 y93 reg qualitexy);
run;

PROC EXPORT DATA= WORK.cnam2016 
            OUTFILE= "V:\PSAR-AU\ADS\CNAM\cnam2016.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
