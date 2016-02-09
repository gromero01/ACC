/**************************************************************************************************
* * lectBases.SAS
* * AUTOR: Jorge Mario Carrasco Ortiz
* * 
* * SABER 3° 5° 9° 
* * DESCRIPCIÓN: Construcción de scripts para cargar en libreria sas todos los insumos
* * 
* * INPUTS: Bases creadas por equipo de CC de 3° 5° 9°, se debe definir los parametros 
* *		    en el archivo "../parametros.sas"
* * 
* * OUTPUTS: Bases leidas en sas y archivos .sas7bdat almacenados en BASES/sas
* * 
* * FILE HISTORY:
* * 	20150306: Creación de script
* *
* * ToDo: 
* *       
**************************************************************************************************/
/**************************************************************************************************
* * LIBRERIAS PARA GUARDAR LA INFORMACIÓN DE ENTRADA
**************************************************************************************************/
%LET PATH_PARAM = &sysparm.; 
%include "&PATH_PARAM";
*%include 'C:\Users\jorge\Desktop\20141_FICHA_TECNICA_CC\parametros.sas';
*%include 'C:\PRODUCTO_JMC_2015\20141_FICHA_TECNICA_CC\parametros.sas';
LIBNAME INPATH "&PATH_PROJECT\BASES\SAS";
LIBNAME FT "&PATH_PROJECT\OUTPUT\SAS";

%put "------------------------------ Parametros de corrida ------------------------------";
%put &BASE_PATH_COG.;
%put &BASE_PATH.;
%put &GLOBAL_ANO.;
%put &PATH_PROJECT.;
%put &INDICADORAS_PATH.;
%put &PESOS_PATH.;
%put &CODIFICA_PATH.;
%put "-----------------------------------------------------------------------------------";

/**************************************************************************************************
* * Lectura de bases infoEstab, BibliaRetorno, matricula, indicadoras, copia, pesosCensal, copia
**************************************************************************************************/

data INPATH.copia;
     %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
     infile "&BASE_PATH_COG/copia.txt" 
		delimiter='09'x MISSOVER DSD lrecl=32767 firstobs=2 ;
        informat consLect $8. ;				format consLect $8. ;			
        informat copia best32. ;			format copia best12. ;			
        informat copiaC best32. ;			format copiaC best12. ;			
        informat copiaD best32. ;			format copiaD best12. ;
        informat copiaF best32. ;			format copiaF best12. ;			
        informat copiaI best32. ;			format copiaI best12. ;				
        informat copiaL best32. ;			format copiaL best12. ;			
        informat copiaM best32. ;			format copiaM best12. ;			
     input
                 consLect $
                 copia
                 copiaC
                 copiaD
				 copiaF
				 copiaI
                 copiaL
                 copiaM
     ;
     if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
RUN;

data INPATH.infoEstab; * Lectura de informacion establecimientos;
     %let _EFIERR_ = 0; 
     infile "&BASE_PATH_COG\infoEstab.txt"
		delimiter='09'x MISSOVER DSD lrecl=32767 firstobs=2 ;
        informat daneEstab $200. ;		format daneEstab $200. ;		
        informat sector $1. ;			format sector $1. ;	
        informat zonaEstab $1. ;		format zonaEstab $1. ;		
        informat sZEstab $1. ;			format sZEstab $1. ;	
        informat enteId $2. ;			format enteId $2. ;	
        informat mpio $5. ;				format mpio $5. ;
        informat nse $1. ;				format nse $1. ;
        informat nombre $55. ;			format nombre $55. ;
        informat direccion $18. ;		format direccion $18. ;	
		informat calendario $1. ;		format calendario $1. ;
     input
                 daneEstab $
                 sector $
                 zonaEstab $
                 sZEstab $
                 enteId $
                 mpio $
                 nse $
                 nombre $
                 direccion $
				 calendario $
     ;
     if _ERROR_ then call symputx('_EFIERR_',1);  
run;
data INPATH.infoSejos; * Lectura de informacion de sejos;
   %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
     infile "&BASE_PATH_COG\infoSejos.txt" delimiter = '09'x MISSOVER DSD lrecl=32767 firstobs=2  ;
			informat daneSede $200. ;				format daneSede $200. ;
			informat daneEstab $200. ;				format daneEstab $200. ;
			informat codSitio $200. ;				format codSitio $200. ;
			informat jornada $200. ;				format jornada $200. ;
			informat sector $200. ;				format sector $200. ;
			informat zona $200. ;				format zona $200. ;
			informat sZ $200. ;				format sZ $200. ;
			informat mpio $200. ;				format mpio $200. ;
			informat enteId $200. ;				format enteId $200. ;
			informat nombre $200. ;				format nombre $200. ;
			informat direccion $200. ;				format direccion $200. ;
	 input
			daneSede $
			daneEstab $
			codSitio $
			jornada $
			sector $
			zona $
			sZ $
			mpio$
			enteId $
			nombre $
			direccion $
;
     if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
run;

data INPATH.bibliaRetorno; * Lectura de Biblia de Retorno;
	%let _EFIERR_ = 0; /* set the ERROR detection macro variable */
	infile "&BASE_PATH_COG\bibliaRetorno.txt"
	delimiter='09'x MISSOVER DSD lrecl=32767 firstobs=2 ;
		informat depto $2. ;				format depto $2. ;		
		informat mpio $5. ;					format mpio $5. ;	
		informat daneEstab $200. ;			format daneEstab $200. ;			
		informat codSitio $7. ;				format codSitio $7. ;		
		informat codSaber $9. ;				format codSaber $9. ;	
		informat tipoApli $1. ;				format tipoApli $1. ;		
		informat snp $8. ;					format snp $8. ;	
		informat consLect $8. ;				format consLect $8. ;		
		informat grado best32. ;				format grado best12. ;		
		/*informat presente $1. ;				format presente $1. ;		*/
		informat grupo $2. ;				format grupo $2. ;		
		informat disCog best32. ;				format disCog best12. ;		
		informat disSen  $1. ;				format disSen  $1. ;		
		informat disFis  $1. ;				format disFis  $1. ;		
		informat nivAgre $8. ;				format nivAgre $8. ;		
	input
	            depto $
	            mpio $
	            daneEstab $
	            codSitio $
	            codSaber $
	            tipoApli $
	            snp $
	            consLect $
	            grado $
				/*presente $*/
	            grupo $
	            disCog $
	            disSen $
	            disFis $
	            nivAgre $		
	;
run;

data INPATH.matricula; * Lectura de matricula;
    %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
    infile "&BASE_PATH_COG\matricula.txt" delimiter='09'x MISSOVER DSD
	lrecl=32767 firstobs=2 ;
       informat daneEstab $200. ;			format daneEstab $200. ;
       informat codSitio $6. ;				format codSitio $6. ;
       informat grado best32. ;					format grado best12. ;
       informat grupo $2. ;					format grupo $2. ;
       informat n best32. ;					format n best12. ;
       /*informat ind $1. ;					format ind $1. ;
	   informat nivAgre $8. ;				format nivAgre $8. ;			*/
    input
                daneEstab $
                codSitio $
                grado $
                grupo $
                n
    ;
    if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
RUN;

data INPATH.codificaIND; * Lectura de Codificacion de indices;
     %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
     infile "&BASE_PATH/&CODIFICA_PATH" 
		delimiter='09'x MISSOVER DSD lrecl=32767 firstobs=2 ;
        informat NOMBRE  $20.;	informat NOMBRE  $20.;
		informat cod_escala  $3.;	informat cod_escala  $3.;
		informat cod_item  $4.;	informat cod_item  $4.;
		informat grado5  $1.;	informat grado5  $1.;
		informat grado9  $1.;	informat grado9  $1.;
     input
        NOMBRE 
		cod_escala 
		cod_item 
		grado5 
		grado9 
     ;
     if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
RUN;

data INPATH.ficha_tecnica_COG; * Lectura de Ficha Tecnica Cognitivo;
     %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
     infile "&FTC_PATH" 
		delimiter='09'x MISSOVER DSD lrecl=32767 firstobs=2 ;
       informat id best32. ;					format id best12. ;
       informat anio $4.;       format  anio $4.;
       informat aplicacion $1.;       format  aplicacion $1.;
       informat modo $1.;       format  modo $1.;
       informat id_ent_territorial $5.;       format  id_ent_territorial $5.;
       informat id_institucion $12.;       format  id_institucion $12.;
       informat id_sede $6.;       format  id_sede $6.;
       informat grado $4.;       format  grado $4.;
       informat presentes $6.;       format  presentes $6.;
       informat ninos_presentes $6.;       format  ninos_presentes $6.;
       informat ninas_presentes $6.;       format  ninas_presentes $6.;
       informat noespecifica_presentes $5.;       format  noespecifica_presentes $5.;
       informat lenguaje $6.;       format  lenguaje $6.;
       informat matematicas $6.;       format  matematicas $6.;
       informat ciencias $4.;       format  ciencias $4.;
       informat compciud $6.;       format  compciud $6.;
       informat ausentes $6.;       format  ausentes $6.;
       informat confiable_l $4.;       format  confiable_l $4.;
       informat confiable_m $4.;       format  confiable_m $4.;
       informat confiable_c $4.;       format  confiable_c $4.;
       informat confiable_d $4.;       format  confiable_d $4.;
       informat copia_eval_l $5.;       format  copia_eval_l $5.;
       informat copia_eval_m $5.;       format  copia_eval_m $5.;
       informat copia_eval_c $4.;       format  copia_eval_c $4.;
       informat copia_eval_d $5.;       format  copia_eval_d $5.;
       informat copia_sede_l $4.;       format  copia_sede_l $4.;
       informat copia_sede_m $4.;       format  copia_sede_m $4.;
       informat copia_sede_c $4.;       format  copia_sede_c $4.;
       informat copia_sede_d $4.;       format  copia_sede_d $4.;
       informat copia_inst_l $4.;       format  copia_inst_l $4.;
       informat copia_inst_m $4.;       format  copia_inst_m $4.;
       informat copia_inst_c $4.;       format  copia_inst_c $4.;
       informat copia_inst_d $4.;       format  copia_inst_d $4.;
       informat inst_oficiales $5.;       format  inst_oficiales $5.;
       informat inst_no_oficiales $4.;       format  inst_no_oficiales $4.;
       informat inst_urbanos $4.;       format  inst_urbanos $4.;
       informat inst_rurales $4.;       format  inst_rurales $4.;
       informat inst_nse1 $4.;       format  inst_nse1 $4.;
       informat inst_nse2 $4.;       format  inst_nse2 $4.;
       informat inst_nse3 $4.;       format  inst_nse3 $4.;
       informat inst_nse4 $4.;       format  inst_nse4 $4.;
       informat estud_oficiales $6.;       format  estud_oficiales $6.;
       informat estud_no_oficiales $6.;       format  estud_no_oficiales $6.;
       informat estud_urbanos $6.;       format  estud_urbanos $6.;
       informat estud_rurales $6.;       format  estud_rurales $6.;
       informat estud_nse1 $6.;       format  estud_nse1 $6.;
       informat estud_nse2 $6.;       format  estud_nse2 $6.;
       informat estud_nse3 $6.;       format  estud_nse3 $6.;
       informat estud_nse4 $6.;       format  estud_nse4 $6.;
       informat inst_of_urb $4.;       format  inst_of_urb $4.;
       informat inst_of_rur $4.;       format  inst_of_rur $4.;
       informat inst_noof_urb $4.;       format  inst_noof_urb $4.;
       informat inst_noof_rur $4.;       format  inst_noof_rur $4.;
       informat estud_of_urb $6.;       format  estud_of_urb $6.;
       informat estud_of_rur $6.;       format  estud_of_rur $6.;
       informat estud_noof_urb $6.;       format  estud_noof_urb $6.;
       informat estud_noof_rur $4.;       format  estud_noof_rur $4.;
       informat tipo_reporte_l $4.;       format  tipo_reporte_l $4.;
       informat tipo_reporte_m $4.;       format  tipo_reporte_m $4.;
       informat tipo_reporte_c $4.;       format  tipo_reporte_c $4.;
       informat tipo_reporte_d $4.;       format  tipo_reporte_d $4.;
    input
                id 
                anio $
                aplicacion $
                modo $
                id_ent_territorial $
                id_institucion $
                id_sede $
                grado $
                presentes $
                ninos_presentes $
                ninas_presentes $
                noespecifica_presentes $
                lenguaje $
                matematicas $
                ciencias $
                compciud $
                ausentes $
                confiable_l $
                confiable_m $
                confiable_c $
                confiable_d $
                copia_eval_l $
                copia_eval_m $
                copia_eval_c $
                copia_eval_d $
                copia_sede_l $
                copia_sede_m $
                copia_sede_c $
                copia_sede_d $
                copia_inst_l $
                copia_inst_m $
                copia_inst_c $
                copia_inst_d $
                inst_oficiales $
                inst_no_oficiales $
                inst_urbanos $
                inst_rurales $
                inst_nse1 $
                inst_nse2 $
                inst_nse3 $
                inst_nse4 $
                estud_oficiales $
                estud_no_oficiales $
                estud_urbanos $
                estud_rurales $
                estud_nse1 $
                estud_nse2 $
                estud_nse3 $
                estud_nse4 $
                inst_of_urb $
                inst_of_rur $
                inst_noof_urb $
                inst_noof_rur $
                estud_of_urb $
                estud_of_rur $
                estud_noof_urb $
                estud_noof_rur $
                tipo_reporte_l $
                tipo_reporte_m $
                tipo_reporte_d $
                tipo_reporte_c $
     ;
     if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
RUN;
PROC IMPORT OUT=INPATH.IndicadorasCC /*Importacion indicadoras de No Cognitivo (3 5 9)*/
            DATAFILE= "&BASE_PATH/&INDICADORAS_PATH" 
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
PROC IMPORT OUT= INPATH.PesosCC /*Importacion de pescos de No Cognitivo (3 5 9)*/
            DATAFILE= "&BASE_PATH/&PESOS_PATH" 
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
PROC SQL; *Correcion Formato de Indicadoras y Pesos;

/* Creando indices Unicos */
create table inpath.ident as select distinct codSitio, codSaber, daneEstab, depto
from inpath.bibliaRetorno;

/* Exclusion */
delete from INPATH.IndicadorasCC where grado = 3;
delete from INPATH.PesosCC where grado = 3;
delete from INPATH.Bibliaretorno where grado = 3;
delete from INPATH.Matricula where grado = 3;

alter table INPATH.IndicadorasCC add consLect_aux char(8) format=$8.;
update INPATH.IndicadorasCC set consLect_aux = strip(put(consLect, z8.));
alter table INPATH.IndicadorasCC drop COLUMN consLect;

/*Cambiando tipos para Pesos*/
alter table INPATH.PesosCC add consLect_aux char(8) format=$8.;
update INPATH.PesosCC set consLect_aux = strip(put(consLect, z8.));
alter table INPATH.PesosCC drop COLUMN consLect;

quit;
PROC SORT DATA = INPATH.ficha_tecnica_COG; 
	BY id_ent_territorial id_institucion id_sede;
run;
PROC SORT DATA = INPATH.PesosCC; 
	BY consLect_aux;
run;
PROC SORT DATA = INPATH.Codificaind;
	BY cod_escala;
run;
PROC SORT DATA = INPATH.indicadorasCC;
	BY consLect_aux;
run;
PROC SORT DATA = INPATH.bibliaRetorno;
	BY consLect;
run;

PROC SORT DATA = INPATH.infoEstab;
	BY daneEstab;
run;
PROC SORT DATA = INPATH.infoSejos;
	BY daneEstab daneSede;
run;
PROC SORT DATA = INPATH.matricula;
	BY daneEstab codSitio;
run;

PROC SQL; *Tablas Iniciales;

/* Consolidando matricula a nivel Grado */
create table inpath.matriculaG as 
select codSitio as codSitioM, grado as gradoM, daneEstab as daneEstabM, 
       sum(n) as n_grado
from (select * from inpath.matricula where grado ^= 3)
group by codSitioM, gradoM, daneEstabM;

/*  recuperando codSaber en matrícula */
create table inpath.matriculaGrade as select m.*, i.codSaber
from inpath.matriculaG m left join inpath.ident i 
	on compress(m.codSitioM || m.daneEstabM) = compress(i.codSitio || i.daneEstab);

/* Errores sede-grado sin CodSaber (No se encuentran en biblia)*/
create table inpath.hallazgosMG as select *
from inpath.matriculaGrade where codSaber = '';

*create table p3 as select * from inpath.IndicadorasCC where consLect = 01002991;
*create table p4 as select * from inpath.pesosCC where consLect = 01002991;

/* Cruzando indicadoras, pesos y biblia*/
create table INPATH.cruceTodo as select i.consLect_aux as consLect_IND, p.consLect_aux as consLect_PES, i.*, p.*, b.*
from inpath.Indicadorascc i
full join inpath.pesosCC p on (i.consLect_aux = p.consLect_aux)
full join inpath.bibliaRetorno b on (i.consLect_aux = b.consLect);

/* guardando posibles errores en el cruce*/
create table inpath.hallazgosIniciales as select *
from INPATH.cruceTodo sp where consLect = '' or consLect_PES = '' or consLect_IND = '';

/*guardando auqellos que cruzan */
delete from INPATH.cruceTodo where consLect = '' or consLect_PES = '' or consLect_IND = '';

/*  lugares con matrícula. */
create table inpath.saber2 as 
select s.codSaber, s.DaneEstab, s.grado
from INPATH.cruceTodo s 
INTERSECT ALL 
select mg.codSaber, mg.DaneEstabm, mg.gradom
from inpath.matriculaGrade mg;

/* Creando tabla de informacion */

create table inpath.filtroEstab as select distinct daneEstab, sector, zonaEstab, sZEstab, enteId, mpio, nse
from Inpath.infoestab;

create table inpath.INFO as 
select * from inpath.saber2 p left join INPATH.cruceTodo q
on compress(p.codSaber||p.DaneEstab||put(p.grado, 1.)) = compress(q.codSaber||q.DaneEstab||put(q.grado, 1.))
left join inpath.filtroEstab r 
on compress(p.daneEstab) = compress(r.daneEstab);

/* No cruzaron por CodSABER - CodSitio*/
create table inpath.hallazgosEliminados as select * from inpath.crucetodo where
consLect_IND not in (select consLect_IND from inpath.INFO) and 
(pesoACDISC ^= 0 or pesoACTAGR ^= 0 or pesoACTCORR ^= 0 or pesoACTDEM ^= 0 or pesoACTDIVIT01 ^= 0 
or pesoACTDIVIT02 ^= 0 or pesoACTDIVIT03 ^= 0 or pesoACTDIVIT04 ^= 0 or pesoACTDIVIT05 ^= 0 
or pesoACTDIVIT06 ^= 0 or pesoACTGEN ^= 0 or pesoACTPAR ^= 0 or pesoASERT ^= 0 
or pesoDESLEY ^= 0 or pesoEMPCONV ^= 0 or pesoMANRAB ^= 0 or pesoOPORPART ^= 0 
or pesoPERSEGIT01 ^= 0 or pesoPERSEGIT02 ^= 0 or pesoPERSEGIT03 ^= 0 or pesoPERSEGIT04 ^= 0 
or pesoROLESIT01 ^= 0 or pesoROLESIT02 ^= 0 or pesoROLESIT03 ^= 0 or pesoVICAGR2IT01 ^= 0 
or pesoVICAGR2IT02 ^= 0 or pesoVICAGR2IT03 ^= 0);

select COUNT(*) INTO:ninfo FROM inpath.INFO where grado = 5;
select COUNT(*) INTO:nsaber FROM INPATH.cruceTodo  where grado = 5;
%put "O_O-------- Numero de registros a agrupar :&ninfo de &nsaber (para 5)";
select COUNT(*) INTO:ninfo FROM inpath.INFO where grado = 9;
select COUNT(*) INTO:nsaber FROM INPATH.cruceTodo  where grado = 9;
%put "O_O-------- Numero de registros a agrupar :&ninfo de &nsaber (para 9)";

/* Errores encontrando cruzando con infoEstab*/
create table inpath.hallazgosINFO as select *
from inpath.INFO where consLect = '' or enteId = '';
quit;

PROC DATASETS library=INPATH;
   modify Info;
      index create key_sede = (daneEstab codSitio grado);
      index create key_estab = (daneEstab grado);
	  index create key_muni = (mpio grado);
	  index create key_dep = (depto grado);
	  index create key_Ente = (enteId grado);
	  index create grado;
run;

PROC SQL;

/* Creando base de ficha tecnica y establecimientos pequenos + Empty Data*/
*select name, type, length from dictionary.columns where libname = 'FT' and memname = 'FICHATE';
*select name, type, length from dictionary.columns where libname = 'INPATH' and memname = 'FEVALUACION';
*select * from dictionary.columns where libname = 'FT' and memname = 'FICHATE';

quit;
