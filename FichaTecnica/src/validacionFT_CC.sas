/**************************************************************************************************
* * lectBases.SAS
* * AUTOR: Jorge Mario Carrasco Ortiz
* * 
* * SABER 3° 5° 9° 
* * DESCRIPCIÓN: Validaciones de ficha tecnica y ficha Evaluados
* * 
* * INPUTS: Ficha_Tecnica_Salida proceso SAS y ficha de Evaluados
* *		    
* * 
* * OUTPUTS: Archivos de Salidas en .csv con errores encontrados en Ficha Tecnica
* * 
* * FILE HISTORY:
* * 	20150327: Creación de script
* *
* * ToDo: 
* *       
**************************************************************************************************/
/**************************************************************************************************
* * LIBRERIAS PARA GUARDAR LA INFORMACIÓN DE ENTRADA Y SALIDA
**************************************************************************************************/
%LET PATH_PARAM = &sysparm.; 
%include "&PATH_PARAM";
*%include 'C:\PRODUCTO_JMC_2015\20141_FICHA_TECNICA_CC\parametros.sas';
LIBNAME INPATH "&PATH_PROJECT\BASES\SAS";
LIBNAME FT "&PATH_PROJECT\OUTPUT\SAS";
%put "--------------------------------------------";
%put "Ruta del proyecto: ---- &PATH_PROJECT ---";
%put "Evaluacion leido de: ---- &EVAL_PATH ---";
%put "------------------------------------------------------";

/**************************************************************************************************
* * Lectura de ficha Evaluacion 
**************************************************************************************************/
DATA INPATH.Fevaluacion                                  ;
     %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
     infile "&EVAL_PATH"
 		delimiter='09'x MISSOVER DSD lrecl=32767 firstobs=2 ;
        informat anio BEST12. ;      format anio BEST12. ;
        informat aplicacion BEST12. ;      format aplicacion BEST12. ;
        informat modo $1. ;      format modo $1. ;
        informat id_ent_territorial $6. ;      format id_ent_territorial $6. ;
        informat id_institucion $12. ;      format id_institucion $12. ;
        informat id_sede $4. ;      format id_sede $4. ;
        informat tipo BEST12. ;      format tipo BEST12. ;
        informat subtipo $4. ;      format subtipo $4. ;
        informat cod_escala $3. ;      format cod_escala $3. ;
        informat cod_item $4. ;      format cod_item $4. ;
        informat grado BEST12. ;      format grado BEST12. ;
        informat N BEST12. ;      format N BEST12. ;
        informat N_Estab BEST12. ;      format N_Estab BEST12. ;
        informat puntuacion $6. ;      format puntuacion $6. ;
        informat margen $6. ;      format margen $6. ;
        informat linf $6. ;      format linf $6. ;
        informat lsup $6. ;      format lsup $6. ;
        informat indicadora_EE $5. ;      format indicadora_EE $5. ;
        informat desviacion $5. ;      format desviacion $5. ;
        informat nivel1 $4. ;      format nivel1 $4.;
        informat ErrorEstandarNivel1 $4. ;      format ErrorEstandarNivel1 $4. ;
        informat nivel2 $4. ;      format nivel2 $4. ;
        informat ErrorEstandarNivel2 $4. ;      format ErrorEstandarNivel2 $4. ;
        informat param1_promedio $4. ;      format param1_promedio $4. ;
        informat param2_promedio $4. ;      format param2_promedio $4. ;
        informat param3_promedio $4. ;      format param3_promedio $4. ;
        informat param4_promedio $4. ;      format param4_promedio $4. ;
        informat param5_promedio $4. ;      format param5_promedio $4. ;
        informat param6_promedio $4. ;      format param6_promedio $4. ;
        informat param7_promedio $4. ;      format param7_promedio $4. ;
        informat param8_promedio $4. ;      format param8_promedio $4. ;
        informat param9_promedio $4. ;      format param9_promedio $4. ;
        informat param10_promedio $4. ;      format param10_promedio $4. ;
        informat param11_promedio $4. ;      format param11_promedio $4. ;
        informat param12_promedio $4. ;      format param12_promedio $4. ;
        informat param13_promedio $4. ;      format param13_promedio $4. ;
        informat param14_promedio $4. ;      format param14_promedio $4. ;
        informat param1_desvest $4. ;      format param1_desvest $4. ;
        informat param2_desvest $4. ;      format param2_desvest $4. ;
        informat param3_desvest $4. ;      format param3_desvest $4. ;
        informat param4_desvest $4. ;      format param4_desvest $4. ;
        informat param5_desvest $4. ;      format param5_desvest $4. ;
        informat param6_desvest $4. ;      format param6_desvest $4. ;
        informat param7_desvest $4. ;      format param7_desvest $4. ;
        informat param8_desvest $4. ;      format param8_desvest $4. ;
        informat param9_desvest $4. ;      format param9_desvest $4. ;
        informat param10_desvest $4. ;      format param10_desvest $4. ;
        informat param11_desvest $4. ;      format param11_desvest $4. ;
        informat param12_desvest $4. ;      format param12_desvest $4. ;
        informat param13_desvest $4. ;      format param13_desvest $4. ;
        informat param14_desvest $4. ;      format param14_desvest $4. ;
input
                 anio 
                 aplicacion 
                 modo $
                 id_ent_territorial $
                 id_institucion $
                 id_sede $
                 tipo 
                 subtipo $
                 cod_escala $
                 cod_item $
                 grado 
                 N 
                 N_Estab 
                 puntuacion $
                 margen $
                 linf $
                 lsup $
                 indicadora_EE $
                 desviacion $
                 nivel1 
                 ErrorEstandarNivel1 $
                 nivel2 
                 ErrorEstandarNivel2 $
                 param1_promedio $
                 param2_promedio $
                 param3_promedio $
                 param4_promedio $
                 param5_promedio $
                 param6_promedio $
                 param7_promedio $
                 param8_promedio $
                 param9_promedio $
                 param10_promedio $
                 param11_promedio $
                 param12_promedio $
                 param13_promedio $
                 param14_promedio $
                 param1_desvest $
                 param2_desvest $
                 param3_desvest $
                 param4_desvest $
                 param5_desvest $
                 param6_desvest $
                 param7_desvest $
                 param8_desvest $
                 param9_desvest $
                 param10_desvest $
                 param11_desvest $
                 param12_desvest $
                 param13_desvest $
                 param14_desvest $
     ;
     if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
run;
PROC SQL; *Validacion de fichas (Evaluacion - Tecnica);
alter table INPATH.Fevaluacion add iden_registro char(11) format=$11.;
update INPATH.Fevaluacion set  iden_registro = 'SEDE       ' where id_institucion = 'NULL' and id_sede ^= 'NULL';
update INPATH.Fevaluacion set  iden_registro = 'INSTITUCION' where id_institucion ^= 'NULL' and id_sede = 'NULL';
update INPATH.Fevaluacion set  iden_registro = 'ENTE' where id_institucion = 'NULL' and id_sede = 'NULL';

/*****************************************************************************
* * Validación Ficha Técnica - Ficha Evaluación 
*****************************************************************************/
create table ft.rev_Fichas as select 
b.anio as anio_FE, 
a.id_ent_territorial as id_ent_territorial_FT, b.id_ent_territorial as id_ent_territorial_FE, 
a.id_institucion as id_institucion_FT, b.id_institucion as id_institucion_FE, 
a.id_sede as id_sede_FT, b.id_sede as id_sede_FE, 
a.grado as grado_FT, b.grado as grado_FE, 
a.iden_registro as iden_registro_FT, b.iden_registro as iden_registro_FE, 
a.cod_escala as cod_escala_FT, b.cod_escala as cod_escala_FE, b.cod_item, b.tipo, b.subtipo, 
b.grado, b.N, b.N_Estab,  b.puntuacion, b.margen, b.desviacion, b.nivel1, b.nivel2, 
a.tipo_reporte, a.Escala_noDisCog, a.Escala_DisCog, a.t_Escala_noDisCog, a.t_Escala_DisCog
from INPATH.Fevaluacion b full join ft.Fichatcc a
on (a.id_ent_territorial = b.id_ent_territorial and a.id_institucion = b.id_institucion and 
    a.id_sede = b.id_sede and a.cod_escala = b.cod_escala and a.grado = b.grado);

/*************************************************************************************************
* No deben estar en evaluación (Verificación tipo '02', '09')
**************************************************************************************************/

create table error_tipos0209 as select * from ft.rev_Fichas where (tipo_reporte = '02' or tipo_reporte = '09') 
and iden_registro_FT = iden_registro_FE and N > 2;

/**********************************************************************************************
* Comprobando estan en evaluación y no en tecnica (Verificación todos los tipos)
**************************************************************************************************/
create table errores_noestaenFT as select * from ft.rev_Fichas where iden_registro_FT = '' and 
iden_registro_FE ^= '';

/**********************************************************************************************
* Comprobando contra Ficha Tecnica Cognitivo
**************************************************************************************************/ 
create table errores_errorFTCog as select distinct * from ft.rev_Fichas 
where iden_registro_FT ^= '' and id_institucion_FT ^= 'NULL' and 
id_institucion_FT not in (select distinct id_institucion from INPATH.Ficha_tecnica_cog where id_institucion ^= 'NULL');

create table errores_errorFTCogII as select distinct * from INPATH.Ficha_tecnica_cog
where id_institucion ^= 'NULL' and grado ^= '3' and
id_institucion not in (select distinct id_institucion_FT from ft.rev_Fichas  where id_institucion_FT ^= 'NULL' and iden_registro_FT ^= '');

/**********************************************************************************************
* Comprobando codigos de items y Escalas
**************************************************************************************************/

create table infoItems as select NOMBRE, cod_escala, cod_item, compress(cod_escala || cod_item) as keyAll 
from INPATH.Codificaind where cod_escala in (select distinct cod_escala from INPATH.Codificaind where grado5 = '1' or grado9 = '1');

create table infoEscalas as select NOMBRE, cod_escala, n_items
from (select distinct NOMBRE, cod_escala, cod_item, count(distinct cod_item) as n_items
from INPATH.Codificaind group by cod_escala) where cod_item = 'NULL' and 
cod_escala in (select distinct cod_escala from INPATH.Codificaind where grado5 = '1' or grado9 = '1');

create table errores_codigosEscalaFT as select * from ft.rev_Fichas
where iden_registro_FT ^= '' and cod_escala_FT not in (select distinct cod_escala from infoEscalas) and tipo_reporte ^= '30' 
and tipo_reporte not in ('01', '02', '08', '09', '10', '11', '13', '15', '17', '18', '19', '23', '24', '25', '29');

create table errores_codigosEscalaFE as select * from ft.rev_Fichas
where iden_registro_FE ^= '' and compress(cod_escala_FE || cod_item) not in (select distinct keyAll from infoItems);

/**********************************************************************************************
* Comprobando campos segun iden_registro
**************************************************************************************************/
create table errores_camposApaga as select * from ft.rev_Fichas
where iden_registro_FT = 'SEDE' and ( (Fichatcc) or ());

insert into errores_camposApaga as select * from ft.rev_Fichas
where iden_registro_FT = 'INSTITUCION' and ( () or ());

insert into errores_camposApaga as select * from ft.rev_Fichas
where iden_registro_FT = 'ENTE' and ( () or ());


/*************************************************************************************************
* Verificación tipo 01 y 10
**************************************************************************************************/
create table errores_noestaenFE as select distinct * from ft.rev_Fichas where iden_registro_FT ^= '' and iden_registro_FE = '' and 
tipo_reporte not in ('01', '10', '02', '09') and compress(iden_registro_FT) not in ('SEDE', 'NACIONAL');

create table errores_tipo0110 as select distinct * from ft.rev_Fichas where iden_registro_FT ^= '' and 
tipo_reporte in ('01', '10') and iden_registro_FE ^= '' and 
((tipo = 2 and margen = 'NULL' and N > 1) or (tipo = 2 and margen ^= 'NULL' and N = 1 and cod_item = 'NULL'));

*Excluyendo aquellos que son copia masiva;
create table aux_copia as select * from INPATH.copia where consLect in 
(select consLect from INPATH.info where compress(daneEstab || put(grado, 1.)) in 
(select compress(id_institucion_FT || put(grado_FT, 1.)) from errores_tipo0110));

create table aux_copia_II as select a.*, b.daneEstab, b.grado from aux_copia a left join INPATH.info b on 
a.consLect = b.consLect;
create table errores_copia as select * from aux_copia_II where daneEstab = '';

delete from errores_tipo0110 where compress(id_institucion_FT || put(grado_FT, 1.)) in
(select compress(daneEstab || put(grado, 1.)) from 
		(select daneEstab, grado, sum(CASE WHEN copia = 2 then 1 else 0 end) as nCopia, 
        		count(*) as n_total from aux_copia_II group by daneEstab)
where n_total - nCopia <= 1);

/*************************************************************************************************
* Verificación tipo 08
**************************************************************************************************/
create table errores_tipo08 as select distinct * from ft.rev_Fichas where iden_registro_FT ^= '' and 
tipo_reporte = '08' and iden_registro_FE ^= '' and tipo = 2 and (puntuacion ^= 'NULL' or margen^= 'NULL');

insert into errores_tipo08 select distinct * from ft.rev_Fichas where iden_registro_FT ^= '' and 
tipo_reporte = '08' and t_Escala_noDisCog + t_Escala_DisCog = 0;

/*************************************************************************************************
* Verificación tipo 11 y 12
**************************************************************************************************/
create table errores_tipo1112 as select distinct * from ft.rev_Fichas where iden_registro_FT ^= '' and 
tipo_reporte in ('11', '12') and iden_registro_FE = '';

insert into errores_tipo1112 select distinct * from ft.rev_Fichas where (tipo_reporte = '11' or tipo_reporte = '12')
and iden_registro_FT ^= '' and tipo = 2 and puntuacion = 'NULL';

insert into errores_tipo1112 select distinct * from ft.rev_Fichas where (tipo_reporte = '11' or tipo_reporte = '12')
and iden_registro_FT ^= '' and tipo = 2 and margen = 'NULL';

insert into errores_tipo1112 select distinct * from ft.rev_Fichas where tipo_reporte = '11'  and 
iden_registro_FT ^= '' and input(nivel1, 6.) + input(nivel2, 6.) ^= 100 and tipo = 2;

/*************************************************************************************************
* Verificación tipo 13 y 14
**************************************************************************************************/

create table errores_tipo1314 as select distinct * from ft.rev_Fichas where tipo_reporte in ('13', '14')  and 
iden_registro_FT ^= '' and (tipo = 21) and input(nivel1, 6.) + input(nivel2, 6.) ^= N;

delete from  errores_tipo1314 where input(nivel1, 6.) + input(nivel2, 6.) <= N and cod_escala_FT in 
(select cod_escala from infoEscalas where n_items > 1);

/*************************************************************************************************
* Verificación tipo 15 y 16 (El numero de discapacitados debe ser igual que en Evaluación)
**************************************************************************************************/

create table errores_tipo1516 as select distinct * from ft.rev_Fichas where tipo_reporte in ('15', '16') and 
iden_registro_FT ^= '' and iden_registro_FT = iden_registro_FE and (tipo = 3) and input(nivel1, 6.) + input(nivel2, 6.) ^= N;

delete from  errores_tipo1516 where input(nivel1, 6.) + input(nivel2, 6.) <= N and cod_escala_FT in 
(select cod_escala from infoEscalas where n_items > 1);

/**************************************************************************************************
* Comprobacion agreagados uniestabs (Verificación tipo '29')
**************************************************************************************************/
create table nEstab_Ent as select a.*, b.mpio, b.enteId  from 
ft.rev_Fichas a left join inPath.infoEstab b on (compress(b.daneEstab) = compress(a.id_institucion_FE))
where id_institucion_FE NOT IN ('NULL', '');

insert into nEstab_Ent select a.*, b.mpio, b.enteId  from 
ft.rev_Fichas a left join inPath.infoEstab b on (compress(b.daneEstab) = compress(a.id_institucion_FE))
where id_institucion_FT NOT IN ('NULL', '');

update nEstab_Ent set id_institucion_FE =  id_institucion_FT where id_institucion_FT ^= '' and id_institucion_FE = '';

create table uniEstab as select mpio, count(distinct id_institucion_FE) as n_estab from nEstab_Ent where tipo IN (1, 2, 4, 17) group by mpio;
insert into uniEstab select enteId, count(distinct id_institucion_FE) as n_estab from nEstab_Ent where tipo IN (1, 2, 4, 17) group by enteId;
delete from uniEstab where n_estab > 1;

create table error_tipo29 as select * from ft.rev_Fichas where tipo_reporte = '29'  /*Uniestab*/
and id_ent_territorial_FT ^= 'NULL' and compress(id_ent_territorial_FT) not in (select compress(mpio) from uniEstab);

insert into error_tipo29 select * from ft.rev_Fichas where tipo_reporte not in ('01', '29')  /*Uniestab*/
and id_ent_territorial_FT ^= 'NULL' and compress(id_ent_territorial_FT) in (select compress(mpio) from uniEstab);

/*************************************************************************************************
* Verificación tipo 17, 18, 19 (El numero de discapacitados debe ser igual que en Evaluación)
**************************************************************************************************/

create table errores_tipo171819 as select distinct * from ft.rev_Fichas where 
tipo_reporte in ('17', '18', '19') and iden_registro_FT ^= '' 
and iden_registro_FT = iden_registro_FE and tipo = 1 and 
(puntuacion = 'NULL' or margen = 'NULL' or desviacion = 'NULL');

insert into errores_tipo171819 select * from ft.rev_Fichas where tipo_reporte in ('17', '18', '19') and 
iden_registro_FT ^= iden_registro_FE;

/*************************************************************************************************
* Verificación tipo 17, 18, 19 (Escalas con reporte Grande)
**************************************************************************************************/

create table errores_tipo171819 as select distinct * from ft.rev_Fichas where 
tipo_reporte in ('17', '18', '19') and iden_registro_FT ^= '' 
and iden_registro_FT = iden_registro_FE and tipo = 1 and (puntuacion = 'NULL' or margen = 'NULL' or desviacion = 'NULL' 
or (input(nivel1, 6.) + input(nivel2, 6.) ^= 100));

insert into errores_tipo171819 select * from ft.rev_Fichas where tipo_reporte in ('17', '18', '19') and 
iden_registro_FT ^= iden_registro_FE;

/*************************************************************************************************
* Verificación tipo 20, 21, 22 (Items con reporte grande)
**************************************************************************************************/

create table errores_tipo202122 as select distinct * from ft.rev_Fichas where 
tipo_reporte in ('20', '21', '22') and iden_registro_FT ^= '' 
and iden_registro_FT = iden_registro_FE and tipo = 1 and (puntuacion = 'NULL' or margen = 'NULL');

insert into errores_tipo202122 select * from ft.rev_Fichas where tipo_reporte in ('20', '21', '22') and 
iden_registro_FT ^= iden_registro_FE;

/*************************************************************************************************
* Verificación tipo 23, 24, 25 (Escalas reporte pequeño)
**************************************************************************************************/
create table errores_tipo232425 as select distinct * from ft.rev_Fichas where 
tipo_reporte in ('23', '24', '25') and iden_registro_FT ^= '' 
and iden_registro_FT = iden_registro_FE and tipo = 22 and 
(input(nivel1, 6.) + input(nivel2, 6.) ^= N or N ^= Escala_noDisCog);

insert into errores_tipo232425 select * from ft.rev_Fichas where tipo_reporte in ('23', '24', '25') 
and cod_escala_FT not in (select cod_escala from infoEscalas where n_items = 1);

insert into errores_tipo232425 select * from ft.rev_Fichas where tipo_reporte in ('23', '24', '25') and 
iden_registro_FT ^= iden_registro_FE;


/*************************************************************************************************
* Verificación tipo 26, 27, 28 (Items reporte pequeño)
**************************************************************************************************/
create table errores_tipo262728 as select distinct * from ft.rev_Fichas where 
tipo_reporte in ('26', '27', '28') and iden_registro_FT ^= '' 
and iden_registro_FT = iden_registro_FE and tipo = 22 and (input(nivel1, 6.) + input(nivel2, 6.) > N);

insert into errores_tipo262728 select * from ft.rev_Fichas where tipo_reporte in ('26', '27', '28') 
and cod_escala_FT not in (select cod_escala from infoEscalas where n_items > 1);

insert into errores_tipo262728 select * from ft.rev_Fichas where tipo_reporte in ('26', '27', '28') 
and iden_registro_FT ^= iden_registro_FE;

/*************************************************************************************************
* Isumo para validaciones
**************************************************************************************************/
create table ft.Rev_fichas_Agrega as select * from ft.Rev_Fichas where 
(id_ent_territorial_FT ^= 'NULL' or id_ent_territorial_FE ^= 'NULL') and
(id_institucion_FT = 'NULL' or id_institucion_FE = 'NULL') and
(id_sede_FT  = 'NULL' or id_sede_FE = 'NULL');

create table ft.Rev_fichas_Estab as select * from ft.Rev_Fichas where 
(id_ent_territorial_FT = 'NULL' or id_ent_territorial_FE = 'NULL') and
(id_institucion_FT ^= 'NULL' or id_institucion_FE ^= 'NULL') and
(id_sede_FT  = 'NULL' or id_sede_FE = 'NULL');

quit;

PROC SQL; *Busquedas Manuales;
/*
create table p3 as select * from ft.Fichatcc where tipo_reporte in ("23", "24", "26", "27");
create table p4 as select * from ft.Rev_Fichas where tipo_reporte in ("23", "24", "26", "27");

PROC EXPORT DATA= WORK.P3 
            OUTFILE= "C:\Users\jcarrasco\Desktop\P3.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= WORK.P4 
            OUTFILE= "C:\Users\jcarrasco\Desktop\P4.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
create table unicos as select distinct daneEstab as id_institucion, tipoApli from Inpath.Bibliaretorno;

create table filtro_ente as select * from Inpath.Fevaluacion where (id_ent_territorial ne 'NULL' and cod_escala in ('e01', 'e02')) or (tipo = 17);

create table filtro_nac19 as select * from Inpath.Fevaluacion where tipo = 19;


select tipo, count(*) as n_tipo from Inpath.Fevaluacion group by tipo;

select distinct grado from Inpath.Fevaluacion;
select * from Inpath.Fevaluacion where id_sede = '484104' and grado = '5';
select * from Inpath.Fevaluacion where id_ent_territorial = '52480' and tipo = '1';
select * from Inpath.Fevaluacion where id_institucion = "108675000099" and cod_escala = 'e07' and grado = 9;
select * from Inpath.Fevaluacion where id_institucion = "208675000034" and cod_escala = 'e07' and grado = 9;

select * from ft.rev_Fichas where id_institucion_FT = "252356001485" and area = '2' and grado = '3';

create table p3 as select * from INPATH.Info where mpio = '8770';
create table p3 as select * from INPATH.Info where Codsitio = '178188';
create table p3 as select * from INPATH.Info where daneEstab = "270771001219" and grado = 5 and parACTAGR = 1;

select distinct cod_escala, cod_item  from Inpath.Fevaluacion;
*/
quit;
data _null_; *Reporte de hallazgos para la revision de Fichas;
%MACRO saveLog(FILE);
%LET DATA_SET = &FILE;
%LET OPEN_DATA = %SYSFUNC(OPEN(&DATA_SET));
%LET OBSERVATIONS= %eval(%SYSFUNC(ATTRN(&OPEN_DATA, NOBS)) - %SYSFUNC(ATTRN(&OPEN_DATA, NDEL)));
%LET CLOSE_DATASET=%SYSFUNC(CLOSE(&OPEN_DATA));
%if &OBSERVATIONS > 0 %then %do;
	PROC EXPORT DATA= &FILE
		OUTFILE= "&PATH_PROJECT\OUTPUT\Validacion_&FILE..csv" 
		DBMS=CSV REPLACE;
	PUTNAMES=YES;
	%end;
%MEND;

%saveLog(errores_copia);   
%saveLog(error_tipos0209);
%saveLog(errores_noestaenFT);
%saveLog(errores_errorFTCog);
%saveLog(errores_errorFTCogII);
%saveLog(errores_codigosEscalaFT);
%saveLog(errores_codigosEscalaFE);
*%saveLog(errores_camposApaga);
%saveLog(errores_noestaenFE);
%saveLog(errores_tipo0110);
%saveLog(errores_tipo08);
%saveLog(errores_tipo1112);
%saveLog(errores_tipo1314);
%saveLog(errores_tipo1516);
%saveLog(errores_tipo171819);
%saveLog(errores_tipo202122);
%saveLog(errores_tipo232425);
%saveLog(errores_tipo262728);
%saveLog(error_tipo29);  
run;

data _null_; *Reporte de hallazgos para la revision de Fichas;
%MACRO saveLog(FILE);
%LET DATA_SET = &FILE;
%LET OPEN_DATA = %SYSFUNC(OPEN(&DATA_SET));
%LET OBSERVATIONS= %eval(%SYSFUNC(ATTRN(&OPEN_DATA, NOBS)) - %SYSFUNC(ATTRN(&OPEN_DATA, NDEL)));
%LET CLOSE_DATASET=%SYSFUNC(CLOSE(&OPEN_DATA));
%if &OBSERVATIONS > 0 %then %do;
	PROC EXPORT DATA= &FILE
		OUTFILE= "&PATH_PROJECT\OUTPUT\&FILE..txt" 
		DBMS=TAB REPLACE;
	PUTNAMES=YES;
	%end;
%MEND;

%saveLog(ft.Rev_fichas_Agrega);   
%saveLog(ft.Rev_fichas_Estab);
run;
