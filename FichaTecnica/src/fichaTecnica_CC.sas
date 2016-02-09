PROC SQL;
/**************************************************************************************************
* * lectBases.SAS
* * AUTOR: Jorge Mario Carrasco Ortiz
* * 
* * SABER 3° 5° 9° 
* * DESCRIPCIÓN: Construcción de Ficha Tecnica de Cognitivo
* * 
* * INPUTS: Bases creadas por jcalderon y mcordoba para el procesamiento ubicadas en 
* *		    \\icfesserv5\academica$\SABER\SABER_YYYY\CENSAL\BASES
* * 
* * OUTPUTS: 
* * 
* * FILE HISTORY:
* * 	20150306: Creación de script
* *
* * ToDo: 
* *       
*****************************************************************************************************/
%LET PATH_PARAM = &sysparm.; 
%include "&PATH_PARAM";
*%include C:\Users\jorge\Desktop\20141_FICHA_TECNICA_CC\parametros.sas;
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
* * En las bases el inPath tiene que estar:
*Biblia de retorno ------------ bibliaRetorno
matricula  ------------------- matricula
indicadoras ------------------ indicadorasCC
copia ------------------------ copia
pesos ------------------------ pesosCC;
**************************************************************************************************/

%LET varAgregaI= 'NULL' 'NULL' mpio depto enteId 'NULL';
%LET varAgregaII= daneEstab daneEstab 'NULL' 'NULL' 'NULL' 'NULL';
%LET varAgregaIII= codSitio 'NULL' 'NULL' 'NULL' 'NULL' 'NULL';

PROC SQL; /* Auxiliar unicos por ft.auxConteo*/
create table ft.auxConteo as select distinct daneEstab, codSitio, grado, mpio, depto, enteID, sector, zonaEstab, nse from  inpath.INFO;
create table ft.hallazgosMunicipio as select * from ft.auxConteo where daneEstab in 
(select daneEstab from (select daneEstab, count(distinct mpio) as nfilas from ft.auxConteo group by daneEstab) where nfilas >= 2);

/* Seleccionando sedes, Estab y agregados que no tienen reporte */
create table ft.muerteCognitivo as select id_ent_territorial, id_institucion, id_sede, grado, tipo_reporte_l, tipo_reporte_m,
tipo_reporte_c, tipo_reporte_d, 
(CASE WHEN prxchange('s/NULL|00|01|02|03|06//', -1, compress(tipo_reporte_l||tipo_reporte_m||tipo_reporte_c||tipo_reporte_d)) = '' then 1 else 0 end)
as ind_NOREP from inpath.Ficha_tecnica_cog;

/* Consolidando informacion del colegio*/
create table ft.IdentII as select distinct a.daneEstab, a.depto, b.mpio, b.enteId 
from inpath.Ident a inner join INPATH.infoEstab b 
on (a.daneEstab = b.daneEstab);

QUIT;



%macro dropAuxEscala(libra= , _ESCALA_NAME=);
  %if %sysfunc(exist(&libra..auxEscala_&_ESCALA_NAME.)) %then %do;
    proc datasets library = &libra. NOPRINT;
   		delete auxEscala_&_ESCALA_NAME.;
	run;
  %end;
%mend dropAuxEscala;

%macro conteosEscala(_ESCALA_NAME= , _COD_ESCALA=, _IND_PEGV02=); /* Macro que armado los conteos Escala-Grado */
	/* Construcción de tabla auxiliar por Escala*/ 
	PROC SQL;
		create table ft.auxEscala_&_ESCALA_NAME. as 
		select codSaber, daneEstab, grado, consLect_IND, consLect_PES, sexo, disCog, PCog, parCC, pesoCC, depto, mpio, 
		codSitio, tipoApli, snp, consLect, grupo, disSen, disFis, nivAgre, sector, zonaEstab, sZEstab, enteId, nse, 
    	par&_ESCALA_NAME.  as parEscala, peso&_ESCALA_NAME. as pesoEscala
		from inpath.INFO
	quit;
	/*Auxiliar de la escala indexando*/
	/*PROC APPEND 
		data = inpath.INFO (keep = codSaber daneEstab grado consLect_IND consLect_PES sexo disCog PCog parCC pesoCC depto mpio codSitio tipoApli snp consLect 
		grupo disSen disFis nivAgre sector zonaEstab sZEstab enteId nse par&_ESCALA_NAME peso&_ESCALA_NAME.)
		base = ft.auxEscala_&_ESCALA_NAME. (rename = (par&_ESCALA_NAME = parEscala peso&_ESCALA_NAME = pesoEscala));
	run;*/

	/* Construcción de conteo de estudiantes por los diferentes niveles de agregación */
	%LET jj = 1;
	%LET pais = '0';
	PROC SQL;
	%DO %UNTIL(NOT %LENGTH(%SCAN(&varAgregaI,&jj.)));
	    %if %SCAN(&varAgregaI, &jj.) = 'NULL' and %SCAN(&varAgregaII, &jj.) = 'NULL' and %SCAN(&varAgregaIII, &jj.) = 'NULL' %then %LET pais = '1';
	CREATE TABLE aux_conteo_&_ESCALA_NAME._&jj. as
		%if %SCAN(&varAgregaI, &jj.) = depto and %SCAN(&varAgregaII, &jj.) = 'NULL' and %SCAN(&varAgregaIII, &jj.) = 'NULL' %then SELECT '1' || %SCAN(&varAgregaI, &jj.) as id_ent_territorial,;
		%if %SCAN(&varAgregaI, &jj.) ^= depto or %SCAN(&varAgregaII, &jj.) ^= 'NULL' or %SCAN(&varAgregaIII, &jj.) ^= 'NULL' %then SELECT %SCAN(&varAgregaI, &jj.) as id_ent_territorial,; 
        %SCAN(&varAgregaII,&jj.) as id_institucion, %SCAN(&varAgregaIII,&jj.) as id_sede, grado,
		"&_COD_ESCALA" as cod_escala,
		sum(CASE WHEN parEscala = 1 and parCC = 1 and sexo = 1 then 1 else 0 end) as ninos_participantes,
		sum(CASE WHEN parEscala = 1 and parCC = 1 and sexo = 2 then 1 else 0 end) as ninas_participantes,		
		sum(CASE WHEN parEscala = 1 and parCC = 1 and sexo = 3 then 1 else 0 end) as noEspecifica_participantes,		
		sum(CASE WHEN parCC = 1 then 1 else 0 end) as t_Presentes,		
		sum(CASE WHEN parCC = 1 and pesoCC > 0 then 1 else 0 end) as Presentes,		
		sum(CASE WHEN parEscala = 1 and parCC = 1 then 1 else 0 end) as t_Escala,
		sum(CASE WHEN parEscala = 1 and parCC = 1 and disCog = 0 then 1 else 0 end) as t_Escala_noDisCog,
		sum(CASE WHEN parEscala = 1 and parCC = 1 and disCog = 1 then 1 else 0 end) as t_Escala_DisCog,
		sum(CASE WHEN parEscala = 1 and parCC = 1 and pesoEscala > 0 then 1 else 0 end) as Escala,
		sum(CASE WHEN parEscala = 1 and parCC = 1 and disCog = 0 and pesoEscala > 0 then 1 else 0 end) as Escala_noDisCog,
		sum(CASE WHEN parEscala = 1 and parCC = 1 and disCog = 1 and pesoEscala > 0 then 1 else 0 end) as Escala_DisCog,
		sum(CASE WHEN sector = '1' and parEscala = 1 then 1 else 0 end) as estud_oficiales,
		sum(CASE WHEN sector = '2' and parEscala = 1 then 1 else 0 end) as estud_no_oficiales,
		sum(CASE WHEN zonaEstab = '1' and parEscala = 1 then 1 else 0 end) as estud_urbanos, 
		sum(CASE WHEN zonaEstab = '2' and parEscala = 1 then 1 else 0 end) as estud_rurales,
		sum(CASE WHEN nse = '1' and parEscala = 1 then 1 else 0 end) as estud_nse1,
		sum(CASE WHEN nse = '2' and parEscala = 1 then 1 else 0 end) as estud_nse2,
		sum(CASE WHEN nse = '3' and parEscala = 1 then 1 else 0 end) as estud_nse3,
		sum(CASE WHEN nse = '4' and parEscala = 1 then 1 else 0 end) as estud_nse4,
		sum(CASE WHEN sector = '1' and zonaEstab = '1' and parEscala = 1 then 1 else 0 end) as estud_of_urb,
		sum(CASE WHEN sector = '1' and zonaEstab = '2' and parEscala = 1 then 1 else 0 end) as estud_of_rur,
		sum(CASE WHEN sector = '2' and zonaEstab = '1' and parEscala = 1 then 1 else 0 end) as estud_noof_urb,
		sum(CASE WHEN sector = '2' and zonaEstab = '2' and parEscala = 1 then 1 else 0 end) as estud_noof_rur, 
		%if %SCAN(&varAgregaI, &jj.) ^= 'NULL' and &pais. ^= '1' and %SCAN(&varAgregaII, &jj.) = 'NULL' %then "%SCAN(&varAgregaI, &jj.)" as id_tip_Agr, ;
		%if %SCAN(&varAgregaI, &jj.) = 'NULL' and %SCAN(&varAgregaII, &jj.) ^= 'NULL' and %SCAN(&varAgregaIII, &jj.) = 'NULL' %then "daneEstab" as id_tip_Agr, ;
		%if &pais. = '1' %then "NULL" as id_tip_Agr, ;
		%if %SCAN(&varAgregaIII, &jj.) ^= 'NULL' %then "SEDE       "  as iden_registro;
	    %if %SCAN(&varAgregaII, &jj.) ^= 'NULL' and %SCAN(&varAgregaIII, &jj.) = 'NULL' %then  "INSTITUCION" as iden_registro;
	    %if %SCAN(&varAgregaI, &jj.) ^= "NULL" and &pais. = '0' %then "ENTE" as iden_registro;
		%if &pais. = '1' %then "NACIONAL" as iden_registro;
	FROM ft.auxEscala_&_ESCALA_NAME. group by
		%if %SCAN(&varAgregaI, &jj.) ^= 'NULL' %then %SCAN(&varAgregaI, &jj.),;
		%if %SCAN(&varAgregaII, &jj.) ^= 'NULL' %then %SCAN(&varAgregaII, &jj.),;
		%if %SCAN(&varAgregaIII, &jj.) ^= 'NULL' %then %SCAN(&varAgregaIII, &jj.),;
		grado;

		CREATE TABLE ft.conteo_&_ESCALA_NAME._&jj. as SELECT
		%if %SCAN(&varAgregaI, &jj.) = 'NULL' and &pais. = '0' %then datA.id_ent_territorial, datA.id_institucion, datA.id_sede, datA.grado, datA.cod_escala,;
		%if %SCAN(&varAgregaI, &jj.) ^= 'NULL' or &pais. = '1' %then datA.id_ent_territorial, datA.id_institucion, datA.id_sede, datA.grado, datA.cod_escala,;
	 	ninos_participantes, ninas_participantes, noEspecifica_participantes, t_Presentes, Presentes, t_Escala, t_Escala_noDisCog, t_Escala_DisCog, Escala, Escala_noDisCog, Escala_DisCog,
		%if %SCAN(&varAgregaI, &jj.) ^= 'NULL' or &pais. = '1' %then datB.establecimientos as establecimientos,;
		%if %SCAN(&varAgregaI, &jj.) ^= 'NULL' or &pais. = '1' %then datB.sedes as sedes,;
		%if %SCAN(&varAgregaI, &jj.) ^= 'NULL' or &pais. = '1' %then datB.inst_oficiales as inst_oficiales,;
		%if %SCAN(&varAgregaI, &jj.) ^= 'NULL' or &pais. = '1' %then datB.inst_no_oficiales as inst_no_oficiales,;
		%if %SCAN(&varAgregaI, &jj.) ^= 'NULL' or &pais. = '1' %then datB.inst_urbanos as inst_urbanos,;
		%if %SCAN(&varAgregaI, &jj.) ^= 'NULL' or &pais. = '1' %then datB.inst_rurales as inst_rurales,;
		%if %SCAN(&varAgregaI, &jj.) ^= 'NULL' or &pais. = '1' %then datB.inst_nse1 as inst_nse1,;
		%if %SCAN(&varAgregaI, &jj.) ^= 'NULL' or &pais. = '1' %then datB.inst_nse2 as inst_nse2,;
		%if %SCAN(&varAgregaI, &jj.) ^= 'NULL' or &pais. = '1' %then datB.inst_nse3 as inst_nse3,;
		%if %SCAN(&varAgregaI, &jj.) ^= 'NULL' or &pais. = '1' %then datB.inst_nse4 as inst_nse4,;
		%if %SCAN(&varAgregaII, &jj.) ^= 'NULL' and %SCAN(&varAgregaIII, &jj.) = 'NULL' %then datB.establecimientos as establecimientos,;
		%if %SCAN(&varAgregaII, &jj.) ^= 'NULL' and %SCAN(&varAgregaIII, &jj.) = 'NULL' %then datB.sedes as sedes,;
		%if %SCAN(&varAgregaII, &jj.) ^= 'NULL' and %SCAN(&varAgregaIII, &jj.) ^= 'NULL' %then . as establecimientos,;
		%if %SCAN(&varAgregaII, &jj.) ^= 'NULL' and %SCAN(&varAgregaIII, &jj.) ^= 'NULL' %then . as sedes,;
		%if %SCAN(&varAgregaII, &jj.) ^= 'NULL' %then . as inst_oficiales,;
		%if %SCAN(&varAgregaII, &jj.) ^= 'NULL' %then . as inst_no_oficiales,;
		%if %SCAN(&varAgregaII, &jj.) ^= 'NULL' %then . as inst_urbanos,;
		%if %SCAN(&varAgregaII, &jj.) ^= 'NULL' %then . as inst_rurales,;
		%if %SCAN(&varAgregaII, &jj.) ^= 'NULL' %then . as inst_nse1,;
		%if %SCAN(&varAgregaII, &jj.) ^= 'NULL' %then . as inst_nse2,;
		%if %SCAN(&varAgregaII, &jj.) ^= 'NULL' %then . as inst_nse3,;
		%if %SCAN(&varAgregaII, &jj.) ^= 'NULL' %then . as inst_nse4,;
		estud_oficiales, estud_no_oficiales, estud_urbanos,  estud_rurales, estud_nse1, estud_nse2, estud_nse3, estud_nse4,
		%if %SCAN(&varAgregaI, &jj.) ^= 'NULL' or &pais. = '1' %then datB.inst_of_urb as inst_of_urb,;
		%if %SCAN(&varAgregaI, &jj.) ^= 'NULL' or &pais. = '1' %then datB.inst_of_rur as inst_of_rur,;
		%if %SCAN(&varAgregaI, &jj.) ^= 'NULL' or &pais. = '1' %then datB.inst_noof_urb as inst_noof_urb,;	
		%if %SCAN(&varAgregaI, &jj.) ^= 'NULL' or &pais. = '1' %then datB.inst_noof_rur as inst_noof_rur,;
		%if %SCAN(&varAgregaII, &jj.) ^= 'NULL' %then . as inst_of_urb,;
		%if %SCAN(&varAgregaII, &jj.) ^= 'NULL' %then . as inst_of_rur,;
		%if %SCAN(&varAgregaII, &jj.) ^= 'NULL' %then . as inst_noof_urb,;
		%if %SCAN(&varAgregaII, &jj.) ^= 'NULL' %then . as inst_noof_rur,;
		estud_of_urb, estud_of_rur, estud_noof_urb, estud_noof_rur, iden_registro, 
		%if &_IND_PEGV02. ^= 1 %then 'NULL' as tipo_reporte;
		%if &_IND_PEGV02. = 1 %then datC.tipo_reporte as tipo_reporte;
	FROM 
	/*Sejos*/     %if %SCAN(&varAgregaI, &jj.) = 'NULL' and &pais. = '0' and  %SCAN(&varAgregaIII, &jj.) ^= 'NULL' %then aux_conteo_&_ESCALA_NAME._&jj. datA;
	/*Agregados*/ %if %SCAN(&varAgregaI, &jj.) ^= 'NULL' or &pais. = '1' %then aux_conteo_&_ESCALA_NAME._&jj. datA inner join INPATH.CONTEOAGREGA datB on (datA.id_ent_territorial = datB.id_ent_territorial and datA.grado = datB.grado and datA.id_tip_Agr = datB.id_tip_Agr); 
	/*Estabs*/    %if %SCAN(&varAgregaII, &jj.) ^= 'NULL'  and %SCAN(&varAgregaIII, &jj.) = 'NULL' %then aux_conteo_&_ESCALA_NAME._&jj. datA inner join INPATH.CONTEOAGREGA datB on (datA.id_institucion = datB.id_institucion and datA.grado = datB.grado and datA.id_tip_Agr = datB.id_tip_Agr); 
	%if &_IND_PEGV02. = 1 %then %do;
     %let auxTRItem = %substr(&_ESCALA_NAME., 1, %EVAL(%LENGTH(&_ESCALA_NAME.) - 1));
	 left join (select id_ent_territorial, id_institucion, id_sede, grado, 
				 cod_escala, tipo_reporte from ft.conteo_&auxTRItem.1_&jj.) datC 
	 on (datA.id_ent_territorial = datC.id_ent_territorial and datA.id_institucion = datC.id_institucion and  datA.id_sede = datC.id_sede and datA.grado = datC.grado)
	%end;
;  

	%if %SCAN(&varAgregaI, &jj.) = depto %then %do;
		delete from ft.conteo_&_ESCALA_NAME._&jj. where 
        id_ent_territorial in ('191','181','194','195','186','188','197','199','111');
	%end;
	%if %SCAN(&varAgregaI, &jj.) = mpio %then %do;
	delete from ft.conteo_&_ESCALA_NAME._&jj. where compress(id_ent_territorial) in ('05001','05045', '05088', '05266', '05360', '05615', 
		'05631', '05837', '08001', '08433', '08758', '11001', '13001', '13430', '15001', '15238', '15759', '17001', 
		'18001', '19001', '20001', '23001', '23417', '23660', '25175', '25269', '25290', '25307', '25473', '25754', 
		'25899', '27001', '41001', '41551', '44001', '44430', '44847', '47001', '47189', '50001', '52001', '52356', 
		'52835', '54001', '63001', '66001', '66170', '68001', '68081', '68276', '68307', '68547', '70001', '73001', 
		'76001', '76109', '76111', '76147', '76364', '76520', '76834', '85001');
	%end;
	  %LET jj = %EVAL(&jj. + 1);
	%END;
*Eliminando sejos 999999;
delete from ft.conteo_&_ESCALA_NAME._1 where id_sede = '999999';
QUIT;
%mend conteosEscala;
*%dropAuxEscala(libra= ft, _ESCALA_NAME=ACTAGR);
*%dropAuxEscala(libra= ft, _ESCALA_NAME=ACTCORR);
*%conteosEscala(_ESCALA_NAME= ACTAGR, _COD_ESCALA= e03, _IND_PEGV02 = 0);
*%conteosEscala(_ESCALA_NAME= ACTCORR, _COD_ESCALA= e08, _IND_PEGV02 = 0);
*%dropAuxEscala(libra= ft, _ESCALA_NAME=ACTDIV_V02);
*%conteosEscala(_ESCALA_NAME= ACTDIV_V02, _COD_ESCALA= e14, _IND_PEGV02 = 1);
%macro makeTipos(_ESCALA_NAME, flagItem); /*Macro creada para la asignacion de tipos de reporte de una escala*/
/*******************************************************************************************************/
/* TIPOS PARA SEDES - JORNADAS */
/*******************************************************************************************************/

PROC SQL;

*Tipo de reporte 10 Sejo (Problemas cognitivo);
update ft.conteo_&_ESCALA_NAME._1 set tipo_reporte = '10'
where tipo_reporte = 'NULL' and compress(id_sede || put(grado, 1.)) in (select compress(id_sede || grado)
from ft.Muertecognitivo where id_sede ^= 'NULL' and ind_NOREP = 1 and grado ^= 'NULL');

*Tipo de reporte 01 Sejo (No participantes en el escala - grado);
update ft.conteo_&_ESCALA_NAME._1 set tipo_reporte = '01'
where tipo_reporte = 'NULL' and t_Escala = 0;

*Tipo de reporte 08 Sejo (Sejo normal);
update ft.conteo_&_ESCALA_NAME._1 set tipo_reporte = '08'
where tipo_reporte = 'NULL';
quit;	

*create table p4 as select id_institucion, grado, tipo_reporte from ft.Conteo_actagr_2;
*create table p1 as select * from inpath.INFO where daneEstab = "113001000143" and grado = 9 and parACTAGR = 1 and pesoACTAGR > 0;

/*******************************************************************************************************/
/* TIPOS PARA ESTABS */
/*******************************************************************************************************/
PROC SQL;
*Tipo de reporte 10 Estabs (Problemas cognitivo);
update ft.conteo_&_ESCALA_NAME._2 set tipo_reporte = '10'
where tipo_reporte = 'NULL' and compress(id_institucion || put(grado, 1.)) in (select compress(id_institucion || grado)
from ft.Muertecognitivo where id_institucion ^= 'NULL' and ind_NOREP = 1 and grado ^= 'NULL');

*Tipo de reporte 01 Estabs (No participantes en la escala - grado);
update ft.conteo_&_ESCALA_NAME._2 set tipo_reporte = '01'
where tipo_reporte = 'NULL' and t_Escala = 0;

*Tipo de reporte 02 Estabs (Baja tasa de participación);
update ft.conteo_&_ESCALA_NAME._2 set tipo_reporte = '02'
where tipo_reporte = 'NULL' and t_Escala  ^= 0 and Escala = 0;

	*Tipo de reporte 15/16 Estabs (Reporte discapacitados para escala - grado);
	%if &flagItem. = 1 %then update ft.conteo_&_ESCALA_NAME._2 set tipo_reporte = '15';
	%if &flagItem. > 1 %then update ft.conteo_&_ESCALA_NAME._2 set tipo_reporte = '16';
	where tipo_reporte = 'NULL' and Escala_noDisCog = 0 and Escala_DisCog > 0;

	*Tipo de reporte 11/12 Estabs (Reporte normal para escala - grado);
	create table auxCSejo as select compress(id_institucion || put(grado, 1.)) as keyIntgr, grado, 
    sum(CASE WHEN Escala_noDisCog >= 2 and compress(id_sede || put(grado, 1.)) not in (select compress(id_sede || grado)
        from ft.Muertecognitivo where id_sede ^= 'NULL' and ind_NOREP = 1 and grado ^= 'NULL') then 1 else 0 end) as n_sedeGr
    from Aux_conteo_&_ESCALA_NAME._1 group by id_institucion, grado;

	%if &flagItem. = 1 %then update ft.conteo_&_ESCALA_NAME._2 set tipo_reporte = '11';
    %if &flagItem. > 1 %then update ft.conteo_&_ESCALA_NAME._2 set tipo_reporte = '12';
	where tipo_reporte = 'NULL' and 
    %if &flagItem. = 1 %then Escala_noDisCog >= 6;
	%if &flagItem. > 1 %then Escala_noDisCog >= 25;
	and compress(id_institucion || put(grado, 1.)) in (select keyIntgr
	from auxCSejo where n_sedeGr > 0);
	
	*Tipo de reporte 13/14 Estabs (Reporte especial para escala - grado);
	%if &flagItem. = 1 %then update ft.conteo_&_ESCALA_NAME._2 set tipo_reporte = '13';
	%if &flagItem. > 1 %then update ft.conteo_&_ESCALA_NAME._2 set tipo_reporte = '14';
	where tipo_reporte = 'NULL' and 
    %if &flagItem. = 1 %then (Escala_noDisCog < 6;
	%if &flagItem. > 1 %then (Escala_noDisCog < 25;
    or compress(id_institucion || put(grado, 1.)) not in (select keyIntgr
	from auxCSejo where n_sedeGr > 0));
quit;
/*******************************************************************************************************/
/* TIPOS PARA AGREGADOS */
/*******************************************************************************************************/
PROC SQL noprint;

*Cruzando la información del colegio (Depto, mpio, enteId);
create table auxCEstab as select * from ft.conteo_&_ESCALA_NAME._2 a left join ft.IdentII b 
on (a.id_institucion = b.daneEstab);

%LET varAgre = mpio depto enteId;
%LET jj = 1;
%DO %UNTIL(NOT %LENGTH(%SCAN(&varAgre,&jj.)));
  %LET auxAgre = %SCAN(&varAgre., &jj.);
	%if &jj. = 1 %then create table ft.auxCES_&_ESCALA_NAME. as;
	%if &jj. ^= 1 %then insert into ft.auxCES_&_ESCALA_NAME.;
	/*Creando conteos de # de establecimientos con TR = 10*/
	%if &auxAgre. = depto %then select compress('1' || id_ent_territorial) as id_ent_territorial,;
	%if &auxAgre. ^= depto %then select &auxAgre. as id_ent_territorial,;
	%if &auxAgre. = depto %then compress('1' || &auxAgre. || put(grado, 1.)) as keyIntgr,;
	%if &auxAgre. ^= depto %then compress(&auxAgre. || put(grado, 1.)) as keyIntgr,;
    grado, count(*) as n_estab, sum(CASE WHEN tipo_reporte = '10' then 1 else 0 end) as n_estabMuertos,
	sum(CASE WHEN tipo_reporte = '11' or tipo_reporte = '13' then 1 else 0 end) as n_granEscala,
	sum(CASE WHEN tipo_reporte = '12' or tipo_reporte = '14' then 1 else 0 end) as n_granItem
	from  auxCEstab group by &auxAgre., grado, '';

	*Tipo de reporte 09 Agregados (Problemas cognitivo);
	update ft.conteo_&_ESCALA_NAME._%eval(&jj. + 2) set tipo_reporte = '09'
	where tipo_reporte = 'NULL' and (compress(id_ent_territorial || put(grado, 1.)) in (select compress(id_ent_territorial || grado)
	from ft.Muertecognitivo where id_ent_territorial ^= 'NULL' and ind_NOREP = 1 and grado ^= 'NULL')
	or compress(id_ent_territorial || put(grado, 1.)) in (select keyIntgr from ft.auxCES_&_ESCALA_NAME. where n_estab = n_estabMuertos));

	*Tipo de reporte 01 Agregados (No participantes en el escala - grado);
	update ft.conteo_&_ESCALA_NAME._%eval(&jj. + 2) set tipo_reporte = '01'
	where tipo_reporte = 'NULL' and t_Escala = 0;

	*Tipo de reporte 02 Agregados (Baja tasa de participación);
	update ft.conteo_&_ESCALA_NAME._%eval(&jj. + 2) set tipo_reporte = '02'
	where tipo_reporte = 'NULL' and t_Escala  ^= 0 and Escala = 0;

	/* Tipo de reporte 29 Agregados (Uni-establecimientos)*/

	create table cuentas22 as select id_ent_territorial, max(n_estab) as n_estab
	from ft.auxCES_&_ESCALA_NAME. group by id_ent_territorial;

	update ft.conteo_&_ESCALA_NAME._%eval(&jj. + 2) set tipo_reporte = '29'
	where tipo_reporte = 'NULL' and id_ent_territorial in (select id_ent_territorial
	from cuentas22 where n_estab = 1);	

	*Tipo de reporte 17/18/19 Agregados (Reporte normal para escala - grado);
	%if &flagItem. = 1 %then %do;
		update ft.conteo_&_ESCALA_NAME._%eval(&jj. + 2) 
 	    %if &jj. = 3 %then set tipo_reporte = '17';
		%if &jj. = 2 %then set tipo_reporte = '18';
		%if &jj. = 1 %then set tipo_reporte = '19';
		where tipo_reporte = 'NULL' and Escala_noDisCog >= 100
		and compress(id_ent_territorial || put(grado, 1.)) in (select keyIntgr from ft.auxCES_&_ESCALA_NAME. where n_granEscala >= 5);
	%end;

	*Tipo de reporte 20/21/22 Agregados (Reporte normal para item - grado);	
	%if &flagItem. > 1 %then %do;
		update ft.conteo_&_ESCALA_NAME._%eval(&jj. + 2) 
 	    %if &jj. = 3 %then set tipo_reporte = '20';
		%if &jj. = 2 %then set tipo_reporte = '21';
		%if &jj. = 1 %then set tipo_reporte = '22';
		where tipo_reporte = 'NULL' and Escala_noDisCog >= 100
		and compress(id_ent_territorial || put(grado, 1.)) in (select keyIntgr from ft.auxCES_&_ESCALA_NAME. where n_granItem >= 5);
	%end;

	*Tipo de reporte 23/24/25 Agregados (Reporte pequeño para escala - grado);
	%if &flagItem. = 1 %then %do;
		update ft.conteo_&_ESCALA_NAME._%eval(&jj. + 2) 
 	    %if &jj. = 3 %then set tipo_reporte = '23';
		%if &jj. = 2 %then set tipo_reporte = '24';
		%if &jj. = 1 %then set tipo_reporte = '25';
		where tipo_reporte = 'NULL' and (Escala_noDisCog < 100
		or compress(id_ent_territorial || put(grado, 1.)) in (select keyIntgr from ft.auxCES_&_ESCALA_NAME. where n_granEscala < 5));
	%end;

	*Tipo de reporte 26/27/28 Agregados (Reporte pequeño para item - grado);	
	%if &flagItem. > 1 %then %do;
		update ft.conteo_&_ESCALA_NAME._%eval(&jj. + 2) 
 	    %if &jj. = 3 %then set tipo_reporte = '26';
		%if &jj. = 2 %then set tipo_reporte = '27';
		%if &jj. = 1 %then set tipo_reporte = '28';
		where tipo_reporte = 'NULL' and (Escala_noDisCog < 100
		or compress(id_ent_territorial || put(grado, 1.)) in (select keyIntgr from ft.auxCES_&_ESCALA_NAME. where n_granItem < 5));
	%end;

  %LET jj = %EVAL(&jj. + 1);
%END;
/* Tipo de reporte 30 pais*/
update ft.conteo_&_ESCALA_NAME._6 set tipo_reporte = '30';	
quit;
/*******************************************************************************************************/
/* TIPOS PARA AGREGADOS */
/*******************************************************************************************************/
PROC SQL;
*Tipo de reporte 10 Sejo (Problemas cognitivo);
update ft.conteo_&_ESCALA_NAME._6 set tipo_reporte = '30';
quit;
%mend makeTipos;
*%makeTipos(_ESCALA_NAME = ACDISC, flagItem = 1);
*%makeTipos(_ESCALA_NAME = ACTCORR, flagItem = 1);

%macro parEscalaEspecial(codificaInd, _ESCALA_NAME);
	proc sql noprint;
	%let colGrado = grado5 grado9;
	%let numGrado = 5 9;
	%let zz = 1;
			
	%DO %UNTIL(NOT %LENGTH(%SCAN(&colGrado, &zz.)));
		%let aux_gr = %SCAN(&colGrado, &zz.);
		%let aux_grII = %SCAN(&numGrado, &zz.); 

		select NOMBRE, cod_item into :_nom_items SEPARATED by ' _', :_cod_items SEPARATED by ' _'
		from &codificaInd. where NOMBRE like "&_ESCALA_NAME%" and cod_item ^= 'NULL' and &aux_gr. = '1';
		%let nitems = %sysfunc(count(_&_nom_items., _));

		update inpath.INFO set par&_ESCALA_NAME._V01 = (CASE WHEN 
		%do jj = 1 %to &nitems.;
		par%substr(%sysfunc(scan(_&_nom_items., &jj.)), 2, %EVAL(%LENGTH(%sysfunc(scan(_&_nom_items.,&jj.))) - 1))
		%if &jj. < &nitems. %then  = 1 and;
		%if &jj. = &nitems. %then  = 1;
		%end;
		THEN 1 ELSE 0 end) where grado = &aux_grII.;

		update inpath.INFO set par&_ESCALA_NAME._V02 = (CASE WHEN
		%do jj=1 %to &nitems.;
		par%substr(%sysfunc(scan(_&_nom_items., &jj.)), 2, %EVAL(%LENGTH(%sysfunc(scan(_&_nom_items.,&jj.))) - 1))
		%if &jj. < &nitems. %then  = 1 or;
		%if &jj. = &nitems. %then  = 1;
		%end;
		THEN 1 ELSE 0 end) where grado = &aux_grII.;
		*update inpath.INFO set par&_ESCALA_NAME._V02 = (CASE WHEN par&_ESCALA_NAME._V01 = &nitems. THEN 1 ELSE 0 end);
		*update inpath.INFO set par&_ESCALA_NAME._V01 = (CASE WHEN par&_ESCALA_NAME._V01 > 0. THEN 1 ELSE 0 end);

		update inpath.INFO set peso&_ESCALA_NAME._V01 = (CASE WHEN 
		%do jj=1 %to &nitems.;
		peso%substr(%sysfunc(scan(_&_nom_items., &jj.)), 2, %EVAL(%LENGTH(%sysfunc(scan(_&_nom_items.,&jj.))) - 1))
		%if &jj. < &nitems. %then > 0 or;
		%if &jj. = &nitems. %then > 0;
		%end;
		THEN 1 ELSE 0 end) where grado = &aux_grII.;

		/*Con la condicion de que todos los pesos sean mayor que 0 da lo mismo que tener la condición al menos un peso es mayor que 0*/
		/*update inpath.INFO set peso&_ESCALA_NAME._V02 = (CASE WHEN 
		%do i = 1 %to &nitems.;
		peso%substr(%sysfunc(scan(_&_nom_items., &i.)), 2, %EVAL(%LENGTH(%sysfunc(scan(_&_nom_items.,&i.))) - 1))
		%if &i. < &nitems. %then > 0 and;
		%if &i. = &nitems. %then > 0;
		%end;
		THEN 1 ELSE 0 end) where grado = &aux_grII.;*/	
		%LET zz = %EVAL(&zz. + 1);
	%END;
	update inpath.INFO set peso&_ESCALA_NAME._V02 = peso&_ESCALA_NAME._V01;
	quit;
%mend parEscalaEspecial;
*%parEscalaEspecial(INPATH.Codificaind , PERSEG);

%macro Crea_ficha_Tecnica(codificaInd);
%put ....Procesando Ficha Tecnica con : &codificaInd.;
/* Conteos por agregados*/
%LET tipAgregados = daneEstab enteId mpio depto 'NULL';
%LET i = 1;
PROC SQL noprint;
%DO %UNTIL(NOT %LENGTH(%SCAN(&tipAgregados,&i.)));
	%let auxAgre = %SCAN(&tipAgregados, &i.);
	%if &i. = 1 %then CREATE TABLE INPATH.conteoAgrega as ;
	%if &i. > 1 %then INSERT INTO INPATH.conteoAgrega;
	%if &auxAgre. = depto %then select "NULL" as id_institucion, '1' || %SCAN(&tipAgregados, &i.) as id_ent_territorial, ;
	%if &auxAgre. ^= depto and &auxAgre. ^= daneEstab %then select "NULL" as id_institucion, %SCAN(&tipAgregados, &i.) as id_ent_territorial, ;
	%if &auxAgre. = daneEstab %then select %SCAN(&tipAgregados, &i.) as id_institucion, "NULL" as id_ent_territorial,;
    grado, 
	%if %SCAN(&tipAgregados, &i.) ^= 'NULL' %then "&auxAgre" as id_tip_Agr,;
	%if %SCAN(&tipAgregados, &i.) = 'NULL' %then "NULL" as id_tip_Agr,;
	count(DISTINCT daneEstab) as establecimientos,
	count(DISTINCT codSitio) as sedes,
	count(DISTINCT CASE WHEN sector = '1' then daneEstab else '' end) as inst_oficiales,
	count(DISTINCT CASE WHEN sector = '2' then daneEstab else '' end) as inst_no_oficiales,
	count(DISTINCT CASE WHEN zonaEstab = '1' then daneEstab else '' end) as inst_urbanos,
	count(DISTINCT CASE WHEN zonaEstab = '2' then daneEstab else '' end) as inst_rurales,
	count(DISTINCT CASE WHEN nse = '1' then daneEstab else '' end) as inst_nse1,
	count(DISTINCT CASE WHEN nse = '2' then daneEstab else '' end) as inst_nse2,
	count(DISTINCT CASE WHEN nse = '3' then daneEstab else '' end) as inst_nse3,
	count(DISTINCT CASE WHEN nse = '4' then daneEstab else '' end) as inst_nse4,
	count(DISTINCT CASE WHEN sector = '1' and zonaEstab = '1' then daneEstab else '' end) as inst_of_urb,
	count(DISTINCT CASE WHEN sector = '1' and zonaEstab = '2' then daneEstab else '' end) as inst_of_rur,
	count(DISTINCT CASE WHEN sector = '2' and zonaEstab = '1' then daneEstab else '' end) as inst_noof_urb,
	count(DISTINCT CASE WHEN sector = '2' and zonaEstab = '2' then daneEstab else '' end) as inst_noof_rur
	from ft.auxConteo group by 
    %if %SCAN(&tipAgregados, &i.) ^= 'NULL' %then %SCAN(&tipAgregados, &i.), grado;
    %if %SCAN(&tipAgregados, &i.) = 'NULL' %then grado;
	;
	%if &i. = 1 %then alter table INPATH.conteoAgrega modify id_ent_territorial char(5) format=$5.;;
	%LET i = %EVAL(&i.+1);
	%END;
QUIT;

/* Determinando cuales son escalas y escalas especiales */
proc sql noprint;
  select NOMBRE, cod_escala, n_items  into :_nom_escalas SEPARATED by ' _', :_cod_escalas SEPARATED by ' _',
  										   :_num_items SEPARATED by ' _' 
  from (select distinct NOMBRE, cod_escala, cod_item, count(distinct cod_item) as n_items
  from &codificaInd. group by cod_escala) where cod_item = 'NULL' and 
  cod_escala in (select distinct cod_escala from &codificaInd. where grado5 = '1' or grado9 = '1');

/* Creando columnas de participación y pesos para items*/
	alter table inpath.INFO 
	%do i=1 %to %sysfunc(count(_&_nom_escalas., _));
	  %let nombre   = %substr(%sysfunc(scan(_&_nom_escalas., &i.)), 2, %EVAL(%LENGTH(%sysfunc(scan(_&_nom_escalas.,&i.))) - 1));
	  %let ind_item = %substr(%sysfunc(scan(_&_num_items., &i.)), 2, %EVAL(%LENGTH(%sysfunc(scan(_&_num_items.,&i.))) - 1));
	  %if &ind_item. ^= 1 %then %do;
		  add par&nombre._V01 num format = best12.
	      add par&nombre._V02 num format = best12.
	      add peso&nombre._V01 num format = best12.
	      add peso&nombre._V02 num format = best12.
	  %end;
	%end;
	;
quit;

/* Iterando para escalas */
    %do i=1 %to %sysfunc(count(_&_nom_escalas., _));
      %let nombre   = %substr(%sysfunc(scan(_&_nom_escalas., &i.)), 2, %EVAL(%LENGTH(%sysfunc(scan(_&_nom_escalas.,&i.))) - 1));
	  %let codigo   = %substr(%sysfunc(scan(_&_cod_escalas., &i.)), 2, 3);
	  %let ind_item = %substr(%sysfunc(scan(_&_num_items., &i.)), 2, %EVAL(%LENGTH(%sysfunc(scan(_&_num_items.,&i.))) - 1));
	  %put &ind_item.;
	  %if &ind_item. = 1 %then %do;
	  	%put --------------------- Construcción Ficha Tecnica para la Escala &nombre.;
		%dropAuxEscala(libra = ft, _ESCALA_NAME = &nombre.);
	  	%conteosEscala(_ESCALA_NAME = &nombre., _COD_ESCALA = &codigo., _IND_PEGV02 = 0);
		%makeTipos(_ESCALA_NAME = &nombre., flagItem = &ind_item.);
	  %end;
	  %else %do;
	  	%put --------------------- Construcción Ficha Tecnica para la Escala &nombre. -- Escala Especial --;
	    %parEscalaEspecial(&codificaInd., &nombre.);
		%dropAuxEscala(libra = ft, _ESCALA_NAME = &nombre._V01);
	  	%conteosEscala(_ESCALA_NAME = &nombre._V01, _COD_ESCALA = &codigo., _IND_PEGV02 = 0);
		%makeTipos(_ESCALA_NAME = &nombre._V01, flagItem = &ind_item.);
		%conteosEscala(_ESCALA_NAME = &nombre._V02, _COD_ESCALA = &codigo., _IND_PEGV02 = 1);
	  %end;
    %end;

/* Consolidando Ficha Tecnica*/
%do i=1 %to %sysfunc(count(_&_nom_escalas., _));
	%let nombre   = %substr(%sysfunc(scan(_&_nom_escalas., &i.)), 2, %EVAL(%LENGTH(%sysfunc(scan(_&_nom_escalas.,&i.))) - 1));
	%let ind_item = %substr(%sysfunc(scan(_&_num_items., &i.)), 2, %EVAL(%LENGTH(%sysfunc(scan(_&_num_items.,&i.))) - 1));
	* Creando tablas finales de Ficha Tecnica;
	%if &i. = 1 %then %do;
		data ft.FichaTCC;
	  		set ft.conteo_&nombre._1;
			stop;
		run;
		proc sql;
			alter table ft.FichaTCC 
				modify id_ent_territorial char(5) format=$5.
				modify id_institucion char(12) format=$12.
				modify id_sede char(6) format=$6.;
		quit;
		data ft.FichaTS;
	  		set ft.conteo_&nombre._1;
			stop;
		run;
		data ft.FichaTE;
	  		set ft.conteo_&nombre._2;
			stop;
		run;
		data ft.FichaTAgregados;
	  		set ft.conteo_&nombre._3;
			stop;
		run;
		data ft.FichaTMPIO;
	  		set ft.conteo_&nombre._3;
			stop;
		run;
		data ft.FichaTDPTO;
	  		set ft.conteo_&nombre._4;
			stop;
		run;
		data ft.FichaTET;
	  		set ft.conteo_&nombre._5;
			stop;
		run;
		data ft.FichaNAC;
	  		set ft.conteo_&nombre._6;
			stop;
		run;

		* Creando tabla de establecimiento pequenio;
		data ft.EstabPequenioCC(rename = (t_Escala_noDisCog = participantes t_Escala_DisCog = participantes_dis_cog t_Escala = total_participantes));
	  		set ft.conteo_&nombre._1(keep= id_institucion grado cod_escala escala t_Escala_noDisCog	
                                           t_Escala_DisCog t_Escala tipo_reporte);	
			stop;
		run;
	%end;

	* Haciendo append de las fichas;
	%if &ind_item. > 1 %then %let nombre = &nombre._V02;
	%do jj=1 %to 6;
	proc append base= ft.FichaTCC data = ft.conteo_&nombre._&jj. force;run;
	%end;
	proc append base= ft.FichaTS data = ft.conteo_&nombre._1; 
	proc append base= ft.FichaTE data = ft.conteo_&nombre._2;
	proc append base= ft.FichaTMPIO data = ft.conteo_&nombre._3;
	proc append base= ft.FichaTDPTO data = ft.conteo_&nombre._4;
	proc append base= ft.FichaTET data = ft.conteo_&nombre._5;
	proc append base= ft.FichaNAC data = ft.conteo_&nombre._6;

	proc append base= ft.FichaTAgregados data = ft.conteo_&nombre._3;
	proc append base= ft.FichaTAgregados data = ft.conteo_&nombre._4;
	proc append base= ft.FichaTAgregados data = ft.conteo_&nombre._5;

    run;

	* Append Estabpequeniocc;
	proc sql;
		create table auxEstabPe as select id_institucion, grado, cod_escala, escala, t_Escala_noDisCog as participantes, 
    	t_Escala_DisCog as participantes_dis_cog, t_Escala as total_participantes, tipo_reporte 
		from ft.conteo_&nombre._2 where tipo_reporte in ('02', '13', '14', '15', '16');
	quit;
	proc append base= ft.EstabPequenioCC data = auxEstabPe;
	run;
%end;
proc sql;
create table ft.erroresFTCC as select * from ft.FichaTCC where tipo_reporte = 'NULL';
quit;
%mend Crea_ficha_Tecnica;
%Crea_ficha_Tecnica(INPATH.Codificaind);
***Sección para guardar en txt las salidas del proceso de fichas Tecnicas;
PROC IMPORT OUT= INPATH.PARCHAR_FTCC  /*Lectura parchar*/
            DATAFILE= "&PARCHA_PATH" 
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
PROC SQL; *Agregando numero de sedes y numero de establecimientos a los tipos 11, 12, 13, 14, 15, 16;
/*create table fichatcc_Aux as select a.*, b.establecimientos as new_establecimiento, b.sedes as new_sedes from 
ft.fichatcc a left join (select * from inpath.conteoagrega where id_institucion ^= "NULL") b on (a.id_institucion = b.id_institucion and a.grado = b.grado and a.id_sede = b.id_ent_territorial);

update fichatcc_Aux set establecimientos = new_establecimiento where id_institucion ^= "NULL" and id_sede = "NULL" and new_establecimiento ^= .;
update fichatcc_Aux set sedes = new_sedes where id_institucion ^= "NULL" and id_sede = "NULL" and new_sedes ^= .;

update fichatcc_Aux set establecimientos = 1 where id_institucion ^= "NULL" and id_sede = "NULL" and new_establecimiento = .;
update fichatcc_Aux set sedes = 1 where id_institucion ^= "NULL" and id_sede = "NULL" and new_sedes = .;

update fichatcc_Aux set establecimientos = . where id_institucion ^= "NULL" and id_sede = "NULL" and new_establecimiento = . and tipo_reporte = "01";
update fichatcc_Aux set sedes = . where id_institucion ^= "NULL" and id_sede = "NULL" and new_sedes = . and tipo_reporte = "01";

alter table fichatcc_Aux drop COLUMN new_establecimiento;
alter table fichatcc_Aux drop COLUMN new_sedes; */

create table ft.error_establecimiento as select * from ft.fichatcc where tipo_reporte in ('11', '12', '13', '14', '15', '16') and (establecimientos = . or sedes = .);
quit;
PROC SQL; * Parchar Establecimiento pequeños comparación historica;
*select * from ft.FichaTCC where compress(id_institucion || cod_escala || put(grado, 1.)) in (select compress(put(id_institucion, 12.) || cod_escala || put(grado, 1.)) from INPATH.PARCHAR_FTCC where cod_item = 'NULL') and id_sede = 'NULL';
*select * from ft.FichaTCC where compress(id_institucion || cod_escala || put(grado, 1.)) in (select compress(put(id_institucion, 12.) || cod_escala || put(grado, 1.)) from INPATH.PARCHAR_FTCC where cod_item ^= 'NULL') and id_sede = 'NULL';
update ft.FichaTCC set tipo_reporte = '13' where compress(id_institucion || cod_escala || put(grado, 1.)) in 
(select compress(put(id_institucion, 12.) || cod_escala || put(grado, 1.)) 
from INPATH.PARCHAR_FTCC where cod_item = 'NULL') and id_sede = 'NULL';

update ft.FichaTCC set tipo_reporte = '14' where compress(id_institucion || cod_escala || put(grado, 1.)) 
in (select compress(put(id_institucion, 12.) || cod_escala || put(grado, 1.)) 
from INPATH.PARCHAR_FTCC where cod_item ^= 'NULL') and id_sede = 'NULL';
quit;

data _null_; * Guardando salida Ficha Tecnica CC;
%MACRO saveFichas(FILE);

%let codificaInd = INPATH.Codificaind;
%LET DATA_SET = &FILE;
%LET OPEN_DATA=%SYSFUNC(OPEN(&DATA_SET));
%LET OBSERVATIONS= %SYSFUNC(ATTRN(&OPEN_DATA, NOBS));
%LET CLOSE_DATASET=%SYSFUNC(CLOSE(&OPEN_DATA));

*Completitud de las escalas y arreglo formatos para salidas;
PROC SQL;

create table copy_ft as
select * from &FILE.;
delete * from copy_ft;

/* registros nulos*/
insert into copy_ft 
values ("NULL", "NULL", "NULL", 5, "NULL", ., ., ., ., ., ., ., ., ., ., ., ., ., ., 
., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., "NULL", "NULL")
values ("NULL", "NULL", "NULL", 9, "NULL", ., ., ., ., ., ., ., ., ., ., ., ., ., ., 
., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., "NULL", "NULL");

/* Encontrando registros que deben tener reporte 01*/
%if &FILE ^= ft.FichaTE %then %do;
	CREATE TABLE Auxi2 as 
	select &GLOBAL_ANO. as anio, &GLOBAL_APLICACION. as aplicacion, &GLOBAL_MODO. as modo, C.*, A.*, B.* from 
	(select distinct id_ent_territorial, id_institucion, id_sede from &FILE.) A, (select * from copy_ft) B, 
	(select distinct cod_escala from &codificaInd. where grado5 = '1' or grado9 = '1') C;
%end;

%if &FILE = ft.FichaTE %then %do;
	CREATE TABLE Auxi2 as 
	select &GLOBAL_ANO. as anio, &GLOBAL_APLICACION. as aplicacion, &GLOBAL_MODO. as modo, C.*, A.*, B.* from 
	(select distinct 'NULL' as id_ent_territorial, id_institucion as id_institucion, 'NULL' as id_sede 
 	from INPATH.Ficha_tecnica_cog) A, (select * from copy_ft) B, 
	(select distinct cod_escala from &codificaInd. where grado5 = '1' or grado9 = '1') C;
%end;

%if &FILE = ft.FichaTCC or &FILE = Fichatcc_aux %then %do;
	create table auxi2_Completa as 
	select &GLOBAL_ANO. as anio, &GLOBAL_APLICACION. as aplicacion, &GLOBAL_MODO. as modo, C.*, A.*, B.* from 
	(select distinct 'NULL' as id_ent_territorial, id_institucion, 'NULL' as id_sede 
 	from INPATH.Ficha_tecnica_cog) A, (select * from copy_ft) B, 
    (select distinct cod_escala from &codificaInd. where grado5 = '1' or grado9 = '1') C;
	delete from auxi2_Completa where id_institucion in (select distinct id_institucion from Auxi2 where id_institucion ^= 'NULL');
	INSER INTO Auxi2 select * from auxi2_Completa;
%end;

create table &FILE._repo01 as
select * FROM Auxi2 WHERE (Id_ent_territorial || id_institucion || Id_sede || '-'|| put(grado, 1.) || cod_escala)
NOT IN (select (Id_ent_territorial || id_institucion || Id_sede || '-'|| put(grado, 1.) || cod_escala) from &FILE.);

update &FILE._repo01 set tipo_reporte = '01';

*Eliminando si hay filas de registros nacionales;
delete from &FILE._repo01 where id_sede = "NULL" and  id_institucion = "NULL" and id_ent_territorial = "NULL";

* Cargando en ficha tecnica tipos de reporte 01;
insert into &FILE 
select id_ent_territorial, id_institucion, id_sede, grado, cod_escala, ninos_participantes, 
ninas_participantes, noEspecifica_participantes, t_Presentes, Presentes, t_Escala, 
t_Escala_noDisCog, t_Escala_DisCog, Escala, Escala_noDisCog, Escala_DisCog, establecimientos, 
sedes, inst_oficiales, inst_no_oficiales, inst_urbanos, inst_rurales, inst_nse1, inst_nse2, 
inst_nse3, inst_nse4, estud_oficiales, estud_no_oficiales, estud_urbanos, estud_rurales, 
estud_nse1, estud_nse2, estud_nse3, estud_nse4, inst_of_urb, inst_of_rur, inst_noof_urb, 
inst_noof_rur, estud_of_urb, estud_of_rur, estud_noof_urb, estud_noof_rur, iden_registro, 
tipo_reporte from &FILE._repo01;

* Seleccion de campos para salida;
create table Ficha_Tecnica_Salida as 
select &GLOBAL_ANO. as anio, &GLOBAL_APLICACION. as aplicacion, 
&GLOBAL_MODO. as modo, ID_ENT_TERRITORIAL, ID_INSTITUCION, ID_SEDE, GRADO, cod_escala,
strip(put(t_Escala, best32.)) as total_participantes, 
strip(put(t_Escala_noDisCog, best32.)) as participantes, 
strip(put(t_Escala_DisCog, best32.)) as participantes_dis_cog, 
strip(put(ninos_participantes, best32.)) as ninos_participantes, 
strip(put(ninos_participantes, best32.)) as ninas_participantes, 
strip(put(establecimientos, best32.)) as establecimientos,
strip(put(sedes, best32.)) as sedes, strip(put(inst_oficiales, best32.)) as inst_oficiales, 
strip(put(inst_no_oficiales, best32.)) as inst_no_oficiales, strip(put(inst_urbanos, best32.)) as inst_urbanos, 
strip(put(inst_rurales, best32.)) as inst_rurales, strip(put(inst_nse1, best32.)) as inst_nse1, 
strip(put(inst_nse2, best32.)) as inst_nse2, strip(put(inst_nse3, best32.)) as inst_nse3, 
strip(put(inst_nse4, best32.)) as inst_nse4, strip(put(estud_oficiales, best32.)) as estud_oficiales, 
strip(put(estud_no_oficiales, best32.)) as estud_no_oficiales, strip(put(estud_urbanos, best32.)) as estud_urbanos, 
strip(put(estud_rurales, best32.)) as estud_rurales, strip(put(estud_nse1, best32.)) as estud_nse1, 
strip(put(estud_nse2, best32.)) as estud_nse2, strip(put(estud_nse3, best32.)) as estud_nse3, 
strip(put(estud_nse4, best32.)) as estud_nse4, strip(put(inst_of_urb, best32.)) as inst_of_urb, 
strip(put(inst_of_rur, best32.)) as inst_of_rur, strip(put(inst_noof_urb, best32.)) as inst_noof_urb, 
strip(put(inst_noof_rur, best32.)) as inst_noof_rur, strip(put(estud_of_urb, best32.)) as estud_of_urb, 
strip(put(estud_of_rur, best32.)) as estud_of_rur, strip(put(estud_noof_urb, best32.)) as estud_noof_urb, 
strip(put(estud_noof_rur, best32.)) as estud_noof_rur, tipo_reporte
from &FILE.;

* Arreglando tipo de reporte para sede;
update Ficha_Tecnica_Salida set id_institucion = 'NULL' where  id_sede ^= 'NULL';
quit;

PROC SORT DATA = Ficha_Tecnica_Salida ;
	BY id_ent_territorial id_institucion id_sede cod_escala;
RUN;

PROC SQL;
* Cambiando formato ente, sede e institucion;
alter table Ficha_Tecnica_Salida modify id_ent_territorial char(5) format=$5.;
alter table Ficha_Tecnica_Salida modify id_sede char(6) format=$6.;
alter table Ficha_Tecnica_Salida modify id_institucion char(12) format=$12.;

* Cambiando faltantes por NULL;
update Ficha_Tecnica_Salida set total_participantes = 'NULL' where total_participantes = '.';
update Ficha_Tecnica_Salida set participantes = 'NULL' where participantes = '.';
update Ficha_Tecnica_Salida set participantes_dis_cog = 'NULL' where participantes_dis_cog = '.';
update Ficha_Tecnica_Salida set ninos_participantes = 'NULL' where ninos_participantes = '.';
update Ficha_Tecnica_Salida set ninas_participantes = 'NULL' where ninas_participantes = '.';
update Ficha_Tecnica_Salida set establecimientos = 'NULL' where establecimientos = '.';
update Ficha_Tecnica_Salida set sedes = 'NULL' where sedes = '.';
update Ficha_Tecnica_Salida set inst_oficiales = 'NULL' where inst_oficiales = '.';
update Ficha_Tecnica_Salida set inst_no_oficiales = 'NULL' where inst_no_oficiales = '.';
update Ficha_Tecnica_Salida set inst_urbanos = 'NULL' where inst_urbanos = '.';
update Ficha_Tecnica_Salida set inst_rurales = 'NULL' where inst_rurales = '.';
update Ficha_Tecnica_Salida set inst_nse1 = 'NULL' where inst_nse1 = '.';
update Ficha_Tecnica_Salida set inst_nse2 = 'NULL' where inst_nse2 = '.';
update Ficha_Tecnica_Salida set inst_nse3 = 'NULL' where inst_nse3 = '.';
update Ficha_Tecnica_Salida set inst_nse4 = 'NULL' where inst_nse4 = '.';
update Ficha_Tecnica_Salida set estud_oficiales = 'NULL' where estud_oficiales = '.';
update Ficha_Tecnica_Salida set estud_no_oficiales = 'NULL' where estud_no_oficiales = '.';
update Ficha_Tecnica_Salida set estud_urbanos = 'NULL' where estud_urbanos = '.';
update Ficha_Tecnica_Salida set estud_rurales = 'NULL' where estud_rurales = '.';
update Ficha_Tecnica_Salida set estud_nse1 = 'NULL' where estud_nse1 = '.';
update Ficha_Tecnica_Salida set estud_nse2 = 'NULL' where estud_nse2 = '.';
update Ficha_Tecnica_Salida set estud_nse3 = 'NULL' where estud_nse3 = '.';
update Ficha_Tecnica_Salida set estud_nse4 = 'NULL' where estud_nse4 = '.';
update Ficha_Tecnica_Salida set inst_of_urb = 'NULL' where inst_of_urb = '.';
update Ficha_Tecnica_Salida set inst_of_rur = 'NULL' where inst_of_rur = '.';
update Ficha_Tecnica_Salida set inst_noof_urb = 'NULL' where inst_noof_urb = '.';
update Ficha_Tecnica_Salida set inst_noof_rur = 'NULL' where inst_noof_rur = '.';
update Ficha_Tecnica_Salida set estud_of_urb = 'NULL' where estud_of_urb = '.';
update Ficha_Tecnica_Salida set estud_of_rur = 'NULL' where estud_of_rur = '.';
update Ficha_Tecnica_Salida set estud_noof_urb = 'NULL' where estud_noof_urb = '.';
update Ficha_Tecnica_Salida set estud_noof_rur = 'NULL' where estud_noof_rur = '.';
update Ficha_Tecnica_Salida set tipo_reporte = 'NULL' where tipo_reporte = '.';
quit;

*Salida archivos separados por tabulaciones;
%if &OBSERVATIONS > 0 %then %do;
	PROC EXPORT DATA= Ficha_Tecnica_Salida /*Salida de ficha Tecnica en .txt*/
            OUTFILE= "&PATH_PROJECT\OUTPUT\&FILE..txt"
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
	RUN;
%end;
%MEND;

%saveFichas(ft.FichaTS);   /*Ficha tecnica de Sedes*/
%saveFichas(ft.FichaTE);   /*Ficha tecnica de Establecimientos*/
%saveFichas(ft.FichaTMPIO);/*Ficha tecnica de Municipios*/
%saveFichas(ft.FichaTDPTO);/*Ficha tecnica de Departamentos*/
%saveFichas(ft.FichaTET);  /*Ficha tecnica de Entes*/
%saveFichas(ft.FichaTAgregados); /*Ficha tecni  ca de agregados*/
%saveFichas(ft.FichaNAC);  /*Ficha tecnica de Nacional*/
%saveFichas(ft.erroresFTCC); /* Registros que quedaron sin tipo de reporte*/
%saveFichas(ft.FichaTCC);  /*Ficha tecnica Total*/
%saveFichas(Fichatcc_aux);
run;

 /* Guardando errores de peso PROC SQL; SELECT COUNT(*) INTO:OBSERVATIONS FROM FT.&FILE;*/
data _null_; *Reporte de hallazgos al hacer agregaciones;
%MACRO saveLog(FILE, LIBNAME, logFlag, sepTab);
%LET DATA_SET = &LIBNAME..&FILE;
%LET OPEN_DATA=%SYSFUNC(OPEN(&DATA_SET));
%LET OBSERVATIONS= %SYSFUNC(ATTRN(&OPEN_DATA, NOBS));
%LET CLOSE_DATASET=%SYSFUNC(CLOSE(&OPEN_DATA));
%if &OBSERVATIONS > 0 %then %do;
	%if &sepTab = 1 %then %do;
		PROC EXPORT DATA= &LIBNAME..&FILE
			%if &logFlag = 1 %then OUTFILE= "&PATH_PROJECT\OUTPUT\log_&FILE..csv";
			%if &logFlag = 0 %then OUTFILE= "&PATH_PROJECT\OUTPUT\&FILE..txt";
			DBMS=TAB REPLACE;
		PUTNAMES=YES;
	%end;
	%if &sepTab = 0 %then %do;
		PROC EXPORT DATA= &LIBNAME..&FILE
			%if &logFlag = 1 %then OUTFILE= "&PATH_PROJECT\OUTPUT\log_&FILE..csv";
			%if &logFlag = 0 %then OUTFILE= "&PATH_PROJECT\OUTPUT\&FILE..txt";
			DBMS=CSV REPLACE;
		PUTNAMES=YES;
	%end;
%end;
%MEND saveLog;
%saveLog(EstabPequenioCC, ft, 0, 1); /*Isumo para ficha de evaluacion*/
%saveLog(hallazgosIniciales, inpath, 1, 0); /* Registros que no cruzan*/
%saveLog(hallazgosMG, inpath, 1, 0);    /*No cruzan con Matricula*/
%saveLog(hallazgosINFO, inpath, 1, 0);  /*Errores al cruzan toda la informacion*/
%saveLog(hallazgosEliminados, inpath, 1, 0);  /*Errores al cruzan toda la informacion (No cruza el CODSABER - daneESTAB) y tienen Peso */
%saveLog(hallazgosMunicipio, ft, 1, 0); /*DaneEstab que tienen dos municipios o mas en Biblia*/
%saveLog(Error_establecimiento, ft, 1, 0);
run;

