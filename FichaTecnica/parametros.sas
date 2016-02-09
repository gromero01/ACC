/**************************************************************************************************
* * parametros.SAS
* * AUTOR: Jorge Mario Carrasco Ortiz
* * 
* * SABER 3° 5° 9° 
* * DESCRIPCIÓN: Parametros para la construcción de la Ficha tecnica de las pruebas 
* *              Cognitivas de 3, 5, 9. Se debe especificar:
* * 			 'PATH_PROJECT' La ubicación de la carpeta de trabajo
* *              'BASE_PATH' La ubicación de las bases que se tienen que leer
* *              'GLOBAL_ANO' El periodo de la aplicación
* *              'GLOBAL_APLICACION' Generalmente es la letra 'C'
*****************************************************************************************************/

%LET PATH_PROJECT  = C:\PRODUCTO_JMC_2015\20141_FICHA_TECNICA_CC;
%LET BASE_PATH_COG = \\icfesserv5\academica$\SABER\SABER_2014\CENSAL\FUENTES\BASES\FINAL;
%LET BASE_PATH     = \\icfesserv5\faycc$\CC\2015\BASES2014\OUTPUT;
%LET INDICADORAS_PATH = indicadorasCC_V03.txt; *Deben estar en BASE_PATH;
%LET PESOS_PATH = pesosCensalCCV04.txt; *Deben estar en BASE_PATH;
%LET CODIFICA_PATH = codificaIndices2014.txt; *Deben estar en BASE_PATH;

* Lectura de Ficha Tecnica Cognitivo (Se debe especificar toda la ruta);
%LET FTC_PATH = \\icfesserv5\academica$\SABER\SABER_2014\CENSAL\20141_FICHA_TECNICA_COGNITIVO\output\Tecnica.txt; 

%LET GLOBAL_ANO = "2014";
%LET GLOBAL_APLICACION = "2";
%LET GLOBAL_MODO = "C";
%LET EVAL_PATH = \\icfesserv5\faycc$\CC\2015\BASES2014\Evaluacion-Calif.txt;
%LET PARCHA_PATH = \\icfesserv5\faycc$\CC\2015\BASES2014\OUTPUT\parcharFT2014.txt;
