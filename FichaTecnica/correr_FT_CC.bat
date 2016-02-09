:: Se debe especificar la ruta en la compartida donde se encuentran las bases a leer (Cambiar el archivo parametros.sas)
:: Se debe especificar la ruta de trabajo (donde esta la carpeta) (Cambiar el archivo parametros.sas)
:: Se ejecuta Correr_FTC con un parametro que tiene tres posibles valores :
::			('Lectura' solo lee las bases, 'Proceso'  Si ya se estan las bases y se quiere generar Ficha Tecnica y Ficha Evaluados
::           'Revision' comparar Ficha Tecnica y Ficha de Evaluados
::           'Todo' si se quiere la lectura de las bases y genera las Fichas)

@ECHO off
IF [%1] == [] goto correr_All

for %%i in (Todo Lectura) do if %1 == %%i del /q "BASES\SAS" "lectBases_CC.log"
for %%i in (Todo Proceso) do if %1 == %%i del /q "output" "fichaTecnica_CC.log"
for %%i in (Todo Proceso) do if %1 == %%i del /q "output\sas" "validacionFT_CC.log"

for %%i in (Todo Lectura) do if %1 == %%i "C:\Program Files\SASHome\SASFoundation\9.4\sas.exe" -sysin src\lectBases_CC.sas -ICON -NOSPLASH -sysparm "parametros.sas"
for %%i in (Todo Proceso) do if %1 == %%i "C:\Program Files\SASHome\SASFoundation\9.4\sas.exe" -sysin src\fichaTecnica_CC.sas -ICON -NOSPLASH -sysparm "parametros.sas"
for %%i in (Todo Revision) do if %1 == %%i "C:\Program Files\SASHome\SASFoundation\9.4\sas.exe" -sysin src\validacionFT_CC.sas -ICON -NOSPLASH -sysparm "parametros.sas"

goto Finalizar

:correr_All
ECHO OjO...... Se debe pasar como parametro 'Todo', 'Lectura', 'Proceso' y 'Revision'

:Finalizar
ECHO El proceso finalizo, el parametro fue --%1--
