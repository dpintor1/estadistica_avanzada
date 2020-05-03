Trabajo Métodos Estadísticos Avanzados
================
Carlos Andres Cuartas Murillo, Carlos Alberto Cerro Espinal, Daniel Enrique Pinto Restrepo, Daniel Roman, Santiago Mejía Chitiva
9 de abril de 2020

``` r
Sys.setlocale("LC_TIME","Spanish")
```

    ## [1] "Spanish_Spain.1252"

Antes de empezar es necesario establecer las librerías que usaremos a lo largo del trabajo, ya sea para manipular los datos, leer los archivos externos y gráficar los diferentes datos.

``` r
library(readxl)
library(ggplot2)
library(knitr)
library(lme4)
```

    ## Loading required package: Matrix

``` r
library(nlme)
```

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmList

``` r
library(reshape2)
library(reshape)
```

    ## 
    ## Attaching package: 'reshape'

    ## The following objects are masked from 'package:reshape2':
    ## 
    ##     colsplit, melt, recast

    ## The following object is masked from 'package:Matrix':
    ## 
    ##     expand

``` r
library(corrgram)
```

    ## Registered S3 method overwritten by 'seriation':
    ##   method         from 
    ##   reorder.hclust gclus

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:reshape':
    ## 
    ##     rename

    ## The following object is masked from 'package:nlme':
    ## 
    ##     collapse

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
```

    ## 
    ## Attaching package: 'tidyr'

    ## The following objects are masked from 'package:reshape':
    ## 
    ##     expand, smiths

    ## The following object is masked from 'package:reshape2':
    ## 
    ##     smiths

    ## The following objects are masked from 'package:Matrix':
    ## 
    ##     expand, pack, unpack

Introducción
============

En este documento Rmarkdown se realiza todo el código para el trabajo de métodos estadísticos avanzados. Dentro del docoumento se hace una guía de toda la descarga de las bases de datos de supersociedades, la carga de las variable macroeconómicas, las diferentes transformaciones y desarrollo de modelos que se realizan en el trabajo. Adelantadonos un poco, nos enfocamos en el sector de cultivo de especies de flor de corte. El documento sigue de la siguiente forma: Primero manipulamos las bases de datos desde el 2015 hasta 2018 de supersociedades, formamos una gran base de datos, con base en la disponibilidad de empresas e información de costos, tomamos una decisión de un sector. Luego de esto, hacemos las diferentes transofrmaciones de nuestras variables respuesta y nuestras variables macroecnómicas y se realiza un análisis exploratorio de los datos. Finalmente, se realizan dos tipos de metodologías, la primera es la regresión lineal múltiple y luego pasamos a los modelos de efectos mixtos, dichas metodlogías se aplican para las 3 variables respuesta que tenemos en nuestra base de datos.

Metodología
===========

Lo Datos
--------

Para el desarrollo del presente trabajo, obtenemos dos fuentes de datos, para los costos de ventas, que es nuestra variable objetivo de las empresas, utilizamos la página de la [Superintendencia de Sociedades](http://pie.supersociedades.gov.co/Pages/default.aspx#/). El procedimiento para el tratamiento de los datos es el siguiente, utilizaremos los datos del año 2016 y los del año 2018, debido a que en este último también aparecen las variables del periodo anterior, luego obtendremos de estos archivos un diccionario de códigos ciiu. Al final del procedimiento, obtendremos una base de datos que tiene las variables: Nit, Punto Entrada, Costos de Venta y ciiu.

Luego para las variables macroeconómicas, utilizamos Bloomberg, de acá obtenemos el crecimiento del PIB, inflación, balance fiscal, balance cuenta corriente, TRM. Finalmente de la página del banco de la república obtenemos los dato del pib desestacionalizado.

### Año 2015

Para tener un buen número de observaciones, es necesario tener más de 3 años. Debido a esto, obtenemos información desde el año 2015 para todas las industrias.

``` r
d_2015 = read.delim("EstadoResultadosIntegrales(ERI)2015.txt", sep = "¬", quote = "")
d_2015 = d_2015 %>%
  select(NIT,PUNTO_ENTRADA, CIIU_VERSION,Costo.de.ventas,Gastos.de.ventas, Ingresos.de.actividades.ordinarias)
```

Dentro del procedimiento es necesario renombar las variables, para que luego sea más fácil unir los datos:

``` r
library(dplyr)
d_2015 = d_2015 %>% rename(nit = NIT, "punto_entrada" = PUNTO_ENTRADA, ciiu = CIIU_VERSION, costo = Costo.de.ventas, gasto = Gastos.de.ventas,ingresos_ordinarios = Ingresos.de.actividades.ordinarias)
```

Tenemos que verificar datos duplicados en la base

``` r
d_2015 = d_2015[!duplicated(d_2015),]
```

Por último, creamos la varible perido para el año de los datos

``` r
d_2015$periodo = 2015
```

### Año 2016

Realizamos el mismo procedimiento para este año

``` r
library(dplyr)
d_2016 = read.delim2("EstadoResultadosIntegrales(ERI)2016.txt", sep = "¬", quote = "")
d_2016 = d_2016 %>%
  select(NIT,PUNTO_ENTRADA, CIIU_VERSION,Costo.de.ventas,Gastos.de.ventas, Ingresos.de.actividades.ordinarias)
```

Realizamos el mismo procedimeinto de renombrar las variables

``` r
d_2016 = d_2016 %>%
  rename(nit = NIT, "punto_entrada" = PUNTO_ENTRADA, ciiu = CIIU_VERSION, costo = Costo.de.ventas, gasto = Gastos.de.ventas, ingresos_ordinarios = Ingresos.de.actividades.ordinarias)
```

Por último, creamos la varible perido para con el a?o de los datos

``` r
d_2016$periodo = 2016
```

### Año 2018 y 2017

En esta parte del trabajo, procesaremos los datos de los años 2017 y 2018. Solo procesaremos los archivos del año 2018 debido a que en el se encuentran tambin los del periodo anterior. De los archivos del 2018, también se obtendrá un diccionario de códigos CIIU, que nos ayudará a clasificar las empresas de los años 2015 y 2016 que tienen el código, sin embargo, no poseen una descripción de este.

A diferencia de los años 2015 y 2016, los Estados de Resultados para el año 2018 se encuentran separados para la variable "Punto Entrada" que clasifica las empresas por Plenas Individuales, Plenas Separados, Pymes Individuales y Pymes Separados. El tratamiento de los archivos se da en este mismo orden.

#### Plenas Individuales

El procedimiento es el siguiente, en el archivo de Excel encontramos varias hojas, las de nuestro interés es "ERI" que trata sobre el Estado de Resultado de las empresas, y en la cual obtenemos la variable Costos de venta; luego, vamos a la hoja "Caratula" que es donde encontramos el CIIU de cada unas de las empresas.

``` r
#Cargamos la librearia readxl para leer los datos de Excel
library(readxl)
library(dplyr)
#Obtengo los datos del Estado de Resultados
plenas_i_2018 = read_excel("NIIF Plenas Individuales.xlsx", sheet = "ERI")
#Selecciono las variables deseadas
plenas_i_2018 = plenas_i_2018 %>%
  select(Nit,`Punto Entrada`,Periodo,`Costo de ventas`,`Gastos de ventas`, `Ingresos de actividades ordinarias`)
plenas_i_2018 = plenas_i_2018 %>%
  rename(nit = Nit, punto_entrada = `Punto Entrada`, periodo = Periodo, costo = `Costo de ventas`,gasto = `Gastos de ventas`, ingresos_ordinarios = `Ingresos de actividades ordinarias`)
#Obtengo el CIUU para estas empresas
plenas_i_2018_ciuu = read_excel("NIIF Plenas Individuales.xlsx", sheet = "Caratula")
plenas_i_2018_ciuu = plenas_i_2018_ciuu %>%
  rename(nit = Nit, ciiu = `Clasificación Industrial Internacional Uniforme Versión 4 A.C`) %>%
  select(nit,ciiu)
```

#### Plenas Separados

El procedimiento es el mismo que el anteriormente explicado pero para las Plenas Separados.

``` r
#Obtengo los datos del Estado de Resultados
plenas_s_2018 = read_excel("NIIF Plenas Separados.xlsx", sheet = "ERI")
```

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Coercing text to numeric in A1928 / R1928C1: '830058272'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Coercing text to numeric in A1929 / R1929C1: '830058272'

``` r
#Selecciono las variables deseadas
plenas_s_2018 = plenas_s_2018 %>%
  select(Nit,`Punto Entrada`,Periodo,`Costo de ventas`,`Gastos de ventas`,`Ingresos de actividades ordinarias`)
plenas_s_2018 = plenas_s_2018 %>%
  rename(nit = Nit, punto_entrada = `Punto Entrada`, periodo = Periodo, costo = `Costo de ventas`,gasto = `Gastos de ventas`,ingresos_ordinarios = `Ingresos de actividades ordinarias`)
#Obtengo el CII para estas empresas
plenas_s_2018_ciuu = read_excel("NIIF Plenas Separados.xlsx", sheet = "Caratula")
plenas_s_2018_ciuu = plenas_s_2018_ciuu %>%
  rename(nit = Nit, ciiu = `Clasificación Industrial Internacional Uniforme Versión 4 A.C`) %>%
  select(nit,ciiu)
```

#### Pymes Individuales

``` r
#Obtengo los datos del Estado de Resultados
pymes_i_2018 = read_excel("NIIF Pymes Individuales.xlsx", sheet = "ERI")
#Selecciono las variables deseadas
pymes_i_2018 = pymes_i_2018 %>%
  select(Nit,`Punto Entrada`,Periodo,`Costo de ventas`,`Gastos de ventas`,`Ingresos de actividades ordinarias`)  %>%
  rename(nit = Nit, punto_entrada = `Punto Entrada`, periodo = Periodo, costo = `Costo de ventas`,gasto = `Gastos de ventas`,ingresos_ordinarios = `Ingresos de actividades ordinarias`)
#Obtengo el CIUU para estas empresas
pymes_i_2018_ciuu = read_excel("NIIF Pymes Individuales.xlsx", sheet = "Caratula")
pymes_i_2018_ciuu = pymes_i_2018_ciuu %>%
  rename(nit = Nit, ciiu = `Clasificación Industrial Internacional Uniforme Versión 4 A.C`) %>%
  select(nit,ciiu)
```

#### Pymes Separados

``` r
#Obtengo los datos del Estado de Resultados
pymes_s_2018 = read_excel("NIIF Pymes Separados.xlsx", sheet = "ERI")
#Selecciono las variables deseadas
pymes_s_2018 = pymes_s_2018 %>%
  select(Nit,`Punto Entrada`,Periodo,`Costo de ventas`,`Gastos de ventas`,`Ingresos de actividades ordinarias`) %>%
  rename(nit = Nit, punto_entrada = `Punto Entrada`, periodo = Periodo, costo = `Costo de ventas`,gasto = `Gastos de ventas`,ingresos_ordinarios = `Ingresos de actividades ordinarias`)
#Obtengo el CIUU para estas empresas
pymes_s_2018_ciuu = read_excel("NIIF Pymes Separados.xlsx", sheet = "Caratula")
pymes_s_2018_ciuu = pymes_s_2018_ciuu %>%
  rename(nit = Nit, ciiu = `Clasificación Industrial Internacional Uniforme Versión 4 A.C`) %>%
  select(nit,ciiu)
```

### Base 2018 y CIIU 2018

Para cada uno de los punto de entrada tenemos los datos de los costos, gastos, ingresos y el nit de las empresas, y también tenemos los códigos CIIU a los que pertenecen. Procedemos entonces a unir toda la información de los datos del costo, y luego uniremos los códigos CIIU a cada una de las empresas:

``` r
d_2018 = rbind(plenas_i_2018,plenas_s_2018,pymes_i_2018,pymes_s_2018)
ciiu_2018  = rbind(plenas_i_2018_ciuu,plenas_s_2018_ciuu,pymes_i_2018_ciuu,pymes_s_2018_ciuu)
```

Procedemos entonces a pegar el código CIIU a cada empresa:

``` r
d_2018 = merge(d_2018, ciiu_2018 ,by="nit")
```

Por último, creamos los registros de 2017 y 2018 que se encuentran registrados como "Periodo Anterior" y "Periodo Actual", respectivamente.

``` r
d_2018$periodo[d_2018$periodo == 'Periodo Anterior'] = 2017
d_2018$periodo[d_2018$periodo == 'Periodo Actual'] = 2018
d_2018$periodo = as.numeric(d_2018$periodo)
```

### Diccionario CIIU

Ahora obtendremos de nuestra base de datos del 2018 un diccionario de CIIU, dicho diccionario, nos ayudará a asignar los nombres a las empresas del 2015 y 2016, que si bien poseen el código, no poseen la especificidad de la industria.

``` r
library(tidyr)
#Creamos la nueva base del dicionario
dic_ciiu = ciiu_2018[,"ciiu"]
#Dejamos los registros unicos
dic_ciiu =unique(dic_ciiu)
#Creao una replica de la variable
dic_ciiu["ciiu_total"] = dic_ciiu$ciiu
#Separo la replica para obtener el id y el nombre de cada ciiu
dic_ciiu = dic_ciiu %>%
  separate(ciiu,c("ciiu","name_ciiu")," - ")
```

Ahora unimos los códigos CIIU con los años 2015 y 2016

``` r
d_2016 = left_join(d_2016,dic_ciiu[,c("ciiu","ciiu_total")],by="ciiu")
d_2015 = left_join(d_2015,dic_ciiu[,c("ciiu","ciiu_total")],by="ciiu")
```

Por último seleccionamos y renombramos variables para dejar todos los datos en el mismo formato:

Selección de variables:

``` r
d_2016 = d_2016[,c("nit","punto_entrada","ciiu_total","costo","gasto","periodo","ingresos_ordinarios")]
d_2015 = d_2015[,c("nit","punto_entrada","ciiu_total","costo","gasto","periodo","ingresos_ordinarios")]
```

Renombrar variable

``` r
d_2016 = d_2016 %>% rename(ciiu = ciiu_total)
d_2015 = d_2015 %>% rename(ciiu = ciiu_total)
```

### Creación de la base total

Ya que tenemos todos los datos, procedemos a crear toda la base de datos de la siguiente manera:

``` r
supersociedades = rbind(d_2018,d_2016,d_2015)
```

### Tabla resumen

Para tomar la decisión de que empresas tomar, es necesario hacer un resumen de: Cuantas empresas hay por año y que tengan costos, ya que hay varias empresas que no reportan ni costos ni gastos:

``` r
#Asi puedo cambiar mis datos de missing a 0
supersociedades[is.na(supersociedades)] = 0
```

``` r
gastos_cero = supersociedades %>%
  filter(costo==0) %>% count(ciiu,periodo,punto_entrada)
```

``` r
costos_cero = supersociedades %>%
  filter(costo==0) %>% count(ciiu,periodo,punto_entrada)

resumen = supersociedades %>%
  count(ciiu,periodo,punto_entrada)
resumen = left_join(resumen,costos_cero, by = c("ciiu","periodo","punto_entrada"))
resumen = resumen %>% rename(cero_costos = n.y, numero_empresas = n.x )
resumen = left_join(resumen,gastos_cero, by = c("ciiu","periodo","punto_entrada"))
resumen = resumen %>% rename(cero_gastos = n)

resumen[is.na(resumen)] = 0
```

Luego de hacer el análisis, tomamos la decisión de utilizar el A0125 - Cultivo de flor de corte

``` r
bd_trabajo = supersociedades %>%
  filter(ciiu == "A0125 - Cultivo de flor de corte")
```

Contamos el numero de empesas por periodo

``` r
nro_empresas = bd_trabajo %>% count(nit)
nro_empresas = nro_empresas %>% rename(aparicionaes = n)
```

Unimos esa variable para cada uno de los nit

``` r
bd_trabajo = merge(bd_trabajo, nro_empresas, by = "nit")
```

Al final utilizamos las empresas que tienen las 4 apariciones

``` r
bd_trabajo = bd_trabajo %>% filter(aparicionaes==4)
```

Guardamos nuestro espacio de trabajo

``` r
save.image(file = "espacio_trabajo.RData")
```

``` r
load("espacio_trabajo.RData")
```

### Variables Macro

El objetivo de este trabajo es realizar inferecia sobre los costos de un sector empresarial, dependiendo del comportamiento de la economía. Para esto, es necesario utilizar variables marcoeconómicas como el PIB, inflación, TRM, desempleo, entre otras. En esta parte cargaremos las variables macroeconómicas.

Creamos una nueva base de datos llamada `bd_sector` para trabajar sobre esta.

``` r
bd_sector = bd_trabajo
```

Hacemos una cuenta del nÃºmero de empresas por año

``` r
bd_sector %>% count(periodo)
```

    ## # A tibble: 4 x 2
    ##   periodo     n
    ##     <dbl> <int>
    ## 1    2015    63
    ## 2    2016    63
    ## 3    2017    63
    ## 4    2018    63

Tenemos un total de 63 empresas para cada año. Procedemos a cargar las variables macro que obtuvimos por medio de Bloomberg y el Banrep.

``` r
library(readxl)
variables_macro = read_excel("variables_macro.xlsx", sheet = "Hoja2")
```

Luego, unimos las bases de datos:

``` r
bd_reg = merge(bd_sector,variables_macro,by="periodo")
bd_reg$costo = as.numeric(bd_reg$costo)
bd_reg$ingresos_ordinarios = as.numeric(bd_reg$ingresos_ordinarios)
```

### Creación de variables

Primero ordenamos nuestros datos por nit, luego por año:

``` r
bd_reg = bd_reg[order(bd_reg$nit,bd_reg$periodo),]
```

En esta sección creamos las diferentes variables para poder realizar el análisis, primero creamos las variables transformadas con los logaritmos naturales. Al igual que generamos diferentes variables como variable respuesta. Dentro de estas se encuentra la porporción de costos sobre los ingresos, como también la variación de los costos de un año a otro.

Primero filtramos las empresas que no poseen costos, debido a que al aplicar logaritmos o la proporción de costos, tendríamos problemas en nuestra variable respuesta.

``` r
bd_reg = bd_reg %>% filter(costo >0)
```

Creamos los logaritmos de las diferentes variables y la proporción de costos sobre ingresos

``` r
bd_reg$prop_costos = bd_reg$costo/bd_reg$ingresos_ordinarios
bd_reg$ln_costos = log(bd_reg$costo)
bd_reg$log_pib = log(bd_reg$pib)
bd_reg$log_trm = log(bd_reg$trm)
bd_reg$log_ti = log(bd_reg$t_i)
bd_reg$log_ing = log(bd_reg$ingresos_ordinarios)
bd_reg$log_desempl = log(bd_reg$desempl)
```

Creamos la variación de algunas variables

``` r
bd_reg = bd_reg %>%
  group_by(nit) %>%
  mutate(var_costos = ln_costos - lag(ln_costos),var_trm = log_trm - lag(log_trm), var_ti = log_ti - lag(log_ti), lag_costos = lag(ln_costos)) %>% ungroup()
```

Creamos una variable dummy para cada uno de los punto de entrada

``` r
#Primero nos encargamos de las pymes individuales
bd_reg$pymes_ind = NULL
bd_reg$pymes_ind[bd_reg$punto_entrada == "40 NIIF Pymes - Individuales"] = 1
```

    ## Warning: Unknown or uninitialised column: 'pymes_ind'.

``` r
bd_reg$pymes_ind[bd_reg$punto_entrada != "40 NIIF Pymes - Individuales"] = 0
#Plenas Individuales
bd_reg$plenas_ind = NULL
bd_reg$plenas_ind[bd_reg$punto_entrada == "10 NIIF Plenas - Individuales"] = 1
```

    ## Warning: Unknown or uninitialised column: 'plenas_ind'.

``` r
bd_reg$plenas_ind[bd_reg$punto_entrada != "10 NIIF Plenas - Individuales"] = 0
#Pymes Separados
bd_reg$pymes_sep = NULL
bd_reg$pymes_sep[bd_reg$punto_entrada == "50 NIIF Pymes - Separados"] = 1
```

    ## Warning: Unknown or uninitialised column: 'pymes_sep'.

``` r
bd_reg$pymes_sep[bd_reg$punto_entrada != "50 NIIF Pymes - Separados"] = 0
#Plenas Separados
bd_reg$plenas_sep = NULL
bd_reg$plenas_sep[bd_reg$punto_entrada == "20 NIIF Plenas - Separados"] = 1
```

    ## Warning: Unknown or uninitialised column: 'plenas_sep'.

``` r
bd_reg$plenas_sep[bd_reg$punto_entrada != "20 NIIF Plenas - Separados"] = 0
```

Análisis exploratorio
---------------------

|  Periodo|  Número de Empresas|
|--------:|-------------------:|
|     2015|                  62|
|     2016|                  62|
|     2017|                  62|
|     2018|                  62|

Tenmos un total de 62 empresas para los 4 años de estudio.

``` r
library(knitr)
kable(bd_reg%>% count(periodo, punto_entrada), col.names = c("Periodo","Punto Entrada", "Número de Empresas"), label = "Tabla 2")
```

|  Periodo| Punto Entrada                 |  Número de Empresas|
|--------:|:------------------------------|-------------------:|
|     2015| 10 NIIF Plenas - Individuales |                  62|
|     2016| 10 NIIF Plenas - Individuales |                  57|
|     2016| 20 NIIF Plenas - Separados    |                   5|
|     2017| 10 NIIF Plenas - Individuales |                  57|
|     2017| 20 NIIF Plenas - Separados    |                   5|
|     2018| 10 NIIF Plenas - Individuales |                  57|
|     2018| 20 NIIF Plenas - Separados    |                   5|

Tenemos solo dos puntos entrada, plenas separadas y plenas individuales, siendo esta primera la de mayor número de empresas asociadas.

Luego de conocer nuestra estructura de empresas, procedemos a realizar nuestra descripción de las variables objetivo. Primero observemos que sucede con la variable costos in realizar ninguna transformación.

``` r
ggplot(bd_reg, aes(x = costo)) + geom_histogram(bins = 30,fill = "blue") +xlab("Costos")+ggtitle("Histograma Costos")+theme(plot.title = element_text(hjust = 0.5)) + ylab("Cuenta")
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-42-1.png)

Cuando gráficamos los costos sin hacer ninguna transformación, observamos que la mayoría de nuestras observaciones están concentrados a la izquierda del histograma, hay muy pocas observaciones que están a un nivel muy alto de costos

``` r
ggplot(bd_reg, aes(x = punto_entrada, y = costo)) + geom_boxplot() + ggtitle(label = " BoxPlot Costos por Punto Entrada") + xlab("Punto Entrada") + ylab("Costos")
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-43-1.png) Debido a la proporción de Punto entrada que hay en dicha industria, Vemos que el BoxPlot de Plenas Individuales es muy similar al histograma observado anteriormente. Esto quiere decir que hay varias empresas que tienen diferencias altas en su nivel de costos

Luego, es interesante observar como cambia nuestra variable respuesta de acuerdo a las diferentes transformaciones que se realizaron con anterioridad.

``` r
ggplot(bd_reg, aes(x = ln_costos)) + geom_histogram(bins = 30,fill = "blue") +ggtitle(label = "Histograma Logaritmo Natural Costos") + theme(plot.title = element_text(hjust = 0.5)) +xlab("Logaritmo Natural Costos") + ylab("Cuenta")
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-44-1.png) Al aplicar logaritmo natural a los costos, observamos una distribución diferente, la mayoría de datos se encuentran a la izquierda de la distribución, sin embargo, seguimos obteniendo valores que se considerarían outliers.

``` r
ggplot(bd_reg, aes(x = punto_entrada, y = ln_costos)) + geom_boxplot() + ggtitle(label = " BoxPlot Logaritmo Natural Costos por Punto Entrada") + xlab("Punto Entrada") + ylab("Logaritmo Natural Costos")
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-45-1.png) Al aplicar logaritmo, cambiamos la escala en los datos y por lo tanto el Box Plot modifica su visualización, como podemos observar anteriormente.

Veamos ahora estos gráficos relacionando la proporción de los costos.

``` r
ggplot(bd_reg, aes(x = prop_costos)) + geom_histogram(bins = 30,fill = "blue") +ggtitle(label = "Histograma Proporcion Costos") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Proporcion Costos") + ylab("Cuenta")
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-46-1.png)

Observamos un gran cambio en el histograma, esto debido a que hay diferentes tiepos de empresas, pero al realizar la proporción de los costos, generamos que esos valores atípicos observados anteriormente, se normalicen.

``` r
ggplot(bd_reg, aes(x = punto_entrada, y = prop_costos)) + geom_boxplot() + ggtitle(label = "BoxPlot Proporción costos por Punto Entrada") + xlab("Punto Entrada") + ylab("Prop Costos")
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-47-1.png) El BoxPlot de la proporción de los costos ayuda a soportar lo anterior, Observemos que no aparecen máss puntos outliers en el gráfico.

Por úlitmo, veremos los gráficos anteriores para la variación en los costos.

``` r
ggplot(bd_reg, aes(x = var_costos)) + geom_histogram(bins = 30,fill = "blue") +ggtitle(label = "Histograma Variacion Costos") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Variacion Costos") + ylab("Cuenta")
```

    ## Warning: Removed 62 rows containing non-finite values (stat_bin).

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-48-1.png)

``` r
ggplot(bd_reg, aes(x = punto_entrada, y = var_costos)) + geom_boxplot() + ggtitle(label = "BoxPlot Variacion Costos") + xlab("Punto Entrada") + ylab("Costos")
```

    ## Warning: Removed 62 rows containing non-finite values (stat_boxplot).

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-49-1.png) Luego de observar las gráficas, podemos determiar que las transformaciones tienen un efecto de normalizar la variable para las diferentes empresas.

Otro punto importante es observar las correlaciones de las variables:

``` r
corrgram(bd_reg[,c("ln_costos","prop_costos","crec_pib","log_pib","infl","log_desempl","log_ti")], panel = panel.cor)
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-50-1.png)

Por lo que nos muestra la correlación de las diferentes variables que tenemos, no se evidencia una fuerte relación entre las diferentes transformaicones de los costos y las variables macroeconómicas.

Por último es importante observar como se comportan los costos dentro de cada empresa, esto debido a que por la estrucutra de datos que tenemos los modelos de regresión lineal múltiple, no tienen en cuenta la heterogeneidad de cada uno de los individuos, lo que si se logra recoger con los modelos de efectos mixtos.

``` r
#Agregamos una columna factor de sujeto para poder visualizar el boxplot de las empresas
bd_reg$subject = factor(rep(1:62,each=4))
ggplot(bd_reg, aes(x = subject, y = prop_costos)) + geom_boxplot() + ggtitle(label = "BoxPlot Pop Costos por empresa") + xlab("Punto Entrada") + ylab("Prop Costos")
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-51-1.png)

``` r
ggplot(bd_reg, aes(x = subject, y = prop_costos)) + geom_boxplot() + ggtitle(label = "BoxPlot Log Costos por empresa") + xlab("Punto Entrada") + ylab("Log Costos")
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-52-1.png)

Los modelos
-----------

### Datos de entrenamiento y de validación

En esta sección separaremos nuestros datos en entrenamiento y validación, nuestros datos de entrenamiento serán el 75% de nuestra muestra de empresas, el restante será para validació.

``` r
#Obtenemos una muestra del 75% de nuestros nits
nit_empresas = unique(bd_reg$nit)
#Primero establecemos la semilla para asegurar sets de entrenamiento y validacion reproducibles
set.seed(200)
#Especificamos la proporcion de datos de entrenamiento.
training_size = 0.75
#utilizamos la funcion sample
training_rows = sample(seq_len(length(nit_empresas)), size = floor(training_size * length(nit_empresas)))
#nits entrenamiento
nit_entrenamiento <- nit_empresas[training_rows]
#nits de validacion
nit_validation <- nit_empresas[-training_rows]
#filtramos los datos por los nit seleccionados
data_training = subset(bd_reg, nit %in% nit_entrenamiento)
data_validation = subset(bd_reg, nit %in% nit_validation)
```

### Modelo de Regresión Lineal Múltiple

Realizaremos diferentes modelos de regresión lineal múltiple para cada una de las variables respuesta determinadas.

#### Logaritmo costos

Primero realizamos el modelo de regresin lineal con nuestra variable respuesta siendo el logaritmo de los costos

``` r
modelo1 = lm(ln_costos ~ log_pib + log_trm + infl + log_desempl, data = data_training, na.action = na.exclude)
summary(modelo1)
```

    ## 
    ## Call:
    ## lm(formula = ln_costos ~ log_pib + log_trm + infl + log_desempl, 
    ##     data = data_training, na.action = na.exclude)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.26522 -0.54268 -0.06892  0.38055  2.91557 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  58.0314   180.3428   0.322    0.748
    ## log_pib      -3.4497    13.5551  -0.254    0.799
    ## log_trm       0.7840     1.9113   0.410    0.682
    ## infl         -0.1043     0.1987  -0.525    0.600
    ## log_desempl       NA         NA      NA       NA
    ## 
    ## Residual standard error: 0.7899 on 180 degrees of freedom
    ## Multiple R-squared:  0.01359,    Adjusted R-squared:  -0.002847 
    ## F-statistic: 0.8268 on 3 and 180 DF,  p-value: 0.4807

Luego de esta primera regresin, podemos observar que al tener un espectro de tiempo tan corto, y debido a que las variables explicativas se repiten tan constantemente, el modelo solo estima 3 variables regresoras. En este caso el logaritmo del pib, el logaritmo de la trm, y la inflación. Aunque no son significativas, se podría dar un indicio que un aumento del 1% del PIB indicaríaa una disminución del 3% en los costos en general de una compañia que cultiva flore, de igual forma, un aumento del 1% en la TRM tiene un aumento del 0.9% en este tipo de empresas. Vemos que el modelo no se ajusta muy bien en los datos de entrenamiento, esto sucede debido a que como las empresas comparten los mismo valores macroeconómicos, se obtienen los mismos resultados para cada observación. Esto genera que no se logre contemplar los efectos de cada uno de los individuos. En pasos siguientes se realizarán más modelos de regresión lineal múltiple, más no los analizaremos, debido a su similitud en la respuesta.

``` r
par(mfrow=c(2,2))
plot(modelo1)
```

    ## hat values (leverages) are all = 0.02173913
    ##  and there are no factor predictors; no plot no. 5

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-55-1.png)

``` r
modelo2 = lm(ln_costos ~ infl + log_ti + log_desempl, data = data_training, na.action = na.exclude)
summary(modelo2)
```

    ## 
    ## Call:
    ## lm(formula = ln_costos ~ infl + log_ti + log_desempl, data = data_training, 
    ##     na.action = na.exclude)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.26522 -0.54268 -0.06892  0.38055  2.91557 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) 17.15811    5.40483   3.175  0.00177 **
    ## infl        -0.06947    0.06609  -1.051  0.29454   
    ## log_ti       0.11373    0.38298   0.297  0.76685   
    ## log_desempl -0.11077    2.16099  -0.051  0.95917   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7899 on 180 degrees of freedom
    ## Multiple R-squared:  0.01359,    Adjusted R-squared:  -0.002847 
    ## F-statistic: 0.8268 on 3 and 180 DF,  p-value: 0.4807

``` r
plot(modelo2)
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-57-1.png)![](supersociedades2_files/figure-markdown_github/unnamed-chunk-57-2.png)

    ## hat values (leverages) are all = 0.02173913
    ##  and there are no factor predictors; no plot no. 5

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-57-3.png)![](supersociedades2_files/figure-markdown_github/unnamed-chunk-57-4.png)

``` r
modelo3 = lm(ln_costos ~ log_desempl + log_trm + log_pib , data = data_training, na.action = na.exclude)
summary(modelo3)
```

    ## 
    ## Call:
    ## lm(formula = ln_costos ~ log_desempl + log_trm + log_pib, data = data_training, 
    ##     na.action = na.exclude)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.26522 -0.54268 -0.06892  0.38055  2.91557 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -67.97356   74.37058  -0.914    0.362
    ## log_desempl  -1.24055    2.36266  -0.525    0.600
    ## log_trm       0.05846    2.03939   0.029    0.977
    ## log_pib       6.38344    6.45199   0.989    0.324
    ## 
    ## Residual standard error: 0.7899 on 180 degrees of freedom
    ## Multiple R-squared:  0.01359,    Adjusted R-squared:  -0.002847 
    ## F-statistic: 0.8268 on 3 and 180 DF,  p-value: 0.4807

``` r
plot(modelo3)
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-59-1.png)![](supersociedades2_files/figure-markdown_github/unnamed-chunk-59-2.png)

    ## hat values (leverages) are all = 0.02173913
    ##  and there are no factor predictors; no plot no. 5

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-59-3.png)![](supersociedades2_files/figure-markdown_github/unnamed-chunk-59-4.png)

``` r
modelo4 = lm(ln_costos ~ log_pib + log_trm + plenas_sep + log_desempl, data = data_training, na.action = na.exclude)
summary(modelo4)
```

    ## 
    ## Call:
    ## lm(formula = ln_costos ~ log_pib + log_trm + plenas_sep + log_desempl, 
    ##     data = data_training, na.action = na.exclude)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.2345 -0.5332 -0.1065  0.3402  2.9463 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -60.1474    74.4627  -0.808    0.420
    ## log_pib       5.9206     6.4489   0.918    0.360
    ## log_trm      -0.1624     2.0423  -0.080    0.937
    ## plenas_sep    0.2826     0.2156   1.311    0.192
    ## log_desempl  -1.1394     2.3592  -0.483    0.630
    ## 
    ## Residual standard error: 0.7884 on 179 degrees of freedom
    ## Multiple R-squared:  0.02297,    Adjusted R-squared:  0.001135 
    ## F-statistic: 1.052 on 4 and 179 DF,  p-value: 0.3819

``` r
par(mfrow = c(2,2))
plot(modelo4)
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-61-1.png)

#### Proporción de los costos

Ahora observemos el modelo para la proporción de los costos:

``` r
modelo1 = lm(prop_costos ~log_pib + log_trm + plenas_sep, data = data_training)
summary(modelo1)
```

    ## 
    ## Call:
    ## lm(formula = prop_costos ~ log_pib + log_trm + plenas_sep, data = data_training)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.304814 -0.069355  0.005428  0.091229  0.284553 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -10.02670    6.05350  -1.656   0.0994 .
    ## log_pib       0.80341    0.51289   1.566   0.1190  
    ## log_trm      -0.01182    0.29140  -0.041   0.9677  
    ## plenas_sep   -0.04373    0.03395  -1.288   0.1994  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1242 on 180 degrees of freedom
    ## Multiple R-squared:  0.02447,    Adjusted R-squared:  0.008209 
    ## F-statistic: 1.505 on 3 and 180 DF,  p-value: 0.2149

``` r
par(mfrow = c(2,2))
plot(modelo1)
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-63-1.png)

``` r
modelo2 = lm(prop_costos ~log_pib + log_trm + infl, data = data_training)
summary(modelo2)
```

    ## 
    ## Call:
    ## lm(formula = prop_costos ~ log_pib + log_trm + infl, data = data_training)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.30093 -0.06870  0.01045  0.09647  0.26888 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) 25.76178   28.36261   0.908    0.365
    ## log_pib     -1.84454    2.13182  -0.865    0.388
    ## log_trm      0.04906    0.30059   0.163    0.871
    ## infl        -0.03947    0.03126  -1.263    0.208
    ## 
    ## Residual standard error: 0.1242 on 180 degrees of freedom
    ## Multiple R-squared:  0.02412,    Adjusted R-squared:  0.00786 
    ## F-statistic: 1.483 on 3 and 180 DF,  p-value: 0.2207

#### Variación costos

Por último, realizamos la regresión con la variación de los costos

``` r
modelo1 = lm(var_costos~crec_pib + log_trm, data = data_training)
summary(modelo1)
```

    ## 
    ## Call:
    ## lm(formula = var_costos ~ crec_pib + log_trm, data = data_training)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.60936 -0.07656  0.00528  0.06806  0.66016 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -17.70576    8.63779  -2.050   0.0423 *
    ## crec_pib     -0.08564    0.03695  -2.318   0.0220 *
    ## log_trm       2.24358    1.08145   2.075   0.0399 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1925 on 135 degrees of freedom
    ##   (46 observations deleted due to missingness)
    ## Multiple R-squared:  0.05565,    Adjusted R-squared:  0.04166 
    ## F-statistic: 3.978 on 2 and 135 DF,  p-value: 0.02097

Hasta ahora, este se el mejor modelo que se ha realizado, esto en cuanto significancia de las variables y el R cuadrado que tenemos. Sin embargo, al revisar el ajuste del modelo, comprobamos que tiene los mismos problemas, el ajuste en los datos de entrenamiento sigue siendo igual, ya que se mantienen las mismas observaciones para cada uno de los individuos y no hay nada que los diferencie, como se ve en los gráficos de ajuste.

``` r
par(mfrow = c(2,2))
plot(modelo1)
```

    ## hat values (leverages) are all = 0.02173913
    ##  and there are no factor predictors; no plot no. 5

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-66-1.png)

``` r
predichos = predict(modelo1,na.omit(data_validation))
```

``` r
residuales = na.omit(data_validation$var_costos)-predichos 
```

``` r
plot(predichos, residuales)
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-69-1.png)

Estos mismos problemas se traducen para los datos de validación, observese que para los datos predichos de validación solo hay 3 valores predichos, ya que los residuales se encuentran rondando estos mismos datos.

### Modelos Efectos Mixtos

Como parte final del trabajo, tenemos los modelos de efectos mixtos, esta sección sigue el mismo orden anteriormente presentado.

#### Logaritmo de los costos

``` r
mixtos1 = lmer(ln_costos~log_pib+log_trm + infl + (1|nit), data = data_training )
summary(mixtos1)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: ln_costos ~ log_pib + log_trm + infl + (1 | nit)
    ##    Data: data_training
    ## 
    ## REML criterion at convergence: 68.7
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.6642 -0.3508 -0.0289  0.2956  7.3719 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  nit      (Intercept) 0.5966   0.7724  
    ##  Residual             0.0274   0.1655  
    ## Number of obs: 184, groups:  nit, 46
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept) 58.03138   37.79143   1.536
    ## log_pib     -3.44969    2.84051  -1.214
    ## log_trm      0.78403    0.40052   1.958
    ## infl        -0.10435    0.04165  -2.506
    ## 
    ## Correlation of Fixed Effects:
    ##         (Intr) log_pb lg_trm
    ## log_pib -0.997              
    ## log_trm  0.312 -0.386       
    ## infl    -0.977  0.971 -0.266

Este modelo es sobre el logaritmo de los costos, en función del logaritmo del pib, el logaritmo de la trm y la inflación, con efectos fijos en cada uno de los individuos de los datos de entrenamiento. La interpretación puede ser la misma que se dio anteriormente para los efectos fijos, pero lo que podemos entender para los efectos aleatorios es que el intercepto por individuo no indica que hay un gran porcentaje de variabilidad explicado por este.

``` r
plot(mixtos1)
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-71-1.png)

``` r
qqnorm(resid(mixtos1))
qqline(resid(mixtos1))
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-72-1.png)

Ahora, podemos observar de acuerdo a los gráficos anteriores que el modelo funcionó mejor para los datos, ya que no se predice los mismos 3 valores que se realizaban antes. Sin embargo, seguimos con problemas en el Normal QQ Plot.

``` r
mixtos2 = lmer(ln_costos~crec_pib +log_trm+(1+crec_pib|nit), data = data_training )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00300272 (tol = 0.002, component 1)

``` r
summary(mixtos2)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: ln_costos ~ crec_pib + log_trm + (1 + crec_pib | nit)
    ##    Data: data_training
    ## 
    ## REML criterion at convergence: 92.6
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4974 -0.4163 -0.0432  0.3531  6.5059 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  nit      (Intercept) 0.715563 0.84591       
    ##           crec_pib    0.002751 0.05245  -0.67
    ##  Residual             0.031173 0.17656       
    ## Number of obs: 184, groups:  nit, 46
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  9.44124    3.61197   2.614
    ## crec_pib    -0.06672    0.02994  -2.228
    ## log_trm      0.93641    0.44701   2.095
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr) crc_pb
    ## crec_pib -0.634       
    ## log_trm  -0.999  0.618
    ## convergence code: 0
    ## Model failed to converge with max|grad| = 0.00300272 (tol = 0.002, component 1)

El análisis con los datos anteriores es similar, solo que eta vez agregamos una pendiente de efectos aleatorios al modelo, con respecto al crecimiento del pib. Aunque lo interesante,es que la variabilidad se la sigue llevando la pendiente del individuo.

``` r
plot(mixtos2)
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-74-1.png)

``` r
qqnorm(resid(mixtos2))
qqline(resid(mixtos2))
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-75-1.png)

Ahora vemos un análisis de la predicción, observemos que los valores se encuentran ajustados para este modelo, así como lo mostraba anteriormente los residuales.

``` r
predichos_train = predict(mixtos2,data_training)
plot(predichos_train,data_training$ln_costos)
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-76-1.png)

En esta parte realizaremos la predicción de los datos de validación. Utilizando el comando simulate:

``` r
predichos_validation = simulate(mixtos2, nsim = 1,seed = 200,re.form = ~(1+crec_pib|nit), newdata = data_validation, allow.new.levels = TRUE)
```

``` r
residuales = data_validation$ln_costos-predichos_validation$sim_1
```

``` r
plot(predichos_validation$sim_1, residuales)
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-79-1.png)

``` r
plot(predichos_validation$sim_1, data_validation$ln_costos)
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-80-1.png)

Vemos que el comportamiento es toalmente aleatorio y no hay una buena predicción para los datos de validación

#### Proporción de los costos

``` r
mixtos1 = lmer(prop_costos ~log_pib + log_trm + infl +(1|nit), data = data_training)
summary(mixtos1)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: prop_costos ~ log_pib + log_trm + infl + (1 | nit)
    ##    Data: data_training
    ## 
    ## REML criterion at convergence: -360.3
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.97470 -0.41943 -0.01842  0.42816  2.56749 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  nit      (Intercept) 0.011252 0.10607 
    ##  Residual             0.004183 0.06467 
    ## Number of obs: 184, groups:  nit, 46
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept) 25.76178   14.76486   1.745
    ## log_pib     -1.84454    1.10977  -1.662
    ## log_trm      0.04906    0.15648   0.314
    ## infl        -0.03947    0.01627  -2.426
    ## 
    ## Correlation of Fixed Effects:
    ##         (Intr) log_pb lg_trm
    ## log_pib -0.997              
    ## log_trm  0.312 -0.386       
    ## infl    -0.977  0.971 -0.266

``` r
plot(mixtos1)
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-82-1.png)

``` r
qqnorm(resid(mixtos1))
qqline(resid(mixtos1))
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-83-1.png)

``` r
mixtos2 = lmer(prop_costos ~log_pib + log_trm + infl +(1+log_pib|nit)+(1+log_trm|nit), data = data_training)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## unable to evaluate scaled gradient

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge: degenerate Hessian with 1 negative eigenvalues

``` r
summary(mixtos2)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: prop_costos ~ log_pib + log_trm + infl + (1 + log_pib | nit) +  
    ##     (1 + log_trm | nit)
    ##    Data: data_training
    ## 
    ## REML criterion at convergence: -360.3
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.97417 -0.41632 -0.01893  0.42836  2.56844 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev. Corr 
    ##  nit      (Intercept) 0.0035632 0.05969       
    ##           log_pib     0.0001031 0.01016  -1.00
    ##  nit.1    (Intercept) 0.0034620 0.05884       
    ##           log_trm     0.0001072 0.01036  -0.54
    ##  Residual             0.0041788 0.06464       
    ## Number of obs: 184, groups:  nit, 46
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept) 25.76178   14.75804   1.746
    ## log_pib     -1.84454    1.10926  -1.663
    ## log_trm      0.04906    0.15642   0.314
    ## infl        -0.03947    0.01626  -2.427
    ## 
    ## Correlation of Fixed Effects:
    ##         (Intr) log_pb lg_trm
    ## log_pib -0.997              
    ## log_trm  0.312 -0.386       
    ## infl    -0.977  0.971 -0.266
    ## convergence code: 0
    ## unable to evaluate scaled gradient
    ## Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

``` r
plot(mixtos2)
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-85-1.png)

``` r
qqnorm(resid(mixtos2))
qqline(resid(mixtos2))
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-86-1.png)

``` r
predichos_train = predict(mixtos2)
```

``` r
plot(predichos_train, data_training$prop_costos)
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-88-1.png)

Vemos un menor ajuste para este modelo de la proporción de los costos. Sin embargo, sigue mostrando mejores resultados que los efectos del modelo lineal múltiple mostrado con anterioridad.

``` r
predichos_validation = predict(mixtos2,newdata = data_validation, allow.new.levels = TRUE)
```

``` r
residuales = na.omit(data_validation$prop_costos)-predichos_validation 
```

``` r
ggplot(data_validation,aes(x=prop_costos,y=predichos_validation))+geom_point(size=2)
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-91-1.png)

#### Variación Costos

``` r
mixtos1 = lmer(var_costos ~ crec_pib + log_trm +(1|nit),data = data_training)
```

    ## boundary (singular) fit: see ?isSingular

``` r
summary(mixtos1)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: var_costos ~ crec_pib + log_trm + (1 | nit)
    ##    Data: data_training
    ## 
    ## REML criterion at convergence: -56.9
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -8.3581 -0.3976  0.0274  0.3534  3.4285 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  nit      (Intercept) 0.00000  0.0000  
    ##  Residual             0.03708  0.1925  
    ## Number of obs: 138, groups:  nit, 46
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error t value
    ## (Intercept) -17.70576    8.63779  -2.050
    ## crec_pib     -0.08564    0.03695  -2.318
    ## log_trm       2.24358    1.08145   2.075
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr) crc_pb
    ## crec_pib  0.210       
    ## log_trm  -1.000 -0.218
    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
plot(mixtos1)
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-93-1.png)

``` r
qqnorm(resid(mixtos1))
qqline(resid(mixtos1))
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-94-1.png)

``` r
mixtos2 = lmer(var_costos ~ crec_pib + log_trm +(1+crec_pib|nit),data = data_training)
```

    ## boundary (singular) fit: see ?isSingular

``` r
summary(mixtos2)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: var_costos ~ crec_pib + log_trm + (1 + crec_pib | nit)
    ##    Data: data_training
    ## 
    ## REML criterion at convergence: -56.9
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -8.3581 -0.3976  0.0274  0.3534  3.4285 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev.  Corr 
    ##  nit      (Intercept) 5.696e-10 2.387e-05      
    ##           crec_pib    2.534e-10 1.592e-05 -1.00
    ##  Residual             3.708e-02 1.925e-01      
    ## Number of obs: 138, groups:  nit, 46
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error t value
    ## (Intercept) -17.70576    8.63779  -2.050
    ## crec_pib     -0.08564    0.03695  -2.318
    ## log_trm       2.24358    1.08145   2.075
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr) crc_pb
    ## crec_pib  0.210       
    ## log_trm  -1.000 -0.218
    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

Los efectos fijos muestran una similitud a lo que ncontramos en el modelo de regresión lineal múltiple. Lo curioso en este caso, es que los efectos aleatorios tienen poco impacto en el modelo, es una variabilidad mínima. Esto lo refleja la gráfica de los residuales. Si vemos con atención, muestran un comportamiento similar, aunque no igual, a las estimaciones del modelo de regresión lineal múltiple, esto debido al bajo impacto de los efectos aleatorios.

``` r
plot(mixtos2)
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-96-1.png)

``` r
qqnorm(resid(mixtos2))
qqline(resid(mixtos2))
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-97-1.png)

``` r
predichos_train = predict(mixtos1, newdata = na.omit(data_training))
plot(predichos_train,na.omit(data_training$var_costos))
```

![](supersociedades2_files/figure-markdown_github/unnamed-chunk-98-1.png)

Vemos entonces que hay un poco más de variabilidad en las predicciones, sin embargo, el modelo de fectos mixtos para la variable de variación costos, sigue sin ajustarse bien.

Conclusiones
============

Este trabajo ayuda a entender como las variables macroeconómicas impactan los costos de una empresa, en tiempos donde una pandemia mundial tien en riesgo la economía, se hace necesario concer los posibles impactos que se puedan tener. Se pudo evidenciar que los modelos de efectos mixtos son una herramienta muy útil para tener en cuenta la heterogeneidad de cada uno de los individuos, en este caso las empresas, lo que nos lleva a entender que los modelos de regresión lineal múltiple quedan cortos en algunos casos, precisamente cuando hay un comportamiento en los individuos que no se puede reflejar. El espectro de tiempo del trabajo es corto, por lo tanto el análisis es limitado para solo unas variables macroeconómicas que se pueden incluir en el modelo, de acuerdo a esto, sería interesante observar como los datos trimestrales se ajustarían con este tipo de metodologías.
