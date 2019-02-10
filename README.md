# Demography_analysis_Rpopbio
## Efectos de la salinidad sobre demografía, dinámica y crecimiento a lo largo del gradiente ambiental 
### Introducción
Los árboles de _R. mangle_ son halófitas facultativas, los cuales típicamente crecen en ambientes salobres costeros de las regiones tropicales y subtropicales y son capaces de crecer tanto en agua dulce como salina. Cuentan con una serie de adaptaciones a las condiciones de salinidad, los cuales incluyen la exclusión salina durante la absorción de agua, así como una osmorregulación y comparmentalización eficiente de iones en las hojas (Ball, 1988; Scholander et al., 1964; Werner & Stellzer, 1990). Existen variaciones en el crecimiento de _R. mangle_: árboles altos, los cuales se ubican a lo largo de la zona costera y orillas de los ríos, y enanos en forma de arbusto, localizados tierra adentro (Feller, 1995; Feller et al., 1999; Lin & Sternberg, 1992b; Lovelock & Feller, 2003; Lugo & Snedaker, 1974). Las diferencias en la estatura de _R. mangle_ entre zonas costeras (alto) y tierra adentro (enano) no se deben a su edad (Cheeseman & Lovelock, 2004; Feller, 1995), ni a diferencias genéticas (Lin & Sternberg, 1992b). Por lo contrario, factores como mala aireación y anegamiento (Davis, 1940; Egler, 1952), compactación de la turba (Craighead, 1971), limitación de nutrientes (Feller, 1995; Feller et al., 2003; Lovelock et al., 2006, 2004; Lugo & Snedaker, 1974; Mckee et al., 2002), fluctuación en salinidad (Lin & Sternberg, 1992b), y la hipersalinidad (Lin & Sternberg, 1992a) han sido descritos como factores contribuyentes al enanismo de _R. mangle_. Existen una serie de estudios que describen el crecimiento de _R. mangle_ en general (Gill & Tomlinson, 1977, 1971a, 1971b, 1969). La gran mayoría de los estudios que abordan el desarrollo de _R. mangle_ bajo diferentes condiciones ambientales evalúan el crecimiento de plántulas (Ellison & Farnsworth, 1997, 1996, 1993; Farnsworth et al., 1996; Farnsworth & Ellison, 1996; Koch, 1997; Koch & Snedaker, 1997; Krauss & Allen, 2003; Lin & Sternberg, 1992a, 1992b; Mckee, 1995; Werner & Stellzer, 1990), pero estudios que han abordado el crecimiento de plantas adultas bajo diferentes condiciones salinas son extremadamente escasas (Menezes, Berger, & Worbes, 2003). El efecto de la salinidad sobre el crecimiento del mangle rojo ha sido estudiado a una escala ecofisiológica, pero no hay un estudio que evalúe cómo afecta la salinidad en la dinámica poblacional. 

Los estudios poblacionales se apoyan en la construcción de modelos matemáticos, generalizados como herramientas populares para la conservación y manejo, usados para predecir cambios en las poblaciones de animales y plantas en riesgo por actividades de explotación (Ajonina, 2008; Caswell, 2001; López-Hoffman et al., 2006; Raimondo & Donaldson, 2003). Los modelos de proyección se construyen a partir de mediciones repetitivas de las tasas vitales (sobrevivencia, crecimiento y la reproducción) en todos los individuos de una población (censo) o en una muestra representativa de plantas. A partir de estos datos se puede calcular la tasa de crecimiento poblacional lambda (λ), la cual permite evaluar si una población se encuentra creciendo, disminuyendo o estable al largo plazo, dado que las condiciones actuales no cambian (Hal Caswell, 2001). El uso de los modelos poblacionales permite predecir cambios cuantitativos en la estructura poblacional y por lo tanto permite evaluar estrategias de manejo establecido para un bosque de manglar en particular (Rajkaran & Adams, 2012).

En México todas las especies de mangle están sujetas a protección especial de acuerdo a la NOM-059-ECOL-2010, porque podrían llegar a encontrarse amenazadas por factores que inciden negativamente en su viabilidad, lo que determinaría la necesidad de propiciar su recuperación y conservación (SEMARNAT, 2010). Los mangles se pueden dañar tanto por causas naturales (sedimentación, erosión, efectos directos o indirectos de tormentas tropicales o tsunamis), así como por actividades humanas (contaminación, cambio de uso de suelo, sobre explotación, acuicultura, alteración de la hidrología o hidroperiodo) (J. López-Portillo et al., 2017). Los bosques de mangle se encuentran amenazados principalmente por conversión de hábitat, contaminación, y huracanes, y se ha estimado una pérdida del 17% de su extensión desde 1980 a nivel mundial, por lo cual es importante monitorear esta especie (A. Ellison et al., 2015), y generar información de utilidad para su manejo, conservación y restauración. Sin embargo, Rhizophora mangle no es considerada una especie en peligro de extinción, ya que tiene una distribución geográfica amplia, es común en su rango de distribución, y en muchas localidades representa la especie de mayor dominancia (A. Ellison et al., 2015), por lo tanto es de particular importancia en el mantenimiento de las funciones de amplios ecosistemas. 

_Rhizophora mangle_ típicamente alcanza tallas de 10-12 m bajo condiciones buenas. El crecimiento de los mangles usualmente disminuye a salinidades altas, mientras el crecimiento óptimo se obtiene a salinidades moderadas (B. F. Clough, 1984). Krauss & Allen (2003) reportan crecimiento óptimo con baja incidencia solar y salinidades bajas. La salinidad óptima según Duke & Allen (2006) radica entre 8-26. Los cambios espaciales y temporales de los regímenes de salinidad, como los ocasionado por inundaciones o lluvias, pueden inducir estrés hídrico a estas plantas halófitas afectando el crecimiento y la fisiología (Naidoo, 1989).
Las plantas por lo normal presentan un solo tronco y raíces aéreas limitados a la base del tronco (Gill & Tomlinson, 1969). La plántula, una vez desprendida, sirve como propágulo, y consiste en un hipocótilo, de unos 20 cm de longitud, así como una plúmula corta de 0.5 cm. La parte visible de la plúmula consiste en un par de estípulas cotiledóneas, los cuales envuelven el primer par de hojas. El crecimiento de la plántula solo se lleva a cabo cuando esta se establece e inicia siempre con el crecimiento de la raíces en el extremo distal del hipocótilo (Gill & Tomlinson, 1969). Las plántulas crecen por ramificación exponencial siléptica (Ellison & Farnsworth, 1996; Tomlinson, 1986). Las ramas y hojas se desarrollan de forma simétrica y opuesta (Gill & Tomlinson, 1971a). La reproducción típicamente no ocurre hasta el tercer año de vida (Farnsworth et al., 1996) o cuando las plántulas alcanzan una estatura de apenas de 1 m (Gill & Tomlinson, 1969). El crecimiento de las especies del genero Rhizophora puede ser limitado por la salinidad, así como características fisicoquímicas del suelo (Lin & Sternberg, 1992b).

Más allá se tiene poco conocimiento cómo el crecimiento individual afecta la dinámica poblacional de esta especie. Los modelos estructurados proveen una descripción de la dinámica poblacional, incorporando diferencias biológicas entre individuos y la forma en la cual estas diferencias afectan el destino de cada individuo (Caswell & Fujiwara, 2004). 

Los modelos estructurados se suelen denotar como ecuaciones diferenciales parciales o retardadas diferenciales dependiendo de la segregación de individuos en medición de variables continuas o cuando el intervalo de tiempo es continuo, o bien con ecuaciones integro-diferenciales, o como modelos matriciales de población, si la segregación de individuos ocurre en clases discretas en un  intervalo de tiempo discreto (Hal Caswell, 2001).

Los modelos matriciales consisten en modelos donde los individuos son separados en distintas clases basadas en su talla, edad o historia de la vida, lo cual permite estudiar el desempeño de una población, y determinar parámetros como la tasa de crecimiento de una población (λ) y otros parámetros, como sobrevivencia, crecimiento y fecundidad, a nivel de individuos (Hal Caswell, 2001). Además, se pueden realizar análisis de viabilidad poblacional (Menges, 2000), para generar recomendaciones de uso sustentable y extracción (López-Hoffman et al., 2006), o evaluar estrategias alternativas de manejo (Crouse et al., 1987; Ramula, Knight, Burns, & Buckley, 2008), y para analizar procesos más complejos de la dinámica poblacional (Ramula, Toivonen, & Mutikainen, 2007; M. Smith, Caswell, & Mettler-Cherry, 2005). Los modelos matriciales pueden representar poblaciones estructuradas por edad (matriz de Leslie) y o por estado (matriz de Leftkovich) (Hal Caswell, 2001). El ciclo de vida de la mayoría de las plantas no es compatible con los supuestos de los modelos demográficos clásicos del tipo Leslie, estructurados por edad (Hal Caswell, 2001). La edad de una planta tiene poco valor predictivo sobre su destino demográfico, por lo cual las funciones vitales no se pueden describir en términos de edad (Hal Caswell, 2001). Por lo tanto, se usa el estado de desarrollo (e.g., adulto, juvenil, plántula, semilla) o alguna medida de tamaño (e.g., peso, altura, biomasa, volumen, número de ramas, entre otras) (Hal Caswell, 2001).

Una revisión bibliográfica arrojó 3 publicaciones de análisis matricial de poblaciones de _Rhizophora sp._. Los modelos utilizados fueron matrices del tipo Lefkovitch, basados en la altura de las plantas. López-Hoffman et al. (2007b, 2006) examinaron la talla extractiva de _R. mangle_ en el área Río Limón, Lago Maracaibo, Venezuela, con la finalidad de generar un concepto de sustentabilidad, tanto del punto de vista biológico, como sociológico, estudiando y modelando los efectos de extracción sobre la dinámica poblacional. Los autores emplearon un modelo mixto, basado en 5 categorías de altura y diámetro: Plántula (<70 cm de altura); Juvenil (≥70 cm altura+ <4 cm diámetro); Adulto 1 (4-14.9 cm diámetro); Adulto 2 (15-29.9 cm diámetro); y Adulto 3 (≥30 cm diámetro). Ajonina (2008) realizó un trabajo de tesis con el objetivo de desarrollar modelos adecuados los cuales facilitan la evaluación, monitoreo y manejo sustentable de los bosques de manglar con diferentes presiones de extracción de madera, en Camerún, África Central. Utilizó modelos matriciales para modelar la dinámica la dinámica de los bosques de manglar después de diferentes grados de impacto, utilizando un modelo basado en 7 clases de perímetro basal (Clase 1: ≥1-3< cm; Clase 2: ≥3-5< cm; Clase 3: ≥5-7< cm; Clase 4: ≥7-10< cm; Clase 5: ≥10-30< cm; Clase 6: ≥30-50< cm; Clase 7: ≥50 cm). Por último, Rajkaran & Adams (2012) utilizaron modelos matriciales para evaluar el efecto de escenarios de explotación de diferentes intensidades en poblaciones de _Rhizophora mucronata_ y utilizaron un modelo basado en 5 clases de altura (Clase 1: < 50 cm; Clase 2: ≥50-151< cm; Clase 3: ≥151-251< cm; Clase 4: ≥251-351< cm; Clase 5: >350 cm). Estos estudios reportan poblaciones con tasas de crecimiento poblacionales (λ) cercanos a 1, con mayores elasticidades en la permanencia de individuos adultos.
En este capítulo se estudió el crecimiento poblacional de _Rhizophora mangle_ en función de las tasas vitales de los individuos que se desarrollan en un gradiente de la salinidad, con la finalidad de determinar cómo afecta la salinidad el desempeño de la población, haciendo uso de los modelos matriciales de proyección poblacional. Los resultados de este trabajo permitirán entender la contribución en el crecimiento poblacional de esta especie bajo distintas condiciones ambientales, así como identificar las etapas críticas en su ciclo de vida, lo cual puede ser de utilidad para desarrollar estrategias de rehabilitación y conservación, y entender cómo las alteraciones ambientales, tales como por ejemplo previstas por el cambio climático global, afectarán esta especie.

### Materiales y Métodos
El estudio se realizó en la caleta de Xel-Há, en Quintana Roo, en tres sitios que fueron seleccionados por el grado de estratificación salina (capítulo 2): Río (salinidad baja), Brazo Norte (salinidad media) y Bocana (salinidad alta). En cada sitio se estableció una parcela permanente de muestreo. El tamaño de la parcela se determinó en función de los límites y características naturales del sitio. En Río la parcela tuvo una dimensión de 30 m x 10 m, en Brazo Norte de 3 m x 6 m y en Bocana de 10 m x 3 m. 

_Demografía y estructura de la población_

Se marcó a la totalidad de individuos encontrados en cada parcela de muestreo en febrero 2014 (censo 1) con una etiqueta de latón marcada (impresión del latón y plumón indeleble) con un número consecutivo, para poder distinguir a cada individuo. La etiqueta fue amarrada con un alambre de acero inoxidable en tronco de cada planta. Se determinó la densidad de la población con un método de individuos por unidad de área (Krebs, 1985). La estructura de tallas se obtuvo a partir de la medición del perímetro basal del tronco justo encima del primer par de raíces aéreas, la altura total de la planta, el número de ramas laterales, y el número de raíces aéreas. Se volvieron a registrar todas las medidas en 3 censos consecutivos: febrero 2015, agosto 2015 y agosto 2016. Además, se registraron y marcaron todas las plantas recientemente reclutadas en cada censo. Para describir la estructura de población se usó tanto el perímetro basal, como la altura.  Para el análisis del perímetro basal de los mangles se hizo una clasificación de 6 clases: 1) 1 cm; 2) 2-4 cm; 3) 5-6 cm; 4) 7-9 cm-; 5) 10-29 cm; 6) ≥ 30 cm. Por su parte, para el análisis de la altura, también se construyeron 6 clases, con los siguientes intervalos: 1) < 25 cm; 2) 25-70 cm; 3) 71-150 cm; 4) 151-250 cm; 5) 251-350; 6) > 350 cm. Se utilizaron histogramas para describir la estructura poblacional.

_Dinámica de la población_

Para determinar la tasa de crecimiento de la población se utilizaron modelos matriciales de proyección poblacional. El modelo matricial se resuelve como: n(t+1) = Ant, donde nt y n(t+1) representan el número de individuos en cada categoría de tamaño en el tiempo t y t+1, y donde A es la matriz de proyección que contiene las probabilidades de transición de una categoría a la otra en el periodo de tiempo evaluado (Hal Caswell, 2001). Se construyó una matriz Lefkovitch de dimensión 6  6 (Lefkovitch, 1965), 1965). Dado que perímetro basal y altura resultaron significativamente correlacionados, y se encontró menor variabilidad en el aumento de perímetro basal de los individuos, se utilizó el perímetro basal como variable para describir el crecimiento poblacional. Los intervalos del perímetro basal y su equivalente en estado de desarrollo para la clasificación de 6 clases de la población de mangle son: 1) 1cm-plántula; 2) 2-4 cm-juvenil; 3) 5-6 cm-adulto 1; 4) 7-9 cm-adulto 2; 5) 10-29 cm-adulto 3; 6) ≥ 30 cm-adulto 4. El modelo matricial de Lefkovitch está formado por un vector columnar n(t) y una matriz (A={aij}) que toma la siguiente forma:

A = 

	P11	F21	F31	F41	F51	F61		n(t)
  
	C12	P22	0	0	0	0		n(t)
  
	0	C23	P33	0	0	0	x	n(t)

	0	0	C34	P44	0	0		n(t)
  
	0	0	0	C45	P55	0		n(t)
  
	0	0	0	0	C56	P66		n(t)

Donde los elementos aij corresponden a contribuciones de sobrevivencia o permanencia, crecimiento y fecundidad de los individuos de una categoría j en el tiempo t a la categoría i en el tiempo t+1. Así, Pij define la probabilidad de permanencia en el mismo estadio, Cij representa la probabilidad de que un individuo sobreviva y pase al siguiente estadio (crecimiento); y Fij representa la fecundidad en cada categoría (Hal Caswell, 2001). Por su parte, el vector de densidad poblacional n(t+1) es el vector resultante y representa el número de individuos en cada clase de talle en t+1.

La fecundidad per cápita se obtuvo de observaciones directas durante un censo en septiembre 2014 para el periodo febrero 2014-febrero 2015, para poder representar el pico en la reproducción, y directamente durante el censo de agosto 2015 para representar el pico de reproducción del periodo de agosto 2015 a agosto 2016, contando todas las estructuras florales y frutos visibles, en todas las plantas marcadas en los tres sitios (véase capítulo 4). Se utilizó el número de frutos por individuo como variable de fecundidad, ya que cada fruto representa una plántula vivípara. La clase 1 (plántula) es representada por individuos establecidos sobre el suelo, usualmente plántulas pequeñas con 2-4 hojas. En la transición de agosto 2015 a agosto 2016 no se observaron reclutas, por lo cual todos los elementos [a1j] tuvieron un valor 0, lo cual no permite correr el análisis de proyección. Por lo tanto, se sustituyó el valor de transición del elemento [a12] con una probabilidad de 0.000001.

La estimación de los parámetros demográficos se realizó utilizando el paquete popbio (Stubben & Milligan, 2007) en R (R Core Team, 2014). Las probabilidades de que un individuo sobreviva (Pi)t o pase al siguiente estadio (Ci)t se estimaron en términos de: Ci = (σt γt ) y Pi = σ (1- γi ). Asimismo, se calcularon los vectores propios derecho e izquierdo los cuales representaron la distribución estable por estadios (w) y el valor reproductivo por estadio (v), respectivamente (Hal Caswell, 2001). Se calculó lambda (λ), la tasa finita de crecimiento poblacional, que corresponde al eigenvalor dominante de la matriz A, donde la tasa de crecimiento de la población toma valores de menor a 1, lo cual indica un crecimiento poblacional negativo, valores igual a 1, lo cual indica una población en equilibrio, o valores mayores a 1, lo cuales indican una población con crecimiento positivo, así como su intervalo de confianza (IC). Se evaluaron los cambios absolutos en la tasa finita de crecimiento poblacional, λ, ocasionados por cambios en los elementos de la matriz de transición mediante un análisis de sensibilidad Sij = {sij} (Caswell, 2001):

s_ij=∂λ/(∂a_ij )=(v_i w_j)/(w,v)

Dónde wj es el jth elemento del vector propio derecho (distribución estable de estadios), vi es el ith elemento del vector propio (valor reproductivo) y  w,v  son el producto escalar de los vectores propios derecho e izquierdo. Se calculó la elasticidad (Eij = {eij}) a partir de las sensibilidades:

e_ij=∂λ/(∂a_ij )=a_ij/λ  s_ij=∂lnλ/(∂ln⁡(a_ij))

Habiendo calculado la matriz de elasticidad, se determinó la contribución porcentual de los elementos de la matriz y se construyó el triángulo de demográfico (Silvertown, Franco, & McConway, 1992), donde se sumaron las contribuciones relativas de los procesos demográficos (P, C, F). La permanencia se obtuvo P=e_11+e_22+e_33+e_44+e_55+e_66. El proceso demográfico C (crecimiento) se definió como  C=e_12+e_23+e_34+e_45+e_56. La contribución relativa de F (fecundidad) se obtuvo mediante la suma de F=e_21+e_31+e_41+e_51+e_61.
La elasticidad en cada casilla se subdividió para los 3 sitios analizados, así se presenta el resumen de la sensibilidad de lambda por proceso demográfico y categoría de tamaño de cada uno de los 3 sitios, esto se hizo siguiendo el método propuesto por Mandujano (2007), calculando primero la tabla de frecuencia para cada sitio, luego la contribución porcentual de cada sitio, y finalmente se multiplicaron estas matrices resultantes con la matriz de elasticidad.

Finalmente se realizaron simulaciones utilizando el paquete popbio (Stubben & Milligan, 2007) en R (R Core Team, 2014)  para evaluar los cambios relativos en λ como resultado de cambios en los elementos de la matriz de transición aij, sustituyendo los elementos de mayor elasticidad, los cuales fueron los elementos a22, a55 y a66. 

Basado en el análisis climático (capítulo 2) se determinó la probabilidad de que se presente un año como el 2014 y el 2015 es de 7/15 (0.47), y la probabilidad de presentarse años como el 2016 es de 1/15 (0.06). Por lo tanto, como es casi ocho veces más probable que se presente un año como 2014-2015, se construyó el modelo el modelo de proyección estocástico con una probabilidad de 7/8 (0.875) para la ocurrencia de la matriz correspondiente al periodo 2014-2015, y una probabilidad de 1/8 (0.125) para el periodo 2015-2016.

###	Resultados 

Dinámica de la población- Con los datos de perímetros basales obtenidos en los censos de febrero 2014, febrero 2015, agosto 2015 y agosto 2016 se procedió a construir los modelos matriciales de proyección. Se construyeron dos posibles ciclos de vida para el mangle rojo, para 2 periodos de transición: de febrero 2014 a febrero 2015; de agosto 2015 a agosto 2016. Todos los individuos se juntaron, independientemente del sitio de origen dado que al ser una especie polinizada por viento conforman la misma población, y porque son pocos individuos para hacer un análisis separado por condición de salinidad o sitio. El ciclo de vida resultante de la clasificación por perímetros basales de Rhizophora mangle es complejo (transición de febrero 2014-2015, Fig. 5.14.A, transición de agosto 2015-2016, Fig. 5.14.B). La lambda para el primer periodo (febrero) fue de 1.1887 (IC: 1.1500-1.1860; media=1.1550; mediana=0.1510), mientras la lambda del segundo periodo (agosto 2015-2016) fue de 0.9522 (IC: 0.9647-0.9917; media=0.9670; mediana=0.9649). Durante el primer periodo se observó una probabilidad de transición de 0.3372 para la primera clase de talla (a12), así como una probabilidad de permanencia de 0.0233 (a11). Por lo contrario, durante el segundo periodo tanto la permanencia (a11), como la probabilidad de transición (a12), fueron de 0, por lo cual se sustituyó el valor de transición con una probabilidad de 0.0000001(a12). Más allá se observó que la fecundidad per cápita fue menor en el segundo periodo, probablemente explicando los valores bajos de lambda. Se puede observar que los sitios contribuyen de forma diferencial al ciclo de vida. Las plántulas, juveniles y adultos1 se hallan casi exclusivamente en Bocana y Brazo Norte, mientras los adultos 4 son exclusivos de Río.
Se determinó el crecimiento poblacional utilizando un modelo estocástico, suponiendo que la matriz del periodo 2014-2015 tiene una probabilidad de 0.875 de ocurrir, mientras la matriz del año 2015-2016 ocurre con una probabilidad de 0.125. Se obtuvo una lambda de 1.1525 con un intervalo de confianza de 1.1414 a 1.1636 bajo este supuesto, indicando que la población encontrada en Xel-Há generalmente tiene un crecimiento positivo, bajo el supuesto que las condiciones actuales persisten, lo cual se debe probablemente al crecimiento constante de individuos y alta permanencia de adultos.
 
Figura 5.14. Ciclo de vida de Rhizophora mangle, para el periodo de (A) febrero 2014 a febrero 2015, y (B) agosto 2015 a agosto 2016, utilizando el criterio de clasificación poblacional basado en perímetros basales del tronco, donde plántula= 1cm, juvenil=2-4 cm, adulto1=5-6 cm, adulto2=7-9 cm, adulto3=10-29 cm, adulto4= ≥ 30 cm, mostrando la contribución de cada sitio (anillo: Bocana-blanco; Brazo Norte-gris, Río-negro) a la dinámica poblacional. 
Se construyeron las matrices de transición para cada uno de los sitios (anexo-Tabla 8.2), con la finalidad de determinar la sensibilidad y elasticidad por sitio. En esta tabla se puede observar que la categoría de adultos 4 fue exclusiva del sitio Río. Más allá se observó únicamente reproducción en el sitio Bocana durante el segundo periodo.
En la figura 5.15 se muestra la estructura poblacional observada y la estructura poblacional estable determinada por el modelo para ambos periodos. Se observaron diferencias significativas entre la estructura estable y observada en ambos periodos (2014-2015: Χ2cal =97.56, P<0.0001; 2015-2016: Χ2cal =523.58, P <0.0001), donde las mayores diferencias se atribuyen a la abundancia relativa de la clase de plántulas. De acuerdo con el modelo, la población requiere de más de un 60% de plántulas para alcanzar la estabilidad.
 Figura 5.15. Estructura poblacional observada y estable de Rhizophora mangle, utilizando el criterio de clasificación poblacional basado en perímetros basales (donde plántula= 1cm, juvenil=2-4 cm, adulto1=5-6 cm, adulto2=7-9 cm, adulto3=10-29 cm, adulto4= ≥ 30 cm), para (A) el periodo de febrero 2014-2015, y (B) agosto 2015-2016.
En la figura 5.16 se muestran los valores reproductivos por clase. En el primer periodo la clase adulto4 tuvo el valor reproductivo más alto, mientras en el segundo periodo fue la clase adultos3, ya que no se observó la producción de frutos en esta clase durante el último censo. 
Las dos matrices construidas tienen los mayores valores de sensibilidad en la clase de plántulas (Anexo: Tabla 8.2).
 
Figura 5.16. Valores reproductivos de Rhizophora mangle para (A) el periodo de febrero 2014-2015, y (B) agosto 2015-2016, utilizando el criterio de clasificación poblacional basado en perímetros basales, donde plántula= 1cm, juvenil=2-4 cm, adulto1=5-6 cm, adulto2=7-9 cm, adulto3=10-29 cm, adulto4= ≥ 30 cm.
Con respeto a las elasticidades, se detectaron diferencias entre los periodos estudiados. En el primer periodo (Tabla 5.2.A) los valores más altos correspondieron a la transición de la clase 1 a la clase 2 (e12), así como la permanencia de los individuos de la clase 2 (e22), indicando que el cuello de botella de la población es la sobrevivencia de los individuos del primer año. La suma de las elasticidades (Fig. 5.17) por proceso demográfico mostró que el crecimiento fue el proceso con mayor contribución, con un valor de 0.45, seguido por la permanencia con 0.33. La fecundidad contribuyó con 0.19. En el segundo periodo (Tabla 5.2.B), no hubo reclutamiento, y los mayores valores de elasticidad se observaron en la permanencia de las clases 5 (e55) y 6 (e66). Por lo tanto, el proceso demográfico con mayor contribución durante este periodo fue la permanencia (0.85) (Fig. 5.17), seguido por el crecimiento (0.07). La contribución de la fecundidad durante este periodo fue extremadamente baja. 
 
Figura 5.17. Triángulo demográfico de las elasticidades de los procesos demográficos fecundidad, permanencia, y crecimiento de Rhizophora mangle para el periodo de febrero 2014-2015, y agosto 2015-2016, en la caleta de Xel-há, Quintana Roo, utilizando el criterio de clasificación poblacional basado en perímetros basales.



Tabla 5.2. Matrices de elasticidades de Rhizophora mangle para (A) el periodo de febrero 2014-2015, y (B) agosto 2015-2016, utilizando el criterio de clasificación poblacional basado en perímetros basales.
 
Se calculó la elasticidad por sitio (Tabla 8.2 (AnexoII) Fig. 5.18 y Fig. 5.19). Para el primer periodo la matriz de Bocana explicó el 47.8% de la elasticidad. Para Bocana los procesos demográficos de mayor contribución fueron la estasis y el crecimiento (Fig. 5.18), mientras en Río fue la estasis. En el segundo periodo el proceso demográfico de mayor importancia fue la estasis en los tres sitios. 
También se detectaron diferencias en la elasticidad por clase de talla. Brazo Norte fue el sito con mayor elasticidad en la clase plántulas (Fig. 5.19). Para el segundo periodo se detectó alta elasticidad para la clase adultos 3 en los tres sitios estudiados.




 
Figura 5.18. Triángulo demográfico de las elasticidades de los procesos demográficos fecundidad, estasis, y crecimiento de Rhizophora mangle para el periodo de febrero 2014-2015, y agosto 2015-2016, en los sitios Bocana, Brazo N y Río, de la caleta de Xel-há, Quintana Roo, utilizando el criterio de clasificación poblacional basado en perímetros basales.

 
Figura 5.19. Elasticidades por clase de talla (plántula, juvenil, adulto 1, adulto 2, adulto 3 y adulto 4) de Rhizophora mangle para el periodo de febrero 2014-2015, y agosto 2015-2016, en los sitios Bocana, Brazo N y Río, de la caleta de Xel-há, Quintana Roo, utilizando el criterio de clasificación poblacional basado en perímetros basales.
Se realizaron simulaciones basados en los valores más altos de elasticidad sustituyendo las transiciones de mayor impacto sobre el ciclo de vida en la matriz con valores de 0 a 1 para medir la respuesta de lambda. Para el periodo de 2014-2015 fueron las transiciones a22 (Fig. 5.20.A) a55 (Fig. 5.20.B) y a66 (Fig. 5.20.C). Como se puede observar, el valor de lambda varía muy poco en cualquiera de los casos observados, siendo más importante para la transición a55 la cual corresponde a la permanencia de los adultos3. Para el segundo periodo se realizaron simulaciones de las transiciones a55 y a66 (Fig. 5.20). Como se puede observar la disminución de la probabilidad de permanencia de la clase adultos3 (a55) resultó en una disminución de 0.1 en lambda. La modificación de valores de la clase adultos4 (a66) tuvo poco impacto sobre lambda.
 
Figura 5.20. Simulaciones de la matriz de Lefkovitch para crecimiento poblacional de Rhizophora mangle del periodo 2014-2015 para las transiciones (A) a22, (B) a55 y (C) a66, y para el periodo 2015-2016 para las transiciones (D) a22, (E) a55 y (F) a66.

