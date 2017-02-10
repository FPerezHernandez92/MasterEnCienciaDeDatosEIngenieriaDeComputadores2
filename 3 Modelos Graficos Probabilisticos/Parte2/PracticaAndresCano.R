#Francisco Pérez Hernández

# 1 INTRODUCCIÓN AL GUIÓN
library(bnlearn)

# 2 EL PROBLEMA A MODELAR: ENCUESTA SOBRE USO DE MEDIOS DE TRANSPORTE

# 3 CREACIÓN DEL DAG DE LA RED BAYESIANA
#Crearemos el DAG de la red bayesiana, a partir de un grafo dirigido acíclico vacío o aleatorio 
#para un conjunto de nodos dado. 
dag <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))
#Mostramos:
dag
#Añadimos los arcos entre los nodos:
dag <- set.arc(dag, from = "A", to = "E")
dag
#Añadimos el resto de arcos:
dag <- set.arc(dag, from = "S", to = "E") #S-E
dag <- set.arc(dag, from = "E", to = "O") #E-O
dag <- set.arc(dag, from = "E", to = "R") #E-R
dag <- set.arc(dag, from = "O", to = "T") #O-T
dag <- set.arc(dag, from = "R", to = "T") #R-T
#Quedando:
dag
#Para ver el modelo:
modelstring(dag)
#También podríamos haber añadido los arcos con el comando:
modelstring(dag)= "[A][S][E|A:S][O|E][R|E][T|O:R]"
modelstring(dag)
#Dando el mismo resultado
#También con la función model2network podríamos haber creado el DAG:
dag3 <- model2network("[A][S][E|A:S][O|E][R|E][T|O:R]")
modelstring(dag3)
#Para comprobar que son iguales dag y dag3 podemos usar:
all.equal(dag,dag3)
#Podemos mostrar la lista de nodos del DAG de la red bayesiana con:
nodes(dag)
#También se puede mostrar la lista de enlaces:
arcs(dag)
#Otra forma alternativa para añadir los arcos a un DAG es: 
dag2 <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))
arc.set <- matrix(c("A", "E", "S", "E",  "E", "O",  "E", "R", "O", "T", "R", "T"),
                  byrow = TRUE, ncol = 2, dimnames = list(NULL, c("from", "to")))
arcs(dag2) <- arc.set
#Comprobemos que son iguales:
all.equal(dag,dag2)
#Las dos formas nos garantizan que el DAG es acíclico. Intentemos introducir un ciclo:
try(set.arc(dag, from="T", to="E"))
#Por lo que no está permitido

# 4 DIBUJANDO EL DAG DE LA RED BAYESIANA
#Con plot se puede mostar un gráfico del DAG:
plot(dag)
#Otra alternativa sería:
graphviz.plot(dag)
#De forma que esta función minimiza el solapamiento. 

# 5 ESPECIFICACIÓN DE LAS PROBABILIDADES
#En la red bayesiana, todas las variables son discretas y por tanto cada una está definida para
#un número finito y  no ordenado de estados (el dominio de esa variable). Definimos los estados
#de las variables de la red bayesiana:
A.st <- c("young", "adult", "old")
S.st <- c("M", "F")
E.st <- c("high", "uni")
O.st <- c("emp", "self")
R.st <- c("small", "big")
T.st <- c("car", "train", "other")
#Podemos definir las seis distribuciones de probabilidad condicional de la anterior factorización: 
A.prob <- array(c(0.30, 0.50, 0.20), dim=3, dimnames = list(A = A.st))
#Para mostrar las probabilidades que acabamos de definir para P(A):
A.prob
#Para P(S) haríamos:
S.prob <- array(c(0.60, 0.40), dim=2, dimnames = list(S = S.st))
S.prob
#Las distribuciones condicionales:
O.prob <- array(c(0.96, 0.04, 0.92, 0.08), dim=c(2,2), dimnames = list(O = O.st, E = E.st))
O.prob
R.prob <- array(c(0.25, 0.75, 0.20, 0.80), dim=c(2,2), dimnames = list(R = R.st, E = E.st))
R.prob
#Para las distribuciones unidmensionales y bidimensionales podemos usar: 
R.prob <- matrix(c(0.25, 0.75, 0.20, 0.80), ncol = 2, dimnames = list(R = R.st, E = E.st))
E.prob <- array(c(0.75, 0.25, 0.72, 0.28, 0.88, 0.12, 0.64, 0.36, 0.70, 0.30, 0.90, 0.10), dim=c(2, 3, 2),
                dimnames = list(E = E.st, A = A.st, S = S.st))
T.prob <- array(c(0.48, 0.42, 0.10, 0.56, 0.36, 0.08, 0.58, 0.24, 0.18, 0.70, 0.21, 0.09), dim=c(3, 2, 2),
                dimnames = list(T = T.st, O = O.st, R = R.st))

# 6 CREACIÓN DE LA RED BAYESIANA
#Ahora crearemos un alista:
cpt <- list(A=A.prob, S=S.prob, E=E.prob, O=O.prob, R=R.prob, T=T.prob)
#Ya tenemos todo lo necesario para crear nuestra red bayesiana:
bn <- custom.fit(dag,cpt)
#Podemos calcular el número de parámetros de la red bayesiana: 
nparams(bn)
#Podemos:
arcs(bn)
#Podemos mostrar las tablas de probabilidad condicional:
bn$R
#Podemos extraer una tabla de distribución de probabildiad condicional:
R.cpt <- coef(bn$R)
R.cpt
#Para mostar todas las distribuciones de probabilidad:
bn

# 7 ESTIMACIÓN DE LOS PARÁMETROS: DISTRIBUCIONES DE PROBABILIDAD CONDICIONAL
#Abrimos el fichero:
survey <- read.table("survey.txt", header = TRUE)
#Podemos mostrar las priemras líneas
head(survey)
#Vamos a calcular los estimadores especificando un DAG
bn.mle <- bn.fit(dag, data=survey, method="mle")
#Se podrían haber calculado manualmente:
prop.table(table(survey[,c("O","E")]),margin=2)
#Dando el mismo resultado que nos dió:
bn.mle$O
#Podemos estimar las probabilidades por un procedimiento bayesiano, con las distribuciones a posteriori:
bn.bayes <- bn.fit(dag, data=survey, method="bayes",iss=10)
#Si consultamos la distribución estimada para:
bn.bayes$O
#Están más lejos de 0 o 1 que cuando se usó el método de máxima verosimilitud debido a la influencia
#de la distribución a priori.
#Cuanto mayor sea el valor de iss, más planas serán las distribuciones a posteriori estimadas, empujándolas
#hacia la distribución uniforme usada como la a priori.
bn.bayes <- bn.fit(dag, data=survey, method="bayes", iss=20)
bn.bayes$O

# 8 APRENDIENDO EL DAG: TESTS Y SCORES
#Para evaluar posibles DAGs tenemos el test de independencia condicional y scores de redes. 
#Para usar scores haciendo uso de greedy:
learned <- hc(survey)
modelstring(learned)
score(learned, data=survey,type="bic")
#Si queremos usar otro tipo de score:
learned2 <- hc(survey, score="bde")

# 9 INFERENCIA EN REDES BAYESIANAS
# 9.1 OBTENCIÓN DE LAS INDEPENDENCIAS EXPRASDAS POR EL DAG
#Podemos saber si dos variables x e y del DAG están d-separadas dada otra variable z o un vector de variables
dsep(dag, x = "S", y = "R")
dsep(dag, x = "O", y = "R")
#Podemos ver si existe un camino dirigido de un nodo a otro:
path(dag, from = "S", to = "R")
#El camino entre S y R queda bloqueado si condicionamos sobre E(S y R se vuelven independientes conocido el 
# valor de E) como puede verse:
dsep(dag, x = "S", y = "R", z = "E")

#Otro ejemplo: 
dsep(dag, x="S", y="T", z=c("O", "R"))
dsep(dag, x="O", y="R", z="E")
#El condicionar sobre otras variables, puede hacer que variables que eran idependientes marginalmente (sin
#evidencia) se vuelvan dependientes.
dsep(dag, x="A", y="S")
dsep(dag, x="A", y="S", z="E")

# 9.2 INFERENCIA EXACTA
library(gRain)
#Para construir el árbol de grupos con el paquete gRain, debemos convertir nuestra red bayesiana en un objeto
#de la clase grain. 
junction <- compile(as.grain(bn))
#Para obtener la distribución de probabilidad a posteriori para una variable dada la evidencia:
querygrain(junction, nodes="T")$T
#Anteriormente no estaba introducida la envidencia en la red bayesiana. Una vez construido el árbol y
#calculadas las tablas de probabildiad a posteirori, podemos definir la evidencia:
jsex <- setEvidence(junction, nodes="S", states="F")
#Para obtener de nuevo:
querygrain(jsex, nodes="T")$T
#Se puede ver que no han cambiado las evidencias. Esto indica que las mujeres muestran más o menos las mismas
#preferencias hacia el uso de coche o tren que la población entrevistada como un todo.
#Vamos a hacer otra consulta para conocer cómo afecta el vivir en una ciudad pequeña al uso de coche o tren.
jres <- setEvidence(junction, nodes="R", states="small")
querygrain(jres, nodes="T")$T
#Es posible también obtener la probabilidad conjunta condicional dada la evidencia para más de una variable
#a la vez:
jedu <- setEvidence(junction, nodes="E", states="high")
SxT.cpt <- querygrain(jedu, nodes= c("S","T"), type = "joint")
SxT.cpt
#Si usamos en type, "marginal" nos da la distribución marginal para S y la marginal para T
querygrain(jedu, nodes = c("S", "T"), type = "marginal")
#Si usamos "conditional" tendremos la distribución de probabilidad de la primera variable en nodes condicionado 
#al resto de variables:
querygrain(jedu, nodes = c("S", "T"), type = "conditional")
#Todas las probabilidad son idénticas, idependientemente del valor de T
#Esto se puede extender con el critero de la d-separación:
dsep(bn, x="S", y="T", z="E")

# 9.3 INFERENCIA APROXIMADA
#Para usar el algoritmo de muestreo lógico usamos:
cpquery(bn, event = (S=="M") & (T=="car"), evidence=(E=="high"))
#Cambiaremos el valor por defecto de n en cpquery:
cpquery(bn,event=(S=="M") & (T=="car"),evidence=(E=="high"),n=10^6)
#Una aproximación que suele funcionar mejor es el método de ponderación por verosimilitud, donde ninguna muestra
#es rechazada:
cpquery(bn, event = (S == "M") & (T == "car"), evidence = list(E = "high"), method = "lw")
#Se pueden hacer consultas más complejas como:
cpquery(bn, event = (S == "M") & (T == "car"), evidence = ((A == "young") & (E == "uni")) | (A == "adult"))
#El algoritmo de ponderación por verosimilitud no está implementado para permitir hacer el anterior tipo de
#consulta. Por lo que podemos devolver un data frame de la siguiente forma:
SxT <- cpdist(bn, nodes = c("S","T"), evidence = (E=="high"))
head(SxT)
#Ahora se puede usar para obtener la probabilidad conjunta a posteriori: 
prop.table(table(SxT))
