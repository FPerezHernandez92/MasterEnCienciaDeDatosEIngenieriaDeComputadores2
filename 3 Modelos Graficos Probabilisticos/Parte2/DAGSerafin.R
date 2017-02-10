#Francisco Pérez Hernández PARTE 2 Opcional
library(bnlearn)
#CREACIÓN DEL DAG DE LA RED BAYESIANA
#Crear DAG de PARTE 1 de Serafín
dagS <- model2network("[A][B][D|A][C|B][T|D:C][E|T][G|T][H|T][I][F|T][V|H:G][J|E:I]")
modelstring(dagS)

#DIBUJANDO EL DAG DE LA RED BAYESIANA
graphviz.plot(dagS)

#ESPECIFICACIÓN DE LAS PROBABILIDADES
#Añadir tablas de probabilidad
A.st <- c("Altos", "Medios", "Bajos")
A.prob <- array(c(0.35, 0.55, 0.10), dim=3,
                dimnames = list(A = A.st))
A.prob
B.st <- c("Altos", "Medios", "Bajos")
B.prob <- array(c(0.35, 0.55, 0.10), dim=3,
                dimnames = list(B = B.st))
B.prob

D.st <- c("Altos", "Medios", "Bajos")
D.prob <- array(c(0.8, 0.17, 0.03, 0.55, 0.4, 0.05, 0.1, 0.35, 0.55), dim=c(3,3), 
                dimnames = list(D = D.st, A = A.st))
D.prob
C.st <- c("Altos", "Medios", "Bajos")
C.prob <- array(c(0.85, 0.13, 0.02, 0.7, 0.26, 0.04, 0.2, 0.45, 0.35), dim=c(3,3), 
                dimnames = list(C = C.st, B = B.st))
C.prob

T.st <- c("Altos", "Medios", "Bajos")
T.prob <- array(c(0.95, 0.04, 0.01, 0.82, 0.15, 0.03, 0.76, 0.2, 0.04, 
                  0.9, 0.07, 0.03, 0.35, 0.5, 0.15, 0.2, 0.35, 0.45, 
                  0.65, 0.3, 0.05, 0.25, 0.35, 0.4, 0.05, 0.3, 0.65)
                , dim = c(3,3,3), dimnames = list(T = T.st, D = D.st, C=C.st))
T.prob

H.st <- c("Más de dos", "Dos", "Uno", "Ninguno")
H.prob <- array(c(0.2, 0.3, 0.35, 0.15,
                  0.15, 0.3, 0.35, 0.2,
                  0.05, 0.2, 0.5, 0.25),
                dim = c(4,3), dimnames = list(H = H.st, T = T.st))
H.prob

G.st <- c("Altos", "Medios", "Bajos")
G.prob <- array(c(0.55, 0.4, 0.05,
                  0.4, 0.35, 0.25,
                  0.15, 0.35, 0.5),
                dim = c(3,3), dimnames = list(G=G.st, T= T.st))
G.prob

V.st <- c("Si", "No")
V.prob <- array(c(0.95, 0.05,
                  0.8, 0.2,
                  0.75, 0.25,
                  0.55, 0.45,
                  0.8, 0.2,
                  0.7,0.3,
                  0.65, 0.35,
                  0.4, 0.6,
                  0.65, 0.35, 
                  0.55, 0.45, 
                  0.4, 0.6, 
                  0.25, 0.75),
                dim = c(2,4,3), dimnames = list(V=V.st, H=H.st, G = G.st))
V.prob

E.st <- c("Presente", "Ausente")
E.prob <- array(c(0.85, 0.15,
                  0.7, 0.3,
                  0.55, 0.45),
                dim = c(2,3), dimnames = list(E=E.st, T=T.st))
E.prob

I.st <- c("Si", "No")
I.prob <- array(c(0.65, 0.35), dim=2, dimnames = list(I=I.st))
I.prob

F.st <- c("Más de uno", "Uno", "Ninguno")
F.prob <- array(c(0.65, 0.3, 0.05,
                  0.35, 0.45, 0.2,
                  0.15, 0.4, 0.45),
                dim = c(3,3), dimnames = list(F=F.st, T=T.st))
F.prob

J.st <- c("Si", "No")
J.prob <- array(c(0.8, 0.2,
                  0.7, 0.3, 
                  0.18, 0.82,
                  0.05,0.95),
                dim = c(2,2,2), dimnames = list(J=J.st, I=I.st, E=E.st))
J.prob

#CREACIÓN DE LA RED BAYESIANA
cpt <- list(A=A.prob, B=B.prob, D=D.prob, C=C.prob, T=T.prob, E=E.prob, F=F.prob, I=I.prob,
            G=G.prob, H=H.prob, J=J.prob, V=V.prob)
#Creamos la red bayesiana:
bn <- custom.fit(dagS,cpt)
#Número de parámetros:
nparams(bn)
#Arcos:
arcs(bn)
#Tablas de probabilidad condicional:
bn$T
bn$V
#Todas las distribuciones de probabilidad condicional:
bn

#PROBABILIDADES A POSTERIORI
#Para obtener la probabilidad a posteriori de una variable dada una evidencia:
library(gRain)
junction <- compile(as.grain(bn))
#Para cada estado de T sin ninguna evidencia:
querygrain(junction, nodes="T")$T
#Si la evidencia es que A sean Altos:
jAAltos <- setEvidence(junction, nodes="A", states="Altos")
querygrain(jAAltos, nodes="T")$T
#Viendo la comparativa de antes y ahora:
querygrain(junction, nodes="T")$T
querygrain(jAAltos, nodes="T")$T

#Ahora vamos a hacer que se tengan Dos hijos, para ver que pasa con las Videoconsolas V:
querygrain(junction, nodes="V")$V
jHDos <- setEvidence(junction, nodes="H", states = "Dos")
querygrain(jHDos, nodes="V")$V
#Además los gastos en Ocio G, van a ser altos:
jGH <- setEvidence(jHDos, nodes="G", states="Altos")
querygrain(jGH, nodes="V")$V
#Aumentando mucho

#Otra consulta será que los ingresos totales T, sean Altos, y guste el futbol I, para ver 
#si hay televisión de pago
querygrain(junction, nodes="J")$J
jTAltos <- setEvidence(junction, nodes="T", states="Altos")
querygrain(jTAltos, nodes="J")$J
jIT <- setEvidence(jTAltos, nodes="I", states="Si")
querygrain(jIT, nodes="J")$J
#Viendo como quedaría con esas evidencias


#D-SEPARACIÓN
#Si queremos ver si el valor de F influye para conocer J, conociendo T:
dsep(bn, x="F", y="J", z="T")
#Viendo que estan d-separados
#Esto no ocurre entre E,I y J
dsep(bn, x="E", y="I", z="J")
#Efectivamente
#Veamos si los estudios del padre B, influyen a tener Videoconsola, sabiendo G
dsep(bn, x="B", y="V", z="G")
#Como era de esperar