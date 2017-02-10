// Bayesian Network
//   Elvira format 

bnet  "PracticaSerafin" { 

// Network Properties

kindofgraph = "directed";
visualprecision = "0.00";
version = 1.0;
default node states = (presente , ausente);

// Variables 

node AEstudiosMadre(finite-states) {
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =367;
pos_y =171;
relevance = 7.0;
purpose = "";
num-states = 3;
states = ("Altos" "Medios" "Bajos");
}

node BEstudiosPadre(finite-states) {
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =619;
pos_y =173;
relevance = 7.0;
purpose = "";
num-states = 3;
states = ("Altos" "Medios" "Bajos");
}

node CIngresosPadre(finite-states) {
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =618;
pos_y =249;
relevance = 7.0;
purpose = "";
num-states = 3;
states = ("Altos" "Medios" "Bajos");
}

node DIngresosMadre(finite-states) {
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =365;
pos_y =252;
relevance = 7.0;
purpose = "";
num-states = 3;
states = ("Altos" "Medios" "Bajos");
}

node TIngresosTotalesFamilia(finite-states) {
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =486;
pos_y =332;
relevance = 7.0;
purpose = "";
num-states = 3;
states = ("Altos" "Medios" "Bajos");
}

node EPresenciaInternetCasa(finite-states) {
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =342;
pos_y =427;
relevance = 7.0;
purpose = "";
num-states = 2;
states = ("Presente" "Ausente");
}

node FCoche(finite-states) {
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =741;
pos_y =424;
relevance = 7.0;
purpose = "";
num-states = 3;
states = ("Más de uno" "Uno" "Ninguno");
}

node GGastosFamiliaOcio(finite-states) {
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =528;
pos_y =427;
relevance = 7.0;
purpose = "";
num-states = 3;
states = ("Altos" "Medios" "Bajos");
}

node HHijos(finite-states) {
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =655;
pos_y =427;
relevance = 7.0;
purpose = "";
num-states = 4;
states = ("Más de dos" "Dos" "Uno" "Ninguno");
}

node IGustaFutbol(finite-states) {
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =180;
pos_y =428;
relevance = 7.0;
purpose = "";
num-states = 2;
states = ("Si" "No");
}

node JTelevisionPago(finite-states) {
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =301;
pos_y =518;
relevance = 7.0;
purpose = "";
num-states = 2;
states = ("Si" "No");
}

node HVideoconsolas(finite-states) {
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =549;
pos_y =519;
relevance = 7.0;
purpose = "";
num-states = 2;
states = ("Si" "No");
}

// Links of the associated graph:

link AEstudiosMadre DIngresosMadre;

link BEstudiosPadre CIngresosPadre;

link CIngresosPadre TIngresosTotalesFamilia;

link DIngresosMadre TIngresosTotalesFamilia;

link EPresenciaInternetCasa JTelevisionPago;

link GGastosFamiliaOcio HVideoconsolas;

link HHijos HVideoconsolas;

link IGustaFutbol JTelevisionPago;

link TIngresosTotalesFamilia EPresenciaInternetCasa;

link TIngresosTotalesFamilia FCoche;

link TIngresosTotalesFamilia GGastosFamiliaOcio;

link TIngresosTotalesFamilia HHijos;

//Network Relationships: 

relation AEstudiosMadre { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.35 0.55 0.1 );
}

relation BEstudiosPadre { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.35 0.55 0.1 );
}

relation DIngresosMadre AEstudiosMadre { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.8 0.55 0.1 0.17 0.4 0.35 0.03 0.05 0.55 );
}

relation CIngresosPadre BEstudiosPadre { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.85 0.7 0.2 0.13 0.26 0.45 0.02 0.04 0.35 );
}

relation TIngresosTotalesFamilia CIngresosPadre DIngresosMadre { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.95 0.82 0.76 0.9 0.35 0.2 0.65 0.25 0.05 0.04 0.15 0.2 0.07 0.5 0.35 0.3 0.35 0.3 0.01 0.03 0.04 0.03 0.15 0.45 0.05 0.4 0.65 );
}

relation GGastosFamiliaOcio TIngresosTotalesFamilia { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.55 0.4 0.15 0.4 0.35 0.35 0.05 0.25 0.5 );
}

relation EPresenciaInternetCasa TIngresosTotalesFamilia { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.85 0.7 0.55 0.15 0.3 0.45 );
}

relation JTelevisionPago EPresenciaInternetCasa IGustaFutbol { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.8 0.7 0.18 0.05 0.2 0.3 0.82 0.95 );
}

relation HVideoconsolas GGastosFamiliaOcio HHijos { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.95 0.8 0.75 0.55 0.8 0.7 0.65 0.4 0.65 0.55 0.4 0.25 0.05 0.2 0.25 0.45 0.2 0.3 0.35 0.6 0.35 0.45 0.6 0.75 );
}

relation HHijos TIngresosTotalesFamilia { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.2 0.15 0.05 0.3 0.3 0.2 0.35 0.35 0.5 0.15 0.2 0.25 );
}

relation FCoche TIngresosTotalesFamilia { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.65 0.35 0.15 0.3 0.45 0.4 0.05 0.2 0.45 );
}

relation IGustaFutbol { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.65 0.35 );
}

}
