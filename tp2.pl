%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicados básicos %%
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% proceso(+P)
proceso(computar).
proceso(leer(B)).
proceso(escribir(B,X)).
proceso(secuencia(P,Q)) :- proceso(P),proceso(Q).
proceso(paralelo(P,Q)) :- proceso(P),proceso(Q).

%% Ejercicio 2
%% buffersUsados(+P,-BS)
buffersUsados(leer(B),[B]).
buffersUsados(escribir(B,_),[B]).
buffersUsados(secuencia(P,Q),BS) :- buffersUsados(P,R1),buffersUsados(Q,R2),append(R1,R2,BS). 
buffersUsados(paralelo(P,Q),BS) :- buffersUsados(P,R1),buffersUsados(Q,R2),append(R1,R2,BS). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Organización de procesos %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 3
%% intercalar(+XS,+YS,?ZS)
intercalar([],[],[]).
intercalar([X|XS],YS,[X|ZS]) :- intercalar(XS,YS,ZS).
intercalar(XS,[Y|YS],[Y|ZS]) :- intercalar(XS,YS,ZS).


%% Ejercicio 4
%% serializar(+P,?XS)
serializar(leer(B),[leer(B)]).
serializar(escribir(B,X),[escribir(B,X)]).
serializar(computar,[computar]).
serializar(secuencia(P,Q),XS) :- serializar(P,R1),serializar(Q,R2),append(R1,R2,XS).
serializar(paralelo(P,Q),XS) :- serializar(P,R1),serializar(Q,R2),intercalar(R1,R2,XS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% contenidoBuffer(+B,+ProcesoOLista,?Contenidos)
contenidoBuffer(B, [], []).
contenidoBuffer(B,[leer(B)|XS],C) :- contenidoBuffer(B,XS,L), [_ | C] is L .
contenidoBuffer(B1,[leer(B2)|XS],C) :- B1 \= B2,contenidoBuffer(B1,XS,C).
contenidoBuffer(B,[escribir(B,P)|XS],[P|C]) :- contenidoBuffer(B,XS,C).
contenidoBuffer(B1,[escribir(B2,P)|XS],C) :- B1 \= B2,contenidoBuffer(B1,XS,C).
contenidoBuffer(B,[secuencia(P,Q)|XS],C) :- contenidoBuffer(B,P,C1), contenidoBuffer( B, Q, C2 ), append(C1, C2, C3),
                                        append(C3, XS, RS), contenidoBuffer( B, RS, C ).
contenidoBuffer(B, [paralelo(P,Q)|XS], C) :- contenidoBuffer(B, P, C1), contenidoBuffer(B, Q, C2),
                                        intercalar( C1, C2, C3 ), append(C3, XS, RS), contenidoBuffer(B, RS, C).
contenidoBuffer(B, leer(F), [] ).
contenidoBuffer(B, escribir(B, P), [P]).
contenidoBuffer(B1, escribir(B2, F), []).
contenidoBuffer(B, secuencia(P,Q), C) :- contenidoBuffer(B, [secuencia(P,Q)], C).
contenidoBuffer(B, paralelo(P,Q), C) :- contenidoBuffer(B, [paralelo(P,Q),C]).

%% Ejercicio 6
%% contenidoLeido(+ProcesoOLista,?Contenidos)
contenidoLeido(_,_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 7
%% esSeguro(+P)

%% Ejercicio 8
%% ejecucionSegura( XS,+BS,+CS) - COMPLETAR LA INSTANCIACIÓN DE XS
ejecucionSegura(_,_,_).

%% 8.1. Analizar la reversibilidad de XS, justificando adecuadamente por qué el predicado se comporta como
%% lo hace.



%%%%%%%%%%%
%% TESTS %%
%%%%%%%%%%%

% Se espera que completen con las subsecciones de tests que crean necesarias, más allá de las puestas en estos ejemplos

cantidadTestsBasicos(2). % Actualizar con la cantidad de tests que entreguen
testBasico(1) :- proceso(computar).
testBasico(2) :- proceso(secuencia(escribir(1,pepe),escribir(2,pipo))).
testBasico(3) :- buffersUsados(escribir(1, hola), [1]).
% Agregar más tests

cantidadTestsProcesos(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests

cantidadTestsBuffers(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests

cantidadTestsSeguros(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests


tests(basico) :- cantidadTestsBasicos(M), forall(between(1,M,N), testBasico(N)).
tests(procesos) :- cantidadTestsProcesos(M), forall(between(1,M,N), testProcesos(N)).
tests(buffers) :- cantidadTestsBuffers(M), forall(between(1,M,N), testBuffers(N)).
tests(seguros) :- cantidadTestsSeguros(M), forall(between(1,M,N), testSeguros(N)).

tests(todos) :-
  tests(basico),
  tests(procesos),
  tests(buffers),
  tests(seguros).

tests :- tests(todos).


