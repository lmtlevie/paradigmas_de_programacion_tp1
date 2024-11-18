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
%% contenidoInversoBuffer(+B,+ProcesoOLista,?Contenidos)

aplanarProceso( [],[] ).
aplanarProceso( computar,[] ).
aplanarProceso( escribir(B, P) ,[escribir(B, P) ] ).
aplanarProceso( leer(B),[leer(B)] ).
aplanarProceso(secuencia(P,Q), RS) :- aplanarProceso(P, PS), aplanarProceso(Q, QS), append(PS, QS, RS). 
aplanarProceso(paralelo(P,Q), RS) :- aplanarProceso(P, PS), aplanarProceso(Q, QS), intercalar(PS, QS, RS). 

aplanarProceso( [X| XS], RS ) :- aplanarProceso(X, XR), aplanarProceso(XS, XSR), append(XR, XSR, RS).


contenidoInversoBuffer(B, [], []).
contenidoInversoBuffer(B, computar, []). 
contenidoInversoBuffer(B, escribir(B, P), [P]).
contenidoInversoBuffer(B1, escribir(B2, F), []):- B1 \= B2.

contenidoInversoBuffer(B, [computar | XS], C) :- contenidoInversoBuffer(B,XS,C).
contenidoInversoBuffer(B,[leer(B)|XS],C) :- member(escribir(B, _), XS), contenidoInversoBuffer(B,XS,L), append( C, [_], L).
contenidoInversoBuffer(B1,[leer(B2)|XS],C) :- B1 \= B2, contenidoInversoBuffer(B1,XS,C).

contenidoInversoBuffer(B,[escribir(B,P)|XS],[P|C]) :- contenidoInversoBuffer(B,XS,C).
contenidoInversoBuffer(B1,[escribir(B2,P)|XS],C) :- B1 \= B2,contenidoInversoBuffer(B1,XS,C).

contenidoBuffer(B, PS, C) :- aplanarProceso(PS, GU), reverse(GU, RS), contenidoInversoBuffer(B, RS, C1), reverse(C1, C).



%% Ejercicio 6
%% contenidoLeido(+ProcesoOLista,?Contenidos)
contenidoAplanadoLeido( [], []).
contenidoAplanadoLeido( [leer(B) | XS], C) :-select( escribir(B,CACA), XS, XSSC),!,  contenidoAplanadoLeido(XSSC, RS), append([CACA], RS, C).
contenidoAplanadoLeido(  [escribir(_, _) | XS], C ) :- contenidoAplanadoLeido(XS, C).

contenidoLeido( POL , C) :- aplanarProceso(POL, APOL), reverse(APOL, RAPOL), contenidoAplanadoLeido(RAPOL, CR), reverse(CR, C).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 7
%% esSeguro(+P)
esSeguro([]).

% Verifica cada operación en la lista.
esSeguro([ X| XS]):-esSeguro(XS), esOperacionMinimalSegura(X).
esSeguro([X | XS]) :-
    esSeguro(XS),           % Verifica recursivamente el resto de la lista.
    esOperacionSegura(X). % Verifica si la operación X es segura.

esOperacionMinimalSegura(leer(_)).
esOperacionMinimalSegura(escribir(_, _)).
esOperacionMinimalSegura(computar).

esOperacionSegura(secuencia(P, Q)) :-
    esSeguro(P), % Ambos procesos deben ser seguros.
    esSeguro(Q),
    forall(aplanarProceso(secuencia(P, Q), PC), contenidoLeido(PC, _)).

esOperacionSegura(paralelo(P, Q)) :-
    esSeguro(P),
    esSeguro(Q),
    forall(aplanarProceso(paralelo(P, Q), PC), contenidoLeido(PC, _)),
    buffersUsados(P, BP),
    buffersUsados(Q, BQ),
    intersection(BP, BQ, []). % No deben compartir buffers.
%% Ejercicio 8
%% ejecucionSegura( XS,+BS,+CS) - COMPLETAR LA INSTANCIACIÓN DE XS

ejecucionSegura([], _,_).
ejecucionSegura([computar|XS], BS, CS):- esSeguro(XS), ejecucionSegura(XS, BS, CS).
ejecucionSegura([leer(B)|XS], BS, CS):- esSeguro(XS), member(B, BS), ejecucionSegura(XS, BS, CS).
ejecucionSegura([escribir(B,C)|XS], BS, CS):-esSeguro(XS), member(B, BS), member(C,CS), ejecucionSegura(XS, BS, CS).




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
testBasico(4) :- contenidoInversoBuffer(1,[escribir(1,hola), escribir(2,no), escribir(2,2),leer(1), escribir(1,eyy),escribir(1,a)],[hola,a]).
testBasico(5) :- contenidoInversoBuffer(2,[escribir(1,pp),escribir(2,ala),escribir(1,ola), computar,escribir(1,mundo),leer(1)],[ala] ).
testBasico(6) :- contenidoBuffer(1,[escribir(1,pa),escribir(2,ma),escribir(1,hola), computar,escribir(1,mundo),leer(1)],C).
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


