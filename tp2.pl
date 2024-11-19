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

contenidoInversoBuffer(B, [computar | XS], C) :- contenidoInversoBuffer(B,XS,C).
contenidoInversoBuffer(B,[leer(B)|XS],C) :- member(escribir(B, _), XS), !, contenidoInversoBuffer(B,XS,L), append( C, [_], L). %member devuelve true para distintas escrituras por eso está el '!'.
contenidoInversoBuffer(B1,[leer(B2)|XS],C) :- B1 \= B2, contenidoInversoBuffer(B1,XS,C).

contenidoInversoBuffer(B,[escribir(B,P)|XS],[P|C]) :- contenidoInversoBuffer(B,XS,C).
contenidoInversoBuffer(B1,[escribir(B2,P)|XS],C) :- B1 \= B2,contenidoInversoBuffer(B1,XS,C).

contenidoBuffer(B, PS, C) :- aplanarProceso(PS, GU), reverse(GU, RS), contenidoInversoBuffer(B, RS, C1), reverse(C1, C).
%contenidoBufer tiene en PS las cosas que van a pasar, ContenidoInversoBuffer tiene en PS lo que pasó. 


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
esSeguro(escribir(_, _)).
esSeguro(computar).
esSeguro(leer(_)).
esSeguro([escribir(_, _)]).
esSeguro([computar]).
esSeguro([leer(_)]).
esSeguro([X|XS]) :- contenidoLeido([X|XS], C).
esSeguro(secuencia(P,Q)) :- esSeguro(P), esSeguro(Q) ,  forall( aplanarProceso(secuencia(P,Q), PC), contenidoLeido(PC, C) ). 
esSeguro( paralelo(P,Q) ) :- forall( aplanarProceso(paralelo(P,Q), PC), contenidoLeido(PC, C) ), buffersUsados(P, BP), buffersUsados(Q,BQ), intersection(BP, BQ, []).

esOperacionMinimalSegura(leer(_)).
esOperacionMinimalSegura(escribir(_, _)).
esOperacionMinimalSegura(computar).

esOperacionSegura(secuencia(P, Q)) :-
    esSeguro(P), 
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
generarOperacionMinimalSegura( computar , CS, BS ).  
generarOperacionMinimalSegura( escribir(B, C), CS, BS ) :- member(B, BS), member(C,CS).  
generarOperacionMinimalSegura( leer(B) , CS, BS ) :- member(B, BS).  

generarEjecucion( [], _ , _).
generarEjecucion( [X| XS], BS, CS) :-generarEjecucion(XS,BS, CS), generarOperacionMinimalSegura(X, CS, BS).
ejecucionSegura(XS, BS, CS):- generarEjecucion(XS, BS, CS), esSeguro(XS).



%% 8.1. Analizar la reversibilidad de XS, justificando adecuadamente por qué el predicado se comporta como
%% lo hace.



%%%%%%%%%%%
%% TESTS %%
%%%%%%%%%%%

% Se espera que completen con las subsecciones de tests que crean necesarias, más allá de las puestas en estos ejemplos
cantidadTestsBasicos(20).

%ej1
testBasico(1) :- proceso(computar).
testBasico(2) :- proceso(leer(1)).
testBasico(3) :- proceso(escribir(1, a)).
testBasico(4) :- proceso(secuencia(escribir(1, a), leer(1))).
testBasico(5) :- proceso(paralelo(escribir(1, a), escribir(1, b))).

testBasico(6) :- proceso(secuencia(computar, computar)).
testBasico(7) :- proceso(secuencia(computar, leer(1))).
testBasico(8) :- proceso(secuencia(computar, escribir(1, a))).
testBasico(9) :- proceso(secuencia(computar, secuencia(computar, computar))).
testBasico(10) :- proceso(secuencia(computar, paralelo(computar, computar))).

testBasico(11) :- proceso(secuencia(escribir(1, a), computar)).
testBasico(12) :- proceso(secuencia(escribir(1, a), leer(1))).
testBasico(13) :- proceso(secuencia(escribir(1, a), leer(2))).
testBasico(14) :- proceso(secuencia(escribir(1, a), escribir(1, b))).
testBasico(15) :- proceso(secuencia(escribir(1, b), secuencia(escribir(1, b), escribir(1, c)))).
testBasico(16) :- proceso(secuencia(escribir(1, a), paralelo(leer(1), escribir(2, b)))).

testBasico(17) :- proceso(secuencia(computar, secuencia(leer(1), escribir(1, a)))).
testBasico(18) :- proceso(secuencia(computar, secuencia(paralelo(leer(1), leer(2)), escribir(1, a)))).
testBasico(19) :- proceso(secuencia(computar, secuencia(paralelo(escribir(1, a), escribir(1, b)), escribir(1, c)))).
testBasico(20) :- proceso(secuencia(computar, secuencia( secuencia(leer(1), escribir(1, a)), secuencia(leer(1), escribir(1, c))))).
% Agregar más tests

cantidadTestsProcesos(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests

cantidadTestsBuffers(10). % Actualizar con la cantidad de tests que entreguen

%ej 2
testBuffers(1) :- buffersUsados(leer(1), [1]).
testBuffers(2) :- buffersUsados(escribir(1, a), [1]).
testBuffers(3) :- buffersUsados(secuencia(leer(1), escribir(1, a)), [1, 1]).
testBuffers(4) :- buffersUsados(paralelo(leer(1), escribir(2, b)), [1, 2]).
testBuffers(5) :- buffersUsados(secuencia(secuencia(leer(1), escribir(2, a)), leer(3)), [1, 2, 3]).
testBuffers(6) :- buffersUsados(paralelo(secuencia(leer(1), escribir(2, a)), escribir(3, b)), [1, 2, 3]).
testBuffers(7) :- buffersUsados(paralelo(leer(1), paralelo(escribir(2, a), leer(3))), [1, 2, 3]).
testBuffers(8) :- buffersUsados(secuencia(paralelo(leer(1), escribir(2, b)), paralelo(leer(3), escribir(4, c))), [1, 2, 3, 4]).
testBuffers(9) :- buffersUsados(secuencia(secuencia(leer(1), escribir(2, a)), secuencia(leer(3), escribir(4, b))), [1, 2, 3, 4]).
testBuffers(10) :- buffersUsados(paralelo(
    secuencia(leer(1), escribir(2, a)),
    secuencia(leer(3), paralelo(leer(4), escribir(5, b)))
), [1, 2, 3, 4, 5]).

%ej 5

testBuffers(11) :- contenidoBuffer(1, [escribir(1, a), escribir(1,b), leer(1)], [b]).
testBuffers(12) :- contenidoBuffer(1, [escribir(1, a), escribir(2, b), leer(2)], [a]).
testBuffers(13) :- contenidoBuffer(1, escribir(1, a), [a]).
testBuffers(14) :- contenidoBuffer(1, [escribir(1, a)], [a]).
testBuffers(15) :- contenidoBuffer(1, [escribir(1, a), escribir(1,b), escribir(2, e) ,escribir(1,c)], [a,b,c]).
testBuffers(16) :- contenidoBuffer(2, [escribir(1, a), escribir(1,b), escribir(2, e) ,escribir(1,c)], [e]).
testBuffers(17) :- contenidoBuffer(1,  secuencia(escribir(1, a), secuencia(escribir(1, b), secuencia(escribir(2, e), secuencia(escribir(1, c), escribir(3,f))  )) ), [a,b,c]).

testBuffers(18) :- contenidoBuffer(1,  secuencia(escribir(1, a), secuencia(escribir(1, b), secuencia(escribir(2, e), paralelo(escribir(1, c), escribir(3,f))  )) ), [a,b,c]).
testBuffers(19) :- contenidoBuffer(1,  secuencia(escribir(1, a), secuencia(escribir(1, b), secuencia(escribir(2, e), paralelo(escribir(1, c), escribir(1,d))  )) ), [a,b,c,d]).
testBuffers(20) :- contenidoBuffer(1,  secuencia(escribir(1, a), secuencia(escribir(1, b), secuencia(escribir(2, e), paralelo(escribir(1, c), escribir(1,d))  )) ), [a,b,d,c]).
testBuffers(21) :- contenidoBuffer(2,  secuencia(escribir(1, a), secuencia(escribir(1, b), secuencia(escribir(2, e), paralelo(escribir(1, c), escribir(1,d))  )) ), [e]).

testBuffers(22) :- contenidoBuffer(1,  [escribir(1,a), escribir(1,b), escribir(2,e),escribir(1,c), leer(1), escribir(1,d)] , [b,c,d]).
testBuffers(23) :- contenidoBuffer(2,  [escribir(1,a), escribir(1,b), escribir(2,e),escribir(1,c), leer(1), escribir(2,d)] , [e,d]).
testBuffers(24) :- contenidoBuffer(2,  [escribir(1,a), escribir(1,b), escribir(2,e),escribir(1,c), leer(2), escribir(2,d)] , [d]).

% FALTAN

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


