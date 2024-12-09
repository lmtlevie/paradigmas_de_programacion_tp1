%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicados básicos %%
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% proceso(+P)
proceso(computar).
proceso(leer(_)).
proceso(escribir(_,_)).
proceso(secuencia(P,Q)) :- proceso(P),proceso(Q).
proceso(paralelo(P,Q)) :- proceso(P),proceso(Q).

%% Ejercicio 2
%% buffersUsados(+P,-BS)
buffersUsados(leer(B),[B]).
buffersUsados(escribir(B,_),[B]).
buffersUsados(secuencia(P,Q),BS) :- buffersUsadosSecuenciaParalelo(P,Q,BS).
buffersUsados(paralelo(P,Q),BS) :- buffersUsadosSecuenciaParalelo(P,Q,BS).

%% buffersUsadosSecuenciaParalelo(+P,+Q,-BS)
buffersUsadosSecuenciaParalelo(P,Q,BS) :- buffersUsados(P,R1),buffersUsados(Q,R2),append(R1,R2,B), sort(B, BS).


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
serializar([],[]).
serializar(leer(B),[leer(B)]).
serializar(escribir(B,X),[escribir(B,X)]).
serializar(computar,[computar]).
serializar(secuencia(P,Q),XS) :- serializar(P,R1),serializar(Q,R2),append(R1,R2,XS).
serializar(paralelo(P,Q),XS) :- serializar(P,R1),serializar(Q,R2),intercalar(R1,R2,XS).
serializar([X| XS], RS ) :- serializar(X, XR), serializar(XS, XSR), append(XR, XSR, RS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% contenidoInversoBuffer(+B,+ProcesoOLista,?Contenidos)
contenidoBuffer(B, Proceso, C):- serializar(Proceso, Lista), contenidoBufferDeSerializacion(B, Lista, C). 

contenidoBufferDeSerializacion(_, [], []).
contenidoBufferDeSerializacion(B,Lista,C) :-  append( ListaSinUltimoElemento, [computar], Lista ), contenidoBuffer(B, ListaSinUltimoElemento, C). 
contenidoBufferDeSerializacion(B1,Lista,C) :- append( ListaSinUltimoElemento, [leer(B2)], Lista ), B2 \= B1, contenidoBuffer(B1, ListaSinUltimoElemento, C). 

contenidoBufferDeSerializacion(B,Lista,C1) :- append( ListaSinUltimoElemento, [leer(B)], Lista ), 
                                    contenidoBuffer(B, ListaSinUltimoElemento, C2),
                                    append([_], C1, C2).

contenidoBufferDeSerializacion(B,Lista,C) :-   append( ListaSinUltimoElemento, [escribir(B, P)], Lista ),
                                    contenidoBuffer( B, ListaSinUltimoElemento, ResultadoRecursivo),
                                    append( ResultadoRecursivo, [P], C). 

contenidoBufferDeSerializacion(B1,Lista,C) :- append( ListaSinUltimoElemento, [escribir(B2, _)], Lista ), B2 \= B1, contenidoBuffer(B1, ListaSinUltimoElemento, C). 

%% Ejercicio 6

%% convertirALista(+ProcesoOLista,?Contenidos)
convertirALista(P,S):- proceso(P), serializar(P,S).
convertirALista(P,P):- not(proceso(P)).

%% contenidoLeido(+ProcesoOLista,?Contenidos)
contenidoLeido(POL, C) :- convertirALista(POL, Lista), contenidoLeidoConHistoria(Lista, [], C).

%% contenidoAplanadoLeido(+P,+Hist,-C)

contenidoLeidoConHistoria([], _, []).
contenidoLeidoConHistoria([computar|XS], Hist, ContenidoLeido) :-
    contenidoLeidoConHistoria(XS, Hist, ContenidoLeido).
contenidoLeidoConHistoria([escribir(A,B)|XS], Hist, ContenidoLeido) :-
    append(Hist, [escribir(A,B)], NuevaHist),
    contenidoLeidoConHistoria(XS, NuevaHist, ContenidoLeido).
contenidoLeidoConHistoria([leer(B)|XS], Hist, [C|ContenidoLeido]) :-
    contenidoBuffer(B, Hist, [C|_]),
    append(Hist, [leer(B)], NuevaHist),
    contenidoLeidoConHistoria(XS, NuevaHist, ContenidoLeido).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 7
%% esSeguro(+P)
esSeguro([]).
esSeguro(escribir(_, _)).
esSeguro(computar).
esSeguro([X|XS]) :- contenidoLeido([X|XS], _).
esSeguro(secuencia(P,Q)) :- serializar(secuencia(P,Q), PC), esSeguro(PC). 
esSeguro(paralelo(P,Q) ) :- 
    esSeguro(P), esSeguro(Q), 
    forall(serializar(paralelo(P,Q), PC), esSeguro(PC)), 
    buffersUsados(P, BP), buffersUsados(Q,BQ), intersection(BP, BQ, []).


%% Ejercicio 8
%% ejecucionSegura(?XS,+BS,+CS)
ejecucionSegura(XS, BS, CS):- generarEjecucion(XS, BS, CS), esSeguro(XS).

%% generarOperacionMinimalSegura(?X,+CS,+BS)
generarOperacionMinimalSegura(computar , _, _ ).  
generarOperacionMinimalSegura(escribir(B, C), CS, BS) :- member(B, BS), member(C,CS).  
generarOperacionMinimalSegura(leer(B), _, BS) :- member(B, BS).  

%% generarEjecucion(?XS,+BS,+CS)
generarEjecucion([], _ , _).
generarEjecucion([X| XS], BS, CS) :-generarEjecucion(XS,BS, CS), generarOperacionMinimalSegura(X, CS, BS).



%% 8.1. Analizar la reversibilidad de XS, justificando adecuadamente por qué el predicado se comporta como
%% lo hace.
%   El predicado es reversible en XS, esto es asi porque el predicado generarOperacionMinimalSegura que es el generador 
%   usa únicamente member, por lo que al ser usado de manera instanciada todos los predicados internos funcionarian correctamente.
%   Si en XS se le pasa un elemento instanciado el cual no es seguro, el predicado generarOperacionMinimalSegura devuelve true 
%   si es posible construir XS con computar, escribir, leer con los elementos de XS.
%   De manera más resumida, el predicado funciona correctamente con elementos de manera instanciada como sin instanciar. 
%   Si la instancia de XS es correcta, primero se chequea que sea posible construirlo con instrucciones de computar, leer o escribir y luego se ejecuta esSeguro.
%   Si no es una instancia de XS correcta, ya sea porque contiene elementos que no sean computar, leer o escribir no se traba sino que devuelve false. Esto es asi
%   porque generarEjecucion es recursiva analizando todos los elementos, en un momento por ejemplo se hará generarOperacionMinimalSegura(hola, CS, BS) que devolvera false.
%   Por otro lado, si la instancica de XS tiene únicamente elemetntos de computar, leer o escribir, como se usa esSeguro que toma elementos instanciados funcionara correctamente
%   y devuelve falso en caso de que no sea seguro.

%   XS con computar, escribir, leer con los elememtos de XS
%   Vamos a ver cuando XS esta instanciado y cuando no.
%   Si XS esta instanciado y es una ejecucion segura que usa
%   los contenidos y buffers (BS y CS respectivamente) entonces
%   el predicado termina de manera esperada dando true.
%   Si XS usamos una instancia que contiene lecturas a buffers
%   vacios o que no conocemos, o contiene una lectura en paralelo
%   el predicado nos da false. En otras palabras, el predicado 
%   se comporta de manera esperada en cada caso donde XS esta
%   instanciado, no se cuelga. En los dos casos estamos verificando que XS
%   es una posible ejecucion y luego lo testeamos viendo si es segura.
%
%   Si XS no esta instanciado, entonces nos da las infinitas
%   combinaciones de ejecuciones seguras que podemos conseguir
%   usando los contenidos y buffers dados. Que es exactamente
%   lo que queremos conseguir, podemos observar que no se generan
%   soluciones repetidas. 
%
%   De esta manera podemos afirmar que XS es reversible efectivamente.
%   
%
%
%
%
%
%%%%%%%%%%%
%% TESTS %%
%%%%%%%%%%%

% Se espera que completen con las subsecciones de tests que crean necesarias, más allá de las puestas en estos ejemplos
cantidadTestsBasicos(30).

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

%% ej 2
testBasico(21) :- buffersUsados(leer(1), [1]).
testBasico(22) :- buffersUsados(escribir(1, a), [1]).
testBasico(23) :- buffersUsados(secuencia(leer(1), escribir(1, a)), [1]).
testBasico(24) :- buffersUsados(paralelo(leer(1), escribir(2, b)), [1, 2]).
testBasico(25) :- buffersUsados(secuencia(secuencia(leer(1), escribir(2, a)), leer(3)), [1, 2, 3]).
testBasico(26) :- buffersUsados(paralelo(secuencia(leer(1), escribir(2, a)), escribir(3, b)), [1, 2, 3]).
testBasico(27) :- buffersUsados(paralelo(leer(1), paralelo(escribir(2, a), leer(3))), [1, 2, 3]).
testBasico(28) :- buffersUsados(secuencia(paralelo(leer(1), escribir(2, b)), paralelo(leer(3), escribir(4, c))), [1, 2, 3, 4]).
testBasico(29) :- buffersUsados(secuencia(secuencia(leer(1), escribir(2, a)), secuencia(leer(3), escribir(4, b))), [1, 2, 3, 4]).
testBasico(30) :- buffersUsados(paralelo(
    secuencia(leer(1), escribir(2, a)),
    secuencia(leer(3), paralelo(leer(4), escribir(5, b)))
), [1, 2, 3, 4, 5]).


cantidadTestsProcesos(9). % Actualizar con la cantidad de tests que entreguen

testProcesos(1) :- intercalar([1,2,3],[4,5,6],[1,2,3,4,5,6]).
testProcesos(2) :- intercalar([],[4,5,6],[4,5,6]).
testProcesos(3) :- intercalar([],[],[]).
testProcesos(4) :- intercalar([1],[4],[4,1]).
testProcesos(5) :- intercalar([[computar],[1,2],4],[3],[[computar], [1, 2], 4, 3]).
testProcesos(6) :- serializar(secuencia(computar,leer(2)),[computar,leer(2)]).
testProcesos(7) :- serializar(paralelo(paralelo(leer(1),leer(2)),secuencia(leer(3),leer(4))),[leer(2),leer(3),leer(1),leer(4)]).
testProcesos(8) :- serializar(secuencia(secuencia(leer(1),leer(2)),paralelo(computar,escribir(4,a))),[leer(1),leer(2),escribir(4,a),computar]).
testProcesos(9) :- serializar(paralelo(paralelo(leer(1),leer(2)),paralelo(paralelo(leer(3),leer(4)),paralelo(computar,escribir(1,s)))),[leer(3), leer(4), computar, escribir(1, s), leer(1), leer(2)]).

cantidadTestsBuffers(21). % Actualizar con la cantidad de tests que entreguen

%ej 5
testBuffers(1) :- contenidoBuffer(1, [escribir(1, a), escribir(1,b), leer(1)], [b]).
testBuffers(2) :- contenidoBuffer(1, [escribir(1, a), escribir(2, b), leer(2)], [a]).
testBuffers(3) :- contenidoBuffer(1, escribir(1, a), [a]).
testBuffers(4) :- contenidoBuffer(1, [escribir(1, a)], [a]).
testBuffers(5) :- contenidoBuffer(1, [escribir(1, a), escribir(1,b), escribir(2, e) ,escribir(1,c)], [a,b,c]).
testBuffers(6) :- contenidoBuffer(2, [escribir(1, a), escribir(1,b), escribir(2, e) ,escribir(1,c)], [e]).
testBuffers(7) :- contenidoBuffer(1,  secuencia(escribir(1, a), secuencia(escribir(1, b), secuencia(escribir(2, e), secuencia(escribir(1, c), escribir(3,f))  )) ), [a,b,c]).

testBuffers(8) :- contenidoBuffer(1,  secuencia(escribir(1, a), secuencia(escribir(1, b), secuencia(escribir(2, e), paralelo(escribir(1, c), escribir(3,f))  )) ), [a,b,c]).
testBuffers(9) :- contenidoBuffer(1,  secuencia(escribir(1, a), secuencia(escribir(1, b), secuencia(escribir(2, e), paralelo(escribir(1, c), escribir(1,d))  )) ), [a,b,c,d]).
testBuffers(10) :- contenidoBuffer(1,  secuencia(escribir(1, a), secuencia(escribir(1, b), secuencia(escribir(2, e), paralelo(escribir(1, c), escribir(1,d))  )) ), [a,b,d,c]).
testBuffers(11) :- contenidoBuffer(2,  secuencia(escribir(1, a), secuencia(escribir(1, b), secuencia(escribir(2, e), paralelo(escribir(1, c), escribir(1,d))  )) ), [e]).

testBuffers(12) :- contenidoBuffer(1,  [escribir(1,a), escribir(1,b), escribir(2,e),escribir(1,c), leer(1), escribir(1,d)] , [b,c,d]).
testBuffers(13) :- contenidoBuffer(2,  [escribir(1,a), escribir(1,b), escribir(2,e),escribir(1,c), leer(1), escribir(2,d)] , [e,d]).
testBuffers(14) :- contenidoBuffer(2,  [escribir(1,a), escribir(1,b), escribir(2,e),escribir(1,c), leer(2), escribir(2,d)] , [d]).
testBuffers(15) :- not(contenidoBuffer(1,[escribir(1,a),leer(1),leer(1)],_)). %% lectura incorrecta da false
testBuffers(16) :- contenidoBuffer(1, [escribir(1, a), escribir(1,b), escribir(2, e) ,escribir(1,c),leer(3),leer(3)], [a,b,c]).
%% si la lectura es incorrecta en otro buffer no me importa

%%  ej 6
testBuffers(17) :- contenidoLeido(paralelo(secuencia(escribir(2,sol),leer(2)),secuencia(escribir(1,agua),leer(1))),[sol,agua]).
testBuffers(18) :- not(contenidoLeido([escribir(1, agua), escribir(2, sol), leer(1), leer(1)],_)).
testBuffers(19) :- contenidoLeido(paralelo(secuencia(escribir(2,sol),secuencia(leer(2),escribir(2,agua))),secuencia(leer(2),computar)), [sol, agua] ).
testBuffers(20) :- contenidoLeido([escribir(1,a),escribir(1,b),leer(1)],[a]).
testBuffers(21) :- contenidoLeido([escribir(1,a),escribir(1,b),escribir(2,c),escribir(8,d),computar],[]).


cantidadTestsSeguros(21). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests

%% ej 7
testSeguros(1) :- not(esSeguro(secuencia(leer(1),escribir(1,agua)))).
testSeguros(2) :- esSeguro(secuencia(escribir(1,agua),leer(1))).
testSeguros(3) :- not(esSeguro(paralelo(escribir(1,sol),secuencia(escribir(1,agua),leer(1))))).
testSeguros(4) :- esSeguro(paralelo(escribir(2,sol),secuencia(escribir(1,agua),leer(1)))).
testSeguros(5) :- esSeguro(paralelo(escribir(1,a),escribir(2,b))).
testSeguros(6) :- esSeguro(secuencia(escribir(1,a),escribir(2,b))).
testSeguros(7) :- not(esSeguro(paralelo(escribir(2,sol),escribir(2,papa)))).
testSeguros(8) :- not(esSeguro(paralelo(leer(1,sol),escribir(2,papa)))).
testSeguros(9) :- esSeguro(secuencia(computar,secuencia(computar,paralelo(escribir(1,a),computar)))).


%% ej 8

testSeguros(10) :- ejecucionSegura([computar],[1,2],[a,b]).
testSeguros(11) :- ejecucionSegura([escribir(1,a),escribir(1,b)],[1,2],[a,b]).
testSeguros(12) :- ejecucionSegura([escribir(1,b),leer(1),escribir(2,b),leer(2)],[1,2],[a,b]).
testSeguros(13) :- ejecucionSegura([computar,computar,computar],[],[]).
testSeguros(14) :- ejecucionSegura([escribir(1, b), leer(1), escribir(1, b)],[1],[b]).
testSeguros(15) :-not(ejecucionSegura([leer(1),escribir(1,b)],[1],[b])).
testSeguros(16) :- not(ejecucionSegura([leer(1)],[1],[b])).
testSeguros(17) :- not(ejecucionSegura([hola],[1],[b])).
testSeguros(18) :- ejecucionSegura(XS, [1], [b]), XS = [escribir(1, b)].
testSeguros(19) :- ejecucionSegura(XS,[1,2],[a,b]), XS = [computar,  escribir(1,a),escribir(1,b)].
testSeguros(20) :- ejecucionSegura(XS, [1], [b,a]), XS = [computar, escribir(1,a), leer(1), escribir(1, b)].
testSeguros(21) :- ejecucionSegura(XS,[1,2],[a,b]), XS = [computar].

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
