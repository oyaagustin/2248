:- module(proylcc, 
	[  
		join/4,
        prediccion/4,
        boosterColapser/3
	]).

/**
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */
join(Grid, NumOfColumns, Path, RGrids) :-
    sumatoria(Grid, NumOfColumns, Path, Sum),
    pot_2(Sum, Val),
    pathDLT(Grid, NumOfColumns, Path, Val, NewGrid),
    eliminar_ultimo(Path, PathDLT),
    get_filas(PathDLT, Filas),
    gravedad(NewGrid,Filas,NumOfColumns, RGrids).

/**Devuelve el resultado preliminar.
*/
prediccion(Grid, NumOfColumns, Path, Res) :- 
    sumatoria(Grid, NumOfColumns, Path, Sum),
    pot_2(Sum, Res).
    
/**Power up. Colapsa todos los grupos de bloques adyacentes
* y de igual valor reemplazándolos por un bloque numérico
* calculado como la menor potencia de 2 mayor o igual a sumatoria de los bloques
* del grupo.
*/

boosterColapser(Grid, NumOfColumns, RGrids):-
    numOfRows(Grid, NumOfColumns, NumOfRows),
    findPaths(Grid, [0,0], NumOfColumns, NumOfRows, [], [], CaminosFinales),
    subListSort(CaminosFinales, CaminosOrdenados),
	getListaSumatoria(Grid, NumOfColumns, CaminosOrdenados, ListaSum),
    getUltimo(CaminosOrdenados, ListaUltimos),
	reemplazarTodos(Grid, NumOfColumns, ListaUltimos, ListaSum, GrillaIntermedia),
	eliminarTodosUltimo(CaminosOrdenados, ListaPosicionesVacias),
	concatenar(ListaPosicionesVacias, PosicionesVacias),
    pathDLT(GrillaIntermedia, NumOfColumns, PosicionesVacias, 0, GrillaIntermedia2),
    get_filas(PosicionesVacias, Filas),
    gravedad(GrillaIntermedia2,Filas,NumOfColumns, RGrids).
    
reemplazarTodos(Grid, _, [], [], Grid).
reemplazarTodos(Grid, NumOfColumns, [Pos|PosList], [Value|ValueList], FinalGrid) :-
    replace(Grid, NumOfColumns, Pos, Value, NewGrid),
    reemplazarTodos(NewGrid, NumOfColumns, PosList, ValueList, FinalGrid).

concatenar([], []).
concatenar([L|Ls], Res) :-
    concatenar(Ls, Res1),
    append(L, Res1, Res).

eliminarTodosUltimo([], []).
eliminarTodosUltimo([L1|Ls1], [L2|Ls2]) :-
    eliminar_ultimo(L1, L2),
    eliminarTodosUltimo(Ls1, Ls2).

eliminar_ultimo([], []).
eliminar_ultimo([_], []).
eliminar_ultimo([X|Xs], [X|Ys]) :-
    eliminar_ultimo(Xs, Ys).

getUltimo([], []).
getUltimo([H|T], [X|Rest]) :- 
    reverse(H, [X|_]),
    getUltimo(T, Rest).

getListaSumatoria(_, _, [], []).
getListaSumatoria(Grid, NumOfColumns, [L|Ls], [Sum|Sums]) :-
    sumatoria(Grid, NumOfColumns, L, TSum),
    pot_2(TSum, Sum),
    getListaSumatoria(Grid, NumOfColumns, Ls, Sums).

/*Para cada sublista dentro de una lista, elimina la última posición*/
eliminarUltimo([], []).
eliminarUltimo([L|Sl], [NL|NSubList]):-
    append(NL,[_], L),
    eliminarUltimo([Sl], NSubList).
    
/*Ordena cada sublista dentro de una lista de sublistas*/
subListSort([], []).
subListSort([L|S], [SL|SortLists]) :-
    sort(L, SL),
    subListSort(S, SortLists).
    

/*Calcula 2 a la N potencia*/
 pot_2(N, R) :- R is 2**floor(log(N)/log(2)).

/*Devuelve una potencia de 2 aleatoria usando como techo el máximo elemento de la grilla*/
random_pot(Grid, R) :-  
	max_list(Grid, Max),
	N is floor(log(Max)/log(2))-1,
	random_between(1, N, Exp),
	R is 2**Exp.

/**Devuelve el valor de la posición en la grilla
*/
 getValue(Grid, [X,Y], NumOfColumns, Val):- Celda is X*NumOfColumns+Y, nth0(Celda, Grid, Val).

/**Reemplaza el elemento de la posición por un nuevo elemento
*/
 replace(Grid, NumOfColumns, [X,Y], Value, NewGrid) :-
    Index is X* NumOfColumns + Y,
    nth0(Index, Grid, _, TempGrid),
    nth0(Index, NewGrid, Value, TempGrid).

/*Devuelve la sumatoria de los valores de las posiciones en la grilla*/
 sumatoria(_, _, [], 0).
 sumatoria(Grid, NumOfColumns, [[X,Y]|T], Res) :- 
   sumatoria(Grid, NumOfColumns, T, ResAux),
   getValue(Grid, [X,Y], NumOfColumns, Val),
   Res is ResAux+Val.

/** Se encarga de settear en 0 las posiciones de la grilla. 
*Excepto a la última posición, que es cambiada por el valor que se pase en Val
*por ejemplo, una potencia de 2 o un 0 ;) 
*/

pathDLT(Grid, NumOfColumns,[[X,Y]], Val, NewGrid) :- 
    replace(Grid, NumOfColumns, [X,Y], Val, NewGrid).
 
 pathDLT(Grid, NumOfColumns, [[X,Y]|T], Val, NewGrid) :-
   pathDLT(Grid, NumOfColumns, T, Val, TempGrid),
   replace(TempGrid, NumOfColumns, [X,Y], 0, NewGrid).  

/**Intercambia el elemento de la posición dada
* con el elemento de la posición de arriba.
* En caso de estár en la posición más alta, devu
*/
swap(Grid, [0,_],_, Grid).

swap(Grid,[X,Y], NumOfColumns, NewGrid) :-
    Xminus is X-1,
	getValue(Grid, [Xminus,Y], NumOfColumns, Val_arriba),
	getValue(Grid, [X,Y], NumOfColumns, Val_abajo),
	replace(Grid, NumOfColumns, [X,Y], Val_arriba, TGrid),
	replace(TGrid, NumOfColumns, [Xminus,Y], Val_abajo, NewGrid).

/**Se encarga de generar el efecto de gravedad sobre una columna.
* Toma una posición y la "sube" hasta sacarla de la grilla,
* seteando un valor en el lugar más alto.  
*/
subir(Grid,[0,Y], NumOfColumns, Valor, NewGrid):-
	replace(Grid, NumOfColumns, [0,Y], Valor, NewGrid).
subir(Grid,[X,Y], NumOfColumns, Valor, NewGrid) :-
    Xminus is X-1,
	swap(Grid,[X,Y], NumOfColumns, TGrid),
	subir(TGrid, [Xminus,Y], NumOfColumns, Valor, NewGrid).

/*Se encarga de separar la lista en una lista de 
 * sublistas que representanlas filas :)
*/
get_filas(List, Result) :-
    findall(X, member([X,_], List), XValues),
    sort(XValues, SortedXValues),
    get_filas_aux(SortedXValues, List, Result).

get_filas_aux([], _, []).
get_filas_aux([X|XValues], List, [Sublist|Result]) :-
    findall([X,Y], member([X,Y], List), Sublist),
    get_filas_aux(XValues, List, Result).


/**Sube cada elemento de la lista y setea una potencia de 
 * 2 aleatoria.
*/
subir_fila(Grid,[],_,Grid).

subir_fila(Grid, [[X,Y]|T], NumOfColumns, NewGrid):-
    random_pot(Grid, Val),
    subir(Grid, [X,Y], NumOfColumns, Val, TempGrid),
    subir_fila(TempGrid, T, NumOfColumns, NewGrid).
    
/**
 * 
*/
gravedad(Grid, [], _, [Grid]).
gravedad(Grid, [Fila|Filas], NumOfColumns, [Grid|RGrids]) :-
    subir_fila(Grid, Fila, NumOfColumns, TGrid),
    gravedad(TGrid, Filas, NumOfColumns, RGrids).


/* Predicado para verificar si dos celdas tienen el mismo valor*/
mismoValor(Grid, NumOfColumns, [X1,Y1], [X2,Y2]) :-
    Pos1 is floor(X1 * NumOfColumns + Y1),
    Pos2 is floor(X2 * NumOfColumns + Y2),
    nth0(Pos1, Grid, Value),
    nth0(Pos2, Grid, Value).

/*Comprueba si dos números son adyacentes, o devuelve un número adyacente*/
adyacente([X,Y], NumOfColumns, NumOfRows, [X2,Y2]):-
    between(-1,1, V1), X2 is X+V1,
    between(-1,1, V2), Y2 is Y+V2,
     (\+ ([X2,Y2] = [X,Y])),
    validatePos([X2,Y2], NumOfColumns, NumOfRows).

/*Comprueba que una posición esté dentro del arreglo*/
validatePos([X2,Y2], NumOfColumns, NumOfRows) :-
    X2 >= 0, Y2 >= 0, 
    X2 < NumOfRows, Y2 < NumOfColumns.

/*Devuelve todos los adyacentes iguales al elemento de la posición dada*/
findAdy(Grid, [X,Y], NumOfColumns, NumOfRows, Adyacentes):-
    findall([Xa,Ya],(adyacente([X,Y], NumOfColumns, NumOfRows, [Xa,Ya]), 
                    mismoValor(Grid, NumOfColumns, [X,Y], [Xa,Ya])),Adyacentes).

/*Calcula la siguiente posición, devuelve falso si se está en una posición inválida o final*/
nextPos([X,Y], NumOfColumns, NumOfRows, [NX,NY]) :-
    NX is (X + ((Y+1) // NumOfColumns)),
    NY is (Y+1) mod NumOfColumns,
    validatePos([NX,NY], NumOfColumns, NumOfRows).

/*Calcula el número de filas de la grilla*/
numOfRows(Grid, NumOfColumns, Rows):-
    length(Grid, L), Rows is ceiling(L/NumOfColumns).

/**Encuentra todos los caminos posibles de la grilla.
*/
findPaths(Grid, [X,Y], NumOfColumns, NumOfRows, Visitados, CaminoActual, CaminosFinales):-
    nextPos([X,Y], NumOfColumns, NumOfRows, [Nx,Ny]), \+ member([X,Y], Visitados),
	path(Grid, [X,Y], NumOfColumns, NumOfRows, Visitados, [], CaminoIntermedio1),
    append(Visitados, CaminoIntermedio1, NuevosVisitados), 
	append(CaminoActual, [CaminoIntermedio1], CaminoIntermedio2),
    findPaths(Grid, [Nx,Ny],NumOfColumns, NumOfRows, NuevosVisitados, CaminoIntermedio2, CaminosFinales).
    
/**Caso base, se termina cuando la próxima posición está fuera de la grilla.
*/
findPaths(_, [X,Y], NumOfColumns, NumOfRows, _, CaminoActual, CaminoActual):-
    \+nextPos([X,Y], NumOfColumns, NumOfRows, _).

/**En caso
 * 
*/
findPaths(Grid, [X,Y], NumOfColumns, NumOfRows, Visitados, CaminoActual, CaminosFinales):-
    nextPos([X,Y], NumOfColumns, NumOfRows, [Nx,Ny]), member([X,Y], Visitados), 
    findPaths(Grid, [Nx,Ny], NumOfColumns, NumOfRows, Visitados, CaminoActual, CaminosFinales).


path(Grid, [X,Y], NumOfColumns, NumOfRows, Visitados, CaminoInicial, CaminoFinal):-
    \+ member([X,Y], Visitados), \+ member([X,Y], CaminoInicial), append([[X,Y]], Visitados, NuevosVisitados),
    append([[X,Y]], CaminoInicial, CaminoIntermedio1),
    findAdy(Grid, [X,Y], NumOfColumns, NumOfRows, Adyacentes),
    path_multi(Grid, Adyacentes, NumOfColumns, NumOfRows, NuevosVisitados, CaminoIntermedio1, CaminoFinal).

path(_, [X,Y], _, _, Visitados, CaminoInicial, CaminoInicial):-
    (member([X,Y], Visitados); member([X,Y], CaminoInicial)).


path_multi(_,[],_,_,_,Camino,Camino).

path_multi(Grid, [[X,Y]|T], NumOfColumns, NumOfRows, Visitados, CaminoInicial, CaminoFinal):-
    \+ member([X,Y], Visitados), \+ member([X,Y], CaminoInicial),
    path(Grid, [X,Y], NumOfColumns, NumOfRows, Visitados, CaminoInicial, CaminoIntermedio),
    append([[X,Y]], Visitados, NuevosVisitados),
    path_multi(Grid, T, NumOfColumns, NumOfRows, NuevosVisitados, CaminoIntermedio, CaminoFinal).

path_multi(Grid, [[X,Y]|T], NumOfColumns, NumOfRows, Visitados, CaminoInicial, CaminoFinal):-
    (member([X,Y], Visitados); member([X,Y], CaminoInicial)),
	path_multi(Grid, T, NumOfColumns, NumOfRows, Visitados, CaminoInicial, CaminoFinal).