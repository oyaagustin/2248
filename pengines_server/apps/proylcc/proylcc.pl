:- module(proylcc, 
	[  
		join/4,
        prediccion/4,
        boosterCollapser/3
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

/*Devuelve el resultado preliminar.*/
prediccion(Grid, NumOfColumns, Path, Res) :- 
    sumatoria(Grid, NumOfColumns, Path, Sum),
    pot_2(Sum, Res).
    
/**
 * Power up. Colapsa todos los grupos de bloques adyacentes
 * y de igual valor reemplazándolos por un bloque numérico
 * calculado como la menor potencia de 2 mayor o igual a sumatoria de los bloques
 * del grupo.
*/

boosterCollapser(Grid, NumOfColumns, RGrids):-
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
    

/**
 * Toma dos listas, una con posiciones y otra con valores,
 * reemplaza los valores originales de las posiciones con
 * los valores de la lista de valores.
*/
reemplazarTodos(Grid, _, [], [], Grid).

reemplazarTodos(Grid, NumOfColumns, [Pos|PosList], [Value|ValueList], FinalGrid) :-
    replace(Grid, NumOfColumns, Pos, Value, NewGrid),
    reemplazarTodos(NewGrid, NumOfColumns, PosList, ValueList, FinalGrid).

/**
 * Concatena listas.
*/
concatenar([], []).
concatenar([L|Ls], Res) :-
    concatenar(Ls, Res1),
    append(L, Res1, Res).

/**
 * Dada una lista de listas, elimina todos los últimos elementos de
 * las listas dentro de la lista.
*/
eliminarTodosUltimo([], []).
eliminarTodosUltimo([L1|Ls1], [L2|Ls2]) :-
    eliminar_ultimo(L1, L2),
    eliminarTodosUltimo(Ls1, Ls2).

/**
 * Elimina el último elemento de una lista.
*/
eliminar_ultimo([], []).
eliminar_ultimo([_], []).
eliminar_ultimo([X|Xs], [X|Ys]) :-
    eliminar_ultimo(Xs, Ys).

/**
 * Devuelve el último elemento de una lista.
*/
getUltimo([], []).
getUltimo([H|T], [X|Rest]) :- 
    reverse(H, [X|_]),
    getUltimo(T, Rest).

/**
 * Opera sobre una lista de listas, devuelve una lista que contiene
 * los resultados de la sumatoria de las listas dentro de la lista.
*/
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
 pot_2(N, R) :- R is 2**ceil(log(N)/log(2)).

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

/**
 * Genera el efecto de gravedad sobre una columna.
 * Toma una posición y la "sube" hasta sacarla de la grilla,
 * seteando un valor en el lugar más alto.  
*/
subir(Grid,[0,Y], NumOfColumns, Valor, NewGrid):-
	replace(Grid, NumOfColumns, [0,Y], Valor, NewGrid).
subir(Grid,[X,Y], NumOfColumns, Valor, NewGrid) :-
    Xminus is X-1,
	swap(Grid,[X,Y], NumOfColumns, TGrid),
	subir(TGrid, [Xminus,Y], NumOfColumns, Valor, NewGrid).

/**
 * Se encarga de separar la lista en una lista de 
 * sublistas que representan las filas :)
*/
get_filas(List, Result) :-
    findall(X, member([X,_], List), XValues),
    sort(XValues, SortedXValues),
    get_filas_aux(SortedXValues, List, Result).


get_filas_aux([], _, []).
get_filas_aux([X|XValues], List, [Sublist|Result]) :-
    findall([X,Y], member([X,Y], List), Sublist),
    get_filas_aux(XValues, List, Result).


/**
 * Sube cada elemento de la lista y setea una potencia de 
 * 2 aleatoria.
*/
subir_fila(Grid,[],_,Grid).

subir_fila(Grid, [[X,Y]|T], NumOfColumns, NewGrid):-
    random_pot(Grid, Val),
    subir(Grid, [X,Y], NumOfColumns, Val, TempGrid),
    subir_fila(TempGrid, T, NumOfColumns, NewGrid).
    
/**
 * Dada una lista de filas, se encarga de aplicar efecto 
 * de gravedad sobre cada fila.
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

/* Comprueba si dos números son adyacentes, o devuelve un número adyacente*/
adyacente([X,Y], NumOfColumns, NumOfRows, [X2,Y2]):-
    between(-1,1, V1), X2 is X+V1,
    between(-1,1, V2), Y2 is Y+V2,
     (\+ ([X2,Y2] = [X,Y])),
    validatePos([X2,Y2], NumOfColumns, NumOfRows).

/* Comprueba que una posición esté dentro del arreglo*/
validatePos([X2,Y2], NumOfColumns, NumOfRows) :-
    X2 >= 0, Y2 >= 0, 
    X2 < NumOfRows, Y2 < NumOfColumns.

/* Devuelve todos los adyacentes iguales al elemento de la posición dada*/
findAdy(Grid, [X,Y], NumOfColumns, NumOfRows, Adyacentes):-
    findall([Xa,Ya],(adyacente([X,Y], NumOfColumns, NumOfRows, [Xa,Ya]), 
                    mismoValor(Grid, NumOfColumns, [X,Y], [Xa,Ya])),Adyacentes).

/* Calcula la siguiente posición, devuelve falso si se está en una posición inválida o final*/
nextPos([X,Y], NumOfColumns, NumOfRows, [NX,NY]) :-
    NX is (X + ((Y+1) // NumOfColumns)),
    NY is (Y+1) mod NumOfColumns,
    validatePos([NX,NY], NumOfColumns, NumOfRows).

/* Calcula el número de filas de la grilla*/
numOfRows(Grid, NumOfColumns, Rows):-
    length(Grid, L), Rows is ceiling(L/NumOfColumns).

/* Encuentra todos los caminos posibles de la grilla*/
findPaths(Grid, [X,Y], NumOfColumns, NumOfRows, Visitados, CaminoActual, CaminosFinales):-
    nextPos([X,Y], NumOfColumns, NumOfRows, [Nx,Ny]), \+ member([X,Y], Visitados),
	path(Grid, [X,Y], NumOfColumns, NumOfRows, Visitados, [], CaminoIntermedio1),
    length(CaminoIntermedio1,L), L > 1,!,
    append(Visitados, CaminoIntermedio1, NuevosVisitados),
	append(CaminoActual, [CaminoIntermedio1], CaminoIntermedio2),
    findPaths(Grid, [Nx,Ny],NumOfColumns, NumOfRows, NuevosVisitados, CaminoIntermedio2, CaminosFinales).
    
findPaths(Grid, [X,Y], NumOfColumns, NumOfRows, Visitados, CaminoActual, CaminosFinales):-
    nextPos([X,Y], NumOfColumns, NumOfRows, [Nx,Ny]), \+ member([X,Y], Visitados),
	append(Visitados, [[X,Y]], NuevosVisitados),
    findPaths(Grid, [Nx,Ny],NumOfColumns, NumOfRows, NuevosVisitados, CaminoActual, CaminosFinales).

    
/* Caso base, se termina cuando la próxima posición está fuera de la grilla. */
findPaths(_, [X,Y], NumOfColumns, NumOfRows, _, CaminoActual, CaminoActual):-
    \+nextPos([X,Y], NumOfColumns, NumOfRows, _).

/**
 * Si la posición ya fue visitada, se avanza en la grilla.
*/
findPaths(Grid, [X,Y], NumOfColumns, NumOfRows, Visitados, CaminoActual, CaminosFinales):-
    nextPos([X,Y], NumOfColumns, NumOfRows, [Nx,Ny]), member([X,Y], Visitados), 
    findPaths(Grid, [Nx,Ny], NumOfColumns, NumOfRows, Visitados, CaminoActual, CaminosFinales).

/**
 * Toma una posición y se encarga de agregarla a la lista de visitados y al camino. 
 * Llama a multiPath/7 para recorrer a los adyacentes iguales de la posición.
*/
path(Grid, [X,Y], NumOfColumns, NumOfRows, Visitados, CaminoInicial, CaminoFinal):-
    \+ member([X,Y], Visitados), \+ member([X,Y], CaminoInicial), append([[X,Y]], Visitados, NuevosVisitados),
    append([[X,Y]], CaminoInicial, CaminoIntermedio1),
    findAdy(Grid, [X,Y], NumOfColumns, NumOfRows, Adyacentes),
    multiPath(Grid, Adyacentes, NumOfColumns, NumOfRows, NuevosVisitados, CaminoIntermedio1, CaminoFinal).

path(_, [X,Y], _, _, Visitados, CaminoInicial, CaminoInicial):-
    (member([X,Y], Visitados); member([X,Y], CaminoInicial)).

/**
 * Recorre una lista de posiciones y por cada posición, llama a path/7 para que lo agregue al camino recorrido
 * y evaluar sus adyacentes. 
*/
multiPath(_,[],_,_,_,Camino,Camino).

multiPath(Grid, [[X,Y]|T], NumOfColumns, NumOfRows, Visitados, CaminoInicial, CaminoFinal):-
    \+ member([X,Y], Visitados), \+ member([X,Y], CaminoInicial),
    path(Grid, [X,Y], NumOfColumns, NumOfRows, Visitados, CaminoInicial, CaminoIntermedio),
    append([[X,Y]], Visitados, NuevosVisitados),
    multiPath(Grid, T, NumOfColumns, NumOfRows, NuevosVisitados, CaminoIntermedio, CaminoFinal).

multiPath(Grid, [[X,Y]|T], NumOfColumns, NumOfRows, Visitados, CaminoInicial, CaminoFinal):-
    (member([X,Y], Visitados); member([X,Y], CaminoInicial)),
	multiPath(Grid, T, NumOfColumns, NumOfRows, Visitados, CaminoInicial, CaminoFinal).

maxSumPath(Grid, NumOfColumns, [Path|Paths], MaxParcial, MaxFinal):-
    sumatoria(Grid, NumOfColumns, Path, Sum),
    sumatoria(Grid, NumOfColumns, MaxParcial, Sum2),
    length(Path, L),
    ((Sum >= Sum2), L > 1), !, 
    maxSumPath(Grid, NumOfColumns, Paths, Path, MaxFinal).

maxSumPath(Grid, NumOfColumns, [_|Paths], MaxParcial, MaxFinal):-    
    maxSumPath(Grid, NumOfColumns, Paths, MaxParcial, MaxFinal).

maxSumPath(_, _, [], MaxParcial, MaxParcial).
 
 
maxPath(Grid, NumOfColumns, MaxPath):-
    numOfRows(Grid, NumOfColumns, NumOfRows),
    findAllPaths(Grid, [0,0], NumOfColumns, NumOfRows, [], AllPaths),
    maxSumPath(Grid, NumOfColumns, AllPaths, [], MaxPath).

/*Chequea si la posición [X1,Y1] contiene el doble del valor de [X,Y]*/
isDouble(Grid, NumOfColumns, [X,Y], [X1,Y1]):-
    getValue(Grid, [X,Y], NumOfColumns, Val1), 
    getValue(Grid, [X1,Y1], NumOfColumns, Val2),
    2*Val1 =:= Val2.

 /*Devuelve una posición conectable con [X,Y] que no haya sido conectada antes*/
findConectable(Grid, [X,Y], NumOfColumns, NumOfRows, CaminoActual, [X1,Y1]):-
    adyacente([X,Y], NumOfColumns, NumOfRows,[X1,Y1]),
    (mismoValor(Grid, NumOfColumns, [X,Y], [X1,Y1]);
    (\+ CaminoActual = [],
    last(CaminoActual, Ultimo),
     mismoValor(Grid,NumOfColumns, [X,Y], Ultimo),
    isDouble(Grid,NumOfColumns, [X,Y],[X1,Y1]))),
    \+ member([X1,Y1], CaminoActual). 

/*Encuentra todos los posibles caminos de la grilla*/
findAllPaths(_, [X,Y],NumOfColumns, NumOfRows, CaminosParciales, CaminosParciales):-
	(\+ nextPos([X,Y], NumOfColumns, NumOfRows, _)).

findAllPaths(Grid, [X,Y],NumOfColumns, NumOfRows, CaminosParciales, CaminosFinales):-
    findall(Path, linealPath(Grid, [X,Y], NumOfColumns, NumOfRows, [], Path), Paths),
    nextPos([X,Y], NumOfColumns, NumOfRows, [Xn,Yn]),
    findAllPaths(Grid, [Xn,Yn], NumOfColumns, NumOfRows, CaminosParciales, CaminosNuevos),
    append(Paths, CaminosNuevos, CaminosFinales).

/*Conecta una posición con otra recursivamente hasta formar un camino*/
linealPath(Grid, [X,Y], NumOfColumns, NumOfRows, CaminoActual, CaminoFinal):-
    append(CaminoActual, [[X,Y]], CaminoNuevo),
    findConectable(Grid, [X,Y], NumOfColumns, NumOfRows, CaminoNuevo, [X1,Y1]),
    linealPath(Grid, [X1,Y1], NumOfColumns, NumOfRows, CaminoNuevo, CaminoFinal).

/*Caso base, si no encuentra posiciones conectables devuelve el camino*/
linealPath(Grid, [X,Y], NumOfColumns, NumOfRows, CaminoActual, CaminoFinal):-
	\+ findConectable(Grid, [X,Y], NumOfColumns, NumOfRows, CaminoActual, _),
    append(CaminoActual, [[X,Y]], CaminoFinal).

