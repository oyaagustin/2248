:- module(proylcc, 
	[  
		join/4,
        prediccion/4,
        boosterCollapser/3,
        maxPath/3,
        findMaxAdy/3
	]).

/**
  * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
  * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */
join(Grid, NumOfColumns, Path, RGrids) :-
    sumatoria(Grid, NumOfColumns, Path, Sum),
    pot2(Sum, Val),
    pathDLT(Grid, NumOfColumns, Path, Val, NewGrid),
    eliminarUltimo(Path, PathDLT),
    getFilas(PathDLT, Filas),
    gravedad(NewGrid,Filas,NumOfColumns, RGrids).


/**
 * Devuelve el resultado preliminar.
*/
prediccion(Grid, NumOfColumns, Path, Res) :- 
    sumatoria(Grid, NumOfColumns, Path, Sum),
    pot2(Sum, Res).
    
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
    getFilas(PosicionesVacias, Filas),
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
    eliminarUltimo(L1, L2),
    eliminarTodosUltimo(Ls1, Ls2).

/**
 * Elimina el último elemento de una lista.
*/
eliminarUltimo([], []).
eliminarUltimo([_], []).
eliminarUltimo([X|Xs], [X|Ys]) :-
    eliminarUltimo(Xs, Ys).

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
    pot2(TSum, Sum),
    getListaSumatoria(Grid, NumOfColumns, Ls, Sums).

    
/**
 * Ordena cada sublista dentro de una lista de sublistas
*/
subListSort([], []).
subListSort([L|S], [SL|SortLists]) :-
    sort(L, SL),
    subListSort(S, SortLists).
    

/**
 *Calcula la menor potencia de 2, mayor o igual al número N
*/
 pot2(N, R) :- R is floor(2**ceil(log(N)/log(2))).

/**
 *Devuelve una potencia de 2 aleatoria usando como techo el máximo elemento de la grilla
*/
randomPot(Grid, R) :-  
	max_list(Grid, Max),
	N is floor(log(Max)/log(2))-1,
	random_between(1, N, Exp),
	R is floor(2**Exp).

/**
 * Devuelve el valor de la posición en la grilla
*/
 getValue(Grid, [X,Y], NumOfColumns, Val):- Celda is X*NumOfColumns+Y, nth0(Celda, Grid, Val).

/**Reemplaza el elemento de la posición por un nuevo elemento
*/
 replace(Grid, NumOfColumns, [X,Y], Value, NewGrid) :-
    Index is floor(X* NumOfColumns + Y),
    nth0(Index, Grid, _, TempGrid),
    nth0(Index, NewGrid, Value, TempGrid).

/**
 * Devuelve la sumatoria de los valores de las posiciones en la grilla
*/
 sumatoria(_, _, [], 0).
 sumatoria(Grid, NumOfColumns, [[X,Y]|T], Res) :- 
   sumatoria(Grid, NumOfColumns, T, ResAux),
   getValue(Grid, [X,Y], NumOfColumns, Val),
   Res is floor(ResAux+Val).

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
    Xminus is floor(X-1),
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
    Xminus is floor(X-1),
	swap(Grid,[X,Y], NumOfColumns, TGrid),
	subir(TGrid, [Xminus,Y], NumOfColumns, Valor, NewGrid).

/**
 * Se encarga de separar la lista en una lista de 
 * sublistas que representan las filas :)
*/
getFilas(List, Result) :-
    findall(X, member([X,_], List), XValues),
    sort(XValues, SortedXValues),
    getFilasAux(SortedXValues, List, Result).


getFilasAux([], _, []).
getFilasAux([X|XValues], List, [Sublist|Result]) :-
    findall([X,Y], member([X,Y], List), Sublist),
    getFilasAux(XValues, List, Result).


/**
 * Sube cada elemento de la lista y setea una potencia de 
 * 2 aleatoria.
*/
subirFila(Grid,[],_,Grid).

subirFila(Grid, [[X,Y]|T], NumOfColumns, NewGrid):-
    randomPot(Grid, Val),
    subir(Grid, [X,Y], NumOfColumns, Val, TempGrid),
    subirFila(TempGrid, T, NumOfColumns, NewGrid).
    
/**
 * Dada una lista de filas, se encarga de aplicar efecto 
 * de gravedad sobre cada fila.
*/
gravedad(Grid, [], _, [Grid]).
gravedad(Grid, [Fila|Filas], NumOfColumns, [Grid|RGrids]) :-
    subirFila(Grid, Fila, NumOfColumns, TGrid),
    gravedad(TGrid, Filas, NumOfColumns, RGrids).


/** 
 * Predicado para verificar si dos celdas tienen el mismo valor
*/
mismoValor(Grid, NumOfColumns, [X1,Y1], [X2,Y2]) :-
    Pos1 is floor(X1 * NumOfColumns + Y1),
    Pos2 is floor(X2 * NumOfColumns + Y2),
    nth0(Pos1, Grid, Value),
    nth0(Pos2, Grid, Value).

/**
 * Comprueba si dos números son adyacentes, o devuelve un número adyacente
*/
adyacente([X,Y], NumOfColumns, NumOfRows, [X2,Y2]):-
    between(-1,1, V1), X2 is X+V1,
    between(-1,1, V2), Y2 is Y+V2,
     (\+ ([X2,Y2] = [X,Y])),
    validatePos([X2,Y2], NumOfColumns, NumOfRows).

/** 
 * Comprueba que una posición esté dentro del arreglo
*/
validatePos([X2,Y2], NumOfColumns, NumOfRows) :-
    X2 >= 0, Y2 >= 0, 
    X2 < NumOfRows, Y2 < NumOfColumns.

/** 
 * Devuelve todos los adyacentes iguales al elemento de la posición dada
*/
findAdy(Grid, [X,Y], NumOfColumns, NumOfRows, Adyacentes):-
    findall([Xa,Ya],(adyacente([X,Y], NumOfColumns, NumOfRows, [Xa,Ya]), 
                    mismoValor(Grid, NumOfColumns, [X,Y], [Xa,Ya])),Adyacentes).

/** 
 * Calcula la siguiente posición, devuelve falso si se está en una posición inválida o final
*/
nextPos([X,Y], NumOfColumns, NumOfRows, [NX,NY]) :-
    NX is (X + ((Y+1) // NumOfColumns)),
    NY is (Y+1) mod NumOfColumns,
    validatePos([NX,NY], NumOfColumns, NumOfRows).

/** 
 * Calcula el número de filas de la grilla
*/
numOfRows(Grid, NumOfColumns, Rows):-
    length(Grid, L), Rows is ceiling(L/NumOfColumns).

/** 
 * Encuentra todos los caminos posibles de la grilla
*/
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

    
/** 
 * Caso base, se termina cuando la próxima posición está fuera de la grilla. 
*/
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

/**
 * Si mi posición ya fue visitada entonces dejo de recorrer el camino. 
*/
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

/**
 * Si la posición ya fue visitada entonces se avanza en la lista 
 * y se llama recursivamente con la siguiente posición.
*/
multiPath(Grid, [[X,Y]|T], NumOfColumns, NumOfRows, Visitados, CaminoInicial, CaminoFinal):-
    (member([X,Y], Visitados); member([X,Y], CaminoInicial)),
	multiPath(Grid, T, NumOfColumns, NumOfRows, Visitados, CaminoInicial, CaminoFinal).


/************************************************************************************************************/
/****************************A partir de acá el código corresponde al proyecto 2****************************/
/**********************************************************************************************************/

maxSumPath(Grid, NumOfColumns, [[Sum|Path]|Paths], [Sum2|_], MaxFinal):-
    (Sum >= Sum2), !, 
    maxSumPath(Grid, NumOfColumns, Paths, [Sum|Path], MaxFinal).

maxSumPath(Grid, NumOfColumns, [_|Paths], MaxParcial, MaxFinal):-    
    maxSumPath(Grid, NumOfColumns, Paths, MaxParcial, MaxFinal).

maxSumPath(_, _, [], MaxParcial, MaxParcial).
 
 
maxPath(Grid, NumOfColumns, MaxPath):-
    numOfRows(Grid, NumOfColumns, NumOfRows),
    findAllPaths(Grid, [0,0], NumOfColumns, NumOfRows, [], AllPaths),
    maxSumPath(Grid, NumOfColumns, AllPaths, [0], [_|MaxPath]).

/*Chequea si la posición [X1,Y1] contiene el doble del valor de [X,Y]*/
isDouble(Grid, NumOfColumns, [X,Y], [X1,Y1]):-
    getValue(Grid, [X,Y], NumOfColumns, Val1), 
    getValue(Grid, [X1,Y1], NumOfColumns, Val2),
    2*Val1 =:= Val2.

/*Devuelve una posición adyacente a [X,Y] con el mismo valor*/
findIgual(Grid, [X,Y], NumOfColumns, NumOfRows, [X1,Y1]):-
    adyacente([X,Y], NumOfColumns, NumOfRows, [X1,Y1]), 
    mismoValor(Grid, NumOfColumns, [X,Y], [X1,Y1]).

/*Devuelve una posición conectable con [X,Y] que no haya sido conectada antes*/
findConectable(Grid, [X,Y], NumOfColumns, NumOfRows, CaminoActual, [X1,Y1]):-
    adyacente([X,Y], NumOfColumns, NumOfRows,[X1,Y1]),
    (mismoValor(Grid, NumOfColumns, [X,Y], [X1,Y1]);
    (isDouble(Grid, NumOfColumns, [X,Y], [X1,Y1]))),
    \+ member([X1,Y1], CaminoActual). 

/*Encuentra todos los posibles caminos de la grilla*/
findAllPaths(_, [X,Y],NumOfColumns, NumOfRows, CaminosParciales, CaminosParciales):-
	(\+ nextPos([X,Y], NumOfColumns, NumOfRows, _)).

findAllPaths(Grid, [X,Y],NumOfColumns, NumOfRows, CaminosParciales, CaminosFinales):-
    findall(Path, (linealPath(Grid, [X,Y], NumOfColumns, NumOfRows, [], 0, Path), 
    length(Path, L), L > 2), Paths),
    nextPos([X,Y], NumOfColumns, NumOfRows, [Xn,Yn]),
    findAllPaths(Grid, [Xn,Yn], NumOfColumns, NumOfRows, CaminosParciales, CaminosNuevos),
    append(Paths, CaminosNuevos, CaminosFinales).

/*Primer caso, se cpmecta con una posición adyacente igual a la actual*/
linealPath(Grid, [X,Y], NumOfColumns, NumOfRows, [], Sum, CaminoFinal):-
    getValue(Grid, [X,Y], NumOfColumns, Val),
    Sum2 is (Sum + Val),
    findIgual(Grid, [X,Y], NumOfColumns, NumOfRows, [X1,Y1]),
    linealPath(Grid, [X1,Y1], NumOfColumns, NumOfRows, [[X,Y]], Sum2, CaminoFinal).

/*Conecta una posición con otra recursivamente hasta formar un camino*/
linealPath(Grid, [X,Y], NumOfColumns, NumOfRows, CaminoActual, Sum, CaminoFinal):-
    append(CaminoActual, [[X,Y]], CaminoNuevo),
	length(CaminoNuevo, L), L>1,
    getValue(Grid, [X,Y], NumOfColumns, Val),
    Sum2 is (Sum + Val),
    findConectable(Grid, [X,Y], NumOfColumns, NumOfRows, CaminoNuevo, [X1,Y1]),
    linealPath(Grid, [X1,Y1], NumOfColumns, NumOfRows, CaminoNuevo, Sum2, CaminoFinal).

/*Caso base, si no encuentra posiciones conectables devuelve el camino*/
linealPath(Grid, [X,Y], NumOfColumns, NumOfRows, CaminoActual, Sum, [Sum2|CaminoParcial]):-
	\+ findConectable(Grid, [X,Y], NumOfColumns, NumOfRows, CaminoActual, _),
    getValue(Grid, [X,Y], NumOfColumns, Val),
    Sum2 is (Sum + Val),
    append(CaminoActual, [[X,Y]], CaminoParcial).

    findMaxAdy(Grid, NumOfColumns, BestPath):- 
        numOfRows(Grid, NumOfColumns, Rows),
        findAllPossiblePaths(Grid, [0,0], NumOfColumns, Rows, [], Paths),
        findAllElements(Grid, Elements),
        checkForPath(Grid, NumOfColumns, Rows,Paths, Elements, BestPath),
        BestPath \= [].
    
    
    /*Encuentra todos los posibles caminos de la grilla*/
    findAllPossiblePaths(_, [X,Y],NumOfColumns, NumOfRows, CaminosParciales, CaminosParciales):-
        (\+ nextPos([X,Y], NumOfColumns, NumOfRows, _)).
    
    findAllPossiblePaths(Grid, [X,Y], NumOfColumns, NumOfRows, CaminosParciales, CaminosFinales):-
        findall(Paths, posPath(Grid, [X,Y], NumOfColumns, NumOfRows, [], [], 0, Paths), CaminosParciales1),
        concatenar(CaminosParciales1, CaminosConcatenados),
        nextPos([X,Y], NumOfColumns, NumOfRows, [Xn,Yn]),
        findAllPossiblePaths(Grid, [Xn,Yn], NumOfColumns, NumOfRows, CaminosParciales, CaminosNuevos),
        append(CaminosConcatenados, CaminosNuevos, CaminosFinales).
    
    
    /*Primer caso, necesito una posición adyacente igual a la actual*/
    posPath(Grid, [X,Y], NumOfColumns, NumOfRows, [], Caminos, Sum, CaminoFinal):-
        getValue(Grid, [X,Y], NumOfColumns, Val),
        Sum2 is (Sum + Val),
        findIgual(Grid, [X,Y], NumOfColumns, NumOfRows, [X1,Y1]),
        posPath(Grid, [X1,Y1], NumOfColumns, NumOfRows, [[X,Y]], Caminos, Sum2, CaminoFinal).
    
    
    /* Caso base, si no encuentra posiciones conectables devuelve el camino */
    posPath(Grid, [X,Y], NumOfColumns, NumOfRows, CaminoActual, Caminos, Sum, CaminoFinal) :-
        \+ findConectable(Grid, [X,Y], NumOfColumns, NumOfRows, CaminoActual, _),
        length(CaminoActual, Len),
        Len > 1,  % Verificar que el camino tenga más de una coordenada
        getValue(Grid, [X,Y], NumOfColumns, Val), 
        Sum2 is (Sum + Val),
        append(CaminoActual, [[X,Y]], CaminoParcial),
        CaminoFinal = [[Sum2|CaminoParcial]|Caminos].
    
    /*Conecta una posición con otra recursivamente hasta formar un camino*/
    posPath(Grid, [X,Y], NumOfColumns, NumOfRows, CaminoActual, Caminos, Sum, CaminoFinal):-
        findConectable(Grid, [X,Y], NumOfColumns, NumOfRows, CaminoActual, [X1,Y1]),
        append(CaminoActual, [[X,Y]], CaminoNuevo),
        getValue(Grid, [X,Y], NumOfColumns, Val),
        Sum2 is (Sum + Val),
        CaminosNuevos = [[Sum2|CaminoNuevo]|Caminos],
        posPath(Grid, [X1,Y1], NumOfColumns, NumOfRows, CaminoNuevo,  CaminosNuevos,Sum2, CaminoFinal).
    
    
    checkForPath(_,_,_,_,[],[]).
    
    checkForPath(Grid, NumOfColumns, NumOfRows, Paths, [Elem|_], BestPath):-
        findPositions(Grid, NumOfColumns, Elem, Positions),
        checkForPath2(Grid, NumOfColumns, NumOfRows, Paths, Elem, Positions, BestPath).
    
    checkForPath(Grid, NumOfColumns, NumOfRows, Paths, [_|Elements], BestPath):-
        checkForPath(Grid, NumOfColumns, NumOfRows, Paths, Elements, BestPath).
    
    checkForPath2(_,_,_,_,_,[],[]).
    
    checkForPath2(Grid, NumOfColumns, NumOfRows, Paths, Elem, [Pos|_], BestPath):-
        maxAdyCheck(Grid, NumOfColumns, NumOfRows, Paths, Elem, Pos, BestPath).
    
    checkForPath2(Grid, NumOfColumns, NumOfRows, Paths, Elem, [_|Positions], BestPath):-
        checkForPath2(Grid, NumOfColumns, NumOfRows, Paths, Elem, Positions, BestPath).
    
    
    maxAdyCheck(Grid, NumOfColumns, NumOfRows, [[H|T]|_], Elem, Pos, T):-
        pot2(H, Pot), Pot =:= Elem,
        length(T,L), L>1,
        join(Grid, NumOfColumns, T, RGrids),
        last(RGrids, Grid2), 
        last(T, LPos),
        findIgual(Grid2, Pos, NumOfColumns, NumOfRows, LPos).
    
    maxAdyCheck(Grid, NumOfColumns, NumOfRows, [_|Paths], Elem, Pos, FinalPath):-
        maxAdyCheck(Grid, NumOfColumns, NumOfRows, Paths, Elem, Pos, FinalPath).
    
    
    findAllElements(List, Elements):-
        findAllElementsAux(List, [], TElements1),
        sort(TElements1, TElements2),
        reverse(TElements2, Elements).
    
    findAllElementsAux([],Elems,Elems).
    
    findAllElementsAux([H|T], Elems, ElemFinales):-
        (\+member(H, Elems)),
        append([H], Elems, ElemParciales),
        findAllElementsAux(T, ElemParciales, ElemFinales).
    
    findAllElementsAux([_|T], Elems, ElemFinales):-
        findAllElementsAux(T, Elems, ElemFinales).
    
    indexToPos(Index, NumOfColumns, [X,Y]):-
        X is Index div NumOfColumns,
        Y is Index mod NumOfColumns.
    
    findPositions(Grid, NumOfColumns, Elem, Positions):-
        findPositionsAux(Grid, NumOfColumns, 0, Elem, [], Positions).
    
    findPositionsAux([], _, _, _, Positions, Positions).
    
    findPositionsAux([H|T], NumOfColumns, Index, Elem, Positions, FinalPositions):-
        H=:=Elem, !, indexToPos(Index, NumOfColumns, Pos),
        append([Pos], Positions, TPositions),
        NIndex is Index+1,
        findPositionsAux(T, NumOfColumns, NIndex, Elem, TPositions, FinalPositions).
       
    findPositionsAux([_|T], NumOfColumns, Index, Elem, Positions, FinalPositions):-
        NIndex is Index+1, 
        findPositionsAux(T, NumOfColumns, NIndex, Elem, Positions, FinalPositions).
    
    
    
    