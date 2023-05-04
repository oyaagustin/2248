:- module(proylcc, 
	[  
		join/4
	]).

/**
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */
join(+Grid, +NumOfColumns, +Path, -RGrids) :-
   sumatoria(Grid, NumOfColumns,Path, Val),
   pathDLT(Grid, NumOfColumns, Path, Val, NewGrid),
   RGrids = [Grid,NewGrid].

/*Calcula 2 a la N potencia*/
 pot_2(N, R) :- R is ceil(log(N)/log(2)).

/*Devuelve una potencia de 2 aleatoria usando como techo el máximo elemento de la grilla*/
random_pot(Grid, R) :-
   max_list(Grid, M),
   random_between(1, M, N), 
   R is 2**N.

/*Devuelve el valor de la posición en la grilla*/
 getValue(Grid, [X,Y], NumOfColumns, Val):- Celda is Y*NumOfColumns+X, nth0(Celda, Grid, Val).

/*Reemplaza el elemento de la posición por un nuevo elemento*/
 replace(Grid, NumOfColumns, [X,Y], Value, NewGrid) :-
    Index is Y * NumOfColumns + X,
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
*por ejemplo, una potencia de 2 ;) 
*/

pathDLT(Grid, NumOfColumns,[[X,Y]], Val, NewGrid) :- 
   replace(Grid, NumOfColumns, [X,Y], Val, NewGrid).

pathDLT(Grid, NumOfColumns, [[X,Y]|T], Val, NewGrid) :-
  pathDLT(Grid, NumOfColumns, T, Val, TempGrid),
  replace(TempGrid, NumOfColumns, [X,Y], 0, NewGrid).  
  
  swap(Grid, [_,0],_, Grid).

swap(Grid,[X,Y], NumOfColumns, NewGrid) :-
    Yminus is Y-1,
	getValue(Grid, [X,Yminus], NumOfColumns, Val_arriba),
	getValue(Grid, [X,Y], NumOfColumns, Val_abajo),
	replace(Grid, NumOfColumns, [X,Y], Val_arriba, TGrid),
	replace(TGrid, NumOfColumns, [X,Yminus], Val_abajo, NewGrid).

subir(Grid,[X,0], NumOfColumns, Valor, NewGrid):-
	replace(Grid, NumOfColumns, [X,0], Valor, NewGrid).
subir(Grid,[X,Y], NumOfColumns, Valor, NewGrid) :-
    Yminus is Y-1,
	swap(Grid,[X,Y], NumOfColumns, TGrid),
	subir(TGrid, [X,Yminus], NumOfColumns, Valor, NewGrid).

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


