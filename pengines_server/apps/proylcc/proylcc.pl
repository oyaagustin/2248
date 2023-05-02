:- module(proylcc, 
	[  
		join/4
	]).


/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 


/*Calcula 2 a la N potencia*/
 pot_2(N, R) :- R is ceil(log(N)/log(2)).

/*Devuelve una potencia aleatoria de 2*/
 random_pot(R) :- random_between(1,5, N), R is 2**N.

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



