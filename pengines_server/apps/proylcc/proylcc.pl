:- module(proylcc, 
	[  
		join/4
	]).


/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 



 pot_2(N, R) :- R is ceil(log(N)/log(2)).

 getValue(Grid, [X,Y] , NumOfColumns, Val):- Celda is X*NumOfColumns+Y, nth0(Celda, Grid, Val).

 replace(Grid, NumOfColumns, [X,Y], Value, NewGrid) :-
    Index is X * NumOfColumns + Y,
    nth0(Index, Grid, _, TempGrid),
    nth0(Index, NewGrid, Value, TempGrid).