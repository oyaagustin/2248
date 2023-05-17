:- module(init, [ init/2 ]).

/**
 * init(-Grid, -NumOfColumns).
 * 
 * Predicado especificando la grilla inicial, que será mostrada al comienzo del juego, donde
 * Grid es una lista con los números que conforman la grilla, y NumOfColumns es la cantidad de columnas, 
 * determinando las dimensiones de la misma.
 */

 init([
	64,4,128,32,16,
	2,8,16,256,64,
	4,64,32,1024,2,
	2,128,256,64,32,
	16,4,16,1024,16,
	2,64,2,256,128,
	128,4,1024,32,2,
	32,2,256,16,4
], 5).