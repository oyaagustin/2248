import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import { joinResult, numberToColor } from './util';

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [numOfColumns, setNumOfColumns] = useState(null);
  const [score, setScore] = useState(0);
  const [preview, setPreview] = useState(0);
  const [path, setPath] = useState([]);
  const [waiting, setWaiting] = useState(false);

  useEffect(() => {
    // This is executed just once, after the first render.
    PengineClient.init(onServerReady);
  }, []);


  /**
   * Called when the server was successfully initialized
   */
  function onServerReady(instance) {
    pengine = instance;
    const queryS = 'init(Grid, NumOfColumns)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setNumOfColumns(response['NumOfColumns']);
      }
    });
  }

  /**
   * Called while the user is drawing a path in the grid, each time the path changes.
   */
  function onPathChange(newPath) {
    // No effect if waiting.
    if (waiting) {
      return;
    }
    setPath(newPath);
    calcularPrediccion(newPath);
  }

  /**
   */
  function calcularPrediccion(newPath) {
    if(newPath.length > 1 ){
      const gridS = JSON.stringify(grid);
      const pathS = JSON.stringify(newPath);
      const queryS = "prediccion(" + gridS + "," + numOfColumns + "," + pathS + ",Res)";
      pengine.query(queryS, (success, response) => {
        if (success) {
          const res = response['Res'];
          setPreview(res);
        }
      });
    }
    else {
      setPreview(0);
    }
  }


  /**
   * Called when the user finished drawing a path in the grid.
   */
  function onPathDone() {
    const gridS = JSON.stringify(grid);
    const pathS = JSON.stringify(path);
    const queryS = "join(" + gridS + "," + numOfColumns + "," + pathS + ", RGrids)";
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setScore(score + joinResult(path, grid, numOfColumns));
        setPath([]);
        setPreview(0);
        animateEffect(response['RGrids']);
      } else {
        setWaiting(false);
      }
    });
  }

  /**
   * Displays each grid of the sequence as the current grid in 1sec intervals.
   * @param {number[][]} rGrids a sequence of grids.
   */
  function animateEffect(rGrids) {
    setGrid(rGrids[0]);
    const restRGrids = rGrids.slice(1);
    if (restRGrids.length > 0) {
      setTimeout(() => {
        animateEffect(restRGrids);
      }, 100);
    } else {
      setWaiting(false);
    }
  }

  function onClickBooster() {     
    if(!waiting && path.length === 0){
      const gridS = JSON.stringify(grid);
      const queryS = "boosterCollapser(" + gridS + "," + numOfColumns + ", RGrids)";    
      pengine.query(queryS, (success, response) => {        
      if (success) {                    
        setWaiting(true);
        animateEffect(response['RGrids']);              
      } else {
        setWaiting(false);
      }        
    }); 
    setWaiting(false);
    }
  }

  function onClickBestPath(){
    if(!waiting && path.length === 0){
      const gridS = JSON.stringify(grid);
      const queryS = "maxPath("+gridS+","+numOfColumns+", MaxPath)";
      pengine.query(queryS, (success, response) =>{
        if(success){
          setPath(response['MaxPath']);
          calcularPrediccion(response['MaxPath']);
      }
  });
  }
}

  if (grid === null) {
    return null;
  }
  return (
    <div className="game">
      <div className="header">
        <div className="score"
        style={preview === 0? {display: "block"} : {display: "none"}}>
          {score}
        </div>
        <div className="preview"
        style={preview === 0? null: {backgroundColor: numberToColor(preview), display: "block"}}>
          <span className="preview-value">{preview}</span>
        </div>
      </div>
      <Board
        grid={grid}
        numOfColumns={numOfColumns}
        path={path}
        onPathChange={onPathChange}
        onDone={onPathDone}
      />
      <div className="herramientas">
        <div className="powerUp" 
        onClick={onClickBooster}
        style={(preview === 0 || waiting)? null: {backgroundColor: "#8B0000", cursor:"not-allowed"}}>
        Booster colapsar
        </div>
        <div className="powerUp" 
        onClick={onClickBestPath}
        style={preview === 0? null: {backgroundColor: "#8B0000", cursor:"not-allowed"}}>
          Mejor camino
        </div>
        <div className="powerUp" 
        onClick={onClickBestPath}
        style={preview === 0? null: {backgroundColor: "#8B0000", cursor:"not-allowed"}}>
          Camino Mejor Adyacente
        </div>
      </div>
    </div>
  );
}

export default Game;