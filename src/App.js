import React, { Component } from "react";
import "./App.css";
import { main } from "./output/Main";

main();

class App extends Component
{
  render ()
  {
    return (
      <div className="App">
        <header className="App-header">
          <h1 className="App-title">Perspectives editor</h1>
        </header>
        <div>
          <p>
            <button onClick={() => alert("clicked me")}>Clear</button>
          </p>
        </div>
      </div>
    );
  }
}

//bla
export default App;
