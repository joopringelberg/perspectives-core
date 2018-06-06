import React, { Component } from "react";
import "./App.css";
import { main } from "./output/Main";
import { contextComponent } from "./client/hoc";

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
        <Systeem id="model:User$MijnSysteem" />
      </div>
    );
  }
}

const Systeem =
        contextComponent(
          "model:Systeem$Systeem",
          [
            "gebruiker",
            "trustedCluster"
          ],
          function ()
          {
            return (
              <div>
                <Gebruiker id={this.state.gebruiker} />
                <TrustedCluster id={this.state.trustedCluster} />
              </div>
            );
          });

function Gebruiker(props)
{
  return <p>De gebruiker heeft id: {props.id}</p>;
}

function TrustedCluster(props)
{
  return <p>Het TrustedCluster heeft id: {props.id}</p>;
}
export default App;
