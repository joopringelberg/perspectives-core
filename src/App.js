import React, { Component } from "react";
import "./App.css";
import { main } from "./output/Main";
import { Perspectives } from "./client/perspectivesApiProxy";

main();

class App extends Component
{
  // TODO. Only when perspectives is available, render Systeem!
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

class Systeem extends Component
{
  constructor (props)
  {
    super(props);
    this.state = {
      gebruiker: undefined,
      trustedCluster: undefined
    };
    this.unsubscribers = {};
  }

  componentDidMount ()
  {
    const component = this;
    // Fetch the rollen.
    Perspectives.then(
      function(pproxy)
      {
        pproxy.getRol(
          component.props.id, "model:Systeem$Systeem$gebruiker",
          function (rolIds)
          {
            component.setState({gebruiker: rolIds});
          },
          function (unsubscriber)
          {
            component.unsubscribers.gebruiker = unsubscriber;
          });
        pproxy.getRol(
          component.props.id, "model:Systeem$Systeem$trustedCluster",
          function (rolIds)
          {
            component.setState({trustedCluster: rolIds});
          },
          function (unsubscriber)
          {
            component.unsubscribers.trustedCluster = unsubscriber;
          });
      }
    );
  }

  componentWillUnmount ()
  {
    const unsubscribers = this.state.unsubscribers;
    Object.keys( unsubscribers ).forEach(
      unsubscriber => unsubscribers[unsubscriber]()
    );
  }

  render ()
  {
    // only render if the state is complete!
    if ( this.state.gebruiker && this.state.trustedCluster )
    {
      return (
        <div>
          <Gebruiker id={this.state.gebruiker} />
          <TrustedCluster id={this.state.trustedCluster} />
        </div>
      );
    }
    else
    {
      return <div/>
    }
  }
}

function Gebruiker(props)
{
  return <p>De gebruiker heeft id: {props.id}</p>;
}

function TrustedCluster(props)
{
  return <p>Het TrustedCluster heeft id: {props.id}</p>;
}

export default App;
