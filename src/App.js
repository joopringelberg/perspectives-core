import React, { Component } from "react";

import AceEditor from "react-ace";

import "./App.css";
import { main } from "./output/Main";
import { Context, Binding, View } from "./client/hoc";

main();

class App extends Component
{
  render ()
  {
    return (
      <div className="App">
        <header className="App-header">
          <h1 className="App-title">Perspectives</h1>
        </header>
        <Context type="model:Systeem$Systeem" rollen={["gebruiker", "modellen", "trustedCluster"]} instance="model:User$MijnSysteem">
          <View rol="gebruiker" viewnaam="VolledigeNaam">
            <GebruikerNaam/>
          </View>
          <Binding rol="trustedCluster">
            <View viewnaam="Kaartje">
              <TrustedCluster_BuitenRol_Kaartje/>
            </View>
          </Binding>
          {/*<View rol="modellen" viewnaam="AlleModellen">*/}
            {/*<Models/>*/}
          {/*</View>*/}
        </Context>

        <AceEditor
          mode="perspectives"
          theme="perspectives"
          onChange={onChange}
          name="aceEditor"
          editorProps={{$blockScrolling: true}}
        />
      </div>
    );
  }
}

function onChange(newValue) {
  // console.log("change",newValue);
}

// function test()
// {
//   return (<View naam="volledigeNaam">
//     <p><label>Gebruiker:</label><Property naam="voornaam"/><Property naam="achternaam"/></p>
//   </View>);
// }


function GebruikerNaam(props)
{
  return <p><label>Gebruiker:</label>{props.voornaam + " " + props.achternaam}</p>;
}

function TrustedCluster_BuitenRol_Kaartje(props)
{
  return <p><label>TrustedCluster:</label>{props.naam}</p>;
}

function Models(props)
{
  return <p><label>Models:</label>{props.modellen}</p>;
}

export default App;
