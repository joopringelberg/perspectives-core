import React, { Component } from "react";
import { Perspectives } from "./perspectivesApiProxy";

// NameSpace -> RolName[] ->
function contextComponent(namespace, rolNames, renderer)
{

  return class extends Component {
    constructor (props)
    {
      super(props);
      const component = this;
      this.state = {};
      rolNames.forEach( rn => component.state[rn] = undefined);
      this.unsubscribers = {};
    }

    componentDidMount()
    {
      const component = this;
      rolNames.forEach(
        function(rolName)
        {
          Perspectives.then(
            function(pproxy)
            {
              pproxy.getRol(
                component.props.id,
                namespace + "$" + rolName,
                function (rolIds)
                {
                  const updater = {};
                  updater[rolName] = rolIds;
                  component.setState(updater);
                },
                function (unsubscriber)
                {
                  component.unsubscribers[rolName] = unsubscriber;
                });
            }
          );
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
      const component = this;

      function stateIsComplete()
      {
        var isComplete = true;
        Object.keys( component.state ).forEach(
          function(prop)
          {
            if (!component.state[prop]) {isComplete = false;}
          });

        return isComplete;
      }

      if (stateIsComplete())
      {
        return renderer.call(component);
      }
      else
      {
        return <div/>;
      }
    }
  };
}

export {contextComponent}