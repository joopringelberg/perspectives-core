import React, { Component } from "react";
import { Perspectives } from "./perspectivesApiProxy";

// Context -> Rol[] ->
function contextComponent(contextType, rolNames, renderer)
{

  return class extends Component {
    // {instance: String}
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
                component.props.instance,
                contextType + "$" + rolName,
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

function viewComponent(c)
{
  return class extends Component{
    // {instance: String}
    constructor (props)
    {
      super(props);
      const component = this;
      this.state = {};
      this.unsubscribers = {};
      Object.keys( c.propTypes ).forEach( rn => component.state[rn] = undefined);
    }

    componentDidMount()
    {
      const component = this;
      Object.keys( c.propTypes ).forEach(
        function(propertyName)
        {
          Perspectives.then(
            function(pproxy)
            {
              pproxy.getProperty(
                component.props.instance,
                c.rol + "$" + propertyName,
                function (propertyValues)
                {
                  const updater = {};
                  updater[propertyName] = propertyValues;
                  component.setState(updater);
                },
                function (unsubscriber)
                {
                  component.unsubscribers[propertyName] = unsubscriber;
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
      var props;

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
        props = Object.assign({instance: component.props.instance}, component.state);
        return c(props);
      }
      else
      {
        return <div/>;
      }
    }
  };
}

class Binding extends Component
{
  constructor(props)
  {
    super(props);
    this.state.binding = undefined;
    this.unsubscriber = undefined;
  }
  componentDidMount()
  {
    const component = this;
    Perspectives.then(
      function(pproxy)
      {
        pproxy.getBinding(
          component.props.instance,
          function(binding)
          {
            component.setState({binding: binding});
          },
          function(unsubscriber)
          {
            component.unsubscriber = unsubscriber;
          }
        );
      }
    );
  }

  componentWillUnmount ()
  {
    this.unsubscriber();
  }

  // Render! props.children contains the nested elements.
  // These should be provided the retrieved binding value.
  // However, this can only be done after state is available.
  render()
  {
    const component = this;
    if ( component.state.binding )
    {
      return component.props.children.map(
        function(childComponent)
        {
          childComponent({instance: component.state.binding});
        }
      );
    }
    else {
      return <div/>;
    }
  }
}
export {contextComponent, viewComponent, Binding};