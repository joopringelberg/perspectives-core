import React, { Component } from "react";
import { Perspectives } from "./perspectivesApiProxy";

class Context extends Component
{
  // {instance: String}
  constructor (props)
  {
    super(props);
    const component = this;
    component.state = {};
    component.props.rollen.forEach(rn => component.state[rn] = undefined);
    component.unsubscribers = {};
  }

  componentDidMount ()
  {
    const component = this;
    component.props.rollen.forEach(
      function (rolName)
      {
        Perspectives.then(
          function (pproxy)
          {
            pproxy.getRol(
              component.props.instance,
              component.props.type + "$" + rolName,
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
    Object.keys(unsubscribers).forEach(
      unsubscriber => unsubscribers[unsubscriber]()
    );
  }

  render ()
  {
    const component = this;

    if (stateIsComplete(this))
    {
      if (Array.isArray(component.props.children))
      {
        return React.Children.map(
          component.props.children,
          function (child)
          {
            return React.cloneElement(
              child,
              {
                instance: component.state[child.props.rol],
                namespace: component.props.type
              });
          });
      }
      else
      {
        return React.cloneElement(component.props.children, {instance: component.state.binding});
      }
    }
    else
    {
      return <div />;
    }
  }
}

function viewComponent (c)
{
  return class extends Component
  {
    // {instance: String}
    constructor (props)
    {
      super(props);
      const component = this;
      this.state = {};
      this.unsubscribers = {};
      Object.keys(c.propTypes).forEach(rn => component.state[rn] = undefined);
    }

    componentDidMount ()
    {
      const component = this;
      Object.keys(c.propTypes).forEach(
        function (propertyName)
        {
          Perspectives.then(
            function (pproxy)
            {
              pproxy.getProperty(
                component.props.instance,
                component.props.namespace + "$" + component.props.rol + "$" + propertyName,
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
      Object.keys(unsubscribers).forEach(
        unsubscriber => unsubscribers[unsubscriber]()
      );
    }

    render ()
    {
      const component = this;
      var props;

      if (stateIsComplete(this))
      {
        props = Object.assign({instance: component.props.instance}, component.state);
        return c(props);
      }
      else
      {
        return <div />;
      }
    }
  };
}

class Binding extends Component
{
  constructor (props)
  {
    super(props);
    this.state = {};
    this.state.binding = undefined;
    this.unsubscriber = undefined;
  }

  componentDidMount ()
  {
    const component = this;
    Perspectives.then(
      function (pproxy)
      {
        // Retrieve the type of the binding.
        // This will be the namespace that its properties are defined in.
        pproxy.getBinding(
          component.props.instance,
          function (binding)
          {
            component.setState({binding: binding[0]});
          },
          function (unsubscriber)
          {
            component.unsubscriber = unsubscriber;
          }
        );
        pproxy.getBindingType(
          component.props.instance,
          function (bindingType)
          {
            component.setState({bindingType: bindingType[0]});
          },
          function (unsubscriber)
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
  render ()
  {
    const component = this;

    if (component.state.binding)
    {
      if (Array.isArray(component.props.children))
      {
        return React.Children.map(
          component.props.children,
          function (child)
          {
            return React.cloneElement(
              child,
              {
                instance: component.state.binding,
                namespace: component.state.bindingType
              });
          });
      }
      else
      {
        return React.cloneElement(
          component.props.children,
          {
            instance: component.state.binding,
            namespace: component.state.bindingType
          });
      }
    }
    else
    {
      return <div />;
    }
  }

}

function stateIsComplete (comp)
{
  var isComplete = true;
  Object.keys(comp.state).forEach(
    function (prop)
    {
      if (!comp.state[prop])
      {isComplete = false;}
    });

  return isComplete;
}

export { Context, viewComponent, Binding };