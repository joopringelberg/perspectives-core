import React, { Component } from "react";
import { Perspectives } from "./perspectivesApiProxy";
import PropTypes from "prop-types";
import {deconstructLocalNameFromDomeinURI_ as localName} from "../output/Perspectives.Identifiers";

class PerspectivesComponent extends Component
{
  constructor(props)
  {
    super(props);
    this.state = {};
    this.unsubscribers = [];
  }

  componentWillUnmount ()
  {
    this.unsubscribers.forEach( unsubscriber => unsubscriber() );
  }

  addUnsubscriber(unsubscriber)
  {
    this.unsubscribers.push(unsubscriber);
  }

  stateIsComplete ()
  {
    const component = this;
    let isComplete = true;
    Object.keys(component.state).forEach(
      function (prop)
      {
        if (!component.state[prop])
        {isComplete = false;}
      });

    return isComplete;
  }

  stateIsEmpty ()
  {
    return Object.keys(this.state).length === 0;
  }

}

class Context extends PerspectivesComponent
{
  constructor (props)
  {
    let component;
    super(props);
    component = this;
    component.props.rollen.forEach(rn => component.state[rn] = undefined);
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
              component.addUnsubscriber.bind(component));
          }
        );
      }
    );
  }

  render ()
  {
    const component = this;
    let child;

    if (component.stateIsComplete())
    {
      if (Array.isArray(component.props.children))
      {
        return React.Children.map(
          component.props.children,
          function (child)
          {
            const childRol = child.props.rol;
            let instance;
            if (!childRol)
            {
              throw "Context (" + component.props.type + ") finds child of type '" + child.type.name + "' that has no 'rol' on its props.";
            }
            instance = component.state[childRol];
            if (!instance)
            {
              throw "Context (" + component.props.type + ") has no rol '" + childRol + "' while the child of type '" + child.type.name + "' asks for it.";
            }
            return React.cloneElement(
              child,
              {
                instance: instance[0],
                namespace: component.props.type
              });
          });
      }
      else
      {
        child = component.props.children;
        return React.cloneElement(
          child,
          {
            instance: component.state[child.props.rol][0],
            namespace: component.props.type
          });
      }
    }
    else
    {
      return <div />;
    }
  }
}

Context.propTypes = {
  instance: PropTypes.string.isRequired,
  type: PropTypes.string.isRequired,
  rollen: PropTypes.array.isRequired
};

class Binding extends PerspectivesComponent
{
  constructor (props)
  {
    super(props);
    this.state.binding = undefined;
    this.state.bindingType = undefined;
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
          component.addUnsubscriber.bind(component)
        );
        pproxy.getBindingType(
          component.props.instance,
          function (bindingType)
          {
            component.setState({bindingType: bindingType[0]});
          },
          component.addUnsubscriber.bind(component)
        );
      }
    );
  }

  // Render! props.children contains the nested elements.
  // These should be provided the retrieved binding value.
  // However, this can only be done after state is available.
  render ()
  {
    const component = this;

    if (component.stateIsComplete())
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

Binding.propTypes = {
  instance: PropTypes.string,
  namespace: PropTypes.string,
  rol: PropTypes.string.isRequired
};

class View extends PerspectivesComponent
{
  constructor (props)
  {
    super(props);
  }

  componentDidMount ()
  {
    const component = this;
    Perspectives.then(
      function(pproxy)
      {
        let qualifiedView;
        if (component.props.rol)
        {
          qualifiedView = component.props.namespace + "$" + component.props.rol + "$" + component.props.viewnaam;
        }
        else
        {
          qualifiedView = component.props.namespace + "$" + component.props.viewnaam;
        }
        pproxy.getViewProperties(
          qualifiedView,
          function(propertyNames)
          {
            // First initialize state
            // NOTE: React will not notice this.
            propertyNames.forEach(
              function(propertyName)
              {
                const ln = localName(propertyName);
                component.state[ln] = undefined;
              }
            );
            propertyNames.forEach(
              function(propertyName)
              {
                pproxy.getProperty(
                  component.props.instance,
                  propertyName,
                  function(propertyValues)
                  {
                    const updater = {};
                    const ln = localName(propertyName);
                    updater[ln] = propertyValues;
                    component.setState(updater);
                  },
                  component.addUnsubscriber.bind(component)
                );
              }
            );
          },
          component.addUnsubscriber.bind(component)
        );
      }
    );
  }


  // Render! props.children contains the nested elements.
  // These should be provided all property name-value pairs,
  // except when it has a prop 'propertyName'.
  // However, this can only be done after state is available.
  render ()
  {
    const component = this;

    function cloneChild (child)
    {
      // If the child has a prop 'propertyName', just provide the property value.
      if (child.props.propertyName)
      {
        return React.cloneElement(
          child,
          {
            value: component.state[child.props.propertyName]
          });
      }
      else
      {
        return React.cloneElement(
          child,
          component.state);
      }
    }

    if (!component.stateIsEmpty() && component.stateIsComplete())
    {
      if (Array.isArray(component.props.children))
      {
        return React.Children.map(
          component.props.children,
          cloneChild);
      }
      else
      {
        return cloneChild(component.props.children);
      }
    }
    else
    {
      return <div />;
    }
  }

}

View.propTypes = {
  instance: PropTypes.string,
  namespace: PropTypes.string,
  rol: PropTypes.string,
  viewnaam: PropTypes.string.isRequired
};

export { Context, Binding, View };