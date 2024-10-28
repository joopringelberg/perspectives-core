// BEGIN LICENSE
// Perspectives Distributed Runtime
// Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//
// Full text of this license can be found in the LICENSE file in the projects root.

// END LICENSE

let EventSource;

if (typeof window === 'undefined') {
    // Running in Node.js
    EventSource = require('eventsource');
} else {
    // Running in the browser
    EventSource = window.EventSource;
}

export function createEventSourceImpl ( databaseUrl, queryParams )
{
  return new EventSource( databaseUrl + "/_changes?feed=eventsource" + queryParams );
}

// This function will be called from Perspectives Core if it want to set up an internal channel to a GUI.
// emitStep will be bound to the constructor Emit, finishStep will be the constructor Finish.
// databaseUrl should point to the database. It need not be terminated with a slash.
export function createChangeEmitterImpl (es, emitStep, finishStep, emit)
{
  // Set the handler.
  es.onmessage = function(e) {
    var doc;
    // closeEventSourceImpl signals closing down by emitting "finish".
    if ( e === "finish" )
    {
      // Emit the change to Purescript. `emit` is in Effect:
      // emit :: forall m a r. Emitter m a r -> a -> m Unit
      // so we have to apply the result we get from emit to sort the effect.
      emit( finishStep( {} ) )();
    }
    else
    {
      // console.log( JSON.parse( e.data ));
      emit( emitStep( e.data ) )();
    }
  };
}

export function closeEventSourceImpl( es )
{
  es.dispatchEvent({type: "message", detail: "finish"});
  es.close();
}
