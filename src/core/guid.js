// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

'use strict';
/*
 Generate a guid (taken from http://stackoverflow.com/questions/105034/how-to-create-a-guid-uuid-in-javascript)
 */
exports.guid_ = function(ignore)
{
  return function()
  {
    return 'xxxxxxxx_xxxx_4xxx_yxxx_xxxxxxxxxxxx'.replace(
      /[xy]/g, function( c )
      {
        //noinspection MagicNumberJS
        var r = Math.random() * 16 | 0, v = c === 'x' ? r : (
          r & 0x3 | 0x8
          );
        return v.toString( 16 );
      } );
  }
};

exports.show_ = function(x){return x};
