perspectives-core
======================

The core provides the middleware between Couchd and clients. It can be approached through two channels:
* an internal channel
* a TCP (socket-based) channel on port 7777.

Clients communicate with the core on both channels using exactly the same protocol that essentially is querying for roles or properties.

### Develop this interface
Assuming code will be changed with Atom (and the Atom-purescript-ide), changes in source code will be immediately compiled into javascript in the `output` directory. If

`npm run watch`

is executed before changing code, changes will be immediately reflected in the bundled code in `dist/perspectives-core.js`.

### Symlinks for easy updates
`package.json` contains a run target `symlinks` that will replace the subdirectory
* aff-sockets

in the .psc-package directory with a symlink to ~Code/purescript-aff-sockets. Run this script after calling `npm install`. A change in this project is then immediately picked up by webpack (when watching, of course).

**Note**. If a new version of `psc-0.11.7-perspectives-core3` is created, the script needs to be adapted!
