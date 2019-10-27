perspectives-core
======================

The core provides the middleware between Couchd and clients. It can be approached through two channels:
* an internal channel
* a TCP (socket-based) channel on port 7777.

Clients communicate with the core on both channels using exactly the same protocol that essentially is querying for roles or properties.

### Setting up
Follow these steps to start developing.

1. Clone this repository.
2. Install [Purescript](http://www.purescript.org).
3. Install [psc-package](https://github.com/purescript/psc-package)
4. Install [pulp](https://github.com/purescript-contrib/pulp)
5. Install [Webpack](https://webpack.js.org/)
5. To install the dependencies, run:

```
  $ psc-package install
```

6. To compile the .purs source files, run:

```
  $ pulp build
```

Additionally, install [Atom](https://atom.io/) and two packages to support Purescript:
  * [purescript-contrib/atom-language-purescript](https://github.com/purescript-contrib/atom-language-purescript) provides syntax highlighting
  * [nwolverson/atom-ide-purescript](https://github.com/nwolverson/atom-ide-purescript) provides build support, REPL, and autocomplete etc. via psc-ide


### Develop this interface; construct a bundle
Assuming code will be changed with Atom (and the Atom-purescript-ide), changes in source code will be immediately compiled into javascript in the `output` directory. The project contains a Webpack configuration file. If

`npm run watch`

is executed before changing code, changes will be immediately reflected in the bundled code in `dist/perspectives-core.js`.

### Program documentation
The source files have many annotations. Moreover, the compiler can generate standard documentation from the types in the sources. The entrance point to this documentation is [here](generated-docs/Perspectives.Docu.Main.html#t:x).

Generate a new version of the documentation with:

```
pulp docs -- --format html
```

### Symlinks for easy updates
`package.json` contains a run target `symlinks` that will replace the subdirectories
* aff-sockets
* perspectives-apitypes
* avar-monadask

in the .psc-package directory with symlinks to projects in ~Code. Run this script after calling `npm install`. A change in this project is then immediately picked up by webpack (when watching, of course).

**Note**. If a new version of `psc-0.11.7-perspectives-core3` is created, the script needs to be adapted!
