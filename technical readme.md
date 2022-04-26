perspectives-core
======================

The core provides the middleware between Couchd and clients. It is used in the browser in InPlace, where it runs in a SharedService.

### Setting up
Follow these steps to set up your development environment.

1. Clone this repository.
2. Install [Purescript](http://www.purescript.org) version 0.14.9 (we do not yet support higher versions).

```
npm install -g purescript@0.14.9
```

3. Install [spago](https://github.com/purescript/spago)

```
npm npm install -g spago
```

5. Install [Webpack](https://webpack.js.org/)
```
npm install --save-dev webpack
```

5. To install the purescript dependencies, run:

```
  $ spago install
```

6. To install the javascript dependencies, run:

```
  $ npm install
```

### Developing

To compile the .purs source files, run:

```
  $ spago build
```

Preferrably install [vscode](https://code.visualstudio.com/) and add two extensions:
  * [Purescript IDE](https://marketplace.visualstudio.com/items?itemName=nwolverson.ide-purescript)
  * [Purescript Language Support](https://marketplace.visualstudio.com/items?itemName=nwolverson.language-purescript)

Alternatively, install [Atom](https://atom.io/) and two packages to support Purescript:
  * [purescript-contrib/atom-language-purescript](https://github.com/purescript-contrib/atom-language-purescript) provides syntax highlighting
  * [nwolverson/atom-ide-purescript](https://github.com/nwolverson/atom-ide-purescript) provides build support, REPL, and autocomplete etc. via psc-ide

The vscode version has more recent language server support than the Atom version.

### Develop this interface; construct a bundle
Execute this command to combine the sources into a bundle, as it is used by InPlace:

```
npm run build
```
(which just calls `webpack`)


### Program documentation
The source files have many annotations. Moreover, the compiler can generate standard documentation from the types in the sources. The entrance point to this documentation is [here](https://joopringelberg.github.io/perspectives-core/Perspectives.Docu.Main.html#t:x).

Generate a new version of the documentation with:

```
npm run docs
```

__NOTE__ do not use the `pulp docs` command. It [generates documentation](https://github.com/purescript-contrib/pulp#building-documentation) in the directory `generated-docs`, while [Github Pages](https://help.github.com/en/github/working-with-github-pages/configuring-a-publishing-source-for-your-github-pages-site) expects it to be in `docs`.

### Symlinks for easy updates
`package.json` contains a run target `symlinks` that will replace the subdirectories of dependencies that are co-developed with perspectives-core with symlinks to those projects, assuming that they live side-by-side with perspectives-core. Run this script after calling `npm install` or `spago install`. A change in this project is then immediately picked up by webpack (when watching, of course).

## Publishing a new version
Before publishing a new version (through Github), all dependencies need to be up to date. For this we have an executable script `bumpVersions.sh`. One must edit this script before using it, to make sure all version variables in it have up to date values. Then, running this script will produce up to date versions of

* package.json
* packages.dhall
* createPerspectivesLinks.sh
