perspectives-core
======================

The core provides the middleware between Couchd and clients. It is used in the browser in MyContexts, where it runs in a SharedService.

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

4. To install the purescript dependencies, run:

```
  $ spago install
```

5. To install the javascript dependencies, run:

```
  $ npm install
```

### Development environment
Preferrably install [vscode](https://code.visualstudio.com/) and add two extensions:
  * [Purescript IDE](https://marketplace.visualstudio.com/items?itemName=nwolverson.ide-purescript)
  * [Purescript Language Support](https://marketplace.visualstudio.com/items?itemName=nwolverson.language-purescript)

Alternatively, install [Atom](https://atom.io/) and two packages to support Purescript:
  * [purescript-contrib/atom-language-purescript](https://github.com/purescript-contrib/atom-language-purescript) provides syntax highlighting
  * [nwolverson/atom-ide-purescript](https://github.com/nwolverson/atom-ide-purescript) provides build support, REPL, and autocomplete etc. via psc-ide

The vscode version has more recent language server support than the Atom version.

### Develop

To compile the .purs source files, run:

```
  $ spago build
```

Execute this command to combine the sources into a bundle, as it is used by MyContexts:

```
npm run build
```
(which just calls `rollup`)

### Develop for MyContexts
The PDR forms the base of MyContexts. To make changes to the PDR effective in MyContexts, you will have publish the new PDR to https://mycontexts.com (or https://mycontexts.com/remotetest for testing). There is a script in the package.json to do the latter: `copyCore`. You will be prompted for a password on the server.

### Program documentation
The source files have many annotations. Moreover, the compiler can generate standard documentation from the types in the sources. The entrance point to this documentation is [here](https://joopringelberg.github.io/perspectives-core/Perspectives.Docu.Main.html#t:x).

Generate a new version of the documentation with:

```
npm run docs
```

__NOTE__ do not use the `pulp docs` command. It [generates documentation](https://github.com/purescript-contrib/pulp#building-documentation) in the directory `generated-docs`, while [Github Pages](https://help.github.com/en/github/working-with-github-pages/configuring-a-publishing-source-for-your-github-pages-site) expects it to be in `docs`.

### Symlinks for easy updates
`package.json` contains a run target `symlinks` that will replace the subdirectories of dependencies that are co-developed with perspectives-core with symlinks to those projects, assuming that they live side-by-side with perspectives-core. Run this script after calling `npm install` or `spago install`. A change in this project is then immediately picked up by webpack (when watching, of course).

## Publish new package version:
1. In spago.yaml: update the version of `perspectives-apitypes` at `ref`;
2. outcomment the `path` section;
3. incomment the `git` and `ref` sections.
3. Repeat this for: `purescript-avar-monadask`, `purescript-lru-cache`, `purescript-parsing`, `perspectives-utilities`, `serialisable-nonempty-arrays`, `purescript-subtlecrypto`.
4. In package.json: update the version of `perspectives-proxy`;
4. increase the package number.
5. Commit.
6. Create tag.
7. Push tag.
8. In spago.yaml: switch back to the local versions.
