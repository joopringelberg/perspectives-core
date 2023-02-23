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

Execute this command to combine the sources into a bundle, as it is used by InPlace:

```
npm run build
```
(which just calls `webpack`)

### Develop for InPlace
The PDR forms the base of InPlace. To make changes to the PDR effective in InPlace, you will have to build both 
*  the [SharedWorker](https://github.com/joopringelberg/perspectives-sharedworker) package with the new PDR and 
*  [InPlace](https://github.com/joopringelberg/inplace) with the new SharedWorker.

Assuming you've set up your working environment with Symlinks (see below), just run `npm build` in the former and `npm builddevelopment` (or production) in the latter package's root directory.

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
Start by setting a new version number in package.json.

Then commit all source changes to Git and sync them with Github.

Before publishing a new version (through Github), all dependencies need to be up to date. Use the executable script `bumpVersions.sh` for this. Update version variables of dependencies that are maintained in the Perspective project in this script. They look like this:

```
AVAR_MONADASK=v2.1.1
KISHIMEN=v1.1.0
```

 Then run it. This will produce up to date versions of

* package.json
* packages.dhall
* createPerspectivesLinks.sh

Now you're ready to commit these changes (including the adapted `bumpversions.sh` file). Tag this commit with a new version number.

## Adding new dependencies
When you add a new dependency with npm and use the --save or --save-dev flag, it will be registered in the `package.json` file. **However**, the next time you run `bumpVersions.sh`, these additions will be lost **unless** you add the new dependency manually to the `package.template.json` file, too.

More rare is the occasion that a new Purescript dependency is added, for a package that is part of the Perspectives project. In that case, adapt the packages.template.dhall file.