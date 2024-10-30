perspectives-distributed-runtime
======================

### About
The Perspectives Distributed Runtime (PDR) is part of the software created in the course of the [Perspectives Project](https://academy.perspect.it).

The PDR interprets models written in the Perspectives Language. **Patterns of co-operation** can easily be expressed in PL. Examples of co-operation are: buying and selling, renting stuff, a formal meeting, etc.

The PDR has an API for client programs that offer end users screens to interact with each other. Users of such programs exchange information directly, in a Peer-to-Peer fashion, without intermediate servers that store their information.

### Getting started
The PDR is work in progress. It is not yet in a state that it can be used. Hence, we provide no instructions on how to use it right now. However, [here](./technical%20readme.md) is a page with instructions for developers of the PDR.

### Contributing
The PDR is being intensively developed by the core team. We appreciate feedback on the code you can find in this repository

### Dependencies
#### On Purescript packages belonging to the Perspectives Project
* perspectives-couchdb
* purescript-avar-monadask
* purescript-aff-sockets
* perspectives-apitypes
* perspectives-lru-cache
* perspectives-utilities
* serialisable-nonempty-arrays

#### On cloned Purescript packages
* purescript-affjax
* purescript-parsing
* kishimen

#### On Node packages belonging to the Perspectives Project
* perspectives-proxy

### Packages belonging to the project but no core dependencies
* perspectives-react
* perspectives-react-integrated-client
* perspectives-documentation
* perspectives-screens
* screenuploader

### License information
This project is available as open source under the terms of the GPL-3.0-or-later license. For accurate information, please check individual files.

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