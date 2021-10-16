# mxtodo

An Emacs package for managing TODOs spread across Markdown notes.

## Dependencies

To develop on mxtodo, you'll want to have [eldev](https://github.com/doublep/eldev) installed locally.
Additionally, building the mxtodo-searcher dynamic module requires a working Rust development environment.

## Running tests

```shell
eldev -p -dtT -C test --expect 5
eldev -dtT -C compile --set all --warnings-as-errors
```
