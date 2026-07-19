# Decompiler test data

The files in this directory are used to test the decompiler output.

## Building

To build a particular source file :

```shell
haxe -cp data -main <ClassName> -hl data/<ClassName>.hl
```

Or with [just](https://just.systems) :

```shell
just fixtures
```
