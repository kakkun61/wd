# WD

[![CI](https://github.com/kakkun61/wd/actions/workflows/main.yaml/badge.svg)](https://github.com/kakkun61/wd/actions/workflows/main.yaml) [![Sponsor](https://img.shields.io/badge/Sponsor-%E2%9D%A4-red?logo=GitHub)](https://github.com/sponsors/kakkun61)

This is a command to run another command on a specified directory.

## Usage

```
Usage: wd DIR CMD [ARGS]
```

## Examples

```
> pwd
/foo
> wd bar pwd
/foo/bar
```

## Install

On Linux:

```
> cd linux
> make install
```

On Nix:

Set up [Nix User Repository](https://github.com/nix-community/NUR) and

```
> nix-env -f '<nixpkgs>' -iA nur.repos.kakkun61.wd
```

On Windows:

```
> cd windows
> msbuild
```

## Note

*Why you use `_spawnvp` on Windows instead of `_execvp` like `execvp` on Linux?*

`_execvp` on Windows works like that overlays a calling process with a new process and that destroys the calling process. The new process runs in background.
