# Dotfiles configuration management system

This repository contains my personal Arch based configuration management
system. Though I use Arch, you can use this system on any Unix based system as
it is written mostly in bash.

# Design goals

## Separation

Group together related configurations, dependencies and scripts into bundles.

## Encapsulation

Each configuration bundle knows how to deploy itself. No global knowledge is
needed. The only requirement is to be compatible with the simple _deployment
API_.

## Simple user interface

I like to use `make` as an interface for my tools and projects. It is present
almost every system and provides a clean usage in contrast to scripts based
interfaces.

```
$ make
===================================================================
                .o0 ~ DOTFILES make interface ~ 0o.
===================================================================

   help            Prints out this help message.

   all             Performs all commands at once.

   install         Installs the dependencies.
   install-custom  Executes the custom dependency installations.
   deploy          Deploys the configuration files.

===================================================================
```

## Documentation

Every configuration bundle can and should be documented separatedly. This helps
others (and ourselves as time passes) to understand the configuration


# Deployment API

Every configuration bundle and script group has to comply with the __Deployment
API__ in order to be able to handled by the system.


# License

This project is under the __MIT license__. See the included __LICENSE__ file.

