# OSX dotfiles

This repo contains my dotfile management system that I use to take care of my
OSX configuration. This project is partially based on
[Zach Holman's dotfiles](https://github.com/holman/dotfiles) project.

## Features

- dotfile management: bashrc, bash_profile
- hosts file management
- OSX configuration
- Homebrew isntall, update, package install
- Atom configuration file management
- Atom package synchronization
- versioned binary path

All mentioned file will be linked to place, by optionally backing up the original
file. The heavy duty package installations are optional to be able to save some
time in a hurry.

## Executable files

There are two executable files in the project at the time.


`./deploy`

Executes the whole dotfile integration to the system. The script optionally bakes up the existing files while replacing them wiht a symbolic link that points to
this repository.

`./backup-atom`

Bakes up all Atom packages to the repo, and pushes the backup to GitHub.
