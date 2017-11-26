# Dotfiles time

This repo contains my dotfile management system that I use to take care of my
OS X and Linux configurations. This project is partially based on
[Zach Holman's dotfiles](https://github.com/holman/dotfiles) project.



# TMUX

Last updated on _2017-11-26_.

## Installed plugins

* [tmux-plugins/tpm](https://github.com/tmux-plugins/tpm) - Tmux Plugin Manager.
* [tmux-plugins/tmux-sensible](https://github.com/tmux-plugins/tmux-sensible) - Basic tmux settings everyone can agree on.
* [tmux-plugins/tmux-battery](https://github.com/tmux-plugins/tmux-battery) - Plug and play battery percentage and icon indicator for Tmux.
* [tmux-plugins/tmux-cpu](https://github.com/tmux-plugins/tmux-cpu) - Plug and play cpu percentage and icon indicator for Tmux..
* [tmux-plugins/tmux-yank](https://github.com/tmux-plugins/tmux-yank) - Tmux plugin for copying to system clipboard. Works on OSX, Linux and Cygwin.

| Command | Description |
| --- | --- |
| <kbd>prefix</kbd> + <kbd>I</kbd>  | Install tmux plugins.  |
| <kbd>prefix</kbd> + <kbd>U</kbd>  | Update tmux plugins.  |
| <kbd>prefix</kbd> + <kbd>Alt</kbd> + <kbd>u</kbd>  | Update tmux plugins.  |

## Navigation

New faster navigation added.

| Command | Description |
| --- | --- |
| <kbd>Alt</kbd> + <kbd>h</kbd>  | Switch between panes left.  |
| <kbd>Alt</kbd> + <kbd>j</kbd>  | Switch between panes down.  |
| <kbd>Alt</kbd> + <kbd>k</kbd>  | Switch between panes up.  |
| <kbd>Alt</kbd> + <kbd>l</kbd>  | Switch between panes right.  |
| <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>h</kbd>  | Switch to previous window.  |
| <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>l</kbd>  | Switch to next window.  |
| <kbd>Alt</kbd> + <kbd>z</kbd>  | Zoom into pane.  |
| <kbd>prefix</kbd> + <kbd>%</kbd>  | Divide pane vertically.  |
| <kbd>prefix</kbd> + <kbd>"</kbd>  | Divide pane horizontally.  |
| <kbd>prefix</kbd> + <kbd>Ctrl</kbd> + <kbd>h</kbd> | Resize pane left. |
| <kbd>prefix</kbd> + <kbd>Ctrl</kbd> + <kbd>j</kbd> | Resize pane down. |
| <kbd>prefix</kbd> + <kbd>Ctrl</kbd> + <kbd>k</kbd> | Resize pane up. |
| <kbd>prefix</kbd> + <kbd>Ctrl</kbd> + <kbd>l</kbd> | Resize pane right. |

## Copy and paste

Since the mouse mode is turned on, copying is pretty straightforward:

| Command | Description |
| --- | --- |
| _mouse selection_  | Copies selected content (yellow hightlight) to the __tmux buffer__ and to the __system clipboard__.  |
| <kbd>Shift</kbd> + _mouse selection_  | Selects text in the legacy way (white highlight), and puts the selected text into the __legacy system clipboard__.  |
| <kbd>prefix</kbd> + <kbd>P</kbd>  | Paste from the __tmux buffer__.  |
| <kbd>Shift</kbd> + <kbd>Ctrl</kbd> + <kbd>v</kbd>  | Paste from the __system clipboard__.  |







## License

This project is under the __MIT license__. 
Copyright (c) 2016 Tibor Simon

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


