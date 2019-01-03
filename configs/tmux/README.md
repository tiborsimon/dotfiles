# TMUX

Last updated on _2017-11-26_.

<kbd>prefix</kbd> = <kbd>Ctrl</kbd> + <kbd>Space</kbd>

## Plugins

* [tmux-plugins/tpm](https://github.com/tmux-plugins/tpm) - Tmux Plugin Manager.
* [tmux-plugins/tmux-sensible](https://github.com/tmux-plugins/tmux-sensible) - Basic tmux settings everyone can agree on.
* [tmux-plugins/tmux-battery](https://github.com/tmux-plugins/tmux-battery) - Plug and play battery percentage and icon indicator for Tmux.
* [tmux-plugins/tmux-cpu](https://github.com/tmux-plugins/tmux-cpu) - Plug and play cpu percentage and icon indicator for Tmux..
* [tmux-plugins/tmux-yank](https://github.com/tmux-plugins/tmux-yank) - Tmux plugin for copying to system clipboard. Works on OSX, Linux and Cygwin.

|Command |Description |
| --- | --- |
| <kbd>prefix</kbd> + <kbd>I</kbd>  | Install tmux plugins.  |
| <kbd>prefix</kbd> + <kbd>U</kbd>  | Update tmux plugins.  |
| <kbd>prefix</kbd> + <kbd>Alt</kbd> + <kbd>u</kbd>  | Update tmux plugins.  |

## Navigation

New faster navigation added.

|Command |Description |
| --- | --- |
| <kbd>Alt</kbd> + <kbd>h</kbd>  | Switch to the pane in the left.  |
| <kbd>Alt</kbd> + <kbd>j</kbd>  | Switch to the pane below.  |
| <kbd>Alt</kbd> + <kbd>k</kbd>  | Switch to the pane above.  |
| <kbd>Alt</kbd> + <kbd>l</kbd>  | Switch to the oane in the right.  |
| <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>h</kbd>  | Switch to previous window.  |
| <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>l</kbd>  | Switch to next window.  |
| <kbd>Alt</kbd> + <kbd>z</kbd>  | Zoom into pane.  |
| <kbd>prefix</kbd> + <kbd>\|</kbd>  | Divide pane vertically. |
| <kbd>prefix</kbd> + <kbd>-</kbd>  | Divide pane horizontally.  |
| <kbd>prefix</kbd> + <kbd>Ctrl</kbd> + <kbd>h</kbd> | Resize pane left. |
| <kbd>prefix</kbd> + <kbd>Ctrl</kbd> + <kbd>j</kbd> | Resize pane down. |
| <kbd>prefix</kbd> + <kbd>Ctrl</kbd> + <kbd>k</kbd> | Resize pane up. |
| <kbd>prefix</kbd> + <kbd>Ctrl</kbd> + <kbd>l</kbd> | Resize pane right. |

## Copy and paste

Since the mouse mode is turned on, copying is pretty straightforward:

|Command |Description |
| --- | --- |
| _mouse selection_  | Copies selected content (yellow hightlight) to the __tmux buffer__ and to the __system clipboard__.  |
| <kbd>Shift</kbd> + _mouse selection_  | Selects text in the legacy way (white highlight), and puts the selected text into the __legacy system clipboard__.  |
| <kbd>prefix</kbd> + <kbd>P</kbd>  | Paste from the __tmux buffer__.  |
| <kbd>Shift</kbd> + <kbd>Ctrl</kbd> + <kbd>v</kbd>  | Paste from the __system clipboard__.  |
