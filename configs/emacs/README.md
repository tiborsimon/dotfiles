# My Emacs Configuration


# Org mode

This section is mainly derived from Rainer Konig's [Getting yourself organized with OrgMode](https://www.youtube.com/watch?v=sQS06Qjnkcc&list=PLVtKhBrRV_ZkPnBtt_TD1Cs9PJlU0IIdE) video series, along with my own customizations.


## Headlines

Every headline starts with a `*`.

`Tab` controls the visibility of the local headlines.
`Shift`+`Tab` controls the global visibility of the headlines.

Increase and decrase the indentation level with `Alt`-`Left` and `Alt`-`Right`.

Move up/down within a level with `Alt`-`Up` and `Alt`-`Down`

To refile a headings to another higher level heding, use `Ctrl`-`c` `Ctrl`-`w`.

## Todo items

Define your own custom todo keyword:
  
```
#+SEQ_TODO: TODO(t) NEXT(n) WAITING(w) | DONE(d) CANCELLED(c)
```

Items before the pipe are _ToDo_ items.
Items after the pipe are _Done_ items.

When edited, it can be activated with `Ctrl`-`c` `Ctrl`-`c`.

You can cycle trough all defined __ToDo__ keywords with `Shift`-`Left` or `Shift`-`Right`.

To open up a manu with all keyboards use `Ctrl`-`c` `Ctrl`-`t`



## Checklist

Checklists are composed with the followint type if item list:

```
- [ ] Checklist one.
- [ ] checklist two.
```

To insert a new checklist item, press `Alt`-`Shift`-`Enter`.

Checkboxes can be toggled with `Ctrl`-`C` `Ctrl`-`C`.

Headings containing checkboxes can display the state of the checkboxes with the following pattern:

```
** My heading [/] [%]
```

The `[/]` template will display the completed ones over the all checklist items.
The `[%]` template will display the currently ready percentage.

