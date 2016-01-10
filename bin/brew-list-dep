brew list | while read cask; do echo -n "$cask ->"; brew deps $cask | awk '{printf(" %s ", $0)}'; echo ""; done
