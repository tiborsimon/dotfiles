polkit.addRule(function(action, subject) {
    if (action.id == 'org.freedesktop.resolve1.set-dns-servers' ||
        action.id == 'org.freedesktop.resolve1.set-domains' ||
        action.id == 'org.freedesktop.resolve1.set-dnssec') {
        if (subject.user == 'openvpn') {
            return polkit.Result.YES;
        }
    }
});
