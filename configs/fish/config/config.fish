#    __ _     _
#   / _(_)   | |
#  | |_ _ ___| |__
#  |  _| / __| '_ \
#  | | | \__ \ | | |
#  |_| |_|___/_| |_|
#

# ===================================================================
#  S P A C E F I S H

# Exit code config
set SPACEFISH_EXIT_CODE_SHOW true
set SPACEFISH_EXIT_CODE_SYMBOL ''

# turning off unnecessary languages
set SPACEFISH_PHP_SHOW false
set SPACEFISH_GOLANG_SHOW false
set SPACEFISH_HASKELL_SHOW false
set SPACEFISH_CONDA_SHOW false
set SPACEFISH_RUBY_SHOW false
set SPACEFISH_JULIA_SHOW false


# ===================================================================
#  U S E R   K E Y   B I N D I N G S

if status --is-interactive
    function fish_user_key_bindings
        # vim movement
        bind \cl forward-char
        bind \ch backward-char
        bind \cj down-or-search
        bind \ck up-or-search
    end
end


# ===================================================================
#  F U N C T I O N S

# Jumping convenience functions
function dot --description 'Jumps to the dotfiles repository.'
    cd ~/.dotfiles
end

function pro --description 'Jumps to the projects directory.'
    cd ~/projects
    ls
end

function sand --description 'Jumps to the sandbox directory.'
    cd ~/sandbox
    ls
end

function secrets --description 'Jumps to the secrets directory'
    cd ~/secrets
end


# Command abbrevations
function r --description 'Fires up ranger.'
    ranger $argv
end

function gs --description 'Runs git status.'
    git status $argv
end
