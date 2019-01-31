#    __ _     _
#   / _(_)   | |
#  | |_ _ ___| |__
#  |  _| / __| '_ \
#  | | | \__ \ | | |
#  |_| |_|___/_| |_|
#

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


# ===================================================================
#  V I R T U A L F I S H   S E T T I N G S
eval (python -m virtualfish auto_activation)


# ===================================================================
#  P J   S E T T I N G S
set -gx PROJECT_PATHS ~/projects


# ===================================================================
#  B O B   T H E   F I S H   S E T T I N G S
set -g theme_display_git                yes
set -g theme_display_git_dirty          yes
set -g theme_display_git_untracked      yes
set -g theme_display_git_ahead_verbose  yes
set -g theme_display_git_dirty_verbose  no
set -g theme_display_git_master_branch  yes
set -g theme_git_worktree_support       yes
set -g theme_display_vagrant            yes
set -g theme_display_docker_machine     yes
set -g theme_display_k8s_context        yes
set -g theme_display_hg                 no
set -g theme_display_virtualenv         yes
set -g theme_display_ruby               no
set -g theme_display_user               ssh
set -g theme_display_hostname           ssh
set -g theme_display_vi                 no
set -g theme_display_date               yes
set -g theme_display_cmd_duration       yes
set -g theme_title_display_process      yes
set -g theme_title_display_path         no
set -g theme_title_display_user         no
set -g theme_title_use_abbreviated_path yes
set -g theme_date_format                "+%F %a %H:%M"
set -g theme_avoid_ambiguous_glyphs     yes
set -g theme_powerline_fonts            no
set -g theme_nerd_fonts                 no
set -g theme_show_exit_status           yes
set -g default_user                     tibor
set -g theme_color_scheme               dark
set -g fish_prompt_pwd_dir_length       0
set -g theme_project_dir_length         1
set -g theme_newline_cursor             yes
