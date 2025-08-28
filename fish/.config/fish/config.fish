if status is-interactive
    # Commands to run in interactive sessions can go here
end

function e
    /home/sc/emacs/src/emacs -nw $argv
end

function em
    /home/sc/emacs/src/emacs $argv
end

function n
    nvim $argv
end

function fzf_file
    set file (fzf)
    if test -n "$file"
        commandline -i -- $file
    end
end

bind \ct fzf_file   # Ctrl+F runs fzf_file

function fzf_history_search
    set selected (history | fzf)
    if test -n "$selected"
        commandline --replace -- $selected
    end
end

bind \e\[15\~ fzf_history_search   # F5 keycode in fish

