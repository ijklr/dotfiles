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
