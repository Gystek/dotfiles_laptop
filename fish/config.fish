if status is-interactive
    set -gx GPG_TTY (tty)
end

set fish_greeting ""

function fish_prompt
    printf "[%s@%s] %s \$ " (whoami) (hostname) (prompt_pwd)
end

set PATH /usr/local/bin \
         /bin \
         /usr/bin \
         /usr/local/sbin \
         /usr/sbin \
         $HOME/.local/bin \
	 $HOME/.opam/default/bin \
	 $HOME/.cargo/bin

# opam configuration
source /home/gustek/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true
