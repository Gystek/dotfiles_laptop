if status is-interactive
    set -gx GPG_TTY (tty)
end

set fish_greeting ""

function fish_prompt
    printf "\e[0;90m[\e[0;35m%s\e[0;90m@\e[0;32m%s \e[0;34m%s\e[0;90m]\e[0;97m\$ " (whoami) (hostname) (prompt_pwd)
end

set -gx ORGANIZATION "Gustek <gustek@riseup.net>"

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
