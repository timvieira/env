#######
# Augmented bash history with metadta (~/.bash_history_meta), such as the
# working directory command was executed.
#
# TODO:
#
#  * PROMPT_COMMAND is only called after the command executes. What we actually
#    need is a pre-execute hook.
#
#  * some commands don't get written in history (e.g. ls, pwd
#
#  * might "break" if there are two shells writting to history
#
function prompt_command {

  # timv: this seems to write to bash history
  history -a

  CMD=`history 1`  # much faster than `history |tail -n1`

  # pull command number out of history file; $HISTCMD didn't work..
#  HISTNUM=`echo "$CMD" |cut -f1 -d' ' `   # broken now has space prefix...
#  HISTNUM=`echo "$CMD" |linepy 'print line.strip().split()[0]' `   # slow
  HISTNUM=`echo "$CMD" |sed 's/^ *//' | cut -f1 -d' ' `  # trim prefix

  if [[ "$PREV_HISTNUM" -ne "$HISTNUM" ]]; then
      if [ ! -z "$PREV_DIRECTORY" ]; then
          DIR="$PREV_DIRECTORY"
          echo "$DIR $CMD" >> ~/.bash_history_metadata
      fi
  fi

  export PREV_DIRECTORY="$PWD"
  export PREV_HISTNUM="$HISTNUM"
}

# TODO: last_command function


PROMPT_COMMAND="prompt_command"

# view bash history for this directory
function dir-history {
    cat ~/.bash_history_metadata |grep "^$PWD "
}

function dir-history-list {
    dir-history | linepy 'print re.sub("^\\S+\\s+\\S+\\s+\\S+\\s+\\S+ ", "", line)'
}

# note: this is not an accurate count because store succesive repeats of an
# identical command.
function dir-history-common {
    dir-history-list |freq
}
