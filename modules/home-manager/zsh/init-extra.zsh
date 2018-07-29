#####################################################################
# options
#####################################################################

if [[ -o interactive ]]; then
	# If a completion is performed with the cursor within a word, and a full
	# completion is inserted, the cursor is moved to the end of the word. That is,
	# the cursor is moved to the end of the word if either a single match is
	# inserted or menu completion is performed.
	setopt alwaystoend

	# If a command is issued that can't be executed as a normal command, and the
	# command is the name of a directory, perform the cd command to that directory.
	# This option is only applicable if the option SHIN_STDIN is set, i.e. if
	# commands are being read from standard input. The option is designed for
	# interactive use; it is recommended that cd be used explicitly in scripts to
	# avoid ambiguity.
	setopt autocd

	# Automatically use menu completion after the second consecutive request for
	# completion, for example by pressing the tab key repeatedly. This option is
	# overridden by MENU_COMPLETE.
	setopt automenu

	# Any parameter that is set to the absolute name of a directory immediately
	# becomes a name for that directory, that will be used by the ‘%~’ and related
	# prompt sequences, and will be available when completion is performed on a
	# word starting with ‘~’. (Otherwise, the parameter must be used in the form
	# ‘~param’ first.).
	setopt autonamedirs

	# Make cd push the old directory onto the directory stack.
	setopt autopushd

	# If this is set, zsh sessions will append their history list to the history
	# file, rather than replace it. Thus, multiple parallel zsh sessions will all
	# have the new entries from their history lists added to the history file, in
	# the order that they exit. The file will still be periodically re-written to
	# trim it when the number of lines grows 20% beyond the value specified by
	# $SAVEHIST (see also the HIST_SAVE_BY_COPY option).
	setopt appendhistory

	# If the argument to a cd command (or an implied cd with the AUTO_CD option
	# set) is not a directory, and does not begin with a slash, try to expand the
	# expression as if it were preceded by a ‘~’ (see Filename Expansion).
	setopt cdablevars

	# If unset, the cursor is set to the end of the word if completion is started.
	# Otherwise it stays there and completion is done from both ends.
	setopt completeinword

	# Save each command’s beginning timestamp (in seconds since the epoch) and the
	# duration (in seconds) to the history file. The format of this prefixed data
	# is:
	#   : <beginning time>:<elapsed seconds>;<command>
	setopt extendedhistory

	# If this option is unset, output flow control via start/stop characters
	# (usually assigned to ^S/^Q) is disabled in the shell’s editor.
	setopt noflowcontrol

	# If the internal history needs to be trimmed to add the current command line,
	# setting this option will cause the oldest history event that has a duplicate
	# to be lost before losing a unique event from the list. You should be sure to
	# set the value of HISTSIZE to a larger number than SAVEHIST in order to give
	# you some room for the duplicated events, otherwise this option will behave
	# just like HIST_IGNORE_ALL_DUPS once the history fills up with unique events.
	setopt histexpiredupsfirst

	# Do not enter command lines into the history list if they are duplicates of
	# the previous event.
	setopt histignoredups

	# Remove command lines from the history list when the first character on the
	# line is a space, or when one of the expanded aliases contains a leading
	# space. Only normal aliases (not global or suffix aliases) have this
	# behaviour. Note that the command lingers in the internal history until the
	# next command is entered before it vanishes, allowing you to briefly reuse or
	# edit the line. If you want to make it vanish right away without entering
	# another command, type a space and press return.
	setopt histignorespace

	# Whenever the user enters a line with history expansion, don't execute the
	# line directly; instead, perform history expansion and reload the line into
	# the editing buffer.
	setopt histverify

	# This options works like APPEND_HISTORY except that new history lines are
	# added to the $HISTFILE incrementally (as soon as they are entered), rather
	# than waiting until the shell exits. The file will still be periodically
	# re-written to trim it when the number of lines grows 20% beyond the value
	# specified by $SAVEHIST (see also the HIST_SAVE_BY_COPY option).
	setopt incappendhistory

	# Allow comments even in interactive shells.
	setopt interactivecomments

	# This is a login shell. If this option is not explicitly set, the shell
	# becomes a login shell if the first character of the argv[0] passed to the
	# shell is a ‘-’.
	setopt login

	# List jobs in the long format by default.
	setopt longlistjobs

	# Perform implicit tees or cats when multiple redirections are attempted (see
	# Redirection).
	setopt multios

	# On an ambiguous completion, instead of listing possibilities or beeping,
	# insert the first match immediately. Then when completion is requested again,
	# remove the first match and insert the second match, etc. When there are no
	# more matches, go back to the first one again. reverse-menu-complete may be
	# used to loop through the list in the other direction. This option overrides
	# AUTO_MENU.
	setopt nomenucomplete

	# If set, parameter expansion, command substitution and arithmetic expansion
	# are performed in prompts. Substitutions within prompts do not affect the
	# command status.
	setopt promptsubst

	# Don't push multiple copies of the same directory onto the directory stack.
	setopt pushdignoredups

	# Exchanges the meanings of ‘+’ and ‘-’ when used with a number to specify a
	# directory in the stack.
	setopt pushdminus

	# Do not print the directory stack after pushd or popd.
	setopt pushdsilent

	# Have pushd with no arguments act like ‘pushd $HOME’.
	setopt pushdtohome

	# This option both imports new commands from the history file, and also causes
	# your typed commands to be appended to the history file (the latter is like
	# specifying INC_APPEND_HISTORY, which should be turned off if this option is
	# in effect). The history lines are also output with timestamps ala
	# EXTENDED_HISTORY (which makes it easier to find the spot where we left off
	# reading the file after it gets re-written).
	#
	# By default, history movement commands visit the imported lines as well as the
	# local lines, but you can toggle this on and off with the set-local-history
	# zle binding. It is also possible to create a zle widget that will make some
	# commands ignore imported commands, and some include them.
	#
	# If you find that you want more control over when commands get imported, you
	# may wish to turn SHARE_HISTORY off, INC_APPEND_HISTORY or
	# INC_APPEND_HISTORY_TIME (see above) on, and then manually import commands
	# whenever you need them using ‘fc -RI’.
	setopt sharehistory

	# Use the zsh line editor. Set by default in interactive shells connected to a
	# terminal.
	setopt zle
fi

#####################################################################
# functions
#####################################################################

# TODO: move all functions
# Use a map, see https://github.com/rycee/home-manager/blob/30cba446f2c2e04db5a5001aaf606a4a5563e1de/modules/programs/zsh.nix#L371
#source "@out_dir@/zsh/functions.zsh"
