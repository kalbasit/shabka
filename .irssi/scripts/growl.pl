# == WHAT
# Simple script for growl notifications in irssi
#
# == WHO
# Nate Murray 2008
# 
# == CONFIG
#   /SET growl_icon [filename]
#   /SET growl_on_regex [regex]
#   /SET growl_channel_regex [regex]
#
# == EXAMPLES
#
#   growl on mynickname
#   /SET growl_on_regex mynickname
#
#   growl on everything:
#   /SET growl_on_regex .*
#
#   everything but jdewey
#   /SET growl_on_regex (?=^(?:(?!jdewey).)*$).*
#
#   only growl things for mychannel1 and mychannel2
#   /SET growl_channel_regex (mychannel1|mychannel2)
# 
#   enable the icon
#   /SET growl_icon irssi-flame.png
# 
# == INSTALL
# Place these files in `~/.irssi/scripts/`. Put your growl icon in there too.
# /script load growl.pl
#
# == CONTRIBUTE
# If anyone has a better suggestion to DRY up the signals I would appreciate it. 
# 
# http://gist.github.com/6206
# or 
# git clone git://gist.github.com/6206.git gist-6206

use strict;
use Irssi;
use vars qw($VERSION %IRSSI);
# use Config;

# Dev. info ^_^
$VERSION = "0.0";
%IRSSI = (
	authors     => "Nate Murray",
	contact     => "nate\@natemurray.com",
	name        => "Growl",
	description => "Simple script that will growlnotify the messages",
	license     => "GPL",
	url         => "http://www.xcombinator.com",
	changed     => "Mon Sep 22 11:55:07 PDT 2008"
);

# All the works
sub do_growl {
	my ($server, $title, $data) = @_;
	my $icon = growl_locate_icon(Irssi::settings_get_str('growl_icon'));
    $data =~ s/["';]//g;
    if ($server->{usermode_away}) {
      system("growlnotify --sticky --image '$icon' -m '$data' -t '$title' >> /dev/null 2>&1");
    } else {
      system("growlnotify --image '$icon' -m '$data' -t '$title' >> /dev/null 2>&1");
    }
    return 1
}

sub growl_it {
	my ($server, $title, $data, $channel, $nick) = @_;

    my $filter = Irssi::settings_get_str('growl_on_regex');
    my $channel_filter = Irssi::settings_get_str('growl_channel_regex');
    my $growl_on_nick = Irssi::settings_get_str('growl_on_nick');

    my $current_nick = $server->{nick};
		if($filter) {
			return 0 if $data !~ /$filter/;
		}
		if($channel_filter && $server->ischannel($channel)) {
			return 0 if $channel !~ /$channel_filter/;
		}

    $title = $title . " " . $channel;
    do_growl($server, $title, $data);
}

# All the works
sub growl_message {
	my ($server, $data, $nick, $mask, $target) = @_;
    growl_it($server, $nick, $data, $target, $nick);
	Irssi::signal_continue($server, $data, $nick, $mask, $target);
}

sub growl_join {
	my ($server, $channel, $nick, $address) = @_;
    growl_it($server, "Join", "$nick has joined", $channel, $nick);
	Irssi::signal_continue($server, $channel, $nick, $address);
}

sub growl_part {
	my ($server, $channel, $nick, $address) = @_;
    growl_it($server, "Part", "$nick has parted", $channel, $nick);
	Irssi::signal_continue($server, $channel, $nick, $address);
}

sub growl_quit {
	my ($server, $nick, $address, $reason) = @_;
    growl_it($server, "Quit", "$nick has quit: $reason", $server, $nick);
	Irssi::signal_continue($server, $nick, $address, $reason);
}

sub growl_invite {
	my ($server, $channel, $nick, $address) = @_;
    growl_it($server, "Invite", "$nick has invited you on $channel", $channel, $nick);
	Irssi::signal_continue($server, $channel, $address);
}

sub growl_topic {
	my ($server, $channel, $topic, $nick, $address) = @_;
    growl_it($server, "Topic: $topic", "$nick has changed the topic to $topic on $channel", $channel, $nick);
	Irssi::signal_continue($server, $channel, $topic, $nick, $address);
}

sub growl_privmsg {
	# $server = server record where the message came
	# $data = the raw data received from server, with PRIVMSGs it is:
	#         "target :text" where target is either your nick or #channel
	# $nick = the nick who sent the message
	# $host = host of the nick who sent the message
	my ($server, $data, $nick, $host) = @_;
    my ($target, $text) = split(/ :/, $data, 2);
    # growl_it($server, $nick, $data, $target, $nick); # actually, don't do this.
	Irssi::signal_continue($server, $data, $nick, $host);
}

sub growl_locate_icon {
	# $file = the name of the icon file to look for
	my ($file) = @_;
	if (-e "$file") {
		return "$file";
	}
	foreach (@INC) {
		if (-e "$_/$file") {
			return "$_/$file";
		}
	}
}

# Hook me up
Irssi::settings_add_str('misc', 'growl_icon', 'irssi-flame.png');
Irssi::settings_add_str('misc', 'growl_on_regex', 0);      # false
Irssi::settings_add_str('misc', 'growl_channel_regex', 0); # false
Irssi::settings_add_str('misc', 'growl_on_nick', 1);       # true
Irssi::signal_add('message public', 'growl_message');
Irssi::signal_add('message private', 'growl_message');
Irssi::signal_add('message own_public', 'growl_message');
Irssi::signal_add('message own_private', 'growl_message');
Irssi::signal_add('message join', 'growl_join');
Irssi::signal_add('message part', 'growl_part');
Irssi::signal_add('message quit', 'growl_quit');
Irssi::signal_add('message invite', 'growl_invite');
Irssi::signal_add('message topic', 'growl_topic');
Irssi::signal_add('event privmsg', 'growl_privmsg');