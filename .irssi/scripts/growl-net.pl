#!/usr/bin/env perl -w
#
# This is a simple irssi script to send out Growl notifications ovet the network using
# Net::Growl. Currently, it sends notifications when your name is
# highlighted, and when you receive private messages.
# Based on the original growl script by Nelson Elhage and Toby Peterson.

use strict;
use vars qw($VERSION %IRSSI $growl);

use Irssi;
use Growl::GNTP;
use IO::Socket::PortState qw(check_ports);

$VERSION = '0.2';
%IRSSI = (
	authors		=>	'Paul Traylor (gntp version), '.
					'Andrew Berry, ' .
					'Alex Mason, Jason Adams (based on the growl.pl script from Growl.info by Nelson Elhage and Toby Peterson)',
	contact		=>	'http://github.com/kfdm/irssi-growl',
	name		=>	'growl-net',
	description	=>	'Sends out Growl notifications over the netwotk or internet for Irssi. '.
					'Requires Growl::GNTP',
	license		=>	'BSD',
	url			=>	'http://github.com/kfdm/irssi-growl (gntp version), '.
					'http://axman6.homeip.net/blog/growl-net-irssi-script-its-back.html (udp version),  '.
					'http://growl.info/',
);

# Notification Settings
Irssi::settings_add_bool($IRSSI{'name'}, 'growl_show_privmsg', 1);
Irssi::settings_add_bool($IRSSI{'name'}, 'growl_show_hilight', 1);
Irssi::settings_add_bool($IRSSI{'name'}, 'growl_show_notify', 1);
Irssi::settings_add_bool($IRSSI{'name'}, 'growl_show_topic', 1);
Irssi::settings_add_bool($IRSSI{'name'}, 'growl_auto_register', 0);
# Network Settings
Irssi::settings_add_str($IRSSI{'name'}, 'growl_net_pass', 'password');
Irssi::settings_add_str($IRSSI{'name'}, 'growl_net_client', 'localhost');
Irssi::settings_add_str($IRSSI{'name'}, 'growl_net_port', '23053');
Irssi::settings_add_str($IRSSI{'name'}, 'growl_net_server', 'local');
Irssi::settings_add_str($IRSSI{'name'}, 'growl_net_icon', '');
# Sticky Settings
Irssi::settings_add_bool($IRSSI{'name'}, 'growl_net_sticky', 0);
Irssi::settings_add_bool($IRSSI{'name'}, 'growl_net_sticky_away', 0);

sub cmd_help {
	Irssi::print('Growl-net can be configured with these settings:');

	Irssi::print('%WNotification Settings%n');
	Irssi::print('  %ygrowl_show_privmsg%n :    Notify about private messages.');
	Irssi::print('  %ygrowl_show_hilight%n :    Notify when your name is hilighted.');
	Irssi::print('  %ygrowl_show_topic%n :      Notify about topic changes.');
	Irssi::print('  %ygrowl_show_notify%n :     Notify when someone on your away list joins or leaves.');

	Irssi::print('%WNetwork Settings%n');
	Irssi::print('  %ygrowl_net_client%n :      Set to the hostname you want to recieve notifications on.');
	Irssi::print('    %R>>>> (computer.local for a Mac network. Your \'localhost\').'); 
	Irssi::print('  %ygrowl_net_port%n :        Set to the port you want to recieve notifications on.');
	Irssi::print('  %ygrowl_net_server%n :      Set to the name you want to give the machine irssi is running on. (remote)');
	Irssi::print('  %ygrowl_net_pass%n :        Set to your destination\'s Growl password. (Your machine)');
	Irssi::print('  %ygrowl_auto_register%n :   Automatically send gntp registration on script load');

	Irssi::print('%WSticky Settings%n');
	Irssi::print('  %ygrowl_net_sticky%n :      Whether growls are sticky or not (ON/OFF/TOGGLE)');
	Irssi::print('  %ygrowl_net_sticky_away%n : Sets growls to sticky when away (ON/OFF/TOGGLE)');
}

sub cmd_growl_net_test {
	my $GrowlHost	= Irssi::settings_get_str('growl_net_client');
	my $AppName		= Irssi::settings_get_str('growl_net_server');
	my $GrowlIcon	= Irssi::settings_get_str('growl_net_icon');
	
	my $Sticky = set_sticky();
	
	growl_notify(
		Event => "Private Message",
		Title => "Test:",
		Message => "This is a test.\n AppName = $AppName \n GrowlHost = $GrowlHost \n Sticky = $Sticky",
		Sticky => "$Sticky",
		Priority => 0,
	);
} 

sub sig_message_private ($$$$) {
	return unless Irssi::settings_get_bool('growl_show_privmsg');

	my ($server, $data, $nick, $address) = @_;
	
	my $Sticky = set_sticky();
	
	growl_notify(
		Event => "Private Message",
		Title => "$nick",
		Message => "$data",
		Priority => 0,
		Sticky => "$Sticky",
	);
}

sub sig_print_text ($$$) {
	return unless Irssi::settings_get_bool('growl_show_hilight');

	my ($dest, $text, $stripped) = @_;
	
	my $Sticky = set_sticky();
	
	if ($dest->{level} & MSGLEVEL_HILIGHT) {
		
		growl_notify(
			Event => "Hilight",
			Title => "$dest->{target}",
			Message => "$stripped",
			Priority => 0,
			Sticky => "$Sticky",
		);
	}
}

sub sig_notify_joined ($$$$$$) {
	return unless Irssi::settings_get_bool('growl_show_notify');
	
	my ($server, $nick, $user, $host, $realname, $away) = @_;
	
	my $Sticky = set_sticky();
	
	growl_notify(
		Event => "Join",
		Title => "$realname" || "$nick",
		Message => "<$nick!$user\@$host>\nHas joined $server->{chatnet}",
		Priority => 0,
		Sticky => "$Sticky",
	);
}

sub sig_notify_left ($$$$$$) {
	return unless Irssi::settings_get_bool('growl_show_notify');
	
	my ($server, $nick, $user, $host, $realname, $away) = @_;
	
	my $Sticky = set_sticky();
	
	growl_notify(
		Event => "Part",
		Title => "$realname" || "$nick",
		Message => "<$nick!$user\@$host>\nHas left $server->{chatnet}",
		Priority => 0,
		Sticky => "$Sticky",
	);
}

#"message topic", SERVER_REC, char *channel, char *topic, char *nick, char *address
sub sig_message_topic {
	return unless Irssi::settings_get_bool('growl_show_topic');
	my($server, $channel, $topic, $nick, $address) = @_;
	
	my $Sticky = set_sticky();
	
	growl_notify(
		Event => "Topic",
		Title => "$channel",
		Message => "Topic for $channel: $topic",
		Priority => 0,
		Sticky => "$Sticky",
	);
}

sub set_sticky {
	my ($server);
	$server = Irssi::active_server();
	
	if (Irssi::settings_get_bool('growl_net_sticky_away')) {
		if (!$server->{usermode_away}) {
			return 0;
		} else {
			return 1;
		}
			# $Sticky = Server{'usermode_away'};
		} else {
		return Irssi::settings_get_bool('growl_net_sticky');
	}
}

sub setup {
	my $GrowlHost	= Irssi::settings_get_str('growl_net_client');
	my $GrowlPort	= Irssi::settings_get_str('growl_net_port');
	my $GrowlPass	= Irssi::settings_get_str('growl_net_pass');
	my $AppName		= Irssi::settings_get_str('growl_net_server');
	my $GrowlIcon	= Irssi::settings_get_str('growl_net_icon');

	Irssi::print("%G>>%n Registering to send messages to $GrowlHost:$GrowlPort");
	$growl = Growl::GNTP->new(
		AppName => $AppName,
		PeerHost => $GrowlHost,
		PeerPort => $GrowlPort,
		Password => $GrowlPass,
		AppIcon => $GrowlIcon,
	);
}

sub cmd_register {
	$growl->register([
		{ Name => "Private Message", },
		{ Name => "Hilight", },
		{ Name => "Join", },
		{ Name => "Part", },
		{ Name => "Topic", },
	]);
}

sub check_connection {
	my $GrowlHost	= Irssi::settings_get_str('growl_net_client');
	my $GrowlPort	= Irssi::settings_get_str('growl_net_port');
	my %check = (
		tcp  => {
			$GrowlPort => {
				name => 'Growl',
			},
		},
	);

	check_ports($GrowlHost, 5, \%check);
	return $check{tcp}{$GrowlPort}{open};
}

sub growl_notify {
	if (!check_connection()) {
		Irssi::print("The Growl server is not responding.");
		return;
	}

	my (%args) = @_;

	$growl->notify(%args);
}

Irssi::command_bind('growl-net',      'cmd_help');
Irssi::command_bind('gn-test',        'cmd_growl_net_test');
Irssi::command_bind('growl-register', 'cmd_register');

Irssi::signal_add_last('message private',   'sig_message_private');
Irssi::signal_add_last('print text',        'sig_print_text');
Irssi::signal_add_last('notifylist joined', 'sig_notify_joined');
Irssi::signal_add_last('notifylist left',   'sig_notify_left');
Irssi::signal_add_last('message topic',     'sig_message_topic');

setup();
if (Irssi::settings_get_bool('growl_auto_register')) {
	cmd_register();
}
Irssi::print('%G>>%n '.$IRSSI{name}.' '.$VERSION.' loaded (/growl-net for help. /gn-test to test.)');
