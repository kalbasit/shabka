// Plugin that adds StumbleUpon commands and key mappings to Vimperator
//
// @version 0.1.1
// @author Karl Möller
//
// Tested with Vimperator 1.2 and 2.0b1
// Tested with StumbleUpon 3.28
//
// Usage:
//
// :stu[mble], <A-Esc>, <M-Esc>, <A-`>, <M-`>
// Stumble - Show next page
//
// :tabstu[mble], :ts[tumble]
// Tab-Stumble - Show next page in a new tab
//
// :thumbup, :tu, :like, <A-F1>, <M-F1>, <A-1>, <M-1>
// Thumb this page up
//
// :thumbdown, :td, :dislike, <A-F2>, <M-F2>, <A-2>, <M-2>
// Thump this page down
//
// :nothumb, :nt, :unrate
// Remove rating for this page
//
// :rev[iew], <A-F3>, <M-F3>, <A-3>, <M-3>
// Show page review
//
// :tabrev[iew], :tr[eview]
// Show page review in a new tab
//
// :tag, <A-/>, <M-/>
// Tag this page
//
// :su[toolbar], :sutb, <C-F11>
// Toggle StumbleUpon toolbar
//
// :susignin, :sulogin
// Sign-in to StumbleUpon
//
// :susignout, :sulogout
// Sign-out from StumbleUpon

mappings.add([modes.NORMAL], ["<A-Esc>", "<M-Esc>"],
	"Show next page from StumbleUpon",
	function() { stumble(0); });

mappings.add([modes.NORMAL], ["<A-F1>", "<M-F1>"],
	"Add to favourites, show more like this \(StumbleUpon\)",
	function() { su_rate(1, 0, 0, 0); });

mappings.add([modes.NORMAL], ["<A-F2>", "<M-F2>"],
	"No more like this \(StumbleUpon\)",
	function() { su_rate(0, 0, 0, 0); });

mappings.add([modes.NORMAL], ["<A-F3>", "<M-F3>"],
	"Show StumbleUpon page review",
	function() { su_website_info(0,'', 0); });

mappings.add([modes.NORMAL], ["<A-/>", "<M-/>"],
	"Tag this page \(StumbleUpon\)",
	function() { su_handle_tag_command(true); });

mappings.add([modes.NORMAL], ["<C-F11>"],
	"Toggle StumbleUpon toolbar",
	function() { su_toggle_toolbar(); });

commands.addUserCommand(["stu[mble]"],
	"Show next page from StumbleUpon",
	function() { stumble(0); });

commands.addUserCommand(["tabstu[mble]", "ts[tumble]"],
	"Show next page from StumbleUpon in a new tab",
	function() { stumble(1); });

commands.addUserCommand(["thumbup", "tu", "like"],
	"Add to favourites, show more like this \(StumbleUpon\)",
	function() { su_rate(1, 0, 0, 0); });

commands.addUserCommand(["thumbdown", "td", "dislike"],
	"No more like this \(StumbleUpon\)",
	function() { su_rate(0, 0, 0, 0); });

commands.addUserCommand(["nothumb", "nt", "unrate"],
	"Remove StumbleUpon rating",
	function() { su_unrate(); });

commands.addUserCommand(["rev[iew]"],
	"Show StumbleUpon page review",
	function() { su_website_info(0,'', 0); });

commands.addUserCommand(["tabrev[iew]", "tr[eview]"],
	"Show StumbleUpon page review in a new tab",
	function() { su_website_info(1,'', 0); });

commands.addUserCommand(["tag"],
	"Tag this page \(StumbleUpon\)",
	function() { su_handle_tag_command(true); });

commands.addUserCommand(["su[toolbar]", "sutb"],
	"Toggle StumbleUpon toolbar",
	function() { su_toggle_toolbar(); });

commands.addUserCommand(["susignin", "sulogin"],
	"Sign-in to StumbleUpon",
	function() { su_show_signin_dialog(); });

commands.addUserCommand(["susignout", "sulogout"],
	"Sign-out from StumbleUpon",
	function() { su_handle_logout(false); });
