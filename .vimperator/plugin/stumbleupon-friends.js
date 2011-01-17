// Plugin that adds access to stumbleupon friend capabilities
//
// For stumbleupon 2.0
//
// Usage:
//
// stu[mble]send <friend>
// Send the current tab to a friend
//
// Pressing <tab> while typing the friend's username will tabcomplete
//
commands.addUserCommand(["stumblesend", "susend"],
	"Send the current page to a friend \(StumbleUpon\)",
	function(friend) {
		su_sendto(friend);
	},
	{
		completer: function (context, args) {
			var friends = su_ds.selectAllRows("contact");
			var completions = [];
			for(var i = 0; i < friends.length; i++)
			{
				if(friends[i].mutual)
					completions.push([friends[i].nickname]);
			}
			context.title=["StumbleUpon Friends"];
			context.completions = completions;
		}
	});
