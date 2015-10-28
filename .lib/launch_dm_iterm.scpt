tell application "iTerm"
	activate

	-- make a new terminal
	set dm_term to (make new terminal)

	-- talk to the new terminal
	tell dm_term
		repeat with sess in {"dmx-api", "dmx-bridge", "dmx-inventory", "dmx-eve", "dmx-console", "squad-ads","go-libs","go-liverail"}
			launch session "Default Session"
			tell the last session
				set my_command to ("cd ~/code/src/github.com/dailymotion/" & sess & "; machdev; dm; tmx")
				write text my_command
			end tell
		end repeat

		select (the first session)
	end tell
end tell
