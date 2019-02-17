{
  system.activationScripts.postActivation.text = ''
    # TODO: Rewrite as much of these as possible using nix-darwin

    # From https://github.com/mathiasbynens/dotfiles/blob/master/.macos taken
    # at commit e72d1060f3df8c157f93af52ea59508dae36ef50.

    ###############################################################################
    # General UI/UX                                                               #
    ###############################################################################

    # Set standby delay to 24 hours (default is 1 hour)
    sudo pmset -a standbydelay 86400

    # Disable the sound effects on boot
    sudo nvram SystemAudioVolume=" "

    # Disable transparency in the menu bar and elsewhere on Yosemite
    defaults write com.apple.universalaccess reduceTransparency -bool true

    # Set highlight color to green
    defaults write NSGlobalDomain AppleHighlightColor -string "0.764700 0.976500 0.568600"

    # Automatically quit printer app once the print jobs complete
    defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true

    # Remove duplicates in the “Open With” menu (also see `lscleanup` alias)
    /System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user

    # Disable Resume system-wide
    defaults write com.apple.systempreferences NSQuitAlwaysKeepsWindows -bool false

    # Set Help Viewer windows to non-floating mode
    defaults write com.apple.helpviewer DevMode -bool true

    # Reveal IP address, hostname, OS version, etc. when clicking the clock
    # in the login window
    sudo defaults write /Library/Preferences/com.apple.loginwindow AdminHostInfo HostName

    # Restart automatically if the computer freezes
    sudo systemsetup -setrestartfreeze on

    # Never go into computer sleep mode
    sudo systemsetup -setcomputersleep Off > /dev/null

    # Disable Notification Center and remove the menu bar icon
    launchctl unload -w /System/Library/LaunchAgents/com.apple.notificationcenterui.plist 2> /dev/null

    ###############################################################################
    # SSD-specific tweaks                                                         #
    ###############################################################################

    # Disable hibernation (speeds up entering sleep mode)
    sudo pmset -a hibernatemode 0

    ###############################################################################
    # Trackpad, mouse, keyboard, Bluetooth accessories, and input                 #
    ###############################################################################

    # Increase sound quality for Bluetooth headphones/headsets
    defaults write com.apple.BluetoothAudioAgent "Apple Bitpool Min (editable)" -int 40

    # Use scroll gesture with the Ctrl (^) modifier key to zoom
    defaults write com.apple.universalaccess closeViewScrollWheelToggle -bool true
    defaults write com.apple.universalaccess HIDScrollZoomModifierMask -int 262144
    # Follow the keyboard focus while zoomed in
    defaults write com.apple.universalaccess closeViewZoomFollowsFocus -bool true

    # Show language menu in the top right corner of the boot screen
    sudo defaults write /Library/Preferences/com.apple.loginwindow showInputMenu -bool true

    # Set language and text formats
    # Note: if you’re in the US, replace `EUR` with `USD`, `Centimeters` with
    # `Inches`, `en_GB` with `en_US`, and `true` with `false`.
    defaults write NSGlobalDomain AppleLanguages -array "en"
    defaults write NSGlobalDomain AppleLocale -string "en_US@currency=US"
    defaults write NSGlobalDomain AppleMeasurementUnits -string "Centimeters"
    defaults write NSGlobalDomain AppleMetricUnits -bool true

    # Stop iTunes from responding to the keyboard media keys
    launchctl unload -w /System/Library/LaunchAgents/com.apple.rcd.plist 2>/dev/null

    ###############################################################################
    # Screen                                                                      #
    ###############################################################################

    # Require password immediately after sleep or screen saver begins
    defaults write com.apple.screensaver askForPassword -int 1
    defaults write com.apple.screensaver askForPasswordDelay -int 0

    # Save screenshots in PNG format (other options: BMP, GIF, JPG, PDF, TIFF)
    defaults write com.apple.screencapture type -string "png"

    # Disable shadow in screenshots
    defaults write com.apple.screencapture disable-shadow -bool true

    # Enable HiDPI display modes (requires restart)
    sudo defaults write /Library/Preferences/com.apple.windowserver DisplayResolutionEnabled -bool true

    ###############################################################################
    # Finder                                                                      #
    ###############################################################################

    # Finder: disable window animations and Get Info animations
    defaults write com.apple.finder DisableAllAnimations -bool true

    # Set Desktop as the default location for new Finder windows
    # For other paths, use `PfLo` and `file:///full/path/here/`
    defaults write com.apple.finder NewWindowTarget -string "PfDe"
    defaults write com.apple.finder NewWindowTargetPath -string "file://$HOME/Desktop/"

    # Show icons for hard drives, servers, and removable media on the desktop
    defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool true
    defaults write com.apple.finder ShowHardDrivesOnDesktop -bool true
    defaults write com.apple.finder ShowMountedServersOnDesktop -bool true
    defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool true

    # Finder: show status bar
    defaults write com.apple.finder ShowStatusBar -bool true

    # Finder: show path bar
    defaults write com.apple.finder ShowPathbar -bool true

    # Keep folders on top when sorting by name
    defaults write com.apple.finder _FXSortFoldersFirst -bool true

    # When performing a search, search the current folder by default
    defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

    # Avoid creating .DS_Store files on network or USB volumes
    defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
    defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true

    # Disable disk image verification
    defaults write com.apple.frameworks.diskimages skip-verify -bool true
    defaults write com.apple.frameworks.diskimages skip-verify-locked -bool true
    defaults write com.apple.frameworks.diskimages skip-verify-remote -bool true

    # Automatically open a new Finder window when a volume is mounted
    defaults write com.apple.frameworks.diskimages auto-open-ro-root -bool true
    defaults write com.apple.frameworks.diskimages auto-open-rw-root -bool true
    defaults write com.apple.finder OpenWindowForNewRemovableDisk -bool true

    # Show item info near icons on the desktop and in other icon views
    /usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:showItemInfo true" ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:showItemInfo true" ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:showItemInfo true" ~/Library/Preferences/com.apple.finder.plist

    # Show item info to the right of the icons on the desktop
    /usr/libexec/PlistBuddy -c "Set DesktopViewSettings:IconViewSettings:labelOnBottom false" ~/Library/Preferences/com.apple.finder.plist

    # Enable snap-to-grid for icons on the desktop and in other icon views
    /usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist

    # Increase grid spacing for icons on the desktop and in other icon views
    /usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:gridSpacing 100" ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:gridSpacing 100" ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:gridSpacing 100" ~/Library/Preferences/com.apple.finder.plist

    # Increase the size of icons on the desktop and in other icon views
    /usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:iconSize 80" ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:iconSize 80" ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:iconSize 80" ~/Library/Preferences/com.apple.finder.plist

    # Use list view in all Finder windows by default
    # Four-letter codes for the other view modes: `icnv`, `clmv`, `Flwv`
    defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"

    # Disable the warning before emptying the Trash
    defaults write com.apple.finder WarnOnEmptyTrash -bool false

    # Enable AirDrop over Ethernet and on unsupported Macs running Lion
    defaults write com.apple.NetworkBrowser BrowseAllInterfaces -bool true

    # Show the ~/Library folder
    chflags nohidden ~/Library

    # Show the /Volumes folder
    sudo chflags nohidden /Volumes

    # Expand the following File Info panes:
    # “General”, “Open with”, and “Sharing & Permissions”
    defaults write com.apple.finder FXInfoPanesExpanded -dict \
      General -bool true \
      OpenWith -bool true \
      Privileges -bool true

    ###############################################################################
    # Dock, Dashboard, and hot corners                                            #
    ###############################################################################

    # Disable Dashboard
    defaults write com.apple.dashboard mcx-disabled -bool true

    # Hot corners
    # Possible values:
    #  0: no-op
    #  2: Mission Control
    #  3: Show application windows
    #  4: Desktop
    #  5: Start screen saver
    #  6: Disable screen saver
    #  7: Dashboard
    # 10: Put display to sleep
    # 11: Launchpad
    # 12: Notification Center
    # Top left screen corner → Mission Control
    defaults write com.apple.dock wvous-tl-corner -int 2
    defaults write com.apple.dock wvous-tl-modifier -int 0
    # Top right screen corner → Desktop
    defaults write com.apple.dock wvous-tr-corner -int 4
    defaults write com.apple.dock wvous-tr-modifier -int 0
    # Bottom left screen corner → Start screen saver
    defaults write com.apple.dock wvous-bl-corner -int 5
    defaults write com.apple.dock wvous-bl-modifier -int 0
  '';

  # Enable “natural” (Lion-style) scrolling (mths.be/macos disables it)
  system.defaults.NSGlobalDomain."com.apple.swipescrolldirection" = true;

  # Set sidebar icon size to medium
  system.defaults.NSGlobalDomain.NSTableViewDefaultSizeMode = 2;

  # Always show scrollbars
  system.defaults.NSGlobalDomain.AppleShowScrollBars = "Always";

  # Disable the over-the-top focus ring animation
  system.defaults.NSGlobalDomain.NSUseAnimatedFocusRing = false;

  # Increase window resize speed for Cocoa applications
  system.defaults.NSGlobalDomain.NSWindowResizeTime = "0.001";

  # Expand save panel by default
  system.defaults.NSGlobalDomain.NSNavPanelExpandedStateForSaveMode = true;
  system.defaults.NSGlobalDomain.NSNavPanelExpandedStateForSaveMode2 = true;

  # Expand print panel by default
  system.defaults.NSGlobalDomain.PMPrintingExpandedStateForPrint = true;
  system.defaults.NSGlobalDomain.PMPrintingExpandedStateForPrint2 = true;

  # Save to disk (not to iCloud) by default
  system.defaults.NSGlobalDomain.NSDocumentSaveNewDocumentsToCloud = false;

  # Disable the “Are you sure you want to open this application?” dialog
  system.defaults.LaunchServices.LSQuarantine = false;

  # Display ASCII control characters using caret notation in standard text views
  # Try e.g. `cd /tmp; unidecode "\x{0000}" > cc.txt; open -e cc.txt`
  system.defaults.NSGlobalDomain.NSTextShowsControlCharacters = true;

  # Disable automatic termination of inactive apps
  system.defaults.NSGlobalDomain.NSDisableAutomaticTermination = true;

  # Override the keyboard repeat rate to something more sensible.
  system.defaults.NSGlobalDomain.KeyRepeat = 2;
  system.defaults.NSGlobalDomain.InitialKeyRepeat = 15;

  # Disable automatic capitalization as it’s annoying when typing code
  system.defaults.NSGlobalDomain.NSAutomaticCapitalizationEnabled = false;

  # Disable smart dashes as they’re annoying when typing code
  system.defaults.NSGlobalDomain.NSAutomaticDashSubstitutionEnabled = false;

  # Disable automatic period substitution as it’s annoying when typing code
  system.defaults.NSGlobalDomain.NSAutomaticPeriodSubstitutionEnabled = false;

  # Disable smart quotes as they’re annoying when typing code
  system.defaults.NSGlobalDomain.NSAutomaticQuoteSubstitutionEnabled = false;

  # Disable auto-correct
  system.defaults.NSGlobalDomain.NSAutomaticSpellingCorrectionEnabled = false;

  # Remap CapsLock to Control
  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToControl = true;

  # Trackpad: enable tap to click
  system.defaults.trackpad.Clicking = true;

  # Trackpad: enable three-finger drag
  system.defaults.trackpad.TrackpadThreeFingerDrag = true;

  # Trackpad: enable right click on the bottom right of the trackpad
  system.defaults.trackpad.TrackpadRightClick = true;

  # Enable full keyboard access for all controls
  # (e.g. enable Tab in modal dialogs)
  system.defaults.NSGlobalDomain.AppleKeyboardUIMode = 3;

  # Disable press-and-hold for keys in favor of key repeat
  system.defaults.NSGlobalDomain.ApplePressAndHoldEnabled = false;

  # Save screenshots to the desktop
  system.defaults.screencapture.location = "$HOME/Desktop";

  # Enable subpixel font rendering on non-Apple LCDs
  # Reference: https://github.com/kevinSuttle/macOS-Defaults/issues/17#issuecomment-266633501
  system.defaults.NSGlobalDomain.AppleFontSmoothing = 1;

  # Finder: allow quitting via ⌘ + Q; doing so will also hide desktop icons
  system.defaults.finder.QuitMenuItem = true;

  # Finder: show all filename extensions
  system.defaults.NSGlobalDomain.AppleShowAllExtensions = true;

  # Display full POSIX path as Finder window title
  system.defaults.finder._FXShowPosixPathInTitle = true;

  # Disable the warning when changing a file extension
  system.defaults.finder.FXEnableExtensionChangeWarning = false;

  # Enable spring loading for directories
  system.defaults.NSGlobalDomain."com.apple.springing.enabled" = true;

  # Remove the spring loading delay for directories
  system.defaults.NSGlobalDomain."com.apple.springing.delay" = "0.0";

  # Enable highlight hover effect for the grid view of a stack (Dock)
  system.defaults.dock.mouse-over-hilite-stack = true;

  # Set the icon size of Dock items to 36 pixels
  system.defaults.dock.tilesize = 36;

  # Change minimize/maximize window effect
  system.defaults.dock.mineffect = "scale";

  # Minimize windows into their application’s icon
  system.defaults.dock.minimize-to-application = true;

  # Enable spring loading for all Dock items
  system.defaults.dock.enable-spring-load-actions-on-all-items = true;

  # Show indicator lights for open applications in the Dock
  system.defaults.dock.show-process-indicators = true;

  # Don’t animate opening applications from the Dock
  system.defaults.dock.launchanim = false;

  # Speed up Mission Control animations
  system.defaults.dock.expose-animation-duration = "0.1";

  # Don’t group windows by application in Mission Control
  # (i.e. use the old Exposé behavior instead)
  system.defaults.dock.expose-group-by-app = false;

  # Don’t show Dashboard as a Space
  system.defaults.dock.dashboard-in-overlay = true;

  # Don’t automatically rearrange Spaces based on most recent use
  system.defaults.dock.mru-spaces = false;

  # Remove the auto-hiding Dock delay
  system.defaults.dock.autohide-delay = "0";
  # Remove the animation when hiding/showing the Dock
  system.defaults.dock.autohide-time-modifier = "0";

  # Automatically hide and show the Dock
  system.defaults.dock.autohide = true;

  # Make Dock icons of hidden applications translucent
  system.defaults.dock.showhidden = true;

  # Don’t show recent applications in Dock
  system.defaults.dock.show-recents = false;
}
