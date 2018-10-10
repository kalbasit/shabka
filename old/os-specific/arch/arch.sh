#!/usr/bin/env bash
set -euo pipefail

# make sure pacman is colorful
if grep -q '^#Color' /etc/pacman.conf; then
	sudo sed -e 's:^#Color:Color:g' -i /etc/pacman.conf
fi

# NOTE: This is needed because Termite could not find the xterm-termite terminfo
# See https://github.com/NixOS/nixpkgs/issues/19785
if [[ ! -L "${HOME}/.terminfo" ]]; then
	echo ">> linking terminfo to allow terminals to find installed terminfo"
	ln -sf .nix-profile/share/terminfo "${HOME}/.terminfo"
fi

while read line; do
	if [[ "$(pacman -Qi "${line}" | wc -l)" -eq 0 ]]; then
		echo ">> installing ${line} in the OS"
		yay -S --nocleanmenu --nodiffmenu --noeditmenu --noupgrademenu --noconfirm "${line}"
	fi
done < <(grep -v '^#\|^$' arch-packages)

# install the nasreddine certificate
if [[ ! -f /etc/ca-certificates/trust-source/anchors/nasreddine.crt ]]; then
	echo ">> installing the Nasreddine cert"
	curl -LO http://nasreddine.com/ca.crt
	sudo mv ca.crt /etc/ca-certificates/trust-source/anchors/nasreddine.crt
	sudo trust extract-compat
fi

if [[ -f ~/.charles/ca/charles-proxy-ssl-proxying-certificate.pem ]]; then
	if [[ ! -f /etc/ca-certificates/trust-source/anchors/charles.crt ]] || [[ "$(md5sum ~/.charles/ca/charles-proxy-ssl-proxying-certificate.pem | awk '{print $1}')" != "$(md5sum /etc/ca-certificates/trust-source/anchors/charles.crt | awk '{print $1}')" ]]; then
		echo ">> installing the Charles cert"
		sudo cp ~/.charles/ca/charles-proxy-ssl-proxying-certificate.pem /etc/ca-certificates/trust-source/anchors/charles.crt
		sudo trust extract-compat
	fi
fi

# make sure the keyboard is set to Colemak
if ! localectl status | grep -q 'VC Keymap: colemak'; then
	echo ">> setting up the keymap on the Console"
	sudo localectl set-keymap --no-convert colemak
fi
if ! localectl status | grep -q 'X11 Variant: colemak'; then
	echo ">> setting up the keymap on X11"
	sudo localectl set-x11-keymap --no-convert us pc104 colemak 'ctrl:nocaps'
fi

if [[ ! -f /etc/systemd/system/kbdrate.service ]]; then
	echo ">> creating /etc/systemd/system/kbdrate.service"
	cat <<-EOF | sudo tee /etc/systemd/system/kbdrate.service
	[Unit]
	Description=Keyboard repeat rate in tty.

	[Service]
	Type=oneshot
	RemainAfterExit=yes
	StandardInput=tty
	StandardOutput=tty
	ExecStart=/usr/bin/kbdrate -s -d 300 -r 30

	[Install]
	WantedBy=multi-user.target
	EOF

	sudo systemctl enable --now kbdrate.service
fi

# find the chromium binary that needs to be setsuid
for derivation in $(nix-store -q --tree /nix/var/nix/profiles/per-user/kalbasit/profile | grep -E 'chromium-.*-sandbox' | sed -e 's:.*--/:/:g'); do
	if [[ "$(stat -c '%a' "${derivation}/bin/__chromium-suid-sandbox")" != "4755" ]]; then
		echo ">> setting setuid on ${derivation}/bin/__chromium-suid-sandbox"
		sudo chmod 4755 "${derivation}/bin/__chromium-suid-sandbox"
	fi
done

# enable natural Scrolling and tap-to-click on the touchpad
if [[ ! -f /etc/X11/xorg.conf.d/30-touchpad.conf ]]; then
	echo ">> installing the touchpad libinput config file"
	cat <<-EOF | sudo tee /etc/X11/xorg.conf.d/30-touchpad.conf
	Section "InputClass"
	  Identifier "MyTouchPad"
	  MatchIsTouchpad "on"
	  Driver "libinput"
	  Option "Tapping" "on"
	  Option "Natural Scrolling" "on"
	EndSection
	EOF
fi

# give anyone in the video group access to alter the brightness of the screen
if [[ ! -f /etc/udev/rules.d/70-backlight.rules ]]; then
	echo ">> installing the udev rules for the backlight"
	cat <<-EOF | sudo tee /etc/udev/rules.d/70-backlight.rules
	ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="/bin/chgrp video /sys/class/backlight/%k/brightness"
	ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="/bin/chmod g+w /sys/class/backlight/%k/brightness"
	EOF
fi

# tell keybase not to use a pinentry
if [[ ! -f "${HOME}/.zsh/rc.d/keybase-no-pinentry.zsh" ]]; then
	mkdir -p "${HOME}/.zsh/rc.d"
	echo "alias keybase='keybase --pinentry=none'" >> "${HOME}/.zsh/rc.d/keybase-no-pinentry.zsh"
fi
