// vim:ft=javascript:foldmethod=marker:foldlevel=0:

//////////////////////////////////////
// Settings {{{

// align then hint to the right
settings.hintAlign = "left";

// hint characters for Colemak
Hints.characters = "arstneio";

// intercept all error pages so Surfing keys can work on them
settings.interceptedErrors = ["*"];

// turn off smoothscroll
settings.smoothScroll = false;

// show the current mode in the status bar
settings.showModeStatus = true;

// show the current proxy mode in the status bar
settings.showProxyInStatusBar = true;

// open new tabs to the right of the current one
settings.newTabPosition = "right";

// disable smart page boundry jump next/previous
settings.smartPageBoundary = false;

// make Google translate languages clickable
settings.clickableSelector = "*.jfk-button, *.goog-flat-menu-button";

// show only tabs from the current window
settings.omnibarTabsQuery = {currentWindow: true};

// END Settings }}}
//////////////////////////////////////

//////////////////////////////////////
// Colemak bindings {{{

// normal mode

// save the originals into an alias so there will be no dependency or order
// issue
map("_h",  "h");
map("_j",  "j");
map("_k",  "k");
map("_l",  "l");
map("_n",  "n");
map("_N",  "N");
map("_R",  "R");
map("_af", "af");
map("_v",  "v");
map("_V",  "V");
map("_yt", "yt");
map("_yy", "yy");
map("_yl", "yl");
map("_yf", "yf");
map("_S",  "S");
map("_D",  "D");
map("_I", "I");
map("<Alt-_p>", "<Alt-p>");
map("_go", "go");
map("_gt", "gt");
map("_gT", "gT");
map("_ya", "ya");
map("_yma", "yma");

// remove the one we backed up
unmap("h");
unmap("j");
unmap("k");
unmap("l");
unmap("n");
unmap("N");
unmap("R");
unmap("af");
unmap("v");
unmap("V");
unmap("yt");
unmap("yy");
unmap("yl");
unmap("yf");
unmap("S");
unmap("D");
unmap("I");
unmap("<Alt-p>");
unmap("go");
unmap("gt");
unmap("gT");
unmap("ya");
unmap("yma");

// map the Colemak bindings
map("n",  "_h");        // scroll left
map("e",  "_j");        // scroll down
map("i",  "_k");        // scroll up
map("o",  "_l");        // scroll right
map("k",  "_n");        // find next
map("K",  "_N");        // find previous
map("I",  "_R");        // go one tab right
map("F",  "_af");       // open hint in a new tab
map("a",  "_v");        // Toggle visual mode
map("A",  "_V");        // Restore visual mode
map("ct", "_yt");       // duplicate current page
map("cc", "_yy");       // copy current page URL
map("cl", "_yl");       // copy current page"s title
map("cf", "_yf");       // copy form data in JSON on current page
map("N",  "_S");        // go back in history
map("O",  "_D");        // go forward in history
map("v", "_I");         // open editor for input
map("gp", "<Alt-_p>");  // pin tab
map("gt", "_go");       // open a url in the current tab
map("cy", "_ya");       // copy the hint
map("cmy", "_yma");     // copy multiple hints

// finally remove what we have saved
unmap("_h");
unmap("_j");
unmap("_k");
unmap("_l");
unmap("_n");
unmap("_N");
unmap("_R");
unmap("_af");
unmap("_v");
unmap("_V");
unmap("_yt");
unmap("_yy");
unmap("_yl");
unmap("_yf");
unmap("_S");
unmap("_D");
unmap("_I");
unmap("<Alt-_p>");
unmap("_go");
unmap("_gt");
unmap("_gt");
unmap("_cy");
unmap("_cmy");

// visual mode

// save the originals into an alias so there will be no dependency or order
// issue
vmap("_h",  "h");
vmap("_j",  "j");
vmap("_k",  "k");
vmap("_l",  "l");

// remove the one we backed up
vunmap("h");
vunmap("j");
vunmap("k");
vunmap("l");

// map the Colemak bindings
vmap("n",  "_h");  // backward character
vmap("e",  "_j");  // forward line
vmap("i",  "_k");  // backward line
vmap("o",  "_l");  // forward character

// finally remove what we have saved
vunmap("_h");
vunmap("_j");
vunmap("_k");
vunmap("_l");

// END Colemak bindings }}}
//////////////////////////////////////

//////////////////////////////////////
// AceVim Colemak bindings {{{

// change the default layout of the AceVim keyboard
addVimMapKey(
	// left
	{
		keys: 'n',
		type: 'motion',
		motion: 'moveByCharacters',
		motionArgs: {
			forward: false
		}
	},

	// down
	{
		keys: 'e',
		type: 'motion',
		motion: 'moveByLines',
		motionArgs: {
			forward: true,
			linewise: true
		}
	},

	// up
	{
		keys: 'i',
		type: 'motion',
		motion: 'moveByLines',
		motionArgs: {
			forward: false,
			linewise: true
		}
	},

	// right
	{
		keys: 'o',
		type: 'motion',
		motion: 'moveByCharacters',
		motionArgs: {
			forward: true
		}
	},

	// movement by word
	{
		keys: 'y',
		type: 'motion',
		motion: 'moveByWords',
		motionArgs: {
			forward: true,
			wordEnd: false
		}
	},
	{
		keys: 'Y',
		type: 'motion',
		motion: 'moveByWords',
		motionArgs: {
			forward: true,
			wordEnd: false,
			bigWord: true
		}
	},
	{
		keys: 'u',
		type: 'motion',
		motion: 'moveByWords',
		motionArgs: {
			forward: true,
			wordEnd: true,
			inclusive: true
		}
	},
	{
		keys: 'U',
		type: 'motion',
		motion: 'moveByWords',
		motionArgs: {
			forward: true,
			wordEnd: true,
			bigWord: true,
			inclusive: true
		}
	},
	{
		keys: 'l',
		type: 'motion',
		motion: 'moveByWords',
		motionArgs: {
			forward: false,
			wordEnd: false
		}
	},
	{
		keys: 'L',
		type: 'motion',
		motion: 'moveByWords',
		motionArgs: {
			forward: false,
			wordEnd: false,
			bigWord: true
		}
	},

	// find next
	{
		keys: 'k',
		type: 'motion',
		motion: 'findNext',
		motionArgs: {
			forward: true,
			toJumplist: true
		}
	},

	// find previous
	{
		keys: 'K',
		type: 'motion',
		motion: 'findNext',
		motionArgs: {
			forward: false,
			toJumplist: true
		}
	},

	// copying
	{
		keys: 'c',
		type: 'operator',
		operator: 'yank'
	},
	{
		keys: 'C',
		type: 'operatorMotion',
		operator: 'yank',
		motion: 'moveToEol',
		motionArgs: {
			inclusive: true
		},
		context: 'normal'
	},
	{
		keys: 'C',
		type: 'operator',
		operator: 'yank',
		operatorArgs: {
			linewise: true
		},
		context: 'visual'
	},

	// paste
	{
		keys: 'v',
		type: 'action',
		action: 'paste',
		isEdit: true,
		actionArgs: {
			after: true,
			isEdit: true
		}
	},
	{
		keys: 'V',
		type: 'action',
		action: 'paste',
		isEdit: true,
		actionArgs: {
			after: false,
			isEdit: true
		}
	},

	// change
	{
		keys: 'w',
		type: 'operator',
		operator: 'change'
	},
	{
		keys: 'W',
		type: 'operatorMotion',
		operator: 'change',
		motion: 'moveToEol',
		motionArgs: {
			inclusive: true
		},
		context: 'normal'
	},
	{
		keys: 'W',
		type: 'operator',
		operator: 'change',
		operatorArgs: {
			linewise: true
		},
		context: 'visual'
	},

	// visual
	{
		keys: 'a',
		type: 'action',
		action: 'toggleVisualMode'
	},
	{
		keys: 'A',
		type: 'action',
		action: 'toggleVisualMode',
		actionArgs: {
			linewise: true
		}
	},

	// open new line
	{
		keys: 'h',
		type: 'action',
		action: 'newLineAndEnterInsertMode',
		isEdit: true,
		interlaceInsertRepeat: true,
		actionArgs: {
			after: true
		},
		context: 'normal'
	},
	{
		keys: 'H',
		type: 'action',
		action: 'newLineAndEnterInsertMode',
		isEdit: true,
		interlaceInsertRepeat: true,
		actionArgs: {
			after: false
		},
		context: 'normal'
	},

	// insert
	{
		keys: 's',
		type: 'action',
		action: 'enterInsertMode',
		isEdit: true,
		actionArgs: {
			insertAt: 'inplace'
		},
		context: 'normal'
	},
	{
		keys: 'S',
		type: 'action',
		action: 'enterInsertMode',
		isEdit: true,
		actionArgs: {
			insertAt: 'firstNonBlank'
		},
		context: 'normal'
	},
	{
		keys: 'S',
		type: 'action',
		action: 'enterInsertMode',
		isEdit: true,
		actionArgs: {
			insertAt: 'startOfSelectedArea'
		},
		context: 'visual'
	},
	{
		keys: 't',
		type: 'action',
		action: 'enterInsertMode',
		isEdit: true,
		actionArgs: {
			insertAt: 'charAfter'
		},
		context: 'normal'
	},
	{
		keys: 'T',
		type: 'action',
		action: 'enterInsertMode',
		isEdit: true,
		actionArgs: {
			insertAt: 'eol'
		},
		context: 'normal'
	},
	{
		keys: 'T',
		type: 'action',
		action: 'enterInsertMode',
		isEdit: true,
		actionArgs: {
			insertAt: 'endOfSelectedArea'
		},
		context: 'visual'
	},

	// undo/redo
	{
		keys: 'z',
		type: 'action',
		action: 'undo',
		context: 'normal'
	},
	{
		keys: 'Z',
		type: 'action',
		action: 'redo'
	}
);

// END Colemak bindings }}}
//////////////////////////////////////

//////////////////////////////////////
// Custom functions {{{

function copyLastElementInPath() {
	const locationParts = window.location.href.split("/");
	const lastElement = locationParts[locationParts.length-1].split("#")[0].split("?")[0];
	if (!lastElement) {
		Front.showBanner(`No last element was found.`);
		return;
	}
	Clipboard.write(lastElement);
	Front.showBanner(`Copied ${lastElement} to the clipboard.`);
}

// openAlternate opens the alternate page:
// - When on Github viewing a PR, open the build for this PR                       (DONE)
// - When on Github viewing a source file, open the test file                      (DONE)
// - When on Github viewing a test file, open the source file                      (DONE)
// - When on Github viewing anything else, go the Travis repo                      (DONE)
// - When on Travis viewing a build for a PR, go to Github PR                      (DONE)
// - When on Travis viewing a build for branch, go to Github with branch selected  (TODO)
// - When on Travis viewing the main build, go to Github repo                      (DONE)
//
// openAlternate requires the username and the api_token in order to work.  The
// username and the api_token must be stored in the chrome storage. To set them
// in the storage, execute the following in the chrome console on Surfingkey"s
// background page:
//   chrome.storage.sync.set({"github_api_username": "..."});
//   chrome.storage.sync.set({"github_api_token": "..."});
function openAlternate(newTab) {
	return () => {
		// start by splitting the path by slashes, ignoring the first slash to
		// prevent having the first element an empty string
		const pp = window.location.pathname.substring(1).split("/");

		// extract the anchor from the last element
		if (pp[pp.length - 1].includes("#")) {
			var tmp = pp[pp.length - 1].split("#");
			pp[pp.length - 1] = tmp[0];
			const anchor = tmp[1];
		}

		// extract any query params from the last element
		if (pp[pp.length - 1].includes("?")) {
			var tmp = pp[pp.length - 1].split("?");
			pp[pp.length - 1] = tmp[0];
			const query = tmp[1];
		}

		function openURL(url) {
			if (newTab === true) {
				window.open(url);
			} else {
				openURL(url);
			}
		}

		// isGithubPrivateRepo() returns true if the current page is a private repo
		function isGithubPrivateRepo() {
			return document.getElementsByClassName("label-private").length > 0;
		}

		// isGithubPR returns true if we are viewing a PR
		function isGithubPR() {
			return pp[2] === "pull";
		}

		// getGithubPullRequestURL() returns a promise that returns the URL of the PR (if
		// successful) or rejects it with a string error.
		function getGithubPullRequestURL(username, password) {
			return new Promise((resolve, reject) => {
				// compute the XHR URL
				const api_pulls_url = `https://api.github.com/repos${repoPath()}/pulls`;
				// find the branch name
				const commitBranches = window.document.getElementsByClassName("commit-branch");
				if (commitBranches.length === 1) {
					const commitBranch = commitBranches[0].title;
					// make an XHR Request to Github to find the URL
					const xhr = new XMLHttpRequest();
					xhr.open("GET", api_pulls_url, true);
					xhr.setRequestHeader("Authorization", `Basic ${btoa(username + ":" + password)}`);
					xhr.onreadystatechange = () => {
						if (xhr.readyState === 4) {
							if (xhr.status >= 200 && xhr.status < 299) {
								const pulls = JSON.parse(xhr.responseText);

								for (const pull of pulls) {
									if (pull.head.ref === commitBranch) {
										resolve(pull.html_url);
										return;
									}
								}

								reject(`did not find a pull request for branch ${commitBranch}`);
							} else {
								reject(`got ${xhr.statusText} in the HTTP request to ${api_pulls_url}`);
								return;
							}
						}
					}
					xhr.send();
				} else {
					console.log(`found ${commitBranches.length} elements with class commit-branch, was expecting only one`);
					reject("");
				}
			});
		}

		// getTravisURL returns a promise that returns the URL of the Travis
		// push build (if successful) or rejects it with a string error.
		function getTravisURL(username, password) {
			return new Promise((resolve, reject) => {
				const api_pull_url = `https://api.github.com/repos${repoPath()}/pulls/${pp[3]}`;
				// make an XHR Request to Github to find the URL
				const xhr = new XMLHttpRequest();
				xhr.open("GET", api_pull_url);
				xhr.setRequestHeader("Authorization", `Basic ${btoa(`${username}:${password}`)}`);
				xhr.onreadystatechange = () => {
					if (xhr.readyState === 4) {
						if (xhr.status >= 200 && xhr.status < 299) {
							const pr = JSON.parse(xhr.responseText);
							if (!pr || !pr._links || !pr._links.statuses || !pr._links.statuses.href) {
								reject(`statuses href not found in ${xhr.responseText}`);
								return;
							}
							// make a new request to fetch the Travis link from the status
							const innerXhr = new XMLHttpRequest();
							innerXhr.open("GET", pr._links.statuses.href);
							innerXhr.setRequestHeader("Authorization", `Basic ${btoa(`${username}:${password}`)}`);
							innerXhr.onreadystatechange = () => {
								if (innerXhr.readyState === 4) {
									if (innerXhr.status >= 200 && innerXhr.status < 299) {
										const statuses = JSON.parse(innerXhr.responseText);

										for (const status of statuses) {
											if (status.context === "continuous-integration/travis-ci/push") {
												resolve(status.target_url);
												return;
											}
										}

										reject(`did not find the status continuous-integration/travis-ci/push in ${JSON.stringify(statuses)}`);
									} else {
										reject(`got ${innerXhr.statusText} in the HTTP request to ${pr._links.statuses.href}`);
										return;
									}
								}
							}
							innerXhr.send();
						} else {
							reject(`got ${xhr.statusText} in the HTTP request to ${api_pull_url}`);
							return;
						}
					}
				}
				xhr.send();
			})
		}

		// repoPath returns the repo path taken from the URL, repoPath returns an
		// empty string if no repo was found in the URL.
		function repoPath() {
			if (pp.length < 2) {
				return "";
			}
			return `/${pp[0]}/${pp[1]}`;
		}

		function openTravis() {
			const targetURL = `https://travis-ci.${isGithubPrivateRepo() ? "com" : "org"}${rp}`;
			chrome.storage.sync.get(["github_api_username", "github_api_token"], data => {
				if (typeof data.github_api_username === "string" && data.github_api_username !== "" && typeof data.github_api_token === "string" && data.github_api_token !== "") {
					if (isGithubPR() === true) {
						getTravisURL(data.github_api_username, data.github_api_token)
							.then(url => {
								openURL(url);
							})
							.catch(error => {
								Front.showBanner(`error fetching the Travis URL: ${JSON.stringify(error)}`);
								setTimeout(() => {
									openURL(targetURL);
								}, 1000);
							});
					} else {
						openURL(targetURL);
					}
				} else {
					Front.showBanner("API username and password were not found, follow the documentation in the config");
					setTimeout(() => {
						openURL(targetURL);
					}, 1000);
				}
			});
		}

		function openGithub() {
			chrome.storage.sync.get(["github_api_username", "github_api_token"], data => {
				if (typeof data.github_api_username === "string" && data.github_api_username !== "" && typeof data.github_api_token === "string" && data.github_api_token !== "") {
					getGithubPullRequestURL(data.github_api_username, data.github_api_token)
						.then(url => {
							openURL(url);
						})
						.catch(error => {
							Front.showBanner(`error fetching the Github URL: ${JSON.stringify(error)}`);
							setTimeout(() => {
								openURL(`https://github.com${rp}`);
							}, 1000);
						});
				} else {
					Front.showBanner("API username and password were not found, follow the documentation in the config");
					setTimeout(() => {
						openURL(`https://github.com${rp}`);
					}, 1000);
				}
			});
		}

		// get the repository path, works on both travis and Github
		var rp = repoPath();
		if (rp === "") {
			Front.showBanner("You must be viewing a repository");
			return;
		}

		function openAlternateFile() {
			const file = pp[pp.length - 1];
			if (file.match(/_test\.go$/) !== null) {
				openURL(window.location.href.replace("_test.go", ".go"));
			} else if (file.match(/\.go$/) !== null) {
				openURL(window.location.href.replace(".go", "_test.go"));
			}
		}

		// are we on a source file?
		if (window.location.hostname == "github.com") {
			// are we viewing a source file?
			if (pp[2] === "blob") {
				openAlternateFile();
			} else {
				openTravis();
			}
		} else if (window.location.hostname == "travis-ci.com" || window.location.hostname == "travis-ci.org") {
			openGithub();
		}
	}
}

// END Custom functions }}}
//////////////////////////////////////

//////////////////////////////////////
// Custom bindings {{{

// NOTE: the help section are defined at
// https://github.com/brookhong/Surfingkeys/blob/aa4aae5af30979e43fb7408a392359bfadc3f991/pages/front.js#L226

mapkey("ce", "#7Copy the last element of the path in the URL", copyLastElementInPath);
mapkey(",.", "#8Open alternate", openAlternate(false));
mapkey("g,.", "#8Open alternate in a new tab", openAlternate(true));
mapkey(",r", "#4Reload the page skipping the cache", () => {
	RUNTIME("reloadTab", {
		nocache: true
	});
});
mapkey("sr", "#11Reload settings", () => {
	RUNTIME("loadSettingsFromUrl", {
		url: "file://@home_dir@/.surfingkeys.js"
	});
	Front.showBanner('settings were reloaded');
});
mapkey("spl", "#13Set Proxy to Charles", () => {
	// setProxy 127.0.0.1:8888;
	// setProxyMode always;
	RUNTIME("updateProxy", {
		mode: "always",
		proxy: "127.0.0.1:8888"
	});
});
mapkey('su', '#4Edit current URL with vim editor', () => {
	Front.showEditor(window.location.href, data => {
		window.location.href = data;
	}, 'url');
});

// paste and go
mapkey('v', '#3Paste and go', () => {
	Clipboard.read(response => {
		window.location.href = response.data;
	});
});
mapkey('V', '#1Paste and go in a new tab', () => {
	Clipboard.read(response => {
		window.open(response.data);
	});
});

// END Custom bindings }}}
//////////////////////////////////////
