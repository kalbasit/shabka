//////////////////////////////////////
// Settings

Hints.characters = "arstneio";
settings.smoothScroll = false;

//////////////////////////////////////
// Colemak bindings

// normal mode

// save the originals into an alias so there will be no dependency or order
// issue
map('_h',  'h');
map('_j',  'j');
map('_k',  'k');
map('_l',  'l');
map('_n',  'n');
map('_N',  'N');
map('_R',  'R');
map('_af', 'af');
map('_v',  'v');
map('_V',  'V');
map('_yt', 'yt');
map('_yy', 'yy');
map('_yl', 'yl');
map('_yf', 'yf');
map('_S',  'S');
map('_D',  'D');

// remove the one we backed up
unmap('h');
unmap('j');
unmap('k');
unmap('l');
unmap('n');
unmap('N');
unmap('R');
unmap('af');
unmap('v');
unmap('V');
unmap('yt');
unmap('yy');
unmap('yl');
unmap('yf');
unmap('S');
unmap('D');

// map the Colemak bindings
map('n',  '_h');  // scroll left
map('e',  '_j');  // scroll down
map('i',  '_k');  // scroll up
map('o',  '_l');  // scroll right
map('k',  '_n');  // find next
map('K',  '_N');  // find previous
map('I',  '_R');  // go one tab right
map('F',  '_af'); // open hint in a new tab
map('a',  '_v');  // Toggle visual mode
map('A',  '_V');  // Restore visual mode
map('ct', '_yt'); // duplicate current page
map('cc', '_yy'); // copy current page URL
map('cl', '_yl'); // copy current page's title
map('cf', '_yf'); // copy form data in JSON on current page
map('N',  '_S');  // go back in history
map('O',  '_D');  // go forward in history

// finally remove what we have saved
unmap('_h');
unmap('_j');
unmap('_k');
unmap('_l');
unmap('_n');
unmap('_N');
unmap('_R');
unmap('_af');
unmap('_v');
unmap('_V');
unmap('_yt');
unmap('_yy');
unmap('_yl');
unmap('_yf');
unmap('_S');
unmap('_D');

// visual mode

// save the originals into an alias so there will be no dependency or order
// issue
vmap('_k', 'k');

// remove the one we backed up
vunmap('k');

// map the Colemak bindings
vmap('i', '_k') // backward line

// finally remove what we have saved
vunmap('_k')
