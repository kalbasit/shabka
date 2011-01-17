/**
 * Plugin to interact with firebug.
 *
 * Usage:
 *   :firebug                     opens firebug
 *   :firebug open                opens firebug
 *   :firebug close               closes firebug
 *   :firebug off                 turns firebug off
 *   :firebug toggle              if closed, open firebug, otherwise close it.
 *   :firebug console-focus       places the cursor in the console command line.
 *   :firebug console-clear       clears the console
 *   :firebug tab <tabname>       focuses the specified firebug tab (console,
 *                                html, css, etc)
 *   :[count]firebug tabnext      focuses the next firebug tab (wraps at the end).
 *   :[count]firebug tabprevious  focuses the prev firebug tab (wraps at the
 *                                beginning).
 *
 * The following vimpartor key bindings are supported while the firebug panel
 * has focus, meaning that they will perform the expected action on the firebug
 * panel instead of the current web page.
 *   - scrolling:         j, k, h, l, gg, G, 0, $, <c-d>, <c-u>, <c-f>, <c-b>
 *   - tab switching:     gt, gT, g0, g$
 *
 * Note: the wincmd plugin[1] supports navigating to/from firebug panels like
 * regular html frames, giving them focus for allowing you to then navigate via
 * the supported vimperator key bindings noted above.
 *
 * [1] http://vimperator.org/trac/ticket/56
 *
 * @author Eric Van Dewoetine (ervandew@gmail.com)
 * @version 0.3
 *
 * TODO:
 *   - modify scrolling in panels to scroll down by focussing entries in the
 *     page.
 *   - for expandable/collapsible elements, add zo, zc to treat them like folds.
 *   - add ability to simulate a right click, and then j,k scroll items.
 */

function FirebugVimperator(){
  var fbContentBox = document.getElementById('fbContentBox');
  var panelFocused = null;

  // event listener to keep track of if/which firebug panel is focused.
  window.addEventListener('focus', function(event){
    var doc = null;
    if (event.target.nodeType == Node.DOCUMENT_NODE){
      doc = event.target;
    }else if (event.target.ownerPanel){
      doc = event.target.ownerPanel.document;
    }else if (event.target.parentNode.ownerPanel){ // script panel
      doc = event.target.parentNode.ownerPanel.document;
    }

    if (doc.location == 'chrome://firebug/content/panel.html'){
      for each (node in doc.getElementsByClassName('panelNode')){
        var match = /.*\spanelNode-(\w+)\s.*/.exec(node.className);
        if (match && node.getAttribute('active') == 'true'){
          panelFocused = match[1];
          return;
        }
      }
    }

    panelFocused = null;
  }, true);

  // listen for user clicking a firebug tab, to set proper focus
  document.getElementById('fbPanelBar1').addEventListener(
    'click', function(event){
      var name = event.target.getAttribute('label').toLowerCase();
      if (name == 'css'){
        name = 'stylesheet';
      }
      panelFocused = name;
    },
    true
  );
  document.getElementById('fbPanelBar2').addEventListener(
    'click', function(event){
      var name = event.target.getAttribute('label').toLowerCase();
      if (name == 'style'){
        name = 'css';
      }else if (name == 'dom'){
        name = 'domSide';
      }
      panelFocused = name;
    },
    true
  );

  function getPanelNode(name){
    var node = FirebugContext.getPanel(panelFocused, true).panelNode;
    if (panelFocused == 'script'){
      node = node.lastChild;
    }
    return node;
  }

  // hook into buffer scrolling to support scrolling in firebug panels
  var bufferScrollLines = buffer.scrollLines;
  buffer.scrollLines = function(lines){
    if (panelFocused){
      var node = getPanelNode(panelFocused);
      node.scrollTop += 10 * lines;
    }else{
      bufferScrollLines(lines);
    }
  };

  var bufferScrollToPercentile = buffer.scrollToPercentile;
  buffer.scrollToPercentile = function(percentage){
    if (panelFocused){
      var node = getPanelNode(panelFocused);
      node.scrollTop = node.scrollHeight * (percentage / 100);
    }else{
      bufferScrollToPercentile(percentage);
    }
  };

  var bufferScrollByScrollSize = buffer.scrollByScrollSize;
  buffer.scrollByScrollSize = function(count, direction){
    if (panelFocused){
      if (options["scroll"] > 0){
        bufferScrollLines(options["scroll"] * direction);
      }else{ // scroll half a page down in pixels
        var node = getPanelNode(panelFocused);
        node.scrollTop += node.clientHeight / 2 * direction;
      }
    }else{
      bufferScrollByScrollSize(count, direction);
    }
  };

  var bufferScrollPages = buffer.scrollPages;
  buffer.scrollPages = function(pages){
    if (panelFocused){
      var node = getPanelNode(panelFocused);
      node.scrollTop += (node.clientHeight - 20) * pages;
    }else{
      bufferScrollPages(pages);
    }
  };

  var bufferScrollColumns = buffer.scrollColumns;
  buffer.scrollColumns = function(cols){
    if (panelFocused){
      var node = getPanelNode(panelFocused);
      node.scrollLeft += 10 * cols;
    }else{
      bufferScrollColumns(cols);
    }
  };

  var bufferScrollStart = buffer.scrollStart;
  buffer.scrollStart = function(){
    if (panelFocused){
      var node = getPanelNode(panelFocused);
      node.scrollLeft = 0;
    }else{
      bufferScrollStart();
    }
  };

  var bufferScrollEnd = buffer.scrollEnd;
  buffer.scrollEnd = function(){
    if (panelFocused){
      var node = getPanelNode(panelFocused);
      node.scrollLeft = node.scrollHeight;
    }else{
      bufferScrollEnd();
    }
  };

  // hook into tab switching
  var tabsSelect = tabs.select;
  tabs.select = function(spec, wrap){
    if (panelFocused){
      if (spec !== undefined && spec !== ""){
        var names = fbv._getPanelNames();
        var browser = FirebugChrome.getCurrentBrowser();
        var position = names.indexOf(panelFocused);
        var focus = fbv._focusPanel;
        if (position == -1){
          names = fbv._getSidePanelNames(browser.chrome.getSelectedPanel());
          position = names.indexOf(panelFocused);
          focus = fbv._focusSidePanel;
        }
        var last = names.length - 1;
        var length = names.length;

        if (typeof spec === "number")
          position = spec;
        else if (spec === "$")
          position = last;
        else if (/^[+-]\d+$/.test(spec))
          position += parseInt(spec, 10);
        else if (/^\d+$/.test(spec))
          position = parseInt(spec, 10);

        if (position > last)
          position = wrap ? position % length : last;
        else if (position < 0)
          position = wrap ? (position % length) + length : 0;
        focus(names[position]);
      }
    }else{
      tabsSelect(spec, wrap);
    }
  };

  return {
    open: function(){
      if (fbContentBox.collapsed)
        Firebug.toggleBar(true, 'console');

      // when :firebug (open|toggle) mapped via key binding
      // (map ... :firebug toggle<cr>), focus needs to be delayed.
      setTimeout(function(){
        var browser = FirebugChrome.getCurrentBrowser();
        browser.chrome.getSelectedPanel().document.defaultView.focus();
      }, 100);
    },

    off: function() {
      Firebug.closeFirebug(true);
    },

    close: function(){
      if (!fbContentBox.collapsed)
        Firebug.toggleBar();
    },

    toggle: function(){
      if (fbContentBox.collapsed){
        fbv.open();
      }else{
        fbv.close();
      }
    },

    console_focus: function(){
      if (fbContentBox.collapsed){
        fbv.open();
      }
      Firebug.chrome.switchToPanel(FirebugContext, "console");
      var commandLine = Firebug.largeCommandLine
          ? Firebug.chrome.$("fbLargeCommandLine")
          : Firebug.chrome.$("fbCommandLine");
      setTimeout(function(){
        commandLine.select();
      }, 100);
    },

    console_clear: function(){
      if (!fbContentBox.collapsed){
        Firebug.Console.clear();
      }
    },

    tab: function(args){
      fbv.open();
      var name = args[0].toLowerCase();
      if (name == 'css'){
        name = 'stylesheet';
      }
      fbv._focusPanel(name);
    },

    tabnext: function(args, count){
      fbv.open();
      fbv._gotoNextPrevTabName(count, false);
    },

    tabprevious: function(args, count){
      fbv.open();
      fbv._gotoNextPrevTabName(count, true);
    },

    _execute: function(args){
      var name = args.length ? args.shift().replace('-', '_') : 'open';
      var cmd = fbv[name];
      if (!cmd){
        liberator.echoerr('Unsupported firebug command: ' + name);
        return false;
      }
      return cmd(args, args.count > 1 ? args.count : 1);
    },

    _gotoNextPrevTabName: function(count, previous){
      var names = fbv._getPanelNames();
      var browser = FirebugChrome.getCurrentBrowser();
      var index = names.indexOf(browser.chrome.getSelectedPanel().name);
      count = count % names.length;
      if(previous){
        index = index - count;
        if (index < 0){
          index += names.length;
        }
      }else{
        index = index + count;
        if (index >= names.length){
          index -= names.length;
        }
      }
      fbv.tab([names[index]]);
    },

    _getPanelNames: function(){
      var panels = Firebug.getMainPanelTypes(FirebugContext);
      return [p.prototype.name for each (p in panels)];
    },

    _getSidePanelNames: function(mainPanel){
      var panels = Firebug.getSidePanelTypes(FirebugContext, mainPanel);
      return [p.prototype.name for each (p in panels)];
    },

    _focusPanel: function(name){
      var browser = FirebugChrome.getCurrentBrowser();
      browser.chrome.selectPanel(name);
      browser.chrome.syncPanel();
      Firebug.showBar(true);
      var panel = FirebugContext.getPanel(name, true);
      if (name == 'script'){
        panel.panelNode.lastChild.focus();
      }else{
        panel.panelNode.focus();
      }
      panelFocused = name;
    },

    _focusSidePanel: function(name){
      var browser = FirebugChrome.getCurrentBrowser();
      browser.chrome.selectSidePanel(name);
      browser.chrome.syncPanel();
      Firebug.showBar(true);
      browser.chrome.getSelectedSidePanel().document.defaultView.focus();
      panelFocused = name;
    },

    _completer: function(context){
      var commands = [];
      for (var name in fbv){
        if (name.indexOf('_') !== 0 && fbv.hasOwnProperty(name)){
          commands.push(name.replace('_', '-'));
        }
      }
      context.completions = [[c, ''] for each (c in commands)];
    }
  };
}

var fbv = new FirebugVimperator();

commands.add(['firebug'],
  'Control firebug from within vimperator.',
  function(args) { fbv._execute(args); },
  { count: true, argCount: '*', completer: fbv._completer }
);
