// INFO {{{
var INFO = xml`
<plugin name="getip.js" version="1.0.0"
        href="https://github.com/kalbasit/dotfiles"
        summary="getIP displays the current external IP"
        lang="en-US"
        xmlns="http://vimperator.org/namespaces/liberator">
  <author email="me@kalbas.it">kalbasit</author>
  <license>MIT</license>
  <project name="Vimperator" minVersion="3.0"/>
</plugin>`;
// }}}


(function() {
  // Functions {{{
  function getIP() { // {{{
    return new Promise(function(resolve, reject) {
      var xhr = new XMLHttpRequest();
      xhr.open("GET", "https://api.ipify.org/?format=json", true);
      xhr.onreadystatechange = function() { // {{{
        if (xhr.readyState === 4) {
          if (xhr.status >= 200 && xhr.status < 300) {
            try {
              var res = JSON.parse(xhr.responseText);
              resolve(res.ip)
            } catch(e) {
              reject(e);
            }
          } else {
            reject("got status: " + xhr.statusText);
          }
        }
      }  // }}}
      xhr.send();
    });
  } // }}}
  // }}}

  commands.addUserCommand( // {{{
    ['getIP'],
    'Get the current IP',
    function() {
      getIP()
        .then((ip) => {
          liberator.echo("IP: " + ip);
        })
        .catch((e) => {
          liberator.echoerr(e);
        });
    },
    {},
    true
  ); // }}}

})();

// vim:sw=2 ts=2 et si fdm=marker:
