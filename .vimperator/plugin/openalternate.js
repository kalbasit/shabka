// INFO {{{
var INFO = xml`
<plugin name="openalternate.js" version="1.0.0"
        href="https://github.com/kalbasit/dotfiles"
        summary="getIP displays the current external IP"
        lang="en-US"
        xmlns="http://vimperator.org/namespaces/liberator">
  <author email="me@kalbas.it">kalbasit</author>
  <license>MIT</license>
  <project name="Vimperator" minVersion="3.0"/>
  <p></p>
  <item>
    <tags>:pdfjs-mapping-sample</tags>
    <description><p>mapping sample</p><code><![CDATA[
      nnoremap -urls ^\\.pdf$ i :<C-u>pdfjs index<Space>
      nnoremap -urls ^\\.pdf$ z :<C-u>pdfjs zoom<Space>
    ]]></code></description>
  </item>
</plugin>`;
// }}}


(function() {
  // Functions {{{

  // isGithubPrivateRepo() returns true if the current page is a private repo
  function isGithubPrivateRepo() { // {{{
    return document.getElementsByClassName("label-private").length > 0;
  } // }}}

  // isGithubPR returns true if we are viewing a PR
  function isGithubPR(pp) { // {{{
    return pp[2] === "pull";
  } // }}}

  // getGithubPullRequestURL() returns a promise that returns the URL of the PR (if
  // successful) or rejects it with a string error.
  function getGithubPullRequestURL(username, password) { // {{{
    return new Promise(function(resolve, reject) {
      // compute the XHR URL
      var api_pulls_url = "https://api.github.com/repos" + repoPath() + "/pulls";
      // find the branch name
      var commitBranches = window.document.getElementsByClassName("commit-branch");
      if (commitBranches.length === 1) {
        var commitBranch = commitBranches[0].title;
        // make an XHR Request to Github to find the URL
        var xhr = new XMLHttpRequest();
        xhr.open("GET", api_pulls_url, true);
        xhr.setRequestHeader("Authorization", "Basic " + btoa(username + ":" + password));
        xhr.onreadystatechange = function() {
          if (xhr.readyState === 4) {
            if (xhr.status >= 200 && xhr.status < 299) {
              var pulls = JSON.parse(xhr.responseText);
              for (var i = 0; i < pulls.length; i++) {
                var pull = pulls[i];
                if (pull.head.ref === commitBranch) {
                  resolve(pull.html_url);
                  return;
                }
              }
              reject("did not find a pull request for branch " + commitBranch);
            } else {
              reject("got " + xhr.statusText + " in the HTTP request to " + api_pulls_url);
              return;
            }
          }
        }
        xhr.send();
      } else {
        console.log("found " + commitBranches.length + " elements with class commit-branch, was expecting only one");
        reject("");
      }
    });
  } // }}}

  // getTravisURL returns a promise that returns the URL of the Travis
  // push build (if successful) or rejects it with a string error.
  function getTravisURL(username, password) { // {{{
    return new Promise(function(resolve, reject) {
      var api_pull_url = "https://api.github.com/repos" + repoPath() + "/pulls/" + pp[3];
      // make an XHR Request to Github to find the URL
      var xhr = new XMLHttpRequest();
      xhr.open("GET", api_pull_url, true);
      xhr.setRequestHeader("Authorization", "Basic " + btoa(username + ":" + password));
      xhr.onreadystatechange = function() {
        if (xhr.readyState === 4) {
          if (xhr.status >= 200 && xhr.status < 299) {
            var pr = JSON.parse(xhr.responseText);
            if (!pr || !pr._links || !pr._links.statuses || !pr._links.statuses.href) {
              reject("statuses href not found in " + xhr.responseText);
              return;
            }
            var innerXhr = new XMLHttpRequest();
            innerXhr.open("GET", pr._links.statuses.href, true);
            innerXhr.setRequestHeader("Authorization", "Basic " + btoa(username + ":" + password));
            innerXhr.onreadystatechange = function() {
              if (innerXhr.readyState === 4) {
                if (xhr.status >= 200 && xhr.status < 299) {
                  var statuses = JSON.parse(innerXhr.responseText);
                  for (var i = 0; i < statuses.length; i++) {
                    var status = statuses[i];
                    if (status.context === "continuous-integration/travis-ci/push") {
                      resolve(status.target_url);
                      return;
                    }
                  }
                  reject("did not find the status continuous-integration/travis-ci/push in " + JSON.stringify(statuses));
                } else {
                  reject("got " + xhr.statusText + " in the HTTP request to " + pr._links.statuses.href);
                  return;
                }
              }
            }
            innerXhr.send();
          } else {
            reject("got " + xhr.statusText + " in the HTTP request to " + api_pull_url);
            return;
          }
        }
      }
      xhr.send();
    })
  } // }}}

  // repoPath returns the repo path taken from the URL, repoPath returns an
  // empty string if no repo was found in the URL.
  function repoPath() { // {{{
    pp, _, _ = pathParts();
    if (pp.length < 2) {
      return "";
    }
    return "/" + pp[0] + "/" + pp[1];
  } // }}}

  function pathParts() {
    // start by splitting the path by slashes, ignoring the first slash to
    // prevent having the first element an empty string
    var pp = window.location.pathname.substring(1).split("/");

    // extract the anchor from the last element
    if (pp[pp.length - 1].indexOf("#") !== -1) {
      var tmp = pp[pp.length - 1].split("#");
      pp[pp.length - 1] = tmp[0];
      var anchor = tmp[1];
    }

    // extract any query params from the last element
    if (pp[pp.length - 1].indexOf("?") !== -1) {
      var tmp = pp[pp.length - 1].split("?");
      pp[pp.length - 1] = tmp[0];
      var query = tmp[1];
    }

    return [pp, query, anchor];
  }

  function openAlternate() { // {{{

  } // }}}

  // }}}

  commands.addUserCommand( // {{{
    ['openAlternate'],
    'open the alternate file (or CI build)',
    openAlternate,
    {},
    true
  ); // }}}

})();

// vim:sw=2 ts=2 et si fdm=marker:

