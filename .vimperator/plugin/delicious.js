// Vimperator Plugin: 'Delicious'
// Last Change: 25-Aug-2009
// License: MIT
// Maintainer: Travis Jeffery <travisjeffery@gmail.com>
// Usage: Use :delicious "description in quotes (optional)" <tags delimited by spaces> command
// Usage: if successfully posted you will see "done" echoed

commands.addUserCommand(['delicious'], "Save page as a bookmark on Delicious",
    function(args) {
        var url = "https://api.del.icio.us/v1/posts/add?";
        url += "&url=" + encodeURIComponent(buffer.URL);
        url += "&description=" + encodeURIComponent(buffer.title);
        var re = new RegExp(/"([^"]+)"/);
        var ext = args.string.match(re);
        if (ext) {
            url += "&extended=" + encodeURIComponent(ext[1]);
            url += "&tags=" + encodeURIComponent(args.string.substr(ext[0].length));
        } else {
            url += "&tags=" + encodeURIComponent(args.string);
        }

        var xhr = new XMLHttpRequest();
        xhr.open("POST", url, false);
        xhr.send(null);
        var xml = (new DOMParser()).parseFromString(xhr.responseText, "text/xml");
        var status = xml.getElementsByTagName('result')[0].getAttribute('code');

        liberator.echo(status);
    }
);
