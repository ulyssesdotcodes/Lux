function createSocket() {
    /* Running on localhost */
    return new WebSocket('ws://localhost:9160/');
}

function onMessage(event) {
    var p = $(document.createElement('p')).text(event.data);

    $('#messages').append(p);
    $('#messages').animate({scrollTop: $('#messages')[0].scrollHeight});

    if(event.data.match(/^[^:]* joined/)) {
        var user = event.data.replace(/ .*/, '');
        users.push(user);
        refreshUsers();
    }

    if(event.data.match(/^[^:]* disconnected/)) {
        var user = event.data.replace(/ .*/, '');
        var idx = users.indexOf(user);
        users = users.slice(0, idx).concat(users.slice(idx + 1));
        refreshUsers();
    }
}

$(document).ready(function () {
    $('#connect').click(function () {
        var ws = createSocket();

        ws.onopen = function() {
          ws.send(JSON.stringify({type: "connecting"}));
        };

        ws.onmessage = function(event) {
          $("#events").append(event.data);
            // if(event.data.match('^Welcome! Users: ')) {
            //     /* Calculate the list of initial users */
            //     var str = event.data.replace(/^Welcome! Users: /, '');
            //     if(str != "") {
            //         users = str.split(", ");
            //         refreshUsers();
            //     }

            //     $('#join-section').hide();
            //     $('#chat-section').show();
            //     $('#users-section').show();

            //     ws.onmessage = onMessage;

            //     $('#message-form').submit(function () {
            //         var text = $('#text').val();
            //         ws.send(text);
            //         $('#text').val('');
            //         return false;
            //     });
            // } else {
            //     $('#warnings').append(event.data);
            //     ws.close();
            // }
        };

        // $('#join').append('Connecting...');

        return false;
    });
});
