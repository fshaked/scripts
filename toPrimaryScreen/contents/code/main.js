var clients;
var i=0;

var debugPrint = function(msg) {
//     print(msg);
}

var moveWindow = function() {
    clients[i].screenChanged.disconnect(moveWindow);
    debugPrint("done " + clients[i].caption);
    i++;
    moveWindows();
}

var activeWindow = function() {
    clients[i].activeChanged.disconnect(activeWindow);
    debugPrint("moving " + clients[i].caption);
    clients[i].screenChanged.connect(moveWindow);
    workspace.slotWindowToNextScreen();
}

var moveWindows = function() {
    for (; i < clients.length; i++) {
        if (!clients[i].specialWindow && clients[i].screen != 0) {
            if (workspace.activeClient === clients[i]) {
                debugPrint("already active " + clients[i].caption);
                activeWindow();
            } else {
                debugPrint("activating " + clients[i].caption);
                clients[i].activeChanged.connect(activeWindow);
                workspace.activeClient = clients[i];
            }
            break;
        }
    }
}

var trigger = function(count) {
    debugPrint("triggered");
    clients = workspace.clientList();
    i = 0;
    moveWindows();
}

registerShortcut("To Primary Screen", "Move All Windows to the Primary Screen", "Meta+Down", trigger);
