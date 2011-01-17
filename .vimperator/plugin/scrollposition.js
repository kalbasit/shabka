var bufferPositionWidget = document.getElementById("liberator-statusline-field-bufferposition");


statusline.updateBufferPosition = function(percent)
{
    if (!percent || typeof percent != "number")
    {
        let win = document.commandDispatcher.focusedWindow;
        if (!win)
            return;
        percentY = win.scrollMaxY == 0 ? -1 : win.scrollY / win.scrollMaxY;
        percentX = win.scrollMaxX == 0 ? -1 : win.scrollX / win.scrollMaxX;
    }

    bufferPositionWidget.value = bufferPositionString(percentY, true);
    bufferPositionWidget.value += bufferPositionString(percentX, false);
}


function bufferPositionString(percent, horiz)
{
    var bufferPositionStr = "";
    percent = Math.round(percent * 100);
    if (percent < 0)
        bufferPositionStr = horiz ? "All" : "";
    else if (percent == 0)
        bufferPositionStr = horiz ? "Top" : " Left";
    else if (percent < 10)
        bufferPositionStr = " " + percent + "%";
    else if (percent >= 100)
        bufferPositionStr = horiz ? "Bot" : " Right";
    else
        bufferPositionStr = " " + percent + "%";

    return bufferPositionStr;
}
