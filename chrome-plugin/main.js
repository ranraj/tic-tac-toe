var storedState = localStorage.getItem('tictactoe');
var startingState = storedState ? JSON.parse(storedState) : null;
var ticTacToe = Elm.TicTacToe.fullscreen(startingState);
ticTacToe.ports.focus.subscribe(function(selector) {
    setTimeout(function() {
        var nodes = document.querySelectorAll(selector);
        if (nodes.length === 1 && document.activeElement !== nodes[0]) {
            nodes[0].focus();
        }
    }, 50);
});
ticTacToe.ports.setStorage.subscribe(function(state) {
    localStorage.setItem('tictactoe', JSON.stringify(state));
});
