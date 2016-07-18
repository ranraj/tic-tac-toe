# tic-tac-toe
  This is project is a use case to practice typed functional language. 

##Language : Elm-lang


How to install TciTacToe application dependecy packages?

    Check out the source code    

    Get in to the project directory. 

    Install Dependency using 
    
    ```elm
    elm package install
    ```

        Execute the elm package install and it will prompt your permission to download the dependency. Type y and Hit.

How to access the application?

    Note : Make sure ther service is up and running
    Find the index.html file placed in the src/web location. Simply open the html in the browser and start play.

How to make project?

    If you want to customize the service running port in the Elm ui.

    Do the chanes in the src/Elm/AppConfig.elm 

    ```
    serverAddress = localhost
    ```
    Execute the below command from the root directory to rebuild the project

    elm make src/Elm/TicTacToe.elm --output=src/web/tictactoe.js

    It creats a javascript file ( Elm source complied to a JS file )


