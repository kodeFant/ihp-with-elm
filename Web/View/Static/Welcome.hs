module Web.View.Static.Welcome where
import Web.View.Prelude

data WelcomeView = WelcomeView

instance View WelcomeView where
    html WelcomeView = [hsx|
        <h1>The Book App</h1>
        <div class="elm">Elm app not loaded 💩</div>
        <script src="elm/index.js"></script>
    |]