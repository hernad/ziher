from textual.app import App, ComposeResult
from textual.widgets import Button

class ButtonApp(App):

    header = None

    CSS = """
    Screen {
       align: center middle;
    }
    """

    def on_button_pressed(self, event: Button.Pressed) -> None:
        if event.button.id == "push01":
           print("kraj price")
           self.exit()

    def compose(self) -> ComposeResult:
        yield Button(self.header, id="push01")

def run( header: str | None):
    app = ButtonApp()
    app.header = header if header else "PUSH 01" 

    app.run()

if __name__ == "__main__":
	run(None)
