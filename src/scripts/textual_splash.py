from textual.app import App, ComposeResult
from textual.widgets import Button

class ButtonApp(App):

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
        yield Button("PUSH 01", id="push01")

def run():
    app = ButtonApp()
    app.run()

if __name__ == "__main__":
	run()
