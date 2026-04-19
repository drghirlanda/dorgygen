MAX_SIZE = 100
"""Maximum size of the buffer."""

IGNORED = 42


def process(items: list[str], max_count: int = 10) -> bool:
    """Process a list of items up to max_count."""
    pass


def undocumented(x):
    pass


def multi_line(x: int) -> str:
    """Convert x to a string.

    Uses the built-in str() function.
    Returns an empty string for None.
    """
    pass


class Buffer:
    """A fixed-size buffer."""

    capacity = 0
    """Maximum number of items."""

    hidden = None

    def __init__(self, capacity: int) -> None:
        """Initialize the buffer with given capacity."""
        pass

    def push(self, item: str) -> None:
        """Add an item to the buffer."""
        pass

    def pop(self) -> str:
        """Remove and return the last item."""
        pass

    def undocumented_method(self):
        pass
