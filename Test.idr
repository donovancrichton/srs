
data ZID
data Options
data HashValue

data Message =
  Hello
  | HelloAck
  | Commit (ZID, Options, HashValue)

