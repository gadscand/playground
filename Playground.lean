import Playground.Basic

def main (args : List String) : IO Unit := do
  let argc := args.length

  if argc < 2 then
    IO.println "{\"authorized\":false,\"error\":\"No JSON received\"}"
  else
    let json := getElem! args 1
    IO.println json
