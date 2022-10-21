type state =
  | Dead
  | Alive

let dead =
  [
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
  ]

let block =
  [
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Alive; Alive; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Alive; Alive; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
  ]

let toad =
  [
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Alive; Alive; Alive; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Alive; Alive; Alive; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
  ]

let glider =
  [
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Alive; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Alive; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Alive; Alive; Alive; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
  ]
