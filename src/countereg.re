
  type action =
    | Tick
    | Start(Js.Global.intervalId)
    | Stop;
  type state = {
    count: int,
    timerId: option(Js.Global.intervalId),
  };
  let component = ReasonReact.reducerComponent("CleanCounter");
  let clearTimer = (timerId: option(Js.Global.intervalId)) : unit =>
    timerId
    |> Js.Option.map((. timerId) => Js.Global.clearInterval(timerId))
    |> Js.Option.getWithDefault();
  let make = _children => {
    let startTimer = (_, self) : unit =>
      self.ReasonReact.send(
        Start(
          Js.Global.setInterval(() => self.ReasonReact.send(Tick), 1000),
        ),
      );
    {
      ...component,
      initialState: () => {count: 9999, timerId: None},
      reducer: (action, state) =>
        switch (action) {
        | Tick => ReasonReact.Update({...state, count: state.count + 1})
        | Start(timer) =>
          ReasonReact.Update({...state, timerId: Some(timer)})
        | Stop =>
          ReasonReact.UpdateWithSideEffects(
            {...state, timerId: None},
            (_self => clearTimer(state.timerId)),
          )
        },
      render: ({state, send, handle}) =>
        <div>
          <button onClick=(handle(startTimer))>
            (Utils.text("CLEAN START" ++ string_of_int(state.count)))
          </button>
          <button onClick=(_e => send(Stop))> (Utils.text("STOP")) </button>
        </div>,
    };
  };
