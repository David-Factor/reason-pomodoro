/*

    Todo:
    [ ] - make counter count down rather than up
    [ ] - provide way to set timerOption
    [ ] - explore chrome extension

 */
[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo : string = "./logo.svg";

type timerOption =
  | Pomodoro
  | ShortBreak
  | LongBreak;

type timerState =
  | Beginning
  | Started(Js.Global.intervalId)
  | Paused
  | Finished;

type action =
  | Start(timerOption, Js.Global.intervalId)
  | Pause
  | Restart(Js.Global.intervalId)
  | Tick;

type settings = {
  pomodoro: int,
  shortBreak: int,
  longBreak: int,
};

type timer = {
  timerOption,
  timerState,
};

type state = {
  count: int,
  timer,
  settings,
};

let minsToSeconds = (seconds: int) : int => seconds * 60;

let defaultSettings: settings = {
  pomodoro: 1 |> minsToSeconds,
  shortBreak: 5 |> minsToSeconds,
  longBreak: 10 |> minsToSeconds,
};

let startTimer = send : Js.Global.intervalId =>
  Js.Global.setInterval(() => send(Tick), 1000);

let stopTimer = (timerId: timerState) : unit =>
  switch (timerId) {
  | Started(id) => Js.Global.clearInterval(id)
  | _ => ()
  };

let timerOptionDurations = (options: timerOption, settings: settings) : int =>
  switch (options) {
  | Pomodoro => settings.pomodoro
  | LongBreak => settings.longBreak
  | ShortBreak => settings.shortBreak
  };

let isFinished = ({count}: state) : bool => count == 0;

/*
let isBeginning = ({timer, count, settings}: state) : bool =>
  switch (timer.timerOption) {
  | Pomodoro => settings.pomodoro == count
  | LongBreak => settings.longBreak == count
  | ShortBreak => settings.shortBreak == count
  };
*/

let component = ReasonReact.reducerComponent("App");

let make = _children => {
  ...component,
  initialState: () => {
    count: defaultSettings.pomodoro,
    timer: {
      timerOption: Pomodoro,
      timerState: Beginning,
    },
    settings: defaultSettings,
  },
  reducer: (action, state) =>
    switch (action) {
    | Start(timerOption, timerId) =>
      ReasonReact.Update({
        ...state,
        timer: {
          timerOption,
          timerState: Started(timerId),
        },
      })
    | Pause =>
      ReasonReact.UpdateWithSideEffects(
        {
          ...state,
          timer: {
            timerOption: state.timer.timerOption,
            timerState: Paused,
          },
        },
        (_self => stopTimer(state.timer.timerState)),
      )
    | Restart(timerId) =>
      ReasonReact.UpdateWithSideEffects(
        {
          ...state,
          count:
            timerOptionDurations(state.timer.timerOption, state.settings),
          timer: {
            timerOption: state.timer.timerOption,
            timerState: Started(timerId),
          },
        },
        (_self => stopTimer(state.timer.timerState)),
      )
    | Tick when isFinished(state) =>
      ReasonReact.UpdateWithSideEffects(
        {
          ...state,
          timer: {
            ...state.timer,
            timerState: Finished,
          },
        },
        (_self => stopTimer(state.timer.timerState)),
      )
    | Tick => ReasonReact.Update({...state, count: state.count - 1})
    },
  render: ({state, send}) =>
    <div>
      <h1> (Utils.text(string_of_int(state.count))) </h1>
      <button onClick=(_e => send(Start(Pomodoro, startTimer(send))))>
        (Utils.text("Start"))
      </button>
      <button onClick=(_e => send(Pause))> (Utils.text("Pause")) </button>
      <button onClick=(_e => send(Restart(startTimer(send))))>
        (Utils.text("Restart"))
      </button>
    </div>,
};
