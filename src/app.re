/*

    Todo:
    [x] - make counter count down rather than up
    [x] - provide way to set timerOption
    [x] - show clock in digital format
    [~] - make the actions buttons state dependent -> clicking restart should show timerOption menu
    [~] - allow user to start pomodoro cycle
          + add start pomodoro cycle button
    [ ] - add options: allow timer options to be set by user
    [ ] - explore chrome extension

 */
[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo : string = "./logo.svg";

open Belt;

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
  | StartTimerOption(timerOption, Js.Global.intervalId)
  | Continue(Js.Global.intervalId)
  | Pause
  | Restart(Js.Global.intervalId)
  | Tick;

type timer = {
  timerOption,
  timerState,
};

type settings = {
  pomodoro: int,
  shortBreak: int,
  longBreak: int,
};

type cycle = {
  previous: list(timerOption),
  current: timerOption,
  remaining: list(timerOption),
};

type state = {
  count: int,
  timer,
  settings,
  cycle,
};

let nextCycle = ({remaining, current, previous}: cycle) : cycle => {
  let nextCurrent =
    Belt.List.head(remaining) |> Js.Option.getWithDefault(Pomodoro);
  let nextRemaining =
    Belt.List.drop(remaining, 1) |> Js.Option.getWithDefault([]);
  {
    current: nextCurrent,
    remaining: Belt.List.concat(nextRemaining, [current]),
    previous: Belt.List.concat([current], previous),
  };
};

let minsToSeconds = (seconds: int) : int => seconds * 60;

let toDecimalTime = (count: int) : string => {
  let minutes: int = (count |> float_of_int) /. 60. |> floor |> int_of_float;
  let seconds: int = count mod 60;
  let format = count =>
    switch ((count: int)) {
    | 0 => "00"
    | _ when count < 10 => "0" ++ (count |> string_of_int)
    | _ => count |> string_of_int
    };
  format(minutes) ++ ":" ++ format(seconds);
};

let defaultSettings: settings = {
  pomodoro: 25 |> minsToSeconds,
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

let timerOptionDuration = (options: timerOption, settings: settings) : int =>
  switch (options) {
  | Pomodoro => settings.pomodoro
  | LongBreak => settings.longBreak
  | ShortBreak => settings.shortBreak
  };

let timerOptionString = (options: timerOption) : string =>
  switch (options) {
  | Pomodoro => "Pomodoro"
  | LongBreak => "Long break"
  | ShortBreak => "Short break"
  };

let isFinished = ({count}: state) : bool => count == 0;

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
    cycle: {
      previous: [],
      remaining: [ShortBreak, Pomodoro, LongBreak],
      current: Pomodoro,
    },
  },
  reducer: (action, state) =>
    switch (action) {
    | StartTimerOption(timerOption, timerId) =>
      ReasonReact.UpdateWithSideEffects(
        {
          ...state,
          count: timerOptionDuration(timerOption, state.settings),
          timer: {
            timerOption,
            timerState: Started(timerId),
          },
        },
        (_self => stopTimer(state.timer.timerState)),
      )
    | Continue(timerId) =>
      ReasonReact.UpdateWithSideEffects(
        {
          ...state,
          timer: {
            ...state.timer,
            timerState: Started(timerId),
          },
        },
        (_self => stopTimer(state.timer.timerState)),
      )
    | Restart(timerId) =>
      ReasonReact.UpdateWithSideEffects(
        {
          ...state,
          count: timerOptionDuration(state.timer.timerOption, state.settings),
          timer: {
            timerOption: state.timer.timerOption,
            timerState: Started(timerId),
          },
        },
        (_self => stopTimer(state.timer.timerState)),
      )
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
    | Tick =>
      ReasonReact.Update({
        ...state,
        cycle: nextCycle(state.cycle),
        count: state.count - 1,
      })
    },
  render: ({state, send}) => {
    let pauseButton = <Button onClick=(_e => send(Pause)) text="Pause" />;
    let restartButton =
      <Button
        onClick=(_e => send(Restart(startTimer(send))))
        text="Restart"
      />;
    let continueButton =
      <Button
        onClick=(_e => send(Continue(startTimer(send))))
        text="Continue"
      />;
    let timerControls = <div> pauseButton restartButton </div>;
    let timerOptions =
      <div>
        <Button
          onClick=(_e => send(StartTimerOption(Pomodoro, startTimer(send))))
          text="Start Focusing"
        />
        <Button
          onClick=(
            _e => send(StartTimerOption(LongBreak, startTimer(send)))
          )
          text="Start Long Break"
        />
        <Button
          onClick=(
            _e => send(StartTimerOption(ShortBreak, startTimer(send)))
          )
          text="Start Short Break"
        />
      </div>;
    let menu =
      switch (state.timer.timerState) {
      | Started(_) => <div> timerControls timerOptions </div>
      | Paused => <div> restartButton continueButton timerOptions </div>
      | _ => <div> timerOptions </div>
      };
    <div>
      <h4> (Utils.text(timerOptionString(state.timer.timerOption))) </h4>
      <h1> (Utils.text(toDecimalTime(state.count))) </h1>
      menu
      <h4> (Utils.text(timerOptionString(state.cycle.current))) </h4>
    </div>;
  },
};
