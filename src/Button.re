let component = ReasonReact.statelessComponent("Button");

let make = (~onClick, ~text : string, _children) => {
  ...component,
  render: _self => {
    <button onClick>(Utils.text(text))</button>
  }
};
