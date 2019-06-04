let component = ReasonReact.statelessComponent("App");

module Styles = {
  open Css;
  let card =
    style([
      border(px(1), `solid, hex("898989")),
      borderRadius(px(4)),
      padding(rem(1.0)),
    ]);
};

let make = (~name, ~description, ~href, _children) => {
  ...component,
  render: _self =>
    <div>
      <h3>
        <a href target="_blank" rel="noopener noreferrer">
          {ReasonReact.string(name)}
        </a>
      </h3>
      <p> {ReasonReact.string(description)} </p>
    </div>,
};