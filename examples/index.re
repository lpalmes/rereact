open RereactElements;

open Jsxnice;

type action =
  | Increment
  | Decrement;

module App = {
  let createElement = (~children as _, _) =>
    Rereact.element({
      ...Rereact.reducerComponent("App"),
      initialState: () => 1,
      reducer: (action, state) =>
        switch action {
        | Increment => Rereact.Update(state + 1)
        | Decrement => Rereact.Update(state - 1)
        },
      render: self =>
        <Box backgroundColor=White padding=(Px(32))>
          <Col width=(Percent(50.))>
            <button onClick=((_) => self.send(Increment))> (Rereact.string("Increment")) </button>
            <Row margin=(Px(8))> (Rereact.string(string_of_int(self.state) ++ " clicks")) </Row>
            <button onClick=((_) => self.send(Increment))> (Rereact.string("Decrement")) </button>
          </Col>
        </Box>
    });
};

RereactDom.render(<App />, "container");