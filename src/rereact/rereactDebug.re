open Rereact;

open RereactTypes;

let getFiberTag = fiber =>
  switch fiber.tag {
  | HostRoot =>
    switch fiber.alternate {
    | Some(_) => print_endline("Has alternatev")
    | None => print_endline("No alternate")
    };
    "Root";
  | Host => "Host"
  | Component => "Component"
  };

let getFiberEffect = fiber =>
  switch fiber.effectTag {
  | Some(Placement) => "Placement"
  | Some(Update) => "Update"
  | Some(Deletion) => "Deletion"
  | None => "No effect"
  };

let getFiberElement = element =>
  switch element {
  | Nested(t, _, _) => t
  | Flat(t) =>
    switch t {
    | Component({debugName}) => debugName
    | String(text) => "Text(" ++ text ++ ")"
    | Nil => "nil"
    }
  };

let printEffects = effects =>
  Belt.List.forEach(effects, f =>
    getFiberEffect(f) ++ " " ++ getFiberElement(f.fiberType) |> print_endline
  );

let rec printFiber = (Fiber(fiber), spaces: int) => {
  let rec printSibiling = (Fiber(fiber), spaces: int) => {};
  let fiberTag = getFiberTag(fiber);
  let elementType =
    switch fiber.fiberType {
    | Nested(t, _, _) => t
    | Flat(t) =>
      switch t {
      | Component({debugName}) => debugName
      | String(text) => "Text(" ++ text ++ ")"
      | Nil => "nil"
      }
    };
  fiberTag
  ++ " ("
  ++ elementType
  ++ ")\n"
  ++ (
    switch fiber.child {
    | Some(f) => printFiber(f, spaces + 1)
    | None => ""
    }
  )
  ++ (
    switch fiber.sibling {
    | Some(f) => printFiber(f, spaces + 1)
    | None => ""
    }
  );
};