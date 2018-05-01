open Rereact;

type fiberTag =
  | Host
  | Component
  | HostRoot;

type effectTag =
  | Placement
  | Deletion
  | Update;

type fiberUpdateHost = {
  from: fiberTag,
  dom: Dom.element,
  children: reactElement
};

type fiber = {
  tag: fiberTag,
  fiberType: reactElement,
  parent: option(fiber),
  mutable child: option(fiber),
  mutable sibling: option(fiber),
  alternate: option(fiber),
  mutable effectTag: option(effectTag),
  mutable stateNode: option(Dom.element),
  mutable effects: list(fiber)
};

type fiberUpdate =
  | Host(fiberUpdateHost);

let updateQueue: ref(list(fiberUpdate)) = ref([]);

let fiberRoot: ref(option(fiber)) = ref(None);

let nextUnitOfWork: ref(option(fiber)) = ref(None);

let pendingCommit: ref(option(fiber)) = ref(None);

module Debug = {
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
    List.iter(
      f => getFiberEffect(f) ++ " " ++ getFiberElement(f.fiberType) |> print_endline,
      effects
    );
  let rec printFiber = (fiber: fiber, spaces: int) => {
    let rec printSibiling = (fiber: fiber, spaces: int) => {};
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
    ++ String.make(spaces, '-')
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
};

let requestIdleCallback = f =>
  WindowRe.requestIdleCallback((_) => f(), Webapi.Dom.window) |> ignore;

let reconcileChildrenArray = (wipFiber: fiber, elements: list(reactElement)) => {
  let index = ref(0);
  let oldFiber =
    switch wipFiber.alternate {
    | Some({child}) => ref(child)
    | None => ref(None)
    };
  let newFiber: ref(option(fiber)) = ref(None);
  List.length(elements) |> string_of_int |> print_endline;
  List.map(Debug.getFiberElement, elements)
  |> List.fold_left((a, b) => a ++ " " ++ b, "")
  |> print_endline;
  while (index^ < List.length(elements) || oldFiber^ != None) {
    let prevFiber = newFiber^;
    let element =
      if (index^ < List.length(elements)) {
        Some(List.nth(elements, index^));
      } else {
        None;
      };
    switch (oldFiber^, element) {
    | (None, Some(elm)) =>
      newFiber :=
        Some({
          tag:
            switch elm {
            | Flat(Component(_)) => Component
            | Flat(String(_)) => Host
            | Flat(Nil) => Host
            | Nested(_, _, _) => Host
            },
          fiberType: elm,
          parent: Some(wipFiber),
          child: None,
          sibling: None,
          alternate: None,
          effectTag: Some(Placement),
          effects: [],
          stateNode: None
        })
    | (Some(oldFiber), None) =>
      oldFiber.effectTag = Some(Deletion);
      wipFiber.effects = wipFiber.effects @ [oldFiber];
    | (Some(oldFiber), Some(elm)) =>
      let sameType =
        switch (oldFiber.fiberType, elm) {
        | (Flat(Component(a)), Flat(Component(b))) => true
        | (Flat(String(a)), Flat(String(b))) when a == b => true
        | (Flat(Nil), Flat(Nil)) => true
        | (Nested(a, _, _), Nested(b, _, _)) when a == b => true
        | _ => false
        };
      if (sameType) {
        newFiber :=
          Some({
            tag:
              switch elm {
              | Flat(Component(_)) => Component
              | Flat(String(_)) => Host
              | Flat(Nil) => Host
              | Nested(_, _, _) => Host
              },
            fiberType: elm,
            parent: Some(wipFiber),
            child: None,
            sibling: None,
            alternate: Some(oldFiber),
            effectTag: Some(Update),
            effects: [],
            stateNode: oldFiber.stateNode
          });
      } else {
        newFiber :=
          Some({
            tag:
              switch elm {
              | Flat(Component(_)) => Component
              | Flat(String(_)) => Host
              | Flat(Nil) => Host
              | Nested(_, _, _) => Host
              },
            fiberType: elm,
            parent: Some(wipFiber),
            child: None,
            sibling: None,
            alternate: None,
            effectTag: Some(Placement),
            effects: [],
            stateNode: None
          });
        oldFiber.effectTag = Some(Deletion);
        wipFiber.effects = wipFiber.effects @ [oldFiber];
      };
    | _ => print_endline("default case")
    };
    switch oldFiber^ {
    | Some(old) => oldFiber := old.sibling
    | None => ()
    };
    if (index^ == 0) {
      wipFiber.child = newFiber^;
    } else {
      switch (prevFiber, element) {
      | (Some(prev), Some(_)) => prev.sibling = newFiber^
      | _ => ()
      };
    };
    index := index^ + 1;
  };
};

let createSelf = component : self(_) => {
  state: component.initialState(),
  reduce: (payloadToAction, payload) => {
    let action = payloadToAction(payload);
    let stateUpdate = component.reducer(action);
    ();
  },
  send: action => {
    let newState = component.reducer(action, component.initialState());
    switch newState {
    | NoUpdate => component.initialState()
    | Update(state) => state
    };
    Js.log(newState);
  }
};

let updateComponent = (wipFiber: fiber) =>
  switch wipFiber.fiberType {
  | Flat(Component(component)) =>
    let self = createSelf(component);
    let element = component.render(self);
    reconcileChildrenArray(wipFiber, [element]);
  | _ => ()
  };

let updateHost = (wipFiber: fiber) =>
  switch wipFiber.fiberType {
  | Nested(name, props, elements) =>
    switch wipFiber.stateNode {
    | None =>
      let node = Webapi.Dom.Document.createElement(name, Webapi.Dom.document);
      RereactProps.reconcile(node, None, props);
      wipFiber.stateNode = Some(node);
    | Some(_) => ()
    };
    reconcileChildrenArray(wipFiber, elements);
  | Flat(Component(_)) => updateComponent(wipFiber)
  | Flat(String(value)) =>
    switch wipFiber.stateNode {
    | None =>
      let node = Webapi.Dom.Document.createTextNode(value, Webapi.Dom.document);
      Webapi.Dom.Document.setNodeValue(Obj.magic(node), Js.Null.return(value));
      wipFiber.stateNode = Some(Obj.magic(node));
    | Some(_) => ()
    }
  | Flat(Nil) => ()
  };

let beginWork = wipFiber =>
  switch wipFiber.tag {
  | Host
  | HostRoot => updateHost(wipFiber)
  | Component => updateComponent(wipFiber)
  };

let completeWork = fiber =>
  switch fiber.parent {
  | Some(parent) =>
    let childEffects = fiber.effects;
    let fiberEffects = fiber.effectTag != None ? [fiber] : [];
    let parentEffects = parent.effects;
    /* "Complete work effects " ++ (List.length(parentEffects) |> string_of_int) |> print_endline; */
    /* Debug.printEffects(parentEffects); */
    parent.effects = parentEffects @ childEffects @ fiberEffects;
  | None => pendingCommit := Some(fiber)
  };

let perfomUnitOfWork = (wipFiber: fiber) : option(fiber) => {
  beginWork(wipFiber);
  switch wipFiber.child {
  | Some(child) =>
    "Child: " ++ Debug.getFiberElement(child.fiberType) |> print_endline;
    wipFiber.child;
  | None =>
    let unitOfWork = ref(Some(wipFiber));
    let returnUnitOfWork: ref(option(fiber)) = ref(None);
    while (unitOfWork^ != None) {
      let Some(unit) = unitOfWork^;
      /* "New effects " ++ (List.length(unit.effects) |> string_of_int) |> print_endline; */
      completeWork(unit);
      switch unit.sibling {
      | Some(sibiling) =>
        unitOfWork := None;
        returnUnitOfWork := Some(sibiling);
      | None => unitOfWork := unit.parent
      };
    };
    returnUnitOfWork^;
  };
};

let resetNextUnitOfWork = () => {
  let update =
    switch updateQueue^ {
    | [Host(update)] =>
      updateQueue := [];
      Some(update);
    | [Host(update), ...data] =>
      updateQueue := data;
      Some(update);
    | [] => None
    };
  switch update {
  | Some(update) =>
    nextUnitOfWork :=
      Some({
        tag: HostRoot,
        fiberType: update.children,
        parent: None,
        child: None,
        sibling: None,
        alternate: fiberRoot^,
        effectTag: None,
        effects: [],
        stateNode: Some(update.dom)
      })
  | None => ()
  };
};

let commitDeletion = (fiber, domParent) => {
  print_endline("Commiting deletion");
  let node = ref(Some(fiber));
  let break = ref(false);
  while (break^ == false) {
    switch node^ {
    | Some({tag: Component, child}) =>
      print_endline("Fiber is component, moving into child");
      node := child;
    | Some({stateNode: Some(domNode)} as fiberNode) =>
      print_endline("Fiber has stateNode, removing it");
      Webapi.Dom.Element.removeChild(domNode, domParent) |> ignore;
      let internalBreak = ref(false);
      while (internalBreak^ == false) {
        print_endline("In while loop");
        switch node^ {
        | Some(n) =>
          if (n !== fiber) {
            internalBreak := true;
          };
          if (n.sibling != None) {
            internalBreak := true;
          };
          node := n.parent;
        | None => internalBreak := true
        };
      };
      if (fiberNode == fiber) {
        print_endline("Fiber node is the same as passed, breaking loop");
        break := true;
      };
      node := fiberNode.sibling;
      print_endline("Moving to sibling");
    | _ =>
      break := true;
      print_endline("Default case, just stop it");
    };
  };
};

let commitWork = fiber =>
  /* Debug.getFiberElement(fiber) ++ " " ++ Debug.getFiberEffect(fiber) |> print_endline; */
  switch fiber.tag {
  | HostRoot => ()
  | _ =>
    switch fiber.parent {
    | None => ()
    | Some(parent) =>
      let parentFiber = ref(parent);
      while (parentFiber.contents.tag == Component) {
        switch parentFiber.contents.parent {
        | Some(parent) => parentFiber := parent
        | None => ()
        };
      };
      switch parentFiber.contents.stateNode {
      | None => ()
      | Some(domParent) =>
        switch fiber.effectTag {
        | Some(Placement) =>
          switch fiber.stateNode {
          | Some(node) => Webapi.Dom.Element.appendChild(node, domParent)
          | None => ()
          }
        | Some(Update) =>
          switch fiber {
          | {
              fiberType: Nested(_, props, _),
              stateNode: Some(node),
              alternate: Some({fiberType: Nested(_, oldProps, _)})
            } =>
            RereactProps.reconcile(node, Some(oldProps), props)
          | {fiberType: Nested(_, props, _), stateNode: Some(node), alternate: None} =>
            RereactProps.reconcile(node, None, props)
          | _ => ()
          }
        | Some(Deletion) => commitDeletion(fiber, domParent)
        | None => Js.log("No effect")
        }
      };
    }
  };

let commitAllWork = fiber => {
  /* print_endline("Committing:"); */
  List.iter(commitWork, fiber.effects);
  nextUnitOfWork := None;
  pendingCommit := None;
  fiberRoot := Some(fiber);
};

let workLoop = () => {
  /* print_endline("Work Loop: "); */
  switch nextUnitOfWork^ {
  | None => resetNextUnitOfWork()
  | _ => ()
  };
  while (nextUnitOfWork^ != None) {
    nextUnitOfWork :=
      (
        switch nextUnitOfWork^ {
        | Some(unitOfWork) =>
          /* Debug.getFiberTag(unitOfWork)
             ++ " "
             ++ Debug.getFiberElement(unitOfWork)
             |> print_endline; */
          perfomUnitOfWork(unitOfWork)
        | None => None
        }
      );
  };
  switch pendingCommit^ {
  | Some(pendingCommit) =>
    /* let printedFiber = Debug.printFiber(pendingCommit, 1);
       print_endline(printedFiber); */
    commitAllWork(pendingCommit)
  | None => ()
  };
};

let rec perfomWork = () => {
  workLoop();
  let moreWork =
    switch nextUnitOfWork^ {
    | Some(_) => true
    | None => false
    };
  if (moreWork || List.length(updateQueue^) > 0) {
    requestIdleCallback(perfomWork);
  };
};

let parentContainer = ref(Webapi.Dom.Document.createElement("span", Webapi.Dom.document));

let render = (reactElement: reactElement, containerName) => {
  print_newline();
  print_newline();
  print_endline("Render:");
  switch fiberRoot^ {
  | Some(_) => print_endline("There is a fiber root")
  | None => print_endline("Fiber root empty")
  };
  switch (Webapi.Dom.Document.getElementById(containerName, Webapi.Dom.document)) {
  | Some(dom) =>
    updateQueue := updateQueue^ @ [Host({from: HostRoot, dom, children: reactElement})];
    perfomWork();
    print_newline();
    print_newline();
  | None => ()
  };
};