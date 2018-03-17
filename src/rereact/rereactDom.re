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
  effectTag: option(effectTag),
  mutable stateNode: option(Dom.element),
  mutable effects: list(fiber)
};

type fiberUpdate =
  | Host(fiberUpdateHost);

let updateQueue: ref(list(fiberUpdate)) = ref([]);

let fiberRoot: ref(option(fiber)) = ref(None);

let nextUnitOfWork: ref(option(fiber)) = ref(None);

let pendingCommit: ref(option(fiber)) = ref(None);

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
  while (index^ < List.length(elements) || oldFiber^ != None) {
    let prevFiber = newFiber^;
    let element = List.nth(elements, index^);
    switch (oldFiber^, element) {
    | (None, elm) =>
      newFiber :=
        Some({
          tag:
            switch element {
            | Flat(Component(_)) => Component
            | Flat(String(_)) => Host
            | Flat(Nil) => Host
            | Nested(_, _, _) => Host
            },
          fiberType: element,
          parent: Some(wipFiber),
          child: None,
          sibling: None,
          alternate: None,
          effectTag: Some(Placement),
          effects: [],
          stateNode: None
        });
      if (index^ == 0) {
        wipFiber.child = newFiber^;
      } else {
        switch prevFiber {
        | Some(prev) => prev.sibling = newFiber^
        | None => ()
        };
      };
    | _ => ()
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
    parent.effects = parentEffects @ childEffects @ fiberEffects;
  | None => pendingCommit := Some(fiber)
  };

let perfomUnitOfWork = (wipFiber: fiber) : option(fiber) => {
  beginWork(wipFiber);
  switch wipFiber.child {
  | Some(_) => wipFiber.child
  | None =>
    let unitOfWork = ref(Some(wipFiber));
    let returnUnitOfWork: ref(option(fiber)) = ref(None);
    while (unitOfWork^ != None) {
      let Some(unit) = unitOfWork^;
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

let commitWork = fiber =>
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
        | None => assert false
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
        | Some(Update) => Js.log("Update")
        | Some(Deletion) => Js.log("Deleition")
        | None => Js.log("No effect")
        }
      };
    }
  };

let commitAllWork = fiber => {
  List.iter(commitWork, fiber.effects);
  nextUnitOfWork := None;
  pendingCommit := None;
  fiberRoot := Some(fiber);
};

let workLoop = () => {
  switch nextUnitOfWork^ {
  | None => resetNextUnitOfWork()
  | _ => ()
  };
  while (nextUnitOfWork^ != None) {
    Js.log("inside workloop");
    nextUnitOfWork :=
      (
        switch nextUnitOfWork^ {
        | Some(unit) => perfomUnitOfWork(unit)
        | None => None
        }
      );
  };
  switch pendingCommit^ {
  | Some(pendingCommit) => commitAllWork(pendingCommit)
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
  ();
  /* if (moreWork || List.length(updateQueue^) > 0) {
       requestIdleCallback(perfomWork);
     }; */
};

let parentContainer = ref(Webapi.Dom.Document.createElement("span", Webapi.Dom.document));

let render = (reactElement: reactElement, containerName) => {
  Js.log(fiberRoot);
  switch (Webapi.Dom.Document.getElementById(containerName, Webapi.Dom.document)) {
  | Some(dom) =>
    updateQueue := updateQueue^ @ [Host({from: HostRoot, dom, children: reactElement})];
    requestIdleCallback(perfomWork);
  | None => ()
  };
};