open Rereact;

open RereactTypes;

let reconcileChildrenArray = (Fiber(wipFiber), elements: list(reactElement)) => {
  let index = ref(0);
  let oldFiber =
    switch wipFiber.alternate {
    | Some(Fiber({child})) => ref(child)
    | None => ref(None)
    };
  let newFiber: ref(option(opaqueFiber)) = ref(None);
  while (index^ < Belt.List.length(elements) || oldFiber^ != None) {
    let prevFiber = newFiber^;
    let element = Belt.List.get(elements, index^);
    switch (oldFiber^, element) {
    | (None, Some(elm)) =>
      newFiber :=
        Some(
          Fiber({
            tag:
              switch elm {
              | Flat(Component(_)) => Component
              | Flat(String(_)) => Host
              | Flat(Nil) => Host
              | Nested(_, _, _) => Host
              },
            state: None,
            fiberType: elm,
            parent: Some(Fiber(wipFiber)),
            child: None,
            sibling: None,
            alternate: None,
            effectTag: Some(Placement),
            effects: [],
            stateNode: None
          })
        )
    | (Some(Fiber(oldFiber)), None) =>
      oldFiber.effectTag = Some(Deletion);
      wipFiber.effects = wipFiber.effects @ [Fiber(oldFiber)];
    | (Some(Fiber(oldFiber)), Some(elm)) =>
      let sameType =
        switch (oldFiber.fiberType, elm) {
        | (Flat(Component(_)), Flat(Component(_))) => true
        | (Flat(String(a)), Flat(String(b))) when a == b => true
        | (Flat(Nil), Flat(Nil)) => true
        | (Nested(a, _, _), Nested(b, _, _)) when a == b => true
        | _ => false
        };
      if (sameType) {
        newFiber :=
          Some(
            Fiber({
              tag:
                switch elm {
                | Flat(Component(_)) => Component
                | Flat(String(_)) => Host
                | Flat(Nil) => Host
                | Nested(_, _, _) => Host
                },
              state: None,
              fiberType: elm,
              parent: Some(Fiber(wipFiber)),
              child: None,
              sibling: None,
              alternate: Some(Fiber(oldFiber)),
              effectTag: Some(Update),
              effects: [],
              stateNode: oldFiber.stateNode
            })
          );
      } else {
        newFiber :=
          Some(
            Fiber({
              tag:
                switch elm {
                | Flat(Component(_)) => Component
                | Flat(String(_)) => Host
                | Flat(Nil) => Host
                | Nested(_, _, _) => Host
                },
              state: None,
              fiberType: elm,
              parent: Some(Fiber(wipFiber)),
              child: None,
              sibling: None,
              alternate: None,
              effectTag: Some(Placement),
              effects: [],
              stateNode: None
            })
          );
        oldFiber.effectTag = Some(Deletion);
        wipFiber.effects = wipFiber.effects @ [Fiber(oldFiber)];
      };
    | _ => ()
    };
    switch oldFiber^ {
    | Some(Fiber(old)) => oldFiber := old.sibling
    | None => ()
    };
    if (index^ == 0) {
      wipFiber.child = newFiber^;
    } else {
      switch (prevFiber, element) {
      | (Some(Fiber(prev)), Some(_)) => prev.sibling = newFiber^
      | _ => ()
      };
    };
    index := index^ + 1;
  };
};

let createSelf = f : self('state, 'action) =>
  switch f {
  | Fiber({fiberType: Flat(Component(component)), state: Some(state)} as fiber) => {
      state: Obj.magic(state),
      send: action => {
        let newState = component.reducer(Obj.magic(action), Obj.magic(state));
        fiber.state = (
          switch newState {
          | NoUpdate => Some(state)
          | Update(state) => Some(Obj.magic(state))
          }
        );
        ReconcilerGlobals.updateQueue :=
          ReconcilerGlobals.updateQueue^ @ [Component({fiber: Fiber(fiber)})];
        ReconcilerGlobals.globalWorker.work();
      }
    }
  | _ => assert false
  };

let updateComponent = (Fiber(wipFiber) as fiber) =>
  switch wipFiber.fiberType {
  | Flat(Component(component)) =>
    switch wipFiber {
    | {alternate: Some(Fiber({state}))} => wipFiber.state = Obj.magic(state)
    | _ => wipFiber.state = Some(Obj.magic(component.initialState()))
    };
    let self = createSelf(fiber);
    let element = component.render(self);
    reconcileChildrenArray(Fiber(wipFiber), [element]);
  | _ => ()
  };

let updateHost = (Fiber(wipFiber)) =>
  switch wipFiber.fiberType {
  | Nested(name, props, elements) =>
    switch wipFiber.stateNode {
    | None =>
      let node = Webapi.Dom.Document.createElement(name, Webapi.Dom.document);
      RereactProps.reconcile(node, None, props);
      wipFiber.stateNode = Some(node);
    | Some(_) => ()
    };
    reconcileChildrenArray(Fiber(wipFiber), elements);
  | Flat(Component(_)) => updateComponent(Fiber(wipFiber))
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

let beginWork = (Fiber(wipFiber)) =>
  switch wipFiber.tag {
  | Host
  | HostRoot => updateHost(Fiber(wipFiber))
  | Component => updateComponent(Fiber(wipFiber))
  };