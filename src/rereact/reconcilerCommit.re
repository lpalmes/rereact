open RereactTypes;

let commitDeletion = (fiber, domParent) => {
  let node = ref(Some(fiber));
  let break = ref(false);
  while (break^ == false) {
    switch node^ {
    | Some(Fiber({tag: Component, child})) => node := child
    | Some(Fiber({stateNode: Some(domNode), sibling}) as fiberNode) =>
      Webapi.Dom.Element.removeChild(domNode, domParent) |> ignore;
      let internalBreak = ref(false);
      while (internalBreak^ == false) {
        switch node^ {
        | Some(Fiber(internalNode) as n) =>
          if (n !== fiber) {
            internalBreak := true;
          };
          if (internalNode.sibling != None) {
            internalBreak := true;
          };
          node := internalNode.parent;
        | None => internalBreak := true
        };
      };
      if (fiberNode == fiber) {
        break := true;
      };
      node := sibling;
    | _ => break := true
    };
  };
};

let commitWork = (Fiber(fiber)) =>
  switch fiber.tag {
  | HostRoot => ()
  | _ =>
    switch fiber.parent {
    | None => ()
    | Some(Fiber(parent)) =>
      let parentFiber = ref(parent);
      while (parentFiber.contents.tag == Component) {
        switch parentFiber.contents.parent {
        | Some(Fiber(p)) => parentFiber := Obj.magic(p)
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
              alternate: Some(Fiber({fiberType: Nested(_, oldProps, _)}))
            } =>
            RereactProps.reconcile(node, Some(oldProps), props)
          | {fiberType: Nested(_, props, _), stateNode: Some(node), alternate: None} =>
            RereactProps.reconcile(node, None, props)
          | _ => ()
          }
        | Some(Deletion) => commitDeletion(Fiber(fiber), domParent)
        | None => Js.log("No effect")
        }
      };
    }
  };

let commitAllWork = fiber => {
  Belt.List.forEach(fiber.effects, commitWork);
  ReconcilerGlobals.nextUnitOfWork := None;
  ReconcilerGlobals.pendingCommit := None;
  ReconcilerGlobals.fiberRoot := Some(Fiber(fiber));
};