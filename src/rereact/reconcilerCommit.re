open RereactTypes;

let commitDeletion = (fiber, domParent) => {
  print_endline("Commiting deletion");
  let node = ref(Some(fiber));
  let break = ref(false);
  while (break^ == false) {
    switch node^ {
    | Some(Fiber({tag: Component, child})) =>
      print_endline("Fiber is component, moving into child");
      node := child;
    | Some(Fiber({stateNode: Some(domNode), sibling}) as fiberNode) =>
      print_endline("Fiber has stateNode, removing it");
      Webapi.Dom.Element.removeChild(domNode, domParent) |> ignore;
      let internalBreak = ref(false);
      while (internalBreak^ == false) {
        print_endline("In while loop");
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
        print_endline("Fiber node is the same as passed, breaking loop");
        break := true;
      };
      node := sibling;
      print_endline("Moving to sibling");
    | _ =>
      break := true;
      print_endline("Default case, just stop it");
    };
  };
};

let commitWork = (Fiber(fiber)) =>
  /* Debug.getFiberElement(fiber) ++ " " ++ Debug.getFiberEffect(fiber) |> print_endline; */
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
  /* print_endline("Committing:"); */
  List.iter(commitWork, fiber.effects);
  ReconcilerGlobals.nextUnitOfWork := None;
  ReconcilerGlobals.pendingCommit := None;
  ReconcilerGlobals.fiberRoot := Some(Fiber(fiber));
};