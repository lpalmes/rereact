open Rereact;

open RereactTypes;

let getRoot = fiber => {
  let node = ref(fiber);
  while (node.contents.parent != None) {
    switch node.contents.parent {
    | Some(Fiber(parent)) => node := Obj.magic(parent)
    | _ => ()
    };
  };
  node^;
};

let resetNextUnitOfWork = () => {
  let update =
    switch ReconcilerGlobals.updateQueue^ {
    | [update] =>
      ReconcilerGlobals.updateQueue := [];
      Some(update);
    | [update, ...data] =>
      ReconcilerGlobals.updateQueue := data;
      Some(update);
    | [] => None
    };
  switch update {
  | Some(HostRoot(update)) =>
    ReconcilerGlobals.nextUnitOfWork :=
      Some(
        Fiber({
          tag: HostRoot,
          fiberType: update.children,
          state: None,
          parent: None,
          child: None,
          sibling: None,
          alternate: ReconcilerGlobals.fiberRoot^,
          effectTag: None,
          effects: [],
          stateNode: Some(update.dom)
        })
      )
  | Some(Component({fiber: Fiber(fiber)})) =>
    let rootFiber = getRoot(fiber);
    ReconcilerGlobals.nextUnitOfWork :=
      Some(
        Fiber({
          tag: HostRoot,
          fiberType: rootFiber.fiberType,
          state: rootFiber.state,
          parent: None,
          child: None,
          sibling: None,
          alternate: Some(Fiber(rootFiber)),
          effectTag: None,
          effects: [],
          stateNode: rootFiber.stateNode
        })
      );
  | None => ()
  };
};

let workLoop = () => {
  switch ReconcilerGlobals.nextUnitOfWork^ {
  | None => resetNextUnitOfWork()
  | _ => ()
  };
  while (ReconcilerGlobals.nextUnitOfWork^ != None) {
    ReconcilerGlobals.nextUnitOfWork :=
      (
        switch ReconcilerGlobals.nextUnitOfWork^ {
        | Some(unitOfWork) => ReconcilerWorker.perfomUnitOfWork(unitOfWork)
        | None => None
        }
      );
  };
  switch ReconcilerGlobals.pendingCommit^ {
  | Some(Fiber(pendingCommit)) => ReconcilerCommit.commitAllWork(pendingCommit)
  | None => ()
  };
};

let rec perfomWork = () => {
  workLoop();
  let moreWork =
    switch ReconcilerGlobals.nextUnitOfWork^ {
    | Some(_) => true
    | None => false
    };
  ReconcilerGlobals.globalWorker.work = perfomWork;
  if (moreWork || Belt.List.length(ReconcilerGlobals.updateQueue^) > 0) {
    perfomWork();
  };
};