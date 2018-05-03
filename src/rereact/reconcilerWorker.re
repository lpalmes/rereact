open RereactTypes;

let completeWork = fiber =>
  switch fiber.parent {
  | Some(Fiber(parent)) =>
    let childEffects = fiber.effects;
    let fiberEffects = fiber.effectTag != None ? [Fiber(fiber)] : [];
    let parentEffects = parent.effects;
    parent.effects = Belt.List.concatMany([|parentEffects, childEffects, fiberEffects|]);
  | None => ReconcilerGlobals.pendingCommit := Some(Fiber(fiber))
  };

let perfomUnitOfWork = (Fiber(wipFiber)) : option(opaqueFiber) => {
  ReconcilerBeginWork.beginWork(Fiber(wipFiber));
  switch wipFiber.child {
  | Some(Fiber(_)) => wipFiber.child
  | None =>
    let unitOfWork = ref(Some(Fiber(wipFiber)));
    let returnUnitOfWork: ref(option(opaqueFiber)) = ref(None);
    while (unitOfWork^ != None) {
      switch unitOfWork^ {
      | Some(Fiber(unit)) =>
        completeWork(unit);
        switch unit.sibling {
        | Some(sibiling) =>
          unitOfWork := None;
          returnUnitOfWork := Some(sibiling);
        | None => unitOfWork := unit.parent
        };
      | _ => ()
      };
    };
    returnUnitOfWork^;
  };
};