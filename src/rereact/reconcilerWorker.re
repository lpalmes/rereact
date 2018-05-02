open RereactTypes;

let completeWork = fiber =>
  switch fiber.parent {
  | Some(Fiber(parent)) =>
    let childEffects = fiber.effects;
    let fiberEffects = fiber.effectTag != None ? [Fiber(fiber)] : [];
    let parentEffects = parent.effects;
    /* "Complete work effects " ++ (List.length(parentEffects) |> string_of_int) |> print_endline; */
    /* Debug.printEffects(parentEffects); */
    parent.effects = parentEffects @ childEffects @ fiberEffects;
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
        /* "New effects " ++ (List.length(unit.effects) |> string_of_int) |> print_endline; */
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