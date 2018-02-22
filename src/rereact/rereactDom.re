open Rereact;

type renderedElement =
  | IFlat(list(opaqueInstance))
  | INested(string, list(renderedElement), RereactProps.props, Dom.element)
and instance('state, 'action) = {
  mutable component: component('state, 'action),
  element,
  mutable iState: 'state,
  mutable instanceSubTree: renderedElement,
  mutable dom: Dom.element,
  pendingStateUpdates: ref(list('state => update('state, 'action)))
}
and opaqueInstance =
  | Instance(instance('state, 'action)): opaqueInstance;

let globalInstance = ref(IFlat([]));

let executePendingStateUpdates = opaqueInstance => {
  let Instance(instance) = opaqueInstance;
  let executeUpdate = (~state, stateUpdate) =>
    switch (stateUpdate(state)) {
    | NoUpdate => state
    | Update(newState) => newState
    };
  let rec executeUpdates = (~state, stateUpdates) =>
    switch stateUpdates {
    | [] => state
    | [stateUpdate, ...otherStateUpdates] =>
      let nextState = executeUpdate(~state, stateUpdate);
      executeUpdates(~state=nextState, otherStateUpdates);
    };
  let pendingUpdates = List.rev(instance.pendingStateUpdates^);
  instance.pendingStateUpdates := [];
  let nextState = executeUpdates(~state=instance.iState, pendingUpdates);
  instance.iState = nextState;
};

let createInstance = (~component, ~element) => {
  let iState = component.initialState();
  {
    component,
    element,
    dom: Webapi.Dom.Document.createElement("span", Webapi.Dom.document),
    iState,
    instanceSubTree: IFlat([]),
    pendingStateUpdates: ref([])
  };
};

let mapListToOptionals = (b: list('b)) : list(option('b)) => List.map(a => Some(a), b);

let rec addOptionalElements = howMany =>
  switch howMany {
  | 0 => []
  | n when n > 0 => [None] @ addOptionalElements(howMany - 1)
  | n when n < 0 => []
  | _ => []
  };

let equalizeList = (a, b) => {
  let aLength = List.length(a);
  let bLength = List.length(b);
  if (aLength == bLength) {
    (mapListToOptionals(a), mapListToOptionals(b));
  } else {
    let maxLength = max(aLength, bLength);
    if (aLength == maxLength) {
      (mapListToOptionals(a), mapListToOptionals(b) @ addOptionalElements(aLength - bLength));
    } else {
      (mapListToOptionals(a) @ addOptionalElements(bLength - aLength), mapListToOptionals(b));
    };
  };
};

let rec reconcile =
        (parentDom: Dom.element, instance: option(renderedElement), element: option(reactElement)) =>
  switch (instance, element) {
  | (None, Some(Nested(name, props, elements))) =>
    let node = Webapi.Dom.Document.createElement(name, Webapi.Dom.document);
    Webapi.Dom.Element.appendChild(node, parentDom);
    RereactProps.reconcile(node, None, props);
    let els =
      List.map(reconcile(node, None), List.map(e => Some(e), elements))
      |> List.fold_left(
           (instances, e) =>
             switch e {
             | Some(instance) => [instance, ...instances]
             | None => instances
             },
           []
         )
      |> List.rev;
    Some(INested(name, els, props, node));
  | (Some(INested(_, _, _, dom)), None) =>
    let _ = Webapi.Dom.Element.removeChild(dom, parentDom);
    None;
  | (Some(INested(iName, _, _, dom)), Some(Nested(name, props, elements))) when iName != name =>
    let _ = Webapi.Dom.Element.removeChild(dom, parentDom);
    let node = Webapi.Dom.Document.createElement(name, Webapi.Dom.document);
    Webapi.Dom.Element.appendChild(node, parentDom);
    RereactProps.reconcile(node, None, props);
    let els =
      List.map(reconcile(node, None), List.map(e => Some(e), elements))
      |> List.fold_left(
           (instances, e) =>
             switch e {
             | Some(instance) => [instance, ...instances]
             | None => instances
             },
           []
         )
      |> List.rev;
    Some(INested(name, els, props, node));
  | (Some(INested(iName, iElements, prevProps, dom)), Some(Nested(name, props, elements)))
      when iName == name =>
    let (a, b) = equalizeList(iElements, elements);
    /* List.iter(Js.log, iElements);
       print_newline();
       List.iter(Js.log, elements); */
    if (List.length(a) != List.length(b)) {
      Js.log("Lists are different size");
      Js.log(List.length(a));
      Js.log(List.length(b));
    } else {
      ();
    };
    let els =
      List.map2(reconcile(dom), a, b)
      |> List.fold_left(
           (instances, e) =>
             switch e {
             | Some(instance) => [instance, ...instances]
             | None => instances
             },
           []
         )
      |> List.rev;
    RereactProps.reconcile(dom, Some(prevProps), props);
    Some(INested(name, els, props, dom));
  | (None, Some(Flat(elements))) =>
    let els =
      elements
      |> List.map(reconcileElement(parentDom, None))
      |> List.fold_left(
           (instances, e) =>
             switch e {
             | Some(instance) => [instance, ...instances]
             | None => instances
             },
           []
         );
    Some(IFlat(els));
  | (Some(IFlat(instances)), Some(Flat(elements))) =>
    let els =
      elements
      |> List.map2(reconcileElement(parentDom), List.map(i => Some(i), instances))
      |> List.fold_left(
           (instances, e) =>
             switch e {
             | Some(instance) => [instance, ...instances]
             | None => instances
             },
           []
         )
      |> List.rev;
    Some(IFlat(els));
  | _ => None
  }
and reconcileElement =
    (parentDom: Dom.element, instance: option(opaqueInstance), element)
    : option(opaqueInstance) =>
  switch (instance, element) {
  | (None, Component(component)) =>
    let instance = createInstance(~component, ~element);
    let self = createSelf(Obj.magic(instance));
    let subElements = component.render(Obj.magic(self));
    let instanceSubTree = reconcile(parentDom, None, Some(subElements));
    switch instanceSubTree {
    | Some(v) =>
      switch v {
      | INested(_, _, _, dom) => instance.dom = dom
      | _ => ()
      };
      instance.instanceSubTree = v;
    | None => ()
    };
    component.didMount(Obj.magic(self));
    Some(Instance(instance));
  | (
      Some(Instance({element: Component(_), dom, instanceSubTree} as instance)),
      Component(newComponent)
    ) =>
    instance.component = Obj.magic(newComponent);
    let self = createSelf(Obj.magic(instance));
    let subElements = newComponent.render(Obj.magic(self));
    let instanceSubTree = reconcile(dom, Some(instanceSubTree), Some(subElements));
    switch instanceSubTree {
    | Some(v) =>
      switch v {
      | INested(_, _, _, dom) => instance.dom = dom
      | _ => ()
      };
      instance.instanceSubTree = v;
    | None => ()
    };
    Some(Instance({...instance, element}));
  | (None, String(value)) =>
    let node = Webapi.Dom.Document.createElement("span", Webapi.Dom.document);
    Webapi.Dom.Element.appendChild(node, parentDom);
    Webapi.Dom.Element.setInnerText(node, value);
    Some(
      Instance({
        component: basicComponent("String"),
        element,
        iState: (),
        instanceSubTree: IFlat([]),
        dom: node,
        pendingStateUpdates: ref([])
      })
    );
  | (Some(Instance({element: String(iValue), dom} as instc)), String(value)) =>
    if (iValue == value) {
      Some(Instance(instc));
    } else {
      Webapi.Dom.Element.setInnerText(dom, value);
      Some(Instance({...instc, element}));
    }
  | _ => None
  }
and createSelf = instance : self(_) => {
  state: instance.iState,
  reduce: (payloadToAction, payload) => {
    let action = payloadToAction(payload);
    let stateUpdate = instance.component.reducer(action);
    instance.pendingStateUpdates := [stateUpdate, ...instance.pendingStateUpdates^];
    executePendingStateUpdates(Instance(instance));
    reconcileElement(instance.dom, Some(Instance(instance)), instance.element) |> ignore;
  },
  send: action => {
    let stateUpdate = instance.component.reducer(action);
    instance.pendingStateUpdates := [stateUpdate, ...instance.pendingStateUpdates^];
    executePendingStateUpdates(Instance(instance));
    reconcileElement(instance.dom, Some(Instance(instance)), instance.element) |> ignore;
  }
};

let parentContainer = ref(Webapi.Dom.Document.createElement("span", Webapi.Dom.document));

let rootInstance: ref(option(renderedElement)) = ref(None);

let render = (reactElement, containerName) => {
  print_endline("render");
  switch (Webapi.Dom.Document.getElementById(containerName, Webapi.Dom.document)) {
  | Some(dom) =>
    parentContainer := dom;
    rootInstance := reconcile(parentContainer^, rootInstance^, Some(reactElement));
    switch rootInstance^ {
    | Some(v) => v
    | None => IFlat([])
    };
  | None =>
    print_endline("No dom element found :(");
    IFlat([]);
  };
};

let hotUpdate = (reactElement, instance) =>
  switch (reconcile(parentContainer^, Some(instance), Some(reactElement))) {
  | Some(v) => v
  | None => IFlat([])
  };