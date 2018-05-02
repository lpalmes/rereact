type fiberTag =
  | Host
  | Component
  | HostRoot;

type effectTag =
  | Placement
  | Deletion
  | Update;

type fiber('state) = {
  tag: fiberTag,
  fiberType: Rereact.reactElement,
  parent: option(opaqueFiber),
  mutable state: option('state),
  mutable child: option(opaqueFiber),
  mutable sibling: option(opaqueFiber),
  alternate: option(opaqueFiber),
  mutable effectTag: option(effectTag),
  mutable stateNode: option(Dom.element),
  mutable effects: list(opaqueFiber)
}
and opaqueFiber =
  | Fiber(fiber('state)): opaqueFiber;

type fiberUpdateHost = {
  dom: Dom.element,
  children: Rereact.reactElement
};

type fiberUpdateComponent = {fiber: opaqueFiber};

type fiberUpdate =
  | HostRoot(fiberUpdateHost)
  | Component(fiberUpdateComponent);