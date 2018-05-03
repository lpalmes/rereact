open Rereact;

module Host: Spec.HostConfig = {
  type hostNode = Dom.element;
  let createInstance = (Nested(name, props, _)) => {
    let node = Webapi.Dom.Document.createElement(name, Webapi.Dom.document);
    RereactProps.reconcile(node, None, props);
    node;
  };
  let createTextInstance = value => {
    let node = Webapi.Dom.Document.createTextNode(value, Webapi.Dom.document);
    Webapi.Dom.Document.setNodeValue(Obj.magic(node), Js.Null.return(value));
    Obj.magic(node);
  };
  let commitUpdate = (node, oldProps, props) => RereactProps.reconcile(node, oldProps, props);
  let appendChild = (parent, node) => Webapi.Dom.Element.appendChild(node, parent);
  let removeChild = (parent, node) => Webapi.Dom.Element.removeChild(node, parent) |> ignore;
};

module DOMReconciler = Reconciler.Make(Host);

let render = (reactElement: reactElement, containerName) =>
  switch (Webapi.Dom.Document.getElementById(containerName, Webapi.Dom.document)) {
  | Some(dom) =>
    DOMReconciler.updateQueue :=
      Belt.List.concat(
        DOMReconciler.updateQueue^,
        [HostRoot({node: Obj.magic(dom), children: reactElement})]
      );
    DOMReconciler.perfomWork();
  | None => ()
  };