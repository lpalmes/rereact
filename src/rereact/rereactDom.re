open Rereact;

open RereactTypes;

let requestIdleCallback = f =>
  WindowRe.requestIdleCallback((_) => f(), Webapi.Dom.window) |> ignore;

let parentContainer = ref(Webapi.Dom.Document.createElement("span", Webapi.Dom.document));

let render = (reactElement: reactElement, containerName) =>
  switch (Webapi.Dom.Document.getElementById(containerName, Webapi.Dom.document)) {
  | Some(dom) =>
    ReconcilerGlobals.updateQueue :=
      ReconcilerGlobals.updateQueue^ @ [HostRoot({dom, children: reactElement})];
    ReconcilerScheduler.perfomWork();
  | None => ()
  };