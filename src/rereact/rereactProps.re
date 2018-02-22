type props = {
  id: option(string),
  value: option(string),
  className: option(string),
  placeholder: option(string),
  src: option(string),
  disabled: option(bool),
  onClick: option(Dom.event => unit),
  onChange: option(Dom.event => unit)
};

let defaultProps = {
  id: None,
  value: None,
  disabled: None,
  className: None,
  placeholder: None,
  src: None,
  onClick: None,
  onChange: None
};

let reconcileStringProp =
    (
      domElement: Dom.element,
      prevProps: option(props),
      prop: option(string),
      attributeName: string
    ) =>
  switch (prevProps, prop) {
  | (None, Some(value)) => Webapi.Dom.Element.setAttribute(attributeName, value, domElement)
  | (Some({src: Some(prevSrc)}), Some(src)) =>
    if (prevSrc != src) {
      Webapi.Dom.Element.setAttribute(attributeName, src, domElement);
    }
  | (Some({src: None}), Some(src)) =>
    Webapi.Dom.Element.setAttribute(attributeName, src, domElement)
  | (_, None) => Webapi.Dom.Element.removeAttribute(attributeName, domElement)
  };

let reconcile = (domElement: Dom.element, prevProps: option(props), props: props) => {
  reconcileStringProp(domElement, prevProps, props.id, "id");
  reconcileStringProp(domElement, prevProps, props.className, "class");
  reconcileStringProp(domElement, prevProps, props.src, "src");
  reconcileStringProp(domElement, prevProps, props.value, "value");
  reconcileStringProp(domElement, prevProps, props.placeholder, "placeholder");
  switch props.disabled {
  | Some(value) =>
    if (value) {
      Webapi.Dom.Element.setAttribute("disabled", "true", domElement);
    } else {
      Webapi.Dom.Element.removeAttribute("disabled", domElement);
    }
  | None => Webapi.Dom.Element.removeAttribute("disabled", domElement)
  };
  switch (prevProps, props.onClick) {
  | (None, Some(func)) => Webapi.Dom.Element.addEventListener("click", func, domElement)
  | (Some({onClick: Some(prevFunc)}), Some(func)) when prevFunc !== func =>
    Webapi.Dom.Element.removeEventListener("click", prevFunc, domElement);
    Webapi.Dom.Element.addEventListener("click", func, domElement);
  | (Some({onClick: Some(prevFunc)}), None) =>
    Webapi.Dom.Element.removeEventListener("click", prevFunc, domElement)
  | _ => ()
  };
  switch (prevProps, props.onChange) {
  | (None, Some(func)) => Webapi.Dom.Element.addEventListener("input", func, domElement)
  | (Some({onClick: Some(prevFunc)}), Some(func)) when prevFunc !== func =>
    Webapi.Dom.Element.removeEventListener("input", prevFunc, domElement);
    Webapi.Dom.Element.addEventListener("input", func, domElement);
  | (Some({onClick: Some(prevFunc)}), None) =>
    Webapi.Dom.Element.removeEventListener("input", prevFunc, domElement)
  | _ => ()
  };
};