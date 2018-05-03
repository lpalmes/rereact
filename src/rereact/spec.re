open Rereact;

module type HostConfig = {
  type hostNode;
  let createInstance: reactElement => hostNode;
  let createTextInstance: string => hostNode;
  let commitUpdate: (hostNode, option(RereactProps.props), RereactProps.props) => unit;
  let appendChild: (hostNode, hostNode) => unit;
  let removeChild: (hostNode, hostNode) => unit;
};