module Make = (()) => {
  module type Id_t = {
    type t;
    let create: string => t;
    let toString: t => string;
  };
  module Id: Id_t = {
    type t = string;

    let create = (id: string) => id;
    let toString = s => s;
  };

  type t = Id.t;
  let create = Id.create;
  let toString = Id.toString;

  module OrderedId = {
    type t = Id.t;
    let compare = Stdlib.compare;
  };

  module Map = Map.Make(OrderedId);
  module Set = Set.Make(OrderedId);
  /* module Set = { */
  /*   type t('t) = Set.t(Id.t, Comparable.identity); */
  /*   let make = () => Set.make(~id=(module Comparable)); */
  /*   let fromArray = (vals: array('a)) => */
  /*     Set.fromArray(vals, ~id=(module Comparable)); */
  /* }; */
};

module FocusId =
  Make({});

module ChildId =
  Make({});

module ParentId =
  Make({});

let convertChildToParent = (id: ChildId.t): ParentId.t => {
  id |> ChildId.toString |> ParentId.create;
};

let convertParentToChild = (id: ParentId.t): ChildId.t => {
  id |> ParentId.toString |> ChildId.create;
};

let convertFocusToParent = (id: FocusId.t): ParentId.t => {
  id |> FocusId.toString |> ParentId.create;
};

let convertFocusToChild = (id: FocusId.t): ChildId.t => {
  id |> FocusId.toString |> ChildId.create;
};

let convertParentToFocus = (id: ParentId.t): FocusId.t => {
  id |> ParentId.toString |> FocusId.create;
};

let convertChildToFocus = (id: ChildId.t): FocusId.t => {
  id |> ChildId.toString |> FocusId.create;
};
