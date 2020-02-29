module I = Identity;
module PID = I.ParentId;

module type PATH = {
  type el;
  type t;
  let empty: unit => t;
  let fromList: list(string) => t;
  let fromPathToRootList: list(string) => t;
  let fromRootToPathList: list(string) => t;
  let moveUp: t => t;
  let moveDown: t => t;
  let parent: t => option(el);
  let root: t => option(el);
  let pathToRoot: t => list(el);
  let pathFromRoot: t => list(el);
  let eq: (t, t) => bool;
  let append: (t, el) => t;
  let toString: t => string;
  let removeElement: (t, el) => t;
  let concat: (t, t) => t;
};

module Parents: PATH with type el = PID.t = {
  type el = PID.t;
  type t = {pathUp: list(el)};

  let empty = () => {pathUp: []};
  let fromList = path => {pathUp: path |> List.map(s => PID.create(s))};
  let fromPathToRootList = fromList;
  let fromRootToPathList = path => path |> List.rev |> fromList;
  let moveUp = parents => {
    {
      pathUp:
        switch (parents.pathUp) {
        | [_hd, ...tl] => tl
        | _ => []
        },
    };
  };
  let moveDown = parents => {
    let revd = {pathUp: parents.pathUp |> List.rev} |> moveUp;
    {pathUp: revd.pathUp |> List.rev};
  };
  let parent = parents =>
    switch (parents.pathUp) {
    | [hd, ..._tl] => Some(hd)
    | _ => None
    };
  let root = parents => {
    switch (List.length(parents.pathUp)) {
    | 0 => None
    | n => Some(List.nth(parents.pathUp, n - 1))
    };
  };

  let pathToRoot = (parents: t) => {
    parents.pathUp;
  };

  let pathFromRoot = (parents: t) => {
    parents |> pathToRoot |> List.rev;
  };

  let eq = (p1, p2) => {
    List.for_all2((id1, id2) => id1 == id2, p1.pathUp, p2.pathUp);
  };

  let append = (parents, el) => {
    {pathUp: [el, ...parents.pathUp]};
  };

  let toString = parents =>
    parents.pathUp |> List.map(p => p |> PID.toString) |> String.concat(",");

  let removeElement = (parents, el) => {
    {pathUp: parents.pathUp |> List.filter(pid => pid != el)};
  };

  let concat = (parents, other) => {
    {pathUp: List.concat([parents.pathUp, other.pathUp])};
  };
};
