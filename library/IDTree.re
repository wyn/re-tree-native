module I = Identity;
module ID = I.FocusId;
module CID = I.ChildId;
module P = Path.Parents;

type children = CID.Map.t(t)
and t = {
  me: ID.t,
  children,
  isRoot: bool,
};

// NOTE remember root isn't actually in any maps
let _root = ID.create("root");
let empty = () => {
  {me: _root, children: CID.Map.empty, isRoot: true};
};

let emptySubtree = id => {
  {me: id, children: CID.Map.empty, isRoot: false};
};

let isRoot = t => t.isRoot;
let rootId = t => t |> isRoot ? Some(t.me) : None;

let myId = t => t.me;

let children = t => t.children;

let hasChildren = t => t |> children |> CID.Map.is_empty;

let toSummaryString = t => {
  "{ me: "
  ++ (t |> myId |> ID.toString)
  ++ ", #children: "
  ++ (t |> children |> CID.Map.cardinal |> string_of_int)
  ++ ", isRoot: "
  ++ (t |> isRoot |> string_of_bool)
  ++ "}";
};

let toString = (tree: t): string => {
  let rec _pprint = (tip: t, spacing: int): list(string) => {
    let fst = toSummaryString(tip) ++ "\n";
    let rest =
      CID.Map.fold(
        (cid, child: t, acc) => {
          let s =
            "|"
            ++ String.concat(".", List.init(spacing, _ => " "))
            ++ (cid |> CID.toString)
            ++ ":";
          /* ++ child->toSummaryString; */
          let sRest = _pprint(child, spacing + 2);
          [s, ...List.concat([sRest, acc])];
        },
        tip |> children,
        [],
      );

    [fst, ...rest];
  };
  "\n" ++ (_pprint(tree, 2) |> String.concat(""));
};

let _pathFromRoot = path => {
  path |> P.pathFromRoot |> List.map(I.convertParentToChild);
};

let addChild = (tree: t, path: P.t, id: ID.t): t => {
  // switch the path so that it goes from root -> node
  let pathFromRoot = _pathFromRoot(path);

  let rec aux = (subtree: t, path: list(CID.t)): t => {
    /* %log.debug */
    /* "looking for home for " ++ id->ID.toString; */
    /* %log.debug */
    /* "current subtree: " ++ subtree->toSummaryString; */
    /* %log.debug */
    /* "rest of path: " ++ (path->List.map(CID.toString) |> String.concat(",")); */
    switch (path) {
    | [] =>
      /* %log.debug */
      /* "found home for " */
      /* ++ id->ID.toString */
      /* ++ " under " */
      /* ++ subtree->myId->ID.toString; */
      let childtree = emptySubtree(id);
      let ret = {
        ...subtree,
        children:
          subtree
          |> children
          |> CID.Map.add(I.convertFocusToChild(id), childtree),
      };
      /* %log.debug */
      /* "returning: " ++ ret->toSummaryString; */
      ret;
    | [cid, ...cids] =>
      /* %log.debug */
      /* "getting children for: " ++ cid->CID.toString; */
      /* %log.debug */
      /* "otherids: " ++ (cids->List.map(CID.toString) |> String.concat(",")); */
      let childtree =
        switch (subtree |> children |> CID.Map.find_opt(cid)) {
        | Some(c) => aux(c, cids)
        | None => aux(emptySubtree(cid |> I.convertChildToFocus), cids)
        };
      let ret = {
        ...subtree,
        children: subtree |> children |> CID.Map.add(cid, childtree),
      };
      /* %log.debug */
      /* "returning: " ++ ret->toSummaryString; */
      ret;
    };
  };
  aux(tree, pathFromRoot);
};

let removeChild = (tree: t, path: P.t, child: CID.t): t => {
  // switch the path so that it goes from root -> node
  let pathFromRoot = _pathFromRoot(path);

  let rec aux = (subtree: t, path: list(CID.t)): t => {
    /* %log.debug */
    /* "looking for " ++ child->CID.toString; */
    /* %log.debug */
    /* "current subtree: " ++ subtree->toSummaryString; */
    /* %log.debug */
    /* "rest of path: " ++ (path->List.map(CID.toString) |> String.concat(",")); */
    switch (path) {
    | [] =>
      let ret =
        switch (subtree |> children |> CID.Map.find_opt(child)) {
        | Some(childTreeWithChildToRemove) =>
          /* %log.debug */
          /* "got child, removing and merging its children: " */
          /* ++ childTreeWithChildToRemove->children->Map.size->string_of_int; */
          if (childTreeWithChildToRemove |> hasChildren) {
            let nextLevelDown = childTreeWithChildToRemove |> children;
            let treeWithRemoved =
              subtree |> children |> CID.Map.remove(child);
            let mergeTree =
              CID.Map.merge(
                (_ky, thisLevel, nextLevel) =>
                  switch (thisLevel, nextLevel) {
                  | (Some(_), _) => thisLevel
                  | (_, Some(_)) => nextLevel
                  | (_, _) => None
                  },
                treeWithRemoved,
                nextLevelDown,
              );
            {
              /* %log.debug */
              /* childTreeWithChildToRemove */
              /* ->children */
              /* ->Map.keysToArray */
              /* ->Array.map(CID.toString) */
              /* ->List.fromArray */
              /* |> String.concat(","); */

              ...subtree,
              children: mergeTree,
            };
          } else {
            {
              /* %log.debug */
              /* "no children so just removing child"; */
              ...subtree,
              children: subtree |> children |> CID.Map.remove(child),
            };
          }
        | None => subtree
        };
      /* %log.debug */
      /* "returning: " ++ ret->toSummaryString; */
      ret;

    | [cid, ...cids] =>
      /* %log.debug */
      /* "getting children for: " ++ cid->CID.toString; */
      /* %log.debug */
      /* "otherids: " ++ (cids->List.map(CID.toString) |> String.concat(",")); */
      let childtree =
        switch (subtree |> children |> CID.Map.find_opt(cid)) {
        | Some(c) => aux(c, cids)
        | None => subtree
        };
      let ret = {
        ...subtree,
        children: subtree |> children |> CID.Map.add(cid, childtree),
      };
      /* %log.debug */
      /* "returning: " ++ ret->toSummaryString; */
      ret;
    };
  };
  aux(tree, pathFromRoot);
};

let rec _getSubtreeAtPath = (subtree: t, path: list(CID.t)): option(t) => {
  /* %log.debug */
  /* "current subtree: " ++ subtree->toSummaryString; */
  /* %log.debug */
  /* "rest of path: " ++ (path->List.map(CID.toString) |> String.concat(",")); */
  switch (path) {
  | [] => Some(subtree)
  | [cid, ...cids] =>
    /* %log.debug */
    /* "getting children for: " ++ cid->CID.toString; */
    /* %log.debug */
    /* "otherids: " ++ (cids->List.map(CID.toString) |> String.concat(",")); */
    switch (subtree |> children |> CID.Map.find_opt(cid)) {
    | Some(c) => _getSubtreeAtPath(c, cids)
    | None => None
    }
  };
};

let rec _get =
        (subtree: t, path: P.t, cids: ref(list((CID.t, P.t))), first: bool) => {
  let tip = subtree |> myId |> I.convertFocusToChild;
  // if it is the root then recurse with the root children
  // but dont include the root in the final list
  let tippath =
    subtree |> isRoot
      ? path : first ? path : P.append(path, tip |> I.convertChildToParent);
  /* if (specialCase) { */
  /*   (); */
  /* } else { */
  /*   let p = tippath->P.moveUp; */
  /*   /\* [%log.debug *\/ */
  /*   /\*   "appending onto cids: " *\/ */
  /*   /\*   ++ tip->CID.toString *\/ */
  /*   /\*   ++ ", " *\/ */
  /*   /\*   ++ p->P.toString *\/ */
  /*   /\*   ++ ", " *\/ */
  /*   /\*   ++ tippath->P.toString *\/ */
  /*   /\*   ++ ", " *\/ */
  /*   /\*   ++ path->P.toString; *\/ */
  /*   /\*   ("", "") *\/ */
  /*   /\* ]; *\/ */

  /*   cids := [(tip, p), ...cids^]; */
  /* }; */
  let p = tippath |> P.moveUp;
  cids := subtree |> isRoot ? cids^ : [(tip, p), ...cids^];
  subtree
  |> children
  |> CID.Map.iter((_cid, childtree) => {
       _get(childtree, tippath, cids, false)
     });
};

let getChildPaths =
    (tree: t, path: P.t, inclusive: bool): array((CID.t, P.t)) => {
  // switch the path so that it goes from root -> node
  let pathFromRoot = _pathFromRoot(path);
  switch (_getSubtreeAtPath(tree, pathFromRoot)) {
  | Some(subtree) =>
    //    [%log.debug subtree->toString; ("", "")];
    let cids = ref([]: list((CID.t, P.t)));
    let _ = _get(subtree, path, cids, true);
    let ret = cids^;
    let tip = subtree |> myId |> I.convertFocusToChild;
    (inclusive ? ret : ret |> List.filter(d => {fst(d) != tip}))
    |> Array.of_list;
  | None => [||]
  };
};

let getAllPaths = (tree: t): array((CID.t, P.t)) => {
  getChildPaths(tree, P.empty(), true);
};

let getChildIds = (tree: t, path: P.t, inclusive: bool): array(CID.t) => {
  getChildPaths(tree, path, inclusive) |> Array.map(pr => fst(pr));
};

let getAllIds = (tree: t): array(CID.t) => {
  getChildIds(tree, P.empty(), true);
};

let getSubtree = (tree: t, path: P.t, id: ID.t): option(t) => {
  // switch the path so that it goes from root -> node
  let pathFromRoot = _pathFromRoot(path);
  switch (_getSubtreeAtPath(tree, pathFromRoot)) {
  | Some(parentTree) =>
    parentTree |> children |> CID.Map.find_opt(id |> I.convertFocusToChild)
  | None => None
  };
};

let addSubtree = (tree: t, from: ID.t, under: P.t, subtreeToAdd: t): t => {
  // switch the path so that it goes from root -> node
  let pathFromRoot = _pathFromRoot(under);
  // make sure that the tip of the subtree being added is not a root node
  // only wnat one root node per tree
  let subtreeToAdd =
    subtreeToAdd |> isRoot
      ? {...subtreeToAdd, isRoot: false, me: from} : subtreeToAdd;
  let rec aux = (subtree: t, path: list(CID.t)): t => {
    /* %log.debug */
    /* "looking for home for " ++ id->ID.toString; */
    /* %log.debug */
    /* "current subtree: " ++ subtree->toSummaryString; */
    /* %log.debug */
    /* "rest of path: " ++ (path->List.map(CID.toString) |> String.concat(",")); */
    switch (path) {
    | [] =>
      /* %log.debug */
      /* "found home for " */
      /* ++ id->ID.toString */
      /* ++ " under " */
      /* ++ subtree->myId->ID.toString; */
      let ret = {
        ...subtree,
        children:
          subtree
          |> children
          |> CID.Map.add(from |> I.convertFocusToChild, subtreeToAdd),
      };
      /* %log.debug */
      /* "returning: " ++ ret->toSummaryString; */
      ret;
    | [cid, ...cids] =>
      /* %log.debug */
      /* "getting children for: " ++ cid->CID.toString; */
      /* %log.debug */
      /* "otherids: " ++ (cids->List.map(CID.toString) |> String.concat(",")); */
      let childtree =
        switch (subtree |> children |> CID.Map.find_opt(cid)) {
        | Some(c) => aux(c, cids)
        | None => aux(emptySubtree(cid |> I.convertChildToFocus), cids)
        };
      let ret = {
        ...subtree,
        children: subtree |> children |> CID.Map.add(cid, childtree),
      };
      /* %log.debug */
      /* "returning: " ++ ret->toSummaryString; */
      ret;
    };
  };
  aux(tree, pathFromRoot);
};

let removeSubtree = (tree: t, path: P.t, child: CID.t): t => {
  // switch the path so that it goes from root -> node
  let pathFromRoot = _pathFromRoot(path);

  let rec aux = (subtree: t, path: list(CID.t)): t => {
    /* %log.debug */
    /* "looking for " ++ child->CID.toString; */
    /* %log.debug */
    /* "current subtree: " ++ subtree->toSummaryString; */
    /* %log.debug */
    /* "rest of path: " ++ (path->List.map(CID.toString) |> String.concat(",")); */
    switch (path) {
    | [] => {
        ...subtree,
        children: subtree |> children |> CID.Map.remove(child),
      }
    | [cid, ...cids] =>
      /* %log.debug */
      /* "getting children for: " ++ cid->CID.toString; */
      /* %log.debug */
      /* "otherids: " ++ (cids->List.map(CID.toString) |> String.concat(",")); */
      let childtree =
        switch (subtree |> children |> CID.Map.find_opt(cid)) {
        | Some(c) => aux(c, cids)
        | None => subtree
        };
      let ret = {
        ...subtree,
        children: subtree |> children |> CID.Map.add(cid, childtree),
      };
      /* %log.debug */
      /* "returning: " ++ ret->toSummaryString; */
      ret;
    };
  };
  aux(tree, pathFromRoot);
};
