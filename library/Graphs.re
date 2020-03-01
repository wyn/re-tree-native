module I = Identity;
module ID = I.FocusId;
module PID = I.ParentId;
module CID = I.ChildId;
module P = Path.Parents;

module type GRAPH = {
  type t('a);
  type dataWithPath('a);
  let empty: unit => t('a);
  let toString: (t('a), 'a => string) => string;
  let size: t('a) => int;
  let hasChildren: t('a) => bool;
  let containsId: (t('a), ID.t) => bool;
  let pathFromNode: (t('a), ID.t) => option(P.t);
  let dataForNode: (t('a), ID.t) => option('a);
  let setDataForNode: (t('a), ID.t, 'a => 'a) => t('a);
  let subGraphForNode: (t('a), ID.t) => option(t('a));
  let addNodeAtPath: (t('a), ID.t, 'a, P.t) => t('a);
  let addNode: (t('a), ID.t, 'a) => t('a);
  let addNodeUnder: (t('a), ID.t, 'a, PID.t) => t('a);
  let removeNode: (t('a), ID.t) => result(t('a), string);
  let moveChild: (t('a), CID.t, PID.t) => result(t('a), string);
  let removeSubtree: (t('a), ID.t) => result(t('a), string);
  let setSubGraphForNode: (t('a), ID.t, t('a)) => result(t('a), string);
  let moveSubtree: (t('a), CID.t, PID.t) => result(t('a), string);
  let map: (t('a), 'a => 'b) => t('b);
  let forEach: (t('a), (ID.t, 'a) => unit) => unit;
  let keep: (t('a), (ID.t, 'a) => bool) => t('a);
  let toArray: t('a) => array('a);
};

module Graph: GRAPH = {
  type dataWithPath('a) = {
    pathUp: P.t,
    value: 'a,
  };

  type t('a) = {
    masterLookup: ID.Map.t(dataWithPath('a)),
    tree: IDTree.t,
  };

  let empty = () => {
    let tree = IDTree.empty();
    let masterLookup = ID.Map.empty;
    {masterLookup, tree};
  };

  let toString = (graph, ss) => {
    let treeS = graph.tree |> IDTree.toString;
    let dataS =
      ID.Map.fold(
        (ky, d, acc) => {
          let s =
            ID.toString(ky)
            ++ ": { "
            ++ "value: "
            ++ ss(d.value)
            ++ ", "
            ++ "pathUp: "
            ++ (d.pathUp |> P.toString)
            ++ " }";
          [s, ...acc];
        },
        graph.masterLookup,
        [],
      )
      |> String.concat("\n");
    "{tree:\n" ++ treeS ++ "\n masterLookup:\n" ++ dataS ++ "\n}";
  };

  let size = (graph: t('a)) => {
    graph.masterLookup |> ID.Map.cardinal;
  };

  let hasChildren = graph => {
    graph.tree |> IDTree.hasChildren;
  };

  let containsId = (graph: t('a), id: ID.t): bool => {
    graph.masterLookup |> ID.Map.mem(id);
  };

  let fullDataFromNode = (graph: t('a), id: ID.t): option(dataWithPath('a)) =>
    if (containsId(graph, id)) {
      let dataWithPath = graph.masterLookup |> ID.Map.find(id);
      Some(dataWithPath);
    } else {
      None;
    };

  let pathFromNode = (graph: t('a), id: ID.t): option(P.t) => {
    switch (fullDataFromNode(graph, id)) {
    | Some(d) => Some(d.pathUp)
    | None => None
    };
  };

  let dataForNode = (graph: t('a), id: ID.t): option('a) =>
    switch (fullDataFromNode(graph, id)) {
    | Some(d) => Some(d.value)
    | None => None
    };

  let setDataForNode = (graph: t('a), id: ID.t, f: 'a => 'a): t('a) =>
    switch (fullDataFromNode(graph, id)) {
    | Some(olddata) =>
      let masterLookup =
        graph.masterLookup
        |> ID.Map.add(id, {...olddata, value: f(olddata.value)});
      {...graph, masterLookup};
    | None => graph
    };

  let subGraphForNode = (graph: t('a), id: ID.t): option(t('a)) => {
    switch (pathFromNode(graph, id)) {
    | Some(path) =>
      switch (IDTree.getSubtree(graph.tree, path, id)) {
      | Some(tree) =>
        /* [%log.debug */
        /*   "subgraphfornode tree: " ++ tree |> IDTree.toString; */
        /*   ("", "") */
        /* ]; */
        let allIds =
          graph.masterLookup
          |> ID.Map.to_seq
          |> Seq.map(kv => fst(kv))
          |> ID.Set.of_seq;
        /* [%log.debug */
        /*   "all ids: " */
        /*   ++ ( */
        /*     allIds->Array.map(cid => cid->ID.toString)->List.fromArray */
        /*     |> String.concat(",") */
        /*   ); */
        /*   ("", "") */
        /* ]; */
        let ids = tree |> IDTree.getAllIds;
        /* [%log.debug */
        /*   "to keep: " */
        /*   ++ ( */
        /*     ids->Array.map(cid => cid->CID.toString)->List.fromArray */
        /*     |> String.concat(",") */
        /*   ); */
        /*   ("", "") */
        /* ]; */
        let toKeep =
          ids
          |> Array.map(I.convertChildToFocus)
          |> Array.to_seq
          |> ID.Set.of_seq;

        let toRemove = ID.Set.diff(allIds, toKeep);
        /* [%log.debug */
        /*   "to remove: " */
        /*   ++ ( */
        /*     cids->Array.map(cid => cid->ID.toString)->List.fromArray */
        /*     |> String.concat(",") */
        /*   ); */
        /*   ("", "") */
        /* ]; */
        // remove these from graph
        let masterLookup =
          graph.masterLookup
          |> ID.Map.filter((ky, _) => {!ID.Set.mem(ky, toRemove)});
        let ret = {masterLookup, tree};
        /* [%log.debug */
        /*   "subGraphForNode returning: " ++ ret->toString(_ => ""); */
        /*   ("", "") */
        /* ]; */
        Some(ret);
      | None => None
      }

    | None => None
    };
  };

  let addNodeAtPath = (graph: t('a), id: ID.t, data: 'a, path: P.t): t('a) => {
    let tree = IDTree.addChild(graph.tree, path, id);
    let masterLookup =
      graph.masterLookup |> ID.Map.add(id, {pathUp: path, value: data});
    {masterLookup, tree};
  };

  let addNode = (graph: t('a), id: ID.t, data: 'a): t('a) => {
    let path = P.empty();
    addNodeAtPath(graph, id, data, path);
  };

  let addNodeUnder = (graph: t('a), id: ID.t, data: 'a, under: PID.t): t('a) => {
    switch (pathFromNode(graph, under |> I.convertParentToFocus)) {
    | Some(path) =>
      let path = P.append(path, under);
      addNodeAtPath(graph, id, data, path);
    | None => graph
    };
  };

  let removeNode = (graph: t('a), id: ID.t): result(t('a), string) =>
    // if id exists then remove it from masterLookup and tree
    if (containsId(graph, id)) {
      // first need to find its path in the tree
      switch (pathFromNode(graph, id)) {
      | Some(path) =>
        /* %log.debug */
        /* "removeNode:" ++ id->ID.toString; */
        /* %log.debug */
        /* "removeNode - pathUp:" ++ path->P.toString; */
        let pid = id |> I.convertFocusToParent;
        let masterLookup =
          graph.masterLookup
          |> ID.Map.remove(id)  // have to remove the node
          |> ID.Map.map(dataWithPath => {
               {
                 ...dataWithPath,
                 // and update any paths that may have contained the removed node
                 pathUp: P.removeElement(dataWithPath.pathUp, pid),
               }
             });
        let tree =
          IDTree.removeChild(graph.tree, path, id |> I.convertFocusToChild);
        Ok({masterLookup, tree});
      // can now remove from master list and tree
      | None => Error("removeNode failed to get parent path")
      };
    } else {
      Ok(graph);
    };

  let moveChild =
      (graph: t('a), from: CID.t, under: PID.t): result(t('a), string) => {
    let pid = under |> I.convertParentToFocus;
    if (containsId(graph, pid)) {
      let id = from |> I.convertChildToFocus;
      switch (dataForNode(graph, id)) {
      | Some(data) =>
        // remove old child node first as it might edit the graph
        // then can calculate new paths properly
        switch (removeNode(graph, id)) {
        | Ok(graph) =>
          switch (pathFromNode(graph, pid)) {
          | Some(parentPathUp) =>
            // NOTE remember to append the new "under" id
            let pidPathUp = P.append(parentPathUp, under);
            /* %log.debug */
            /* "moveChild:" ++ pidPathUp->P.toString; */
            let masterLookup =
              graph.masterLookup
              |> ID.Map.add(id, {pathUp: pidPathUp, value: data});

            let tree = IDTree.addChild(graph.tree, pidPathUp, id);
            Ok({masterLookup, tree});
          | None => Error("moveChild failed to get parent path")
          }
        | Error(_) as err => err
        }
      | None => Error("moveChild failed to get data")
      };
    } else {
      /* %log.debug */
      /* "passthrough"; */
      Ok(graph);
    };
  };

  let removeSubtree = (graph: t('a), id: ID.t): result(t('a), string) =>
    // if id exists then remove it from masterLookup and tree
    if (containsId(graph, id)) {
      // first need to find its path in the tree
      switch (pathFromNode(graph, id)) {
      | Some(path) =>
        /* %log.debug */
        /* "found path: " ++ path->P.toString; */
        // NOTE remember to append id because do not want to delete siblings of id
        let idPath = P.append(path, id |> I.convertFocusToParent);
        // can now remove id and all children from master list and tree
        let ids =
          IDTree.getChildIds(graph.tree, idPath, true)  // include the parent ID too
          |> Array.map(I.convertChildToFocus)
          |> Array.to_seq
          |> ID.Set.of_seq;

        /* %log.debug */
        /* ids->Array.map(ID.toString)->List.fromArray |> String.concat(","); */
        let masterLookup =
          graph.masterLookup
          |> ID.Map.filter((ky, _vl) => !ID.Set.mem(ky, ids));
        // remove the subtree tipped by id found at path
        let tree =
          IDTree.removeSubtree(graph.tree, path, id |> I.convertFocusToChild);
        Ok({masterLookup, tree});
      | None => Error("removeSubtree failed to get parent path")
      };
    } else {
      Ok(graph);
    };

  let setSubGraphForNode =
      (graph: t('a), id: ID.t, subgraph: t('a)): result(t('a), string) => {
    /* [%log.debug "setSubGraphForNode: " ++ id->ID.toString; ("", "")]; */
    /* [%log.debug "input graph:" ++ graph->toString(d => "unknown"); ("", "")]; */
    /* [%log.debug */
    /*   "adding subgraph:" ++ subgraph->toString(d => "unknown"); */
    /*   ("", "") */
    /* ]; */
    /* [%log.debug "under:" ++ id->ID.toString; ("", "")]; */
    let masterLookup = graph.masterLookup;
    switch (pathFromNode(graph, id)) {
    | Some(pathUp) =>
      /* [%log.debug "got pathUp: " ++ pathUp->P.toString; ("", "")]; */
      switch (removeSubtree(graph, id)) {
      | Ok(graph) =>
        /* [%log.debug "removed subtree at: " ++ id->ID.toString; ("", "")]; */
        /* [%log.debug graph->toString(d => "unknown"); ("", "")]; */

        let tree = IDTree.addSubtree(graph.tree, id, pathUp, subgraph.tree);
        /* [%log.debug "got tree: " ++ tree->IDTree.toString; ("", "")]; */

        // know pid already exist
        let pidPathUp = P.append(pathUp, id |> I.convertFocusToParent);
        /* [%log.debug "got pid pathUp: " ++ pidPathUp->P.toString; ("", "")]; */

        let toMerge =
          IDTree.getChildPaths(tree, pidPathUp, true)  // include the original parent ID in this
          |> Array.map(d => {
               let i = fst(d) |> I.convertChildToFocus;
               let pth = snd(d);
               /* [%log.debug */
               /*   "got i: " ++ i->ID.toString ++ " - " ++ pth->P.toString; */
               /*   ("", "") */
               /* ]; */
               // if the subgraph lookup doesnt have the dataForNode
               // then hope it is on the original lookup
               switch (subgraph.masterLookup |> ID.Map.find_opt(i)) {
               | Some(dataWithPath) => (i, {...dataWithPath, pathUp: pth})
               | None =>
                 let dataWithPath = masterLookup |> ID.Map.find(i);
                 (i, {...dataWithPath, pathUp: pth});
               };
             })
          |> Array.to_seq
          |> ID.Map.of_seq;

        let merged =
          ID.Map.merge(
            (_ky, origval, newval) => {
              switch (origval, newval) {
              | (_, Some(_)) => newval
              | (Some(_), _) => origval
              | (_, _) => None
              }
            },
            graph.masterLookup,
            toMerge,
          );
        let ret = {masterLookup: merged, tree};
        /* [%log.debug */
        /*   "setSubGraphForNode returning: " ++ ret->toString(_ => ""); */
        /*   ("", "") */
        /* ]; */
        Ok(ret);
      | Error(_) as err => err
      }
    | None =>
      /* [%log.debug "didn't get pathUp for id: " ++ id->ID.toString; ("", "")]; */
      Ok(graph)
    };
  };

  let moveSubtree = (graph: t('a), from: CID.t, under: PID.t) => {
    let id = from |> Identity.convertChildToFocus;
    let pid = under |> I.convertParentToFocus;
    let masterLookup = graph.masterLookup;
    switch (containsId(graph, id), containsId(graph, pid)) {
    | (true, true) =>
      /* %log.debug */
      /* "pidPath In: " ++ graph->pathFromNode(pid)->Option.getExn->P.toString; */
      switch (subGraphForNode(graph, id)) {
      | Some(subtree) =>
        switch (removeSubtree(graph, id)) {
        | Ok(graph) =>
          // know pid already exist
          switch (pathFromNode(graph, pid)) {
          | Some(parentPathUp) =>
            let pidPathUp = P.append(parentPathUp, under);
            let tree =
              IDTree.addSubtree(graph.tree, id, pidPathUp, subtree.tree);
            let recalculatedPaths =
              IDTree.getChildPaths(tree, pidPathUp, false)  // dont want parent ID here
              |> Array.map(d => {
                   let i = fst(d) |> I.convertChildToFocus;
                   let pth = snd(d);
                   /* %log.debug */
                   /* "got i: " ++ i->ID.toString ++ " - " ++ pth->P.toString; */
                   let dataWithPath = masterLookup |> ID.Map.find(i);
                   (i, {...dataWithPath, pathUp: pth});
                 })
              |> Array.to_seq
              |> ID.Map.of_seq;

            let merged =
              ID.Map.merge(
                (_ky, origval, newval) => {
                  switch (origval, newval) {
                  | (_, Some(_)) => newval
                  | (Some(_), _) => origval
                  | (_, _) => None
                  }
                },
                masterLookup,
                recalculatedPaths,
              );
            Ok({masterLookup: merged, tree});
          | None => Error("moveSubtree failed to get parent path")
          }
        | Error(_) as err => err
        }
      | None => Error("moveSubtree failed to get subtree")
      }
    | (false, _)
    | (_, false) => Ok(graph)
    };
  };

  let map = (graph: t('a), f: 'a => 'b): t('b) => {
    {
      ...graph,
      masterLookup:
        graph.masterLookup |> ID.Map.map(d => {...d, value: f(d.value)}),
    };
  };

  let forEach = (graph: t('a), f: (ID.t, 'a) => unit): unit => {
    graph.masterLookup |> ID.Map.iter((k, v) => {f(k, v.value)});
  };

  let keep = (graph: t('a), f: (ID.t, 'a) => bool): t('a) => {
    // partition masterLookup into (keeping, discarding)
    // then remove from tree whats in the discard pile
    // then reform smaller master lookup by getting new paths
    let (willKeep, willDiscard) =
      graph.masterLookup |> ID.Map.partition((ky, vl) => {f(ky, vl.value)});

    let tree =
      ID.Map.fold(
        (id, dataWithPath, tree) => {
          IDTree.removeChild(
            tree,
            dataWithPath.pathUp,
            id |> I.convertFocusToChild,
          )
        },
        willDiscard,
        graph.tree,
      );

    let newChildPaths =
      tree
      |> IDTree.getAllPaths
      |> Array.map(d => {
           let i = fst(d) |> I.convertChildToFocus;
           let pth = snd(d);
           let dataWithPath = willKeep |> ID.Map.find(i);
           (i, {...dataWithPath, pathUp: pth});
         })
      |> Array.to_seq;

    let masterLookup = ID.Map.of_seq(newChildPaths);
    {masterLookup, tree};
  };

  let toArray = (graph: t('a)): array('a) => {
    graph.masterLookup
    |> ID.Map.to_seq
    |> Array.of_seq
    |> Array.map(d => snd(d).value);
  };
};
