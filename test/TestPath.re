module Q = QCheck;
module QT = QCheck.Test;

module P = ReTree.Path.Parents;

let testPath =
  QT.make(
    ~count=1000,
    ~name="pathToRoot_opposite_rootToPath",
    Q.(list(string)),
    ps => {
      let p1 = ps |> P.fromRootToPathList |> P.toString;
      let p2 = ps |> List.rev |> P.fromPathToRootList |> P.toString;
      p1 == p2;
    },
  );

let tests = [testPath];
