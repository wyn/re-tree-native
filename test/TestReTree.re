module Q = QCheck;
module QT = QCheck.Test;

let test =
  QT.make(~count=1000, ~name="list_rev_is_involutive", Q.(list(small_nat)), l =>
    List.rev(List.rev(l)) == l
  );

QT.check_exn(test);

/* let test2 = */
/*   QT.make(~count=1000, ~name="buggy_test", Q.(list(small_nat)), l => */
/*     List.rev(l) == l */
/*   ); */

/* QT.check_exn(test2); */

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

QT.check_exn(testPath);
