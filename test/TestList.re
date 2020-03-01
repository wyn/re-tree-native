module Q = QCheck;
module QT = QCheck.Test;

let test =
  QT.make(~count=1000, ~name="list_rev_is_involutive", Q.(list(small_nat)), l =>
    List.rev(List.rev(l)) == l
  );

/* let test2 = */
/*   QT.make(~count=1000, ~name="buggy_test", Q.(list(small_nat)), l => */
/*     List.rev(l) == l */
/*   ); */

let tests = [test];
