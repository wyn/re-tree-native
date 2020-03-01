module Q = QCheck_runner;

let testsuite = List.concat([TestList.tests, TestPath.tests]);

let () = Q.run_tests_main(testsuite);
