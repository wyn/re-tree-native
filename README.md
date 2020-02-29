# re-tree


[![CircleCI](https://circleci.com/gh/yourgithubhandle/re-tree/tree/master.svg?style=svg)](https://circleci.com/gh/yourgithubhandle/re-tree/tree/master)


**Contains the following libraries and executables:**

```
re-tree@0.0.0
│
├─test/
│   name:    TestReTree.exe
│   main:    TestReTree
│   require: re-tree.lib
│
├─library/
│   library name: re-tree.lib
│   namespace:    ReTree
│   require:
│
└─executable/
    name:    ReTreeApp.exe
    main:    ReTreeApp
    require: re-tree.lib
```

## Developing:

```
npm install -g esy
git clone <this-repo>
esy install
esy build
```

## Running Binary:

After building the project, you can run the main binary that is produced.

```
esy x ReTreeApp.exe 
```

## Running Tests:

```
# Runs the "test" command in `package.json`.
esy test
```
