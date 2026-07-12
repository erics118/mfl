  $ mfl run -I ../../include ../../examples/tree.mfl
  inorder traversal: 1 2 3 4 5 6 7 
  sum: 28
  height: 3

  $ clang -x c ../../examples/tree.mfl && ./a.out
  inorder traversal: 1 2 3 4 5 6 7 
  sum: 28
  height: 3
