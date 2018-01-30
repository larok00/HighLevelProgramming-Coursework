type DUIntBTree1 = | IntBTreeNode of int * DUIntBTree1 * DUIntBTree1 | EmptyNode // leaves do not carry data
type DUIntBTree2 = | IntBTreeNode of DUIntBTree2 * DUIntBTree2 | LeafNode of int // leaves carry data

// polymorpic version
type 'a DUBTree1 = BTreeNode of 'a * 'a DUBTree1 * 'a DUBTree1 | EmptyNodeP
type 'a DUBTree2 = BTreeNode of 'a DUBTree2 * 'a DUBTree2 | LeafNodeP of 'a
