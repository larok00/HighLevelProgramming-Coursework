type Item = string * float * int // name,weight,utility

type ItemState = InSack | OutofSack | Unknown
type Partial = {Items: (Item * ItemState) list}